// Author: Roar Nind Steffensen, 18/06/2023

namespace ChemicalEngine

module Simulator = 
    open Reaction

    let private resolution = 0.01
    let private scaleByResolution x = x * resolution

    let stepsInDuration duration = duration / resolution |> ceil |> int
    let timeAtIteration n = (float n) * resolution
    let approxCycleDuration = 20.0

    let dS ((reactions,solution):CRN) (name:Name) = 
        let concProduct solution (reactants:Reactants) = 
            reactants 
             |> Seq.map (fun (n, m) -> (Map.find n solution, m))
             |> Seq.fold (fun acc ((_,c), m) -> acc * pown c m) 1.0 

        let netChange (name:Name) (reactants:Reactants) (products:Products) :float = 
            let change (name:Name) (cs:ReactionComponent list) = 
                Seq.filter (fun (n,_) -> n = name) cs
                |> Seq.map mult
                |> Seq.sum 
                |> float

            let changeS = change name
            in (changeS products) - (changeS reactants)

        let summands (name:Name) (solution:Solution) ((reactants, rate, products):Reaction) = 
            let summand = rate * (netChange name reactants products) * (concProduct solution reactants)
            summand

        reactions 
            |> Seq.map (summands name solution) 
            |> Seq.sum 
            |> scaleByResolution

    let updateSpecies (crn:CRN) ((name, conc):Species) =
        (name, conc + dS crn name)

    let private update (crn:CRN) =
        let updatedSolution = Map.map (fun _ species -> updateSpecies crn species) (snd crn)
        in (fst crn, updatedSolution)
    let slowCrnUpdater time crn = 
        // if time = ceil time then printfn $"simulating time {time}" else ()
        update crn

    let simulate' crnUpdater (crn:CRN) :CRN seq = 
        let rec simulate' (crn:CRN) n =
            seq {yield crn; yield! simulate' (crnUpdater (timeAtIteration n) crn) (n+1)}
        in simulate' crn 1
    
    let simulateFast (crn:CRN) :CRN seq =
        let names = (snd crn) |> Map.keys |> Seq.toList
        let nameIndexLookup = List.mapi (fun i n -> (n,i)) names |> Map.ofList
        let concentrations = List.map (fun s -> conc <| Map.find s (snd crn)) names

        let getUpdateTemplate reactions s =
            let reactionTemplate s (reactants, rate, products) = 
                let netChange name reactants products = 
                    let change name cs = 
                        Seq.filter (fun (n,_) -> n = name) cs
                        |> Seq.map mult |> Seq.sum |> float
                    let changeS = change name
                    in (changeS products) - (changeS reactants)

                let kc = rate * netChange s reactants products
                let productTemplate = reactants |> List.map (fun (n, m) -> (Map.find n nameIndexLookup, m))
                (kc, productTemplate)
            in List.map (reactionTemplate s) reactions |> List.filter (fun (kc,_) -> kc <> 0.0)

        let updateTemplates = List.map (getUpdateTemplate (fst crn)) names
        
        let calculateConcProductFromTemplate (cs:Concentration list) (kc, productTemplate) =
            Seq.fold (fun acc (i,m) -> acc * pown cs[i] m) (kc*resolution) productTemplate

        let updatedConcentration cs (template, currentConcentration) =
            Seq.map (calculateConcProductFromTemplate cs) template
                |> Seq.sum
                |> ((+) currentConcentration)

        let update cs = 
            Seq.zip updateTemplates cs |> Seq.map (updatedConcentration cs) |> Seq.toList

        let mapToCRN (names:Name list) (concentrations:Concentration list) :CRN =
            let solution = Seq.zip names concentrations 
                            |> Seq.map (fun (n,c) -> (n,(n,c))) 
                            |> Map.ofSeq
            in (fst crn, solution)

        let rec simulate' cs :Concentration list seq =
            seq {yield cs; yield! simulate' (update cs)}
        in simulate' concentrations |> Seq.map (mapToCRN names)

    
    let private mapNameToReactionComponent multiplicity name :ReactionComponent =
        (name, multiplicity)

    let private mapNamesToReactionComponents multiplicity names :ReactionComponent list = 
        List.map (mapNameToReactionComponent multiplicity) names

    let private mapNamedReaction multiplicity (reactantNames, rate, productNames) :Reaction =
        let mapping = mapNamesToReactionComponents multiplicity
        in (mapping reactantNames, rate, mapping productNames)

    let private bindCmpPhases (formula:Formula) :Formula=
        let rec insertCmpPhases = 
            function
            | [] -> []
            | step::rest when List.exists Command.containsCmp step -> 
                        let step2 = Modules.cmpStep2()
                        let step3 = Modules.cmpStep3()
                        step::step2::step3::insertCmpPhases rest
            | step::rest -> step :: insertCmpPhases rest

        let compilation = insertCmpPhases formula
        compilation

    let private catalyseWith ((nameS, concS):Species) (crn:CRN) :CRN =
        let reactions = fst crn |> List.map (fun (reactants, rate, products) -> 
            let comp = mapNameToReactionComponent 1 nameS
            in (comp :: reactants), rate, comp :: products)

        let solution = snd crn |> Map.add nameS (nameS, concS)
        (reactions, solution)

    let private addToSolution (crn:CRN) species :CRN =
        let solution = List.fold (fun m (n,c) -> Map.add n (n,c) m) (snd crn) species
        in (fst crn, solution)

    let private bindClock (formula:Formula) :CRN =
        let clockPhases = Clock.stepPeriod * List.length formula |> max 3 // the clock needs at least 3 phases to oscillate
        let clockCRN = Modules.clock clockPhases

        let bindClockStep stepIndex step =
            let activePhase = Clock.stepPeriod * stepIndex
            let species = Clock.clockSpecies clockPhases activePhase
            step 
                |> List.map Command.toCRN 
                |> CRN.collect 
                |> catalyseWith species

        let bindOffPeriodSpecies crn =
            Seq.initInfinite id
                |> Seq.take clockPhases
                |> Seq.filter (fun i -> not <| Map.containsKey (Clock.clockSpeciesName i) (snd crn))
                |> Seq.map (Clock.clockSpecies clockPhases)
                |> Seq.toList
                |> addToSolution crn

        formula 
            |> List.mapi bindClockStep
            |> (fun crns -> clockCRN :: crns)
            |> CRN.collect 
            |> bindOffPeriodSpecies

    let private ofFormula (formula:Formula) :CRN =
        formula |> bindCmpPhases |> bindClock

    let simulateFormula' crnUpdater (formula:Formula) =
        let crn = ofFormula formula
        let names = snd crn |> Map.keys |> Set.ofSeq
        names, simulate' crnUpdater crn

    let simulateFormula (formula:Formula) =
        // simulateFormula' crnUpdater formula
        let crn = ofFormula formula
        let names = snd crn |> Map.keys |> Set.ofSeq
        names, simulateFast crn

    let extractConcentrations (names:Name Set) (duration:float) (crn:CRN seq) =
        let (slns, xs) = Seq.initInfinite timeAtIteration 
                            |> Seq.zip crn 
                            |> Seq.takeWhile (fun (c,t) -> t < duration) 
                            |> Seq.map (fun ((_,s),t) -> (s,t))
                            |> Seq.toList
                            |> List.unzip

        let data = List.map (fun n -> (n, slns |> List.map (fun s -> Map.find n s |> conc))) (Set.toList names)
        in (xs, data)

    let watch duration (formula:Formula) = 
        let (names, crns) = simulateFormula formula
        extractConcentrations names duration crns

    let watchFiltered duration filter formula = 
        let (xs, data) = watch duration formula
        in (xs, filter data)

    let onlyByNames names =
        List.filter (fun (n, _) -> List.contains n names)

    let onlyBySpecies species =
        onlyByNames (List.map name species)

    let removeByNamePrefixes (prefixes:string list) =
        let startsWith (n:string) :bool = List.exists (n.StartsWith:string->bool) prefixes
        List.filter (fst >> startsWith >> not) 

    let removeClock data =
        removeByNamePrefixes [Clock.clockSpeciesPrefix] data

    let shrinkData (xs,data) =
        let chunkSize = 1.0/resolution |> int
        let shrink f list =
            let chunks = List.chunkBySize chunkSize list
            List.foldBack (fun chunk list -> f chunk :: list) chunks []

        let shrinkData = shrink (fun f -> List.sum f / float chunkSize)
        let shrinkXs = shrink List.head

        in shrinkXs xs, List.map (fun (n,list) -> (n, shrinkData list)) data