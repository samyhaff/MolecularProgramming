// Author: Roar Nind Steffensen, 16/06/2023

namespace ChemicalEngine

module Modules =
    open Reaction
    open CRN

    // todo add input restrictions
    let private mapReactions rs :Reaction list=
        let mapSpecies = 
            List.map (fun s -> (name s, 1))
        in List.map (fun (re,ra,pr) -> mapSpecies re, ra, mapSpecies pr) rs

    let ld A B =
        let rate = 1.0
        let reactions = mapReactions [
            ([A], rate, [A; B]);
            ([B], rate, []);
        ]
        fromReactionAndSpecies reactions [A;B] |> Normal

    let add A B C =
        let rate = 1.0
        let reactions = mapReactions [
            ([A], rate, [A; C]);
            ([B], rate, [B; C]);
            ([C], rate, [])
        ]

        fromReactionAndSpecies reactions [A;B;C] |> Normal

    let subTmpName = "subTmp"
    let sub A B C =
        let rate = 1.0

        let H = (subTmpName, 0.0)
        let reactions = mapReactions [
            ([A], rate, [A; C]);
            ([B], rate, [B; H]);
            ([C], rate, [])
            ([C; H], rate, [])
        ]

        fromReactionAndSpecies reactions [A;B;C;H] |> Normal

    let mul A B C =
        let rate = 1.0
        let reactions = mapReactions [
            ([A; B], rate, [A; B; C]);
            ([C], rate, [])
        ]

        fromReactionAndSpecies reactions [A;B;C] |> Normal

    let div A B C =
        let rate = 1.0
        let reactions = mapReactions [
            ([A], rate, [A; C]);
            ([B; C], rate, [B])
        ]

        fromReactionAndSpecies reactions [A;B;C] |> Normal


    let sqrt A B =
        let reactions :Reaction list= [
            ([name A, 1], 1.0, [name A, 1; name B, 1]);
            ([name B, 2], 0.5, [])
        ]

        fromReactionAndSpecies reactions [A;B] |> Normal

    let cmpGtName = "cmpGt"
    let cmpLtName = "cmpLt"
    let cmpTmpName = "cmpTmp"

    let cmp X Y =
        let rate = 1.0

        let gt = (cmpGtName, 0.5)
        let lt = (cmpLtName, 0.5)
        let reactions = mapReactions [
            ([gt; Y], rate, [lt; Y]);
            ([lt; X], rate, [gt; X]);
        ]
        fromReactionAndSpecies reactions [X;Y;gt;lt] |> Cmp

    let cmpPhase2ReactionTemplate () =
        let gt = cmpGtName
        let lt = cmpLtName
        let B = cmpTmpName
        let rate = 1.0
        let reactions = [
            ([gt; lt], rate, [lt; B ]);
            ([B ; lt], rate, [lt; lt]);
            ([lt; gt], rate, [gt; B ]);
            ([B ; gt], rate, [gt; gt]);
        ]

        (reactions, [(B, 0.0)])

    let private addCatalysts (catalysts:Name list) (reactions:Reaction list) =
        let addCatalystsToComponents components =
            List.map (fun n -> (n, 1)) catalysts @ components
        in List.map (fun (reactants, rate, products) -> 
                addCatalystsToComponents reactants, rate, addCatalystsToComponents products) reactions

    let private applyCatalystsToCmds catalysts commands =
        let rec applyCatalystsToCmd cmd :Command= 
            match cmd with
            | Normal (r,s) -> Normal (addCatalysts catalysts r, s)
            | Cmp (r,s) -> Normal (addCatalysts catalysts r, s)
            | Nested cmds -> Nested (List.map applyCatalystsToCmd cmds)
        in List.map applyCatalystsToCmd commands

    let ifGt (commands:Command list) =
        let catalysts = [cmpGtName]
        in applyCatalystsToCmds catalysts commands
            |> Nested

    let ifLt (commands:Command list) =
        let catalysts = [cmpLtName]
        in applyCatalystsToCmds catalysts commands
            |> Nested


    // todo why is it not oscillating???
    let clock phases =

        let initClockSpecies i = (Clock.clockSpeciesName i, Clock.clockSpeciescConc phases i)
        let phaseIndices = [0..phases-1]
        let clcs = phaseIndices |> List.map initClockSpecies

        let clockReaction i =
            let i1, i2 = (i) % phases, (i+1) % phases
            in ([name clcs[i1], 1; name clcs[i2], 1], 1.0, [name clcs[i2], 2])

        let reactions = phaseIndices |> List.map clockReaction
        fromReactionAndSpecies reactions clcs
