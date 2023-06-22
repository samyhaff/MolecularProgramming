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

    let sub A B C =
        let rate = 1.0

        let subTmpName = sprintf "subTmp:%s-%s" (name A) (name B)
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

    let rxn reactants rate products =
        let species = reactants @ products
        let reactions = mapReactions [(reactants, rate, products)]
        fromReactionAndSpecies reactions species |> Normal


    let cmpE = ("cmpE", 0.5)
    let cmpXE = ("cmpXE", 0.0)
    let cmpYE = ("cmpYE", 0.0)
    let cmpXEgtY = ("cmpXEgtY", 0.5)
    let cmpXEltY = ("cmpXEltY", 0.5)
    let cmpYEgtX = ("cmpYEgtX", 0.5)
    let cmpYEltX = ("cmpYEltX", 0.5)
    let cmpB1 = ("cmpB1", 0.0)
    let cmpB2 = ("cmpB2", 0.0)

    let private cmpMapping X Y gt lt =
        let rate = 1.0

        let reactions = mapReactions [
            ([gt; Y], rate, [lt; Y]);
            ([lt; X], rate, [gt; X]);
        ]
        fromReactionAndSpecies reactions [X;Y;gt;lt] |> Normal

    let private cmpApproximateMajority gt lt B =
        let rate = 1.0
        let reactions = mapReactions [
            ([gt; lt], rate, [lt; B ]);
            ([B ; lt], rate, [lt; lt]);
            ([lt; gt], rate, [gt; B ]);
            ([B ; gt], rate, [gt; gt]);
        ]

        fromReactionAndSpecies reactions [gt;lt;B] |> Normal

    let cmp A B =
        [
            add A cmpE cmpXE;
            add B cmpE cmpYE;
            cmpMapping cmpXE B cmpXEgtY cmpXEltY;
            cmpMapping cmpYE A cmpYEgtX cmpYEltX;
        ] |> Cmp

    let cmpStep2 () :Step =
        [
            cmpApproximateMajority cmpXEgtY cmpXEltY cmpB1;
            cmpApproximateMajority cmpYEgtX cmpYEltX cmpB2;
        ]

    let private addCatalysts (catalysts:Species list) (reactions:Reaction list) =
        let addCatalystsToComponents components =
            List.map (fun (n,_) -> (n, 1)) catalysts @ components
        in List.map (fun (reactants, rate, products) ->
                addCatalystsToComponents reactants, rate, addCatalystsToComponents products) reactions

    let private applyCatalystsToCmds catalysts commands =
        let rec applyCatalystsToCmd cmd :Command=
            match cmd with
            | Normal (r,s) -> Normal (addCatalysts catalysts r, s)
            | Cmp cmds -> Nested (List.map applyCatalystsToCmd cmds)
            | Nested cmds -> Nested (List.map applyCatalystsToCmd cmds)
        in List.map applyCatalystsToCmd commands

    let ifGt (commands:Command list) =
        let catalysts = [cmpXEgtY; cmpYEltX]
        in applyCatalystsToCmds catalysts commands
            |> Nested

    let ifLt (commands:Command list) =
        let catalysts = [cmpXEltY; cmpYEgtX]
        in applyCatalystsToCmds catalysts commands
            |> Nested

    let ifEq (commands:Command list) =
        let catalysts = [cmpXEgtY; cmpYEgtX]
        in applyCatalystsToCmds catalysts commands
            |> Nested

    let ifGe (commands:Command list) =
        let catalysts = [cmpXEgtY]
        in applyCatalystsToCmds catalysts commands
            |> Nested

    let ifLe (commands:Command list) =
        let catalysts = [cmpYEgtX]
        in applyCatalystsToCmds catalysts commands
            |> Nested


    let clock phases =

        let phaseIndices = [0..phases-1]
        let clcs = phaseIndices |> List.map (Clock.clockSpecies phases)

        let clockReaction i =
            let i1, i2 = (i) % phases, (i+1) % phases
            in ([name clcs[i1], 1; name clcs[i2], 1], 1.0, [name clcs[i2], 2])

        let reactions = phaseIndices |> List.map clockReaction
        fromReactionAndSpecies reactions clcs
