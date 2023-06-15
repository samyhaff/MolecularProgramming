namespace ChemicalEngine

module Modules =
    open Reaction

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
        toCRN reactions [A;B]

    let add A B C =
        let rate = 1.0
        let reactions = mapReactions [
            ([A], rate, [A; C]);
            ([B], rate, [B; C]);
            ([C], rate, [])
        ]

        toCRN reactions [A;B;C]

    let sub A B C =
        let rate = 1.0

        // todo manage aux chemical names: H
        let H = ("H", 0.0)
        let reactions = mapReactions [
            ([A], rate, [A; C]);
            ([B], rate, [B; H]);
            ([C], rate, [])
            ([C; H], rate, [])
        ]

        toCRN reactions [A;B;C;H]

    let mul A B C =
        let rate = 1.0
        let reactions = mapReactions [
            ([A; B], rate, [A; B; C]);
            ([C], rate, [])
        ]

        toCRN reactions [A;B;C]

    let div A B C =
        let rate = 1.0
        let reactions = mapReactions [
            ([A], rate, [A; C]);
            ([B; C], rate, [B])
        ]

        toCRN reactions [A;B;C]


    let sqrt A B =
        let reactions :Reaction list= [
            ([name A, 1], 1.0, [name A, 1; name B, 1]);
            ([name B, 2], 0.5, [])
        ]

        toCRN reactions [A;B]

    // todo requires sequential execution for phase two
    // let cmp X Y =
    //     let rate = 1.0

    //     // phase 1:
    //     let gt = ("cmpGt", 0.5)
    //     let lt = ("cmpLt", 0.5)
    //     let reactions = mapReactions [
    //         ([gt; Y], rate, [lt; Y]);
    //         ([lt; X], rate, [gt; X]);
    //     ]

    //     // phase 2:
    //     let B = ("cmpTmp", 0.0)
    //     let reactions = mapReactions [
    //         ([gt; lt], rate, [lt; B ]);
    //         ([B ; lt], rate, [lt; lt]);
    //         ([lt; gt], rate, [gt; B ]);
    //         ([B ; gt], rate, [gt; gt]);
    //     ]

    //     toCRN reactions [X;Y;gt;lt]

    // todo why is it not oscillating???
    let clock phases =
        let initClockSpecies i = if i = 1 || i = phases then ($"clc{i}", 1.0) else ($"clc{i}", 0.0)
        let clcs = [1..phases] |> List.map initClockSpecies

        let clockReaction i =
            let i1, i2 = (i - 1) % phases, (i) % phases
            in ([name clcs[i1], 1; name clcs[i2], 1], 1.0, [name clcs[i2], 2])

        let reactions = [1..phases] |> List.map clockReaction
        toCRN reactions clcs
