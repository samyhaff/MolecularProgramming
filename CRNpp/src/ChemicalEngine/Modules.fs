namespace ChemicalEngine

module Modules =
    open Reaction

    // todo add input restrictions

    let ld A B =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A, 1], rate, [name A, 1; name B, 1]);
            ([name B, 1], rate, []);
        ]
        toCRN reactions [A;B]

    let add A B C =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A, 1], rate, [name A, 1; name C, 1]);
            ([name B, 1], rate, [name B, 1; name C, 1]);
            ([name C, 1], rate, [])
        ]

        toCRN reactions [A;B;C]

    let sub A B C =
        let rate = 1.0

        // todo manage aux chemical names: H
        let H = ("H", 0.0)
        let reactions :Reaction list= [
            ([name A, 1], rate, [name A, 1; name C, 1]);
            ([name B, 1], rate, [name B, 1; name H, 1]);
            ([name C, 1], rate, [])
            ([name C, 1; name H, 1], rate, [])
        ]

        toCRN reactions [A;B;C;H]

    let mul A B C =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A, 1; name B, 1], rate, [name A, 1; name B, 1; name C, 1]);
            ([name C, 1], rate, [])
        ]

        toCRN reactions [A;B;C]

    let div A B C =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A, 1], rate, [name A, 1; name C, 1]);
            ([name B, 1; name C, 1], rate, [name B, 1])
        ]

        toCRN reactions [A;B;C]


    let sqrt A B =
        let reactions :Reaction list= [
            ([name A, 1], 1.0, [name A, 1; name B, 1]);
            ([name B, 2], 0.5, [])
        ]

        toCRN reactions [A;B]