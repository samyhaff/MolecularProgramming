namespace ChemicalEngine

module Modules =
    open Reaction

    // todo add input restrictions

    let ld A B =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A], rate, [name A; name B]);
            ([name B], rate, []);
        ]
        toCRN reactions [A;B]

    let add A B C =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A], rate, [name A; name C]);
            ([name B], rate, [name B; name C]);
            ([name C], rate, [])
        ]

        toCRN reactions [A;B;C]

    let sub A B C =
        let rate = 1.0

        // todo manage aux chemical names: H
        let H = S("H", 0.0, 1)
        let reactions :Reaction list= [
            ([name A], rate, [name A; name C]);
            ([name B], rate, [name B; name H]);
            ([name C], rate, [])
            ([name C; name H], rate, [])
        ]

        toCRN reactions [A;B;C;H]

    let mul A B C =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A; name B], rate, [name A; name B; name C]);
            ([name C], rate, [])
        ]

        toCRN reactions [A;B;C]

    let div A B C =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name A], rate, [name A; name C]);
            ([name B; name C], rate, [name B])
        ]

        toCRN reactions [A;B;C]


    let sqrt A B =
        let reactions :Reaction list= [
            ([name A], 1.0, [name A; name B]);
            ([name B; name B], 0.5, [])
        ]

        toCRN reactions [A;B]