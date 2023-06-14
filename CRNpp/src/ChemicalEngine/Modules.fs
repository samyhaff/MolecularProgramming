namespace ChemicalEngine

module Modules =
    open Reaction
    let mul c1 c2 c3 =
        let rate = 1.0
        let reactions :Reaction list= [
            ([name c1; name c2], rate, [name c1; name c2; name c3]);
            ([name c3], rate, [])
        ]

        toCRN reactions [c1;c2;c3]
