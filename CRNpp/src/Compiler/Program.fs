open Parser

test pcrn "crn = {
    conc[a,8.0],
    conc[b,2.0],
    step[
        ld[a, atmp],
        ld[b, btmp],
        cmp[a,b]
    ],
    step[
        ifGT[sub[atmp,btmp,a]],
        ifLT[sub[btmp,atmp,b]]
    ]
}"
