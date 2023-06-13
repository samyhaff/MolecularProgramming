open Parser

test pconc "conc [atmp, 1.0]"
test pload "ld [a, atmp]"
test padd "add [a, atmp, b]"
test psub "sub [a, atmp, b]"
test pmul "mul [a, atmp, b]"
test pdiv "div [a, atmp, b]"
test psqrt "sqrt [a, atmp]"
test pcmp "cmp [a, b]"
