open Parser.Parser

open Drawing

// Sample CRN programs
let gcd = """
crn = {
conc[a,32],
conc[b,12],
step[{
    ld [a, atmp],
    ld [b, btmp],
    cmp[a,b]
}],
step[{
    ifGT[{ sub[atmp,btmp,a] }],
    ifLT[{ sub[btmp,atmp,b] }]
}]
};
"""
let small = "crn = { conc[a, 32], conc[b, 12] };"
let small2 = "crn = { conc[a, 32], conc[b, 12], step[{ ifGT[{ sub[atmp,btmp,a] }] }] };"

// Parse a CRN program
let res = runCrnParser gcd
printf "%A" res

draw [((fun x -> x*x), "first"); ((fun x -> x*x*x), "second")]