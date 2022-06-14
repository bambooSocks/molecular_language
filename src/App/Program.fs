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

// Draw functions
// draw: (float list * string) list -> 
draw [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

//Draw steps
// drawSteps: (float list * string) list -> 
drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]
