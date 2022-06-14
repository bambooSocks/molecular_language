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
printfn "%A" res

// ----------------------------------
// Draw functions
// draw: (function, sting) list -> min -> max ->
draw [((fun x -> x*x), "first"); ((fun x -> x*x*x), "second")] 0.0 10.0

//Draw steps
//drawSteps: (float list * string) list -> 
drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

// ----------------------------------
open ChemicalReactions.Simulator

printfn "%A" s0
let crn1 =
    let rxn1 = Rxn (["a"; "b"], ["a"; "b"; "c"], 1)
    let rxn2 = Rxn (["c"], [], 1)
    [rxn1; rxn2]
printfn "%A" crn1
let resStates =
    runSteps crn1 s0 "c" 0.01 |>
    Seq.take 1500 |>
    Seq.toList
let speciesConcs species states = List.map (Map.find species) states
drawSteps
    [(speciesConcs "a" resStates, "a");
     (speciesConcs "b" resStates, "b");
     (speciesConcs "c" resStates, "c");]
