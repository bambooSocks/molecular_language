open Parser.Parser
open Interpreter.Interpreter
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

// Interpret a CRN program
let innerresult = 
    match res with 
    | FParsec.CharParsers.ParserResult.Success (result,_,_) -> result
    | FParsec.CharParsers.ParserResult.Failure (_,_,_) -> failwith "parsing failed"

printf "%A" (interpret (Map.ofList []) innerresult) 

// Draw functions
// draw: (function, sting) list -> min -> max ->
draw [((fun x -> x*x), "first"); ((fun x -> x*x*x), "second")] 0.0 10.0

//Draw steps
// drawSteps: (float list * string) list -> 
drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]
