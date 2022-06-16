open Parser.Parser

open Drawing
open TypeCheck.TypeCheck
open App.Examples

let parseCheckExecute src =
    let parserResult = runCrnParser src
    match parserResult with
    | FParsec.CharParsers.ParserResult.Success (ast, _, _) ->
        let checkResult = check ast
        printfn "Parsing succeeded. AST:\n%A" ast
        match snd checkResult with
        | [] -> printfn "No errors, yay! *Interpret*"
        | errs -> printErrors errs
    | FParsec.CharParsers.ParserResult.Failure (err, _, _) -> printfn "PARSING FAILED:\n%s" err

// Parse a CRN program
parseCheckExecute gcd

// let innerres =
//     match res with
//     | FParsec.CharParsers.ParserResult.Success (r, _, _) -> "yay"
//     | FParsec.CharParsers.ParserResult.Failure (s, _, _) -> "nay"
// printfn "%A" res
// printfn "%A" innerres

// let innerres =
//     function
//     | FParsec.CharParsers.ParserResult.Success (r, _, _) -> Some r
//     | FParsec.CharParsers.ParserResult.Failure _ -> None

// let ast = (innerres res).Value

// let ch = check ast

// printErrors (snd ch)

// ----------------------------------
// Draw functions
// draw: (float list * string) list -> 
//draw [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

//Draw steps
//drawSteps: (float list * string) list -> 
//drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

// ----------------------------------
open ChemicalReactions
open ChemicalReactions.Simulator
open ChemicalReactions.Samples

let speciesConcs species states = List.map (Map.find species) states
let drawStates res =
    drawSteps (List.map (fun s -> (speciesConcs s res, s)) (Seq.toList <| Map.keys res[0]))

let crn1res = simulateN crn1 crn1s0 0.01 1500
let crn2res = simulateN crn9 crn9s0 0.01 5000
let ldcrnres = simulateN ldcrn ldcrns0 0.01 1000
let addcrnres = simulateN addcrn addcrns0 0.01 1000
let subcrnres = simulateN subcrn subcrns0 0.01 500
let divcrnres = simulateN divcrn divcrns0 0.01 1000
let sqrtcrnres = simulateN sqrtcrn sqrtcrns0 0.01 1000
let crn7res = simulateN crn7 crn7s0 0.001 1000

//drawStates crn1res
//drawStates crn2res
//drawStates ldcrnres
//drawStates addcrnres
//drawStates subcrnres
//drawStates divcrnres
//drawStates sqrtcrnres
printfn "%A" crn7res
