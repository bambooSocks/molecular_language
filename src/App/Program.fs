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
