﻿open Parser.Parser
open Interpreter.Interpreter
open Drawing
open TypeCheck.TypeCheck
open App.Examples
open ChemicalReactions.modulesToReactions


let parseCheckExecute src =
    let parserResult = runCrnParser src
    match parserResult with
    | FParsec.CharParsers.ParserResult.Success (ast, _, _) ->
        let checkResult = check ast
        printfn "Parsing succeeded. AST:\n%A" ast
        match snd checkResult with
        | [] ->
            printfn "No errors, yay!"
            let resultingStates = interpret (Map.ofList []) ast
            printfn "Resulting sequence of states:\n%A" resultingStates
        | errs -> printErrors errs
    | FParsec.CharParsers.ParserResult.Failure (err, _, _) -> printfn "PARSING FAILED:\n%s" err

// Parse a CRN program
parseCheckExecute gcd

let network = toReactionNetwork ast
printf "\n\n%A" network

// ----------------------------------
// Draw functions
// draw: (float list * string) list -> 
//draw [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

//Draw steps
//drawSteps: (float list * string) list -> 
//drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

// ----------------------------------
