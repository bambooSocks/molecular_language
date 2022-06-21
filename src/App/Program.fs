open Parser.Parser
open Interpreter.Interpreter
open Drawing
open TypeCheck.TypeCheck
open App.Examples
open FParsec

let parseCheckExecute src =
    let parserResult = runCrnParser src //Parse program

    match parserResult with
    | Success (ast, _, _) ->
        let checkResult = check ast
        printfn "\nParsing succeeded. AST:\n%A\n" ast

        match snd checkResult with
        | [] ->
            let resultingStates = interpret (Map.ofList []) ast //Interpret program
            printfn "Resulting sequence of states:\n%A\n" resultingStates

            drawStates (Seq.toList (Seq.take 10 resultingStates)) //Draw program

        | errs -> printErrors errs
    | Failure (err, _, _) -> printfn "PARSING FAILED:\n%s\n" err

// Parse and interpret a CRN program
parseCheckExecute gcd 


// -------------- DRAWING EXAMPLES --------------------
// Draw smoothly connecting lines
// draw: (float list * string) list ->
// draw [([1.0; 2.0; 3.0; 4.0; 5.0], "first") ; ([4.0; 3.0; 2.0; 1.0], "second")]

// Draw steps
// drawSteps: (float list * string) list ->
// drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "first") ; ([4.0; 3.0; 2.0; 1.0], "second")]

// Draw States
// drawStates: Map<Species * float) list ->
// drawStates (Seq.toList (Seq.take 10 seq{Map<Species,, float>}))
// ----------------------------------------------------
