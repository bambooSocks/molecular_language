(*
    Authors: Christopher Acosta, Matej Majtan
*)

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

            drawStates (Seq.toList (Seq.take 10 resultingStates)) // Visualize program

        | errs -> printErrors errs
    | Failure (err, _, _) -> printfn "PARSING FAILED:\n%s\n" err


[<EntryPoint>]
let main a =
    let args = List.ofArray a

    if List.contains "interpreter" args then
        // Parse and interpret a CRN program
        parseCheckExecute gcd
    else if List.contains "simulator" args then
        printfn "Running sim"
    else
        printfn "Wrong command"

    0
