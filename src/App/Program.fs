﻿(*
    Authors: Christopher Acosta, Matej Majtan
*)

open Parser.Parser
open Interpreter.Interpreter
open Drawing
open TypeCheck.TypeCheck
open App.Examples
open FParsec
open ChemicalReactions.modulesToReactions
open ChemicalReactions.Simulator

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


printfn "osccount %A" (computeOscCount [ "osc1"; "osc2"; "osc3"])
[<EntryPoint>]
let main a =
    let args = List.ofArray a

    if List.contains "interpreter" args then
        // Parse and interpret a CRN program
        parseCheckExecute gcd
    else if List.contains "simulator" args then
        match runRxnParser gcdReactions with
        | Success (reactions, _, _) ->
            printfn "Parsed reactions successfully."
            let oscCount = computeOscCount (speciesFromRxns reactions)
            let initialState = computeInitialState reactions [("a", 4.0); ("b", 2.0)] oscCount
            let statesSeq = simulateN reactions initialState 0.05 10000
            drawStates statesSeq
        | Failure (err, _, _) -> printfn "Error when parsing reactions:\n%s" err
    else if List.contains "compile" args then

        printfn "Wrong command"

    0
