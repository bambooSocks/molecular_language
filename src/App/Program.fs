(*
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

let parseCrnAndCheck src = 
    let parserResult = runCrnParser src //Parse program
    match parserResult with
    | Success (ast, _, _) ->
        let checkResult = check ast
        printfn "\nParsing succeeded. AST:\n%A\n" ast
        match snd checkResult with
        | [] -> Some ast
        | errs ->
            printErrors errs
            None
    | Failure (err, _, _) ->
        printfn "PARSING FAILED:\n%s\n" err
        None

[<EntryPoint>]
let main a =
    let args = List.ofArray a

    if List.contains "interpreter" args then
        // Parse and interpret a CRN program
        match parseCrnAndCheck gcd with
        | Some ast ->
            let resultingStates = interpret (Map.ofList []) ast //Interpret program
            printfn "Resulting sequence of states:\n%A\n" resultingStates
            drawStates (Seq.toList (Seq.take 10 resultingStates)) // Visualize program
        | None -> ()

    else if List.contains "simulator" args then
        match runRxnParser gcdReactions with
        | Success (reactions, _, _) ->
            printfn "Parsed reactions successfully."
            let oscCount = computeOscCount (speciesFromRxns reactions)
            let initialState = computeInitialState reactions [("a", 32.0); ("b", 12.0)] oscCount
            let statesSeq = simulateN reactions initialState 0.01 5000
            drawStates statesSeq
        | Failure (err, _, _) -> printfn "Error when parsing reactions:\n%s" err

    else if List.contains "compile" args then
        match parseCrnAndCheck gcd with
        | Some ast ->
            let (reactions, _) = toReactionNetwork ast
            let reactionsS = reactionsOutput reactions
            printfn "Compilation of reactions successful:\n%s" reactionsS
        | None -> ()

    else printfn "Wrong command"
    0
