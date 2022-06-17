open Parser.Parser
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

let speciesConcs species states = List.map (Map.find species) states
let drawStates res =
    drawSteps (List.map (fun s -> (speciesConcs s res, s)) (Seq.toList <| Map.keys res[0]))


// Parse a CRN program
parseCheckExecute gcd

let initial = Map.ofList [("a", 80.0); ("b", 20.0); ("agtb", 0.5); ("altb", 0.5); ("atmp", 0.0); ("btmp", 0.0); ("H", 0.0)]

let temp = match runCrnParser gcd with
            | FParsec.CharParsers.ParserResult.Success (ast, _, _) -> ast
           
let network = toReactionNetwork temp
printf "\n\n%A" network

open ChemicalReactions.Simulator
let sim = simulateN network initial 0.01 2000

drawStates sim


// ----------------------------------
// Draw functions
// draw: (float list * string) list -> 
//draw [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

//Draw steps
//drawSteps: (float list * string) list -> 
//drawSteps [([1.0; 2.0; 3.0; 4.0; 5.0], "fisrt") ; ([4.0; 3.0; 2.0; 1.0], "second")]

// ----------------------------------
