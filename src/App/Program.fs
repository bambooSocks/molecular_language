open Parser.Parser

open Drawing
open TypeCheck.TypeCheck
open App.Examples

// Parse a CRN program
let res = runCrnParser gcd
printfn "%A" res

let innerres =
    function
    | FParsec.CharParsers.ParserResult.Success (r, _, _) -> Some r
    | FParsec.CharParsers.ParserResult.Failure _ -> None

let ast = (innerres res).Value

let ch = check ast

printErrors (snd ch)

// ----------------------------------
// Draw functions
// draw: (float list * string) list ->
// draw [ ([ 1.0; 2.0; 3.0; 4.0; 5.0 ], "fisrt")
//        ([ 4.0; 3.0; 2.0; 1.0 ], "second") ]

//Draw steps
//drawSteps: (float list * string) list ->
// drawSteps [ ([ 1.0; 2.0; 3.0; 4.0; 5.0 ], "fisrt")
//             ([ 4.0; 3.0; 2.0; 1.0 ], "second") ]

// ----------------------------------
// open ChemicalReactions.Simulator

// printfn "%A" s0

// let crn1 =
//     let rxn1 = Rxn([ "a"; "b" ], [ "a"; "b"; "c" ], 1)
//     let rxn2 = Rxn([ "c" ], [], 1)
//     [ rxn1; rxn2 ]

// printfn "%A" crn1

// let resStates =
//     runSteps crn1 s0 "c" 0.01
//     |> Seq.take 1500
//     |> Seq.toList

// let speciesConcs species states = List.map (Map.find species) states

// drawSteps [ (speciesConcs "a" resStates, "a")
//             (speciesConcs "b" resStates, "b")
//             (speciesConcs "c" resStates, "c") ]
