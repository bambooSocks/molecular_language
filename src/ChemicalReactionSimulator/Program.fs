(*
    Author: Christopher Acosta
*)

open ChemicalReactions.Simulator
open FParsec
open Parser.Parser
open ChemicalReactions.Samples
open Drawing 

<<<<<<< HEAD
let crn1res = simulateN crn1 crn1s0 0.01 1500
let crn2res = simulateN crn9 crn9s0 0.01 5000
let ldcrnres = simulateN ldcrn ldcrns0 0.01 1000
let addcrnres = simulateN addcrn addcrns0 0.01 1000
let subcrnres = simulateN subcrn subcrns0 0.01 500
let divcrnres = simulateN divcrn divcrns0 0.01 1000
let sqrtcrnres = simulateN sqrtcrn sqrtcrns0 0.01 1000
let crn7res = simulateN crn7 crn7s0 0.001 1000
=======
let speciesConcs species states = List.map (Map.find species) states
let drawStates res =
    drawSteps (List.map (fun s -> (speciesConcs s res, s)) (Seq.toList <| Map.keys res[0]))

// let crn1res = simulateN crn1 crn1s0 0.01 1500
// let crn2res = simulateN crn9 crn9s0 0.01 5000
// let ldcrnres = simulateN ldcrn ldcrns0 0.01 1000
// let addcrnres = simulateN addcrn addcrns0 0.01 1000
// let subcrnres = simulateN subcrn subcrns0 0.01 500
// let divcrnres = simulateN divcrn divcrns0 0.01 1000
// let sqrtcrnres = simulateN sqrtcrn sqrtcrns0 0.01 1000
// let crn7res = simulateN crn7 crn7s0 0.001 1000
>>>>>>> 7826df3 (working example for conditionals)


let oscs x n = List.map (fun i -> (x + string i, 1.0 / float i)) [1..n]

let condcrns0 = Map.ofList <| [("x", 20.0); ("y", 80.0); ("xGTy", 0.5); ("xLTy", 0.5); ("b", 0.0); ("c1", 0.1); ("c2", 0.1); ("h", 0.0)] @ oscs "osc" 6
open Parser.Types
let addcrn =
        let rxn1 = Rxn (["x"], ["x"; "c1"], 1)
        let rxn2 = Rxn (["y"], ["y"; "c1"], 1)
        let rxn3 = Rxn (["c1"], [], 1)
        [rxn1; rxn2; rxn3]
let subcrn =
        let rxn1 = Rxn (["y"], ["y"; "c2"], 1)
        let rxn2 = Rxn (["x"], ["x"; "h"], 1)
        let rxn3 = Rxn (["c2"], [], 1)
        let rxn4 = Rxn (["c2"; "h"], [], 1)
        [rxn1; rxn2; rxn3; rxn4]
// let ldx =
//     let rxn1 = Rxn (["x"], ["x"; "xtemp"], 1)
//     let rxn2 = Rxn (["xtemp"], [], 1)
//     [rxn1; rxn2]
// let ldy =
//     let rxn1 = Rxn (["y"], ["y"; "xtemp"], 1)
//     let rxn2 = Rxn (["ytemp"], [], 1)
//     [rxn1; rxn2]
let condcrn = addOscs crn7 "osc3" @ addOscs crn8 "osc6" @ genOscCrn "osc" 6 @ addOscs addcrn "xGTy" @ addOscs subcrn "xLTy"
printfn "%A" (Map.toList condcrns0)
printfn "%A" condcrn
printfn "%A" oscs
let condcrnres = simulateN condcrn condcrns0 0.01 1000
drawStates condcrnres
printfn "%A" (List.take 3 <| List.map Map.toList condcrnres)


//printfn "%A" (genOscCrn "osc" 6)
// printfn "%A" (addOscs crn7 "osc3")
// printfn "%A" (addOscs crn8 "osc6")
//drawStates crn1res
//drawStates crn2res
//drawStates ldcrnres
//drawStates addcrnres
//drawStates subcrnres
//drawStates divcrnres
//drawStates sqrtcrnres
//printfn "%A" crn7res

let parseSimulateDraw src =
    let parseResult = runRxnParser src
    match parseResult with
    | Success (rxns, _, _) ->
        printfn "Parsing succeeded:%A" rxns
        let states = simulateN rxns crn1s0 0.01 1000
        drawStates states
    | Failure (err, _, _) -> printfn "PARSING FAILED:\n%A" err

let rs = "rxn[a + b, a + b + c, 1], rxn[c, , 1]"
//let parseResult = parseSimulateDraw rs