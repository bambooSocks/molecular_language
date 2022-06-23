(*
    Author: Christopher Acosta
*)

open ChemicalReactions.Simulator
open FParsec
open Parser.Parser
open ChemicalReactions.Samples
open Drawing 
open ChemicalReactions.modulesToReactions
open Parser.Types

// ********************************

let crn1res = simulateN crn1 crn1s0 0.01 1500
let crn2res = simulateN crn9 crn9s0 0.01 5000
let ldcrnres = simulateN ldcrn ldcrns0 0.01 1000
let addcrnres = simulateN addcrn addcrns0 0.01 1000
let subcrnres = simulateN subcrn subcrns0 0.01 500
let divcrnres = simulateN divcrn divcrns0 0.01 1000
let sqrtcrnres = simulateN sqrtcrn sqrtcrns0 0.01 1000
let crn7res = simulateN crn7 crn7s0 0.001 1000

let speciesConcs species states = List.map (Map.find species) states
let drawStates res =
    drawSteps (List.map (fun s -> (speciesConcs s res, s)) (Seq.toList <| Map.keys res[0]))
let zeroconcs xs = List.map (fun s -> (s, 0.0)) xs

let reactionsOutput reactions =
    let reactionToStr (Rxn (rs, ps, k)) =
        let lhs = concatStrs <| intersperse " + " rs
        let rhs = concatStrs <| intersperse " + " ps
        "rxn[" + lhs + ", " + rhs + ", " + string k + "]"
    concatStrs <| intersperse ",\n" (List.map reactionToStr reactions)

let parseSimulateDraw src =
    let parseResult = runRxnParser src
    match parseResult with
    | Success (rxns, _, _) ->
        printfn "Parsing succeeded:%A" rxns
        let states = simulateN rxns crn1s0 0.01 1000
        drawStates states
    | Failure (err, _, _) -> printfn "PARSING FAILED:\n%A" err
// ********************************


// ********************************
// Compiling
let gcd =
    """
    crn = {
        conc[a,32],
        conc[b,12],
        step[{
            ld [a, atmp],
            ld [b, btmp],
            cmp[a,b]
        }],
        step[{
            ifGT[{ sub[atmp,btmp,a] }],
            ifLT[{ sub[btmp,atmp,b] }]
        }]
    };
    """
let (Success (ast, _, _)) = runCrnParser gcd
let (reactions, oscCount) = toReactionNetwork ast
//printfn "%A" reactions
//printfn "%A" (reactionsOutput reactions)
// ********************************

// ********************************
// Parsing GCD reactions
let rs' = """
rxn[osc3 + a, osc3 + a + atmp, 1],
rxn[osc3 + atmp, osc3, 1],
rxn[osc3 + b, osc3 + b + btmp, 1],
rxn[osc3 + btmp, osc3, 1],
rxn[osc3 + cmpGT + b, osc3 + cmpLT + b, 1],
rxn[osc3 + cmpLT + a, osc3 + cmpGT + a, 1],
rxn[osc6 + cmpGT + cmpLT, osc6 + cmpLT + cmpB, 1],
rxn[osc6 + cmpB + cmpLT, osc6 + cmpLT + cmpLT, 1],
rxn[osc6 + cmpLT + cmpGT, osc6 + cmpGT + cmpB, 1],
rxn[osc6 + cmpB + cmpGT, osc6 + cmpGT + cmpGT, 1],
rxn[osc9 + cmpLT + btmp, osc9 + cmpLT + btmp + b, 1],
rxn[osc9 + cmpLT + atmp, osc9 + cmpLT + atmp + hbtmpatmpb, 1],
rxn[osc9 + cmpLT + b, osc9 + cmpLT, 1],
rxn[osc9 + cmpLT + b + hbtmpatmpb, osc9 + cmpLT, 1],
rxn[osc9 + cmpGT + atmp, osc9 + cmpGT + atmp + a, 1],
rxn[osc9 + cmpGT + btmp, osc9 + cmpGT + btmp + hatmpbtmpa, 1],
rxn[osc9 + cmpGT + a, osc9 + cmpGT, 1],
rxn[osc9 + cmpGT + a + hatmpbtmpa, osc9 + cmpGT, 1],
rxn[osc1 + osc2, osc2 + osc2, 1],
rxn[osc2 + osc3, osc3 + osc3, 1],
rxn[osc3 + osc4, osc4 + osc4, 1],
rxn[osc4 + osc5, osc5 + osc5, 1],
rxn[osc5 + osc6, osc6 + osc6, 1],
rxn[osc6 + osc7, osc7 + osc7, 1],
rxn[osc7 + osc8, osc8 + osc8, 1],
rxn[osc8 + osc9, osc9 + osc9, 1],
rxn[osc9 + osc1, osc1 + osc1, 1]
"""
let ( Success (res, _, _) )= runRxnParser rs'
printfn "%A" res
// ********************************

// ********************************
// Simulating

printfn "%A" (Map.toList (computeInitialState [("a", 32.0); ("b", 12.0)]))


let initialState' =
    Map.ofList [("a", 32.0); ("atmp", 0.0); ("b", 12.0); ("btmp", 0.0); ("cmpB", 0.0);
    ("cmpGT", 0.5); ("cmpLT", 0.5); ("hatmpbtmpa", 0.0); ("hbtmpatmpb", 0.0);
    ("osc1", 1.0); ("osc2", 0.01); ("osc3", 0.01); ("osc4", 0.01); ("osc5", 0.01);
    ("osc6", 0.01); ("osc7", 0.01); ("osc8", 0.01); ("osc9", 0.01)]

let crnresult = simulateN reactions initialState' 0.1 5000
//drawStates crnresult
// ********************************
    
// ********************************
// Simulating CRNs from paper
// let crn1res = simulateN crn1 crn1s0 0.01 1500
// let crn2res = simulateN crn9 crn9s0 0.01 5000
// let ldcrnres = simulateN ldcrn ldcrns0 0.01 1000
// let addcrnres = simulateN addcrn addcrns0 0.01 1000
// let subcrnres = simulateN subcrn subcrns0 0.01 500
// let divcrnres = simulateN divcrn divcrns0 0.01 1000
// let sqrtcrnres = simulateN sqrtcrn sqrtcrns0 0.01 1000
// let crn7res = simulateN crn7 crn7s0 0.001 1000
//let crn9res = simulateN crn9 crn9s0 0.01 10000


//drawStates crn9res
//drawStates crn1res
//drawStates crn2res
//drawStates ldcrnres
//drawStates addcrnres
//drawStates subcrnres
//drawStates divcrnres
//drawStates sqrtcrnres
//printfn "%A" crn7res
// ********************************
