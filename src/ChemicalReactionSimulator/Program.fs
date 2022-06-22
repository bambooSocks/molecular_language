(*
    Author: Christopher Acosta
*)

open ChemicalReactions.Simulator
open FParsec
open Parser.Parser
open ChemicalReactions.Samples
open Drawing 

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

let oscs x n = List.map (fun i -> (x + string i, 1.0 + 0.1 * float i)) [1..n]
let zeroconcs xs = List.map (fun s -> (s, 0.0)) xs

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

open ChemicalReactions.modulesToReactions
open Parser.Types

let (Success (ast, _, _)) = runCrnParser gcd
//printfn "%A" ast


let speciesFromRxns rxns =
    let speciesFromRxn (Rxn (rs, ps, _)) = Set.ofList (rs @ ps)
    List.fold (fun acc rxn -> Set.union acc (speciesFromRxn rxn)) Set.empty rxns
let (reactions, _) = toReactionNetwork ast
let species = Set.toList (speciesFromRxns reactions)
let initialState =
    let m = Map.ofList (List.map (fun species -> (species, 0.5)) species)
    List.fold (fun m (s, c) -> Map.add s c m) m ([("a", 32.0); ("b", 12.0)] @ oscs "osc" 9)
//printfn "%A" species
printfn "%A" (Map.toList initialState)

let reactionsWithOscs = reactions @ genOscCrn "osc" 9
printfn "%A" reactionsWithOscs

let initialState' =
    Map.ofList [("a", 32.0); ("atmp", 0.0); ("b", 12.0); ("btmp", 0.0); ("cmpB", 0.0);
    ("cmpGT", 0.5); ("cmpLT", 0.5); ("hatmpbtmpa", 0.0); ("hbtmpatmpb", 0.0);
    ("osc1", 1.0); ("osc2", 0.01); ("osc3", 0.01); ("osc4", 0.01); ("osc5", 0.01);
    ("osc6", 0.01); ("osc7", 0.01); ("osc8", 0.01); ("osc9", 0.01)]

let crnresult = simulateN reactionsWithOscs initialState' 0.1 5000
drawStates crnresult
