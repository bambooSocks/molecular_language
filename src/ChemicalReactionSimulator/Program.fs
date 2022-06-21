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
let parseResult = parseSimulateDraw rs