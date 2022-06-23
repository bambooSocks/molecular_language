(*
    Author: Christopher Acosta
*)

open ChemicalReactions.Simulator
open ChemicalReactions.Samples
open Drawing

// Simulation and visualization of CRNs from paper

let crn1res = simulateN crn1 crn1s0 0.01 1500
// let crn2res = simulateN crn9 crn9s0 0.01 5000
// let ldcrnres = simulateN ldcrn ldcrns0 0.01 1000
// let addcrnres = simulateN addcrn addcrns0 0.01 1000
// let subcrnres = simulateN subcrn subcrns0 0.01 500
// let divcrnres = simulateN divcrn divcrns0 0.01 1000
// let sqrtcrnres = simulateN sqrtcrn sqrtcrns0 0.01 1000
// let crn7res = simulateN crn7 crn7s0 0.001 1000
// let crn9res = simulateN crn9 crn9s0 0.01 10000

drawStates crn1res
//drawStates crn2res
//drawStates ldcrnres
//drawStates addcrnres
//drawStates subcrnres
//drawStates divcrnres
//drawStates sqrtcrnres
//printfn "%A" crn7res
//drawStates crn9res
