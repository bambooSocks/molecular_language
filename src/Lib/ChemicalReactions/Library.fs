namespace ChemicalReactions

open Parser.Types

module Simulator = 
    type TRxn = Rxn of string list * string list * float

    let s0 = Map.ofList [("a", 6.0); ("b", 2.0); ("c", 0.0)]

    let sum = List.fold (+) 0
    let product = List.fold (*) 1.0

    let stateGet state species = Map.find species state
    let stateAddConc state species  dConc =
        let oldConc = stateGet state species
        Map.add species (oldConc + dConc) state

    let multiplicity species expr = List.length (List.filter (fun x -> x = species) expr)

    let speciesNetChange species (Rxn (reactants, products, _)) = 
        multiplicity species products - multiplicity species reactants

    let concNetChange crn state species dt =
        let calcTerm (rxn as Rxn (reactants, _, rate)) =
            let sNetChange = float (speciesNetChange species rxn)
            let multiplicityProduct = product (List.map (fun s -> pown (stateGet state s) (multiplicity s reactants)) reactants)
            rate * sNetChange * multiplicityProduct * dt
        List.sum (List.map calcTerm crn)

    let step crn state species dt =
        let netChange = concNetChange crn state species dt
        stateAddConc state species netChange

    let rec runNSteps crn state species dt = function
        | 0 -> state
        | n ->
            let nextState = step crn state species dt
            runNSteps crn nextState species dt (n-1)

    let rec runSteps crn state species dt =
        seq {
            let state' = step crn state species dt
            yield step crn state species dt 
            yield! runSteps crn state' species dt }