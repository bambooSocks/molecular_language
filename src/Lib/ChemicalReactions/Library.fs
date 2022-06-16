namespace ChemicalReactions

open Parser.Types

type TRxn = Rxn of string list * string list * float

module Simulator = 
    //let s0 = 

    let sum = List.fold (+) 0
    let product = List.fold (*) 1.0

    let stateGet state species = 1

    let multiplicity species expr = List.length (List.filter (fun x -> x = species) expr)

    let speciesNetChange species (Rxn (reactants, products, _)) = 
        multiplicity species products - multiplicity species reactants

    let concNetChange crn state species dt =
        let calcTerm (rxn as Rxn (reactants, _, rate)) =
            let sNetChange = float (speciesNetChange species rxn)
            let multiplicityProduct = product (List.map (fun s -> pown (stateGet state s) (multiplicity s reactants)) reactants)
            rate * sNetChange * multiplicityProduct * dt
        List.sum (List.map calcTerm crn)