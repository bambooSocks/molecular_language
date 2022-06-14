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


module modulesToReactions =
    open Parser.Types

    let toReaction acc (reaction: TCommand) = 
        match reaction with
        | Module(reac) -> 
            match reac with
            | Ld(x, y)      -> Rxn([x], [x;y], 1.0)     :: Rxn([y], [], 1.0)        :: acc
            | Add(x, y, z)  -> Rxn([x], [x;z], 1.0)     :: Rxn([y], [y;z], 1.0)     :: Rxn([z], [], 1.0) :: acc
            | Sub(x, y, z)  -> Rxn([x], [x;z], 1.0)     :: Rxn([y], [y;"H"], 1.0)   :: Rxn([z], [], 1.0) :: Rxn([z;"H"], [], 1.0) :: acc
            | Mul(x, y, z)  -> Rxn([x;y], [x;y;z], 1.0) :: Rxn([z], [], 1.0)        :: acc
            | Div(x, y, z)  -> Rxn([x], [x;z], 1.0)     :: Rxn([y;z], [y], 1.0)     :: acc
            | Sqrt(x, y)    -> Rxn([x], [x;y], 1.0)     :: Rxn([y;y], [], 0.5)      :: acc
            | _ -> acc
        | _ -> acc

    let toReactionNetwork list = 
        let rec toReactionNetwork' acc elem = 
            match elem with
            | Step(modules) -> acc @ List.fold toReaction [] modules
            | _ -> acc
        List.fold toReactionNetwork' [] list