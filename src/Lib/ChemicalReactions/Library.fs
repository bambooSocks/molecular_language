namespace ChemicalReactions

open Parser.Types

module Simulator = 

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

module modulesToReactions =

    let rec toReaction acc (reaction: Parser.Types.TCommand) = 
        match reaction with
        | Module(reac) -> 
            match reac with
            | Ld(x, y)      -> acc @ [Rxn([x], [x;y], 1.0)] @ [Rxn([y], [], 1.0)]
            | Add(x, y, z)  -> acc @ [Rxn([x], [x;z], 1.0)] @ [Rxn([y], [y;z], 1.0)] @ [Rxn([z], [], 1.0)]
            | Sub(x, y, z)  -> acc @ [Rxn([x], [x;z], 1.0)] @ [Rxn([y], [y;"H"], 1.0)] @ [Rxn([z], [], 1.0)] @ [Rxn([z;"H"], [], 1.0)]
            | Mul(x, y, z)  -> acc @ [Rxn([x;y], [x;y;z], 1.0)] @ [Rxn([z], [], 1.0)]      
            | Div(x, y, z)  -> acc @ [Rxn([x], [x;z], 1.0)] @ [Rxn([y;z], [y], 1.0)]  
            | Sqrt(x, y)    -> acc @ [Rxn([x], [x;y], 1.0)] @ [Rxn([y;y], [], 0.5)]  
            | Cmp(x, y)     -> acc @ [Rxn([x + "gt" + y; y], [x + "lt" + y; y], 1.0)]
                                   @ [Rxn([x + "lt" + y; x], [x+ "gt" + y; x], 1.0)]
                                   @ [Rxn([x + "gt" + y;x + "lt" + y], [x + "lt" + y; y], 1.0)]
                                   @ [Rxn([y; x + "lt" + y], [x + "lt" + y; x + "lt" + y], 1.0)]
                                   @ [Rxn([x + "lt" + y; x + "gt" + y], [x + "gt" + y; y], 1.0)]
                                   @ [Rxn([y; x + "gt" + y], [x + "gt" + y; x + "gt" + y], 1.0)]
        | Conditional(reac) -> match reac with
                                | IfGT(command) -> acc @ List.fold toReaction [] command
                                | IfGE(command) -> acc @ List.fold toReaction [] command
                                | IfEQ(command) -> acc @ List.fold toReaction [] command
                                | IfLT(command) -> acc @ List.fold toReaction [] command
                                | IfLE(command) -> acc @ List.fold toReaction [] command


    let toReactionNetwork' acc elem = 
            match elem with
            | Step(modules) -> acc @ [List.fold toReaction [] modules]
            | _ -> acc
        
    let toReactionNetwork (list: Parser.Types.TRoot list) = List.fold toReactionNetwork' [] list