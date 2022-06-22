(*
    Authors: Kristine Maria Klok Jørgensen, Christopher Acosta
*)

namespace ChemicalReactions

open Parser.Types

module Simulator = 
    let product = List.fold (*) 1.0

    let stateGet s st =
        if Map.containsKey s st
        then Map.find s st
        else printfn "%A" s
             Map.find s st

    let stateAddConc s dc st =
        let c = stateGet s st
        Map.add s (c + dc) st

    let multiplicity s ss = List.length (List.filter (fun x -> x = s) ss)

    let speciesNetChange s (Rxn (rs, ps, _)) = 
        multiplicity s ps - multiplicity s rs

    let concNetChange crn st s dt =
        let calcTerm (rxn as Rxn (rs, _, rate)) =
            let sNetChange = float (speciesNetChange s rxn)
            let multiplicityProduct = product (List.map (fun s -> pown (stateGet s st) (multiplicity s rs)) rs)
            rate * sNetChange * multiplicityProduct * dt
        List.sum (List.map calcTerm crn)

    let step crn st ss dt =
        let netChanges = List.map (fun s -> (s, concNetChange crn st s dt)) ss
        List.fold (fun st' (s, netChange) -> stateAddConc s netChange st') st netChanges

    let crnSpecies crn = List.fold (fun acc (Rxn(ps, rs, _)) -> Set.union acc (Set.ofList (ps @ rs))) Set.empty crn 

    let rec simulate crn st ss dt =
        seq {
            yield st
            let st' = step crn st ss dt
            yield! simulate crn st' ss dt }

    let simulateN crn st dt n =
        let ss = Set.toList (crnSpecies crn)
        simulate crn st ss dt |> Seq.take n |> Seq.toList

module Samples =
    let crn1s0 = Map.ofList [("a", 6.0); ("b", 2.0); ("c", 0.0)]
    let crn1 =
        let rxn1 = Rxn (["a"; "b"], ["a"; "b"; "c"], 1)
        let rxn2 = Rxn (["c"], [], 1)
        [rxn1; rxn2]

    let crn9s0 = Map.ofList [("a", 1.0); ("b", 1.0); ("c", 0.000001)]
    let crn9 =
        let rxn1a = Rxn (["a"; "b"], ["b"; "b"], 1)
        let rxn1b = Rxn (["b"; "c"], ["c"; "c"], 1)
        let rxn2 = Rxn (["c"; "a"], ["a"; "a"], 1)
        [rxn1a; rxn1b; rxn2]

    let ldcrns0 = Map.ofList [("a", 7.0); ("b", 2.0)]
    let ldcrn =
        let rxn1 = Rxn (["a"], ["a"; "b"], 1)
        let rxn2 = Rxn (["b"], [], 1)
        [rxn1; rxn2]

    let addcrns0 = Map.ofList [("a", 2.0); ("b", 3.0); ("c", 0.0)]
    let addcrn =
        let rxn1 = Rxn (["a"], ["a"; "c"], 1)
        let rxn2 = Rxn (["b"], ["b"; "c"], 1)
        let rxn3 = Rxn (["c"], [], 1)
        [rxn1; rxn2; rxn3]

    let subcrns0 = Map.ofList [("a", 5.0); ("b", 3.0); ("c", 0.0); ("h", 0.0)]
    let subcrn =
        let rxn1 = Rxn (["a"], ["a"; "c"], 1)
        let rxn2 = Rxn (["b"], ["b"; "h"], 1)
        let rxn3 = Rxn (["c"], [], 1)
        let rxn4 = Rxn (["c"; "h"], [], 1)
        [rxn1; rxn2; rxn3; rxn4]

    let divcrns0 = Map.ofList [("a", 7.0); ("b", 3.0); ("c", 0.0)]
    let divcrn =
        let rxn1 = Rxn (["a"], ["a"; "c"], 1)
        let rxn2 = Rxn (["b"; "c"], ["b"], 1)
        [rxn1; rxn2]

    let sqrtcrns0 = Map.ofList [("a", 4.0); ("b", 0.01)]
    let sqrtcrn =
        let rxn1 = Rxn (["a"], ["a"; "b"], 1)
        let rxn2 = Rxn (["b"; "b"], [], 0.5)
        [rxn1; rxn2]

    let crn7s0 = Map.ofList [("x", 80.0); ("y", 20.0); ("xGTy", 1.0); ("xLTy", 0.0)]
    let crn7 =
        let rxn1 = Rxn (["xGTy"; "y"], ["xLTy"; "y"], 1)
        let rxn2 = Rxn (["xLTy"; "x"], ["xGTy"; "x"], 1)
        [rxn1; rxn2]

    let crn8 =
        let rxn1 = Rxn (["xGTy"; "xLTy"], ["xLTy"; "b"], 1)
        let rxn2 = Rxn (["b"; "xLTy"], ["xLTy"; "xLTy"], 1)
        let rxn3 = Rxn (["xLTy"; "xGTy"], ["xGTy"; "b"], 1)
        let rxn4 = Rxn (["b"; "xGTy"], ["xGTy"; "xGTy"], 1)
        [rxn1; rxn2; rxn3; rxn4]

module modulesToReactions =
    // Generate the oscillator CRN with species prefix x and numbering 1..n
    let genOscCrn x n =
        let iToRxn i =
            let xi = x + string i
            let xi' = x + string (i % n + 1)
            Rxn ([xi; xi'], [xi'; xi'], 1)
        List.map iToRxn [1..n]

    // Add given species x to both sides of each reaction in the given CRN
    let addCatalysts crn x = List.map (fun (Rxn (rs, ps, k)) -> Rxn (x :: rs, x :: ps, k)) crn

    // Helper functions for constructing reactions from modules with given species
    let ld a b =
        let rxn1 = Rxn ([a], [a; b], 1)
        let rxn2 = Rxn ([b], [], 1)
        [rxn1; rxn2]

    let add a b c =
        let rxn1 = Rxn ([a], [a; c], 1)
        let rxn2 = Rxn ([b], [b; c], 1)
        let rxn3 = Rxn ([c], [], 1)
        [rxn1; rxn2; rxn3]

    let sub a b c h =
            let rxn1 = Rxn ([a], [a; c], 1)
            let rxn2 = Rxn ([b], [b; h], 1)
            let rxn3 = Rxn ([c], [], 1)
            let rxn4 = Rxn ([c; h], [], 1)
            [rxn1; rxn2; rxn3; rxn4]

    let mul a b c =
        let rxn1 = Rxn ([a; b], [a; b; c], 1)
        let rxn2 = Rxn ([c], [], 1)
        [rxn1; rxn2]

    let div a b c =
        let rxn1 = Rxn ([a], [a; c], 1)
        let rxn2 = Rxn ([b; c], [b], 1)
        [rxn1; rxn2]

    let sqrt a b =
        let rxn1 = Rxn ([a], [a; b], 1)
        let rxn2 = Rxn ([b; b], [], 0.5)
        [rxn1; rxn2]

    let cmp a b =
        let aGTb ="cmpGT"
        let aLTb = "cmpLT"
        let rxn1 = Rxn ([aGTb; b], [aLTb; b], 1)
        let rxn2 = Rxn ([aLTb; a], [aGTb;a], 1)
        [rxn1; rxn2]

    let maj fGT fLT b =
            let rxn1 = Rxn ([fGT; fLT], [fLT; b], 1)
            let rxn2 = Rxn ([b; fLT], [fLT; fLT], 1)
            let rxn3 = Rxn ([fLT; fGT], [fGT; b], 1)
            let rxn4 = Rxn ([b; fGT], [fGT; fGT], 1)
            [rxn1; rxn2; rxn3; rxn4]

    // Functions for transforming module types to reactions
    let cmdToCrn = function
        | Ld(a, b)      -> ld a b
        | Add(a, b, c)  -> add a b c
        | Sub(a, b, c)  -> sub a b c ("h" + a + b + c)
        | Mul(a, b, c)  -> mul a b c
        | Div(a, b, c)  -> div a b c  
        | Sqrt(a, b)    -> sqrt a b  
        | _ -> failwith "unexpected module"
    let cmpToCrn = function
        | Cmp(a, b) -> (cmp a b, maj "cmpGT" "cmpLT" "cmpB")
        | _ -> failwith "unexpected module"
    let condToCrn = function
        | IfGT(cmds) -> addCatalysts (List.collect (fun (Module m) -> cmdToCrn m) cmds) "cmpGT"
        | IfLT(cmds) -> addCatalysts (List.collect (fun (Module m) -> cmdToCrn m) cmds) "cmpLT"
        | _ -> failwith "unsupported conditional"

    let isCmp = function | Module (Cmp _) -> true | _ -> false

    // Compile a list of steps into a list of reactions
    let toReactionNetwork (rootList: Parser.Types.TRoot list) =
        // Compile a single step
        let stepToCrns step oscCount =
            let osc = "osc" + string oscCount // Current oscillator species
            let (modules, conds) = // Separate modules and conditionals under TCommand type
                List.fold
                    (fun (ms, cs) cmd ->
                        match cmd with
                        | Module m -> (m :: ms, cs)
                        | Conditional c -> (ms, c ::cs))
                    ([], [])
                    step
            let (noncmps, cmps) = // Separate cmp module from other modules under TModule type
                List.fold
                    (fun (ncs, cs) m ->
                        match m with
                        | Cmp _ -> (ncs, m :: cs)
                        | _ -> (m :: ncs, cs))
                    ([], [])
                    modules
            let bHasCmps = not <| List.isEmpty cmps // Does the cmp module occur in this step?
            let condCrns = List.collect condToCrn conds // Reactions from a conditional (IfGT/IfLT)
            if bHasCmps // If the cmp module occurs in the step, we use 6 oscillator species
            then
                let crnsNoCmp = List.map cmdToCrn noncmps // Reactions from non-cmp modules
                let crnsNoCmpWithOscs = List.collect (fun m -> addCatalysts m osc) crnsNoCmp // Reactions from non-cmp modules with oscillators added
                let (normcrns, amcrns) = // Collection of normalization and AM CRNs (Note: redundant as there can only be one cmp in a step)
                    List.map cmpToCrn cmps
                    |> List.fold
                        (fun (normcrns, amcrns) (normcrn, amcrn) -> (normcrn @ normcrns, amcrn @ amcrns))
                        ([], [])
                let oscNext = "osc" + (string (oscCount + 3)) // Next oscillator species for AM network
                let crns = crnsNoCmpWithOscs @ addCatalysts normcrns osc @ addCatalysts amcrns oscNext @ addCatalysts condCrns oscNext
                (crns, oscCount + 6) // Increment oscCount by 6 due to cmp
            else // If no cmp module occurs, use 3 oscillator species
                // Map modules to reactions and add current oscillator species
                let crns = List.map cmdToCrn noncmps
                let crnsWithOscs = List.collect (fun crn -> addCatalysts crn osc) crns
                (crnsWithOscs @ addCatalysts condCrns osc, oscCount + 3) // Increment oscCount by 3 since no cmp occurred
        
        // Collect reactions from list of steps
        let rec stepsToCrns steps oscCount =
            match steps with
            | [] -> ([], oscCount)
            | step::steps' ->
                let (stepCrns, oscCount') = stepToCrns step oscCount
                let (stepCrns', oscCount'') = stepsToCrns steps' oscCount'
                (stepCrns @ stepCrns', oscCount'')

        let cmdLists = // List of steps from under TRoot type
            List.map (function | Step cmds -> Some cmds | Conc _ -> None) rootList
            |> List.filter (fun o -> o.IsSome)
            |> List.map (fun o -> o.Value)
        
        stepsToCrns cmdLists 3 // Start with oscillator species number 3