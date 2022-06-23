(*
    Authors: Kristine Maria Klok Jørgensen, Christopher Acosta
*)

namespace ChemicalReactions

open Parser.Types

module Simulator = 
    let product = List.fold (*) 1.0

    let stateGet s st = Map.find s st
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
            let st' = step crn st ss dt
            yield st'
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

    let crn7s0 = Map.ofList [("a", 80.0); ("b", 20.0); ("aGTb", 0.5); ("aLTb", 0.5)]
    let crn7 =
        let rxn1 = Rxn (["aGTb"; "b"], ["aLTb"; "b"], 1)
        let rxn2 = Rxn (["aLTb"; "a"], ["aGTb"; "a"], 1)
        [rxn1; rxn2]


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
