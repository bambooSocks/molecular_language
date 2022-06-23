(*
    Author: Andrei Redis
*)

namespace Interpreter

open Parser.Types

module Interpreter =
    let rec concL (state: State) (program: TConc list) =
        match program with
        | [] -> state
        | concList -> List.fold conc state concList

    and conc (state: State) (species, number) = Map.add species number state

    let rec stepL (state: State) (program: TStep list) =
        match program with
        | [] -> state
        | stepList -> List.fold step state stepList

    and step (state: State) commList = 
        let state'= List.fold command state commList
        let stateWCmp = try (Map.add "Cmp" (Map.find "TCmp" state') state') with | ex -> state'
        stateWCmp

    and command (state: State) (program: TCommand) =
        match program with
        | Module md -> mdl state md
        | Conditional cd -> cond state cd

    and mdl (state: State) (program: TModule) =
        let get x = Map.find x state
        let bind x y = Map.add x y state

        match program with
        | Ld (A, B) -> bind B (get A)
        | Add (A, B, C) -> bind C ((get A) + (get B))
        | Sub (A, B, C) ->
            match Sub(A, B, C) with
            | Sub (A, B, C) when ((get A) > (get B)) -> bind C ((get A) - (get B))
            | _ -> state.Add(C, 0)
        | Mul (A, B, C) -> bind C ((get A) * (get B))
        | Div (A, B, C) -> bind C ((get A) / (get B))
        | Sqrt (A, B) -> bind B (sqrt (get A))
        | Cmp (A, B) ->
            match Cmp(A, B) with
            | _ when ((get A - get B) > 0.5) -> bind "TCmp" 1
            | _ when ((get A - get B) < -0.5) -> bind "TCmp" -1
            | _ -> bind "TCmp" 0

    and cond (state: State) (program: TConditional) =
            let fwd x = List.fold command state x
            let flag = try (Map.find "Cmp" state) with | _ -> nan

            match program with
            | IfGT cmdList when (flag = 1) -> fwd cmdList
            | IfGE cmdList when (flag = 1) || (flag = 0) -> fwd cmdList
            | IfEQ cmdList when (flag = 0) -> fwd cmdList
            | IfLT cmdList when (flag = -1) -> fwd cmdList
            | IfLE cmdList when (flag = -1) || (flag = 0) -> fwd cmdList
            | _ -> state

    let rec interpret (initialState: State) (rootList: TRoot list) =
        let concList =
            List.choose
                (function
                | Conc c -> Some c
                | _ -> None)
                rootList

        let stepList =
            List.choose
                (function
                | Step s -> Some s
                | _ -> None)
                rootList
        // interpreter executes the concs first
        let stateAfterConc = concL initialState concList
        //and then proceeds to the steps, producting an infinite sequence
        let rec interpretSteps (initialState: State) (stpL: TStep List) =
            seq {
                let state = stepL initialState stepList
                yield state
                yield! interpretSteps state stepList
            }

        interpretSteps stateAfterConc stepList
    // used to generate matching random Concs (initial concentration declaration) given a Step - used in tests
    let concListFromSet step =

        let rec speciesSuperSet acc (step: TStep) =

            let rec speciesSet acc cmd =

                let add x s = Set.add x s
                let fwd x = List.fold speciesSet acc x

                match cmd with
                | Module md ->
                    match md with
                    | Ld (A, B) -> add A acc |> add B
                    | Add (A, B, C) -> add A acc |> add B |> add C
                    | Sub (A, B, C) -> add A acc |> add B |> add C
                    | Mul (A, B, C) -> add A acc |> add B |> add C
                    | Div (A, B, C) -> add A acc |> add B |> add C
                    | Sqrt (A, B) -> add A acc |> add B
                    | Cmp (A, B) -> add A acc |> add B
                | Conditional cd ->
                    match cd with
                    | IfGT cmdList -> fwd cmdList
                    | IfGE cmdList -> fwd cmdList
                    | IfEQ cmdList -> fwd cmdList
                    | IfLT cmdList -> fwd cmdList
                    | IfLE cmdList -> fwd cmdList

            List.fold speciesSet acc step

        let speciesSetToConcList acc specie =
            acc
            @ [ Conc(specie, System.Random().Next(1, 10)) ]

        speciesSuperSet Set.empty step |>
        Set.fold speciesSetToConcList []
    
    // interpret only a single step together with a conclist - used in tests
    let rec customInterpret step =
        function
        | [] -> Seq.empty
        | cList -> 
            let rtList = List.append cList [Step step]
            printf "------------------Step+Conc -------------------------- \n %A \n" rtList
            interpret Map.empty rtList
