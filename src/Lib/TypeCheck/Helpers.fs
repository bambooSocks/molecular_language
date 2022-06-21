namespace TypeCheck

open Parser.Types
open TypeCheck.Types

module Helpers =
    // Combine functions for custom types
    let combineResults (r1, es1) (r2, es2) = (r1 && r2, es1 @ es2)
    let combineInOut (as1, bs1) (as2, bs2) = (as1 @ as2, bs1 @ bs2)

    // Run check function on a list and combine the results
    let checkMultiple checkFn xs =
        List.map checkFn xs
        |> List.fold combineResults (true, [])

    // Extract inputs and outputs from module
    let getInOutsForModule =
        function
        | Ld (s1, s2)
        | Sqrt (s1, s2) -> ([ s1 ], [ s2 ])
        | Cmp (s1, s2) -> ([ s1; s2 ], [])
        | Add (s1, s2, s3)
        | Sub (s1, s2, s3)
        | Mul (s1, s2, s3)
        | Div (s1, s2, s3) -> ([ s1; s2 ], [ s3 ])

    // Extract inputs and outputs from Conditional, Command and Command list
    let rec getInOutsForConditional =
        function
        | IfGE cmds
        | IfGT cmds
        | IfEQ cmds
        | IfLT cmds
        | IfLE cmds -> getInOutsForCommandList cmds

    and getInOutsForCommand =
        function
        | Module m -> getInOutsForModule m
        | Conditional c -> getInOutsForConditional c

    and getInOutsForCommandList cmds =
        List.map getInOutsForCommand cmds
        |> List.fold combineInOut ([], [])

    // Check for cyclic dependencies in commands of a step
    let checkCyclicDependencyInStep cmds =
        let cycDep =
            getInOutsForCommandList cmds
            |> fun (inp, out) -> Set.intersect (set inp) (set out)
            |> Set.toList

        (cycDep.IsEmpty,
         if cycDep.IsEmpty then
             []
         else
             [ CyclicStepDependency cycDep ])

    let getConditionals =
        function
        | Conditional c -> Some c
        | _ -> None

    // Check command for containing a cmp module
    let containsCmp =
        function
        | Module m ->
            match m with
            | Cmp _ -> true
            | _ -> false
        | _ -> false

    let checkConcStepOrder rs =
        let rec checkConcStepOrder' foundStep =
            function
            | [] -> (true, [])
            | (Conc _) :: _ when foundStep -> (false, [ ConcStepWrongOrder ])
            | (Step _) :: rs -> checkConcStepOrder' true rs
            | (_) :: rs -> checkConcStepOrder' foundStep rs

        checkConcStepOrder' false rs

    let checkMissingCmp rs =
        let rec checkMissingCmp' cmpBefore =
            function
            | [] -> (true, [])
            | r :: rs ->
                match r with
                | Conc _ -> checkMissingCmp' cmpBefore rs
                | Step cmds ->
                    if cmpBefore then
                        (true, [])
                    else
                        let hasCmp = List.exists containsCmp cmds

                        let errors =
                            List.choose getConditionals cmds
                            |> List.map (fun c -> CompareMissing c)

                        combineResults (errors.IsEmpty, errors) (checkMissingCmp' hasCmp rs)

        checkMissingCmp' false rs

    let getConditionalName =
        function
        | IfGT _ -> "ifGT"
        | IfGE _ -> "ifGE"
        | IfEQ _ -> "ifEQ"
        | IfLT _ -> "ifLT"
        | IfLE _ -> "ifLE"

    let moduleToString =
        function
        | Ld (s1, s2) -> sprintf "ld[%s, %s]" s1 s2
        | Sqrt (s1, s2) -> sprintf "sqrt[%s, %s]" s1 s2
        | Cmp (s1, s2) -> sprintf "cmp[%s, %s]" s1 s2
        | Add (s1, s2, s3) -> sprintf "add[%s, %s, %s]" s1 s2 s3
        | Sub (s1, s2, s3) -> sprintf "sub[%s, %s, %s]" s1 s2 s3
        | Mul (s1, s2, s3) -> sprintf "mul[%s, %s, %s]" s1 s2 s3
        | Div (s1, s2, s3) -> sprintf "div[%s, %s, %s]" s1 s2 s3

    let getErrorMessage =
        function
        | CompareMissing c ->
            sprintf "Conditional expression: %s is missing a compare module in previous steps" (getConditionalName c)
        | NegativeConcentration (s, n) -> sprintf "Concentration for %s cannot be negative, value given %f" s n
        | CyclicModuleDependency m -> sprintf "Cyclic dependency in module %A" (moduleToString m)
        | CyclicStepDependency specs ->
            sprintf "Cyclic dependency of variable(s): %A in step" (String.concat ", " specs)
        | ConcStepWrongOrder -> "Concentration declaration cannot be after a step declaration"
