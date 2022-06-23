(*
    Author: Matej Majtan
*)

namespace TypeCheck

open Parser.Types
open TypeCheck.Helpers
open TypeCheck.Types

module TypeCheck =
    // Check functions for type checking
    let rec check rs =
        let notCmpMissing = checkMissingCmp rs
        let concStepNotWrongOrder = checkConcStepOrder rs

        checkMultiple checkRoot rs
        |> combineResults notCmpMissing
        |> combineResults concStepNotWrongOrder

    and checkRoot =
        function
        | Conc c -> checkNegativeConcentration c
        | Step cmds ->
            let noCycDep = checkCyclicDependencyInStep cmds
            let noMultipleCmp = checkMultipleCmpInStep cmds
            let noSameOutputs = checkSameOutputInStep cmds

            checkMultiple checkCommand cmds
            |> combineResults noCycDep
            |> combineResults noMultipleCmp
            |> combineResults noSameOutputs

    and checkCommand =
        function
        | Module m -> checkModule m
        | Conditional c -> checkConditional c

    and checkModule m =
        match m with
        | Ld (s1, s2)
        | Sqrt (s1, s2)
        | Cmp (s1, s2) ->
            let noCycDep = s1 <> s2

            (noCycDep,
             if noCycDep then
                 []
             else
                 [ CyclicModuleDependency m ])
        | Add (s1, s2, s3)
        | Sub (s1, s2, s3)
        | Mul (s1, s2, s3)
        | Div (s1, s2, s3) ->
            let noCycDep = s1 <> s3 && s2 <> s3

            (noCycDep,
             if noCycDep then
                 []
             else
                 [ CyclicModuleDependency m ])

    and checkConditional =
        function
        | IfGT cmds
        | IfGE cmds
        | IfEQ cmds
        | IfLT cmds
        | IfLE cmds -> checkMultiple checkCommand cmds

    let rec printErrors =
        function
        | [] -> ()
        | e :: es ->
            printfn "\x1B[1;31mERROR: %s\x1B[0m" (getErrorMessage e)
            printErrors es
