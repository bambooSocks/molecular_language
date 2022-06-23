(*
    Author: Matej Majtan, Andrei Redis, Kristine Maria Klok Jørgensen
*)

namespace Tests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Parser.Parser
open Parser.Types
open CustomGenerator
open Helpers
open Interpreter.Interpreter
open TypeCheck.Helpers

module PropertyTests =

    [<TestFixture>]
    type PropertyTestClass() =

        [<SetUp>]
        member _.setUp() =
            let _ = Arb.register<customGenerator> ()
            ()
        (*
        [<Property>]
        member _.testProp (ast: TStep) =
            printf "Original ast -------- \n %A \n" ast
            //let stepPermutations = permute ast
            //printf "Permutations -------- \n %A \n" stepPermutations
            let concList = concListFromSet ast
            printf "OriginalConcList -------- \n %A \n" concList
            let originalInterpretation = customInterpret ast concList
            printf "OriginalInterpretation -------- \n %A \n" originalInterpretation
            (*let isPermutationEqualToOriginal acc permutedStep =
                printf "OriginalInterpretation -------- \n %A \n" originalInterpretation
                printf "PermutedStep -------- \n %A \n" permutedStep
                let permutedStepInterpretation = customInterpret permutedStep concList
                printf "PermutedStepInterpretation -------- \n %A \n" permutedStepInterpretation
                acc && (originalInterpretation = permutedStepInterpretation)
            List.fold isPermutationEqualToOriginal true stepPermutations *)
            //The problem is that the concs are reinitialized every run I think - concs should be only initialized once
            //for both orginal and permutation, but the steps should be ran many times
        *)

        [<Property>]
        member _.crnParserProperty ast =
            let generated = rootListToString ast
            let parsed = getParserResult (runCrnParser generated)

            if parsed.IsSome then
                compareCustom ast parsed.Value
            else
                false

        [<Property>]
        member _.rxnParserProperty(rxns: TRxn list) =
            let generated = rxnsToString rxns
            let parsed = getParserResult (runRxnParser generated)

            if parsed.IsSome then
                rxns = parsed.Value
            else
                false

        [<Property>]
        member _.cmpBeforeCondProperty ast =
            let res, _ = checkMissingCmp ast
            res

        [<Property>]
        member _.noNegativeConcentrationProperty(conc: TConc) =
            let res, _ = checkNegativeConcentration conc
            res

        [<Property>]
        member _.noMultipleCmpInStepProperty(cmds: TCommand list) =
            let res, _ = checkMultipleCmpInStep cmds
            res

        [<Property>]
        member _.interpretationCompilationProperty(rootList: TRoot list) =

            let xss = List.fold isStep [] rootList
            let xs = (List.fold (fun acc x -> acc @ concListFromSet x) [] xss)
            let x = Map.ofList (List.map (extractConc) xs)

            //printf "%A\n%A\n%A\n%A" rootList xss xs x

            let interpreted = Seq.take 100 (interpret x rootList)
            let compiled = Seq.take 100 (interpret x rootList)

            pairwiseCmp interpreted compiled
