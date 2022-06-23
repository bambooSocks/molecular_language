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
open ChemicalReactions.modulesToReactions
open ChemicalReactions.Simulator

module PropertyTests =

    [<TestFixture>]
    type PropertyTestClass() =

        [<SetUp>]
        member _.setUp() =
            let _ = Arb.register<customGenerator> ()
            ()

        (*
        [<Property>]
        member _.testProp (ast: TCommand list) =
            let stepPermutations = permute ast
            let concList = concListFromSet ast
            let originalInterpretation = customInterpret ast concList
            let isPermutationEqualToOriginal acc permutedStep =
                let permutedStepInterpretation = customInterpret permutedStep concList
                let boo = (List.ofSeq (Seq.take 100 originalInterpretation)) = (List.ofSeq (Seq.take 100 permutedStepInterpretation))
                acc && boo
            List.fold isPermutationEqualToOriginal true stepPermutations
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

            (*
        [<Property>]
        member _.interpretationCompilationProperty(rootList: Parser.Types.TRoot list) =

            let xss = List.fold isStep [] rootList
            let xs = (List.fold (fun acc x -> acc @ concListFromSet x) [] xss)
            let x = Map.ofList (List.map (extractConc) xs)

            //printf "%A\n%A\n%A\n%A" rootList xss xs x

            let interpreted = Seq.take 100 (interpret x rootList)

            let rxnnetwork = fst(toReactionNetwork rootList)
            let compiled = simulateN rxnnetwork x 0.1 1000

            pairwiseCmp interpreted compiled
            *)