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

(*
        [<Property>]
        member _.interpretationCompilationProperty initial rootList = //TODO extract initial state from rootList & figure out how to actually run this

            let interpreted = interpret (extractInitial rootList) rootList
            let compiled = interpret (extractInitial rootList) rootList
            // let compiled = states computed from the reaction network compiled by the compiler.

            pairwiseCmp interpreted compiled
            *)
