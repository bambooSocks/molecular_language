namespace Tests

open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Parser.Parser
open Parser.Types
open CustomGenerator
open Helpers
open Interpreter.Interpreter

module PropertyTests =

    [<TestFixture>]
    type PropertyTestClass() =

        [<SetUp>]
        member _.setUp() =
            let _ = Arb.register<customGenerator> ()
            ()

        [<Property>]
        member _.parserProperty ast =
            let generated = rootListToString ast
            let parsed = getParserResult (runCrnParser generated)

            if parsed.IsSome then
                ast = parsed.Value
            else
                false

        member _.interpretationCompilationProperty initial rootList = //TODO extract initial state from rootList & figure out how to actually run this
            let interpreted = interpret initial rootList
            let compiled = interpret initial rootList
            // let compiled = states computed from the reaction network compiled by the compiler. 
            pairwiseCmp interpreted compiled



