namespace Tests

open Parser.Parser
open FsCheck
open CustomGenerator
open Helpers

module Tests =

    let parserProperty ast =
        let generated = rootListToString ast
        let parsed = getParserResult (runCrnParser generated)

        if parsed.IsSome then
            ast = parsed.Value
        else
            false

    let _ = Arb.register<customGenerator> ()

    let _ = Check.Verbose parserProperty
