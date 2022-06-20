namespace Tests

open Parser.Parser
open FsCheck
open CustomGenerator
open Helpers
open Interpreter.Interpreter 
open Parser.Types

module Tests =

    let parserProperty ast =
        let generated = rootListToString ast
        let parsed = getParserResult (runCrnParser generated)

        if parsed.IsSome then
            ast = parsed.Value
        else
            false

    let _ = Arb.register<customGenerator> ()

    let mapCmp (a: State) (b: State) = Map.fold (fun acc key aval -> match Map.tryFind key b with 
                                                                            | Some bval when bval = aval -> true
                                                                            | _ -> false
                                                                            ) true a

    let pairwiseCmp seq1 seq2 =  Seq.fold2 (fun acc a b -> mapCmp a b) true seq1 seq2

    let interpretationCompilationProperty initial rootList = //TODO extract initial state from rootList
        let interpreted = interpret initial rootList
        let compiled = interpret initial rootList
        // let compiled = states computed from the reaction scheme. 
        pairwiseCmp interpreted compiled
        

    let _ = Check.Verbose parserProperty

    let _ = Check.Verbose interpretationCompilationProperty