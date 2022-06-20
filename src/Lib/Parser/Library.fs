#nowarn "40"
namespace Parser

open FParsec
open Types

module Parser =
    let token p = p .>> spaces
    let symbol s = token (pstring s)
    let pInteger: Parser<int, unit> = token pint32

    let runRxnParser s = 
        let pSpecies: Parser<string, unit> = token (many1SatisfyL isLetter "species")
        let pExpr = sepBy pSpecies (symbol "+")
        let pRxn =
            parse { let! _ = symbol "rxn"
                    let! _ = symbol "["
                    let! reactants = pExpr
                    let! _ = symbol ","
                    let! products = pExpr
                    let! _ = symbol ","
                    let! n = pfloat
                    let! _ = symbol "]"
                    return Rxn (reactants, products, n) }
        let pRxns = sepBy1 pRxn  (symbol ",") 
        run pRxns s

    let runCrnParser s =
        let pInteger: Parser<int, unit> = token pint32

        let isLetterOrDigit x = isLetter x || isDigit x
        let pSpecies: Parser<string, unit> = token (many1Satisfy2L isLetter isLetterOrDigit "species")

        let pModule =
            let helper2 modName constr =
                parse {
                    let! _ = symbol modName
                    let! _ = symbol "["
                    let! s1 = pSpecies
                    let! _ = symbol ","
                    let! s2 = pSpecies
                    let! _ = symbol "]"
                    return constr (s1, s2)
                }

            let helper3 modName constr =
                parse {
                    let! _ = symbol modName
                    let! _ = symbol "["
                    let! s1 = pSpecies
                    let! _ = symbol ","
                    let! s2 = pSpecies
                    let! _ = symbol ","
                    let! s3 = pSpecies
                    let! _ = symbol "]"
                    return constr (s1, s2, s3)
                }

            choice [ helper2 "ld" Ld
                     helper3 "add" Add
                     helper3 "sub" Sub
                     helper3 "mul" Mul
                     helper3 "div" Div
                     helper2 "sqrt" Sqrt
                     helper2 "cmp" Cmp ]

        let rec parseRootList = sepBy1 parseRoot (symbol ",")

        and parseRoot = pConc <|> pStep

        and pStep =
            parse {
                let! _ = symbol "step"
                let! _ = symbol "["
                let! _ = symbol "{"
                let! cmdlist = pCommandList
                let! _ = symbol "}]"
                return Step cmdlist
            }

        and pCommandList = sepBy1 parseCommand (symbol ",")

        and pConditional =
            let helper modName constr =
                parse {
                    let! _ = symbol modName
                    let! _ = symbol "[{"
                    let! cl = pCommandList
                    let! _ = symbol "}]"
                    return constr cl
                }

            choice [ helper "ifGT" IfGT
                     helper "ifGE" IfGE
                     helper "ifEQ" IfEQ
                     helper "ifLT" IfLT
                     helper "ifLE" IfLE ]

        and parseCommand =
            (pModule |>> Module)
            <|> (pConditional |>> Conditional)

        and pConc =
            parse {
                let! _ = symbol "conc"
                let! _ = symbol "["
                let! species = pSpecies
                let! _ = symbol ","
                let! number = pInteger
                let! _ = symbol "]"
                return Conc(species, number)
            }

        let pCrnProgram: Parser<TRoot list, unit> =
            parse {
                let! _ = symbol "crn"
                let! _ = symbol "="
                let! _ = symbol "{"
                let! rootList = parseRootList
                return rootList
            }

        run (spaces >>. pCrnProgram) s

    let getParserResult = function
        | Success (r, _, _) -> Some r
        | Failure _ -> None