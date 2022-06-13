#nowarn "40"
namespace Parser

open FParsec
open Types

module Parser =

    let token p = p .>> spaces
    let symbol s = token (pstring s)
    let pinteger: Parser<int, unit> = token pint32

    let species: Parser<string, unit> =
        let isUpperLetter c = isLetter c && isUpper c
        token (many1Satisfy2L isUpperLetter isUpperLetter "species")

    let pNumber = pinteger >>= fun x -> preturn (Number x)
    let pSpecies = species >>= fun x -> preturn (Species x)

    let pModule =
        parse {
            let! _ = symbol "ld["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol "]"
            return Ld(s1, s2)
        }
        <|> parse {
            let! _ = symbol "add["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol ","
            let! s3 = pSpecies
            let! _ = symbol "]"
            return Add(s1, s2, s3)
        }
        <|> parse {
            let! _ = symbol "sub["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol ","
            let! s3 = pSpecies
            let! _ = symbol "]"
            return Sub(s1, s2, s3)
        }
        <|> parse {
            let! _ = symbol "Mul["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol ","
            let! s3 = pSpecies
            let! _ = symbol "]"
            return Mul(s1, s2, s3)
        }
        <|> parse {
            let! _ = symbol "div["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol ","
            let! s3 = pSpecies
            let! _ = symbol "]"
            return Div(s1, s2, s3)
        }
        <|> parse {
            let! _ = symbol "sqrt["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol ","
            return Sqrt(s1, s2)
        }
        <|> parse {
            let! _ = symbol "cmp["
            let! s1 = pSpecies
            let! _ = symbol ","
            let! s2 = pSpecies
            let! _ = symbol ","
            return Cmp(s1, s2)
        }

    //     let pConditional =
//         parse {
//             let! _ = symbol "ifGT["
//             let! cl = pCommandList
//             let! _ = symbol "]"
//             return IfGT(cl)
//         }
//         <|> parse {
//             let! _ = symbol "ifGE["
//             let! cl = pCommandList
//             let! _ = symbol "]"
//             return IfGE(cl)
//         }
//         <|> parse {
//             let! _ = symbol "ifEQ["
//             let! cl = pCommandList
//             let! _ = symbol "]"
//             return IfEQ(cl)
//         }
//         <|> parse {
//             let! _ = symbol "ifLT["
//             let! cl = pCommandList
//             let! _ = symbol "]"
//             return IfLT(cl)
//         }
//         <|> parse {
//             let! _ = symbol "ifLE["
//             let! cl = pCommandList
//             let! _ = symbol "]"
//             return IfLE(cl)
//         }

    let rec pExpr =
        parse {
            let! s = pSpecies
            let! _ = symbol "+"
            let! e = pExpr
            return s :: e
        }
        <|> parse {
            let! s = pSpecies
            return [ s ]
        }

    let parse str =
        run (pinteger >>= fun x -> preturn (Number x)) str
