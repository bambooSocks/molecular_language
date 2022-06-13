#nowarn "40"
namespace Parser

open FParsec

module Parser =
    let token p = p .>> spaces

    let symbol s = token (pstring s)

    let parseCrnProgram : Parser<string, unit> = parse {
        let! _ = symbol "crn"
        return "x"}

    //let runParser s = run (symbol "crn" >>= fun _ -> preturn "x") s 
    let runParser s = run parseCrnProgram s

    // type E = V of string | C of int | Sub of E*E

    // let token p = p .>> spaces

    // let symbol s = token (pstring s) 

    // let pinteger:Parser<int,unit> = token pint32;

    // let ident:Parser<string,unit> = 
    //         let identifierOrChar c = isLetter c || isDigit c
    //         token(many1Satisfy2L isLetter identifierOrChar "identifier")
    
    // // for the Grammer part
    // let pV = parse {let! x = ident   
    //                 return V x}
    
    // let pC = parse {let! n = pinteger   
    //                 return C n}

    // // A grammar without left recursion and with disjoint first sets of choices
    // // 
    // // T    -> V | C | ( E )
    // // E    -> T Eopt
    // // Eopt -> - T Eopt | epsilon 

    // // A formulation based on computation expressions

    // let rec pT = pV 
    //             <|> pC 
    //             <|>  parse { let! _ = symbol "(" 
    //                          let! e = pE 
    //                          let! _ = symbol ")"
    //                          return e}

    // and pE = parse { let! e = pT
    //                 return! pEopt e} 

    // and pEopt e = parse {let! _ = symbol "-" 
    //                      let! e' = pT
    //                      return! pEopt(Sub(e,e')) } 
    //             <|> preturn e


    // // remember to skip leading spaces when parsing an expression

    // let parseE str = run (spaces >>. pE) str;;

   
    open Types
    //let token p = p .>> spaces
    //let symbol s = token (pstring s)
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
