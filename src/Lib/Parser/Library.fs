namespace Parser

open FParsec

type Expr = V of string

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

   