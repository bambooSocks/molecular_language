#r "nuget: FParsec"
#nowarn "40"

open FParsec

//type TCrn = Crn of TRootList



type TRootList = RootList of TRoot list
    // | Root of TRoot
    // | Root_Seq of TRoot * TRootList

and TRoot =
    | Conc of TSpecies * TNumber
    | Step of TCommand list //TCommandList

// and TCommandList =
//     | Command of TCommand
//     | Command_Seq of TCommand * TCommandList

and TCommand =
    | Module of TModule
    | Conditional of TConditional
//    | Rxn of TExpr * TExpr * TNumber
and TModule =
    | Ld of TSpecies * TSpecies
    | Add of TSpecies * TSpecies * TSpecies
    | Sub of TSpecies * TSpecies * TSpecies
    | Mul of TSpecies * TSpecies * TSpecies
    | Div of TSpecies * TSpecies * TSpecies
    | Sqrt of TSpecies * TSpecies
    | Cmp of TSpecies * TSpecies

and TConditional =
    | IfGT of TCommand list
    | IfGE of TCommand list
    | IfEQ of TCommand list
    | IfLT of TCommand list
    | IfLE of TCommand list

// and TExpr =
//     | SpExpr of TSpecies
//     | SpExpr_Seq of TSpecies * TExpr

and TSpecies = string
and TNumber = int
// and TSpecies = Species of string
// and TNumber = Number of int

let token p = p .>> spaces
let symbol s = token (pstring s)
let pinteger: Parser<int, unit> = token pint32

let species: Parser<string, unit> =
    //let isUpperLetter c = isLetter c && isUpper c
    token (many1SatisfyL isLetter "species")

let pNumber = pinteger >>= fun x -> preturn x //(Number x)
let pSpecies = species >>= fun x -> preturn x //(Species x)

let pModule =
    let helper2 modName constr =
        parse { let! _ = symbol modName
                let! _ = symbol "["
                let! s1 = pSpecies
                let! _ = symbol ","
                let! s2 = pSpecies
                let! _ = symbol "]"
                return constr(s1, s2) }
    let helper3 modName constr =
        parse { let! _ = symbol modName
                let! _ = symbol "["
                let! s1 = pSpecies
                let! _ = symbol ","
                let! s2 = pSpecies
                let! _ = symbol ","
                let! s3 = pSpecies
                let! _ = symbol "]"
                return constr (s1, s2, s3) } 
    choice [helper2 "ld" Ld;
            helper3 "add" Add;
            helper3 "sub" Sub;
            helper3 "mul" Mul;
            helper3 "div" Div;
            helper2 "sqrt" Sqrt;
            helper2 "cmp" Cmp]

// let rec pExpr =
//     parse {
//         let! s = pSpecies
//         let! _ = symbol "+"
//         let! e = pExpr
//         return s :: e
//     }
//     <|> parse {
//         let! s = pSpecies
//         return [ s ]
//     }

let rec parseRootList = sepBy1 parseRoot (symbol ",")

and parseRoot = pConc <|> pStep

and pStep =
    parse { let! _ = symbol "step[{"
            let! cmdlist = pCommandList
            let! _ = symbol "}]"
            return Step cmdlist}

and pCommandList = sepBy1 parseCommand (symbol ",")

and pConditional =
    let helper modName constr =
        parse { let! _ = symbol modName
                let! _ = symbol "[{"
                let! cl = pCommandList
                let! _ = symbol "}]"
                return constr cl }
    choice [helper "ifGT" IfGT;
            helper "ifGE" IfGE;
            helper "ifEQ" IfEQ;
            helper "ifLT" IfLT;
            helper "ifLE" IfLE]

and parseCommand = (pModule |>> Module) <|> (pConditional |>> Conditional)

and pConc = parse {
    let! _ = symbol "conc["
    let! species = pSpecies
    let! _ = symbol ","
    let! number = pNumber
    let! _ = symbol "]"
    return Conc (species, number)
}

let pCrnProgram : Parser<TRoot list, unit> =
    parse { let! _ = symbol "crn"
            let! _ = symbol "="
            let! _ = symbol "{"
            let! rootList = parseRootList
            return rootList}

    //let token p = p .>> spaces
    //let symbol s = token (pstring s)

let gcd = """
crn = {
  conc[a,32],
  conc[b,12],
  step[{
    ld [a, atmp],
    ld [b, btmp],
    cmp[a,b]
  }],
  step[{
    ifGT[{ sub[atmp,btmp,a] }],
    ifLT[{ sub[btmp,atmp,b] }]
  }]
};
"""

let small = "crn = { conc[a, 32], conc[b, 12] };"
let small2 = "crn = { conc[a, 32], conc[b, 12], step[{ ifGT[{ sub[atmp,btmp,a] }] }] };"
//let runParser s = run (symbol "crn" >>= fun _ -> preturn "x") s 
let runParser s = run (spaces >>. pCrnProgram) s
let res = runParser gcd
//printf "%A" res;;
res;;