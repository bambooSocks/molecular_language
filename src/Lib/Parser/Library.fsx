#r "nuget: FParsec";;
#nowarn "40";;

open FParsec;;

let token p = p .>> spaces

let symbol s = token (pstring s)


let rec parseRootList = parse {
    parseRoot <|> parse { let! r = parseRoot
                          symbol ","
                          return! parseRootList}
}

and parseRoot = parseConc <|> parseStep

and parseStep = parse {
    let! _ = symbol "step["
    let! cmdlist = parseCommandList
    let! _ = symbol "]"
    return cmdlist}

and parseCommandList = parse {
    parseCommand <|> parse { let! r = parseCommand
                             symbol ","
                             return! parseCommandList}
}

and parseCommand = parseModule <|> parseCmp <|> parseConditional

and parseModule = choice [parseAdd; parseSub; parseMul; parseDiv; parseSqrt; parseCmp]

and parseCmp = symbol "cmp"

and parseConditional = symbol "conditional"

and parseConc = parse {
    let! _ = symbol "conc["
    let! species = parseSpecies
    let! _ = symbol ","
    let! number = pint32
}

and parseSpecies = anyChar


let parseCrnProgram : Parser<unit, unit> = parse {
    let! _ = symbol "crn"
    let! _ = symbol "="
    let! _ = symbol "{"
    return! parseRootList}



    //let runParser s = run (symbol "crn" >>= fun _ -> preturn "x") s 
let runParser s = run parseCrnProgram s



let res = runParser "crn = {"
//printf "%A" res;;