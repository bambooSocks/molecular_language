// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let x = Parser.Parser.runParser "crn = {"
printf "%A" x

// let t1 = Parser.Parser.parseE "   1-x"

// let t2 = Parser.Parser.parseE "   1-x-   2 "

// let t3 = Parser.Parser.parseE "   1- ((xjhde -   2) 
//     - y)
//     "

open Parser.Parser
let parseme = "1"

printf "%A" (parse parseme)
