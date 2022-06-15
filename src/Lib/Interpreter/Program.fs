#nowarn "40"
open Parser.Types

type sttt = (float*float*float)

module Intepreter = 
    let rec interpreter ast state =
        match ast with 
        | _ -> 2

let ast1 = [Conc("a", 32); Conc("b", 12)]

printf "%A" (Intepreter.interpreter ast1)