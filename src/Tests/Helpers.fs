namespace Tests

open Parser.Types

module Helpers =
    let mulitpleToStr fn xs = List.map fn xs |> String.concat ","

    let rec rootListToString rs =
        sprintf "crn={%s}" (mulitpleToStr rootToString rs)

    and rootToString =
        function
        | Conc (s, n) -> sprintf "conc[%s, %i]" s n
        | Step cmds -> sprintf "step[{%s}]" (mulitpleToStr commandToString cmds)

    and commandToString =
        function
        | Module m -> moduleToString m
        | Conditional c -> conditionalToString c

    and moduleToString =
        function
        | Ld (s1, s2) -> sprintf "ld[%s, %s]" s1 s2
        | Add (s1, s2, s3) -> sprintf "add[%s, %s, %s]" s1 s2 s3
        | Sub (s1, s2, s3) -> sprintf "sub[%s, %s, %s]" s1 s2 s3
        | Mul (s1, s2, s3) -> sprintf "mul[%s, %s, %s]" s1 s2 s3
        | Div (s1, s2, s3) -> sprintf "div[%s, %s, %s]" s1 s2 s3
        | Sqrt (s1, s2) -> sprintf "sqrt[%s, %s]" s1 s2
        | Cmp (s1, s2) -> sprintf "cmp[%s, %s]" s1 s2

    and conditionalToString =
        function
        | IfGT cmds -> sprintf "ifGT[{%s}]" (mulitpleToStr commandToString cmds)
        | IfGE cmds -> sprintf "ifGE[{%s}]" (mulitpleToStr commandToString cmds)
        | IfEQ cmds -> sprintf "ifEQ[{%s}]" (mulitpleToStr commandToString cmds)
        | IfLT cmds -> sprintf "ifLT[{%s}]" (mulitpleToStr commandToString cmds)
        | IfLE cmds -> sprintf "ifLE[{%s}]" (mulitpleToStr commandToString cmds)
