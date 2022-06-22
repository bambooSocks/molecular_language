namespace Tests

open Parser.Types
open Parser.Parser
open Interpreter.Interpreter
open Drawing
open TypeCheck.TypeCheck
open App.Examples
open FParsec

module Helpers =
    let mulitpleToStr fn xs = List.map fn xs |> String.concat ","

    let exprToString (e: TSpecies list) = String.concat "+" e

    let rxnToString (Rxn (e1, e2, s)) =
        sprintf "rxn[%s,%s,%f]" (exprToString e1) (exprToString e2) s

    let rxnsToString rxns =
        List.map rxnToString rxns |> String.concat ","

    let rec rootListToString rs =
        sprintf "crn={%s}" (mulitpleToStr rootToString rs)

    and rootToString =
        function
        | Conc (s, n) -> sprintf "conc[%s, %f]" s n
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



    let mapCmp (a: State) (b: State) = Map.fold (fun acc key aval -> match Map.tryFind key b with 
                                                                            | Some bval when bval = aval -> true
                                                                            | _ -> false
                                                                            ) true a

    let pairwiseCmp seq1 seq2 =  Seq.fold2 (fun acc a b -> mapCmp a b && mapCmp b a) true seq1 seq2

    let extractInitial (rootList: TRoot list) = 
        let extractInitial' acc rl = 
            match rl with
            | Conc(tConc) :: xs -> acc @ [tConc]
            | _ -> acc
        Map.ofList (extractInitial' [] rootList)

    let rec customFold f acc xs ys =
            match xs, ys with 
            | [],[] ->  acc
            | x::xs, y::ys -> customFold f (f acc x y) xs ys
            | _, [] -> false
            | [], _ -> false

    let compareCustom ast1 ast2 =
        let eqWithError x y = abs (x-y) < 0.5

        let rec concL acc ast1 ast2 =
            match ast1, ast2 with
            | [], [] -> acc
            | concList1, conclist2 -> customFold conc acc concList1 conclist2

        and conc acc (species1, number1) (species2, number2) = acc && (species1 = species2) && (eqWithError number1 number2)

        let rec stepL acc ast1 ast2 =
            match ast1, ast2 with
            | [], [] -> acc
            | stepList1, stepList2 -> customFold step acc stepList1 stepList2

        and step acc commList1 commList2 = customFold command acc commList1 commList2

        and command acc ast1 ast2 =
            match ast1, ast2 with
            | Module md, Module md2 -> mdl acc md md2
            | Conditional cd, Conditional cd2 -> cond acc cd cd2
            | _ -> false

        and mdl acc ast1 ast2 =
            match ast1, ast2 with
            | Ld (A, B), Ld (A', B') -> acc && (A = A') && (B = B') 
            | Add (A, B, C), Add (A', B', C') -> acc && (A = A') && (B = B') && (C = C')
            | Sub (A, B, C), Sub (A', B', C') -> acc && (A = A') && (B = B') && (C = C')
            | Mul (A, B, C), Mul (A', B', C') -> acc && (A = A') && (B = B') && (C = C')
            | Div (A, B, C), Div (A', B', C') -> acc && (A = A') && (B = B') && (C = C')
            | Sqrt (A, B), Sqrt (A', B') -> acc && (A = A') && (B = B')
            | Cmp (A, B), Cmp (A', B') -> acc && (A = A') && (B = B')
            | _ -> false

        and cond acc ast1 ast2 =
            let fwd xs ys = customFold command acc xs ys

            match ast1,ast2 with
            | IfGT cmdList, IfGT cmdList' -> fwd cmdList cmdList'
            | IfGE cmdList, IfGE cmdList' -> fwd cmdList cmdList'
            | IfEQ cmdList, IfEQ cmdList' -> fwd cmdList cmdList'
            | IfLT cmdList, IfLT cmdList' -> fwd cmdList cmdList'
            | IfLE cmdList, IfLE cmdList' -> fwd cmdList cmdList'
            | _ -> false

        let concList rootList = List.choose
                                    (function
                                    | Conc c -> Some c
                                    | _ -> None)
                                    rootList

        let stepList rootList = List.choose
                                    (function
                                    | Step s -> Some s
                                    | _ -> None)
                                    rootList
        

        (stepL true (stepList ast1) (stepList ast2)) && (concL true (concList ast1) (concList ast2))

    let rec permute list = 
        let rec distribute e = function
            | [] -> [[e]]
            | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

        match list with
        | [] -> [[]]
        | e::xs -> List.collect (distribute e) (permute xs)

    let isStep acc troot = match troot with
                                | Step(x) -> acc @ [x]
                                | _ -> acc

    let rec extractConc troot = match troot with
                                | Conc(s, c) -> s, c
                                | _ -> failwith "Not a conc"



    let outputTest src expected =
        let parserResult = runCrnParser src //Parse program

        match parserResult with
        | Success (ast, _, _) ->
            let checkResult = check ast

            match snd checkResult with
            | [] ->
                let resultingStates = Seq.head (interpret (Map.ofList []) ast) //Interpret program

                //printf "%A" resultingStates
                
                mapCmp expected resultingStates

            | _ -> false
        | Failure (err, _, _) -> printfn "PARSING FAILED:\n%s\n" err
                                 false