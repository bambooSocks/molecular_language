#nowarn "40"
open Parser.Types

let rec interpreter' (state:State) =
    function
    | []       -> state
    | rootList -> List.fold root state rootList

and root (state:State) =  // sequence starts here
    function
    | Conc(species,conc) -> state.Add(species,conc)
    | Step commList      -> List.fold command state commList

and command (state:State) =
    function
    | Module md       -> mdl state md
    | Conditional cd  -> cond state cd 
    | Rxn (xs1,xs2,n) -> state

and mdl (state:State) =
    let get x = Map.find x state
    let bind x y = Map.add x y state

    function
    | Ld(A,B)    -> bind B (get A)
    | Add(A,B,C) -> bind C ((get A) + (get B))
    | Sub(A,B,C) -> match Sub(A,B,C) with
                    | Sub(A,B,C) when ((get A) > (get B)) -> bind C ((get A) - (get B))
                    | _                                   -> state.Add(C, 0)
    | Mul(A,B,C) -> bind C ((get A) * (get B))
    | Div(A,B,C) -> bind C ((get A) / (get B))
    | Sqrt(A,B)  -> bind B (sqrt (get A))
    | Cmp(A,B)   -> match Cmp(A,B) with 
                    | _ when ((get A - get B) > 0.5)  -> bind "Cmp" 1
                    | _ when ((get A - get B) < -0.5) -> bind "Cmp" -1
                    | _                               -> bind "Cmp" 0

and cond (state:State) = 
    let fwd x = List.fold command state x
    let flag = Map.find "Cmp" state
    function
    | IfGT cmdList when (flag = 1)                -> fwd cmdList
    | IfGE cmdList when (flag = 1) || (flag = 0)  -> fwd cmdList
    | IfEQ cmdList when (flag = 0)                -> fwd cmdList
    | IfLT cmdList when (flag = -1) || (flag = 0) -> fwd cmdList
    | IfLE cmdList when (flag = -1)               -> fwd cmdList
    | _                                           -> state