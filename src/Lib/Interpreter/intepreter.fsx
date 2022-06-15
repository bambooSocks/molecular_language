#nowarn "40"

type TRootList = RootList of TRoot list
and TRoot =
    | Conc of TSpecies * TNumber
    | Step of TCommand list 
and TCommand =
    | Module of TModule
    | Conditional of TConditional
    | Rxn of TSpecies list * TSpecies list * float
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
and TSpecies = string
and TNumber = int
and State = Map<TSpecies, float>;;

let rec interpreter (state:State) =
    function
    | [] -> state
    | rootList -> List.fold root state rootList

and root (state:State) =
    function
    | Conc(species,conc) -> state.Add(species,conc)
    | Step commList -> List.fold command state commList

and command (state:State) =
    function
    | Module md -> mdl state md
    | Conditional cd -> cond state cd 
    | Rxn (xs1,xs2,n) -> state

and mdl (state:State) =
    function
    | Ld(A,B) -> state.Add(B,state.Item A)
    | Add(A,B,C) -> state.Add(C, (state.Item A) + (state.Item B))
    | Sub(A,B,C) -> function
                    | Sub(A,B,C) when ((state.Item A) > (state.Item B)) -> state.Add(C, (state.Item A) - (state.Item B))
                    | _ -> state.Add(C, 0)
    | Mul(A,B,C) -> state
    | Div(A,B,C) -> state
    | Sqrt(A,B) -> state
    | Cmp(A,B) -> state

and cond (state:State) = 
    function
    | IfGT cmdList -> List.fold command state cmdList
    | IfGE cmdList -> List.fold command state cmdList
    | IfEQ cmdList -> List.fold command state cmdList
    | IfLT cmdList -> List.fold command state cmdList
    | IfLE cmdList -> List.fold command state cmdList

let ast1 = [Conc("a", 32); Conc("b", 12)];;

printf "%A" (interpreter (Map.ofList [("a",0); ("b",0)]) ast1);;
