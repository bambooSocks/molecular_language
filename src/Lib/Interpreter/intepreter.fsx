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
and State = Map<TSpecies, float>

let rec rootL (state:State) =
    function
    | []  -> state 
    | rootList -> List.fold root state rootList

// build an infinite sequence that takes an input a state, and folds it over the whole 

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

let rec interpret (state:State) rtList =
    seq {
        let state' = rootL state rtList
        yield state'
        yield! interpret state' rtList }

//let ast1 = [Conc("a", 32); Conc("b", 12)];;

let ast2 = [Conc("a", 32); Conc("b", 12);
 Step
   [Module (Ld ("a", "atmp"));
    Module (Ld ("b", "btmp"));
    Module (Cmp ("a", "b"))];
 Step
   [Conditional (IfGT [Module (Sub ("atmp", "btmp", "a"))]);
    Conditional (IfLT [Module (Sub ("btmp", "atmp", "b"))])]];;

printf "%A" (interpret (Map.ofList [("Cmp", 1.0); ("a", 20.0); ("atmp", 32.0); ("b", 12.0); ("btmp", 12.0)]) ast2);;