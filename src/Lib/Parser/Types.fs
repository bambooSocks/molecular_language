namespace Parser

module Types =
    type TRoot =
        | Conc of TConc
        | Step of TStep

    and TConc = TSpecies * TNumber
    
    and TStep = TCommand list

    and TCommand =
        | Module of TModule
        | Conditional of TConditional

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

    type State = Map<TSpecies, float>
    type TRxn = Rxn of TSpecies list * TSpecies list * float
