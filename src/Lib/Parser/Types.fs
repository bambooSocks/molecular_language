namespace Parser

module Types =
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
        | Rxn of TSpecies list * TSpecies list * float

    // Should use this up here ^
    //and TRxn = Rxn of TSpecies list * TSpecies list * float

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
    and State = Map<TSpecies, float>