namespace Parser

module Types =
    type TCrn = Crn of TRootList

    and TRootList =
        | Root of TRoot
        | Root_Seq of TRoot * TRootList

    and TRoot =
        | Conc of TSpecies * TNumber
        | Step of TCommandList

    and TCommandList =
        | Command of TCommand
        | Command_Seq of TCommand * TCommandList

    and TCommand =
        | Module of TModule
        | Conditional of TConditional
    //    | Rxn of TExpr * TExpr * TNumber
    and TModule =
        | Ld of TSpecies * TSpecies
        | Add of TSpecies * TSpecies * TSpecies
        | Sub of TSpecies * TSpecies * TSpecies
        | Mul of TSpecies * TSpecies * TSpecies
        | Div of TSpecies * TSpecies * TSpecies
        | Sqrt of TSpecies * TSpecies
        | Cmp of TSpecies * TSpecies

    and TConditional =
        | IfGT of TCommandList
        | IfGE of TCommandList
        | IfEQ of TCommandList
        | IfLT of TCommandList
        | IfLE of TCommandList

    and TExpr =
        | SpExpr of TSpecies
        | SpExpr_Seq of TSpecies * TExpr

    and TSpecies = Species of string
    and TNumber = Number of int
