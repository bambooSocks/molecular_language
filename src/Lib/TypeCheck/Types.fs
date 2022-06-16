namespace TypeCheck

open Parser.Types

module Types =
    type CrnError =
        | NegativeConcentration of TSpecies * TNumber
        | CompareMissing of TConditional
        | CyclicModuleDependency of TModule
        | CyclicStepDependency of TSpecies list

    type CheckResult = bool * CrnError list
    type InputOutput = TSpecies list * TSpecies list
