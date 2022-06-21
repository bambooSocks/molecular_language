namespace Tests

#nowarn "40"

open FsCheck
open Parser.Types

module CustomGenerator =

    let charsSeqGen c1 c2 =
        seq {
            for c in c1..c2 do
                yield gen { return c }
        }

    let genChar =
        Gen.oneof [ gen { return! Gen.oneof (charsSeqGen 'a' 'z') }
                    gen { return! Gen.oneof (charsSeqGen 'A' 'Z') } ]

    let genSpecies =
        gen {
            let! i = Gen.choose (1, 6)
            let! cs = Gen.listOfLength i genChar
            let ss = List.map string cs
            return TSpecies(String.concat "" ss)
        }


    let rec genRootList =
        gen {
            let! conc_num = Gen.choose (1, 5)
            let! step_num = Gen.choose (1, 3)
            let! concs = Gen.listOfLength conc_num genConc
            let! steps = Gen.listOfLength step_num genStep
            return concs @ steps
        }

    and genConc = Gen.map2 (fun s n -> Conc(s, n)) genSpecies Arb.generate<int>

    and genStep = Gen.map (fun cmds -> Step cmds) genCommands

    and genCommands =
        gen {
            let! n = Gen.choose (1, 6)
            return! Gen.listOfLength n genCommand
        }

    and genCommand =
        Gen.frequency [ (10, (Gen.map (fun m -> Module m) genModule))
                        (1, (Gen.map (fun c -> Conditional c) genConditional)) ]

    and genConditional =
        Gen.oneof [ Gen.map (fun cmds -> IfGT cmds) genCommands
                    Gen.map (fun cmds -> IfGE cmds) genCommands
                    Gen.map (fun cmds -> IfEQ cmds) genCommands
                    Gen.map (fun cmds -> IfLT cmds) genCommands
                    Gen.map (fun cmds -> IfLE cmds) genCommands ]

    and genModule =
        Gen.oneof [ Gen.map2 (fun s1 s2 -> Ld(s1, s2)) genSpecies genSpecies
                    Gen.map2 (fun s1 s2 -> Cmp(s1, s2)) genSpecies genSpecies
                    Gen.map2 (fun s1 s2 -> Sqrt(s1, s2)) genSpecies genSpecies
                    Gen.map3 (fun s1 s2 s3 -> Add(s1, s2, s3)) genSpecies genSpecies genSpecies
                    Gen.map3 (fun s1 s2 s3 -> Sub(s1, s2, s3)) genSpecies genSpecies genSpecies
                    Gen.map3 (fun s1 s2 s3 -> Mul(s1, s2, s3)) genSpecies genSpecies genSpecies
                    Gen.map3 (fun s1 s2 s3 -> Div(s1, s2, s3)) genSpecies genSpecies genSpecies ]

    let genRxn =
        gen {
            let! s_num1 = Gen.choose (1, 5)
            let! s_num2 = Gen.choose (1, 5)
            let! s1 = Gen.listOfLength s_num1 genSpecies
            let! s2 = Gen.listOfLength s_num2 genSpecies
            let! f = Gen.map (fun i -> float (i) / 100.0) (Gen.choose (1, 10000))
            return Rxn(s1, s2, f)
        }

    let genRxns =
        gen {
            let! rxn_num = Gen.choose (1, 5)
            return! Gen.listOfLength rxn_num genRxn
        }

    type customGenerator =
        static member RootList() =
            { new Arbitrary<TRoot list>() with
                override x.Generator = genRootList }

        static member Rxns() =
            { new Arbitrary<TRxn list>() with
                override x.Generator = genRxns }
