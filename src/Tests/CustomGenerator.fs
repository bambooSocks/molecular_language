namespace Tests

#nowarn "40"

open FsCheck
open Parser.Types

module CustomGenerator =

    let isCmp =
        function
        | Module m ->
            match m with
            | Cmp _ -> true
            | _ -> false
        | _ -> false

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
            let! steps = genStep false step_num
            return concs @ steps
        }

    and genConc = Gen.map Conc genConcValues

    and genConcValues: Gen<TConc> =
        gen {
            let! fl =
                Arb.generate<NormalFloat>
                |> Gen.where (fun f -> (float f) >= 0.0)

            let! s = genSpecies
            return (s, float fl)
        }

    and genStep wasCmp n =
        gen {
            match n with
            | 0 -> return []
            | n ->
                let! isCurrentCmp, cmds = genCommands wasCmp
                let! rest = genStep isCurrentCmp (n - 1)
                let! step = Gen.constant (Step cmds)
                return step :: rest
        }

    and genCommands wasCmp =
        gen {
            let! n = Gen.choose (1, 6)

            if wasCmp then
                let! cmds = Gen.listOfLength n genAnyCommand
                return (wasCmp, cmds)
            else
                let! cmds = Gen.listOfLength n genModuleCommand
                let! isCurrentCmp = Gen.constant (List.exists isCmp cmds)
                return (isCurrentCmp, cmds)
        }

    and genModuleCommand =
        gen {
            let! m = genModule
            return Module m
        }

    and genAnyCommand =
        Gen.frequency [ (10, genModuleCommand)
                        (1, (Gen.map (fun c -> Conditional c) genConditional)) ]

    and genCommandsForCond =
        gen {
            let! _, cmds = genCommands true
            return cmds
        }

    and genConditional =
        Gen.oneof [ Gen.map (fun cmds -> IfGT cmds) genCommandsForCond
                    Gen.map (fun cmds -> IfGE cmds) genCommandsForCond
                    Gen.map (fun cmds -> IfEQ cmds) genCommandsForCond
                    Gen.map (fun cmds -> IfLT cmds) genCommandsForCond
                    Gen.map (fun cmds -> IfLE cmds) genCommandsForCond ]

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

        static member Concs() =
            { new Arbitrary<TConc>() with
                override x.Generator = genConcValues }
