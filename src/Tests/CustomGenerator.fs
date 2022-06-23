(*
    Author: Matej Majtan
*)

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

    and genStep allowCond n =
        gen {
            match n with
            | 0 -> return []
            | n ->
                let! cmd_n = Gen.choose (1, 6)
                let! cmds = genCommand allowCond false cmd_n
                let! hasCmp = Gen.constant (List.exists isCmp cmds)
                let! rest = genStep hasCmp (n - 1)
                let! step = Gen.constant (Step cmds)
                return step :: rest
        }

    and genCommand allowCond excludeCmp n =
        gen {
            match n with
            | 0 -> return []
            | n ->
                let! cmd =
                    if allowCond then
                        genAnyCommand excludeCmp
                    else
                        genModuleCommand excludeCmp

                let! cmdIsCmp = Gen.constant (isCmp cmd)
                let! rest = genCommand allowCond (cmdIsCmp || excludeCmp) (n - 1)
                return cmd :: rest
        }

    and genModuleCommand excludeCmp =
        gen {
            let! m = genModule excludeCmp
            return Module m
        }

    and genAnyCommand excludeCmp =
        Gen.frequency [ (10, genModuleCommand excludeCmp)
                        (1, (Gen.map (fun c -> Conditional c) (genConditional excludeCmp))) ]

    and genCommandsForCond excludeCmp =
        gen {
            let! n = Gen.choose (1, 6)
            return! genCommand false excludeCmp n
        }

    and genConditional excludeCmp =
        Gen.oneof [ Gen.map (fun cmds -> IfGT cmds) (genCommandsForCond excludeCmp)
                    Gen.map (fun cmds -> IfGE cmds) (genCommandsForCond excludeCmp)
                    Gen.map (fun cmds -> IfEQ cmds) (genCommandsForCond excludeCmp)
                    Gen.map (fun cmds -> IfLT cmds) (genCommandsForCond excludeCmp)
                    Gen.map (fun cmds -> IfLE cmds) (genCommandsForCond excludeCmp) ]

    and genModule excludeCmp =
        let selectCmpGen =
            if excludeCmp then
                []
            else
                [ Gen.map2 (fun s1 s2 -> Cmp(s1, s2)) genSpecies genSpecies ]

        Gen.oneof (
            selectCmpGen
            @ [ Gen.map2 (fun s1 s2 -> Ld(s1, s2)) genSpecies genSpecies
                Gen.map2 (fun s1 s2 -> Sqrt(s1, s2)) genSpecies genSpecies
                Gen.map3 (fun s1 s2 s3 -> Add(s1, s2, s3)) genSpecies genSpecies genSpecies
                Gen.map3 (fun s1 s2 s3 -> Sub(s1, s2, s3)) genSpecies genSpecies genSpecies
                Gen.map3 (fun s1 s2 s3 -> Mul(s1, s2, s3)) genSpecies genSpecies genSpecies
                Gen.map3 (fun s1 s2 s3 -> Div(s1, s2, s3)) genSpecies genSpecies genSpecies ]
        )

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

    let genRandomCommands =
        gen {
            let! n = Gen.choose (1, 6)

            let! allowCond =
                Gen.oneof [ Gen.constant true
                            Gen.constant false ]

            let! excludeCmp =
                Gen.oneof [ Gen.constant true
                            Gen.constant false ]

            return! genCommand allowCond excludeCmp n

        }

    type customGenerator =
        static member RootList() =
            { new Arbitrary<TRoot list>() with
                override _.Generator = genRootList }

        static member Rxns() =
            { new Arbitrary<TRxn list>() with
                override _.Generator = genRxns }

        static member Conc() =
            { new Arbitrary<TConc>() with
                override x.Generator = genConcValues }

        static member Cmds() =
            { new Arbitrary<TCommand list>() with
                override _.Generator = genRandomCommands }
