(*
    Author: Kristine Maria Klok Jørgensen
*)

namespace Tests

open Helpers

open NUnit.Framework

module InterpreterTests =

    let a = 10.0
    let b = 5.0
    let asqrt = sqrt (a)

    let ld = sprintf "crn = { conc[a, %A], conc[b, %A], step[{ ld[a, b] }] };" a b
    let ldExpect = (Map.ofList [ ("a", a); ("b", a) ])
    let add = sprintf "crn = { conc[a, %A], conc[b, %A], step[{ add[a, b, c] }] };" a b
    let addExpect = (Map.ofList [ ("c", a + b) ])
    let sub = sprintf "crn = { conc[a, %A], conc[b, %A], step[{ sub[a, b, c] }] };" a b
    let subExpect = (Map.ofList [ ("c", a - b) ])
    let mul = sprintf "crn = { conc[a, %A], conc[b, %A], step[{ mul[a, b, c] }] };" a b
    let mulExpect = (Map.ofList [ ("c", a * b) ])
    let div = sprintf "crn = { conc[a, %A], conc[b, %A], step[{ div[a, b, c] }] };" a b
    let divExpect = (Map.ofList [ ("c", a / b) ])
    let sqrt = sprintf "crn = { conc[a, %A], step[{ sqrt[a, b] }] };" a
    let sqrtExpect = (Map.ofList [ ("b", asqrt) ])
    let cmp = sprintf "crn = { conc[a, %A], conc[b, %A], step [{ cmp[a, b] }] };" a b
    let cmpExpect = (Map.ofList [ ("Cmp", 1.0) ])


    [<TestFixture>]
    type InterpreterTestClass() =

        [<Test>]
        member _.ldTest() =
            Assert.True(interpreterOutputTest ld ldExpect)

        [<Test>]
        member _.addTest() =
            Assert.True(interpreterOutputTest add addExpect)

        [<Test>]
        member _.subTest() =
            Assert.True(interpreterOutputTest sub subExpect)

        [<Test>]
        member _.mulTest() =
            Assert.True(interpreterOutputTest mul mulExpect)

        [<Test>]
        member _.divTest() =
            Assert.True(interpreterOutputTest div divExpect)

        [<Test>]
        member _.sqrtTest() =
            Assert.True(interpreterOutputTest sqrt sqrtExpect)

        [<Test>]
        member _.cmpTest() =
            Assert.True(interpreterOutputTest cmp cmpExpect)
