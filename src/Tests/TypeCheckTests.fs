(*
    Author: Matej Majtan
*)

namespace Tests

open NUnit.Framework

open Parser.Types
open TypeCheck.Types
open TypeCheck.TypeCheck

module TypeCheckTests =

    [<TestFixture>]
    type TypeCheckTestClass() =

        [<Test>]
        member _.typeCheckPass_AstCorrect() =
            let ast =
                [ Conc("a", 32)
                  Conc("b", 12)
                  Step(
                      [ Module(Ld("a", "atmp"))
                        Module(Ld("b", "btmp"))
                        Module(Cmp("a", "b")) ]
                  )
                  Step(
                      [ Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.True(res)
            Assert.IsEmpty(errors)

        [<Test>]
        member _.typeCheckFail_ConcStepWrongOrder() =
            let ast =
                [ Conc("a", 32)
                  Step(
                      [ Module(Ld("a", "atmp"))
                        Module(Ld("b", "btmp"))
                        Module(Cmp("a", "b")) ]
                  )
                  Conc("b", 12)
                  Step(
                      [ Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ ConcStepWrongOrder ])

        [<Test>]
        member _.typeCheckFail_CompareMissing() =
            let conditional1 = IfGT([ Module(Sub("atmp", "btmp", "a")) ])
            let conditional2 = IfGE([ Module(Sub("atmp", "btmp", "a")) ])
            let conditional3 = IfEQ([ Module(Sub("atmp", "btmp", "a")) ])
            let conditional4 = IfLT([ Module(Sub("atmp", "btmp", "a")) ])
            let conditional5 = IfLE([ Module(Sub("atmp", "btmp", "a")) ])

            let ast =
                [ Conc("a", 32)
                  Conc("b", 12)
                  Step(
                      [ Module(Ld("a", "atmp"))
                        Module(Ld("b", "btmp")) ]
                  )
                  Step(
                      [ Conditional(conditional1)
                        Conditional(conditional2)
                        Conditional(conditional3)
                        Conditional(conditional4)
                        Conditional(conditional5) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ CompareMissing conditional1 ])
            Assert.That(errors, Is.SupersetOf [ CompareMissing conditional2 ])
            Assert.That(errors, Is.SupersetOf [ CompareMissing conditional3 ])
            Assert.That(errors, Is.SupersetOf [ CompareMissing conditional4 ])
            Assert.That(errors, Is.SupersetOf [ CompareMissing conditional5 ])

        [<Test>]
        member _.typeCheckFail_NegativeConcentration() =
            let conc1 = ("a", -32.0)
            let conc2 = ("b", -12.0)

            let ast =
                [ Conc conc1
                  Conc conc2
                  Step(
                      [ Module(Ld("a", "atmp"))
                        Module(Ld("b", "btmp"))
                        Module(Cmp("a", "b")) ]
                  )
                  Step(
                      [ Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ NegativeConcentration conc1 ])
            Assert.That(errors, Is.SupersetOf [ NegativeConcentration conc2 ])

        [<Test>]
        member _.typeCheckFail_CyclicModuleDependency() =
            let ld1 = Ld("a", "a")
            let ld2 = Ld("b", "b")
            let add = Add("a", "b", "a")
            let sub = Sub("a", "b", "a")
            let mul = Mul("a", "b", "a")
            let div = Div("a", "b", "a")
            let sqrt = Sqrt("a", "a")
            let cmp = Cmp("a", "a")

            let ast =
                [ Conc("a", 32)
                  Conc("b", 12)
                  Step(
                      [ Module(ld1)
                        Module(ld2)
                        Module(add)
                        Module(sub)
                        Module(mul)
                        Module(div)
                        Module(sqrt)
                        Module(cmp) ]
                  )
                  Step(
                      [ Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency ld1 ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency ld2 ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency add ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency sub ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency mul ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency div ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency sqrt ])
            Assert.That(errors, Is.SupersetOf [ CyclicModuleDependency cmp ])

        [<Test>]
        member _.typeCheckFail_CyclicStepDependency() =
            let spA = "a"
            let spB = "b"

            let ast =
                [ Conc("a", 32)
                  Conc("b", 12)
                  Step(
                      [ Module(Ld(spA, "atmp"))
                        Module(Ld(spB, "btmp"))
                        Module(Add("c", "d", spA))
                        Module(Sub("c", "d", spA))
                        Module(Mul("c", "d", spA))
                        Module(Div("c", "d", spA))
                        Module(Sqrt(spA, spB))
                        Module(Cmp(spA, spB)) ]
                  )
                  Step(
                      [ Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ CyclicStepDependency [ spA; spB ] ])

        [<Test>]
        member _.typeCheckFail_MultipleComparesInOneStep() =
            let ast =
                [ Conc("a", 32)
                  Conc("b", 12)
                  Step(
                      [ Module(Ld("a", "atmp"))
                        Module(Ld("b", "btmp"))
                        Module(Cmp("a", "b"))
                        Module(Cmp("a", "b")) ]
                  )
                  Step(
                      [ Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ MultipleComparesInOneStep ])

        [<Test>]
        member _.typeCheckFail_SameOutputInStep() =
            let spC = "c"
            let spD = "d"

            let ast =
                [ Conc("a", 32)
                  Conc("b", 12)
                  Step(
                      [ Module(Ld("a", "atmp"))
                        Module(Ld("b", "btmp"))
                        Module(Ld("a", spC))
                        Module(Ld("b", spC))
                        Module(Cmp("a", "b")) ]
                  )
                  Step(
                      [ Module(Ld("a", spD))
                        Module(Ld("b", spD))
                        Module(Ld(spC, spD))
                        Conditional(IfGT([ Module(Sub("atmp", "btmp", "a")) ]))
                        Conditional(IfLT([ Module(Sub("btmp", "atmp", "b")) ])) ]
                  ) ]

            let (res, errors) = check ast
            Assert.False(res)
            Assert.That(errors, Is.SupersetOf [ SameOutputInStep [ spC ] ])
            Assert.That(errors, Is.SupersetOf [ SameOutputInStep [ spD ] ])
