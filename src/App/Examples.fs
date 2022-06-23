(*
    Authors: Matej Majtan, Andrei Redis
*)

namespace App

module Examples =
    // Sample CRN programs
    let gcd =
        """
        crn = {
            conc[a,32],
            conc[b,12],
            step[{
                ld [a, atmp],
                ld [b, btmp],
                cmp[a,b]
            }],
            step[{
                ifGT[{ sub[atmp,btmp,a] }],
                ifLT[{ sub[btmp,atmp,b] }]
            }]
        };
        """
    let gcdReactions = // Reactions compiled from GCD program
        """
        rxn[osc3 + a, osc3 + a + atmp, 1],
        rxn[osc3 + atmp, osc3, 1],
        rxn[osc3 + b, osc3 + b + btmp, 1],
        rxn[osc3 + btmp, osc3, 1],
        rxn[osc3 + cmpGT + b, osc3 + cmpLT + b, 1],
        rxn[osc3 + cmpLT + a, osc3 + cmpGT + a, 1],
        rxn[osc6 + cmpGT + cmpLT, osc6 + cmpLT + cmpB, 1],
        rxn[osc6 + cmpB + cmpLT, osc6 + cmpLT + cmpLT, 1],
        rxn[osc6 + cmpLT + cmpGT, osc6 + cmpGT + cmpB, 1],
        rxn[osc6 + cmpB + cmpGT, osc6 + cmpGT + cmpGT, 1],
        rxn[osc9 + cmpLT + btmp, osc9 + cmpLT + btmp + b, 1],
        rxn[osc9 + cmpLT + atmp, osc9 + cmpLT + atmp + hbtmpatmpb, 1],
        rxn[osc9 + cmpLT + b, osc9 + cmpLT, 1],
        rxn[osc9 + cmpLT + b + hbtmpatmpb, osc9 + cmpLT, 1],
        rxn[osc9 + cmpGT + atmp, osc9 + cmpGT + atmp + a, 1],
        rxn[osc9 + cmpGT + btmp, osc9 + cmpGT + btmp + hatmpbtmpa, 1],
        rxn[osc9 + cmpGT + a, osc9 + cmpGT, 1],
        rxn[osc9 + cmpGT + a + hatmpbtmpa, osc9 + cmpGT, 1],
        rxn[osc1 + osc2, osc2 + osc2, 1],
        rxn[osc2 + osc3, osc3 + osc3, 1],
        rxn[osc3 + osc4, osc4 + osc4, 1],
        rxn[osc4 + osc5, osc5 + osc5, 1],
        rxn[osc5 + osc6, osc6 + osc6, 1],
        rxn[osc6 + osc7, osc7 + osc7, 1],
        rxn[osc7 + osc8, osc8 + osc8, 1],
        rxn[osc8 + osc9, osc9 + osc9, 1],
        rxn[osc9 + osc1, osc1 + osc1, 1]
        """
    let discrete_counter =
        """
        crn = {
            conc[c, 3], conc[ cInitial, 3],
            conc[one, 1], conc[zero, 0],
            step [{
                sub[c,one,cnext ],
                cmp[c,zero ]
            }],
            step [{
                ifGT[{ ld[cnext,c] }],
                ifLE[{ ld[ cInitial ,c] }] 
            }]
        }
        """

    let factorial =
        """
        crn = {
            conc[f ,1] , conc[one, 1], conc[i, f0],
            step[{
                cmp[i, one],
                mul[f, i, fnext],
                sub[i, one, inext]
            }],
            step[{
                ifGT[{
                    ld[inext, i],
                    ld[fnext, f]
                }]
            }]
        }
        """

    let division =
        """
        crn = {
            conc[a, a0], conc[b, b0], conc[one, 1],
            step[{
                cmp[a, b]
            }],
            step[{
                ifGE[{
                    sub[a, b, anext],
                    add[q, one, qnext]
                }]
            }],
            step[{
                ifGE[{
                    ld[anext,a],
                    ld[qnext, q]
                }]
                ifLT[{ld[a, r]}]
            }]
        }
        """

    let int_sqrt =
        """
        crn = { 
            conc[one, 1], conc[n, n0],
            step [{
                add[z, one, znext],
            }], 
            step [{
                mul[znext, znext, zpow],
            }], 
            step [{
                cmp[zpow, n]
            }], 
            step [{
                ifLT[{ld[znext, z]}],
                ifGE[{ld[z, out]}]
            }]
        }
        """

    let approx_euler =
        """
        crn = {
            conc[e, 1], conc[element, 1],
            conc[divisor, 1], conc[one, 1],
            conc[divisorMultiplier, 1],
            step [{
                div[element, divisor, elementNext],
                add[ divisor, one, divisorNext],
                add[e, elementNext, eNext]
            }],
            step [{
                ld[elementNext, element],
                ld[divisorNext, divisor],
                ld[eNext, e]
            }]
        }
        """

    let approx_pi =
        """
        crn={
            conc[four, 4],
            conc[divisor1 , 1],
            conc[divisor2 , 3],
            conc[pi, 0],
            step [{
                div[four, divisor1, factor1],
                add[divisor1, four, divisor1Next],
                div[four, divisor2, factor2],
                add[divisor2, four, divisor2Next]
            }],
            step[{
                sub[factor1, factor2, factor]
            }],
            step[{
                add[pi, factor, piNext]
            }],
            step [{
                ld[divisor1Next, divisor1],
                ld[divisor2Next, divisor2],
                ld[piNext, pi]
            }]
        }
        """

    let small = "crn = { conc[a, 32], conc[b, 12] };"

    let small2 =
        "crn = { conc[a, 32], conc[b, 12], step[{ ifGT[{ sub[atmp,btmp,a] }] }] };"
