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

    let discrete_counter =
        """
        crn = {
            conc[c,c0], conc[ cInitial ,c0],
            conc[one,1], conc[zero,0],
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
                mul[znext, znext, zpow],
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
                add[divisor2, four, divisor2Next],
                sub[factor1, factor2, factor],
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
