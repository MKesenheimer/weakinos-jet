#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 10:08
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub6(4), Sub304(3,2)
        ComplexType Sub308(3,2), Sub312(3,2)
        ComplexType Sub315(3,2), Sub311(3,2)
        ComplexType Sub314(3,2), Sub306(3,2)
        ComplexType Sub309(3,2), Sub16(3,4,2)
        ComplexType Sub17(3,4,2), Sub13(3,4,2)
        ComplexType Sub14(3,4,2)
        common /varXs/ Sub2, Sub6, Sub304, Sub308, Sub312, Sub315
        common /varXs/ Sub311, Sub314, Sub306, Sub309, Sub16, Sub17
        common /varXs/ Sub13, Sub14

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub320(3,3,4)
        ComplexType Sub321(3,3,3,3,4,4)
        ComplexType Sub326(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub320, Sub321, Sub326, S, T, T14
        common /varXa/ T15, U, T24, T25, S34, S35, S45

        HelType F7, F5, F12, F13, F14, F15, F16, F11, F17, F9
        HelType Sub18(HelDim(3),4,2)
        HelType Sub15(HelDim(3),4,2)
        HelType Sub19(HelDim(3),3,2,2)
        HelType Sub21(HelDim(3),3,2,2)
        HelType Sub20(HelDim(3),3,2,2)
        HelType Sub22(HelDim(3),3,2,2)
        common /varXh/ F7, F5, F12, F13, F14, F15, F16, F11, F17, F9
        common /varXh/ Sub18, Sub15, Sub19, Sub21, Sub20, Sub22

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqdbar_nInJqdbarqdbar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif