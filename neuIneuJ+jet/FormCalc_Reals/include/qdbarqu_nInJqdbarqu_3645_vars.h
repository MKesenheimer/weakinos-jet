#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 18:46
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub11(4), Sub16(4), Sub15(3,4,2)
        ComplexType Sub17(3,4,2), Sub12(3,4,Sfe8)
        ComplexType Sub13(3,4,Sfe8)
        common /varXs/ Sub11, Sub16, Sub15, Sub17, Sub12, Sub13

        ComplexType Opt1(3,3,3,3), Sub203(3,3,4)
        ComplexType Sub204(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub203, Sub204, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F5, F3, F11, F14, F12, F13, F15, F8, F16, F9
        HelType Sub18(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,Sfe8)
        HelType Sub19(HelDim(3),3,2,Sfe8)
        HelType Sub20(HelDim(3),3,2,Sfe8)
        common /varXh/ F5, F3, F11, F14, F12, F13, F15, F8, F16, F9
        common /varXh/ Sub18, Sub14, Sub19, Sub20

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqu_nInJqdbarqu_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
