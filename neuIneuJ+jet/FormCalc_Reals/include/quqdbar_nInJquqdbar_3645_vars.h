#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:33
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub12(4), Sub15(4), Sub197(3,2)
        ComplexType Sub200(3,2), Sub16(3,4,2)
        ComplexType Sub17(3,4,2), Sub11(3,4,2)
        ComplexType Sub13(3,4,2)
        common /varXs/ Sub12, Sub15, Sub197, Sub200, Sub16, Sub17
        common /varXs/ Sub11, Sub13

        ComplexType Opt1(3,3,3,3), Sub201(3,3,4)
        ComplexType Sub202(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub201, Sub202, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F3, F6, F11, F14, F12, F13, F15, F8, F16, F10
        HelType Sub18(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub19(HelDim(3),3,2,2)
        HelType Sub20(HelDim(3),3,2,2)
        common /varXh/ F3, F6, F11, F14, F12, F13, F15, F8, F16, F10
        common /varXh/ Sub18, Sub14, Sub19, Sub20

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /quqdbar_nInJquqdbar_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
