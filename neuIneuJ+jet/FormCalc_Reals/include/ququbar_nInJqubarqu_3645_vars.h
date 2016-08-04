#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:38
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub18(4), Sub400(3,2), Sub399(3,2)
        ComplexType Sub396(3,2), Sub398(3,2)
        ComplexType Sub16(3,4,2), Sub23(3,4,2)
        ComplexType Sub22(3,4,2), Sub25(3,4,2)
        ComplexType Sub27(3,4,2), Sub26(3,4,2)
        ComplexType Sub19(3,4,2), Sub20(3,4,2)
        common /varXs/ Sub18, Sub400, Sub399, Sub396, Sub398, Sub16
        common /varXs/ Sub23, Sub22, Sub25, Sub27, Sub26, Sub19
        common /varXs/ Sub20

        ComplexType Opt1(3,3,3,3)
        ComplexType Opt2(3,3,3,3)
        ComplexType Sub402(3,3,3,3,4,4)
        ComplexType Sub403(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Opt2, Sub402, Sub403, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F4, F5, F11, F14, F12, F13, F16, F8, F15, F9
        HelType Sub17(HelDim(3),3)
        HelType Sub24(HelDim(3),4,2)
        HelType Sub28(HelDim(3),4,2)
        HelType Sub21(HelDim(3),4,2)
        HelType Sub29(HelDim(3),3,2,2)
        HelType Sub30(HelDim(3),3,2,2)
        common /varXh/ F4, F5, F11, F14, F12, F13, F16, F8, F15, F9
        common /varXh/ Sub17, Sub24, Sub28, Sub21, Sub29, Sub30

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /ququbar_nInJqubarqu_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
