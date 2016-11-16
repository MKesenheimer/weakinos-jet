#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 10:55
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub19(4), Sub399(3,2), Sub402(3,2)
        ComplexType Sub398(3,2), Sub401(3,2)
        ComplexType Sub389(3,2), Sub392(3,2)
        ComplexType Sub390(3,2), Sub393(3,2)
        ComplexType Sub20(3,4,2), Sub26(3,4,2)
        ComplexType Sub28(3,4,2), Sub29(3,4,2)
        ComplexType Sub22(3,4,2), Sub24(3,4,2)
        ComplexType Sub23(3,4,2)
        common /varXs/ Sub19, Sub399, Sub402, Sub398, Sub401, Sub389
        common /varXs/ Sub392, Sub390, Sub393, Sub20, Sub26, Sub28
        common /varXs/ Sub29, Sub22, Sub24, Sub23

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub395(3,3,3,3,4,4)
        ComplexType Sub404(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub395, Sub404, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F4, F6, F12, F15, F13, F14, F16, F9, F17, F10
        HelType Sub21(HelDim(3),3)
        HelType Sub27(HelDim(3),4,2)
        HelType Sub30(HelDim(3),4,2)
        HelType Sub25(HelDim(3),4,2)
        HelType Sub31(HelDim(3),3,2,2)
        HelType Sub32(HelDim(3),3,2,2)
        common /varXh/ F4, F6, F12, F15, F13, F14, F16, F9, F17, F10
        common /varXh/ Sub21, Sub27, Sub30, Sub25, Sub31, Sub32

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif