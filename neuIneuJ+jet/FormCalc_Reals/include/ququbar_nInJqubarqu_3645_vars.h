#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 26-Sep-2016 14:19
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub17(4), Sub405(3,2), Sub408(3,2)
        ComplexType Sub404(3,2), Sub407(3,2)
        ComplexType Sub387(3,2), Sub391(3,2)
        ComplexType Sub389(3,2), Sub392(3,2)
        ComplexType Sub15(3,4,2), Sub22(3,4,2)
        ComplexType Sub21(3,4,2), Sub24(3,4,2)
        ComplexType Sub26(3,4,2), Sub25(3,4,2)
        ComplexType Sub18(3,4,2), Sub19(3,4,2)
        common /varXs/ Sub17, Sub405, Sub408, Sub404, Sub407, Sub387
        common /varXs/ Sub391, Sub389, Sub392, Sub15, Sub22, Sub21
        common /varXs/ Sub24, Sub26, Sub25, Sub18, Sub19

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub401(3,3,3,3,4,4)
        ComplexType Sub410(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub401, Sub410, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F3, F4, F11, F14, F12, F13, F15, F8, F16, F10
        HelType Sub16(HelDim(3),3)
        HelType Sub23(HelDim(3),4,2)
        HelType Sub27(HelDim(3),4,2)
        HelType Sub20(HelDim(3),4,2)
        HelType Sub28(HelDim(3),3,2,2)
        HelType Sub29(HelDim(3),3,2,2)
        common /varXh/ F3, F4, F11, F14, F12, F13, F15, F8, F16, F10
        common /varXh/ Sub16, Sub23, Sub27, Sub20, Sub28, Sub29

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
