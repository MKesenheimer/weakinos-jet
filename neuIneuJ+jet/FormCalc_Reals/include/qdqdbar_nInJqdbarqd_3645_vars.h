#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 26-Sep-2016 14:07
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub16(4), Sub21(4), Sub335(3,2)
        ComplexType Sub338(3,2), Sub334(3,2)
        ComplexType Sub337(3,2), Sub317(3,2)
        ComplexType Sub321(3,2), Sub319(3,2)
        ComplexType Sub322(3,2), Sub20(3,4,2)
        ComplexType Sub22(3,4,2), Sub24(3,4,2)
        ComplexType Sub25(3,4,2), Sub17(3,4,2)
        ComplexType Sub18(3,4,2)
        common /varXs/ Sub16, Sub21, Sub335, Sub338, Sub334, Sub337
        common /varXs/ Sub317, Sub321, Sub319, Sub322, Sub20, Sub22
        common /varXs/ Sub24, Sub25, Sub17, Sub18

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub331(3,3,3,3,4,4)
        ComplexType Sub340(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub331, Sub340, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F3, F5, F11, F14, F12, F13, F15, F8, F16, F10
        HelType Sub15(HelDim(3),3)
        HelType Sub23(HelDim(3),4,2)
        HelType Sub26(HelDim(3),4,2)
        HelType Sub19(HelDim(3),4,2)
        HelType Sub27(HelDim(3),3,2,2)
        HelType Sub28(HelDim(3),3,2,2)
        common /varXh/ F3, F5, F11, F14, F12, F13, F15, F8, F16, F10
        common /varXh/ Sub15, Sub23, Sub26, Sub19, Sub27, Sub28

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdbarqd_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
