#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 19:01
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub15(4), Sub16(3,4,2)
        ComplexType Sub22(3,4,2), Sub24(3,4,Sfe8)
        ComplexType Sub25(3,4,Sfe8), Sub18(3,4,2)
        ComplexType Sub20(3,4,2), Sub19(3,4,2)
        common /varXs/ Sub15, Sub16, Sub22, Sub24, Sub25, Sub18
        common /varXs/ Sub20, Sub19

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub348(3,3,3,3,4,4)
        ComplexType Sub349(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub348, Sub349, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F3, F4, F11, F14, F12, F13, F16, F8, F15, F10
        HelType Sub17(HelDim(3),3)
        HelType Sub23(HelDim(3),4,2)
        HelType Sub26(HelDim(3),4,Sfe8)
        HelType Sub21(HelDim(3),4,2)
        HelType Sub27(HelDim(3),3,2,Sfe8)
        HelType Sub28(HelDim(3),3,2,Sfe8)
        common /varXh/ F3, F4, F11, F14, F12, F13, F16, F8, F15, F10
        common /varXh/ Sub17, Sub23, Sub26, Sub21, Sub27, Sub28

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
