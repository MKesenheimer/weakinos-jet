#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:39
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub17(4), Sub20(4), Sub313(3,2)
        ComplexType Sub312(3,2), Sub309(3,2)
        ComplexType Sub311(3,2), Sub21(3,4,2)
        ComplexType Sub22(3,4,2), Sub24(3,4,2)
        ComplexType Sub25(3,4,2), Sub16(3,4,2)
        ComplexType Sub18(3,4,2)
        common /varXs/ Sub17, Sub20, Sub313, Sub312, Sub309, Sub311
        common /varXs/ Sub21, Sub22, Sub24, Sub25, Sub16, Sub18

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub315(3,3,3,3,4,4)
        ComplexType Sub316(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub315, Sub316, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F3, F5, F11, F14, F13, F12, F15, F8, F16, F10
        HelType Sub15(HelDim(3),3)
        HelType Sub23(HelDim(3),4,2)
        HelType Sub26(HelDim(3),4,2)
        HelType Sub19(HelDim(3),4,2)
        HelType Sub27(HelDim(3),3,2,2)
        HelType Sub28(HelDim(3),3,2,2)
        common /varXh/ F3, F5, F11, F14, F13, F12, F15, F8, F16, F10
        common /varXh/ Sub15, Sub23, Sub26, Sub19, Sub27, Sub28

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqd_nInJqdqdbar_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
