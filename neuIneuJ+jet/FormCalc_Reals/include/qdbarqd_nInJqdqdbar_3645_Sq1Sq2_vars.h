#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 9:55
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub16(4), Sub19(4), Sub329(3,2)
        ComplexType Sub332(3,2), Sub327(3,2)
        ComplexType Sub331(3,2), Sub321(3,2)
        ComplexType Sub324(3,2), Sub322(3,2)
        ComplexType Sub325(3,2), Sub26(3,4,2)
        ComplexType Sub27(3,4,2), Sub20(3,4,2)
        ComplexType Sub21(3,4,2), Sub15(3,4,2)
        ComplexType Sub17(3,4,2)
        common /varXs/ Sub16, Sub19, Sub329, Sub332, Sub327, Sub331
        common /varXs/ Sub321, Sub324, Sub322, Sub325, Sub26, Sub27
        common /varXs/ Sub20, Sub21, Sub15, Sub17

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub328(3,3,3,3,4,4)
        ComplexType Sub334(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub328, Sub334, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F6, F4, F12, F14, F13, F15, F16, F9, F17, F11
        HelType Sub25(HelDim(3),3)
        HelType Sub28(HelDim(3),4,2)
        HelType Sub22(HelDim(3),4,2)
        HelType Sub18(HelDim(3),4,2)
        HelType Sub23(HelDim(3),3,2,2)
        HelType Sub24(HelDim(3),3,2,2)
        common /varXh/ F6, F4, F12, F14, F13, F15, F16, F9, F17, F11
        common /varXh/ Sub25, Sub28, Sub22, Sub18, Sub23, Sub24

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqd_nInJqdqdbar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif