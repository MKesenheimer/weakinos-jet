#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 9:33
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub17(4), Sub20(4), Sub339(3,2)
        ComplexType Sub342(3,2), Sub340(3,2)
        ComplexType Sub343(3,2), Sub329(3,2)
        ComplexType Sub332(3,2), Sub330(3,2)
        ComplexType Sub333(3,2), Sub21(3,4,2)
        ComplexType Sub22(3,4,2), Sub24(3,4,2)
        ComplexType Sub25(3,4,2), Sub16(3,4,2)
        ComplexType Sub18(3,4,2)
        common /varXs/ Sub17, Sub20, Sub339, Sub342, Sub340, Sub343
        common /varXs/ Sub329, Sub332, Sub330, Sub333, Sub21, Sub22
        common /varXs/ Sub24, Sub25, Sub16, Sub18

        ComplexType Opt1(3,3,3,3), Sub338(3,3,4)
        ComplexType Sub335(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub338, Sub335, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F4, F6, F12, F15, F13, F14, F16, F9, F17, F11
        HelType Sub15(HelDim(3),3)
        HelType Sub23(HelDim(3),4,2)
        HelType Sub26(HelDim(3),4,2)
        HelType Sub19(HelDim(3),4,2)
        HelType Sub27(HelDim(3),3,2,2)
        HelType Sub28(HelDim(3),3,2,2)
        common /varXh/ F4, F6, F12, F15, F13, F14, F16, F9, F17, F11
        common /varXh/ Sub15, Sub23, Sub26, Sub19, Sub27, Sub28

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdqdbar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif