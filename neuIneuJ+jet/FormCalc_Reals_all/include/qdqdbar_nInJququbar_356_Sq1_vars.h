#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 12:46
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub27(4), Sub20(4)
        ComplexType Sub200(3,2), Sub189(3,2)
        ComplexType Sub19(3,4,2), Sub21(3,4,2)
        ComplexType Sub28(3,4,2), Sub29(3,4,2)
        ComplexType Sub4(3,4,2), Sub5(3,4,2)
        common /varXs/ Sub3, Sub27, Sub20, Sub200, Sub189, Sub19
        common /varXs/ Sub21, Sub28, Sub29, Sub4, Sub5

        ComplexType Sub199(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub199, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F25, F20, F26, F18, F17, F19, F2, F5, F28, F27, F21
        HelType F22, F23, F24, F31, F32, F29, F30
        HelType Sub22(HelDim(3),4,2)
        HelType Sub30(HelDim(3),4,2)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub31(HelDim(3),3,2,2)
        HelType Sub25(HelDim(3),3,2,2)
        HelType Sub201(HelDim(3),3,3,4,2,2)
        HelType Sub23(HelDim(3)), Sub24(HelDim(3))
        HelType Sub32(HelDim(3),3,2,2)
        HelType Sub26(HelDim(3),3,2,2)
        common /varXh/ F25, F20, F26, F18, F17, F19, F2, F5, F28, F27
        common /varXh/ F21, F22, F23, F24, F31, F32, F29, F30, Sub22
        common /varXh/ Sub30, Sub6, Sub31, Sub25, Sub201, Sub23
        common /varXh/ Sub24, Sub32, Sub26

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJququbar_356_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
