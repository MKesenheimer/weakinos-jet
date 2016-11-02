#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 13:16
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub16(4), Sub364(3,2)
        ComplexType Sub356(3,2), Sub30(3,4,2)
        ComplexType Sub31(3,4,2), Sub36(3,4,2)
        ComplexType Sub38(3,4,2), Sub37(3,4,2)
        ComplexType Sub4(3,4,2), Sub5(3,4,2)
        common /varXs/ Sub3, Sub16, Sub364, Sub356, Sub30, Sub31
        common /varXs/ Sub36, Sub38, Sub37, Sub4, Sub5

        ComplexType Sub380(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub380, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F17, F28, F19, F26, F27, F25, F2, F5, F20, F18, F29
        HelType F30, F31, F32, F23, F24, F22, F21
        HelType Sub32(HelDim(3),4,2)
        HelType Sub39(HelDim(3),4,2)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub62(HelDim(3),3,2,2)
        HelType Sub42(HelDim(3),3,2,2)
        HelType Sub34(HelDim(3),3,2,2)
        HelType Sub381(HelDim(3),3,3,4,2,2)
        HelType Sub40(HelDim(3)), Sub41(HelDim(3))
        HelType Sub33(HelDim(3)), Sub43(HelDim(3),3,2,2)
        HelType Sub35(HelDim(3),3,2,2)
        common /varXh/ F17, F28, F19, F26, F27, F25, F2, F5, F20, F18
        common /varXh/ F29, F30, F31, F32, F23, F24, F22, F21, Sub32
        common /varXh/ Sub39, Sub6, Sub62, Sub42, Sub34, Sub381
        common /varXh/ Sub40, Sub41, Sub33, Sub43, Sub35

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_356_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
