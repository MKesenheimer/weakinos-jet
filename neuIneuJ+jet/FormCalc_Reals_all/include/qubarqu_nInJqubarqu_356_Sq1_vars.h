#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 13:17
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub1(4), Sub405(3,2), Sub387(3,2)
        ComplexType Sub31(3,4,2), Sub32(3,4,2)
        ComplexType Sub37(3,4,2), Sub39(3,4,2)
        ComplexType Sub38(3,4,2), Sub3(3,4,2)
        ComplexType Sub5(3,4,2), Sub4(3,4,2)
        common /varXs/ Sub1, Sub405, Sub387, Sub31, Sub32, Sub37
        common /varXs/ Sub39, Sub38, Sub3, Sub5, Sub4

        ComplexType Sub411(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub411, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F17, F19, F1, F6, F25, F27, F26, F18, F28, F20, F21
        HelType F29, F30, F22, F24, F31, F32, F23
        HelType Sub33(HelDim(3),4,2)
        HelType Sub40(HelDim(3),4,2)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub41(HelDim(3),3,2,2)
        HelType Sub35(HelDim(3),3,2,2)
        HelType Sub412(HelDim(3),3,3,4,2,2)
        HelType Sub34(HelDim(3)), Sub42(HelDim(3),3,2,2)
        HelType Sub36(HelDim(3),3,2,2)
        common /varXh/ F17, F19, F1, F6, F25, F27, F26, F18, F28, F20
        common /varXh/ F21, F29, F30, F22, F24, F31, F32, F23, Sub33
        common /varXh/ Sub40, Sub6, Sub41, Sub35, Sub412, Sub34
        common /varXh/ Sub42, Sub36

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJqubarqu_356_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
