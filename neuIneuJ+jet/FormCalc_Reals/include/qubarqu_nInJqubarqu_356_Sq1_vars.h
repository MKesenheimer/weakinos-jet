#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 11:00
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub1(4), Sub398(3,2), Sub380(3,2)
        ComplexType Sub30(3,4,2), Sub31(3,4,2)
        ComplexType Sub36(3,4,2), Sub38(3,4,2)
        ComplexType Sub37(3,4,2), Sub3(3,4,2)
        ComplexType Sub5(3,4,2), Sub4(3,4,2)
        common /varXs/ Sub1, Sub398, Sub380, Sub30, Sub31, Sub36
        common /varXs/ Sub38, Sub37, Sub3, Sub5, Sub4

        ComplexType Sub404(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub404, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F18, F20, F2, F7, F26, F28, F27, F19, F29, F21, F22
        HelType F30, F31, F23, F25, F33, F32, F24
        HelType Sub32(HelDim(3),4,2)
        HelType Sub39(HelDim(3),4,2)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub41(HelDim(3),3,2,2)
        HelType Sub34(HelDim(3),3,2,2)
        HelType Sub405(HelDim(3),3,3,4,2,2)
        HelType Sub40(HelDim(3)), Sub33(HelDim(3))
        HelType Sub42(HelDim(3),3,2,2)
        HelType Sub35(HelDim(3),3,2,2)
        common /varXh/ F18, F20, F2, F7, F26, F28, F27, F19, F29, F21
        common /varXh/ F22, F30, F31, F23, F25, F33, F32, F24, Sub32
        common /varXh/ Sub39, Sub6, Sub41, Sub34, Sub405, Sub40
        common /varXh/ Sub33, Sub42, Sub35

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