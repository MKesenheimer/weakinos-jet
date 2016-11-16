#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 10:56
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub19(4), Sub383(3,2)
        ComplexType Sub389(3,2), Sub33(3,4,2)
        ComplexType Sub34(3,4,2), Sub38(3,4,2)
        ComplexType Sub40(3,4,2), Sub39(3,4,2)
        ComplexType Sub4(3,4,2), Sub5(3,4,2)
        common /varXs/ Sub3, Sub19, Sub383, Sub389, Sub33, Sub34
        common /varXs/ Sub38, Sub40, Sub39, Sub4, Sub5

        ComplexType Sub405(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub405, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F18, F27, F19, F29, F26, F28, F5, F3, F21, F20, F30
        HelType F31, F32, F33, F24, F25, F22, F23
        HelType Sub45(HelDim(3),4,2)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub46(HelDim(3),3,2,2)
        HelType Sub44(HelDim(3),3,2,2)
        HelType Sub36(HelDim(3),3,2,2)
        HelType Sub329(HelDim(3),3,2,2)
        HelType Sub35(HelDim(3),3,4,4,2,2)
        HelType Sub41(HelDim(3),3,4,4,2,2)
        HelType Sub43(HelDim(3)), Sub42(HelDim(3))
        HelType Sub37(HelDim(3),3,2,2)
        common /varXh/ F18, F27, F19, F29, F26, F28, F5, F3, F21, F20
        common /varXh/ F30, F31, F32, F33, F24, F25, F22, F23, Sub45
        common /varXh/ Sub6, Sub46, Sub44, Sub36, Sub329, Sub35
        common /varXh/ Sub41, Sub43, Sub42, Sub37

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
