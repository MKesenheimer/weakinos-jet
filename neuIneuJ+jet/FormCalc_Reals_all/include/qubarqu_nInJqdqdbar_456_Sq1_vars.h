#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 13:14
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub38(4), Sub15(4), Sub195(3,2)
        ComplexType Sub184(3,2), Sub39(3,4,2)
        ComplexType Sub40(3,4,2), Sub44(3,4,2)
        ComplexType Sub46(3,4,2), Sub45(3,4,2)
        ComplexType Sub16(3,4,2), Sub17(3,4,2)
        common /varXs/ Sub38, Sub15, Sub195, Sub184, Sub39, Sub40
        common /varXs/ Sub44, Sub46, Sub45, Sub16, Sub17

        ComplexType Sub198(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub198, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F45, F29, F46, F26, F50, F49, F13, F12, F20, F18, F28
        HelType F51, F31, F52, F23, F48, F21, F47
        HelType Sub41(HelDim(3),4,2)
        HelType Sub47(HelDim(3),4,2)
        HelType Sub18(HelDim(3),4,2)
        HelType Sub50(HelDim(3),3,2,2)
        HelType Sub42(HelDim(3),3,2,2)
        HelType Sub199(HelDim(3),3,3,4,2,2)
        HelType Sub48(HelDim(3)), Sub49(HelDim(3))
        HelType Sub51(HelDim(3),3,2,2)
        HelType Sub43(HelDim(3),3,2,2)
        common /varXh/ F45, F29, F46, F26, F50, F49, F13, F12, F20
        common /varXh/ F18, F28, F51, F31, F52, F23, F48, F21, F47
        common /varXh/ Sub41, Sub47, Sub18, Sub50, Sub42, Sub199
        common /varXh/ Sub48, Sub49, Sub51, Sub43

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJqdqdbar_456_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
