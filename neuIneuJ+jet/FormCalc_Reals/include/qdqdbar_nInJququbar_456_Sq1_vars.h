#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 13-Oct-2016 12:34
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub44(4), Sub37(4), Sub10(4)
        ComplexType Sub194(3,2), Sub183(3,2)
        ComplexType Sub36(3,4,2), Sub38(3,4,2)
        ComplexType Sub45(3,4,2), Sub46(3,4,2)
        ComplexType Sub11(3,4,2), Sub17(3,4,2)
        common /varXs/ Sub44, Sub37, Sub10, Sub194, Sub183, Sub36
        common /varXs/ Sub38, Sub45, Sub46, Sub11, Sub17

        ComplexType Sub197(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub197, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F50, F20, F49, F19, F45, F46, F13, F12, F28, F26, F21
        HelType F47, F24, F48, F31, F52, F29, F51
        HelType Sub39(HelDim(3),4,2)
        HelType Sub47(HelDim(3),4,2)
        HelType Sub18(HelDim(3),4,2)
        HelType Sub48(HelDim(3),3,2,2)
        HelType Sub42(HelDim(3),3,2,2)
        HelType Sub198(HelDim(3),3,3,4,2,2)
        HelType Sub40(HelDim(3)), Sub41(HelDim(3))
        HelType Sub49(HelDim(3),3,2,2)
        HelType Sub43(HelDim(3),3,2,2)
        common /varXh/ F50, F20, F49, F19, F45, F46, F13, F12, F28
        common /varXh/ F26, F21, F47, F24, F48, F31, F52, F29, F51
        common /varXh/ Sub39, Sub47, Sub18, Sub48, Sub42, Sub198
        common /varXh/ Sub40, Sub41, Sub49, Sub43

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJququbar_456_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif