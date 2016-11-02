#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 12:53
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub15(4), Sub3(4), Sub20(4)
        ComplexType Sub336(3,2), Sub318(3,2)
        ComplexType Sub46(3,4,2), Sub47(3,4,2)
        ComplexType Sub51(3,4,2), Sub52(3,4,2)
        ComplexType Sub26(3,4,2), Sub27(3,4,2)
        common /varXs/ Sub15, Sub3, Sub20, Sub336, Sub318, Sub46
        common /varXs/ Sub47, Sub51, Sub52, Sub26, Sub27

        ComplexType Sub345(3,3,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub345, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F45, F46, F50, F49, F11, F14, F26, F18, F28, F20, F22
        HelType F29, F51, F47, F24, F31, F52, F48
        HelType Sub48(HelDim(3),4,2)
        HelType Sub53(HelDim(3),4,2)
        HelType Sub28(HelDim(3),4,2)
        HelType Sub54(HelDim(3),3,2,2)
        HelType Sub49(HelDim(3),3,2,2)
        HelType Sub346(HelDim(3),3,3,4,2,2)
        HelType Sub55(HelDim(3),3,2,2)
        HelType Sub50(HelDim(3),3,2,2)
        common /varXh/ F45, F46, F50, F49, F11, F14, F26, F18, F28
        common /varXh/ F20, F22, F29, F51, F47, F24, F31, F52, F48
        common /varXh/ Sub48, Sub53, Sub28, Sub54, Sub49, Sub346
        common /varXh/ Sub55, Sub50

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqd_nInJqdbarqd_456_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
