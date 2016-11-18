#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 17-Nov-2016 19:13
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub17(4), Sub20(4), Sub7(4)
        ComplexType Sub345(3,2), Sub335(3,2)
        ComplexType Sub29(3,4,2), Sub30(3,4,2)
        ComplexType Sub35(3,4,2), Sub36(3,4,2)
        ComplexType Sub16(3,4,2), Sub18(3,4,2)
        common /varXs/ Sub17, Sub20, Sub7, Sub345, Sub335, Sub29
        common /varXs/ Sub30, Sub35, Sub36, Sub16, Sub18

        ComplexType Opt3(3,3,3,3), Sub351(3,3,4)
        ComplexType Sub341(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt3, Sub351, Sub341, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F26, F28, F12, F15, F18, F19, F40, F34, F41, F35, F36
        HelType F43, F42, F37, F38, F44, F45, F39
        HelType Sub31(HelDim(3),4,2)
        HelType Sub37(HelDim(3),4,2)
        HelType Sub19(HelDim(3),4,2)
        HelType Sub44(HelDim(3),3,2,2)
        HelType Sub41(HelDim(3),3,2,2)
        HelType Sub353(HelDim(3),3,3,4,2,2)
        HelType Sub43(HelDim(3)), Sub45(HelDim(3),3,2,2)
        HelType Sub42(HelDim(3),3,2,2)
        common /varXh/ F26, F28, F12, F15, F18, F19, F40, F34, F41
        common /varXh/ F35, F36, F43, F42, F37, F38, F44, F45, F39
        common /varXh/ Sub31, Sub37, Sub19, Sub44, Sub41, Sub353
        common /varXh/ Sub43, Sub45, Sub42

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe8
        common /indices/ Sfe8

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdqdbar_365_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
