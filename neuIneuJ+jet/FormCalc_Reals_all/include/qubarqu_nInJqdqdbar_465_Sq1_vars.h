#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 13:15
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub38(4), Sub7(4), Sub195(3,2)
        ComplexType Sub184(3,2), Sub39(3,4,2)
        ComplexType Sub40(3,4,2), Sub44(3,4,2)
        ComplexType Sub46(3,4,2), Sub45(3,4,2)
        ComplexType Sub6(3,4,2), Sub8(3,4,2)
        common /varXs/ Sub38, Sub7, Sub195, Sub184, Sub39, Sub40
        common /varXs/ Sub44, Sub46, Sub45, Sub6, Sub8

        ComplexType Opt5(3,3,3,3), Sub198(3,3,4)
        ComplexType Sub185(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt5, Sub198, Sub185, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F45, F46, F50, F49, F1, F6, F40, F33, F39, F34, F35
        HelType F41, F55, F53, F38, F43, F56, F54
        HelType Sub41(HelDim(3),4,2)
        HelType Sub47(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub54(HelDim(3),3,2,2)
        HelType Sub52(HelDim(3),3,2,2)
        HelType Sub200(HelDim(3),3,3,4,2,2)
        HelType Sub55(HelDim(3),3,2,2)
        HelType Sub53(HelDim(3),3,2,2)
        common /varXh/ F45, F46, F50, F49, F1, F6, F40, F33, F39, F34
        common /varXh/ F35, F41, F55, F53, F38, F43, F56, F54, Sub41
        common /varXh/ Sub47, Sub9, Sub54, Sub52, Sub200, Sub55
        common /varXh/ Sub53

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJqdqdbar_465_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
