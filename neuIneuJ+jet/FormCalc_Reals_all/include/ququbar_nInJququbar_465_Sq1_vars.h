#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 13:07
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub408(3,2), Sub390(3,2)
        ComplexType Sub55(3,4,2), Sub57(3,4,2)
        ComplexType Sub56(3,4,2), Sub63(3,4,2)
        ComplexType Sub64(3,4,2), Sub1(3,4,2)
        ComplexType Sub8(3,4,2), Sub7(3,4,2)
        common /varXs/ Sub3, Sub408, Sub390, Sub55, Sub57, Sub56
        common /varXs/ Sub63, Sub64, Sub1, Sub8, Sub7

        ComplexType Opt5(3,3,3,3), Sub417(3,3,4)
        ComplexType Sub391(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt5, Sub417, Sub391, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F50, F49, F45, F46, F1, F6, F39, F33, F40, F34, F35
        HelType F42, F55, F53, F37, F43, F56, F54
        HelType Sub58(HelDim(3),4,2)
        HelType Sub65(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub73(HelDim(3),3,2,2)
        HelType Sub70(HelDim(3),3,2,2)
        HelType Sub72(HelDim(3),3,4,4,2,2)
        HelType Sub74(HelDim(3),3,2,2)
        HelType Sub71(HelDim(3),3,2,2)
        common /varXh/ F50, F49, F45, F46, F1, F6, F39, F33, F40, F34
        common /varXh/ F35, F42, F55, F53, F37, F43, F56, F54, Sub58
        common /varXh/ Sub65, Sub9, Sub73, Sub70, Sub72, Sub74, Sub71

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe7
        common /indices/ Sfe7

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /ququbar_nInJququbar_465_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
