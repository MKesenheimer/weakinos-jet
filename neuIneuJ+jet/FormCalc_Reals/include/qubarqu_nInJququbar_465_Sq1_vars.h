#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 17-Nov-2016 20:03
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub374(3,2), Sub366(3,2)
        ComplexType Sub45(3,4,2), Sub46(3,4,2)
        ComplexType Sub50(3,4,2), Sub52(3,4,2)
        ComplexType Sub51(3,4,2), Sub11(3,4,2)
        ComplexType Sub14(3,4,2), Sub13(3,4,2)
        common /varXs/ Sub2, Sub374, Sub366, Sub45, Sub46, Sub50
        common /varXs/ Sub52, Sub51, Sub11, Sub14, Sub13

        ComplexType Opt10(3,3,3,3)
        ComplexType Sub393(3,3,4)
        ComplexType Sub367(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt10, Sub393, Sub367, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F46, F47, F51, F50, F2, F7, F41, F34, F40, F35, F36
        HelType F42, F56, F54, F38, F44, F57, F55
        HelType Sub47(HelDim(3),4,2)
        HelType Sub53(HelDim(3),4,2)
        HelType Sub15(HelDim(3),4,2)
        HelType Sub64(HelDim(3),3,2,2)
        HelType Sub61(HelDim(3),3,2,2)
        HelType Sub63(HelDim(3),3,4,4,2,2)
        HelType Sub65(HelDim(3),3,2,2)
        HelType Sub62(HelDim(3),3,2,2)
        common /varXh/ F46, F47, F51, F50, F2, F7, F41, F34, F40, F35
        common /varXh/ F36, F42, F56, F54, F38, F44, F57, F55, Sub47
        common /varXh/ Sub53, Sub15, Sub64, Sub61, Sub63, Sub65
        common /varXh/ Sub62

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        integer Sfe8
        common /indices/ Sfe8

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_465_Sq1_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
