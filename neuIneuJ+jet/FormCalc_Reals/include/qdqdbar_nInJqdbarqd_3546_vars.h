#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:27
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub6(4), Sub323(3,2)
        ComplexType Sub322(3,2), Sub319(3,2)
        ComplexType Sub321(3,2), Sub2(3,4,2)
        ComplexType Sub4(3,4,2), Sub10(3,4,2)
        ComplexType Sub11(3,4,2), Sub7(3,4,2)
        ComplexType Sub8(3,4,2)
        common /varXs/ Sub3, Sub6, Sub323, Sub322, Sub319, Sub321
        common /varXs/ Sub2, Sub4, Sub10, Sub11, Sub7, Sub8

        ComplexType Sub320(3,3,3,3,4,4)
        ComplexType Sub324(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub320, Sub324, S, T, T14, T15, U, T24, T25
        common /varXa/ S34, S35, S45

        HelType F4, F3, F1, F6, F2, F5, F7, F8, F9, F10
        HelType Sub1(HelDim(3),3)
        HelType Sub5(HelDim(3),4,2)
        HelType Sub12(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub13(HelDim(3),3,2,2)
        HelType Sub14(HelDim(3),3,2,2)
        common /varXh/ F4, F3, F1, F6, F2, F5, F7, F8, F9, F10, Sub1
        common /varXh/ Sub5, Sub12, Sub9, Sub13, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdbarqd_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
