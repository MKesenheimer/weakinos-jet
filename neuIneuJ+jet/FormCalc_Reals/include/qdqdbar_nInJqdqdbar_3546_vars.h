#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:24
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub7(4), Sub318(3,2)
        ComplexType Sub320(3,2), Sub315(3,2)
        ComplexType Sub317(3,2), Sub3(3,4,2)
        ComplexType Sub4(3,4,2), Sub6(3,4,2)
        ComplexType Sub8(3,4,2), Sub10(3,4,2)
        ComplexType Sub11(3,4,2)
        common /varXs/ Sub2, Sub7, Sub318, Sub320, Sub315, Sub317
        common /varXs/ Sub3, Sub4, Sub6, Sub8, Sub10, Sub11

        ComplexType Sub319(3,3,4)
        ComplexType Sub316(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub319, Sub316, S, T, T14, T15, U, T24, T25
        common /varXa/ S34, S35, S45

        HelType F3, F5, F1, F6, F4, F2, F7, F8, F10, F9
        HelType Sub1(HelDim(3),3)
        HelType Sub5(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub12(HelDim(3),4,2)
        HelType Sub13(HelDim(3),3,2,2)
        HelType Sub14(HelDim(3),3,2,2)
        common /varXh/ F3, F5, F1, F6, F4, F2, F7, F8, F10, F9, Sub1
        common /varXh/ Sub5, Sub9, Sub12, Sub13, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdqdbar_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
