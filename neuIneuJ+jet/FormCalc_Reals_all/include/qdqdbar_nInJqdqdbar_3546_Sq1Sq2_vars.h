#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 12:42
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub1(4), Sub6(4), Sub326(3,2)
        ComplexType Sub329(3,2), Sub327(3,2)
        ComplexType Sub330(3,2), Sub318(3,2)
        ComplexType Sub322(3,2), Sub320(3,2)
        ComplexType Sub323(3,2), Sub2(3,4,2)
        ComplexType Sub3(3,4,2), Sub12(3,4,2)
        ComplexType Sub13(3,4,2), Sub5(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub1, Sub6, Sub326, Sub329, Sub327, Sub330
        common /varXs/ Sub318, Sub322, Sub320, Sub323, Sub2, Sub3
        common /varXs/ Sub12, Sub13, Sub5, Sub7

        ComplexType Sub325(3,3,4)
        ComplexType Sub319(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub325, Sub319, S, T, T14, T15, U, T24, T25
        common /varXa/ S34, S35, S45

        HelType F5, F3, F1, F6, F2, F4, F8, F7, F9, F10
        HelType Sub11(HelDim(3),3)
        HelType Sub4(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F5, F3, F1, F6, F2, F4, F8, F7, F9, F10, Sub11
        common /varXh/ Sub4, Sub14, Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdqdbar_3546_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
