#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 12:47
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub1(4), Sub5(4), Sub214(3,2)
        ComplexType Sub218(3,2), Sub216(3,2)
        ComplexType Sub219(3,2), Sub2(3,4,2)
        ComplexType Sub3(3,4,2), Sub6(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub1, Sub5, Sub214, Sub218, Sub216, Sub219
        common /varXs/ Sub2, Sub3, Sub6, Sub7

        ComplexType Sub213(3,3,4)
        ComplexType Sub215(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub213, Sub215, S, T, T14, T15, U, T24, T25
        common /varXa/ S34, S35, S45

        HelType F3, F6, F1, F5, F4, F2, F7, F8, F9, F10
        HelType Sub4(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F3, F6, F1, F5, F4, F2, F7, F8, F9, F10, Sub4
        common /varXh/ Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqu_nInJqdqu_3546_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
