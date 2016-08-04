#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 18:46
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub5(4), Sub1(3,4,2)
        ComplexType Sub3(3,4,2), Sub6(3,4,Sfe8)
        ComplexType Sub7(3,4,Sfe8)
        common /varXs/ Sub2, Sub5, Sub1, Sub3, Sub6, Sub7

        ComplexType Sub201(3,3,4)
        ComplexType Sub202(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub201, Sub202, S, T, T14, T15, U, T24, T25
        common /varXa/ S34, S35, S45

        HelType F5, F3, F1, F6, F2, F4, F7, F8, F10, F9
        HelType Sub4(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,Sfe8)
        HelType Sub9(HelDim(3),3,2,Sfe8)
        HelType Sub10(HelDim(3),3,2,Sfe8)
        common /varXh/ F5, F3, F1, F6, F2, F4, F7, F8, F10, F9, Sub4
        common /varXh/ Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqu_nInJqdbarqu_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
