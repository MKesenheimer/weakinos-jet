#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 18:40
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub4(3,4,2)
        ComplexType Sub5(3,4,2), Sub1(3,4,2)
        ComplexType Sub8(3,4,2), Sub7(3,4,2)
        common /varXs/ Sub3, Sub4, Sub5, Sub1, Sub8, Sub7

        ComplexType Sub196(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub196, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F3, F4, F1, F6, F2, F5, F8, F7, F9, F10
        HelType Sub2(HelDim(3),3)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        common /varXh/ F3, F4, F1, F6, F2, F5, F8, F7, F9, F10, Sub2
        common /varXh/ Sub6, Sub9

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqd_nInJququbar_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
