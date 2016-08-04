#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:40
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub198(3,2), Sub200(3,2)
        ComplexType Sub4(3,4,2), Sub5(3,4,2)
        ComplexType Sub1(3,4,2), Sub8(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub3, Sub198, Sub200, Sub4, Sub5, Sub1, Sub8
        common /varXs/ Sub7

        ComplexType Sub199(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub199, S, T, T14, T15, U, T24, T25, S34, S35
        common /varXa/ S45

        HelType F3, F4, F1, F6, F2, F5, F7, F8, F9, F10
        HelType Sub2(HelDim(3),3)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        common /varXh/ F3, F4, F1, F6, F2, F5, F7, F8, F9, F10, Sub2
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
