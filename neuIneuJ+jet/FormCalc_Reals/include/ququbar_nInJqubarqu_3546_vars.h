#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 26-Sep-2016 14:19
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub1(4), Sub395(3,2), Sub398(3,2)
        ComplexType Sub394(3,2), Sub397(3,2)
        ComplexType Sub387(3,2), Sub391(3,2)
        ComplexType Sub389(3,2), Sub392(3,2)
        ComplexType Sub3(3,4,2), Sub5(3,4,2)
        ComplexType Sub4(3,4,2), Sub11(3,4,2)
        ComplexType Sub13(3,4,2), Sub2(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub1, Sub395, Sub398, Sub394, Sub397, Sub387
        common /varXs/ Sub391, Sub389, Sub392, Sub3, Sub5, Sub4
        common /varXs/ Sub11, Sub13, Sub2, Sub7

        ComplexType Opt1(3,4), Sub388(3,3,3,3,4,4), Sub400(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub388, Sub400, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F3, F4, F1, F6, F2, F5, F7, F8, F9, F10
        HelType Sub12(HelDim(3),3)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F3, F4, F1, F6, F2, F5, F7, F8, F9, F10, Sub12
        common /varXh/ Sub6, Sub14, Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /ququbar_nInJqubarqu_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
