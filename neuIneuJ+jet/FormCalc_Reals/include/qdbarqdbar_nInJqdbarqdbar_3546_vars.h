#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:45
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub6(4), Sub306(3,2)
        ComplexType Sub304(3,2), Sub1(3,4,2)
        ComplexType Sub3(3,4,2), Sub11(3,4,2)
        ComplexType Sub12(3,4,2), Sub14(3,4,2)
        ComplexType Sub15(3,4,2), Sub5(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub2, Sub6, Sub306, Sub304, Sub1, Sub3, Sub11
        common /varXs/ Sub12, Sub14, Sub15, Sub5, Sub7

        ComplexType Sub308(3,3,4)
        ComplexType Sub305(3,3,3,3,4,4)
        ComplexType Sub307(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub308, Sub305, Sub307, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F6, F3, F1, F5, F4, F2, F9, F10, F7, F8
        HelType Sub4(HelDim(3),4,2)
        HelType Sub13(HelDim(3),4,2)
        HelType Sub16(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub17(HelDim(3),3,2,2)
        HelType Sub18(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F6, F3, F1, F5, F4, F2, F9, F10, F7, F8, Sub4
        common /varXh/ Sub13, Sub16, Sub8, Sub9, Sub17, Sub18, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqdbar_nInJqdbarqdbar_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif