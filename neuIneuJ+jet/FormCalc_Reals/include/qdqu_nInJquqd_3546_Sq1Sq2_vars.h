#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Nov-2016 9:48
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub1(4), Sub5(4), Sub223(3,2)
        ComplexType Sub226(3,2), Sub221(3,2)
        ComplexType Sub225(3,2), Sub2(3,4,2)
        ComplexType Sub3(3,4,2), Sub6(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub1, Sub5, Sub223, Sub226, Sub221, Sub225
        common /varXs/ Sub2, Sub3, Sub6, Sub7

        ComplexType Opt1(3,4), Sub222(3,3,3,3,4,4), Sub228(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub222, Sub228, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F4, F7, F2, F6, F5, F3, F10, F11, F8, F9
        HelType Sub4(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F4, F7, F2, F6, F5, F3, F10, F11, F8, F9, Sub4
        common /varXh/ Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqu_nInJquqd_3546_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif