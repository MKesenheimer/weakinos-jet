#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 26-Sep-2016 14:35
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub186(3,2), Sub189(3,2)
        ComplexType Sub184(3,2), Sub188(3,2)
        ComplexType Sub1(3,4,2), Sub3(3,4,2)
        ComplexType Sub5(3,4,2), Sub7(3,4,2)
        ComplexType Sub6(3,4,2)
        common /varXs/ Sub2, Sub186, Sub189, Sub184, Sub188, Sub1
        common /varXs/ Sub3, Sub5, Sub7, Sub6

        ComplexType Opt1(3,4), Sub185(3,3,3,3,4,4), Sub191(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub185, Sub191, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F6, F3, F1, F5, F4, F2, F7, F8, F9, F10
        HelType Sub4(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F6, F3, F1, F5, F4, F2, F7, F8, F9, F10, Sub4
        common /varXh/ Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqdbar_nInJqdbarqubar_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
