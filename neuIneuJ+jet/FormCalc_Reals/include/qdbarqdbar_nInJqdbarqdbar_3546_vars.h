#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 26-Sep-2016 14:26
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub6(4), Sub304(3,2)
        ComplexType Sub308(3,2), Sub312(3,2)
        ComplexType Sub315(3,2), Sub311(3,2)
        ComplexType Sub314(3,2), Sub306(3,2)
        ComplexType Sub309(3,2), Sub1(3,4,2)
        ComplexType Sub3(3,4,2), Sub5(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub2, Sub6, Sub304, Sub308, Sub312, Sub315
        common /varXs/ Sub311, Sub314, Sub306, Sub309, Sub1, Sub3
        common /varXs/ Sub5, Sub7

        ComplexType Opt1(3,4), Sub303(3,3,4)
        ComplexType Sub305(3,3,3,3,4,4)
        ComplexType Sub317(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub303, Sub305, Sub317, S, T, T14, T15
        common /varXa/ U, T24, T25, S34, S35, S45

        HelType F6, F3, F1, F5, F2, F4, F9, F10, F7, F8
        HelType Sub4(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub11(HelDim(3),3,2,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub12(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F6, F3, F1, F5, F2, F4, F9, F10, F7, F8, Sub4
        common /varXh/ Sub8, Sub11, Sub9, Sub12, Sub10

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
