#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:52
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub364(3,2), Sub363(3,2)
        ComplexType Sub360(3,2), Sub362(3,2)
        ComplexType Sub4(3,4,2), Sub5(3,4,2)
        ComplexType Sub10(3,4,2), Sub11(3,4,2)
        ComplexType Sub1(3,4,2), Sub8(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub3, Sub364, Sub363, Sub360, Sub362, Sub4
        common /varXs/ Sub5, Sub10, Sub11, Sub1, Sub8, Sub7

        ComplexType Opt1(3,4), Sub361(3,3,3,3,4,4), Sub365(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub361, Sub365, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F4, F5, F1, F6, F2, F3, F7, F10, F8, F9
        HelType Sub2(HelDim(3),3)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub12(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub13(HelDim(3),3,2,2)
        HelType Sub14(HelDim(3),3,2,2)
        common /varXh/ F4, F5, F1, F6, F2, F3, F7, F10, F8, F9, Sub2
        common /varXh/ Sub6, Sub12, Sub9, Sub13, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
