#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 11-Nov-2016 14:44
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub384(3,2), Sub387(3,2)
        ComplexType Sub382(3,2), Sub386(3,2)
        ComplexType Sub390(3,2), Sub393(3,2)
        ComplexType Sub391(3,2), Sub394(3,2)
        ComplexType Sub4(3,4,2), Sub5(3,4,2)
        ComplexType Sub1(3,4,2), Sub8(3,4,2)
        ComplexType Sub7(3,4,2), Sub10(3,4,2)
        ComplexType Sub13(3,4,2), Sub12(3,4,2)
        common /varXs/ Sub3, Sub384, Sub387, Sub382, Sub386, Sub390
        common /varXs/ Sub393, Sub391, Sub394, Sub4, Sub5, Sub1
        common /varXs/ Sub8, Sub7, Sub10, Sub13, Sub12

        ComplexType Opt1(3,4), Sub383(3,3,3,3,4,4), Sub389(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub383, Sub389, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F4, F6, F2, F7, F5, F3, F8, F9, F10, F11
        HelType Sub2(HelDim(3),3)
        HelType Sub6(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub16(HelDim(3),4,2)
        HelType Sub17(HelDim(3),3,2,2)
        HelType Sub15(HelDim(3),3,2,2)
        HelType Sub14(HelDim(3),3,4,4,2,2)
        common /varXh/ F4, F6, F2, F7, F5, F3, F8, F9, F10, F11, Sub2
        common /varXh/ Sub6, Sub9, Sub16, Sub17, Sub15, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_3546_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
