#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 18:24
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub1(4), Sub4(3,4,2)
        ComplexType Sub5(3,4,2), Sub12(3,4,Sfe8)
        ComplexType Sub13(3,4,Sfe8), Sub11(3,4,2)
        ComplexType Sub15(3,4,2), Sub2(3,4,Sfe8)
        ComplexType Sub7(3,4,Sfe8)
        common /varXs/ Sub3, Sub1, Sub4, Sub5, Sub12, Sub13, Sub11
        common /varXs/ Sub15, Sub2, Sub7

        ComplexType Opt1(3,4), Sub413(3,3,4)
        ComplexType Sub411(3,3,3,3,4,4)
        ComplexType Sub412(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub413, Sub411, Sub412, S, T, T14, T15
        common /varXa/ U, T24, T25, S34, S35, S45

        HelType F3, F6, F1, F4, F5, F2, F7, F8, F9, F10
        HelType Sub6(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,Sfe8)
        HelType Sub16(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,Sfe8)
        HelType Sub9(HelDim(3),3,2,Sfe8)
        HelType Sub17(HelDim(3),3,2,Sfe8)
        HelType Sub18(HelDim(3),3,2,Sfe8)
        HelType Sub10(HelDim(3),3,2,Sfe8)
        common /varXh/ F3, F6, F1, F4, F5, F2, F7, F8, F9, F10, Sub6
        common /varXh/ Sub14, Sub16, Sub8, Sub9, Sub17, Sub18, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /ququ_nInJququ_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
