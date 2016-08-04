#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 4-Aug-2016 14:26
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub10(4), Sub195(3,2), Sub197(3,2)
        ComplexType Sub11(3,4,2), Sub17(3,4,2)
        ComplexType Sub13(3,4,2), Sub15(3,4,2)
        ComplexType Sub14(3,4,2)
        common /varXs/ Sub10, Sub195, Sub197, Sub11, Sub17, Sub13
        common /varXs/ Sub15, Sub14

        ComplexType Opt1(3,3,3,3)
        ComplexType Sub198(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub198, S, T, T14, T15, U, T24, T25, S34
        common /varXa/ S35, S45

        HelType F3, F6, F11, F14, F13, F12, F15, F7, F16, F10
        HelType Sub12(HelDim(3),3)
        HelType Sub18(HelDim(3),4,2)
        HelType Sub16(HelDim(3),4,2)
        common /varXh/ F3, F6, F11, F14, F13, F12, F15, F7, F16, F10
        common /varXh/ Sub12, Sub18, Sub16

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJququbar_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
