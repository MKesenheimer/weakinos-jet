#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 18:59
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub12(4), Sub15(4), Sub16(3,4,2)
        ComplexType Sub17(3,4,2), Sub11(3,4,2)
        ComplexType Sub13(3,4,2)
        common /varXs/ Sub12, Sub15, Sub16, Sub17, Sub11, Sub13

        ComplexType Opt1(3,3,3,3)
        ComplexType Sub190(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub190, S, T, T14, T15, U, T24, T25, S34
        common /varXa/ S35, S45

        HelType F4, F3, F11, F14, F12, F13, F15, F10, F16, F9
        HelType Sub10(HelDim(3),3)
        HelType Sub18(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        common /varXh/ F4, F3, F11, F14, F12, F13, F15, F10, F16, F9
        common /varXh/ Sub10, Sub18, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJqdqdbar_3645_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
