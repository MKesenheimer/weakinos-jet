#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 17-Nov-2016 19:43
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub12(4), Sub15(4), Sub190(3,2)
        ComplexType Sub194(3,2), Sub192(3,2)
        ComplexType Sub195(3,2), Sub16(3,4,2)
        ComplexType Sub17(3,4,2), Sub11(3,4,2)
        ComplexType Sub13(3,4,2)
        common /varXs/ Sub12, Sub15, Sub190, Sub194, Sub192, Sub195
        common /varXs/ Sub16, Sub17, Sub11, Sub13

        ComplexType Opt1(3,3,3,3)
        ComplexType Sub197(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub197, S, T, T14, T15, U, T24, T25, S34
        common /varXa/ S35, S45

        HelType F5, F6, F12, F15, F14, F13, F16, F9, F17, F11
        HelType Sub10(HelDim(3),3)
        HelType Sub18(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        common /varXh/ F5, F6, F12, F15, F14, F13, F16, F9, F17, F11
        common /varXh/ Sub10, Sub18, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /ququbar_nInJqdqdbar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
