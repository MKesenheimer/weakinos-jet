#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 27-Oct-2016 13:10
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub15(4), Sub208(3,2), Sub211(3,2)
        ComplexType Sub206(3,2), Sub210(3,2)
        ComplexType Sub16(3,4,2), Sub17(3,4,2)
        ComplexType Sub11(3,4,2), Sub13(3,4,2)
        ComplexType Sub12(3,4,2)
        common /varXs/ Sub15, Sub208, Sub211, Sub206, Sub210, Sub16
        common /varXs/ Sub17, Sub11, Sub13, Sub12

        ComplexType Opt2(3,3,3,3), Opt3(3,3)
        ComplexType Sub207(3,3,3,3,4,4)
        ComplexType Sub213(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub207, Sub213, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F4, F6, F11, F12, F14, F13, F15, F8, F16, F10
        HelType Sub18(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub19(HelDim(3),3,2,2)
        HelType Sub20(HelDim(3),3,2,2)
        common /varXh/ F4, F6, F11, F12, F14, F13, F15, F8, F16, F10
        common /varXh/ Sub18, Sub14, Sub19, Sub20

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqd_nInJqdqubar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
