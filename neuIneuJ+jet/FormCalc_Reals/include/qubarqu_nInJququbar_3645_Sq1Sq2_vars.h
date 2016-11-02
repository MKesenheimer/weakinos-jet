#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 2-Nov-2016 16:59
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub16(4), Sub368(3,2), Sub371(3,2)
        ComplexType Sub367(3,2), Sub370(3,2)
        ComplexType Sub350(3,2), Sub354(3,2)
        ComplexType Sub352(3,2), Sub355(3,2)
        ComplexType Sub17(3,4,2), Sub23(3,4,2)
        ComplexType Sub25(3,4,2), Sub26(3,4,2)
        ComplexType Sub19(3,4,2), Sub21(3,4,2)
        ComplexType Sub20(3,4,2)
        common /varXs/ Sub16, Sub368, Sub371, Sub367, Sub370, Sub350
        common /varXs/ Sub354, Sub352, Sub355, Sub17, Sub23, Sub25
        common /varXs/ Sub26, Sub19, Sub21, Sub20

        ComplexType Opt2(3,3,3,3)
        ComplexType Opt3(3,3,3,3)
        ComplexType Sub364(3,3,3,3,4,4)
        ComplexType Sub373(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt2, Opt3, Sub364, Sub373, S, T, T14, T15, U
        common /varXa/ T24, T25, S34, S35, S45

        HelType F3, F4, F11, F13, F12, F14, F15, F7, F16, F10
        HelType Sub18(HelDim(3),3)
        HelType Sub24(HelDim(3),4,2)
        HelType Sub27(HelDim(3),4,2)
        HelType Sub22(HelDim(3),4,2)
        HelType Sub28(HelDim(3),3,2,2)
        HelType Sub29(HelDim(3),3,2,2)
        common /varXh/ F3, F4, F11, F13, F12, F14, F15, F7, F16, F10
        common /varXh/ Sub18, Sub24, Sub27, Sub22, Sub28, Sub29

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqu_nInJququbar_3645_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
