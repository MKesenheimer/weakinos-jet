#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 17-Nov-2016 19:17
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub3(4), Sub6(4), Sub324(3,2)
        ComplexType Sub327(3,2), Sub322(3,2)
        ComplexType Sub326(3,2), Sub330(3,2)
        ComplexType Sub333(3,2), Sub331(3,2)
        ComplexType Sub334(3,2), Sub2(3,4,2)
        ComplexType Sub4(3,4,2), Sub7(3,4,2)
        ComplexType Sub8(3,4,2), Sub10(3,4,2)
        ComplexType Sub11(3,4,2)
        common /varXs/ Sub3, Sub6, Sub324, Sub327, Sub322, Sub326
        common /varXs/ Sub330, Sub333, Sub331, Sub334, Sub2, Sub4
        common /varXs/ Sub7, Sub8, Sub10, Sub11

        ComplexType Sub323(3,3,3,3,4,4)
        ComplexType Sub329(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Sub323, Sub329, S, T, T14, T15, U, T24, T25
        common /varXa/ S34, S35, S45

        HelType F4, F6, F2, F7, F5, F3, F8, F9, F10, F11
        HelType Sub1(HelDim(3),3)
        HelType Sub5(HelDim(3),4,2)
        HelType Sub9(HelDim(3),4,2)
        HelType Sub12(HelDim(3),4,2)
        HelType Sub13(HelDim(3),3,2,2)
        HelType Sub14(HelDim(3),3,2,2)
        common /varXh/ F4, F6, F2, F7, F5, F3, F8, F9, F10, F11, Sub1
        common /varXh/ Sub5, Sub9, Sub12, Sub13, Sub14

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdqdbar_nInJqdbarqd_3546_Sq1Sq2_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
