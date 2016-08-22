#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 29-Jul-2016 10:59
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 5

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub9(4), Sub2(4), Sub5(4), Sub14(4)
        ComplexType Sub60(3,2), Sub62(3,2), Sub17(4,4)
        ComplexType Sub18(4,4), Sub1(3,4,2)
        ComplexType Sub3(3,4,2), Sub13(3,4,2)
        ComplexType Sub15(3,4,2), Sub10(3,4,2)
        ComplexType Sub11(3,4,2), Sub6(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub9, Sub2, Sub5, Sub14, Sub60, Sub62, Sub17
        common /varXs/ Sub18, Sub1, Sub3, Sub13, Sub15, Sub10, Sub11
        common /varXs/ Sub6, Sub7

        ComplexType Sub59(3,3,4,4)
        ComplexType Sub58(3,3,4,4)
        RealType S, T, T14, U, T24, S34
        common /varXa/ Sub59, Sub58, S, T, T14, U, T24, S34

        HelType F20, F32, F40, F24, F25, F42, F2, F5, F33, F36, F7
        HelType F16, F21, F1, F9, F23, F15, F8, F35, F30, F31, F13
        HelType F4, F38, F26, F27, F44, F45, F54, F6, F19, F12, F28
        HelType F51, F3, F22, F14, F29, F43, F37, F39, F17, F46, F41
        HelType F34, F11, F47, F48, F49, F50, F53, F56, F18, F52
        HelType F55, F10, Pair1, Pair4, Pair3, Pair2, Abb1, Abb2
        HelType Sub21, Sub26, Sub34(HelDim(4),4)
        HelType Sub19(HelDim(4),4), Sub35(HelDim(4),4)
        HelType Sub20(HelDim(4),4), Sub29(HelDim(4),4)
        HelType Sub43(HelDim(4),4)
        HelType Sub39(HelDim(3),4,4)
        HelType Sub42(HelDim(3),4,4)
        HelType Sub4(HelDim(3),4,2)
        HelType Sub53(HelDim(3),4,2)
        HelType Sub56(HelDim(3),4,2)
        HelType Sub57(HelDim(3),4,2)
        HelType Sub16(HelDim(3),4,2)
        HelType Sub48(HelDim(3),4,2)
        HelType Sub51(HelDim(3),4,2)
        HelType Sub52(HelDim(3),4,2)
        HelType Sub24(HelDim(3),4,4)
        HelType Sub28(HelDim(3),4,4)
        HelType Sub12(HelDim(3),4,2)
        HelType Sub45(HelDim(3),4,2)
        HelType Sub46(HelDim(3),4,2)
        HelType Sub47(HelDim(3),4,2)
        HelType Sub33(HelDim(3),4,2)
        HelType Sub31(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub32(HelDim(3),4,2)
        HelType Sub61(HelDim(3),3,4,4,2)
        HelType Sub63(HelDim(3),3,4,4,2)
        HelType Sub64(HelDim(3),3,4,4,2)
        HelType Sub36(HelDim(4)), Sub37(HelDim(3),4)
        HelType Sub44(HelDim(3),4,4)
        HelType Sub30(HelDim(3),3,4,4)
        HelType Sub55(HelDim(4)), Sub54(HelDim(4))
        HelType Sub49(HelDim(4)), Sub50(HelDim(4))
        HelType Sub40(HelDim(3),4), Sub38(HelDim(3),4)
        HelType Sub41(HelDim(3),4)
        HelType Sub22(HelDim(3),4,4)
        HelType Sub25(HelDim(3),4,4)
        HelType Sub27(HelDim(3),4,4)
        HelType Sub23(HelDim(3),4,4)
        common /varXh/ F20, F32, F40, F24, F25, F42, F2, F5, F33, F36
        common /varXh/ F7, F16, F21, F1, F9, F23, F15, F8, F35, F30
        common /varXh/ F31, F13, F4, F38, F26, F27, F44, F45, F54
        common /varXh/ F6, F19, F12, F28, F51, F3, F22, F14, F29
        common /varXh/ F43, F37, F39, F17, F46, F41, F34, F11, F47
        common /varXh/ F48, F49, F50, F53, F56, F18, F52, F55, F10
        common /varXh/ Pair1, Pair4, Pair3, Pair2, Abb1, Abb2, Sub21
        common /varXh/ Sub26, Sub34, Sub19, Sub35, Sub20, Sub29
        common /varXh/ Sub43, Sub39, Sub42, Sub4, Sub53, Sub56
        common /varXh/ Sub57, Sub16, Sub48, Sub51, Sub52, Sub24
        common /varXh/ Sub28, Sub12, Sub45, Sub46, Sub47, Sub33
        common /varXh/ Sub31, Sub8, Sub32, Sub61, Sub63, Sub64
        common /varXh/ Sub36, Sub37, Sub44, Sub30, Sub55, Sub54
        common /varXh/ Sub49, Sub50, Sub40, Sub38, Sub41, Sub22
        common /varXh/ Sub25, Sub27, Sub23

        integer seq(2), Hel(5)
        common /helind/ seq, Hel

        integer Sfe6
        common /indices/ Sfe6

        HelType Ctree(HelDim(1))
        ComplexType MatSUN(1,1)
        common /qdqdbar_nInJg_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif