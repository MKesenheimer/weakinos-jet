#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 12-Sep-2016 16:31
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 5

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub44(4), Sub1(4), Sub64(3,2)
        ComplexType Sub67(3,2), Sub16(4,4)
        ComplexType Sub17(4,4), Sub3(3,4,2)
        ComplexType Sub5(3,4,2), Sub4(3,4,2)
        ComplexType Sub43(3,4,2), Sub50(3,4,2)
        ComplexType Sub49(3,4,2), Sub45(3,4,2)
        ComplexType Sub46(3,4,2), Sub2(3,4,2)
        ComplexType Sub7(3,4,2)
        common /varXs/ Sub44, Sub1, Sub64, Sub67, Sub16, Sub17, Sub3
        common /varXs/ Sub5, Sub4, Sub43, Sub50, Sub49, Sub45, Sub46
        common /varXs/ Sub2, Sub7

        ComplexType Opt1(3,3,4), Sub69(3,3,4,4)
        ComplexType Sub66(3,3,4,4)
        ComplexType Sub63(3,3,4,4)
        RealType S, T, T14, U, T24, S34
        common /varXa/ Opt1, Sub69, Sub66, Sub63, S, T, T14, U, T24
        common /varXa/ S34

        HelType F19, F25, F13, F14, F5, F35, F6, F29, F24, F1, F11
        HelType F18, F3, F9, F12, F10, F2, F23, F26, F34, F4, F17
        HelType F20, F27, F33, F22, F41, F32, F28, F30, F39, F38
        HelType F15, F16, F7, F31, F8, F21, F37, F40, F36, F42
        HelType Pair2, Pair3, Pair4, Pair1, Abb1, Abb2
        HelType Sub28(HelDim(4),4), Sub18(HelDim(4),4)
        HelType Sub29(HelDim(4),4), Sub19(HelDim(4),4)
        HelType Sub26(HelDim(4),4), Sub36(HelDim(4),4)
        HelType Sub22(HelDim(3),4,4)
        HelType Sub25(HelDim(3),4,4)
        HelType Sub9(HelDim(3),4,2), Sub6(HelDim(3),4,2)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub57(HelDim(3),4,2)
        HelType Sub62(HelDim(3),4,2)
        HelType Sub60(HelDim(3),4,2)
        HelType Sub61(HelDim(3),4,2)
        HelType Sub68(HelDim(3),4,2)
        HelType Sub32(HelDim(3),4,4)
        HelType Sub35(HelDim(3),4,4)
        HelType Sub47(HelDim(3),4,2)
        HelType Sub56(HelDim(3),4,2)
        HelType Sub52(HelDim(3),4,2)
        HelType Sub42(HelDim(3),4,2)
        HelType Sub38(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub41(HelDim(3),4,2)
        HelType Sub51(HelDim(3),3,4,4,2)
        HelType Sub54(HelDim(3),3,4,4,2)
        HelType Sub55(HelDim(3),3,4,4,2)
        HelType Sub65(HelDim(3),3,4,4,2)
        HelType Sub15(HelDim(3),3,4,4,2)
        HelType Sub70(HelDim(3),3,4,4,2)
        HelType Sub11(HelDim(3),3,4,4,2)
        HelType Sub13(HelDim(3)), Sub12(HelDim(3))
        HelType Sub58(HelDim(3)), Sub59(HelDim(3))
        HelType Sub31(HelDim(3)), Sub30(HelDim(3))
        HelType Sub33(HelDim(3)), Sub34(HelDim(3))
        HelType Sub39(HelDim(3)), Sub40(HelDim(3))
        HelType Sub27(HelDim(3),4,4)
        HelType Sub37(HelDim(3),3,4,4)
        HelType Sub21(HelDim(3)), Sub20(HelDim(3))
        HelType Sub23(HelDim(3)), Sub24(HelDim(3))
        common /varXh/ F19, F25, F13, F14, F5, F35, F6, F29, F24, F1
        common /varXh/ F11, F18, F3, F9, F12, F10, F2, F23, F26, F34
        common /varXh/ F4, F17, F20, F27, F33, F22, F41, F32, F28
        common /varXh/ F30, F39, F38, F15, F16, F7, F31, F8, F21
        common /varXh/ F37, F40, F36, F42, Pair2, Pair3, Pair4
        common /varXh/ Pair1, Abb1, Abb2, Sub28, Sub18, Sub29, Sub19
        common /varXh/ Sub26, Sub36, Sub22, Sub25, Sub9, Sub6, Sub14
        common /varXh/ Sub57, Sub62, Sub60, Sub61, Sub68, Sub32
        common /varXh/ Sub35, Sub47, Sub56, Sub52, Sub42, Sub38
        common /varXh/ Sub8, Sub41, Sub51, Sub54, Sub55, Sub65
        common /varXh/ Sub15, Sub70, Sub11, Sub13, Sub12, Sub58
        common /varXh/ Sub59, Sub31, Sub30, Sub33, Sub34, Sub39
        common /varXh/ Sub40, Sub27, Sub37, Sub21, Sub20, Sub23
        common /varXh/ Sub24

        integer seq(2), Hel(5)
        common /helind/ seq, Hel

        integer Sfe6
        common /indices/ Sfe6

        HelType Ctree(HelDim(1))
        ComplexType MatSUN(1,1)
        common /qug_nInJqu_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
