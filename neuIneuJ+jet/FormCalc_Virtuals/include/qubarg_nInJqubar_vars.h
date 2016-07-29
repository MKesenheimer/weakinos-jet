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

        ComplexType Sub2(4), Sub9(4), Sub61(3,2)
        ComplexType Sub60(3,2), Sub27(4,4)
        ComplexType Sub28(4,4), Sub3(3,4,2)
        ComplexType Sub4(3,4,2), Sub10(3,4,2)
        ComplexType Sub15(3,4,2), Sub11(3,4,2)
        ComplexType Sub13(3,4,2), Sub12(3,4,2)
        ComplexType Sub1(3,4,2), Sub7(3,4,2)
        ComplexType Sub6(3,4,2)
        common /varXs/ Sub2, Sub9, Sub61, Sub60, Sub27, Sub28, Sub3
        common /varXs/ Sub4, Sub10, Sub15, Sub11, Sub13, Sub12, Sub1
        common /varXs/ Sub7, Sub6

        ComplexType Opt1(3,3,4), Sub65(3,3,4,4)
        ComplexType Sub59(3,3,4,4)
        ComplexType Sub63(3,3,4,4)
        RealType S, T, T14, U, T24, S34
        common /varXa/ Opt1, Sub65, Sub59, Sub63, S, T, T14, U, T24
        common /varXa/ S34

        HelType F14, F5, F25, F16, F7, F24, F17, F20, F1, F3, F27, F9
        HelType F19, F11, F12, F2, F30, F32, F10, F4, F33, F31, F41
        HelType F39, F21, F23, F38, F29, F18, F26, F35, F34, F15, F6
        HelType F13, F8, F42, F40, F28, F36, F22, F37, Pair1, Pair2
        HelType Pair4, Pair3, Abb1, Abb2, Sub29(HelDim(4),4)
        HelType Sub30(HelDim(4),4), Sub42(HelDim(4),4)
        HelType Sub43(HelDim(4),4), Sub35(HelDim(4),4)
        HelType Sub50(HelDim(4),4)
        HelType Sub32(HelDim(3),4,4)
        HelType Sub34(HelDim(3),4,4)
        HelType Sub26(HelDim(3),4,2)
        HelType Sub22(HelDim(3),4,2)
        HelType Sub5(HelDim(3),4,2)
        HelType Sub25(HelDim(3),4,2)
        HelType Sub66(HelDim(3),4,2)
        HelType Sub17(HelDim(3),4,2)
        HelType Sub16(HelDim(3),4,2)
        HelType Sub20(HelDim(3),4,2)
        HelType Sub21(HelDim(3),4,2)
        HelType Sub62(HelDim(3),4,2)
        HelType Sub46(HelDim(3),4,4)
        HelType Sub49(HelDim(3),4,4)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub52(HelDim(3),4,2)
        HelType Sub57(HelDim(3),4,2)
        HelType Sub41(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub37(HelDim(3),4,2)
        HelType Sub40(HelDim(3),4,2)
        HelType Sub54(HelDim(3),3,4,4,2)
        HelType Sub58(HelDim(3),3,4,4,2)
        HelType Sub64(HelDim(3),3,4,4,2)
        HelType Sub23(HelDim(3)), Sub24(HelDim(3))
        HelType Sub33(HelDim(3)), Sub19(HelDim(3))
        HelType Sub31(HelDim(3)), Sub18(HelDim(3))
        HelType Sub39(HelDim(3)), Sub38(HelDim(3))
        HelType Sub56(HelDim(3)), Sub55(HelDim(3))
        HelType Sub36(HelDim(3),4,4)
        HelType Sub51(HelDim(3),3,4,4)
        HelType Sub44(HelDim(3)), Sub45(HelDim(3))
        HelType Sub48(HelDim(3)), Sub47(HelDim(3))
        common /varXh/ F14, F5, F25, F16, F7, F24, F17, F20, F1, F3
        common /varXh/ F27, F9, F19, F11, F12, F2, F30, F32, F10, F4
        common /varXh/ F33, F31, F41, F39, F21, F23, F38, F29, F18
        common /varXh/ F26, F35, F34, F15, F6, F13, F8, F42, F40
        common /varXh/ F28, F36, F22, F37, Pair1, Pair2, Pair4
        common /varXh/ Pair3, Abb1, Abb2, Sub29, Sub30, Sub42, Sub43
        common /varXh/ Sub35, Sub50, Sub32, Sub34, Sub26, Sub22
        common /varXh/ Sub5, Sub25, Sub66, Sub17, Sub16, Sub20
        common /varXh/ Sub21, Sub62, Sub46, Sub49, Sub14, Sub52
        common /varXh/ Sub57, Sub41, Sub8, Sub37, Sub40, Sub54
        common /varXh/ Sub58, Sub64, Sub23, Sub24, Sub33, Sub19
        common /varXh/ Sub31, Sub18, Sub39, Sub38, Sub56, Sub55
        common /varXh/ Sub36, Sub51, Sub44, Sub45, Sub48, Sub47

        integer seq(2), Hel(5)
        common /helind/ seq, Hel

        integer Sfe6
        common /indices/ Sfe6

        HelType Ctree(HelDim(1))
        ComplexType MatSUN(1,1)
        common /qubarg_nInJqubar_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
