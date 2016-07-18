#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 18-Jul-2016 11:12
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 5

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub2(4), Sub9(4), Sub61(3,2)
        ComplexType Sub60(3,2), Sub22(4,4)
        ComplexType Sub23(4,4), Sub3(3,4,2)
        ComplexType Sub4(3,4,2), Sub10(3,4,2)
        ComplexType Sub15(3,4,2), Sub11(3,4,2)
        ComplexType Sub13(3,4,2), Sub12(3,4,2)
        ComplexType Sub1(3,4,2), Sub7(3,4,2)
        ComplexType Sub6(3,4,2)
        common /varXs/ Sub2, Sub9, Sub61, Sub60, Sub22, Sub23, Sub3
        common /varXs/ Sub4, Sub10, Sub15, Sub11, Sub13, Sub12, Sub1
        common /varXs/ Sub7, Sub6

        ComplexType Opt1(3,3,4), Sub65(3,3,4,4)
        ComplexType Sub59(3,3,4,4)
        ComplexType Sub63(3,3,4,4)
        RealType S, T, T14, U, T24, S34
        common /varXa/ Opt1, Sub65, Sub59, Sub63, S, T, T14, U, T24
        common /varXa/ S34

        HelType F15, F5, F24, F16, F7, F25, F17, F19, F1, F2, F21, F9
        HelType F22, F12, F11, F3, F30, F33, F10, F4, F32, F31, F40
        HelType F37, F20, F23, F35, F29, F18, F28, F36, F34, F14, F6
        HelType F13, F8, F41, F42, F26, F38, F27, F39, Pair1, Pair2
        HelType Pair4, Pair3, Abb1, Abb2, Sub24(HelDim(4),4)
        HelType Sub25(HelDim(4),4), Sub37(HelDim(4),4)
        HelType Sub38(HelDim(4),4), Sub32(HelDim(4),4)
        HelType Sub45(HelDim(4),4)
        HelType Sub28(HelDim(3),4,4)
        HelType Sub31(HelDim(3),4,4)
        HelType Sub36(HelDim(3),4,2)
        HelType Sub34(HelDim(3),4,2)
        HelType Sub5(HelDim(3),4,2)
        HelType Sub35(HelDim(3),4,2)
        HelType Sub66(HelDim(3),4,2)
        HelType Sub16(HelDim(3),4,2)
        HelType Sub21(HelDim(3),4,2)
        HelType Sub19(HelDim(3),4,2)
        HelType Sub20(HelDim(3),4,2)
        HelType Sub62(HelDim(3),4,2)
        HelType Sub41(HelDim(3),4,4)
        HelType Sub44(HelDim(3),4,4)
        HelType Sub14(HelDim(3),4,2)
        HelType Sub47(HelDim(3),4,2)
        HelType Sub52(HelDim(3),4,2)
        HelType Sub58(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub54(HelDim(3),4,2)
        HelType Sub57(HelDim(3),4,2)
        HelType Sub49(HelDim(3),3,4,4,2)
        HelType Sub53(HelDim(3),3,4,4,2)
        HelType Sub64(HelDim(3),3,4,4,2)
        HelType Sub27(HelDim(3)), Sub29(HelDim(3))
        HelType Sub30(HelDim(3)), Sub26(HelDim(3))
        HelType Sub18(HelDim(3)), Sub17(HelDim(3))
        HelType Sub55(HelDim(3)), Sub56(HelDim(3))
        HelType Sub51(HelDim(3)), Sub50(HelDim(3))
        HelType Sub33(HelDim(3),4,4)
        HelType Sub46(HelDim(3),3,4,4)
        HelType Sub42(HelDim(3)), Sub40(HelDim(3))
        HelType Sub43(HelDim(3)), Sub39(HelDim(3))
        common /varXh/ F15, F5, F24, F16, F7, F25, F17, F19, F1, F2
        common /varXh/ F21, F9, F22, F12, F11, F3, F30, F33, F10, F4
        common /varXh/ F32, F31, F40, F37, F20, F23, F35, F29, F18
        common /varXh/ F28, F36, F34, F14, F6, F13, F8, F41, F42
        common /varXh/ F26, F38, F27, F39, Pair1, Pair2, Pair4
        common /varXh/ Pair3, Abb1, Abb2, Sub24, Sub25, Sub37, Sub38
        common /varXh/ Sub32, Sub45, Sub28, Sub31, Sub36, Sub34
        common /varXh/ Sub5, Sub35, Sub66, Sub16, Sub21, Sub19
        common /varXh/ Sub20, Sub62, Sub41, Sub44, Sub14, Sub47
        common /varXh/ Sub52, Sub58, Sub8, Sub54, Sub57, Sub49
        common /varXh/ Sub53, Sub64, Sub27, Sub29, Sub30, Sub26
        common /varXh/ Sub18, Sub17, Sub55, Sub56, Sub51, Sub50
        common /varXh/ Sub33, Sub46, Sub42, Sub40, Sub43, Sub39

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
