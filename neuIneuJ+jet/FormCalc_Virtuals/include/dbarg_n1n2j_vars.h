#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 10-Jun-2016 12:27
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 5

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub8, Sub4, Sub2, Sub18, Sub12, Sub20, Sub1(2)
	ComplexType Sub9(2), Sub11(2), Sub5(2), Sub3(2), Sub10(2)
	ComplexType Sub13(2), Sub6(2)
	common /varXs/ Sub8, Sub4, Sub2, Sub18, Sub12, Sub20, Sub1
	common /varXs/ Sub9, Sub11, Sub5, Sub3, Sub10, Sub13, Sub6

	ComplexType Sub28, Sub30, Sub27
	RealType S, T, T14, U, T24, S34
	common /varXa/ Sub28, Sub30, Sub27, S, T, T14, U, T24, S34

	HelType F7, F3, F20, F9, F1, F13, F2, F4, F10, F8, F14, F18
	HelType F25, F26, F19, F16, F17, F22, F23, F24, F11, F5, F21
	HelType F12, F6, F15, F30, F29, F27, F28, Pair2, Pair3
	HelType Pair4, Pair1, Abb1, Abb2, Sub24, Sub25, Sub23, Sub26
	HelType Sub14(HelDim(2)), Sub32(HelDim(2)), Sub16(HelDim(2))
	HelType Sub29(HelDim(2)), Sub7(HelDim(2)), Sub31(HelDim(2))
	HelType Sub15(HelDim(2)), Sub21, Sub17, Sub19, Sub22
	common /varXh/ F7, F3, F20, F9, F1, F13, F2, F4, F10, F8, F14
	common /varXh/ F18, F25, F26, F19, F16, F17, F22, F23, F24
	common /varXh/ F11, F5, F21, F12, F6, F15, F30, F29, F27
	common /varXh/ F28, Pair2, Pair3, Pair4, Pair1, Abb1, Abb2
	common /varXh/ Sub24, Sub25, Sub23, Sub26, Sub14, Sub32
	common /varXh/ Sub16, Sub29, Sub7, Sub31, Sub15, Sub21
	common /varXh/ Sub17, Sub19, Sub22

	integer seq(2), Hel(5)
	common /helind/ seq, Hel

	integer Sfe6
	common /indices/ Sfe6

	HelType Ctree(HelDim(1))
	ComplexType MatSUN(1,1)
	common /dbarg_n1n2j_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
