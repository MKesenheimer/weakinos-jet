#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:26
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub423, Sub427, Sub2, Sub9, Sub4, Sub3, Sub11
	ComplexType Sub10
	common /varXs/ Sub423, Sub427, Sub2, Sub9, Sub4, Sub3, Sub11
	common /varXs/ Sub10

	ComplexType Opt6, Sub429, Sub428, Sub430
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt6, Sub429, Sub428, Sub430, S, T, T14, T15
	common /varXa/ U, T24, T25, S34, S35, S45

	HelType F1, F10, F11, F14, F12, F13, F15, F7, F16, F9, Sub27
	HelType Sub25, Sub26, Sub28
	common /varXh/ F1, F10, F11, F14, F12, F13, F15, F7, F16, F9
	common /varXh/ Sub27, Sub25, Sub26, Sub28

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ubarubar_n1n2ubarubar_L36R45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
