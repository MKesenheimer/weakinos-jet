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

	ComplexType Opt2, Sub425, Sub424, Sub426
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt2, Sub425, Sub424, Sub426, S, T, T14, T15
	common /varXa/ U, T24, T25, S34, S35, S45

	HelType F1, F10, F2, F5, F3, F4, F6, F7, F8, F9, Sub12, Sub14
	HelType Sub13, Sub15
	common /varXh/ F1, F10, F2, F5, F3, F4, F6, F7, F8, F9, Sub12
	common /varXh/ Sub14, Sub13, Sub15

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ubarubar_n1n2ubarubar_L35R46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
