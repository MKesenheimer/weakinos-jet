#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 12:10
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub387, Sub391, Sub3, Sub4, Sub1, Sub9, Sub5
	ComplexType Sub10
	common /varXs/ Sub387, Sub391, Sub3, Sub4, Sub1, Sub9, Sub5
	common /varXs/ Sub10

	ComplexType Opt2, Sub389, Sub388, Sub390
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt2, Sub389, Sub388, Sub390, S, T, T14, T15
	common /varXa/ U, T24, T25, S34, S35, S45

	HelType F10, F1, F2, F4, F3, F5, F6, F7, F8, F9, Sub13, Sub11
	HelType Sub12, Sub14
	common /varXh/ F10, F1, F2, F4, F3, F5, F6, F7, F8, F9, Sub13
	common /varXh/ Sub11, Sub12, Sub14

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /uu_n1n2uu_L35R46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
