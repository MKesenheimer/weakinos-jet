#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:37
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub376, Sub380, Sub1, Sub9, Sub2, Sub3, Sub5
	ComplexType Sub10
	common /varXs/ Sub376, Sub380, Sub1, Sub9, Sub2, Sub3, Sub5
	common /varXs/ Sub10

	ComplexType Opt2, Sub378, Sub377, Sub379
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt2, Sub378, Sub377, Sub379, S, T, T14, T15
	common /varXa/ U, T24, T25, S34, S35, S45

	HelType F1, F10, F2, F5, F3, F4, F6, F7, F8, F9, Sub11, Sub13
	HelType Sub12, Sub14
	common /varXh/ F1, F10, F2, F5, F3, F4, F6, F7, F8, F9, Sub11
	common /varXh/ Sub13, Sub12, Sub14

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /dbardbar_n1n2dbardbar_L35R46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
