#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:50
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub351, Sub37, Sub27, Sub33, Sub25, Sub38, Sub34
	common /varXs/ Sub351, Sub37, Sub27, Sub33, Sub25, Sub38
	common /varXs/ Sub34

	ComplexType Sub352, Sub353
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub352, Sub353, S, T, T14, T15, U, T24, T25
	common /varXa/ S34, S35, S45

	HelType F10, F9, F14, F15, F13, F12, F16, F8, F11, F6, Sub41
	HelType Sub42
	common /varXh/ F10, F9, F14, F15, F13, F12, F16, F8, F11, F6
	common /varXh/ Sub41, Sub42

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ddbar_n1n2ddbar_R36R45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
