#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 12:03
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub372, Sub375, Sub2, Sub18, Sub1, Sub19, Sub6
	ComplexType Sub5
	common /varXs/ Sub372, Sub375, Sub2, Sub18, Sub1, Sub19, Sub6
	common /varXs/ Sub5

	ComplexType Sub373, Sub374
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub373, Sub374, S, T, T14, T15, U, T24, T25
	common /varXa/ S34, S35, S45

	HelType F10, F9, F3, F5, F4, F2, F7, F8, F1, F6, Sub20, Sub21
	common /varXh/ F10, F9, F3, F5, F4, F2, F7, F8, F1, F6, Sub20
	common /varXh/ Sub21

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /uubar_n1n2uubar_R35L46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
