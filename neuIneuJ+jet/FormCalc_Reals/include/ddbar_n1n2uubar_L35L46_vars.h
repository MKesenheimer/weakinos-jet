#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:52
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub241, Sub2, Sub3, Sub1, Sub4, Sub6, Sub5
	common /varXs/ Sub241, Sub2, Sub3, Sub1, Sub4, Sub6, Sub5

	ComplexType Sub242
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub242, S, T, T14, T15, U, T24, T25, S34, S35
	common /varXa/ S45

	HelType F4, F3, F2, F5, F7, F8, F1, F6
	common /varXh/ F4, F3, F2, F5, F7, F8, F1, F6

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ddbar_n1n2uubar_L35L46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
