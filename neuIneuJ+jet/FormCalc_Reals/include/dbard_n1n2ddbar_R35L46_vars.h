#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:39
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub366, Sub369, Sub1, Sub17, Sub4, Sub18, Sub5
	ComplexType Sub6
	common /varXs/ Sub366, Sub369, Sub1, Sub17, Sub4, Sub18, Sub5
	common /varXs/ Sub6

	ComplexType Opt3, Sub367, Sub368
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt3, Sub367, Sub368, S, T, T14, T15, U, T24
	common /varXa/ T25, S34, S35, S45

	HelType F10, F9, F3, F4, F2, F5, F7, F8, F1, F6, Sub19, Sub20
	common /varXh/ F10, F9, F3, F4, F2, F5, F7, F8, F1, F6, Sub19
	common /varXh/ Sub20

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /dbard_n1n2ddbar_R35L46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
