#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:40
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub248, Sub19, Sub17, Sub18, Sub21, Sub20, Sub22
	common /varXs/ Sub248, Sub19, Sub17, Sub18, Sub21, Sub20
	common /varXs/ Sub22

	ComplexType Sub251
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub251, S, T, T14, T15, U, T24, T25, S34, S35
	common /varXa/ S45

	HelType F12, F11, F10, F13, F14, F8, F9, F6
	common /varXh/ F12, F11, F10, F13, F14, F8, F9, F6

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /dbard_n1n2uubar_L36L45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
