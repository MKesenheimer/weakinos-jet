#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 12:09
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub264, Sub265, Sub19, Sub31, Sub22, Sub27, Sub32
	ComplexType Sub28
	common /varXs/ Sub264, Sub265, Sub19, Sub31, Sub22, Sub27
	common /varXs/ Sub32, Sub28

	ComplexType Opt8, Sub266, Sub267
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt8, Sub266, Sub267, S, T, T14, T15, U, T24
	common /varXa/ T25, S34, S35, S45

	HelType F10, F1, F11, F14, F13, F12, F15, F7, F16, F9, Sub35
	HelType Sub36
	common /varXh/ F10, F1, F11, F14, F13, F12, F15, F7, F16, F9
	common /varXh/ Sub35, Sub36

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ud_n1n2du_R36R45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
