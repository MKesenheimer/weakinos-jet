#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:28
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub290, Sub294, Sub32, Sub23, Sub34, Sub33, Sub24
	ComplexType Sub25
	common /varXs/ Sub290, Sub294, Sub32, Sub23, Sub34, Sub33
	common /varXs/ Sub24, Sub25

	ComplexType Opt7, Sub296, Sub297
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt7, Sub296, Sub297, S, T, T14, T15, U, T24
	common /varXa/ T25, S34, S35, S45

	HelType F1, F10, F11, F14, F13, F12, F16, F9, F15, F7, Sub35
	HelType Sub36
	common /varXh/ F1, F10, F11, F14, F13, F12, F16, F9, F15, F7
	common /varXh/ Sub35, Sub36

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ubardbar_n1n2dbarubar_R36L45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
