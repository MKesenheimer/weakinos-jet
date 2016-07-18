#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 12:02
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub241, Sub25, Sub19, Sub23, Sub17, Sub26, Sub24
	common /varXs/ Sub241, Sub25, Sub19, Sub23, Sub17, Sub26
	common /varXs/ Sub24

	ComplexType Sub242
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub242, S, T, T14, T15, U, T24, T25, S34, S35
	common /varXa/ S45

	HelType F11, F13, F10, F12, F14, F8, F9, F6
	common /varXh/ F11, F13, F10, F12, F14, F8, F9, F6

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /uubar_n1n2ddbar_R36R45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
