#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:29
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub286, Sub287, Sub32, Sub23, Sub28, Sub34, Sub33
	ComplexType Sub29
	common /varXs/ Sub286, Sub287, Sub32, Sub23, Sub28, Sub34
	common /varXs/ Sub33, Sub29

	ComplexType Opt8, Sub288, Sub289
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt8, Sub288, Sub289, S, T, T14, T15, U, T24
	common /varXa/ T25, S34, S35, S45

	HelType F8, F7, F15, F13, F12, F14, F11, F6, F16, F10, Sub37
	HelType Sub38
	common /varXh/ F8, F7, F15, F13, F12, F14, F11, F6, F16, F10
	common /varXh/ Sub37, Sub38

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ubard_n1n2dubar_R36R45_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
