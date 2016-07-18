#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:47
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub278, Sub275, Sub1, Sub2, Sub4, Sub3, Sub6, Sub5
	common /varXs/ Sub278, Sub275, Sub1, Sub2, Sub4, Sub3, Sub6
	common /varXs/ Sub5

	ComplexType Sub276, Sub277
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub276, Sub277, S, T, T14, T15, U, T24, T25
	common /varXa/ S34, S35, S45

	HelType F8, F7, F3, F4, F2, F5, F1, F6, F9, F10, Sub7, Sub8
	common /varXh/ F8, F7, F3, F4, F2, F5, F1, F6, F9, F10, Sub7
	common /varXh/ Sub8

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /dubar_n1n2dubar_L35L46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
