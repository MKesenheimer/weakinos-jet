#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 11:49
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub348, Sub351, Sub1, Sub2, Sub12, Sub3, Sub5
	ComplexType Sub13
	common /varXs/ Sub348, Sub351, Sub1, Sub2, Sub12, Sub3, Sub5
	common /varXs/ Sub13

	ComplexType Sub349, Sub350
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Sub349, Sub350, S, T, T14, T15, U, T24, T25
	common /varXa/ S34, S35, S45

	HelType F10, F9, F3, F4, F2, F5, F7, F8, F1, F6, Sub14, Sub15
	common /varXh/ F10, F9, F3, F4, F2, F5, F7, F8, F1, F6, Sub14
	common /varXh/ Sub15

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /ddbar_n1n2ddbar_L35R46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
