#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 15-Jul-2016 12:05
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub278, Sub279, Sub13, Sub4, Sub9, Sub2, Sub14
	ComplexType Sub10
	common /varXs/ Sub278, Sub279, Sub13, Sub4, Sub9, Sub2, Sub14
	common /varXs/ Sub10

	ComplexType Opt4, Sub275, Sub277
	RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
	common /varXa/ Opt4, Sub275, Sub277, S, T, T14, T15, U, T24
	common /varXa/ T25, S34, S35, S45

	HelType F8, F7, F4, F3, F2, F5, F1, F6, F9, F10, Sub17, Sub18
	common /varXh/ F8, F7, F4, F3, F2, F5, F1, F6, F9, F10, Sub17
	common /varXh/ Sub18

	integer seq(2), Hel(6)
	common /helind/ seq, Hel

	HelType Ctree(HelDim(2))
	ComplexType MatSUN(2,2)
	common /udbar_n1n2dbaru_R35R46_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
