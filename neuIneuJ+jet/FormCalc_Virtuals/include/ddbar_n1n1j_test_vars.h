#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 31-May-2016 15:48
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 5

#include "decl.h"

#else

#include "decl.h"

	ComplexType Sub4, Sub2, Sub1(2), Sub5(2), Sub3(2), Sub6(2)
	common /varXs/ Sub4, Sub2, Sub1, Sub5, Sub3, Sub6

	RealType S, T, T14, U, T24, S34
	common /varXa/ S, T, T14, U, T24, S34

	HelType F2, F1, F3, F4, Pair1, Pair2, Abb1
	common /varXh/ F2, F1, F3, F4, Pair1, Pair2, Abb1

	integer seq(2), Hel(5)
	common /helind/ seq, Hel

	integer Sfe6
	common /indices/ Sfe6

	HelType Ctree(HelDim(1))
	ComplexType MatSUN(1,1)
	common /ddbar_n1n1j_test_formfactors/ Ctree, MatSUN

#if PARALLEL
	marker ends, enda, endhel
	common /varXs/ ends
	common /varXa/ enda
	common /helind/ endhel
#endif

#endif
