#if 0
* vars.h
* variable declarations
* generated by FormCalc 9.4 (7 Jun 2016) on 19-Dec-2016 16:29
#endif

#ifndef vars_h
#define vars_h

#define SQUAREDME
#define LEGS 6

#include "realOS_decl.h"

#else

#include "realOS_decl.h"

        ComplexType Sub1(3,4,2), Sub2(4)
        ComplexType Sub3(3,4,2), Sub5(3,4,2)
        ComplexType Sub6(3,4,2), Sub7(3,4,2)
        ComplexType Sub182(3,2), Sub184(3,2)
        ComplexType Sub186(3,2), Sub187(3,2)
        common /varXs/ Sub1, Sub2, Sub3, Sub5, Sub6, Sub7, Sub182
        common /varXs/ Sub184, Sub186, Sub187

        ComplexType Opt1(3,4), Sub183(3,3,3,3,4,4), Sub189(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub183, Sub189, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F1, F2, F3, F4, F5, F6, F7, F8, F9, F10
        HelType Sub4(HelDim(3),4,2)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,2)
        HelType Sub10(HelDim(3),3,2,2)
        common /varXh/ F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Sub4
        common /varXh/ Sub8, Sub9, Sub10

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qubarqdbar_nInJqdbarqubar_3546_Sq1Sq2_formfactors/ Ctree, MatSUN

#endif

