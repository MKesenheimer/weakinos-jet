#if 0
* vars.h
* variable declarations
* generated by FormCalc 8.4 on 3-Aug-2016 18:52
#endif

#ifndef VARS_H
#define VARS_H

#define LEGS 6

#include "decl.h"

#else

#include "decl.h"

        ComplexType Sub6(4), Sub1(3,4,Sfe8)
        ComplexType Sub3(3,4,Sfe8), Sub2(3,4,Sfe8)
        ComplexType Sub5(3,4,2), Sub7(3,4,2)
        common /varXs/ Sub6, Sub1, Sub3, Sub2, Sub5, Sub7

        ComplexType Opt1(3,4), Sub196(3,3,3,3,4,N
 
          eu4), Sub197(3,3,3,3,4,4)
        RealType S, T, T14, T15, U, T24, T25, S34, S35, S45
        common /varXa/ Opt1, Sub196, Sub197, S, T, T14, T15, U, T24
        common /varXa/ T25, S34, S35, S45

        HelType F3, F6, F1, F5, F4, F2, F10, F9, F8, F7
        HelType Sub4(HelDim(3),4,Sfe8)
        HelType Sub8(HelDim(3),4,2)
        HelType Sub9(HelDim(3),3,2,Sfe8)
        HelType Sub10(HelDim(3),3,2,Sfe8)
        common /varXh/ F3, F6, F1, F5, F4, F2, F10, F9, F8, F7, Sub4
        common /varXh/ Sub8, Sub9, Sub10

        integer seq(2), Hel(6)
        common /helind/ seq, Hel

        HelType Ctree(HelDim(2))
        ComplexType MatSUN(2,2)
        common /qdbarqubar_nInJqubarqdbar_3546_formfactors/ Ctree, MatSUN

#if PARALLEL
        marker ends, enda, endhel
        common /varXs/ ends
        common /varXa/ enda
        common /helind/ endhel
#endif

#endif
