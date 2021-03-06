#if 0
* qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_abbr0s.F
* abbreviations for {-F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], -F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:57
#endif

#include "qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_abbr0s
        implicit none

#include "qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        MatSUN(1,1) = 9

        MatSUN(1,2) = 3

        MatSUN(2,1) = 3

        MatSUN(2,2) = 9


        Sub155(Gen(1),Sq2) = 
     &   -(cI*(MSf(Sq2,4,Gen(1))*WSf(Sq2,4,Gen(1)))) + 
     &    MSf2(Sq2,4,Gen(1))

        Sub158(Gen(1),Sq2) = 
     &   -(cI*(MSf(Sq2,4,Gen(1))*(WREG2 + WSf(Sq2,4,Gen(1))))) + 
     &    MSf2(Sq2,4,Gen(1))



        Sub153(Gen(2),Sq1) = 
     &   -(cI*(MSf(Sq1,3,Gen(2))*WSf(Sq1,3,Gen(2)))) + 
     &    MSf2(Sq1,3,Gen(2))

        Sub157(Gen(2),Sq1) = 
     &   -(cI*(MSf(Sq1,3,Gen(2))*(WREG2 + WSf(Sq1,3,Gen(2))))) + 
     &    MSf2(Sq1,3,Gen(2))



        Sub6(Cha(4),Gen(1),Sq2) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sq2,1,4,Gen(1)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(1))*UChaC(Cha(4),2)*USf(Sq2,2,4,Gen(1)))



        Sub1(Gen(5),Neu(3),Sq1) = 
     &   4*MW*(SB*SW*USf(Sq1,2,3,Gen(5))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(5))*USf(Sq1,1,3,Gen(5))*ZNeu(Neu(3),4))

        Sub2(Gen(5),Neu(3),Sq1) = 
     &   MW*(SB*USf(Sq1,1,3,Gen(5))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(5))*USf(Sq1,2,3,Gen(5))*ZNeuC(Neu(3),4)

        Sub3(Gen(5),Neu(3),Sq1) = 
     &   3*CW*Sub2(Gen(5),Neu(3),Sq1) + 
     &    MW*(SB*SW*USf(Sq1,1,3,Gen(5))*ZNeuC(Neu(3),1))



        Opt1(Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -Mf2(3,Gen(2)) - Mf2(3,Gen(5)) - Mf2(3,Gen(6)) - Mf2(4,Gen(1))


#include "realOS_contains.h"

        end

