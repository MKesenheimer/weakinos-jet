#if 0
* qdqubar_nIxJqdqdbar_356_Sq1_abbr0s.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:55
#endif

#include "qdqubar_nIxJqdqdbar_356_Sq1_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqubar_nIxJqdqdbar_356_Sq1_abbr0s
        implicit none

#include "qdqubar_nIxJqdqdbar_356_Sq1_vars.h"
#include "realOS_inline.h"

        MatSUN(1,1) = 9

        MatSUN(1,2) = 3

        MatSUN(2,1) = 3

        MatSUN(2,2) = 9

        Sub1(Neu(3)) = -(3*CW*ZNeu(Neu(3),2)) + SW*ZNeu(Neu(3),1)

        LOOP(Sfe8, 1,2,1)

        Sub150(Gen(1),Sfe8) = 
     &   -(cI*(MSf(Sfe8,3,Gen(1))*WSf(Sfe8,3,Gen(1)))) + 
     &    MSf2(Sfe8,3,Gen(1))

        Sub151(Gen(1),Sfe8) = 
     &   -(cI*(MSf(Sfe8,4,Gen(1))*WSf(Sfe8,4,Gen(1)))) + 
     &    MSf2(Sfe8,4,Gen(1))

        ENDLOOP(Sfe8)


        Sub148(Gen(5),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(5))*WSf(Sq1,4,Gen(5)))) + 
     &    MSf2(Sq1,4,Gen(5))


        LOOP(Sfe8, 1,2,1)

        Sub10(Cha(4),Gen(1),Sfe8) = 
     &   -(2*MW*(SB*USf(Sfe8,1,3,Gen(1))*VChaC(Cha(4),1))) + 
     &    Sqrt(2.D0)*(Mf(3,Gen(1))*USf(Sfe8,2,3,Gen(1))*VChaC(Cha(4),2))

        Sub13(Cha(4),Gen(1),Sfe8) = 
     &   -(2*MW*(CB*UCha(Cha(4),1)*USfC(Sfe8,1,4,Gen(1)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(1))*UCha(Cha(4),2)*USfC(Sfe8,2,4,Gen(1)))

        ENDLOOP(Sfe8)


        Sub2(Gen(5),Neu(3),Sq1) = 
     &   MW*(CB*Sub1(Neu(3))*USfC(Sq1,1,4,Gen(5))) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sq1,2,4,Gen(5))*ZNeu(Neu(3),3))

        Sub3(Gen(5),Neu(3),Sq1) = 
     &   2*MW*(CB*SW*USfC(Sq1,2,4,Gen(5))*ZNeuC(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sq1,1,4,Gen(5))*ZNeuC(Neu(3),3))


#include "realOS_contains.h"

        end

