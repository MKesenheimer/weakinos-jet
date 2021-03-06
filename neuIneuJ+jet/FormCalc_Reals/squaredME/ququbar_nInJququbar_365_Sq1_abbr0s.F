#if 0
* ququbar_nInJququbar_365_Sq1_abbr0s.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:19
#endif

#include "ququbar_nInJququbar_365_Sq1_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine ququbar_nInJququbar_365_Sq1_abbr0s
        implicit none

#include "ququbar_nInJququbar_365_Sq1_vars.h"
#include "realOS_inline.h"

        MatSUN(1,1) = 9

        MatSUN(1,2) = 3

        MatSUN(2,1) = 3

        MatSUN(2,2) = 9

        Sub15(Neu(4)) = 3*CW*ZNeu(Neu(4),2) + SW*ZNeu(Neu(4),1)

        LOOP(Sfe8, 1,2,1)

        Sub350(Gen(1),Sfe8) = 
     &   -(cI*(MSf(Sfe8,3,Gen(1))*WSf(Sfe8,3,Gen(1)))) + 
     &    MSf2(Sfe8,3,Gen(1))

        ENDLOOP(Sfe8)


        Sub340(Gen(5),Sq1) = 
     &   -(cI*(MSf(Sq1,3,Gen(5))*WSf(Sq1,3,Gen(5)))) + 
     &    MSf2(Sq1,3,Gen(5))


        LOOP(Sfe8, 1,2,1)

        Sub28(Gen(1),Neu(4),Sfe8) = 
     &   4*MW*(SB*SW*USf(Sfe8,2,3,Gen(1))*ZNeu(Neu(4),1)) - 
     &    3*CW*(Mf(3,Gen(1))*USf(Sfe8,1,3,Gen(1))*ZNeu(Neu(4),4))

        Sub29(Gen(1),Neu(4),Sfe8) = 
     &   MW*(SB*USf(Sfe8,1,3,Gen(1))*ZNeuC(Neu(4),2)) + 
     &    Mf(3,Gen(1))*USf(Sfe8,2,3,Gen(1))*ZNeuC(Neu(4),4)

        Sub30(Gen(1),Neu(4),Sfe8) = 
     &   3*CW*Sub29(Gen(1),Neu(4),Sfe8) + 
     &    MW*(SB*SW*USf(Sfe8,1,3,Gen(1))*ZNeuC(Neu(4),1))

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub34(Gen(2),Neu(4),Sfe8) = 
     &   MW*(SB*Sub15(Neu(4))*USfC(Sfe8,1,3,Gen(2))) + 
     &    3*CW*(Mf(3,Gen(2))*USfC(Sfe8,2,3,Gen(2))*ZNeu(Neu(4),4))

        Sub35(Gen(2),Neu(4),Sfe8) = 
     &   4*MW*(SB*SW*USfC(Sfe8,2,3,Gen(2))*ZNeuC(Neu(4),1)) - 
     &    3*CW*(Mf(3,Gen(2))*USfC(Sfe8,1,3,Gen(2))*ZNeuC(Neu(4),4))

        ENDLOOP(Sfe8)


        Sub17(Gen(6),Neu(3),Sq1) = 
     &   4*MW*(SB*SW*USf(Sq1,2,3,Gen(6))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(6))*USf(Sq1,1,3,Gen(6))*ZNeu(Neu(3),4))

        Sub18(Gen(6),Neu(3),Sq1) = 
     &   MW*(SB*USf(Sq1,1,3,Gen(6))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(6))*USf(Sq1,2,3,Gen(6))*ZNeuC(Neu(3),4)

        Sub19(Gen(6),Neu(3),Sq1) = 
     &   3*CW*Sub18(Gen(6),Neu(3),Sq1) + 
     &    MW*(SB*SW*USf(Sq1,1,3,Gen(6))*ZNeuC(Neu(3),1))


#include "realOS_contains.h"

        end

