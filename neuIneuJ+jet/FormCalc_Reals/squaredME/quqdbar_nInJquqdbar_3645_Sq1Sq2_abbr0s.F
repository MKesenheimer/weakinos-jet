#if 0
* quqdbar_nInJquqdbar_3645_Sq1Sq2_abbr0s.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:18
#endif

#include "quqdbar_nInJquqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine quqdbar_nInJquqdbar_3645_Sq1Sq2_abbr0s
        implicit none

#include "quqdbar_nInJquqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        MatSUN(1,1) = 9

        MatSUN(1,2) = 3

        MatSUN(2,1) = 3

        MatSUN(2,2) = 9

        Sub12(Neu(3)) = -(3*CW*ZNeuC(Neu(3),2)) + SW*ZNeuC(Neu(3),1)

        Sub15(Neu(4)) = 3*CW*ZNeu(Neu(4),2) + SW*ZNeu(Neu(4),1)


        Sub210(Gen(1),Sq2) = 
     &   -(cI*(MSf(Sq2,3,Gen(1))*WSf(Sq2,3,Gen(1)))) + 
     &    MSf2(Sq2,3,Gen(1))

        Sub214(Gen(1),Sq2) = 
     &   -(cI*(MSf(Sq2,3,Gen(1))*(WREG2 + WSf(Sq2,3,Gen(1))))) + 
     &    MSf2(Sq2,3,Gen(1))



        Sub212(Gen(2),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(2))*WSf(Sq1,4,Gen(2)))) + 
     &    MSf2(Sq1,4,Gen(2))

        Sub215(Gen(2),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(2))*(WREG2 + WSf(Sq1,4,Gen(2))))) + 
     &    MSf2(Sq1,4,Gen(2))



        Sub16(Gen(5),Neu(4),Sq2) = 
     &   MW*(SB*Sub15(Neu(4))*USfC(Sq2,1,3,Gen(5))) + 
     &    3*CW*(Mf(3,Gen(5))*USfC(Sq2,2,3,Gen(5))*ZNeu(Neu(4),4))

        Sub17(Gen(5),Neu(4),Sq2) = 
     &   4*MW*(SB*SW*USfC(Sq2,2,3,Gen(5))*ZNeuC(Neu(4),1)) - 
     &    3*CW*(Mf(3,Gen(5))*USfC(Sq2,1,3,Gen(5))*ZNeuC(Neu(4),4))



        Sub11(Gen(6),Neu(3),Sq1) = 
     &   2*MW*(CB*SW*USf(Sq1,2,4,Gen(6))*ZNeu(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(6))*USf(Sq1,1,4,Gen(6))*ZNeu(Neu(3),3))

        Sub13(Gen(6),Neu(3),Sq1) = 
     &   MW*(CB*Sub12(Neu(3))*USf(Sq1,1,4,Gen(6))) + 
     &    3*CW*(Mf(4,Gen(6))*USf(Sq1,2,4,Gen(6))*ZNeuC(Neu(3),3))



        Opt2(Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -Mf2(3,Gen(1)) - Mf2(3,Gen(5)) - Mf2(4,Gen(2)) - Mf2(4,Gen(6))


#include "realOS_contains.h"

        end

