#if 0
* qdqd_nIxJqdqu_3546_Sq1Sq2_abbr0s.F
* abbreviations for {F[4, {Gen(1), Col1}], F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[4, {Gen(5), Col5}], F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:54
#endif

#include "qdqd_nIxJqdqu_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqd_nIxJqdqu_3546_Sq1Sq2_abbr0s
        implicit none

#include "qdqd_nIxJqdqu_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        MatSUN(1,1) = 9

        MatSUN(1,2) = 3

        MatSUN(2,1) = 3

        MatSUN(2,2) = 9

        Sub2(Neu(3)) = -(3*CW*ZNeu(Neu(3),2)) + SW*ZNeu(Neu(3),1)


        Sub144(Gen(1),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(1))*WSf(Sq1,4,Gen(1)))) + 
     &    MSf2(Sq1,4,Gen(1))

        Sub147(Gen(1),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(1))*(WREG2 + WSf(Sq1,4,Gen(1))))) + 
     &    MSf2(Sq1,4,Gen(1))



        Sub137(Gen(1),Sq2) = 
     &   -(cI*(MSf(Sq2,4,Gen(1))*WSf(Sq2,4,Gen(1)))) + 
     &    MSf2(Sq2,4,Gen(1))

        Sub140(Gen(1),Sq2) = 
     &   -(cI*(MSf(Sq2,4,Gen(1))*(WREG2 + WSf(Sq2,4,Gen(1))))) + 
     &    MSf2(Sq2,4,Gen(1))



        Sub135(Gen(2),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(2))*WSf(Sq1,4,Gen(2)))) + 
     &    MSf2(Sq1,4,Gen(2))

        Sub139(Gen(2),Sq1) = 
     &   -(cI*(MSf(Sq1,4,Gen(2))*(WREG2 + WSf(Sq1,4,Gen(2))))) + 
     &    MSf2(Sq1,4,Gen(2))



        Sub145(Gen(2),Sq2) = 
     &   -(cI*(MSf(Sq2,4,Gen(2))*WSf(Sq2,4,Gen(2)))) + 
     &    MSf2(Sq2,4,Gen(2))

        Sub148(Gen(2),Sq2) = 
     &   -(cI*(MSf(Sq2,4,Gen(2))*(WREG2 + WSf(Sq2,4,Gen(2))))) + 
     &    MSf2(Sq2,4,Gen(2))



        Sub9(Cha(4),Gen(1),Sq2) = 
     &   -(2*MW*(CB*UCha(Cha(4),1)*USfC(Sq2,1,4,Gen(1)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(1))*UCha(Cha(4),2)*USfC(Sq2,2,4,Gen(1)))



        Sub1(Cha(4),Gen(2),Sq2) = 
     &   -(2*MW*(CB*UCha(Cha(4),1)*USfC(Sq2,1,4,Gen(2)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(2))*UCha(Cha(4),2)*USfC(Sq2,2,4,Gen(2)))



        Sub3(Gen(5),Neu(3),Sq1) = 
     &   MW*(CB*Sub2(Neu(3))*USfC(Sq1,1,4,Gen(5))) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sq1,2,4,Gen(5))*ZNeu(Neu(3),3))

        Sub4(Gen(5),Neu(3),Sq1) = 
     &   2*MW*(CB*SW*USfC(Sq1,2,4,Gen(5))*ZNeuC(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sq1,1,4,Gen(5))*ZNeuC(Neu(3),3))



        Opt1(Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -Mf2(3,Gen(6)) - Mf2(4,Gen(1)) - Mf2(4,Gen(2)) - Mf2(4,Gen(5))


#include "realOS_contains.h"

        end

