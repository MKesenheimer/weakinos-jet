#if 0
* qdg_nInJqd_abbr0s.F
* abbreviations for {F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:40
#endif

#include "qdg_nInJqd_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdg_nInJqd_abbr0s
        implicit none

#include "qdg_nInJqd_vars.h"
#include "virt_inline.h"

        MatSUN(1,1) = 4

        Sub2(Neu(3)) = -(3*CW*ZNeuC(Neu(3),2)) + SW*ZNeuC(Neu(3),1)

        Sub9(Neu(3)) = -(3*CW*ZNeu(Neu(3),2)) + SW*ZNeu(Neu(3),1)

        Sub5(Neu(4)) = -(3*CW*ZNeu(Neu(4),2)) + SW*ZNeu(Neu(4),1)

        Sub14(Neu(4)) = -(3*CW*ZNeuC(Neu(4),2)) + SW*ZNeuC(Neu(4),1)


        Sub1268(Gen(1),Gen(5)) = S - Mf2(4,Gen(1)) - Mf2(4,Gen(5))


        LOOP(Sfe6, 1,2,1)

        Sub72(Gen(1),Sfe6) = 
     &   -(cI*(MSf(Sfe6,4,Gen(1))*WSf(Sfe6,4,Gen(1)))) + 
     &    MSf2(Sfe6,4,Gen(1))

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub15324(Gen(5),Sfe6) = 
     &   -(cI*(MSf(Sfe6,4,Gen(5))*WSf(Sfe6,4,Gen(5)))) + 
     &    MSf2(Sfe6,4,Gen(5))

        ENDLOOP(Sfe6)


        Sub20(Neu(3),Neu(4)) = 
     &   ZNeu(Neu(4),3)*ZNeuC(Neu(3),3) - ZNeu(Neu(4),4)*ZNeuC(Neu(3),4)

        Sub21(Neu(3),Neu(4)) = 
     &   ZNeu(Neu(3),3)*ZNeuC(Neu(4),3) - ZNeu(Neu(3),4)*ZNeuC(Neu(4),4)


        LOOP(Sfe6, 1,2,1)

        Sub1(Gen(1),Neu(3),Sfe6) = 
     &   2*MW*(CB*SW*USf(Sfe6,2,4,Gen(1))*ZNeu(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(1))*USf(Sfe6,1,4,Gen(1))*ZNeu(Neu(3),3))

        Sub3(Gen(1),Neu(3),Sfe6) = 
     &   MW*(CB*Sub2(Neu(3))*USf(Sfe6,1,4,Gen(1))) + 
     &    3*CW*(Mf(4,Gen(1))*USf(Sfe6,2,4,Gen(1))*ZNeuC(Neu(3),3))

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub13(Gen(1),Neu(4),Sfe6) = 
     &   2*MW*(CB*SW*USf(Sfe6,2,4,Gen(1))*ZNeu(Neu(4),1)) + 
     &    3*CW*(Mf(4,Gen(1))*USf(Sfe6,1,4,Gen(1))*ZNeu(Neu(4),3))

        Sub15(Gen(1),Neu(4),Sfe6) = 
     &   MW*(CB*Sub14(Neu(4))*USf(Sfe6,1,4,Gen(1))) + 
     &    3*CW*(Mf(4,Gen(1))*USf(Sfe6,2,4,Gen(1))*ZNeuC(Neu(4),3))

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub10(Gen(5),Neu(3),Sfe6) = 
     &   MW*(CB*Sub9(Neu(3))*USfC(Sfe6,1,4,Gen(5))) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sfe6,2,4,Gen(5))*ZNeu(Neu(3),3))

        Sub11(Gen(5),Neu(3),Sfe6) = 
     &   2*MW*(CB*SW*USfC(Sfe6,2,4,Gen(5))*ZNeuC(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sfe6,1,4,Gen(5))*ZNeuC(Neu(3),3))

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub6(Gen(5),Neu(4),Sfe6) = 
     &   MW*(CB*Sub5(Neu(4))*USfC(Sfe6,1,4,Gen(5))) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sfe6,2,4,Gen(5))*ZNeu(Neu(4),3))

        Sub7(Gen(5),Neu(4),Sfe6) = 
     &   2*MW*(CB*SW*USfC(Sfe6,2,4,Gen(5))*ZNeuC(Neu(4),1)) + 
     &    3*CW*(Mf(4,Gen(5))*USfC(Sfe6,1,4,Gen(5))*ZNeuC(Neu(4),3))

        ENDLOOP(Sfe6)

#include "virt_contains.h"

        end

