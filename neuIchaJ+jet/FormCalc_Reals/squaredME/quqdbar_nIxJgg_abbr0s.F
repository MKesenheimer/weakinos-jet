#if 0
* quqdbar_nIxJgg_abbr0s.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], V[5, {Glu5}], V[5, {Glu6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 15-May-2017 11:49
#endif

#include "quqdbar_nIxJgg_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine quqdbar_nIxJgg_abbr0s
        implicit none

#include "quqdbar_nIxJgg_vars.h"
#include "real_inline.h"

        MatSUN(1,1) = 16/3.D0

        MatSUN(1,2) = -2/3.D0

        MatSUN(2,1) = -2/3.D0

        MatSUN(2,2) = 16/3.D0

        Sub9(Neu(3)) = 1/CW*(CW*ZNeu(Neu(3),2) + SW*ZNeu(Neu(3),1))

        Sub19(Neu(3)) = 1/CW*(CW*ZNeuC(Neu(3),2) + SW*ZNeuC(Neu(3),1))

        Sub309(Neu(3)) = -(3*CW*ZNeu(Neu(3),2)) + SW*ZNeu(Neu(3),1)


        Sub7(Cha(4),Neu(3)) = 
     &   1/Sqrt(2.D0)*(UChaC(Cha(4),2)*ZNeu(Neu(3),3)) + 
     &    UChaC(Cha(4),1)*ZNeu(Neu(3),2)

        Sub10(Cha(4),Neu(3)) = 
     &   1/Sqrt(2.D0)*(Sub9(Neu(3))*VCha(Cha(4),2)) + 
     &    VCha(Cha(4),1)*ZNeu(Neu(3),4)

        Sub20(Cha(4),Neu(3)) = 
     &   1/Sqrt(2.D0)*(Sub19(Neu(3))*UChaC(Cha(4),2)) - 
     &    UChaC(Cha(4),1)*ZNeuC(Neu(3),3)

        Sub22(Cha(4),Neu(3)) = 
     &   -(1/Sqrt(2.D0)*(VCha(Cha(4),2)*ZNeuC(Neu(3),4))) + 
     &    VCha(Cha(4),1)*ZNeuC(Neu(3),2)


        LOOP(Sfe7, 1,2,1)

        Sub608(Gen(1),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(1))*WSf(Sfe7,3,Gen(1)))) + 
     &    MSf2(Sfe7,3,Gen(1))

        Sub614(Gen(1),Sfe7) = 
     &   -(cI*(MSf(Sfe7,4,Gen(1))*WSf(Sfe7,4,Gen(1)))) + 
     &    MSf2(Sfe7,4,Gen(1))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub611(Gen(2),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(2))*WSf(Sfe7,3,Gen(2)))) + 
     &    MSf2(Sfe7,3,Gen(2))

        Sub616(Gen(2),Sfe7) = 
     &   -(cI*(MSf(Sfe7,4,Gen(2))*WSf(Sfe7,4,Gen(2)))) + 
     &    MSf2(Sfe7,4,Gen(2))

        ENDLOOP(Sfe7)


        Sub291(Cha(4),Gen(1),Gen(2)) = 
     &   MCha2(Cha(4)) + 2*Mf2(3,Gen(1)) + Mf2(4,Gen(2))


        LOOP(Sfe7, 1,2,1)

        Sub242(Cha(4),Gen(1),Sfe7) = 
     &   -(2*MW*(SB*USfC(Sfe7,1,3,Gen(1))*VCha(Cha(4),1))) + 
     &    Sqrt(2.D0)*(Mf(3,Gen(1))*USfC(Sfe7,2,3,Gen(1))*VCha(Cha(4),2))

        Sub308(Cha(4),Gen(1),Sfe7) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sfe7,1,4,Gen(1)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(1))*UChaC(Cha(4),2)*USf(Sfe7,2,4,Gen(1)))

        ENDLOOP(Sfe7)


        Sub3(Cha(4),Gen(2),Neu(3)) = MCha2(Cha(4)) - Mf2(4,Gen(2)) - MNeu2(Neu(3))


        LOOP(Sfe7, 1,2,1)

        Sub286(Cha(4),Gen(2),Sfe7) = 
     &   -(2*MW*(SB*USfC(Sfe7,1,3,Gen(2))*VCha(Cha(4),1))) + 
     &    Sqrt(2.D0)*(Mf(3,Gen(2))*USfC(Sfe7,2,3,Gen(2))*VCha(Cha(4),2))

        Sub388(Cha(4),Gen(2),Sfe7) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sfe7,1,4,Gen(2)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(2))*UChaC(Cha(4),2)*USf(Sfe7,2,4,Gen(2)))

        ENDLOOP(Sfe7)


        Sub460(Gen(1),Gen(2),Neu(3)) = 
     &   2*Mf2(3,Gen(1)) + Mf2(4,Gen(2)) + MNeu2(Neu(3))


        LOOP(Sfe7, 1,2,1)

        Sub238(Gen(1),Neu(3),Sfe7) = 
     &   4*MW*(SB*SW*USf(Sfe7,2,3,Gen(1))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(1))*USf(Sfe7,1,3,Gen(1))*ZNeu(Neu(3),4))

        Sub239(Gen(1),Neu(3),Sfe7) = 
     &   MW*(SB*USf(Sfe7,1,3,Gen(1))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(1))*USf(Sfe7,2,3,Gen(1))*ZNeuC(Neu(3),4)

        Sub240(Gen(1),Neu(3),Sfe7) = 
     &   3*CW*Sub239(Gen(1),Neu(3),Sfe7) + 
     &    MW*(SB*SW*USf(Sfe7,1,3,Gen(1))*ZNeuC(Neu(3),1))

        Sub340(Gen(1),Neu(3),Sfe7) = 
     &   MW*(CB*Sub309(Neu(3))*USfC(Sfe7,1,4,Gen(1))) + 
     &    3*CW*(Mf(4,Gen(1))*USfC(Sfe7,2,4,Gen(1))*ZNeu(Neu(3),3))

        Sub341(Gen(1),Neu(3),Sfe7) = 
     &   2*MW*(CB*SW*USfC(Sfe7,2,4,Gen(1))*ZNeuC(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(1))*USfC(Sfe7,1,4,Gen(1))*ZNeuC(Neu(3),3))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub310(Gen(2),Neu(3),Sfe7) = 
     &   MW*(CB*Sub309(Neu(3))*USfC(Sfe7,1,4,Gen(2))) + 
     &    3*CW*(Mf(4,Gen(2))*USfC(Sfe7,2,4,Gen(2))*ZNeu(Neu(3),3))

        Sub311(Gen(2),Neu(3),Sfe7) = 
     &   2*MW*(CB*SW*USfC(Sfe7,2,4,Gen(2))*ZNeuC(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(2))*USfC(Sfe7,1,4,Gen(2))*ZNeuC(Neu(3),3))

        ENDLOOP(Sfe7)


        Sub1(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   MCha2(Cha(4)) + Mf2(3,Gen(1)) + 2*Mf2(4,Gen(2)) + 3*MNeu2(Neu(3))

        Sub4(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   MCha2(Cha(4)) + Mf2(3,Gen(1)) + Mf2(4,Gen(2)) + MNeu2(Neu(3))

        Sub5(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   3*MCha2(Cha(4)) + Mf2(3,Gen(1)) + 2*Mf2(4,Gen(2)) + MNeu2(Neu(3))

        Sub48(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   MCha2(Cha(4)) + 3*Mf2(3,Gen(1)) - Mf2(4,Gen(2)) + MNeu2(Neu(3))

        Sub2(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   3*MCha2(Cha(4)) + Sub460(Gen(1),Gen(2),Neu(3))


#include "real_contains.h"

        end
