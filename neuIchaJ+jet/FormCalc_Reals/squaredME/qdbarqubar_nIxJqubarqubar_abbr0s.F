#if 0
* qdbarqubar_nIxJqubarqubar_abbr0s.F
* abbreviations for {-F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], -F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:57
#endif

#include "qdbarqubar_nIxJqubarqubar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdbarqubar_nIxJqubarqubar_abbr0s
        implicit none

#include "qdbarqubar_nIxJqubarqubar_vars.h"
#include "real_inline.h"

        MatSUN(1,1) = 9

        MatSUN(1,2) = 3

        MatSUN(2,1) = 3

        MatSUN(2,2) = 9

        Sub63(Neu(3)) = -(3*CW*ZNeu(Neu(3),2)) + SW*ZNeu(Neu(3),1)

        Sub109(Neu(3)) = 3*CW*ZNeu(Neu(3),2) + SW*ZNeu(Neu(3),1)

        Sub116(Neu(3)) = 1/CW*(CW*ZNeu(Neu(3),2) + SW*ZNeu(Neu(3),1))

        Sub119(Neu(3)) = 1/CW*(CW*ZNeuC(Neu(3),2) + SW*ZNeuC(Neu(3),1))


        Sub19(Cha(4),Neu(3)) = 
     &   1/Sqrt(2.D0)*(UChaC(Cha(4),2)*ZNeu(Neu(3),3)) + 
     &    UChaC(Cha(4),1)*ZNeu(Neu(3),2)

        Sub22(Cha(4),Neu(3)) = 
     &   -(1/Sqrt(2.D0)*(VCha(Cha(4),2)*ZNeuC(Neu(3),4))) + 
     &    VCha(Cha(4),1)*ZNeuC(Neu(3),2)

        Sub117(Cha(4),Neu(3)) = 
     &   1/Sqrt(2.D0)*(Sub116(Neu(3))*VCha(Cha(4),2)) + 
     &    VCha(Cha(4),1)*ZNeu(Neu(3),4)

        Sub120(Cha(4),Neu(3)) = 
     &   1/Sqrt(2.D0)*(Sub119(Neu(3))*UChaC(Cha(4),2)) - 
     &    UChaC(Cha(4),1)*ZNeuC(Neu(3),3)


        LOOP(Sfe7, 1,2,1)

        Sub191(Gen(1),Sfe7) = 
     &   -(cI*(MSf(Sfe7,4,Gen(1))*WSf(Sfe7,4,Gen(1)))) + 
     &    MSf2(Sfe7,4,Gen(1))

        Sub196(Gen(1),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(1))*WSf(Sfe7,3,Gen(1)))) + 
     &    MSf2(Sfe7,3,Gen(1))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub155(Gen(1),Sfe8) = 
     &   -(cI*(MSf(Sfe8,4,Gen(1))*WSf(Sfe8,4,Gen(1)))) + 
     &    MSf2(Sfe8,4,Gen(1))

        Sub158(Gen(1),Sfe8) = 
     &   -(cI*(MSf(Sfe8,4,Gen(1))*(WREG2 + WSf(Sfe8,4,Gen(1))))) + 
     &    MSf2(Sfe8,4,Gen(1))

        ENDLOOP(Sfe8)


        Sub43(Gen(2),Gen(5)) = 
     &   IndexDelta(Gen(2),Gen(5)) - GaugeXi(3)*IndexDelta(Gen(2),Gen(5))



        Sub24(Gen(2),Gen(6)) = 
     &   IndexDelta(Gen(2),Gen(6)) - GaugeXi(3)*IndexDelta(Gen(2),Gen(6))


        LOOP(Sfe7, 1,2,1)

        Sub153(Gen(2),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(2))*WSf(Sfe7,3,Gen(2)))) + 
     &    MSf2(Sfe7,3,Gen(2))

        Sub157(Gen(2),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(2))*(WREG2 + WSf(Sfe7,3,Gen(2))))) + 
     &    MSf2(Sfe7,3,Gen(2))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub207(Gen(2),Sfe8) = 
     &   -(cI*(MSf(Sfe8,3,Gen(2))*WSf(Sfe8,3,Gen(2)))) + 
     &    MSf2(Sfe8,3,Gen(2))

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub189(Gen(5),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(5))*WSf(Sfe7,3,Gen(5)))) + 
     &    MSf2(Sfe7,3,Gen(5))

        Sub190(Gen(5),Sfe7) = 
     &   -(cI*(MSf(Sfe7,4,Gen(5))*WSf(Sfe7,4,Gen(5)))) + 
     &    MSf2(Sfe7,4,Gen(5))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub194(Gen(6),Sfe7) = 
     &   -(cI*(MSf(Sfe7,4,Gen(6))*WSf(Sfe7,4,Gen(6)))) + 
     &    MSf2(Sfe7,4,Gen(6))

        Sub197(Gen(6),Sfe7) = 
     &   -(cI*(MSf(Sfe7,3,Gen(6))*WSf(Sfe7,3,Gen(6)))) + 
     &    MSf2(Sfe7,3,Gen(6))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub52(Cha(4),Gen(1),Sfe7) = 
     &   -(2*MW*(SB*USfC(Sfe7,1,3,Gen(1))*VCha(Cha(4),1))) + 
     &    Sqrt(2.D0)*(Mf(3,Gen(1))*USfC(Sfe7,2,3,Gen(1))*VCha(Cha(4),2))

        Sub74(Cha(4),Gen(1),Sfe7) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sfe7,1,4,Gen(1)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(1))*UChaC(Cha(4),2)*USf(Sfe7,2,4,Gen(1)))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub6(Cha(4),Gen(1),Sfe8) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sfe8,1,4,Gen(1)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(1))*UChaC(Cha(4),2)*USf(Sfe8,2,4,Gen(1)))

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub39(Cha(4),Gen(5),Sfe7) = 
     &   -(2*MW*(SB*USfC(Sfe7,1,3,Gen(5))*VCha(Cha(4),1))) + 
     &    Sqrt(2.D0)*(Mf(3,Gen(5))*USfC(Sfe7,2,3,Gen(5))*VCha(Cha(4),2))

        Sub62(Cha(4),Gen(5),Sfe7) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sfe7,1,4,Gen(5)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(5))*UChaC(Cha(4),2)*USf(Sfe7,2,4,Gen(5)))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub79(Cha(4),Gen(6),Sfe7) = 
     &   -(2*MW*(CB*UChaC(Cha(4),1)*USf(Sfe7,1,4,Gen(6)))) + 
     &    Sqrt(2.D0)*(Mf(4,Gen(6))*UChaC(Cha(4),2)*USf(Sfe7,2,4,Gen(6)))

        Sub96(Cha(4),Gen(6),Sfe7) = 
     &   -(2*MW*(SB*USfC(Sfe7,1,3,Gen(6))*VCha(Cha(4),1))) + 
     &    Sqrt(2.D0)*(Mf(3,Gen(6))*USfC(Sfe7,2,3,Gen(6))*VCha(Cha(4),2))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub64(Gen(1),Neu(3),Sfe7) = 
     &   MW*(CB*Sub63(Neu(3))*USfC(Sfe7,1,4,Gen(1))) + 
     &    3*CW*(Mf(4,Gen(1))*USfC(Sfe7,2,4,Gen(1))*ZNeu(Neu(3),3))

        Sub65(Gen(1),Neu(3),Sfe7) = 
     &   2*MW*(CB*SW*USfC(Sfe7,2,4,Gen(1))*ZNeuC(Neu(3),1)) + 
     &    3*CW*(Mf(4,Gen(1))*USfC(Sfe7,1,4,Gen(1))*ZNeuC(Neu(3),3))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub110(Gen(2),Neu(3),Sfe8) = 
     &   MW*(SB*Sub109(Neu(3))*USfC(Sfe8,1,3,Gen(2))) + 
     &    3*CW*(Mf(3,Gen(2))*USfC(Sfe8,2,3,Gen(2))*ZNeu(Neu(3),4))

        Sub111(Gen(2),Neu(3),Sfe8) = 
     &   4*MW*(SB*SW*USfC(Sfe8,2,3,Gen(2))*ZNeuC(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(2))*USfC(Sfe8,1,3,Gen(2))*ZNeuC(Neu(3),4))

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub1(Gen(5),Neu(3),Sfe7) = 
     &   4*MW*(SB*SW*USf(Sfe7,2,3,Gen(5))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(5))*USf(Sfe7,1,3,Gen(5))*ZNeu(Neu(3),4))

        Sub2(Gen(5),Neu(3),Sfe7) = 
     &   MW*(SB*USf(Sfe7,1,3,Gen(5))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(5))*USf(Sfe7,2,3,Gen(5))*ZNeuC(Neu(3),4)

        Sub3(Gen(5),Neu(3),Sfe7) = 
     &   3*CW*Sub2(Gen(5),Neu(3),Sfe7) + 
     &    MW*(SB*SW*USf(Sfe7,1,3,Gen(5))*ZNeuC(Neu(3),1))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub98(Gen(5),Neu(3),Sfe8) = 
     &   4*MW*(SB*SW*USf(Sfe8,2,3,Gen(5))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(5))*USf(Sfe8,1,3,Gen(5))*ZNeu(Neu(3),4))

        Sub99(Gen(5),Neu(3),Sfe8) = 
     &   MW*(SB*USf(Sfe8,1,3,Gen(5))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(5))*USf(Sfe8,2,3,Gen(5))*ZNeuC(Neu(3),4)

        Sub100(Gen(5),Neu(3),Sfe8) = 
     &   3*CW*Sub99(Gen(5),Neu(3),Sfe8) + 
     &    MW*(SB*SW*USf(Sfe8,1,3,Gen(5))*ZNeuC(Neu(3),1))

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub10(Gen(6),Neu(3),Sfe7) = 
     &   4*MW*(SB*SW*USf(Sfe7,2,3,Gen(6))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(6))*USf(Sfe7,1,3,Gen(6))*ZNeu(Neu(3),4))

        Sub11(Gen(6),Neu(3),Sfe7) = 
     &   MW*(SB*USf(Sfe7,1,3,Gen(6))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(6))*USf(Sfe7,2,3,Gen(6))*ZNeuC(Neu(3),4)

        Sub12(Gen(6),Neu(3),Sfe7) = 
     &   3*CW*Sub11(Gen(6),Neu(3),Sfe7) + 
     &    MW*(SB*SW*USf(Sfe7,1,3,Gen(6))*ZNeuC(Neu(3),1))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub131(Gen(6),Neu(3),Sfe8) = 
     &   4*MW*(SB*SW*USf(Sfe8,2,3,Gen(6))*ZNeu(Neu(3),1)) - 
     &    3*CW*(Mf(3,Gen(6))*USf(Sfe8,1,3,Gen(6))*ZNeu(Neu(3),4))

        Sub132(Gen(6),Neu(3),Sfe8) = 
     &   MW*(SB*USf(Sfe8,1,3,Gen(6))*ZNeuC(Neu(3),2)) + 
     &    Mf(3,Gen(6))*USf(Sfe8,2,3,Gen(6))*ZNeuC(Neu(3),4)

        Sub133(Gen(6),Neu(3),Sfe8) = 
     &   3*CW*Sub132(Gen(6),Neu(3),Sfe8) + 
     &    MW*(SB*SW*USf(Sfe8,1,3,Gen(6))*ZNeuC(Neu(3),1))

        ENDLOOP(Sfe8)

#include "real_contains.h"

        end

