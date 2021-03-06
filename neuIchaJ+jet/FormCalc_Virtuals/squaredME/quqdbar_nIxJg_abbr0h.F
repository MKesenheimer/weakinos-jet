#if 0
* quqdbar_nIxJg_abbr0h.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 23:19
#endif

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine quqdbar_nIxJg_abbr0h
        implicit none

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"

        F1 = ChainB0(Spinor(1,1,1),1,0,Spinor(4,-1,1))

        F2 = ChainV2(Spinor(2,-1,2),0,ec(5),k(4),1,Spinor(3,1,2))

        F3 = ChainV1(Spinor(2,-1,2),0,ec(5),1,Spinor(3,1,2))

        F4 = ChainB0(Spinor(3,1,2),0,0,Spinor(1,1,1))

        F5 = ChainV1(Spinor(2,-1,2),0,ec(5),0,Spinor(4,-1,1))

        F6 = ChainV1(Spinor(2,-1,2),0,ec(5),0,Spinor(1,1,1))

        F7 = ChainV1(Spinor(3,1,2),0,k(1),0,Spinor(4,-1,1))

        F8 = -ChainB1(Spinor(3,1,2),0,k(1),0,Spinor(4,-1,1))

        F9 = -ChainV2(Spinor(2,-1,2),0,ec(5),k(3),0,Spinor(4,-1,1))

        F10 = -ChainB1(Spinor(2,-1,2),0,ec(5),1,Spinor(3,1,2))

        F11 = ChainB1(Spinor(2,-1,2),0,ec(5),0,Spinor(4,-1,1))

        F12 = -ChainV0(Spinor(3,1,2),0,0,Spinor(4,-1,1))

        F13 = ChainB0(Spinor(3,1,2),0,0,Spinor(4,-1,1))

        F14 = ChainV0(Spinor(2,-1,2),0,1,Spinor(3,1,2))

        F15 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F16 = ChainV1(Spinor(3,1,2),0,ec(5),0,Spinor(4,-1,1))

        F17 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F18 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))

        F19 = -ChainB1(Spinor(3,1,2),0,ec(5),0,Spinor(4,-1,1))

        F20 = ChainV1(Spinor(3,1,2),0,k(2),0,Spinor(4,-1,1))

        F21 = -ChainB1(Spinor(3,1,2),0,k(2),0,Spinor(4,-1,1))

        F22 = -ChainV0(Spinor(1,1,1),1,0,Spinor(4,-1,1))

        F23 = ChainV0(Spinor(3,1,2),0,0,Spinor(1,1,1))

        F24 = -ChainB2(Spinor(2,-1,2),0,ec(5),k(3),0,Spinor(4,-1,1))

        F25 = ChainV0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F26 = -ChainV2(Spinor(2,-1,2),0,ec(5),k(1),0,Spinor(4,-1,1))

        F27 = -ChainB2(Spinor(2,-1,2),0,ec(5),k(1),0,Spinor(4,-1,1))

        F28 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F29 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(4,-1,1))

        F30 = ChainV1(Spinor(2,-1,2),0,ec(5),0,Spinor(3,-1,1))

        F31 = ChainB1(Spinor(2,-1,2),0,ec(5),0,Spinor(3,-1,1))

        F32 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(4,-1,1))

        F33 = -ChainV2(Spinor(2,-1,2),0,ec(5),k(1),0,Spinor(3,-1,1))

        F34 = -ChainV2(Spinor(2,-1,2),0,ec(5),k(4),0,Spinor(3,-1,1))

        F35 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(3,-1,1))

        F36 = -ChainB2(Spinor(2,-1,2),0,ec(5),k(1),0,Spinor(3,-1,1))

        F37 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(3,-1,1))

        F38 = -ChainB2(Spinor(2,-1,2),0,ec(5),k(4),0,Spinor(3,-1,1))

        F39 = ChainV2(Spinor(3,1,2),0,ec(5),k(2),0,Spinor(1,1,1))

        F40 = ChainV1(Spinor(3,1,2),0,ec(5),0,Spinor(1,1,1))

        F41 = ChainB1(Spinor(3,1,2),0,ec(5),0,Spinor(1,1,1))

        F42 = ChainV2(Spinor(3,1,2),0,ec(5),k(4),0,Spinor(1,1,1))

        F43 = ChainB2(Spinor(3,1,2),0,ec(5),k(2),0,Spinor(1,1,1))

        F44 = ChainB2(Spinor(3,1,2),0,ec(5),k(4),0,Spinor(1,1,1))

        F45 = ChainV2(Spinor(4,1,2),0,ec(5),k(2),0,Spinor(1,1,1))

        F46 = ChainB1(Spinor(4,1,2),0,ec(5),0,Spinor(1,1,1))

        F47 = ChainV2(Spinor(4,1,2),0,ec(5),k(3),0,Spinor(1,1,1))

        F48 = ChainV1(Spinor(4,1,2),0,ec(5),0,Spinor(1,1,1))

        F49 = ChainV0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F50 = ChainB0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F51 = ChainB2(Spinor(4,1,2),0,ec(5),k(2),0,Spinor(1,1,1))

        F52 = ChainB2(Spinor(4,1,2),0,ec(5),k(3),0,Spinor(1,1,1))

        Pair1 = Pair(ec(5),k(1))

        Pair2 = Pair(ec(5),k(4))

        Pair3 = Pair(ec(5),k(3))

        Pair4 = Pair(ec(5),k(2))

        Abb1 = Pair1 - Pair2

        Abb2 = Pair1 - Pair3

        Sub5(HelAll(Cha(4))) = F6*F8 - F4*(F9 - F5*MCha(Cha(4)))

        Sub4242(HelAll(Cha(4))) = 
     &   F13*F6*MCha(Cha(4)) + F4*(F9 - F5*MCha(Cha(4)))

        Sub5801(HelAll(Cha(4))) = 
     &   (F15 + F18)*F19 + Sub4242(HelAll(Cha(4)))

        Sub44(HelAll(Cha(4))) = 
     &   Sqrt(2.D0)*(Abb1*F50) + 
     &    1/Sqrt(2.D0)*(F51 - F52 + F48*MCha(Cha(4)))

        Sub46(HelAll(Cha(4))) = F45 - F47 + 2*Abb1*F49 + F46*MCha(Cha(4))

        Sub4(HelAll(Neu(3))) = F6*F7 - F1*(F2 + F3*MNeu(Neu(3)))

        Sub37(HelAll(Neu(3))) = 
     &   Sqrt(2.D0)*(Abb2*F23) + 
     &    1/Sqrt(2.D0)*(F39 - F42 + F41*MNeu(Neu(3)))

        Sub38(HelAll(Neu(3))) = 
     &   Sqrt(2.D0)*(Abb2*F4) + 
     &    1/Sqrt(2.D0)*(F43 - F44 + F40*MNeu(Neu(3)))

        Sub11394(HelAll(Neu(3))) = 2*Abb2*F23 - F42 + F41*MNeu(Neu(3))

        Sub18068(HelAll(Neu(3))) = 2*Abb2*F4 - F44 + F40*MNeu(Neu(3))

        Sub34(HelAll(Neu(3))) = F39 + Sub11394(HelAll(Neu(3)))

        Sub35(HelAll(Neu(3))) = F43 + Sub18068(HelAll(Neu(3)))


        Sub47(HelAll(Cha(4)),Gen(1)) = 
     &   1/Sqrt(2.D0)*(F46*Mf(3,Gen(1))) + Sub44(HelAll(Cha(4)))

        Sub48(HelAll(Cha(4)),Gen(1)) = 
     &   F48*Mf2(3,Gen(1)) + Mf(3,Gen(1))*Sub46(HelAll(Cha(4)))



        Sub20(HelAll(Cha(4)),Gen(2)) = 
     &   F26 - F9 + F5*MCha(Cha(4)) + F11*Mf(4,Gen(2))

        Sub227(HelAll(Cha(4)),Gen(2)) = 
     &   (F24 - F27 - F11*MCha(Cha(4)))*Mf(4,Gen(2)) - F5*Mf2(4,Gen(2))



        Sub3(HelAll(Cha(4)),Neu(3)) = 
     &   F1*F10*Sub1(Cha(4),Neu(3)) + F11*F4*Sub2(Cha(4),Neu(3))

        Sub6(HelAll(Cha(4)),Neu(3)) = 
     &   Sub1(Cha(4),Neu(3))*Sub4(HelAll(Neu(3))) + 
     &    Sub2(Cha(4),Neu(3))*Sub5(HelAll(Cha(4)))

        Sub8(HelAll(Cha(4)),Neu(3)) = 
     &   (F16*F25 - F23*F5)*Sub1(Cha(4),Neu(3)) + 
     &    (F19*F25 - F22*F3)*Sub2(Cha(4),Neu(3))

        Sub3110(HelAll(Cha(4)),Neu(3)) = 
     &   F1*F2 + F6*(F20 + F12*MCha(Cha(4))) + 
     &    (F1*F3 - F13*F6)*MNeu(Neu(3))

        Sub9(HelAll(Cha(4)),Neu(3)) = 
     &   F16*(F15 + F18) + Sub3110(HelAll(Cha(4)),Neu(3))

        Sub10(HelAll(Cha(4)),Neu(3)) = 
     &   Sqrt(2.D0)*(2*F1*F14*(Pair1 + Pair4) - 
     &      Sub9(HelAll(Cha(4)),Neu(3)))

        Sub11(HelAll(Cha(4)),Neu(3)) = 
     &   F6*(F21 - F12*MNeu(Neu(3))) + Sub5801(HelAll(Cha(4)))

        Sub12(HelAll(Cha(4)),Neu(3)) = 
     &   Sqrt(2.D0)*(2*F17*F4*(Pair1 + Pair4) - 
     &      Sub11(HelAll(Cha(4)),Neu(3)))

        Sub13(HelAll(Cha(4)),Neu(3)) = 
     &   Sub1(Cha(4),Neu(3))*Sub10(HelAll(Cha(4)),Neu(3)) + 
     &    Sub12(HelAll(Cha(4)),Neu(3))*Sub2(Cha(4),Neu(3))



        Sub7(HelAll(Cha(4)),Gen(1),Neu(3)) = 
     &   Mf(4,Gen(1))*Sub3(HelAll(Cha(4)),Neu(3)) + 
     &    Sub6(HelAll(Cha(4)),Neu(3))

        Sub14(HelAll(Cha(4)),Gen(1),Neu(3)) = 
     &   Sqrt(2.D0)*(Mf(3,Gen(1))*Sub8(HelAll(Cha(4)),Neu(3))) + 
     &    Sub13(HelAll(Cha(4)),Neu(3))


        LOOP(Sfe6, 1,2,1)

        Sub32(HelAll(Cha(4)),Gen(1),Sfe6) = 
     &   (1/Sqrt(2.D0)*(F32*S2B*Sub25(Cha(4),Gen(1),Sfe6)) + 
     &      2*CB2*F29*Mf(3,Gen(1))*USf(Sfe6,1,4,Gen(1))*VCha(Cha(4),2))/
     &    (CB2*S2B)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub24(HelAll(Cha(4)),Gen(2),Sfe6) = 
     &   (1/Sqrt(2.D0)*(F17*S2B*Sub23(Cha(4),Gen(2),Sfe6)) + 
     &      2*F28*SB2*Mf(4,Gen(2))*UChaC(Cha(4),2)*USfC(Sfe6,1,3,Gen(2)))
     &     /(S2B*SB2)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub18(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   F23*Sub15(Gen(1),Neu(3),Sfe6) - F4*Sub17(Gen(1),Neu(3),Sfe6)

        Sub33(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   F40*Sub15(Gen(1),Neu(3),Sfe6) - F41*Sub17(Gen(1),Neu(3),Sfe6)

        Sub39(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   Sub15(Gen(1),Neu(3),Sfe6)*Sub37(HelAll(Neu(3))) - 
     &    Sub17(Gen(1),Neu(3),Sfe6)*Sub38(HelAll(Neu(3)))

        Sub40(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   1/Sqrt(2.D0)*(Mf(3,Gen(1))*Sub33(HelAll(Gen(1)),Neu(3),Sfe6)) + 
     &    Sub39(HelAll(Gen(1)),Neu(3),Sfe6)

        Sub36(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   Sub15(Gen(1),Neu(3),Sfe6)*Sub34(HelAll(Neu(3))) - 
     &    Sub17(Gen(1),Neu(3),Sfe6)*Sub35(HelAll(Neu(3)))

        Sub41(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   Mf(3,Gen(1))*Sub33(HelAll(Gen(1)),Neu(3),Sfe6) + 
     &    Sub36(HelAll(Gen(1)),Neu(3),Sfe6)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub29(HelAll(Gen(2)),Neu(3),Sfe6) = 
     &   F31*Sub27(Gen(2),Neu(3),Sfe6) + F30*Sub28(Gen(2),Neu(3),Sfe6)

        Sub30(HelAll(Gen(2)),Neu(3),Sfe6) = 
     &   (F33 - F34 + F30*MNeu(Neu(3)))*Sub27(Gen(2),Neu(3),Sfe6) + 
     &    (F36 - F38 + F31*MNeu(Neu(3)))*Sub28(Gen(2),Neu(3),Sfe6)

        Sub31(HelAll(Gen(2)),Neu(3),Sfe6) = 
     &   Mf(4,Gen(2))*Sub29(HelAll(Gen(2)),Neu(3),Sfe6) + 
     &    Sub30(HelAll(Gen(2)),Neu(3),Sfe6)

        Sub43(HelAll(Gen(2)),Neu(3),Sfe6) = 
     &   F35*Sub27(Gen(2),Neu(3),Sfe6) + F37*Sub28(Gen(2),Neu(3),Sfe6)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub22(HelAll(Cha(4)),Gen(1),Gen(2),Sfe6) = 
     &   (1/Sqrt(2.D0)*(S2B*Sub19(Cha(4),Gen(1),Sfe6)*
     &         Sub20(HelAll(Cha(4)),Gen(2))) - 
     &      2*SB2*Sub227(HelAll(Cha(4)),Gen(2))*UChaC(Cha(4),2)*
     &       USfC(Sfe6,1,3,Gen(1)))/(S2B*SB2)

        Sub49(HelAll(Cha(4)),Gen(1),Gen(2),Sfe6) = 
     &   (2*(1/2.D0*(S2B*Sub45(Cha(4),Gen(2),Sfe6)*
     &           Sub47(HelAll(Cha(4)),Gen(1))) + 
     &        CB2*Sub48(HelAll(Cha(4)),Gen(1))*USf(Sfe6,1,4,Gen(2))*
     &         VCha(Cha(4),2)))/(CB2*S2B)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub20939(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6) = 
     &   -(Sub22(HelAll(Cha(4)),Gen(1),Gen(2),Sfe6)/
     &       (Mf2(4,Gen(2)) + Sub20937(Cha(4),Gen(1),Gen(2),Neu(3)))) + 
     &    (2*Abb2*Sub24(HelAll(Cha(4)),Gen(2),Sfe6))/
     &     (T24 - Sub20938(Gen(2),Sfe6))

        Sub20941(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6) = 
     &   (2*Abb1*Sub32(HelAll(Cha(4)),Gen(1),Sfe6))/
     &     (T14 - Sub1204(Gen(1),Sfe6)) + 
     &    Sub49(HelAll(Cha(4)),Gen(1),Gen(2),Sfe6)/
     &     (Mf2(3,Gen(1)) + Sub1611(Cha(4),Gen(1),Gen(2),Neu(3)))

        Sub42(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6) = 
     &   (2*(1/2.D0*(F17*S2B*Sub23(Cha(4),Gen(2),Sfe6)*
     &           Sub40(HelAll(Gen(1)),Neu(3),Sfe6)) + 
     &        F28*SB2*Mf(4,Gen(2))*Sub41(HelAll(Gen(1)),Neu(3),Sfe6)*
     &         UChaC(Cha(4),2)*USfC(Sfe6,1,3,Gen(2))))/(S2B*SB2)

        ENDLOOP(Sfe6)

#include "virt_contains.h"

        end

