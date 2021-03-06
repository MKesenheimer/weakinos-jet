#if 0
* qubarg_nInJqubar_abbr0h.F
* abbreviations for {-F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:04
#endif

#include "qubarg_nInJqubar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qubarg_nInJqubar_abbr0h
        implicit none

#include "qubarg_nInJqubar_vars.h"
#include "virt_inline.h"

        F1 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(4,-1,1))

        F2 = -ChainV0(Spinor(3,1,2),0,0,Spinor(5,-1,1))

        F3 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(4,-1,1))

        F4 = ChainB0(Spinor(3,1,2),0,0,Spinor(5,-1,1))

        F5 = -ChainB2(Spinor(1,-1,2),0,e(2),k(2),0,Spinor(4,-1,1))

        F6 = -ChainV2(Spinor(1,-1,2),0,e(2),k(2),0,Spinor(4,-1,1))

        F7 = ChainV1(Spinor(1,-1,2),0,e(2),0,Spinor(4,-1,1))

        F8 = ChainB1(Spinor(1,-1,2),0,e(2),0,Spinor(4,-1,1))

        F9 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(3,-1,1))

        F10 = -ChainV0(Spinor(4,1,2),0,0,Spinor(5,-1,1))

        F11 = ChainB0(Spinor(4,1,2),0,0,Spinor(5,-1,1))

        F12 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(3,-1,1))

        F13 = -ChainB2(Spinor(1,-1,2),0,e(2),k(2),0,Spinor(3,-1,1))

        F14 = -ChainV2(Spinor(1,-1,2),0,e(2),k(2),0,Spinor(3,-1,1))

        F15 = ChainB1(Spinor(1,-1,2),0,e(2),0,Spinor(3,-1,1))

        F16 = ChainV0(Spinor(1,-1,2),0,1,Spinor(4,1,2))

        F17 = ChainB0(Spinor(5,-1,1),1,0,Spinor(3,-1,1))

        F18 = ChainV1(Spinor(1,-1,2),0,e(2),0,Spinor(3,-1,1))

        F19 = -ChainB0(Spinor(1,-1,2),0,1,Spinor(4,1,2))

        F20 = ChainV0(Spinor(5,-1,1),1,0,Spinor(3,-1,1))

        F21 = ChainV2(Spinor(1,-1,2),0,e(2),k(2),1,Spinor(4,1,2))

        F22 = -ChainB1(Spinor(1,-1,2),0,e(2),1,Spinor(4,1,2))

        F23 = -ChainB2(Spinor(1,-1,2),0,e(2),k(2),1,Spinor(4,1,2))

        F24 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(5,-1,1))

        F25 = ChainV1(Spinor(4,1,2),0,e(2),0,Spinor(3,-1,1))

        F26 = ChainV1(Spinor(1,-1,2),0,e(2),1,Spinor(4,1,2))

        F27 = ChainB1(Spinor(1,-1,2),0,e(2),0,Spinor(5,-1,1))

        F28 = -ChainB1(Spinor(4,1,2),0,k(2),0,Spinor(3,-1,1))

        F29 = ChainV1(Spinor(4,1,2),0,k(2),0,Spinor(3,-1,1))

        F30 = -ChainB1(Spinor(4,1,2),0,e(2),0,Spinor(3,-1,1))

        F31 = ChainB1(Spinor(1,-1,2),0,k(2),0,Spinor(5,-1,1))

        F32 = ChainV1(Spinor(1,-1,2),0,e(2),0,Spinor(5,-1,1))

        F33 = ChainV1(Spinor(1,-1,2),0,k(2),0,Spinor(5,-1,1))

        F34 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(5,-1,1))

        F35 = ChainB2(Spinor(4,1,2),0,e(2),k(2),0,Spinor(5,-1,1))

        F36 = -ChainV2(Spinor(4,1,2),0,e(2),k(2),0,Spinor(5,-1,1))

        F37 = ChainV1(Spinor(4,1,2),0,e(2),0,Spinor(5,-1,1))

        F38 = -ChainB1(Spinor(4,1,2),0,e(2),0,Spinor(5,-1,1))

        F39 = ChainB2(Spinor(3,1,2),0,e(2),k(2),0,Spinor(5,-1,1))

        F40 = -ChainB1(Spinor(3,1,2),0,e(2),0,Spinor(5,-1,1))

        F41 = ChainV1(Spinor(3,1,2),0,e(2),0,Spinor(5,-1,1))

        F42 = -ChainV2(Spinor(3,1,2),0,e(2),k(2),0,Spinor(5,-1,1))

        Pair1 = Pair(e(2),k(5))

        Pair2 = Pair(e(2),k(1))

        Pair3 = Pair(e(2),k(3))

        Pair4 = Pair(e(2),k(4))

        Sub31 = -(F27*F29) + F25*F31 + F10*(F13 + 2*F12*Pair1)

        Sub34 = -(F29*F32) + F25*F33 + F17*(F21 + 2*F16*Pair1)

        Sub403 = F20*F23 - F27*F28 + F30*F31

        Sub408 = F11*F14 - F28*F32 + F30*F33

        Abb1 = Pair2 - Pair3

        Abb2 = Pair2 - Pair4

        Sub32 = 2*F19*F20*Pair1 + Sub403

        Sub35 = 2*F11*F9*Pair1 + Sub408


        Sub29(HelAll(Neu(3)),Neu(4)) = 
     &   (F17*F22 - F24*F25)*Sub27(Neu(3),Neu(4)) + 
     &    (-(F11*F15) + F24*F30)*Sub28(Neu(3),Neu(4))

        Sub30(HelAll(Neu(3)),Neu(4)) = 
     &   (F10*F18 - F25*F34)*Sub27(Neu(3),Neu(4)) + 
     &    (-(F20*F26) + F30*F34)*Sub28(Neu(3),Neu(4))

        Sub37(HelAll(Neu(3)),Neu(4)) = 
     &   1/SW2*(4*SW2*Sub29(HelAll(Neu(3)),Neu(4)) - 
     &      4*CW2*Sub30(HelAll(Neu(3)),Neu(4)) + 
     &      Sub30(HelAll(Neu(3)),Neu(4)))

        Sub39(HelAll(Neu(3)),Neu(4)) = 
     &   F17*F22*Sub27(Neu(3),Neu(4)) - F11*F15*Sub28(Neu(3),Neu(4))

        Sub40(HelAll(Neu(3)),Neu(4)) = 
     &   F10*F18*Sub27(Neu(3),Neu(4)) - F20*F26*Sub28(Neu(3),Neu(4))

        Sub41(HelAll(Neu(3)),Neu(4)) = 
     &   F10*(F13 + 2*F12*Pair2)*Sub27(Neu(3),Neu(4)) + 
     &    F20*(-F23 - 2*F19*Pair2)*Sub28(Neu(3),Neu(4))

        Sub42(HelAll(Neu(3)),Neu(4)) = 
     &   F17*(F21 + 2*F16*Pair2)*Sub27(Neu(3),Neu(4)) + 
     &    F11*(-F14 - 2*F9*Pair2)*Sub28(Neu(3),Neu(4))

        Sub43(HelAll(Neu(3)),Neu(4)) = 
     &   1/SW2*(-(4*CW2*Sub39(HelAll(Neu(3)),Neu(4))) + 
     &      4*SW2*Sub40(HelAll(Neu(3)),Neu(4)) + 
     &      Sub39(HelAll(Neu(3)),Neu(4)))

        Sub199(HelAll(Neu(3)),Neu(4)) = 
     &   1/SW2*(4*SW2*Sub41(HelAll(Neu(3)),Neu(4)) - 
     &      4*CW2*Sub42(HelAll(Neu(3)),Neu(4)) + 
     &      Sub42(HelAll(Neu(3)),Neu(4)))

        Sub33(HelAll(Neu(3)),Neu(4)) = 
     &   Sub31*Sub27(Neu(3),Neu(4)) - Sub32*Sub28(Neu(3),Neu(4))

        Sub36(HelAll(Neu(3)),Neu(4)) = 
     &   Sub34*Sub27(Neu(3),Neu(4)) - Sub35*Sub28(Neu(3),Neu(4))

        Sub217(HelAll(Neu(3)),Neu(4)) = 
     &   1/SW2*(4*SW2*Sub33(HelAll(Neu(3)),Neu(4)) - 
     &      4*CW2*Sub36(HelAll(Neu(3)),Neu(4)) + 
     &      Sub36(HelAll(Neu(3)),Neu(4)))



        Sub44(HelAll(Gen(1)),Neu(3),Neu(4)) = 
     &   -Sub199(HelAll(Neu(3)),Neu(4)) + 
     &    Mf(3,Gen(1))*Sub43(HelAll(Neu(3)),Neu(4))

        Sub38(HelAll(Gen(1)),Neu(3),Neu(4)) = 
     &   Sub217(HelAll(Neu(3)),Neu(4)) + 
     &    Mf(3,Gen(1))*Sub37(HelAll(Neu(3)),Neu(4))


        LOOP(Sfe6, 1,2,1)

        Sub5(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   F9*Sub3(Gen(1),Neu(3),Sfe6) - F12*Sub4(Gen(1),Neu(3),Sfe6)

        Sub21(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   F15*Sub3(Gen(1),Neu(3),Sfe6) - F18*Sub4(Gen(1),Neu(3),Sfe6)

        Sub23(HelAll(Gen(1)),Neu(3),Sfe6) = 
     &   (F14 + 2*F9*Pair2)*Sub3(Gen(1),Neu(3),Sfe6) + 
     &    (-F13 - 2*F12*Pair2)*Sub4(Gen(1),Neu(3),Sfe6)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub17(HelAll(Gen(1)),Neu(4),Sfe6) = 
     &   F3*Sub11(Gen(1),Neu(4),Sfe6) - F1*Sub16(Gen(1),Neu(4),Sfe6)

        Sub18(HelAll(Gen(1)),Neu(4),Sfe6) = 
     &   F8*Sub11(Gen(1),Neu(4),Sfe6) - F7*Sub16(Gen(1),Neu(4),Sfe6)

        Sub19(HelAll(Gen(1)),Neu(4),Sfe6) = 
     &   (F6 + 2*F3*Pair2)*Sub11(Gen(1),Neu(4),Sfe6) + 
     &    (-F5 - 2*F1*Pair2)*Sub16(Gen(1),Neu(4),Sfe6)

        Sub20(HelAll(Gen(1)),Neu(4),Sfe6) = 
     &   Mf(3,Gen(1))*Sub18(HelAll(Gen(1)),Neu(4),Sfe6) - 
     &    Sub19(HelAll(Gen(1)),Neu(4),Sfe6)

        Sub17472(HelAll(Gen(1)),Neu(4),Sfe6) = 
     &   -(Sub20(HelAll(Gen(1)),Neu(4),Sfe6)/(S - Mf2(3,Gen(1)))) + 
     &    (2*Abb2*Sub17(HelAll(Gen(1)),Neu(4),Sfe6))/
     &     (T14 - Sub81(Gen(1),Sfe6))

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub15(HelAll(Gen(5)),Neu(3),Sfe6) = 
     &   F2*Sub12(Gen(5),Neu(3),Sfe6) - F4*Sub14(Gen(5),Neu(3),Sfe6)

        Sub48(HelAll(Gen(5)),Neu(3),Sfe6) = 
     &   F41*Sub12(Gen(5),Neu(3),Sfe6) - F40*Sub14(Gen(5),Neu(3),Sfe6)

        Sub51(HelAll(Gen(5)),Neu(3),Sfe6) = 
     &   (F42 + 2*F2*Pair1)*Sub12(Gen(5),Neu(3),Sfe6) + 
     &    (-F39 - 2*F4*Pair1)*Sub14(Gen(5),Neu(3),Sfe6)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub22(HelAll(Gen(5)),Neu(4),Sfe6) = 
     &   F10*Sub1(Gen(5),Neu(4),Sfe6) - F11*Sub8(Gen(5),Neu(4),Sfe6)

        Sub46(HelAll(Gen(5)),Neu(4),Sfe6) = 
     &   (F36 + 2*F10*Pair1)*Sub1(Gen(5),Neu(4),Sfe6) + 
     &    (-F35 - 2*F11*Pair1)*Sub8(Gen(5),Neu(4),Sfe6)

        ENDLOOP(Sfe6)

        LOOP(Sfe6, 1,2,1)

        Sub9(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   Sub5(HelAll(Gen(1)),Neu(3),Sfe6)*
     &    (F10*Sub1(Gen(5),Neu(4),Sfe6) - F11*Sub8(Gen(5),Neu(4),Sfe6))

        Sub25(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   Sub23(HelAll(Gen(1)),Neu(3),Sfe6)*
     &    (F10*Sub1(Gen(5),Neu(4),Sfe6) - F11*Sub8(Gen(5),Neu(4),Sfe6))

        Sub26(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   Mf(3,Gen(1))*Sub21(HelAll(Gen(1)),Neu(3),Sfe6)*
     &     Sub22(HelAll(Gen(5)),Neu(4),Sfe6) - 
     &    Sub25(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)

        Sub45(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   Sub5(HelAll(Gen(1)),Neu(3),Sfe6)*
     &    (F37*Sub1(Gen(5),Neu(4),Sfe6) - F38*Sub8(Gen(5),Neu(4),Sfe6))

        Sub47(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   Mf(3,Gen(5))*Sub45(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &    Sub46(HelAll(Gen(5)),Neu(4),Sfe6)*
     &     Sub5(HelAll(Gen(1)),Neu(3),Sfe6)

        Sub50(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   (F3*Sub11(Gen(1),Neu(4),Sfe6) - F1*Sub16(Gen(1),Neu(4),Sfe6))*
     &    Sub48(HelAll(Gen(5)),Neu(3),Sfe6)

        Sub53(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   (F3*Sub11(Gen(1),Neu(4),Sfe6) - F1*Sub16(Gen(1),Neu(4),Sfe6))*
     &    Sub51(HelAll(Gen(5)),Neu(3),Sfe6)

        Sub54(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   Mf(3,Gen(5))*Sub50(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &    Sub53(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)

        Sub17474(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) = 
     &   -(Sub26(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)/
     &       ((S - Mf2(3,Gen(1)))*
     &         (Sub17471(Gen(5),Sfe6) + 
     &           Sub17473(Gen(1),Gen(5),Neu(3),Neu(4))))) - 
     &    Sub47(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)/
     &     ((Mf2(3,Gen(5)) + Sub1371(Gen(1),Gen(5),Neu(3),Neu(4)))*
     &       (T - Sub81(Gen(1),Sfe6)))

        ENDLOOP(Sfe6)

#include "virt_contains.h"

        end

