#if 0
* qdqubar_xIxJqdqubar_356_Sq1_abbr0h.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 11:05
#endif

#include "qdqubar_xIxJqdqubar_356_Sq1_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqubar_xIxJqdqubar_356_Sq1_abbr0h
        implicit none

#include "qdqubar_xIxJqdqubar_356_Sq1_vars.h"
#include "realOS_inline.h"

        F1 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(6,-1,1))

        F2 = ChainB0(Spinor(5,1,2),0,0,Spinor(3,-1,1))

        F3 = ChainV0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F4 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(6,-1,1))

        F5 = ChainV1(Spinor(2,-1,2),0,k(1),0,Spinor(6,-1,1))

        F6 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(6,-1,1))

        F7 = ChainB1(Spinor(2,-1,2),0,k(1),0,Spinor(6,-1,1))

        F8 = ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(6,-1,1))

        F9 = -ChainV0(Spinor(5,1,2),0,0,Spinor(3,-1,1))

        F10 = ChainB0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F11 = ChainB0(Spinor(6,1,2),0,0,Spinor(1,1,1))

        F12 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F13 = ChainB1(Spinor(6,1,2),0,k(4),0,Spinor(1,1,1))

        F14 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F15 = ChainB1(Spinor(6,1,2),0,k(2),0,Spinor(1,1,1))

        F16 = ChainV1(Spinor(6,1,2),0,k(4),0,Spinor(1,1,1))

        F17 = ChainV0(Spinor(6,1,2),0,0,Spinor(1,1,1))

        F18 = ChainV1(Spinor(6,1,2),0,k(2),0,Spinor(1,1,1))

        LOOP(Sfe8, 1,2,1)

        Sub4(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(1/2.D0*(F10*S2B*Sub3(Cha(4),Gen(1),Sfe8)) + 
     &        Sqrt(2.D0)*(F3*SB2*Mf(4,Gen(1))*UCha(Cha(4),2)*
     &           USf(Sfe8,1,3,Gen(1)))))/(S2B*SB2)

        Sub5(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(Sqrt(2.D0)*(CB2*F10*Sub3(Cha(4),Gen(1),Sfe8)) + 
     &        F3*S2B*Mf(4,Gen(1))*UCha(Cha(4),2)*USf(Sfe8,1,3,Gen(1))))/
     &    (CB2*S2B)

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub11(HelAll(Gen(1)),Gen(6),Sq1,Sfe8) = 
     &   (F16 - F18)*USf(Sq1,2,3,Gen(6))*USf(Sfe8,1,4,Gen(1)) + 
     &    (F13 - F15)*USf(Sq1,1,3,Gen(6))*USf(Sfe8,2,4,Gen(1))

        Sub12(HelAll(Gen(1)),Gen(6),Sq1,Sfe8) = 
     &   Sub11(HelAll(Gen(1)),Gen(6),Sq1,Sfe8) + 
     &    F11*Mino3C*USf(Sq1,1,3,Gen(6))*USf(Sfe8,1,4,Gen(1)) + 
     &    F17*Mino3*USf(Sq1,2,3,Gen(6))*USf(Sfe8,2,4,Gen(1))

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub1(HelAll(Gen(2)),Gen(6),Sq1,Sfe8) = 
     &   (F1 - F5)*USf(Sq1,1,3,Gen(6))*USfC(Sfe8,1,3,Gen(2)) + 
     &    (-F7 + F8)*USf(Sq1,2,3,Gen(6))*USfC(Sfe8,2,3,Gen(2))

        Sub8(HelAll(Gen(2)),Gen(6),Sq1,Sfe8) = 
     &   F6*Mino3*USf(Sq1,2,3,Gen(6))*USfC(Sfe8,1,3,Gen(2)) + 
     &    F4*Mino3C*USf(Sq1,1,3,Gen(6))*USfC(Sfe8,2,3,Gen(2))

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub6(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sq1,Sfe8) = 
     &   F9*Sub2(Cha(3),Gen(5),Sq1)*Sub4(HelAll(Cha(4)),Gen(1),Sfe8) + 
     &    F2*Mf(4,Gen(5))*Sub5(HelAll(Cha(4)),Gen(1),Sfe8)*UChaC(Cha(3),2)*
     &     USfC(Sq1,1,3,Gen(5))

        Sub7(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sq1,Sfe8) = 
     &   (2*(CB2*(1/2.D0*(F9*S2B*Sub2(Cha(3),Gen(5),Sq1)*
     &              Sub4(HelAll(Cha(4)),Gen(1),Sfe8)) + 
     &           Sqrt(2.D0)*(F10*F2*Mf(4,Gen(5))*Sub3(Cha(4),Gen(1),Sfe8)*
     &              UChaC(Cha(3),2)*USfC(Sq1,1,3,Gen(5)))) + 
     &        F2*F3*S2B*Mf(4,Gen(1))*Mf(4,Gen(5))*UCha(Cha(4),2)*
     &         UChaC(Cha(3),2)*USf(Sfe8,1,3,Gen(1))*USfC(Sq1,1,3,Gen(5))
     &        ))/(CB2*S2B)

        ENDLOOP(Sfe8)

#include "realOS_contains.h"

        end

