#if 0
* qdqubar_xIxJqdqubar_365_Sq1_abbr0h.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 11:05
#endif

#include "qdqubar_xIxJqdqubar_365_Sq1_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqubar_xIxJqdqubar_365_Sq1_abbr0h
        implicit none

#include "qdqubar_xIxJqdqubar_365_Sq1_vars.h"
#include "realOS_inline.h"

        F3 = ChainV0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F10 = ChainB0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F12 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F14 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F19 = ChainV0(Spinor(5,1,2),0,0,Spinor(2,1,1))

        F20 = -ChainV0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F21 = ChainV1(Spinor(5,1,2),0,k(1),0,Spinor(2,1,1))

        F22 = ChainV1(Spinor(5,1,2),0,k(4),0,Spinor(2,1,1))

        F23 = ChainB1(Spinor(5,1,2),0,k(1),0,Spinor(2,1,1))

        F24 = ChainB0(Spinor(5,1,2),0,0,Spinor(2,1,1))

        F25 = ChainB1(Spinor(5,1,2),0,k(4),0,Spinor(2,1,1))

        F26 = ChainB0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F27 = ChainV1(Spinor(5,1,2),0,k(4),0,Spinor(1,1,1))

        F28 = ChainV1(Spinor(5,1,2),0,k(2),0,Spinor(1,1,1))

        F29 = ChainB0(Spinor(5,1,2),0,0,Spinor(1,1,1))

        F30 = ChainV0(Spinor(5,1,2),0,0,Spinor(1,1,1))

        F31 = ChainB1(Spinor(5,1,2),0,k(2),0,Spinor(1,1,1))

        F32 = ChainB1(Spinor(5,1,2),0,k(4),0,Spinor(1,1,1))

        LOOP(Sfe8, 1,2,1)

        Sub18(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(1/Sqrt(2.D0)*(F3*S2B*Mf(4,Gen(1))*UCha(Cha(4),2)*
     &           USf(Sfe8,1,3,Gen(1))) + CB2*F10*Sub3(Cha(4),Gen(1),Sfe8)
     &        ))/(CB2*S2B)

        Sub20(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(1/Sqrt(2.D0)*(F10*S2B*Sub3(Cha(4),Gen(1),Sfe8)) + 
     &        2*F3*SB2*Mf(4,Gen(1))*UCha(Cha(4),2)*USf(Sfe8,1,3,Gen(1))))
     &     /(S2B*SB2)

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub24(HelAll(Gen(1)),Gen(5),Sq1,Sfe8) = 
     &   (F27 - F28)*USf(Sfe8,1,4,Gen(1))*USfC(Sq1,1,4,Gen(5)) + 
     &    (-F31 + F32)*USf(Sfe8,2,4,Gen(1))*USfC(Sq1,2,4,Gen(5))

        Sub25(HelAll(Gen(1)),Gen(5),Sq1,Sfe8) = 
     &   Sub24(HelAll(Gen(1)),Gen(5),Sq1,Sfe8) + 
     &    F30*Mino3*USf(Sfe8,2,4,Gen(1))*USfC(Sq1,1,4,Gen(5)) + 
     &    F29*Mino3C*USf(Sfe8,1,4,Gen(1))*USfC(Sq1,2,4,Gen(5))

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub17(HelAll(Gen(2)),Gen(5),Sq1,Sfe8) = 
     &   (F23 - F25)*USfC(Sq1,2,4,Gen(5))*USfC(Sfe8,1,3,Gen(2)) + 
     &    (F21 - F22)*USfC(Sq1,1,4,Gen(5))*USfC(Sfe8,2,3,Gen(2))

        Sub21(HelAll(Gen(2)),Gen(5),Sq1,Sfe8) = 
     &   F19*Mino3*USfC(Sq1,1,4,Gen(5))*USfC(Sfe8,1,3,Gen(2)) + 
     &    F24*Mino3C*USfC(Sq1,2,4,Gen(5))*USfC(Sfe8,2,3,Gen(2))

        Sub19(HelAll(Gen(2)),Gen(5),Sq1,Sfe8) = 
     &   Sub17(HelAll(Gen(2)),Gen(5),Sq1,Sfe8) - 
     &    Sub21(HelAll(Gen(2)),Gen(5),Sq1,Sfe8)

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)

        Sub22(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Sq1,Sfe8) = 
     &   Sub20(HelAll(Cha(4)),Gen(1),Sfe8)*
     &    (Sub17(HelAll(Gen(2)),Gen(5),Sq1,Sfe8) - 
     &      Sub21(HelAll(Gen(2)),Gen(5),Sq1,Sfe8))

        ENDLOOP(Sfe8)

#include "realOS_contains.h"

        end

