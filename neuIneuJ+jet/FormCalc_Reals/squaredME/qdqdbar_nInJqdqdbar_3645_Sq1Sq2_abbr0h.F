#if 0
* qdqdbar_nInJqdqdbar_3645_Sq1Sq2_abbr0h.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[4, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:16
#endif

#include "qdqdbar_nInJqdqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqdbar_nInJqdqdbar_3645_Sq1Sq2_abbr0h
        implicit none

#include "qdqdbar_nInJqdqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        F6 = ChainV1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F8 = -ChainB1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F9 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F10 = ChainV0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F11 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))

        F12 = ChainB0(Spinor(5,1,2),0,0,Spinor(4,-1,1))

        F13 = -ChainV0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F14 = ChainB0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F15 = -ChainV0(Spinor(5,1,2),0,0,Spinor(4,-1,1))

        F16 = -ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))


        Sub26(HelAll(Gen(5)),Neu(4),Sq1) = 
     &   F15*Sub24(Gen(5),Neu(4),Sq1) + F12*Sub25(Gen(5),Neu(4),Sq1)



        Sub21(HelAll(Gen(5)),Neu(4),Sq2) = 
     &   F15*Sub19(Gen(5),Neu(4),Sq2) + F12*Sub20(Gen(5),Neu(4),Sq2)



        Sub17(HelAll(Gen(6)),Neu(3),Sq1) = 
     &   F13*Sub14(Gen(6),Neu(3),Sq1) + F14*Sub16(Gen(6),Neu(3),Sq1)



        Sub22(HelAll(Gen(1)),Gen(2),Sq1,Sq2) = 
     &   (F11 + F6)*USf(Sq2,1,4,Gen(1))*USfC(Sq1,1,4,Gen(2)) + 
     &    (F16 + F8)*USf(Sq2,2,4,Gen(1))*USfC(Sq1,2,4,Gen(2))

        Sub23(HelAll(Gen(1)),Gen(2),Sq1,Sq2) = 
     &   Sub22(HelAll(Gen(1)),Gen(2),Sq1,Sq2) + 
     &    F10*Mino3*USf(Sq2,2,4,Gen(1))*USfC(Sq1,1,4,Gen(2)) + 
     &    F9*Mino3C*USf(Sq2,1,4,Gen(1))*USfC(Sq1,2,4,Gen(2))


#include "realOS_contains.h"

        end

