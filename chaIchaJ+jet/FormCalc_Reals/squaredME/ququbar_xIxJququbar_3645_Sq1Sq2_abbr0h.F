#if 0
* ququbar_xIxJququbar_3645_Sq1Sq2_abbr0h.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 11:07
#endif

#include "ququbar_xIxJququbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine ququbar_xIxJququbar_3645_Sq1Sq2_abbr0h
        implicit none

#include "ququbar_xIxJququbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        F1 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))

        F2 = -ChainV0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F3 = ChainB0(Spinor(4,1,2),0,0,Spinor(5,-1,1))

        F4 = -ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))

        F5 = ChainV1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F6 = -ChainB1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F7 = -ChainV0(Spinor(4,1,2),0,0,Spinor(5,-1,1))

        F8 = ChainB0(Spinor(3,1,2),0,0,Spinor(6,-1,1))


        Sub3(HelAll(Cha(3)),Gen(6),Sq1) = 
     &   (2*(1/2.D0*(F8*S2B*Sub1(Cha(3),Gen(6),Sq1)) + 
     &        Sqrt(2.D0)*(CB2*F2*Mf(3,Gen(6))*USf(Sq1,1,4,Gen(6))*
     &           VCha(Cha(3),2))))/(CB2*S2B)

        Sub4(HelAll(Cha(3)),Gen(6),Sq1) = 
     &   (2*(Sqrt(2.D0)*(F8*SB2*Sub1(Cha(3),Gen(6),Sq1)) + 
     &        F2*S2B*Mf(3,Gen(6))*USf(Sq1,1,4,Gen(6))*VCha(Cha(3),2)))/
     &    (S2B*SB2)



        Sub5(HelAll(Cha(3)),Cha(4),Gen(5),Gen(6),Sq1) = 
     &   F7*Sub2(Cha(4),Gen(5),Sq1)*Sub3(HelAll(Cha(3)),Gen(6),Sq1) + 
     &    F3*Mf(3,Gen(5))*Sub4(HelAll(Cha(3)),Gen(6),Sq1)*
     &     USfC(Sq1,1,4,Gen(5))*VChaC(Cha(4),2)


#include "realOS_contains.h"

        end

