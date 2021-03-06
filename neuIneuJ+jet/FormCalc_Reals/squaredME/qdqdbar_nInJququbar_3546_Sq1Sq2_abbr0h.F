#if 0
* qdqdbar_nInJququbar_3546_Sq1Sq2_abbr0h.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:17
#endif

#include "qdqdbar_nInJququbar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqdbar_nInJququbar_3546_Sq1Sq2_abbr0h
        implicit none

#include "qdqdbar_nInJququbar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        F1 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F2 = -ChainV0(Spinor(5,1,2),0,0,Spinor(3,-1,1))

        F3 = -ChainV0(Spinor(4,1,2),0,0,Spinor(6,-1,1))

        F4 = ChainB0(Spinor(4,1,2),0,0,Spinor(6,-1,1))

        F5 = ChainB0(Spinor(5,1,2),0,0,Spinor(3,-1,1))

        F6 = ChainV1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F7 = -ChainB1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F8 = -ChainB1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))


        Sub5(HelAll(Gen(5)),Neu(3),Sq1) = 
     &   F2*Sub3(Gen(5),Neu(3),Sq1) - F5*Sub4(Gen(5),Neu(3),Sq1)



        Sub8(HelAll(Gen(6)),Neu(4),Sq1) = 
     &   F3*Sub1(Gen(6),Neu(4),Sq1) - F4*Sub7(Gen(6),Neu(4),Sq1)


#include "realOS_contains.h"

        end

