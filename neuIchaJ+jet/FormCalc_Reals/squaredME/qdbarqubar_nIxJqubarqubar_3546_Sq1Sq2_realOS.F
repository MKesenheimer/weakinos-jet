#if 0
* qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_realOS.F
* form factors for {-F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], -F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 15-May-2017 11:53
#endif

#include "qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_realOS
        implicit none

#include "qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_vars.h"
        HelType dup1, dup2

#include "realOS_inline.h"

        dup1 = 1/(S35 - Sub151(Gen(2),Sq1)) - 
     &    1/(Sub152(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &       Sub153(Gen(1),Sq2))

        dup2 = S35 - Sub152(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &    Sub155(Gen(2),Sq1) - Sub156(Gen(1),Sq2)

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sq1)*
     &        Sub7(HelAll(Gen(1)),Gen(2),Sq1,Sq2)*
     &        Sub8(HelAll(Cha(4)),Gen(1),Gen(6),Sq2))/
     &      (dup2*(MGl2 - cI*(MGl*WGl) + 
     &          Sub158(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))

#if DEBUG >= 4
        DEB("qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_realOS:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sq1)*
     &        Sub7(HelAll(Gen(1)),Gen(2),Sq1,Sq2)*
     &        Sub8(HelAll(Cha(4)),Gen(1),Gen(6),Sq2))/
     &      (dup2*(MGl2 - cI*(MGl*WGl) + 
     &          Sub158(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))

#if DEBUG >= 4
        DEB("qdbarqubar_nIxJqubarqubar_3546_Sq1Sq2_realOS:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "realOS_contains.h"

        end
