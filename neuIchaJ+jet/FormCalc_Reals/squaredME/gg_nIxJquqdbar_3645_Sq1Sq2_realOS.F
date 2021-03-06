#if 0
* gg_nIxJquqdbar_3645_Sq1Sq2_realOS.F
* form factors for {V[5, {Glu1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:54
#endif

#include "gg_nIxJquqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine gg_nIxJquqdbar_3645_Sq1Sq2_realOS
        implicit none

#include "gg_nIxJquqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(5),Gen(6))*IndexDelta(Sq1,Sq2)*
     &        Sub11(HelAll(Gen(6)),Neu(3),Sq1)*
     &        Sub12(HelAll(Cha(4)),Gen(5),Sq1)*
     &        Sub457(HelAll(Cha(4)),Gen(5),Gen(6),Sq1)*
     &        Sub460(Cha(4),Gen(5),Gen(6),Neu(3),Sq1))/
     &      Sub463(Cha(4),Gen(5),Gen(6),Neu(3),Sq1)

#if DEBUG >= 4
        DEB("gg_nIxJquqdbar_3645_Sq1Sq2_realOS:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(5),Gen(6))*IndexDelta(Sq1,Sq2)*
     &        Sub11(HelAll(Gen(6)),Neu(3),Sq1)*
     &        Sub12(HelAll(Cha(4)),Gen(5),Sq1)*
     &        Sub460(Cha(4),Gen(5),Gen(6),Neu(3),Sq1)*
     &        Sub465(HelAll(Cha(4)),Gen(5),Gen(6),Neu(3),Sq1))/
     &      Sub463(Cha(4),Gen(5),Gen(6),Neu(3),Sq1)

#if DEBUG >= 4
        DEB("gg_nIxJquqdbar_3645_Sq1Sq2_realOS:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "realOS_contains.h"

        end

