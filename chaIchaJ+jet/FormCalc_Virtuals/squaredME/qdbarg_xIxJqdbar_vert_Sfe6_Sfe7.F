#if 0
* qdbarg_xIxJqdbar_vert_Sfe6_Sfe7.F
* form factors for {-F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], -F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 20:04
#endif

#include "qdbarg_xIxJqdbar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdbarg_xIxJqdbar_vert_Sfe6_Sfe7
        implicit none

#include "qdbarg_xIxJqdbar_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    Alfa/SW2*(Alfas*GS*
     &       (-(1/(12.D0*MW2)*
     &            Sub13392(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe6,Sfe7))+
     &           2/9.D0*(Sub13339(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe6,
     &              Sfe7)/(S - Mf2(4,Gen(1))) + 
     &            Sub13350(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe6,Sfe7)/
     &             (Mf2(4,Gen(5)) + Sub836(Cha(3),Cha(4),Gen(1),Gen(5)))))*
     &       IndexDelta(Gen(1),Gen(5)))

#if DEBUG >= 4
        DEB("qdbarg_xIxJqdbar_vert_Sfe6_Sfe7:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

