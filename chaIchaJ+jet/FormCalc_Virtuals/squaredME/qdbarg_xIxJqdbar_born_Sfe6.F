#if 0
* qdbarg_xIxJqdbar_born_Sfe6.F
* form factors for {-F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], -F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 20:04
#endif

#include "qdbarg_xIxJqdbar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdbarg_xIxJqdbar_born_Sfe6
        implicit none

#include "qdbarg_xIxJqdbar_vars.h"
#include "virt_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (Alfa*Pi)/(MW2*SW2)*
     &     (GS*IndexDelta(Gen(1),Gen(5))*
     &       (Sub13273(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe6)/
     &          (T - Sub202(Gen(1),Sfe6)) - 
     &         Sub37(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe6)/
     &          ((S - Mf2(4,Gen(1)))*
     &            (Sub13271(Cha(3),Cha(4),Gen(1),Gen(5)) + 
     &              Sub13272(Gen(5),Sfe6)))))

#if DEBUG >= 4
        DEB("qdbarg_xIxJqdbar_born_Sfe6:Ctree(1)", Ctree(HelAll(1)))
#endif

#include "virt_contains.h"

        end

