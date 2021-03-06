#if 0
* qug_xIxJqu_vert_Gen6.F
* form factors for {F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 20:33
#endif

#include "qug_xIxJqu_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qug_xIxJqu_vert_Gen6
        implicit none

#include "qug_xIxJqu_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    Alfa*(Alfas*GS*(1/(MW*SW2)*
     &           (Mf2(3,Gen6)*
     &             Sub12975(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Gen6)) + 
     &          1/12.D0*Sub12983(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Gen6)/
     &            (MZ2 - S34 - cI*MZ*WZ))*IndexDelta(Gen(1),Gen(5)))/
     &      (S + T + T14 - MCha2(Cha(3)) - MCha2(Cha(4)) - 
     &        2*Mf2(3,Gen(1)) - Mf2(3,Gen(5)))

#if DEBUG >= 4
        DEB("qug_xIxJqu_vert_Gen6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

