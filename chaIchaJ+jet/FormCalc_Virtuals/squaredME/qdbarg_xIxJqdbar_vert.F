#if 0
* qdbarg_xIxJqdbar_vert.F
* form factors for {-F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], -F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 20:04
#endif

#include "qdbarg_xIxJqdbar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdbarg_xIxJqdbar_vert
        implicit none

#include "qdbarg_xIxJqdbar_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa*(GS*(1/(36.D0*CW2)*
     &          Sub13494(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5))/
     &           (MZ2 - S34 - cI*MZ*WZ) + 
     &         1/9.D0*(IndexDelta(Cha(3),Cha(4))*
     &             Sub13514(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5)))/S34))

#if DEBUG >= 4
        DEB("qdbarg_xIxJqdbar_vert:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

