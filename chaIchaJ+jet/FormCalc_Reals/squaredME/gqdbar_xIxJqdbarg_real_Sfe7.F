#if 0
* gqdbar_xIxJqdbarg_real_Sfe7.F
* form factors for {V[5, {Glu1}], -F[4, {Gen(2), Col2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], -F[4, {Gen(5), Col5}], V[5, {Glu6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 11:00
#endif

#include "gqdbar_xIxJqdbarg_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine gqdbar_xIxJqdbarg_real_Sfe7
        implicit none

#include "gqdbar_xIxJqdbarg_vars.h"
#include "real_inline.h"

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (4*Alfa*Pi**2)/(MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(2),Gen(5))*
     &       Sub584(HelAll(Cha(3)),Cha(4),Gen(2),Gen(5),Sfe7))

#if DEBUG >= 4
        DEB("gqdbar_xIxJqdbarg_real_Sfe7:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (4*Alfa*Pi**2)/(MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(2),Gen(5))*
     &       Sub593(HelAll(Cha(3)),Cha(4),Gen(2),Gen(5),Sfe7))

#if DEBUG >= 4
        DEB("gqdbar_xIxJqdbarg_real_Sfe7:Ctree(1)", Ctree(HelAll(1)))
#endif

#include "real_contains.h"

        end

