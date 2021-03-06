#if 0
* ququbar_xIxJg_vert_Sfe6.F
* form factors for {F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 20:56
#endif

#include "ququbar_xIxJg_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine ququbar_xIxJg_vert_Sfe6
        implicit none

#include "ququbar_xIxJg_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    Alfa/(72.D0*MW2*SW2)*
     &     (GS*IndexDelta(Gen(1),Gen(2))*
     &       (4*MW2*SW2*(Alfas*
     &            Sub14739(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Sfe6)) + 
     &         3*Sub14790(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Sfe6)))

#if DEBUG >= 4
        DEB("ququbar_xIxJg_vert_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

