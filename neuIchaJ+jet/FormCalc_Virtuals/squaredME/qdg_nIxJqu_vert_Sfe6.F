#if 0
* qdg_nIxJqu_vert_Sfe6.F
* form factors for {F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 21:46
#endif

#include "qdg_nIxJqu_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdg_nIxJqu_vert_Sfe6
        implicit none

#include "qdg_nIxJqu_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    Alfa/(72.D0*CW*MW2*SW2)*
     &     (GS*IndexDelta(Gen(1),Gen(5))*
     &       (CW*MW2*(Alfas*
     &            Sub19203(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6)) + 
     &         Sub19182(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6)))

#if DEBUG >= 4
        DEB("qdg_nIxJqu_vert_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

