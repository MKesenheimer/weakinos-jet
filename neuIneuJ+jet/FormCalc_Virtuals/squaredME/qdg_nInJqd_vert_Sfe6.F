#if 0
* qdg_nInJqd_vert_Sfe6.F
* form factors for {F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:40
#endif

#include "qdg_nInJqd_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdg_nInJqd_vert_Sfe6
        implicit none

#include "qdg_nInJqd_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/CW2*(GS*(1/18.D0*
     &          (Alfas*Sub15512(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6))/
     &           (MZ2 - S34 - cI*MZ*WZ) + 
     &         1/(108.D0*MW2*SW2)*
     &          Sub15611(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)/CB2)*
     &       IndexDelta(Gen(1),Gen(5)))

#if DEBUG >= 4
        DEB("qdg_nInJqd_vert_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

