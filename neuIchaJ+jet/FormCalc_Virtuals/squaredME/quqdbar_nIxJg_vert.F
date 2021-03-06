#if 0
* quqdbar_nIxJg_vert.F
* form factors for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 23:19
#endif

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine quqdbar_nIxJg_vert
        implicit none

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/SW2*(GS*(2/3.D0*
     &           Sub21234(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3))/
     &            (Mf2(4,Gen(2)) + Sub20937(Cha(4),Gen(1),Gen(2),Neu(3))) + 
     &          1/6.D0*(IndexDelta(Gen(1),Gen(2))*
     &             Sub21247(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3))) - 
     &          2/3.D0*Sub21252(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3))/
     &            (Mf2(3,Gen(1)) + Sub1611(Cha(4),Gen(1),Gen(2),Neu(3)))))/
     &      (MW2 - S34 - cI*MW*WW)

#if DEBUG >= 4
        DEB("quqdbar_nIxJg_vert:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

