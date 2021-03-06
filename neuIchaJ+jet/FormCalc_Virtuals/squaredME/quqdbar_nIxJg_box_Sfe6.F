#if 0
* quqdbar_nIxJg_box_Sfe6.F
* form factors for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 23:19
#endif

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine quqdbar_nIxJg_box_Sfe6
        implicit none

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/(CW*MW2*SW2)*
     &     (Alfas*GS*(2/9.D0*
     &          (Sub21393(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6)/
     &             (Mf2(4,Gen(2)) + Sub20937(Cha(4),Gen(1),Gen(2),Neu(3))) + 
     &            Sub21395(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6)/
     &             (Mf2(3,Gen(1)) + Sub1611(Cha(4),Gen(1),Gen(2),Neu(3)))) + 
     &         1/72.D0*(-(Sub21410(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6)/
     &               (T - Sub1026(Gen(1),Sfe6))) - 
     &            Sub21423(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6)/
     &             (T14 - Sub1204(Gen(1),Sfe6)) - 
     &            Sub21433(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6)/
     &             (T24 - Sub20938(Gen(2),Sfe6)) + 
     &            Sub21446(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6)/
     &             (U - Sub20940(Gen(2),Sfe6))))*
     &       IndexDelta(Gen(1),Gen(2)))

#if DEBUG >= 4
        DEB("quqdbar_nIxJg_box_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

