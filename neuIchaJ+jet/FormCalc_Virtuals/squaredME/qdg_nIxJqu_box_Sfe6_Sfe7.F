#if 0
* qdg_nIxJqu_box_Sfe6_Sfe7.F
* form factors for {F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 21:46
#endif

#include "qdg_nIxJqu_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdg_nIxJqu_box_Sfe6_Sfe7
        implicit none

#include "qdg_nIxJqu_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/SW2*(Alfas*GS*
     &       (1/(3.D0*Sqrt(2.D0)*MW)*
     &          Sub19275(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6,Sfe7)/
     &           (S2B*(MHp2C - S34)) + 
     &         1/(36.D0*CW*MW2)*
     &          Sub19315(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6,Sfe7) - 
     &         1/6.D0*Sub19348(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6,
     &            Sfe7)/(MW2 - S34 - cI*MW*WW))*
     &       IndexDelta(Gen(1),Gen(5)))

#if DEBUG >= 4
        DEB("qdg_nIxJqu_box_Sfe6_Sfe7:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

