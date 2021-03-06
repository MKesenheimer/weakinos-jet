#if 0
* qdqubar_nIxJg_vert_Sfe7.F
* form factors for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 22:08
#endif

#include "qdqubar_nIxJg_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdqubar_nIxJg_vert_Sfe7
        implicit none

#include "qdqubar_nIxJg_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    (Alfa*Pi)/(CW*MW2*SW2)*
     &     (GS*(2/3.D0*(Abb2*Sub21424(HelAll(Gen(1)),Gen(2),Neu(3),Sfe7)*
     &             Sub219(HelAll(Cha(4)),Gen(2),Sfe7))/
     &           (T24 - Sub21139(Gen(2),Sfe7)) - 
     &         2/3.D0*(Abb1*Sub21427(HelAll(Cha(4)),Gen(1),Gen(2),Sfe7)*
     &             Sub254(HelAll(Gen(2)),Neu(3),Sfe7))/
     &           (U - Sub8136(Gen(2),Sfe7)))*IndexDelta(Gen(1),Gen(2)))

#if DEBUG >= 4
        DEB("qdqubar_nIxJg_vert_Sfe7:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

