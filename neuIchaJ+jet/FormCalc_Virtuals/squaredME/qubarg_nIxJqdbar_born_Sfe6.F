#if 0
* qubarg_nIxJqdbar_born_Sfe6.F
* form factors for {-F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], -F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 23:43
#endif

#include "qubarg_nIxJqdbar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qubarg_nIxJqdbar_born_Sfe6
        implicit none

#include "qubarg_nIxJqdbar_vars.h"
#include "virt_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (Alfa*Pi)/(CW*MW2*SW2)*
     &     (GS*(2/3.D0*(Sub19998(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6)/
     &             (S - Mf2(3,Gen(1))) + 
     &            Sub19997(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6)/
     &             (T14 - Sub346(Gen(1),Sfe6))) - 
     &         2/3.D0*(Sub19994(HelAll(Cha(4)),Gen(1),Gen(5),Neu(3),Sfe6)*
     &             Sub35(HelAll(Gen(1)),Neu(3),Sfe6))/
     &           (T - Sub151(Gen(1),Sfe6)))*IndexDelta(Gen(1),Gen(5)))

#if DEBUG >= 4
        DEB("qubarg_nIxJqdbar_born_Sfe6:Ctree(1)", Ctree(HelAll(1)))
#endif

#include "virt_contains.h"

        end

