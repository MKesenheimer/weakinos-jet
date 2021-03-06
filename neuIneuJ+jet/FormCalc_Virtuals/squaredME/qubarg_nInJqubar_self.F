#if 0
* qubarg_nInJqubar_self.F
* form factors for {-F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:04
#endif

#include "qubarg_nInJqubar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qubarg_nInJqubar_self
        implicit none

#include "qubarg_nInJqubar_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/CW2*(GS*(1/9.D0*
     &           Sub17549(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))/
     &            ((S - Mf2(3,Gen(1)))*(S - Mf2(3,Gen(5)))) - 
     &          1/9.D0*Sub17551(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))/
     &            ((Mf2(3,Gen(1)) + Sub1371(Gen(1),Gen(5),Neu(3),Neu(4)))*
     &              (Mf2(3,Gen(5)) + Sub1371(Gen(1),Gen(5),Neu(3),Neu(4))))))
     &       /(MZ2 - S34 - cI*MZ*WZ)

#if DEBUG >= 4
        DEB("qubarg_nInJqubar_self:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

