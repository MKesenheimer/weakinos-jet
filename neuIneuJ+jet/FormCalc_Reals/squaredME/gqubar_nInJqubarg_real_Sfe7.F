#if 0
* gqubar_nInJqubarg_real_Sfe7.F
* form factors for {V[5, {Glu1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[3, {Gen(5), Col5}], V[5, {Glu6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:14
#endif

#include "gqubar_nInJqubarg_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine gqubar_nInJqubarg_real_Sfe7
        implicit none

#include "gqubar_nInJqubarg_vars.h"
#include "real_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(2),Gen(5))*
     &        Sub681(HelAll(Gen(2)),Gen(5),Neu(3),Neu(4),Sfe7))/SB2

#if DEBUG >= 4
        DEB("gqubar_nInJqubarg_real_Sfe7:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(2),Gen(5))*
     &        Sub699(HelAll(Gen(2)),Gen(5),Neu(3),Neu(4),Sfe7))/SB2

#if DEBUG >= 4
        DEB("gqubar_nInJqubarg_real_Sfe7:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "real_contains.h"

        end

