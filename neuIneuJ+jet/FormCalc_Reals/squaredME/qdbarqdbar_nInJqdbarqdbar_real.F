#if 0
* qdbarqdbar_nInJqdbarqdbar_real.F
* form factors for {-F[4, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[4, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:17
#endif

#include "qdbarqdbar_nInJqdbarqdbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdbarqdbar_nInJqdbarqdbar_real
        implicit none

#include "qdbarqdbar_nInJqdbarqdbar_vars.h"
#include "real_inline.h"

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (4*Alfa*Pi**2)/(9.D0*CW2)*
     &     (Alfas*Sub271(HelAll(Gen(1)),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))/
     &      (MZ2 - S34 - cI*MZ*WZ)

#if DEBUG >= 4
        DEB("qdbarqdbar_nInJqdbarqdbar_real:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (4*Alfa*Pi**2)/(9.D0*CW2)*
     &     (Alfas*Sub280(HelAll(Gen(1)),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))/
     &      (MZ2 - S34 - cI*MZ*WZ)

#if DEBUG >= 4
        DEB("qdbarqdbar_nInJqdbarqdbar_real:Ctree(1)", Ctree(HelAll(1)))
#endif

#include "real_contains.h"

        end

