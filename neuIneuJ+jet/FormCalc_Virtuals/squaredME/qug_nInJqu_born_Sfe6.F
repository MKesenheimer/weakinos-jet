#if 0
* qug_nInJqu_born_Sfe6.F
* form factors for {F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 20:14
#endif

#include "qug_nInJqu_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qug_nInJqu_born_Sfe6
        implicit none

#include "qug_nInJqu_vars.h"
#include "virt_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (2*Alfa*Pi)/(9.D0*CW2*MW2*SW2)*
     &     (GS*IndexDelta(Gen(1),Gen(5))*
     &        ((Sub13(HelAll(Gen(5)),Neu(3),Sfe6)*
     &             Sub16029(HelAll(Gen(1)),Neu(4),Sfe6))/
     &           (Sub16027(Gen(1),Gen(5),Neu(3),Neu(4)) + 
     &             Sub16028(Gen(5),Sfe6)) + 
     &          Sub16030(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)/
     &           (Mf2(3,Gen(5)) + Sub1182(Gen(1),Gen(5),Neu(3),Neu(4))) + 
     &          (Sub16032(HelAll(Gen(1)),Neu(3),Sfe6)*
     &             Sub8(HelAll(Gen(5)),Neu(4),Sfe6))/
     &           (Sub16028(Gen(5),Sfe6) + 
     &             Sub16031(Gen(1),Gen(5),Neu(3),Neu(4)))))/SB2

#if DEBUG >= 4
        DEB("qug_nInJqu_born_Sfe6:Ctree(1)", Ctree(HelAll(1)))
#endif

#include "virt_contains.h"

        end

