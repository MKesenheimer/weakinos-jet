#if 0
* qdbarg_nInJqdbar_pent_Sfe6.F
* form factors for {-F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:21
#endif

#include "qdbarg_nInJqdbar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdbarg_nInJqdbar_pent_Sfe6
        implicit none

#include "qdbarg_nInJqdbar_vars.h"
#include "virt_inline.h"

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    Alfa/(CW2*MW2*SW2)*
     &     (Alfas*GS*(1/6.D0*
     &           (AaE0i26(Epsi(ee00),Sfe6)*
     &              Sub12457(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &             Sub17144(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)) - 
     &          1/12.D0*Sub17145(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &          1/54.D0*Sub17150(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &          1/108.D0*Sub17151(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),
     &            Sfe6) + 1/3.D0*
     &           Sub17173(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &          1/108.D0*(AaD0i27(Epsi(dd3),Sfe6)*
     &             Sub8224(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)) + 
     &          1/27.D0*(-(Abb1*
     &                Sub17177(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6))-
     &               Pair2*
     &              Sub17182(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &             Pair1*Sub17186(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),
     &               Sfe6) - 
     &             Sub17219(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &             Sub17147(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8150(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17149(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8155(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17152(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8275(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17153(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8326(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17155(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8329(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &             Sub17156(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8351(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17158(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8371(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17159(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8382(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17161(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8605(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17162(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8776(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &             Sub17163(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8830(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17164(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub8967(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17165(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub9138(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17167(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub9324(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17168(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub9407(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) - 
     &             Sub17170(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub9419(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6) + 
     &             Abb2*Sub17179(Gen(1),Gen(5),Neu(3),Neu(4),Sfe6)*
     &              Sub9882(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe6)))*
     &        IndexDelta(Gen(1),Gen(5)))/CB2

#if DEBUG >= 4
        DEB("qdbarg_nInJqdbar_pent_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

