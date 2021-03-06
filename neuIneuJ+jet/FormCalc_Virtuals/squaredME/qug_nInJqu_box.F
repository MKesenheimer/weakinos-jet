#if 0
* qug_nInJqu_box.F
* form factors for {F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 20:14
#endif

#include "qug_nInJqu_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qug_nInJqu_box
        implicit none

#include "qug_nInJqu_vars.h"
        HelType tmp1

#include "virt_inline.h"

        tmp1 = -(Alfa/(36.D0*CW2)*
     &      (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (Sub5720*(4*AaD0i26(Epsi(dd223))*
     &              Sub2915(HelAll(Neu(3)),Neu(4)) + 
     &             (-4*AaD0i24(Epsi(dd122)) - 
     &                36*AaD0i25(Epsi(dd223)))*
     &              Sub3011(HelAll(Neu(3)),Neu(4)) + 
     &             (-4*AaD0i24(Epsi(dd112)) - 
     &                36*AaD0i25(Epsi(dd233)))*
     &              Sub3287(HelAll(Neu(3)),Neu(4)) + 
     &             Pair1*(36*AaD0i25(Epsi(dd112))*
     &                 Sub3084(HelAll(Neu(3)),Neu(4)) + 
     &                4*AaD0i26(Epsi(dd122))*
     &                 Sub3351(HelAll(Neu(3)),Neu(4))) - 
     &             4*(AaD0i24(Epsi(dd123))*
     &                 Sub3362(HelAll(Neu(3)),Neu(4)) + 
     &                AaD0i26(Epsi(dd123))*
     &                 Sub3378(HelAll(Neu(3)),Neu(4))) - 
     &             36*AaD0i25(Epsi(dd123))*
     &              Sub3383(HelAll(Neu(3)),Neu(4)) - 
     &             Pair3*((4*AaD0i24(Epsi(dd111)) + 
     &                   36*AaD0i25(Epsi(dd333)))*
     &                 Sub1120(HelAll(Neu(3)),Neu(4)) - 
     &                36*(AaD0i25(Epsi(dd113))*
     &                    Sub3084(HelAll(Neu(3)),Neu(4)) + 
     &                   AaD0i25(Epsi(dd133))*
     &                    Sub3401(HelAll(Neu(3)),Neu(4))) + 
     &                4*AaD0i24(Epsi(dd113))*
     &                 Sub3626(HelAll(Neu(3)),Neu(4)))) - 
     &          AaC0i27(Epsi(cc1))*Sub5068(HelAll(Neu(3)),Neu(4)) - 
     &          2*AaC0i28(Epsi(cc1))*Sub5083(HelAll(Neu(3)),Neu(4)) + 
     &          2*AaD0i26(Epsi(dd23))*
     &           Sub5107(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &          AaC0i27(Epsi(cc2))*Sub5116(HelAll(Neu(3)),Neu(4)) - 
     &          2*AaC0i28(Epsi(cc2))*Sub5126(HelAll(Neu(3)),Neu(4)) + 
     &          9*AaD0i25(Epsi(dd23))*
     &           Sub5227(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &          2*AaD0i24(Epsi(dd3))*
     &           Sub5263(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &          2*AaD0i26(Epsi(dd3))*
     &           Sub5352(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &          9*AaD0i25(Epsi(dd3))*
     &           Sub5405(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &          4*(AaD0i24(Epsi(dd33))*
     &              Sub5454(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &             AaD0i26(Epsi(dd33))*
     &              Sub5466(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))) - 
     &          9*AaD0i25(Epsi(dd33))*
     &           Sub5526(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &          (-4*AaD0i24(Epsi(dd001)) - 
     &             36*AaD0i25(Epsi(dd003)))*
     &           Sub5560(HelAll(Neu(3)),Neu(4)) + 
     &          36*AaD0i25(Epsi(dd002))*
     &           Sub5621(HelAll(Neu(3)),Neu(4)) - 
     &          36*AaD0i25(Epsi(dd001))*
     &           Sub5683(HelAll(Neu(3)),Neu(4)) + 
     &          4*(Sub16697(Gen(1),Gen(5),Neu(3),Neu(4))*
     &              Sub5621(HelAll(Neu(3)),Neu(4)) + 
     &             AaD0i26(Epsi(dd002))*
     &              Sub5686(HelAll(Neu(3)),Neu(4)) + 
     &             Sub16698(Gen(1),Gen(5),Neu(3),Neu(4))*
     &              Sub5689(HelAll(Neu(3)),Neu(4))))))

        tmp1 = tmp1 - Alfa/(36.D0*CW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (18*AaD0i25(Epsi(dd00))*
     &          Sub6285(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &         9*AaD0i25(Epsi(dd1))*
     &          Sub6485(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &         4*(AaD0i24(Epsi(dd00))*
     &             Sub6182(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd00))*
     &             Sub6199(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd11))*
     &             Sub6509(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))) + 
     &         2*(AaD0i26(Epsi(dd0))*
     &             Sub6122(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaC0i28(Epsi(cc0))*
     &             Sub6212(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd1))*
     &             Sub6420(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd13))*
     &             Sub6813(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd23))*
     &             Sub6833(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd2))*
     &             Sub7031(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd2))*
     &             Sub7095(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))) + 
     &         9*(AaD0i25(Epsi(dd0))*
     &             Sub6150(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i25(Epsi(dd22))*
     &             Sub6569(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i25(Epsi(dd11))*
     &             Sub6581(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i25(Epsi(dd12))*
     &             Sub6744(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i25(Epsi(dd13))*
     &             Sub6896(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i25(Epsi(dd2))*
     &             Sub7141(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))) - 
     &         AaC0i27(Epsi(cc0))*
     &          Sub7282(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) - 
     &         2*(AaD0i24(Epsi(dd0))*
     &             Sub6267(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd1))*
     &             Sub6356(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd11))*
     &             Sub6531(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd22))*
     &             Sub6541(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd12))*
     &             Sub6672(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd12))*
     &             Sub6710(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i24(Epsi(dd13))*
     &             Sub6762(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4)) + 
     &            AaD0i26(Epsi(dd22))*
     &             Sub7320(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4))) + 
     &         Sub5720*(-(Pair1*
     &               ((36*AaD0i25(Epsi(dd122)) - 
     &                    4*AaD0i26(Epsi(dd112)))*
     &                  Sub589(HelAll(Neu(3)),Neu(4)) - 
     &                 4*AaD0i26(Epsi(dd222))*
     &                  Sub794(HelAll(Neu(3)),Neu(4)))) - 
     &            4*Pair3*AaD0i24(Epsi(dd133))*
     &             Sub797(HelAll(Neu(3)),Neu(4)))))

        tmp1 = tmp1 - Alfa/(9.D0*CW2)*
     &     (Alfas*GS*Pair3*Sub5720*AaD0i26(Epsi(dd233))*
     &       IndexDelta(Gen(1),Gen(5))*Sub797(HelAll(Neu(3)),Neu(4)))

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - tmp1/(MZ2 - S34 - cI*MZ*WZ)

#if DEBUG >= 4
        DEB("qug_nInJqu_box:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

