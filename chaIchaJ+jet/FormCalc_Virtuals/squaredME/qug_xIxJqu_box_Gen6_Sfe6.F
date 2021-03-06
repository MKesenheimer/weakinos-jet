#if 0
* qug_xIxJqu_box_Gen6_Sfe6.F
* form factors for {F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 20:33
#endif

#include "qug_xIxJqu_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qug_xIxJqu_box_Gen6_Sfe6
        implicit none

#include "qug_xIxJqu_vars.h"
        HelType tmp1

#include "virt_inline.h"

        tmp1 = Alfa/(2.D0*MW2*SW2)*
     &    (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (Sqrt(2.D0)*((F14 + F28)*
     &             (AaC0i2(Epsi(cc0),Gen6,Sfe6)*Mf2(3,Gen6)*
     &                Sub2445(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               AaC0i7(Epsi(cc0),Gen6,Sfe6)*Mf2(4,Gen6)*
     &                Sub2449(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            (F16 + F32)*
     &             (Mf2(3,Gen6)*
     &                (AaD0i6(Epsi(dd1),Gen6,Sfe6) - 
     &                  Sub13134(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6))*
     &                Sub2446(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &               2*Pair2*
     &                (AaD0i3(Epsi(dd0),Gen6,Sfe6)*Mf2(3,Gen6)*
     &                   Sub2445(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                  AaD0i8(Epsi(dd0),Gen6,Sfe6)*Mf2(4,Gen6)*
     &                   Sub2449(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &               Mf2(4,Gen6)*
     &                (AaD0i11(Epsi(dd1),Gen6,Sfe6) - 
     &                  Sub13135(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6))*
     &                Sub2451(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))) + 
     &         (F72 + F74)*
     &          (Sqrt(2.D0)*(AaD0i5(Epsi(dd0),Gen6,Sfe6)*Mf2(3,Gen6)*
     &                Sub2446(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &               2*Pair2*
     &                (AaD0i3(Epsi(dd0),Gen6,Sfe6)*Mf2(3,Gen6)*
     &                   Sub2445(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                  AaD0i8(Epsi(dd0),Gen6,Sfe6)*Mf2(4,Gen6)*
     &                   Sub2449(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &               2*Pair4*
     &                (AaD0i4(Epsi(dd0),Gen6,Sfe6)*Mf2(3,Gen6)*
     &                   Sub2445(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                  AaD0i9(Epsi(dd0),Gen6,Sfe6)*Mf2(4,Gen6)*
     &                   Sub2449(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &               AaD0i10(Epsi(dd0),Gen6,Sfe6)*Mf2(4,Gen6)*
     &                Sub2451(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            1/2.D0*(S2B*AaD0i10(Epsi(dd2),Gen6,Sfe6)*
     &               Sub2699(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))) - 
     &         S2B*(AaD0i5(Epsi(dd00),Gen6,Sfe6)*
     &             Sub2577(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            AaD0i10(Epsi(dd00),Gen6,Sfe6)*
     &             Sub2604(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            AaD0i6(Epsi(dd001),Gen6,Sfe6)*
     &             Sub2702(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Sub13136(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub2702(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))))/S2B

        tmp1 = tmp1 + Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (-2*(AaD0i12(Epsi(dd0),Gen6,Sfe6)*
     &             Sub2720(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i12(Epsi(dd3),Gen6,Sfe6)*
     &             Sub2733(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i12(Epsi(dd2),Gen6,Sfe6)*
     &             Sub2749(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i12(Epsi(dd1),Gen6,Sfe6)*
     &             Sub2765(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         (-2*AaD0i11(Epsi(dd001),Gen6,Sfe6) + 
     &            2*Sub13139(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6))*
     &          Sub2793(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         (F14 + F28)*
     &          (AaC0i7(Epsi(cc1),Gen6,Sfe6)*MCha(Cha(3))*
     &             Sub2821(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaC0i2(Epsi(cc2),Gen6,Sfe6)*MCha(Cha(4))*
     &             Sub2849(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         (F72 + F74)*
     &          (AaD0i10(Epsi(dd3),Gen6,Sfe6)*
     &             Sub2713(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*Pair2*MCha(Cha(3))*
     &             Sub13140(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub2821(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            2*Pair4*MCha(Cha(4))*
     &             Sub13141(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub2849(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            Sub13142(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub2876(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i5(Epsi(dd2),Gen6,Sfe6)*
     &             Sub2896(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i5(Epsi(dd3),Gen6,Sfe6)*
     &             Sub2905(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         Abb27*(F16 + F32)*
     &          ((2*AaD0i11(Epsi(dd112),Gen6,Sfe6) - 
     &               2*AaD0i13(Epsi(dd112),Gen6,Sfe6))*
     &             Sub2737(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            (-2*AaD0i14(Epsi(dd112),Gen6,Sfe6) + 
     &               2*AaD0i6(Epsi(dd112),Gen6,Sfe6))*
     &             Sub2916(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         (F16 + F32)*
     &          (Sub13142(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub2876(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*Pair4*(Sub13137(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub2737(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               MCha(Cha(4))*
     &                Sub13141(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub2849(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               Sub13143(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub2916(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &            2*Pair2*(Sub13138(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub2737(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               Sub13144(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub2916(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))))

        tmp1 = tmp1 - Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       ((F14 + F28)*(AaC0i7(Epsi(cc2),Gen6,Sfe6)*MCha(Cha(4))*
     &             Sub3111(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaC0i2(Epsi(cc1),Gen6,Sfe6)*MCha(Cha(3))*
     &             Sub3242(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*(AaD0i15(Epsi(dd0),Gen6,Sfe6)*
     &             Sub2947(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd2),Gen6,Sfe6)*
     &             Sub2975(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd3),Gen6,Sfe6)*
     &             Sub2983(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd1),Gen6,Sfe6)*
     &             Sub3032(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd12),Gen6,Sfe6)*
     &             Sub3446(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         Pair4*(-2*AaD0i4(Epsi(dd11),Gen6,Sfe6)*
     &             Sub3499(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            2*(AaD0i4(Epsi(dd1),Gen6,Sfe6)*
     &                Sub3475(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               AaD0i9(Epsi(dd1),Gen6,Sfe6)*
     &                Sub3526(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))) - 
     &         2*(Sub13147(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub3396(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i12(Epsi(dd22),Gen6,Sfe6)*
     &             Sub3421(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i4(Epsi(dd123),Gen6,Sfe6)*
     &             Sub3488(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i6(Epsi(dd123),Gen6,Sfe6)*
     &             Sub3534(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         (F16 + F32)*
     &          (2*Pair4*MCha(Cha(4))*
     &             Sub13145(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub3111(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            Sub13148(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub3433(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i3(Epsi(dd3),Gen6,Sfe6)*
     &             Sub3490(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            Pair2*(2*Sub13149(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub3473(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               4*AaD0i8(Epsi(dd22),Gen6,Sfe6)*
     &                Sub3528(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            2*AaD0i8(Epsi(dd3),Gen6,Sfe6)*
     &             Sub3545(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         (F72 + F74)*
     &          (-(Sub13148(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &               Sub3433(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            2*AaD0i3(Epsi(dd3),Gen6,Sfe6)*
     &             Sub3490(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            2*(Pair4*MCha(Cha(4))*
     &                Sub13145(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub3111(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               Sub13150(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub3502(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            Pair2*(2*
     &                (MCha(Cha(3))*
     &                   Sub13146(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                   Sub3242(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                  Sub13149(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                   Sub3473(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &               4*AaD0i8(Epsi(dd22),Gen6,Sfe6)*
     &                Sub3528(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            2*AaD0i8(Epsi(dd3),Gen6,Sfe6)*
     &             Sub3545(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))))

        tmp1 = tmp1 - Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (-(Pair4*(2*AaD0i9(Epsi(dd11),Gen6,Sfe6)*
     &               Sub3549(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &              2*AaD0i4(Epsi(dd12),Gen6,Sfe6)*
     &               Sub3678(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))) - 
     &         2*(AaD0i13(Epsi(dd223),Gen6,Sfe6)*
     &             Sub3636(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd11),Gen6,Sfe6)*
     &             Sub3674(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Sub13153(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub3698(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         (F72 + F74)*
     &          (2*Pair2*AaD0i8(Epsi(dd2),Gen6,Sfe6)*
     &             Sub3547(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Pair4*(2*
     &                (Sub13151(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                   Sub3606(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                  AaD0i9(Epsi(dd2),Gen6,Sfe6)*
     &                   Sub3608(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &               4*AaD0i9(Epsi(dd22),Gen6,Sfe6)*
     &                Sub3632(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            2*(AaD0i9(Epsi(dd3),Gen6,Sfe6)*
     &                Sub3638(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               Sub13152(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub3663(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               AaD0i4(Epsi(dd3),Gen6,Sfe6)*
     &                Sub3700(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))) + 
     &         2*((F16 + F32)*Pair2*AaD0i8(Epsi(dd2),Gen6,Sfe6)*
     &             Sub3547(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i11(Epsi(dd223),Gen6,Sfe6)*
     &             Sub3553(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i14(Epsi(dd123),Gen6,Sfe6)*
     &             Sub3628(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i5(Epsi(dd123),Gen6,Sfe6)*
     &             Sub3658(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd13),Gen6,Sfe6)*
     &             Sub3720(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         AaD0i6(Epsi(dd12),Gen6,Sfe6)*
     &          Sub3729(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         2*Pair2*(AaD0i3(Epsi(dd12),Gen6,Sfe6)*
     &             Sub3594(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i8(Epsi(dd1),Gen6,Sfe6)*
     &             Sub3667(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i8(Epsi(dd11),Gen6,Sfe6)*
     &             Sub3695(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i3(Epsi(dd1),Gen6,Sfe6)*
     &             Sub3708(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i3(Epsi(dd11),Gen6,Sfe6)*
     &             Sub3731(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         AaC0i16(Epsi(cc0),Gen6,Sfe6)*
     &          Sub3738(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         2*AaD0i12(Epsi(dd23),Gen6,Sfe6)*
     &          Sub3748(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))

        tmp1 = tmp1 - Alfa/(2.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (AaD0i6(Epsi(dd0),Gen6,Sfe6)*
     &           Sub4219(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &          S2B*(1/2.D0*(-(AaC0i17(Epsi(cc1),Gen6,Sfe6)*
     &                   Sub4108(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &                AaC0i16(Epsi(cc2),Gen6,Sfe6)*
     &                 Sub4118(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(AaD0i11(Epsi(dd22),Gen6,Sfe6)*
     &                 Sub3814(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &                AaC0i17(Epsi(cc0),Gen6,Sfe6)*
     &                 Sub3879(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &                AaC0i16(Epsi(cc1),Gen6,Sfe6)*
     &                 Sub3924(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i14(Epsi(dd12),Gen6,Sfe6)*
     &                 Sub4001(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i13(Epsi(dd22),Gen6,Sfe6)*
     &                 Sub4134(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i5(Epsi(dd12),Gen6,Sfe6)*
     &                 Sub4138(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaC0i17(Epsi(cc2),Gen6,Sfe6)*
     &                 Sub4141(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &             1/2.D0*(AaD0i6(Epsi(dd13),Gen6,Sfe6)*
     &                Sub4149(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(AaD0i11(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4212(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i14(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4260(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             Sub13154(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &              Sub3882(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i3(Epsi(dd13),Gen6,Sfe6)*
     &              Sub3921(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             (F16 + F32)*AaD0i8(Epsi(dd23),Gen6,Sfe6)*
     &              Sub4105(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i4(Epsi(dd13),Gen6,Sfe6)*
     &              Sub4112(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             (AaD0i14(Epsi(dd002),Gen6,Sfe6) - 
     &                AaD0i6(Epsi(dd002),Gen6,Sfe6))*
     &              Sub4115(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             (F72 + F74)*
     &              (-(1/2.D0*
     &                   (AaD0i10(Epsi(dd22),Gen6,Sfe6)*
     &                     Sub4228(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))+
     &                  AaD0i8(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4105(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &                AaD0i9(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4122(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i15(Epsi(dd122),Gen6,Sfe6)*
     &                 Sub4252(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             Pair2*Sub13155(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &              Sub4312(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &          AaD0i14(Epsi(dd0),Gen6,Sfe6)*
     &           Sub4319(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))/S2B

        tmp1 = tmp1 - Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (-(AaD0i5(Epsi(dd13),Gen6,Sfe6)*
     &            Sub4420(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         2*(AaD0i15(Epsi(dd00),Gen6,Sfe6)*
     &             Sub4414(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i3(Epsi(dd00),Gen6,Sfe6)*
     &             Sub4424(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*AaD0i4(Epsi(dd00),Gen6,Sfe6)*
     &          Sub4434(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         2*AaD0i6(Epsi(dd00),Gen6,Sfe6)*
     &          Sub4445(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaD0i13(Epsi(dd23),Gen6,Sfe6)*
     &          Sub4469(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         Abb27*(F16 + F32)*
     &          ((2*AaD0i14(Epsi(dd122),Gen6,Sfe6) - 
     &               2*AaD0i6(Epsi(dd122),Gen6,Sfe6))*
     &             Sub4472(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            (2*AaD0i11(Epsi(dd222),Gen6,Sfe6) - 
     &               2*AaD0i13(Epsi(dd222),Gen6,Sfe6))*
     &             Sub4497(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*(Pair4*(AaD0i4(Epsi(dd122),Gen6,Sfe6)*
     &                Sub4448(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               AaD0i4(Epsi(dd112),Gen6,Sfe6)*
     &                Sub4489(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &            AaD0i14(Epsi(dd00),Gen6,Sfe6)*
     &             Sub4506(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         Pair2*(2*Sub13156(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4427(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*(Sub13157(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub4516(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               Sub13158(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                Sub4519(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &            2*Sub13160(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4528(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         (F16 + F32)*
     &          (2*AaD0i15(Epsi(dd133),Gen6,Sfe6)*
     &             Sub4500(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i8(Epsi(dd233),Gen6,Sfe6)*
     &             Sub4531(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         (F72 + F74)*
     &          (2*AaD0i15(Epsi(dd112),Gen6,Sfe6)*
     &             Sub4322(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i15(Epsi(dd133),Gen6,Sfe6)*
     &             Sub4500(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i10(Epsi(dd23),Gen6,Sfe6)*
     &             Sub4510(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            2*AaD0i8(Epsi(dd233),Gen6,Sfe6)*
     &             Sub4531(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*(Sub13159(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4522(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Sub13161(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4534(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))))

        tmp1 = tmp1 + Alfa/(2.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (-(AaD0i13(Epsi(dd0),Gen6,Sfe6)*
     &             Sub4560(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &          S2B*((F16 + F32)*
     &              (AaD0i15(Epsi(dd113),Gen6,Sfe6)*
     &                 Sub4545(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &                AaD0i8(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub4622(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             (F72 + F74)*
     &              (-(AaD0i15(Epsi(dd113),Gen6,Sfe6)*
     &                   Sub4545(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &                Pair4*
     &                 (2*AaD0i4(Epsi(dd22),Gen6,Sfe6)*
     &                    Sub4536(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                   Sub13162(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                    Sub4546(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &                AaD0i8(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub4622(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i12(Epsi(dd122),Gen6,Sfe6)*
     &                 Sub4644(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &                Sub13164(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                 Sub4651(HelAll(Cha(3)),Cha(4),Gen6,Sfe6))) - 
     &          S2B*(1/2.D0*(AaD0i13(Epsi(dd2),Gen6,Sfe6)*
     &                Sub4580(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &             1/2.D0*(AaD0i6(Epsi(dd2),Gen6,Sfe6)*
     &                Sub4612(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &             1/2.D0*(AaD0i11(Epsi(dd3),Gen6,Sfe6)*
     &                Sub4636(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(AaD0i14(Epsi(dd2),Gen6,Sfe6)*
     &                Sub4643(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             AaD0i15(Epsi(dd001),Gen6,Sfe6)*
     &              Sub4542(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             Sub13163(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &              Sub4549(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i11(Epsi(dd233),Gen6,Sfe6)*
     &              Sub4585(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i15(Epsi(dd123),Gen6,Sfe6)*
     &              Sub4589(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i13(Epsi(dd233),Gen6,Sfe6)*
     &              Sub4592(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i3(Epsi(dd123),Gen6,Sfe6)*
     &              Sub4616(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i6(Epsi(dd223),Gen6,Sfe6)*
     &              Sub4617(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i12(Epsi(dd00),Gen6,Sfe6)*
     &              Sub4619(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i14(Epsi(dd223),Gen6,Sfe6)*
     &              Sub4637(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i15(Epsi(dd33),Gen6,Sfe6)*
     &              Sub4650(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             Pair4*(AaD0i6(Epsi(dd133),Gen6,Sfe6)*
     &                 Sub4539(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i5(Epsi(dd133),Gen6,Sfe6)*
     &                 Sub4552(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &                AaD0i9(Epsi(dd122),Gen6,Sfe6)*
     &                 Sub4652(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))))/S2B

        tmp1 = tmp1 + Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (-(AaD0i13(Epsi(dd3),Gen6,Sfe6)*
     &            Sub4672(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         Abb27*(F16 + F32)*
     &          (2*AaD0i11(Epsi(dd122),Gen6,Sfe6) - 
     &            2*AaD0i13(Epsi(dd122),Gen6,Sfe6))*
     &          Sub4673(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         2*(AaD0i8(Epsi(dd00),Gen6,Sfe6)*
     &             Sub4654(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd23),Gen6,Sfe6)*
     &             Sub4675(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         2*(Pair2*Sub13165(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4678(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i9(Epsi(dd00),Gen6,Sfe6)*
     &             Sub4682(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         AaD0i6(Epsi(dd23),Gen6,Sfe6)*
     &          Sub4684(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         2*(AaD0i12(Epsi(dd123),Gen6,Sfe6)*
     &             Sub4685(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i15(Epsi(dd22),Gen6,Sfe6)*
     &             Sub4687(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         2*AaD0i8(Epsi(dd123),Gen6,Sfe6)*
     &          Sub4688(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         AaD0i14(Epsi(dd23),Gen6,Sfe6)*
     &          Sub4690(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         (F16 + F32)*
     &          (2*AaD0i3(Epsi(dd23),Gen6,Sfe6)*
     &             Sub4677(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i3(Epsi(dd233),Gen6,Sfe6)*
     &             Sub4697(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         (F72 + F74)*
     &          (AaD0i5(Epsi(dd33),Gen6,Sfe6)*
     &             Sub4658(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            2*AaD0i3(Epsi(dd23),Gen6,Sfe6)*
     &             Sub4677(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i4(Epsi(dd23),Gen6,Sfe6)*
     &             Sub4680(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            AaD0i5(Epsi(dd23),Gen6,Sfe6)*
     &             Sub4694(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i3(Epsi(dd233),Gen6,Sfe6)*
     &             Sub4697(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*(AaD0i11(Epsi(dd00),Gen6,Sfe6)*
     &             Sub4692(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i9(Epsi(dd123),Gen6,Sfe6)*
     &             Sub4695(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i11(Epsi(dd123),Gen6,Sfe6)*
     &             Sub4696(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i12(Epsi(dd33),Gen6,Sfe6)*
     &             Sub4699(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         2*(AaD0i13(Epsi(dd123),Gen6,Sfe6)*
     &             Sub4700(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i10(Epsi(dd123),Gen6,Sfe6)*
     &             Sub4701(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*AaD0i13(Epsi(dd00),Gen6,Sfe6)*
     &          Sub4703(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))

        tmp1 = tmp1 - Alfa/(2.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (AaD0i11(Epsi(dd0),Gen6,Sfe6)*
     &           Sub4762(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &          S2B*(1/2.D0*(AaD0i6(Epsi(dd22),Gen6,Sfe6)*
     &                Sub4716(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(-(AaD0i6(Epsi(dd3),Gen6,Sfe6)*
     &                   Sub4724(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &                AaD0i11(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4728(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(AaD0i13(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4735(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i11(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4744(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(-(AaD0i14(Epsi(dd33),Gen6,Sfe6)*
     &                   Sub4752(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &                AaD0i10(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4754(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &             1/2.D0*(AaD0i14(Epsi(dd22),Gen6,Sfe6)*
     &                Sub4765(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &             AaD0i6(Epsi(dd233),Gen6,Sfe6)*
     &              Sub4704(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i12(Epsi(dd13),Gen6,Sfe6)*
     &              Sub4706(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i14(Epsi(dd233),Gen6,Sfe6)*
     &              Sub4707(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i8(Epsi(dd13),Gen6,Sfe6)*
     &              Sub4711(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i12(Epsi(dd11),Gen6,Sfe6)*
     &              Sub4712(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &             AaD0i9(Epsi(dd13),Gen6,Sfe6)*
     &              Sub4718(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             AaD0i12(Epsi(dd001),Gen6,Sfe6)*
     &              Sub4733(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &             (F16 + F32)*
     &              (AaD0i8(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4709(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                Pair2*
     &                 (2*AaD0i3(Epsi(dd22),Gen6,Sfe6)*
     &                    Sub4714(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                   AaD0i3(Epsi(dd2),Gen6,Sfe6)*
     &                    Sub4730(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                   Sub13166(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                    Sub4736(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &                AaD0i12(Epsi(dd133),Gen6,Sfe6)*
     &                 Sub4763(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &             (F72 + F74)*
     &              (AaD0i8(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4709(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i9(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4726(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                AaD0i4(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4732(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                Pair2*
     &                 (2*AaD0i3(Epsi(dd22),Gen6,Sfe6)*
     &                    Sub4714(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                   AaD0i3(Epsi(dd2),Gen6,Sfe6)*
     &                    Sub4730(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &                   Sub13166(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &                    Sub4736(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &                AaD0i12(Epsi(dd133),Gen6,Sfe6)*
     &                 Sub4763(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))))/S2B

        tmp1 = tmp1 + Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (-(AaD0i14(Epsi(dd3),Gen6,Sfe6)*
     &            Sub4771(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         AaD0i6(Epsi(dd33),Gen6,Sfe6)*
     &          Sub4779(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         2*Sub13167(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &          Sub4780(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaD0i11(Epsi(dd2),Gen6,Sfe6)*
     &          Sub4786(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         Pair4*(2*(AaD0i11(Epsi(dd133),Gen6,Sfe6)*
     &                Sub4772(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               AaD0i10(Epsi(dd133),Gen6,Sfe6)*
     &                Sub4777(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            2*AaD0i9(Epsi(dd112),Gen6,Sfe6)*
     &             Sub4788(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         (F16 + F32)*
     &          (2*AaD0i3(Epsi(dd33),Gen6,Sfe6)*
     &             Sub4774(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i12(Epsi(dd113),Gen6,Sfe6)*
     &             Sub4817(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         AaD0i13(Epsi(dd33),Gen6,Sfe6)*
     &          Sub4840(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         2*Sub13168(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &          Sub4841(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         (F72 + F74)*
     &          (2*(AaD0i3(Epsi(dd33),Gen6,Sfe6)*
     &                Sub4774(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &               Pair4*AaD0i4(Epsi(dd2),Gen6,Sfe6)*
     &                Sub4776(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &            2*AaD0i12(Epsi(dd112),Gen6,Sfe6)*
     &             Sub4787(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            AaD0i5(Epsi(dd22),Gen6,Sfe6)*
     &             Sub4816(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*AaD0i12(Epsi(dd113),Gen6,Sfe6)*
     &             Sub4817(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            AaD0i10(Epsi(dd33),Gen6,Sfe6)*
     &             Sub4851(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) - 
     &         2*(Sub13169(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4849(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Sub13170(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub4877(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         Abb27*(F16 + F32)*
     &          (2*AaD0i14(Epsi(dd222),Gen6,Sfe6) - 
     &            2*AaD0i6(Epsi(dd222),Gen6,Sfe6))*
     &          Sub4878(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         AaC0i18(Epsi(cc0),Gen6,Sfe6)*
     &          Sub4881(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         2*Sub13171(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &          Sub4882(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaC0i19(Epsi(cc0),Gen6,Sfe6)*
     &          Sub4893(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         2*Sub13172(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &          Sub4894(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))

        tmp1 = tmp1 - Alfa/(4.D0*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &       (AaC0i18(Epsi(cc1),Gen6,Sfe6)*
     &          Sub4994(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaC0i19(Epsi(cc1),Gen6,Sfe6)*
     &          Sub5034(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         (-2*AaD0i11(Epsi(dd002),Gen6,Sfe6) + 
     &            2*AaD0i13(Epsi(dd002),Gen6,Sfe6))*
     &          Sub5064(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         (F72 + F74)*
     &          (2*AaD0i3(Epsi(dd223),Gen6,Sfe6)*
     &             Sub5033(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &            2*Sub13173(Cha(3),Cha(4),Gen(1),Gen(5),Gen6,Sfe6)*
     &             Sub5068(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         2*(AaD0i12(Epsi(dd12),Gen6,Sfe6)*
     &             Sub5002(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            (F16 + F32)*AaD0i3(Epsi(dd223),Gen6,Sfe6)*
     &             Sub5033(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Pair2*AaD0i8(Epsi(dd12),Gen6,Sfe6)*
     &             Sub5129(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &            Pair4*AaD0i9(Epsi(dd12),Gen6,Sfe6)*
     &             Sub5263(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)) + 
     &         AaC0i18(Epsi(cc2),Gen6,Sfe6)*
     &          Sub5309(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaC0i19(Epsi(cc2),Gen6,Sfe6)*
     &          Sub5313(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) - 
     &         AaD0i11(Epsi(dd12),Gen6,Sfe6)*
     &          Sub5376(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaD0i13(Epsi(dd12),Gen6,Sfe6)*
     &          Sub5399(HelAll(Cha(3)),Cha(4),Gen6,Sfe6) + 
     &         AaD0i10(Epsi(dd12),Gen6,Sfe6)*
     &          Sub5441(HelAll(Cha(3)),Cha(4),Gen6,Sfe6)))

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    tmp1/
     &     (S + T + T14 - MCha2(Cha(3)) - MCha2(Cha(4)) - 
     &       2*Mf2(3,Gen(1)) - Mf2(3,Gen(5)))

#if DEBUG >= 4
        DEB("qug_xIxJqu_box_Gen6_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

