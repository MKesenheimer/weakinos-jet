#if 0
* qubarg_nInJqubar_box_Gen6_Sfe6.F
* form factors for {-F[3, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[3, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:04
#endif

#include "qubarg_nInJqubar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qubarg_nInJqubar_box_Gen6_Sfe6
        implicit none

#include "qubarg_nInJqubar_vars.h"
        HelType tmp1

#include "virt_inline.h"

        tmp1 = -(Alfa/(18.D0*CW2*MW2*SW2)*
     &      (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &         ((2*AaD0i7(Epsi(dd002),Gen6,Sfe6) - 
     &              2*AaD0i8(Epsi(dd002),Gen6,Sfe6))*
     &            Sub2657(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &           2*(Sub18102(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2639(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              Sub18105(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2654(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              Pair4*AaD0i10(Epsi(dd112),Gen6,Sfe6)*
     &               Sub2671(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              Sub18107(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2679(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &           AaC0i11(Epsi(cc1),Gen6,Sfe6)*
     &            Sub2682(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &           2*(Sub18103(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2642(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              Sub18104(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2648(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              AaD0i9(Epsi(dd11),Gen6,Sfe6)*
     &               Sub2666(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              Pair3*Sub18108(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2685(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &           (F27 + F32)*AaC0i12(Epsi(cc2),Gen6,Sfe6)*
     &            MNeu(Neu(4))*Sub2687(HelAll(Gen6),Neu(3),Neu(4),Sfe6)-
     &             (F72 + F84)*
     &            (2*AaD0i9(Epsi(dd112),Gen6,Sfe6)*
     &               Sub2660(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &              Sub18106(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2667(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              2*Pair4*MNeu(Neu(4))*
     &               Sub18111(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2687(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &           (F31 + F33)*
     &            (-(Sub18106(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2667(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &              Abb54*(2*AaD0i7(Epsi(dd112),Gen6,Sfe6) - 
     &                 2*AaD0i8(Epsi(dd112),Gen6,Sfe6))*
     &               Sub2686(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &              2*Pair3*
     &               Sub18110(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &               Sub2686(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &              2*Pair4*
     &               (Sub18109(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                  Sub2686(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                 MNeu(Neu(4))*
     &                  Sub18111(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                  Sub2687(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))))/
     &       CB2)

        tmp1 = tmp1 - Alfa/(18.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (AaC0i13(Epsi(cc1),Gen6,Sfe6)*
     &           Sub2695(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &          (F27 + F32)*AaC0i12(Epsi(cc1),Gen6,Sfe6)*
     &           MNeu(Neu(3))*Sub2702(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &          (F72 + F84)*
     &           (2*(Pair3*MNeu(Neu(3))*
     &                 Sub18114(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2702(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i9(Epsi(dd122),Gen6,Sfe6)*
     &                 Sub2708(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             2*AaD0i9(Epsi(dd113),Gen6,Sfe6)*
     &              Sub2711(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &          (F31 + F33)*
     &           (Abb54*(2*AaD0i7(Epsi(dd122),Gen6,Sfe6) - 
     &                2*AaD0i8(Epsi(dd122),Gen6,Sfe6))*
     &              Sub2690(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*AaD0i9(Epsi(dd113),Gen6,Sfe6)*
     &              Sub2711(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &          AaC0i11(Epsi(cc2),Gen6,Sfe6)*
     &           Sub2722(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &          AaC0i13(Epsi(cc2),Gen6,Sfe6)*
     &           Sub2735(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &          2*(AaD0i7(Epsi(dd001),Gen6,Sfe6)*
     &              Sub2701(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i9(Epsi(dd123),Gen6,Sfe6)*
     &              Sub2716(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i10(Epsi(dd123),Gen6,Sfe6)*
     &              Sub2738(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &          2*(Sub18112(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &              Sub2698(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             Sub18113(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &              Sub2701(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i9(Epsi(dd001),Gen6,Sfe6)*
     &              Sub2719(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i14(Epsi(dd123),Gen6,Sfe6)*
     &              Sub2729(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             Pair3*(Sub18115(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2705(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Sub18116(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2732(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             Pair4*AaD0i10(Epsi(dd122),Gen6,Sfe6)*
     &              Sub2741(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))/CB2

        tmp1 = tmp1 + (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (SB2*(-2*AaD0i7(Epsi(dd123),Gen6,Sfe6)*
     &              Sub2747(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             Abb54*(F31 + F33)*
     &              (2*AaD0i7(Epsi(dd222),Gen6,Sfe6) - 
     &                2*AaD0i8(Epsi(dd222),Gen6,Sfe6))*
     &              Sub2785(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(AaD0i8(Epsi(dd123),Gen6,Sfe6)*
     &                 Sub2753(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i20(Epsi(dd123),Gen6,Sfe6)*
     &                 Sub2784(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F31 + F33)*
     &                 (AaD0i9(Epsi(dd133),Gen6,Sfe6)*
     &                    Sub2790(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i14(Epsi(dd223),Gen6,Sfe6)*
     &                    Sub2795(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) - 
     &             (F72 + F84)*
     &              (2*(AaD0i9(Epsi(dd133),Gen6,Sfe6)*
     &                    Sub2790(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i14(Epsi(dd223),Gen6,Sfe6)*
     &                    Sub2795(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &                2*Sub18119(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2800(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &          CB2*((F27 + F32)*AaC0i16(Epsi(cc0),Gen6,Sfe6)*
     &              Mf(3,Gen6)*Sub2773(HelAll(Gen6),Neu(3),Neu(4),Sfe6)
     &               - 2*(AaD0i15(Epsi(dd233),Gen6,Sfe6)*
     &                 Sub2770(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i19(Epsi(dd233),Gen6,Sfe6)*
     &                 Sub2780(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             AaC0i21(Epsi(cc2),Gen6,Sfe6)*
     &              Sub2786(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*Pair3*Sub18117(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &              Sub2787(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(F31 + F33)*
     &              (Pair3*AaD0i17(Epsi(dd0),Gen6,Sfe6)*Mf(3,Gen6)*
     &                 Sub2773(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i22(Epsi(dd133),Gen6,Sfe6)*
     &                 Sub2799(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             (F72 + F84)*
     &              ((2*Pair3*AaD0i17(Epsi(dd0),Gen6,Sfe6) - 
     &                   2*Pair4*AaD0i18(Epsi(dd0),Gen6,Sfe6))*
     &                 Mf(3,Gen6)*
     &                 Sub2773(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                2*Sub18118(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2798(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                2*AaD0i22(Epsi(dd133),Gen6,Sfe6)*
     &                 Sub2799(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             AaC0i23(Epsi(cc2),Gen6,Sfe6)*
     &              Sub2801(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))/S2B**2

        tmp1 = tmp1 + (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (SB2*(-2*(AaD0i8(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub2821(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair4*
     &                 (AaD0i7(Epsi(dd133),Gen6,Sfe6)*
     &                    Sub2822(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i20(Epsi(dd133),Gen6,Sfe6)*
     &                    Sub2832(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &             2*(AaD0i7(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub2810(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F31 + F33)*Pair3*
     &                 Sub18122(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2837(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             (F72 + F84)*
     &              (2*Sub18121(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2827(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                2*(Pair3*
     &                    Sub18122(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                    Sub2837(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   Pair4*
     &                    Sub18123(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                    Sub2842(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))+
     &            CB2*((F27 + F32)*AaC0i16(Epsi(cc2),Gen6,Sfe6)*
     &              MNeu(Neu(4))*Sub2814(HelAll(Gen6),Neu(3),Neu(4),Sfe6)
     &               + (F31 + F33)*
     &              (-2*Pair4*MNeu(Neu(4))*
     &                 Sub18120(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2814(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Abb54*
     &                 (2*AaD0i15(Epsi(dd222),Gen6,Sfe6) - 
     &                   2*AaD0i19(Epsi(dd222),Gen6,Sfe6))*
     &                 Sub2815(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                2*AaD0i17(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub2831(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             2*AaD0i15(Epsi(dd223),Gen6,Sfe6)*
     &              Sub2835(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F72 + F84)*
     &              (-2*AaD0i17(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub2831(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                2*(Pair4*MNeu(Neu(4))*
     &                    Sub18120(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                    Sub2814(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i22(Epsi(dd112),Gen6,Sfe6)*
     &                    Sub2841(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) - 
     &             2*(Pair4*
     &                 (AaD0i15(Epsi(dd133),Gen6,Sfe6)*
     &                    Sub2804(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i24(Epsi(dd133),Gen6,Sfe6)*
     &                    Sub2813(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &                AaD0i22(Epsi(dd11),Gen6,Sfe6)*
     &                 Sub2836(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i19(Epsi(dd223),Gen6,Sfe6)*
     &                 Sub2840(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair3*
     &                 Sub18124(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2843(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))))/
     &      S2B**2

        tmp1 = tmp1 - (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (CB2*(AaC0i21(Epsi(cc1),Gen6,Sfe6)*
     &              Sub2852(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F31 + F33)*
     &              (-(Sub18125(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                   Sub2844(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &                (Abb54*
     &                    (2*AaD0i15(Epsi(dd112),Gen6,Sfe6) - 
     &                      2*AaD0i19(Epsi(dd112),Gen6,Sfe6)) - 
     &                   2*Pair3*
     &                    Sub18127(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6))*
     &                 Sub2850(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                2*(AaD0i17(Epsi(dd233),Gen6,Sfe6)*
     &                    Sub2849(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   Pair4*
     &                    Sub18126(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                    Sub2850(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i22(Epsi(dd113),Gen6,Sfe6)*
     &                    Sub2854(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) - 
     &             (F72 + F84)*
     &              (-(Sub18125(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                   Sub2844(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &                2*(AaD0i17(Epsi(dd233),Gen6,Sfe6)*
     &                    Sub2849(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   Sub18129(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                    Sub2853(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i22(Epsi(dd113),Gen6,Sfe6)*
     &                    Sub2854(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &             2*(Sub18130(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2855(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i15(Epsi(dd001),Gen6,Sfe6)*
     &                 Sub2864(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             2*(Pair4*AaD0i18(Epsi(dd112),Gen6,Sfe6)*
     &                 Sub2845(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i22(Epsi(dd001),Gen6,Sfe6)*
     &                 Sub2846(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Sub18128(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2851(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Sub18131(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2859(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Sub18132(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2864(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &          SB2*((2*(F31 + F33) - 2*(F72 + F84))*
     &              AaD0i14(Epsi(dd233),Gen6,Sfe6)*
     &              Sub2863(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F27 + F32)*AaC0i12(Epsi(cc0),Gen6,Sfe6)*
     &              Mf(4,Gen6)*Sub2868(HelAll(Gen6),Neu(3),Neu(4),Sfe6)
     &             )))/S2B**2

        tmp1 = tmp1 - (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (SB2*((2*(F31 + F33)*Pair3*
     &                 AaD0i14(Epsi(dd0),Gen6,Sfe6) + 
     &                (F72 + F84)*
     &                 (2*Pair4*AaD0i10(Epsi(dd0),Gen6,Sfe6) - 
     &                   2*Pair3*AaD0i14(Epsi(dd0),Gen6,Sfe6)))*
     &              Mf(4,Gen6)*Sub2868(HelAll(Gen6),Neu(3),Neu(4),Sfe6)
     &               + 2*(AaD0i7(Epsi(dd233),Gen6,Sfe6)*
     &                 Sub2880(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i8(Epsi(dd233),Gen6,Sfe6)*
     &                 Sub2886(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &          CB2*(-((F27 + F32)*AaC0i16(Epsi(cc1),Gen6,Sfe6)*
     &                MNeu(Neu(3))*
     &                Sub2879(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaC0i23(Epsi(cc1),Gen6,Sfe6)*
     &              Sub2881(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(Sub18136(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2882(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F72 + F84)*
     &                 (Pair3*
     &                    (Sub18134(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                       Sub2874(HelAll(Gen6),Neu(3),Neu(4),Sfe6)+
     &                        MNeu(Neu(3))*
     &                       Sub18135(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),
     &                       Sfe6)*
     &                       Sub2879(HelAll(Gen6),Neu(3),Neu(4),Sfe6))+
     &                     AaD0i22(Epsi(dd122),Gen6,Sfe6)*
     &                    Sub2888(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   Pair4*
     &                    Sub18137(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                    Sub2889(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) - 
     &             2*(Sub18133(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2872(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F31 + F33)*Pair3*
     &                 Sub18134(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2874(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair4*AaD0i18(Epsi(dd122),Gen6,Sfe6)*
     &                 Sub2890(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Sub18138(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                 Sub2891(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             (2*AaD0i15(Epsi(dd002),Gen6,Sfe6) - 
     &                2*AaD0i19(Epsi(dd002),Gen6,Sfe6))*
     &              Sub2897(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             Abb54*(F31 + F33)*
     &              (2*AaD0i15(Epsi(dd122),Gen6,Sfe6) - 
     &                2*AaD0i19(Epsi(dd122),Gen6,Sfe6))*
     &              Sub2901(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))/S2B**2

        tmp1 = tmp1 - (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (-(CB2*(2*(Pair3*
     &                   Sub18139(Gen(1),Gen(5),Gen6,Neu(3),Neu(4),Sfe6)*
     &                   Sub2906(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i17(Epsi(dd123),Gen6,Sfe6)*
     &                   Sub2921(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i19(Epsi(dd123),Gen6,Sfe6)*
     &                   Sub2927(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i24(Epsi(dd123),Gen6,Sfe6)*
     &                   Sub2928(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &               AaD0i19(Epsi(dd3),Gen6,Sfe6)*
     &                Sub4330(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &               2*(AaD0i22(Epsi(dd123),Gen6,Sfe6)*
     &                   Sub2910(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i18(Epsi(dd123),Gen6,Sfe6)*
     &                   Sub2922(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i15(Epsi(dd123),Gen6,Sfe6)*
     &                   Sub2924(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i22(Epsi(dd00),Gen6,Sfe6)*
     &                   Sub4350(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))) + 
     &          SB2*((F31 + F33)*
     &              (AaD0i20(Epsi(dd1),Gen6,Sfe6) - 
     &                AaD0i7(Epsi(dd1),Gen6,Sfe6) + 
     &                AaD0i8(Epsi(dd1),Gen6,Sfe6))*Mf(4,Gen6)*
     &              Sub4294(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             2*(AaD0i9(Epsi(dd1),Gen6,Sfe6)*
     &                 Sub4242(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair3*
     &                 (AaD0i14(Epsi(dd1),Gen6,Sfe6)*
     &                    Sub4281(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i14(Epsi(dd11),Gen6,Sfe6)*
     &                    Sub4361(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &             (F72 + F84)*
     &              (AaD0i20(Epsi(dd0),Gen6,Sfe6)*Mf(4,Gen6)*
     &                 Sub4294(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                2*Pair4*AaD0i10(Epsi(dd2),Gen6,Sfe6)*
     &                 Sub4369(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             Pair4*(2*AaD0i10(Epsi(dd1),Gen6,Sfe6)*
     &                 Sub4290(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                2*AaD0i10(Epsi(dd11),Gen6,Sfe6)*
     &                 Sub4382(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))))/
     &      S2B**2

        tmp1 = tmp1 - (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (SB2*(-(AaC0i11(Epsi(cc0),Gen6,Sfe6)*
     &                Sub4398(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaC0i13(Epsi(cc0),Gen6,Sfe6)*
     &              Sub4432(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F72 + F84)*
     &              (2*Pair4*AaD0i10(Epsi(dd22),Gen6,Sfe6)*
     &                 Sub4383(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                2*AaD0i10(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4454(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaD0i7(Epsi(dd2),Gen6,Sfe6)*
     &              Sub4491(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*AaD0i9(Epsi(dd0),Gen6,Sfe6)*
     &              Sub4518(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             Mf(4,Gen6)*
     &              (AaD0i7(Epsi(dd0),Gen6,Sfe6)*
     &                 Sub4536(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                AaD0i8(Epsi(dd0),Gen6,Sfe6)*
     &                 Sub4540(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &          CB2*(-2*AaD0i22(Epsi(dd33),Gen6,Sfe6)*
     &              Sub4445(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i15(Epsi(dd12),Gen6,Sfe6)*
     &              Sub4468(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i19(Epsi(dd12),Gen6,Sfe6)*
     &              Sub4503(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             (F72 + F84)*
     &              (AaD0i24(Epsi(dd3),Gen6,Sfe6)*
     &                 Sub4403(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                2*AaD0i17(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4450(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                2*AaD0i18(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4506(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaD0i15(Epsi(dd22),Gen6,Sfe6)*
     &              Sub4530(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(AaD0i17(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4427(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i18(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4436(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F31 + F33)*AaD0i17(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4450(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i15(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4458(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i19(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4512(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i24(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4548(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))))/
     &      S2B**2

        tmp1 = tmp1 + (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (-(CB2*(AaD0i19(Epsi(dd22),Gen6,Sfe6)*
     &                Sub4562(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &               AaD0i15(Epsi(dd33),Gen6,Sfe6)*
     &                Sub4586(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &               AaD0i24(Epsi(dd12),Gen6,Sfe6)*
     &                Sub4599(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &               AaD0i19(Epsi(dd13),Gen6,Sfe6)*
     &                Sub4637(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &               AaD0i19(Epsi(dd33),Gen6,Sfe6)*
     &                Sub4645(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &               (F72 + F84)*
     &                (AaD0i24(Epsi(dd33),Gen6,Sfe6)*
     &                   Sub4704(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                  2*Pair3*
     &                   (AaD0i17(Epsi(dd2),Gen6,Sfe6)*
     &                      Sub4709(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                     AaD0i17(Epsi(dd22),Gen6,Sfe6)*
     &                      Sub4724(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))-
     &                 2*(AaD0i22(Epsi(dd13),Gen6,Sfe6)*
     &                   Sub4675(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i17(Epsi(dd13),Gen6,Sfe6)*
     &                   Sub4692(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i18(Epsi(dd13),Gen6,Sfe6)*
     &                   Sub4723(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  (F31 + F33)*Pair3*
     &                   (AaD0i17(Epsi(dd2),Gen6,Sfe6)*
     &                      Sub4709(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                     AaD0i17(Epsi(dd22),Gen6,Sfe6)*
     &                      Sub4724(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))
     &             ) + SB2*
     &           (AaD0i7(Epsi(dd23),Gen6,Sfe6)*
     &              Sub4578(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i8(Epsi(dd2),Gen6,Sfe6)*
     &              Sub4619(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             2*Pair3*AaD0i14(Epsi(dd12),Gen6,Sfe6)*
     &              Sub4650(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F72 + F84)*AaD0i20(Epsi(dd2),Gen6,Sfe6)*
     &              Sub4686(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(AaD0i9(Epsi(dd12),Gen6,Sfe6)*
     &                 Sub4631(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i9(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4681(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair4*AaD0i10(Epsi(dd12),Gen6,Sfe6)*
     &                 Sub4708(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i14(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4719(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaD0i8(Epsi(dd23),Gen6,Sfe6)*
     &              Sub4745(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))/S2B**2

        tmp1 = tmp1 + (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (SB2*(AaD0i7(Epsi(dd12),Gen6,Sfe6)*
     &              Sub4794(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i7(Epsi(dd22),Gen6,Sfe6)*
     &              Sub4812(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i8(Epsi(dd12),Gen6,Sfe6)*
     &              Sub4833(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             2*AaD0i9(Epsi(dd33),Gen6,Sfe6)*
     &              Sub4835(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(AaD0i10(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4756(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i9(Epsi(dd22),Gen6,Sfe6)*
     &                 Sub4764(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i7(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4814(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i8(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4840(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaD0i8(Epsi(dd22),Gen6,Sfe6)*
     &              Sub4849(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F72 + F84)*
     &              (AaD0i20(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4807(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &                AaD0i20(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub4851(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i20(Epsi(dd22),Gen6,Sfe6)*
     &                 Sub4859(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &          CB2*(-(AaD0i15(Epsi(dd13),Gen6,Sfe6)*
     &                Sub4772(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             AaD0i24(Epsi(dd13),Gen6,Sfe6)*
     &              Sub4792(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(F72 + F84)*Pair4*
     &              (AaD0i18(Epsi(dd2),Gen6,Sfe6)*
     &                 Sub4786(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i18(Epsi(dd22),Gen6,Sfe6)*
     &                 Sub4808(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             2*AaD0i22(Epsi(dd2),Gen6,Sfe6)*
     &              Sub4828(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaC0i21(Epsi(cc0),Gen6,Sfe6)*
     &              Sub4831(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             AaC0i23(Epsi(cc0),Gen6,Sfe6)*
     &              Sub4838(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             AaD0i15(Epsi(dd2),Gen6,Sfe6)*
     &              Sub4867(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))/S2B**2

        tmp1 = tmp1 + (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (CB2*(AaD0i19(Epsi(dd2),Gen6,Sfe6)*
     &              Sub4892(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             AaD0i19(Epsi(dd23),Gen6,Sfe6)*
     &              Sub4907(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             2*Pair4*AaD0i18(Epsi(dd1),Gen6,Sfe6)*
     &              Sub4908(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             (F31 + F33)*
     &              (AaD0i15(Epsi(dd1),Gen6,Sfe6) - 
     &                AaD0i19(Epsi(dd1),Gen6,Sfe6) - 
     &                AaD0i24(Epsi(dd1),Gen6,Sfe6))*Mf(3,Gen6)*
     &              Sub4910(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (F72 + F84)*
     &              (AaD0i24(Epsi(dd2),Gen6,Sfe6)*
     &                 Sub4905(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i24(Epsi(dd0),Gen6,Sfe6)*Mf(3,Gen6)*
     &                 Sub4910(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &             2*(AaD0i22(Epsi(dd1),Gen6,Sfe6)*
     &                 Sub4876(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair3*
     &                 (AaD0i17(Epsi(dd1),Gen6,Sfe6)*
     &                    Sub4877(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i17(Epsi(dd12),Gen6,Sfe6)*
     &                    Sub4927(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &                AaD0i22(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4942(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) + 
     &          SB2*(AaD0i20(Epsi(dd12),Gen6,Sfe6)*
     &              Sub4869(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i8(Epsi(dd33),Gen6,Sfe6)*
     &              Sub4912(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(AaD0i20(Epsi(dd00),Gen6,Sfe6)*
     &                 Sub4879(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i9(Epsi(dd3),Gen6,Sfe6)*
     &                 Sub4925(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F31 + F33)*AaD0i14(Epsi(dd3),Gen6,Sfe6)*
     &                 Sub4943(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             2*(AaD0i9(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4914(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F72 + F84)*
     &                 (AaD0i10(Epsi(dd33),Gen6,Sfe6)*
     &                    Sub4909(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i14(Epsi(dd3),Gen6,Sfe6)*
     &                    Sub4943(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))))
     &       /S2B**2

        tmp1 = tmp1 - (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (-(CB2*(2*(F31 + F33)*AaD0i17(Epsi(dd23),Gen6,Sfe6)*
     &                Sub4975(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &               (F72 + F84)*
     &                (AaD0i24(Epsi(dd23),Gen6,Sfe6)*
     &                   Sub4945(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  2*(AaD0i17(Epsi(dd23),Gen6,Sfe6)*
     &                      Sub4975(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                     AaD0i18(Epsi(dd23),Gen6,Sfe6)*
     &                      Sub4999(HelAll(Gen6),Neu(3),Neu(4),Sfe6))+
     &                    AaD0i24(Epsi(dd22),Gen6,Sfe6)*
     &                   Sub5034(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &               AaD0i15(Epsi(dd23),Gen6,Sfe6)*
     &                Sub5082(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &               2*(Pair3*AaD0i17(Epsi(dd11),Gen6,Sfe6)*
     &                   Sub5010(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  AaD0i22(Epsi(dd22),Gen6,Sfe6)*
     &                   Sub5027(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                  Pair4*
     &                   (AaD0i18(Epsi(dd12),Gen6,Sfe6)*
     &                      Sub4970(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                     AaD0i18(Epsi(dd11),Gen6,Sfe6)*
     &                      Sub5124(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))
     &             ) + SB2*
     &           (AaD0i7(Epsi(dd3),Gen6,Sfe6)*
     &              Sub4964(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             AaD0i7(Epsi(dd13),Gen6,Sfe6)*
     &              Sub4968(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(AaD0i14(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4950(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i10(Epsi(dd13),Gen6,Sfe6)*
     &                 Sub4966(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i9(Epsi(dd23),Gen6,Sfe6)*
     &                 Sub4977(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             AaD0i8(Epsi(dd13),Gen6,Sfe6)*
     &              Sub5001(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             AaD0i8(Epsi(dd3),Gen6,Sfe6)*
     &              Sub5019(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i20(Epsi(dd13),Gen6,Sfe6)*
     &              Sub5032(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*(F31 + F33)*AaD0i14(Epsi(dd23),Gen6,Sfe6)*
     &              Sub5080(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             (F72 + F84)*
     &              (2*(AaD0i10(Epsi(dd3),Gen6,Sfe6)*
     &                    Sub4946(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i14(Epsi(dd23),Gen6,Sfe6)*
     &                    Sub5080(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &                AaD0i20(Epsi(dd3),Gen6,Sfe6)*
     &                 Sub5129(HelAll(Gen6),Neu(3),Neu(4),Sfe6)))))/
     &      S2B**2

        tmp1 = tmp1 + (2*Alfa)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*GS*IndexDelta(Gen(1),Gen(5))*
     &        (CB2*(2*(AaD0i22(Epsi(dd3),Gen6,Sfe6)*
     &                 Sub5144(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F31 + F33)*AaD0i17(Epsi(dd3),Gen6,Sfe6)*
     &                 Sub5686(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                AaD0i22(Epsi(dd0),Gen6,Sfe6)*
     &                 Sub5717(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) - 
     &             2*(AaD0i22(Epsi(dd12),Gen6,Sfe6)*
     &                 Sub5166(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                (F72 + F84)*
     &                 (AaD0i17(Epsi(dd3),Gen6,Sfe6)*
     &                    Sub5686(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i18(Epsi(dd3),Gen6,Sfe6)*
     &                    Sub5842(HelAll(Gen6),Neu(3),Neu(4),Sfe6))) - 
     &             AaD0i15(Epsi(dd0),Gen6,Sfe6)*Mf(3,Gen6)*
     &              Sub5941(HelAll(Gen6),Neu(3),Neu(4),Sfe6) - 
     &             AaD0i15(Epsi(dd3),Gen6,Sfe6)*
     &              Sub6022(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             AaD0i19(Epsi(dd0),Gen6,Sfe6)*Mf(3,Gen6)*
     &              Sub6186(HelAll(Gen6),Neu(3),Neu(4),Sfe6)) + 
     &          SB2*(AaD0i7(Epsi(dd33),Gen6,Sfe6)*
     &              Sub5250(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             2*AaD0i9(Epsi(dd2),Gen6,Sfe6)*
     &              Sub6245(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &             (2*(F31 + F33) - 2*(F72 + F84))*
     &              (AaD0i14(Epsi(dd33),Gen6,Sfe6)*
     &                 Sub5164(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                Pair3*
     &                 (AaD0i14(Epsi(dd2),Gen6,Sfe6)*
     &                    Sub6307(HelAll(Gen6),Neu(3),Neu(4),Sfe6) + 
     &                   AaD0i14(Epsi(dd22),Gen6,Sfe6)*
     &                    Sub6311(HelAll(Gen6),Neu(3),Neu(4),Sfe6))))))
     &       /S2B**2

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) - 
     &    tmp1/
     &     (S + T + T14 - 2*Mf2(3,Gen(1)) - Mf2(3,Gen(5)) - 
     &       MNeu2(Neu(3)) - MNeu2(Neu(4)))

#if DEBUG >= 4
        DEB("qubarg_nInJqubar_box_Gen6_Sfe6:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

