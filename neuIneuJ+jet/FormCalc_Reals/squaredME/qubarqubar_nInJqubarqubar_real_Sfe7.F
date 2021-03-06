#if 0
* qubarqubar_nInJqubarqubar_real_Sfe7.F
* form factors for {-F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], -F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:20
#endif

#include "qubarqubar_nInJqubarqubar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qubarqubar_nInJqubarqubar_real_Sfe7
        implicit none

#include "qubarqubar_nInJqubarqubar_vars.h"
        HelType dup1, dup2, dup3, dup4, dup5, dup6, dup7, dup8

#include "real_inline.h"

        dup1 = ((F27 + F29 - F85 - F86)*
     &       Sub75(HelAll(Gen(1)),Gen(6),Neu(3),Neu(4),Sfe7))/
     &     (T14 - Sub352(Gen(1),Sfe7)) - 
     &    Sub78(HelAll(Gen(1)),Gen(6),Neu(3),Neu(4),Sfe7)/
     &     (Mf2(3,Gen(1)) - Sub377(Gen(1),Gen(2),Gen(5)))

        dup2 = -(Sub84(HelAll(Gen(2)),Gen(6),Neu(3),Neu(4),Sfe7)/
     &       (Mf2(3,Gen(2)) - Sub377(Gen(1),Gen(2),Gen(5)))) - 
     &    ((F103 + F104 - F63 - F65)*
     &       Sub85(HelAll(Gen(2)),Gen(6),Neu(3),Neu(4),Sfe7))/
     &     (T24 - Sub359(Gen(2),Sfe7))

        dup3 = Sub281(HelAll(Gen(2)),Gen(5),Neu(3),Neu(4),Sfe7)/
     &     ((T24 - Sub359(Gen(2),Sfe7))*
     &       (Mf2(3,Gen(5)) + Sub382(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))))
     &      - Sub243(HelAll(Gen(2)),Gen(5),Neu(3),Neu(4),Sfe7)/
     &     ((Mf2(3,Gen(2)) + Sub383(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &       (S45 - Sub429(Gen(5),Sfe7)))

        dup4 = -(Sub311(HelAll(Gen(5)),Neu(4),Sfe7)/
     &       (Mf2(3,Gen(5)) + Sub382(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))))
     &      - ((F77 - F78 + F79 - F80)*
     &       Sub64(HelAll(Gen(5)),Neu(4),Sfe7))/
     &     (S45 - Sub429(Gen(5),Sfe7))

        dup5 = -(Sub286(HelAll(Gen(6)),Neu(4),Sfe7)/
     &       (Mf2(3,Gen(6)) + Sub382(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))))
     &      + ((F63 - F64 + F65 - F66)*
     &       Sub71(HelAll(Gen(6)),Neu(4),Sfe7))/
     &     (Sub353(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &       Sub434(Gen(6),Sfe7))

        dup6 = -(Sub250(HelAll(Gen(1)),Gen(6),Neu(3),Neu(4),Sfe7)/
     &       ((T14 - Sub352(Gen(1),Sfe7))*
     &         (Mf2(3,Gen(6)) - Sub378(Gen(1),Neu(3),Neu(4))))) - 
     &    ((F27 - F28 + F29 - F30)*Sub61(HelAll(Gen(1)),Neu(3),Sfe7)*
     &       Sub71(HelAll(Gen(6)),Neu(4),Sfe7))/
     &     ((T - Sub352(Gen(1),Sfe7))*
     &       (Sub353(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &         Sub434(Gen(6),Sfe7)))

        dup7 = -(dup1/
     &       (Sub369(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &         Sub434(Gen(6),Sfe7))) - 
     &    (Sub71(HelAll(Gen(6)),Neu(4),Sfe7)*
     &       Sub81(HelAll(Gen(1)),Neu(3),Sfe7))/
     &     ((Mf2(3,Gen(1)) - Sub377(Gen(1),Gen(2),Gen(5)))*
     &       (Sub353(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &         Sub434(Gen(6),Sfe7)))

        dup8 = -(dup2/
     &       (Sub369(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &         Sub434(Gen(6),Sfe7))) - 
     &    (Sub71(HelAll(Gen(6)),Neu(4),Sfe7)*
     &       Sub88(HelAll(Gen(2)),Neu(3),Sfe7))/
     &     ((Mf2(3,Gen(2)) - Sub377(Gen(1),Gen(2),Gen(5)))*
     &       (Sub353(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &         Sub434(Gen(6),Sfe7)))

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*(IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &           (dup8/T15 - 
     &             Sub262(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe7)/
     &              ((T14 - Sub352(Gen(1),Sfe7))*
     &                (Mf2(3,Gen(5)) - Sub378(Gen(1),Neu(3),Neu(4)))*
     &                Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))) + 
     &             Sub269(HelAll(Gen(2)),Gen(6),Neu(3),Neu(4),Sfe7)/
     &              (T15*(T24 - Sub359(Gen(2),Sfe7))*
     &                (Mf2(3,Gen(6)) + 
     &                  Sub382(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))) + 
     &             Sub444(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe7)/
     &              (Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))*
     &                (Mf2(3,Gen(1)) + 
     &                  Sub383(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))))) - 
     &          (Sub5(HelAll(Gen(5)),Neu(3),Sfe7)*
     &             ((3*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &                  Sub291(HelAll(Gen(2)),Neu(4),Sfe7))/
     &                ((Mf2(3,Gen(2)) + 
     &                    Sub383(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &                  Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))) + 
     &               ((F45 + F47 - F97 - F98)*
     &                  IndexDelta(Gen(1),Gen(5))*
     &                  IndexDelta(Gen(2),Gen(6))*
     &                  Sub56(HelAll(Gen(1)),Neu(4),Sfe7))/
     &                ((T14 - Sub352(Gen(1),Sfe7))*
     &                  Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))))/
     &           (S35 - Sub429(Gen(5),Sfe7)) - 
     &          (((3*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &                  Sub296(HelAll(Gen(6)),Neu(4),Sfe7))/
     &                (T25*(Mf2(3,Gen(6)) - Sub378(Gen(1),Neu(3),Neu(4))))+
     &                 (IndexDelta(Gen(1),Gen(5))*
     &                  IndexDelta(Gen(2),Gen(6))*
     &                  Sub431(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe7))/
     &                Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &             Sub61(HelAll(Gen(1)),Neu(3),Sfe7))/
     &           (T - Sub352(Gen(1),Sfe7)) - 
     &          3*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &           ((dup6 - dup7)/T25 + 
     &             (dup3 - 
     &                ((F113 + F114 - F77 - F79)*
     &                   Sub5(HelAll(Gen(5)),Neu(3),Sfe7)*
     &                   Sub67(HelAll(Gen(2)),Neu(4),Sfe7))/
     &                 ((T24 - Sub359(Gen(2),Sfe7))*
     &                   (S35 - Sub429(Gen(5),Sfe7))))/
     &              Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))) - 
     &          ((3*dup4*T15*IndexDelta(Gen(1),Gen(6))*
     &                IndexDelta(Gen(2),Gen(5)) - 
     &               dup5*IndexDelta(Gen(1),Gen(5))*
     &                IndexDelta(Gen(2),Gen(6))*
     &                Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &             Sub74(HelAll(Gen(2)),Neu(3),Sfe7))/
     &           (T15*(U - Sub359(Gen(2),Sfe7))*
     &             Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))))/SB2

#if DEBUG >= 4
        DEB("qubarqubar_nInJqubarqubar_real_Sfe7:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*(3*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &           (dup8/T15 - 
     &             Sub262(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe7)/
     &              ((T14 - Sub352(Gen(1),Sfe7))*
     &                (Mf2(3,Gen(5)) - Sub378(Gen(1),Neu(3),Neu(4)))*
     &                Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))) + 
     &             Sub269(HelAll(Gen(2)),Gen(6),Neu(3),Neu(4),Sfe7)/
     &              (T15*(T24 - Sub359(Gen(2),Sfe7))*
     &                (Mf2(3,Gen(6)) + 
     &                  Sub382(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))) + 
     &             Sub444(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe7)/
     &              (Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))*
     &                (Mf2(3,Gen(1)) + 
     &                  Sub383(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))))) - 
     &          (Sub5(HelAll(Gen(5)),Neu(3),Sfe7)*
     &             ((IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &                  Sub291(HelAll(Gen(2)),Neu(4),Sfe7))/
     &                ((Mf2(3,Gen(2)) + 
     &                    Sub383(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &                  Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))) + 
     &               (3*(F45 + F47 - F97 - F98)*
     &                  IndexDelta(Gen(1),Gen(5))*
     &                  IndexDelta(Gen(2),Gen(6))*
     &                  Sub56(HelAll(Gen(1)),Neu(4),Sfe7))/
     &                ((T14 - Sub352(Gen(1),Sfe7))*
     &                  Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))))/
     &           (S35 - Sub429(Gen(5),Sfe7)) - 
     &          (((IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &                  Sub296(HelAll(Gen(6)),Neu(4),Sfe7))/
     &                (T25*(Mf2(3,Gen(6)) - Sub378(Gen(1),Neu(3),Neu(4))))+
     &                 (3*IndexDelta(Gen(1),Gen(5))*
     &                  IndexDelta(Gen(2),Gen(6))*
     &                  Sub431(HelAll(Gen(1)),Gen(5),Neu(3),Neu(4),Sfe7))/
     &                Sub379(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &             Sub61(HelAll(Gen(1)),Neu(3),Sfe7))/
     &           (T - Sub352(Gen(1),Sfe7)) - 
     &          IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &           ((dup6 - dup7)/T25 + 
     &             (dup3 - 
     &                ((F113 + F114 - F77 - F79)*
     &                   Sub5(HelAll(Gen(5)),Neu(3),Sfe7)*
     &                   Sub67(HelAll(Gen(2)),Neu(4),Sfe7))/
     &                 ((T24 - Sub359(Gen(2),Sfe7))*
     &                   (S35 - Sub429(Gen(5),Sfe7))))/
     &              Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))) - 
     &          ((dup4*T15*IndexDelta(Gen(1),Gen(6))*
     &                IndexDelta(Gen(2),Gen(5)) - 
     &               3*dup5*IndexDelta(Gen(1),Gen(5))*
     &                IndexDelta(Gen(2),Gen(6))*
     &                Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &             Sub74(HelAll(Gen(2)),Neu(3),Sfe7))/
     &           (T15*(U - Sub359(Gen(2),Sfe7))*
     &             Sub390(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))))/SB2

#if DEBUG >= 4
        DEB("qubarqubar_nInJqubarqubar_real_Sfe7:Ctree(1)", Ctree(HelAll(1)))
#endif

#include "real_contains.h"

        end

