#if 0
* quqdbar_nInJquqdbar_real_Sfe7_Sfe8.F
* form factors for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:18
#endif

#include "quqdbar_nInJquqdbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine quqdbar_nInJquqdbar_real_Sfe7_Sfe8
        implicit none

#include "quqdbar_nInJquqdbar_vars.h"
        HelType dup1, dup2, dup3, dup4, dup5, dup6

#include "real_inline.h"

        dup1 = 1/(S45 - Sub210(Gen(1),Sfe8)) - 
     &    1/(Sub211(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &       Sub212(Gen(2),Sfe7))

        dup2 = S45 - Sub211(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) - 
     &    Sub214(Gen(1),Sfe8) - Sub215(Gen(2),Sfe7)

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub14(HelAll(Gen(6)),Neu(3),Sfe7)*
     &        Sub18(HelAll(Gen(5)),Neu(4),Sfe8)*
     &        Sub20(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8))/
     &      (CB*dup2*SB*(MGl2 - cI*(MGl*WGl) - 
     &          Sub209(Gen(1),Gen(5),Neu(4))))

#if DEBUG >= 4
        DEB("quqdbar_nInJquqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub14(HelAll(Gen(6)),Neu(3),Sfe7)*
     &        Sub18(HelAll(Gen(5)),Neu(4),Sfe8)*
     &        Sub20(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8))/
     &      (CB*dup2*SB*(MGl2 - cI*(MGl*WGl) - 
     &          Sub209(Gen(1),Gen(5),Neu(4))))

#if DEBUG >= 4
        DEB("quqdbar_nInJquqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        dup3 = 1/(S35 - Sub202(Gen(1),Sfe7)) - 
     &    1/(Sub203(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &       Sub204(Gen(2),Sfe8))

        dup4 = S35 - Sub203(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) - 
     &    Sub206(Gen(1),Sfe7) - Sub207(Gen(2),Sfe8)

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*dup3*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub10(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8)*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sfe7)*
     &        Sub8(HelAll(Gen(6)),Neu(4),Sfe8))/
     &      (CB*dup4*SB*(MGl2 - cI*(MGl*WGl) - 
     &          Sub201(Gen(1),Gen(5),Neu(3))))

#if DEBUG >= 4
        DEB("quqdbar_nInJquqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*dup3*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub10(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8)*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sfe7)*
     &        Sub8(HelAll(Gen(6)),Neu(4),Sfe8))/
     &      (CB*dup4*SB*(MGl2 - cI*(MGl*WGl) - 
     &          Sub201(Gen(1),Gen(5),Neu(3))))

#if DEBUG >= 4
        DEB("quqdbar_nInJquqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        dup5 = (Sub150(HelAll(Gen(5)),Neu(4),Sfe7)*
     &       Sub187(HelAll(Gen(2)),Neu(3),Sfe8)*
     &       Sub189(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8))/
     &     ((S45 - Sub202(Gen(1),Sfe7))*(U - Sub204(Gen(2),Sfe8))) - 
     &    (Sub158(HelAll(Gen(1)),Neu(4),Sfe7)*
     &       Sub192(HelAll(Gen(6)),Neu(3),Sfe8)*
     &       Sub194(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8))/
     &     ((T14 - Sub202(Gen(1),Sfe7))*
     &       (Sub204(Gen(2),Sfe8) + 
     &         Sub211(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4))))

        dup6 = (Sub182(HelAll(Gen(2)),Neu(4),Sfe8)*
     &       Sub248(HelAll(Gen(1)),Gen(5),Gen(6),Neu(3),Sfe7,Sfe8))/
     &     (T24 - Sub204(Gen(2),Sfe8)) - 
     &    (Sub200(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8)*
     &       Sub45(HelAll(Gen(1)),Neu(3),Sfe7)*
     &       Sub8(HelAll(Gen(6)),Neu(4),Sfe8))/
     &     ((T - Sub202(Gen(1),Sfe7))*
     &       (Sub203(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) + 
     &         Sub204(Gen(2),Sfe8)))

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        (dup6/
     &           (MGl2 - cI*(MGl*WGl) - Sub201(Gen(1),Gen(5),Neu(3))) + 
     &          (-dup5 + (Sub158(HelAll(Gen(1)),Neu(4),Sfe7)*
     &                Sub187(HelAll(Gen(2)),Neu(3),Sfe8)*
     &                Sub198(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8))/
     &              ((T14 - Sub202(Gen(1),Sfe7))*
     &                (U - Sub204(Gen(2),Sfe8))))/
     &           (MGl2 - cI*(MGl*WGl) - Sub209(Gen(1),Gen(5),Neu(4)))))/
     &      (CB*SB)

#if DEBUG >= 4
        DEB("quqdbar_nInJquqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        (dup6/
     &           (MGl2 - cI*(MGl*WGl) - Sub201(Gen(1),Gen(5),Neu(3))) + 
     &          (-dup5 + (Sub158(HelAll(Gen(1)),Neu(4),Sfe7)*
     &                Sub187(HelAll(Gen(2)),Neu(3),Sfe8)*
     &                Sub198(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8))/
     &              ((T14 - Sub202(Gen(1),Sfe7))*
     &                (U - Sub204(Gen(2),Sfe8))))/
     &           (MGl2 - cI*(MGl*WGl) - Sub209(Gen(1),Gen(5),Neu(4)))))/
     &      (CB*SB)

#if DEBUG >= 4
        DEB("quqdbar_nInJquqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "real_contains.h"

        end

