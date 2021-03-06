#if 0
* quqdbar_nIxJququbar_real_Sfe7_Sfe8.F
* form factors for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:57
#endif

#include "quqdbar_nIxJququbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine quqdbar_nIxJququbar_real_Sfe7_Sfe8
        implicit none

#include "quqdbar_nIxJququbar_vars.h"
        HelType dup1, dup2, dup3, dup4

#include "real_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub158(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Sfe7,Sfe8)*
     &        Sub20(HelAll(Gen(6)),Neu(3),Sfe7))/
     &      ((MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub153(Cha(4),Gen(1),Gen(2)))*
     &        (Sub152(Gen(5),Sfe7) + 
     &          Sub157(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub158(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Sfe7,Sfe8)*
     &        Sub20(HelAll(Gen(6)),Neu(3),Sfe7))/
     &      ((MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub153(Cha(4),Gen(1),Gen(2)))*
     &        (Sub152(Gen(5),Sfe7) + 
     &          Sub157(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub156(HelAll(Cha(4)),Gen(1),Gen(2),Gen(6),Sfe7,Sfe8)*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sfe7))/
     &      ((S35 - Sub152(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub153(Cha(4),Gen(1),Gen(2))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub156(HelAll(Cha(4)),Gen(1),Gen(2),Gen(6),Sfe7,Sfe8)*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sfe7))/
     &      ((S35 - Sub152(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub153(Cha(4),Gen(1),Gen(2))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        dup1 = 1/(S35 - Sub145(Gen(1),Sfe7)) - 
     &    1/(Sub146(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &       Sub147(Gen(2),Sfe8))

        dup2 = S35 - Sub146(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &    Sub149(Gen(1),Sfe7) - Sub150(Gen(2),Sfe8)

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sfe7)*
     &        Sub7(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8)*
     &        Sub8(HelAll(Cha(4)),Gen(2),Gen(6),Sfe8))/
     &      (dup2*(MGl2 - cI*(MGl*WGl) - Sub144(Gen(1),Gen(5),Neu(3))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub4(HelAll(Gen(5)),Neu(3),Sfe7)*
     &        Sub7(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8)*
     &        Sub8(HelAll(Cha(4)),Gen(2),Gen(6),Sfe8))/
     &      (dup2*(MGl2 - cI*(MGl*WGl) - Sub144(Gen(1),Gen(5),Neu(3))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        dup3 = -((Sub61(HelAll(Gen(1)),Neu(3),Sfe7)*
     &        (Sqrt(2.D0)*(F7*SB2*Sub6(Cha(4),Gen(2),Sfe8)) + 
     &          F3*S2B*Mf(3,Gen(6))*USf(Sfe8,1,4,Gen(2))*VCha(Cha(4),2)))
     &       /(S2B*SB2))

        dup4 = -((dup3*Sub116(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8))/
     &       (Sub146(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &         Sub147(Gen(2),Sfe8))) - 
     &    (Sub127(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8)*
     &       Sub128(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe7,Sfe8))/
     &     (T24 - Sub200(Gen(2),Sfe8))

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        (dup4/(T - Sub145(Gen(1),Sfe7)) + 
     &          (Sub112(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8)*
     &             Sub113(HelAll(Cha(4)),Gen(2),Sfe8)*
     &             Sub4(HelAll(Gen(5)),Neu(3),Sfe7))/
     &           ((S35 - Sub145(Gen(1),Sfe7))*
     &             (T24 - Sub200(Gen(2),Sfe8)))))/
     &      (MGl2 - cI*(MGl*WGl) - Sub144(Gen(1),Gen(5),Neu(3)))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        (dup4/(T - Sub145(Gen(1),Sfe7)) + 
     &          (Sub112(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8)*
     &             Sub113(HelAll(Cha(4)),Gen(2),Sfe8)*
     &             Sub4(HelAll(Gen(5)),Neu(3),Sfe7))/
     &           ((S35 - Sub145(Gen(1),Sfe7))*
     &             (T24 - Sub200(Gen(2),Sfe8)))))/
     &      (MGl2 - cI*(MGl*WGl) - Sub144(Gen(1),Gen(5),Neu(3)))

#if DEBUG >= 4
        DEB("quqdbar_nIxJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "real_contains.h"

        end

