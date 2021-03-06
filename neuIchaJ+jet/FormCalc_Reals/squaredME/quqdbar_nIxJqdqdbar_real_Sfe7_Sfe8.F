#if 0
* quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8.F
* form factors for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:57
#endif

#include "quqdbar_nIxJqdqdbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8
        implicit none

#include "quqdbar_nIxJqdqdbar_vars.h"
        HelType dup1, dup2, dup3

#include "real_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub13(HelAll(Gen(5)),Neu(3),Sfe7)*
     &        Sub153(HelAll(Cha(4)),Gen(1),Gen(2),Gen(6),Sfe7,Sfe8))/
     &      ((S35 - Sub150(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub151(Cha(4),Gen(1),Gen(2))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub13(HelAll(Gen(5)),Neu(3),Sfe7)*
     &        Sub153(HelAll(Cha(4)),Gen(1),Gen(2),Gen(6),Sfe7,Sfe8))/
     &      ((S35 - Sub150(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub151(Cha(4),Gen(1),Gen(2))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub154(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Sfe7,Sfe8)*
     &        Sub4(HelAll(Gen(6)),Neu(3),Sfe7))/
     &      ((Sub144(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &          Sub150(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub151(Cha(4),Gen(1),Gen(2))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub154(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Sfe7,Sfe8)*
     &        Sub4(HelAll(Gen(6)),Neu(3),Sfe7))/
     &      ((Sub144(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &          Sub150(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub151(Cha(4),Gen(1),Gen(2))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        dup1 = -((Sub121(HelAll(Gen(6)),Neu(3),Sfe8)*
     &         Sub122(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8)*
     &         Sub76(HelAll(Cha(4)),Gen(1),Sfe7))/
     &       ((T14 - Sub182(Gen(1),Sfe7))*
     &         (Sub144(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &           Sub198(Gen(2),Sfe8)))) + 
     &    (Sub125(HelAll(Gen(2)),Neu(3),Sfe8)*
     &       ((Sub132(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8)*
     &            Sub133(HelAll(Cha(4)),Gen(1),Gen(5),Sfe7))/
     &          (S45 - Sub185(Gen(1),Sfe7)) - 
     &         (Sub127(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8)*
     &            Sub76(HelAll(Cha(4)),Gen(1),Sfe7))/
     &          (T14 - Sub182(Gen(1),Sfe7))))/(U - Sub198(Gen(2),Sfe8))

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(5))*
     &        IndexDelta(Gen(2),Gen(6)))/
     &      (MGl2 - cI*(MGl*WGl) - Sub142(Cha(4),Gen(1),Gen(5)))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*dup1*IndexDelta(Gen(1),Gen(5))*
     &        IndexDelta(Gen(2),Gen(6)))/
     &      (MGl2 - cI*(MGl*WGl) - Sub142(Cha(4),Gen(1),Gen(5)))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        dup2 = 1/(S45 - Sub143(Gen(1),Sfe8)) - 
     &    1/(Sub144(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) + 
     &       Sub145(Gen(2),Sfe7))

        dup3 = S45 - Sub144(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &    Sub147(Gen(1),Sfe8) - Sub148(Gen(2),Sfe7)

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(3.D0*CW*MW2*SW2)*
     &     (Alfas*dup2*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub4(HelAll(Gen(6)),Neu(3),Sfe7)*
     &        Sub7(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8)*
     &        Sub8(HelAll(Cha(4)),Gen(1),Gen(5),Sfe8))/
     &      (dup3*(MGl2 - cI*(MGl*WGl) - Sub142(Cha(4),Gen(1),Gen(5))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW*MW2*SW2)*
     &     (Alfas*dup2*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &        Sub4(HelAll(Gen(6)),Neu(3),Sfe7)*
     &        Sub7(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8)*
     &        Sub8(HelAll(Cha(4)),Gen(1),Gen(5),Sfe8))/
     &      (dup3*(MGl2 - cI*(MGl*WGl) - Sub142(Cha(4),Gen(1),Gen(5))))

#if DEBUG >= 4
        DEB("quqdbar_nIxJqdqdbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "real_contains.h"

        end

