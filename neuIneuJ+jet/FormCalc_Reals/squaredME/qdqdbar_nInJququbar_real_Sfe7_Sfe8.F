#if 0
* qdqdbar_nInJququbar_real_Sfe7_Sfe8.F
* form factors for {F[4, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:17
#endif

#include "qdqdbar_nInJququbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdqdbar_nInJququbar_real_Sfe7_Sfe8
        implicit none

#include "qdqdbar_nInJququbar_vars.h"
#include "real_inline.h"

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub14(HelAll(Gen(6)),Neu(3),Sfe7)*
     &        Sub194(HelAll(Gen(1)),Gen(2),Gen(5),Neu(4),Sfe7,Sfe8))/
     &      (CB*SB*(Sub181(Gen(5),Sfe7) + 
     &          Sub188(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub191(Gen(1),Gen(2),Neu(4))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub14(HelAll(Gen(6)),Neu(3),Sfe7)*
     &        Sub194(HelAll(Gen(1)),Gen(2),Gen(5),Neu(4),Sfe7,Sfe8))/
     &      (CB*SB*(Sub181(Gen(5),Sfe7) + 
     &          Sub188(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub191(Gen(1),Gen(2),Neu(4))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) - 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub16(HelAll(Gen(5)),Neu(4),Sfe7)*
     &        Sub196(HelAll(Gen(1)),Gen(2),Gen(6),Neu(3),Sfe7,Sfe8))/
     &      (CB*SB*(S45 - Sub181(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub195(Gen(1),Gen(2),Neu(3))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) + 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub16(HelAll(Gen(5)),Neu(4),Sfe7)*
     &        Sub196(HelAll(Gen(1)),Gen(2),Gen(6),Neu(3),Sfe7,Sfe8))/
     &      (CB*SB*(S45 - Sub181(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub195(Gen(1),Gen(2),Neu(3))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub193(HelAll(Gen(1)),Gen(2),Gen(6),Neu(4),Sfe7,Sfe8)*
     &        Sub5(HelAll(Gen(5)),Neu(3),Sfe7))/
     &      (CB*SB*(S35 - Sub181(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub191(Gen(1),Gen(2),Neu(4))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub193(HelAll(Gen(1)),Gen(2),Gen(6),Neu(4),Sfe7,Sfe8)*
     &        Sub5(HelAll(Gen(5)),Neu(3),Sfe7))/
     &      (CB*SB*(S35 - Sub181(Gen(5),Sfe7))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub191(Gen(1),Gen(2),Neu(4))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

        Ctree(HelAll(1)) = 
     &   Ctree(HelAll(1)) + 
     &    (8*Alfa*Pi**2)/(9.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub197(HelAll(Gen(1)),Gen(2),Gen(5),Neu(3),Sfe7,Sfe8)*
     &        Sub8(HelAll(Gen(6)),Neu(4),Sfe7))/
     &      (CB*SB*(Sub181(Gen(5),Sfe7) + 
     &          Sub182(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub195(Gen(1),Gen(2),Neu(3))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(1)", Ctree(HelAll(1)))
#endif

        Ctree(HelAll(2)) = 
     &   Ctree(HelAll(2)) - 
     &    (8*Alfa*Pi**2)/(27.D0*CW2*MW2*SW2)*
     &     (Alfas*IndexDelta(Gen(1),Gen(2))*IndexDelta(Gen(5),Gen(6))*
     &        Sub197(HelAll(Gen(1)),Gen(2),Gen(5),Neu(3),Sfe7,Sfe8)*
     &        Sub8(HelAll(Gen(6)),Neu(4),Sfe7))/
     &      (CB*SB*(Sub181(Gen(5),Sfe7) + 
     &          Sub182(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)))*
     &        (MGl2 - cI*(MGl*(WGl + WREG1)) - 
     &          Sub195(Gen(1),Gen(2),Neu(3))))

#if DEBUG >= 4
        DEB("qdqdbar_nInJququbar_real_Sfe7_Sfe8:Ctree(2)", Ctree(HelAll(2)))
#endif

#include "real_contains.h"

        end

