#if 0
* qdqubar_nIxJququbar_abbr0a.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:56
#endif

#include "qdqubar_nIxJququbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdqubar_nIxJququbar_abbr0a
        implicit none

#include "qdqubar_nIxJququbar_vars.h"
#include "real_inline.h"


        Opt4(Cha(4),Neu(3)) = S34 + T + T14 - MCha2(Cha(4)) - MNeu2(Neu(3))



        Sub158(Cha(4),Gen(1),Gen(2)) = 
     &   S + T14 + T24 - MCha2(Cha(4)) - Mf2(3,Gen(2)) - Mf2(4,Gen(1))



        Sub149(Cha(4),Gen(1),Gen(5)) = 
     &   S45 + T14 + T15 - MCha2(Cha(4)) - Mf2(3,Gen(5)) - Mf2(4,Gen(1))



        Sub56(Cha(4),Gen(1),Neu(3)) = -2*Mf2(4,Gen(1)) + Opt4(Cha(4),Neu(3))

        Sub164(Cha(4),Gen(1),Neu(3)) = -Mf2(4,Gen(1)) + Opt4(Cha(4),Neu(3))



        Sub58(Cha(4),Gen(2),Neu(3)) = 
     &   S34 + T24 + U - MCha2(Cha(4)) - 2*Mf2(3,Gen(2)) - MNeu2(Neu(3))



        Sub55(Cha(4),Gen(5),Neu(3)) = 
     &   S34 + S35 + S45 - MCha2(Cha(4)) - 2*Mf2(3,Gen(5)) - 
     &    MNeu2(Neu(3))



        Opt6(Gen(1),Gen(6),Neu(3)) = 
     &   S + T + T14 + T15 + T25 - Mf2(3,Gen(6)) - 3*Mf2(4,Gen(1)) - 
     &    2*MNeu2(Neu(3))



        Opt5(Cha(4),Gen(1),Gen(2),Gen(6)) = 
     &   S35 - MCha2(Cha(4)) - Mf2(3,Gen(2)) - Mf2(3,Gen(6)) - 
     &    Mf2(4,Gen(1))



        Sub63(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   -Mf2(3,Gen(2)) + Sub164(Cha(4),Gen(1),Neu(3))



        Sub151(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + T + U - Mf2(3,Gen(5)) - 3*MNeu2(Neu(3)) + 
     &    Opt5(Cha(4),Gen(1),Gen(2),Gen(6))

        Sub162(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S + T24 + T25 + U - MCha2(Cha(4)) - 3*Mf2(3,Gen(2)) - 
     &    Mf2(3,Gen(5)) - Mf2(3,Gen(6)) - Mf2(4,Gen(1)) - MNeu2(Neu(3))

        Sub163(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   T24 + U - 3*Mf2(3,Gen(2)) - 2*(MCha2(Cha(4)) + Mf2(3,Gen(5))) + 
     &    Opt6(Gen(1),Gen(6),Neu(3))

        Sub165(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S45 + T15 + T25 - 3*Mf2(3,Gen(5)) - MNeu2(Neu(3)) + 
     &    Opt5(Cha(4),Gen(1),Gen(2),Gen(6))

        Sub167(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S35 + S45 - 2*(MCha2(Cha(4)) + Mf2(3,Gen(2))) - 
     &    3*Mf2(3,Gen(5)) + Opt6(Gen(1),Gen(6),Neu(3))

        Sub175(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub55(Cha(4),Gen(5),Neu(3))/
     &       (Mf2(4,Gen(1)) + Sub163(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))
     &      + Sub56(Cha(4),Gen(1),Neu(3))/
     &     (Mf2(3,Gen(1)) - Sub164(Cha(4),Gen(1),Neu(3)))

        Sub184(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub58(Cha(4),Gen(2),Neu(3))/
     &       (Mf2(4,Gen(1)) + Sub167(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))
     &      + Sub63(Cha(4),Gen(1),Gen(2),Neu(3))/
     &     (Mf2(3,Gen(1)) - Sub164(Cha(4),Gen(1),Neu(3)))


#include "real_contains.h"

        end

