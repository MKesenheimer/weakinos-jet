#if 0
* ququbar_nIxJqdqubar_abbr0a.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:56
#endif

#include "ququbar_nIxJqdqubar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine ququbar_nIxJqdqubar_abbr0a
        implicit none

#include "ququbar_nIxJqdqubar_vars.h"
#include "real_inline.h"


        Opt5(Cha(4),Neu(3)) = S34 + T + T14 - MCha2(Cha(4)) - MNeu2(Neu(3))



        Sub152(Cha(4),Gen(1),Gen(5)) = 
     &   S45 + T14 + T15 - MCha2(Cha(4)) - Mf2(3,Gen(1)) - Mf2(4,Gen(5))



        Sub77(Cha(4),Gen(1),Neu(3)) = -2*Mf2(3,Gen(1)) + Opt5(Cha(4),Neu(3))

        Sub166(Cha(4),Gen(1),Neu(3)) = -Mf2(3,Gen(1)) + Opt5(Cha(4),Neu(3))



        Sub76(Cha(4),Gen(5),Neu(3)) = 
     &   S34 + S35 + S45 - MCha2(Cha(4)) - 2*Mf2(4,Gen(5)) - 
     &    MNeu2(Neu(3))



        Sub163(Gen(1),Gen(2),Gen(5)) = 
     &   S + T15 + T25 - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - Mf2(4,Gen(5))



        Sub159(Gen(1),Gen(2),Neu(3)) = 
     &   S + T + U - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - MNeu2(Neu(3))



        Opt6(Cha(4),Gen(1),Gen(5),Gen(6)) = 
     &   U - MCha2(Cha(4)) - Mf2(3,Gen(1)) - Mf2(3,Gen(6)) - Mf2(4,Gen(5))



        Sub56(Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -Mf2(3,Gen(6)) + Sub163(Gen(1),Gen(2),Gen(5))



        Sub139(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + S45 + T14 + T24 - 3*MCha2(Cha(4)) - Mf2(3,Gen(1)) - 
     &    Mf2(3,Gen(2)) - Mf2(3,Gen(6)) - Mf2(4,Gen(5)) - MNeu2(Neu(3))

        Sub146(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + S35 + T - Mf2(3,Gen(2)) - 3*MNeu2(Neu(3)) + 
     &    Opt6(Cha(4),Gen(1),Gen(5),Gen(6))

        Sub162(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S + T + T14 + T15 + T24 + T25 + U - 
     &    3*(Mf2(3,Gen(1)) + Mf2(3,Gen(2))) - Mf2(3,Gen(6)) - 
     &    2*(MCha2(Cha(4)) + Mf2(4,Gen(5)) + MNeu2(Neu(3)))

        Sub165(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S + T24 + T25 - 3*Mf2(3,Gen(2)) - MNeu2(Neu(3)) + 
     &    Opt6(Cha(4),Gen(1),Gen(5),Gen(6))


#include "real_contains.h"

        end

