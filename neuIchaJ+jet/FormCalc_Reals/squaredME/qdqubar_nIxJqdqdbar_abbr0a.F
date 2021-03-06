#if 0
* qdqubar_nIxJqdqdbar_abbr0a.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:55
#endif

#include "qdqubar_nIxJqdqdbar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdqubar_nIxJqdqdbar_abbr0a
        implicit none

#include "qdqubar_nIxJqdqdbar_vars.h"
#include "real_inline.h"


        Sub149(Cha(4),Gen(1),Gen(2)) = 
     &   S + T14 + T24 - MCha2(Cha(4)) - Mf2(3,Gen(2)) - Mf2(4,Gen(1))



        Sub160(Cha(4),Gen(1),Neu(3)) = 
     &   S34 + T + T14 - MCha2(Cha(4)) - Mf2(4,Gen(1)) - MNeu2(Neu(3))



        Sub42(Cha(4),Gen(2),Neu(3)) = 
     &   S34 + T24 + U - MCha2(Cha(4)) - 2*Mf2(3,Gen(2)) - MNeu2(Neu(3))



        Sub155(Gen(1),Gen(2),Gen(5)) = 
     &   S + T15 + T25 - Mf2(3,Gen(2)) - Mf2(4,Gen(1)) - Mf2(4,Gen(5))



        Sub140(Gen(1),Gen(5),Neu(3)) = 
     &   S35 + T + T15 - Mf2(4,Gen(1)) - Mf2(4,Gen(5)) - MNeu2(Neu(3))



        Opt4(Cha(4),Gen(1),Gen(2),Gen(6)) = 
     &   S35 - MCha2(Cha(4)) - Mf2(3,Gen(2)) - Mf2(4,Gen(1)) - 
     &    Mf2(4,Gen(6))



        Sub32(Cha(4),Gen(1),Gen(2),Neu(3)) = 
     &   -Mf2(3,Gen(2)) + Sub160(Cha(4),Gen(1),Neu(3))



        Sub50(Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -Mf2(4,Gen(6)) + Sub155(Gen(1),Gen(2),Gen(5))



        Sub142(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + S45 + T14 + T24 - 3*MCha2(Cha(4)) - Mf2(3,Gen(2)) - 
     &    Mf2(4,Gen(1)) - Mf2(4,Gen(5)) - Mf2(4,Gen(6)) - MNeu2(Neu(3))

        Sub153(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + T + U - Mf2(4,Gen(5)) - 3*MNeu2(Neu(3)) + 
     &    Opt4(Cha(4),Gen(1),Gen(2),Gen(6))

        Sub156(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S + S35 + S45 + T + T14 + T15 + T25 - 
     &    3*(Mf2(4,Gen(1)) + Mf2(4,Gen(5))) - Mf2(4,Gen(6)) - 
     &    2*(MCha2(Cha(4)) + Mf2(3,Gen(2)) + MNeu2(Neu(3)))

        Sub158(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S45 + T15 + T25 - 3*Mf2(4,Gen(5)) - MNeu2(Neu(3)) + 
     &    Opt4(Cha(4),Gen(1),Gen(2),Gen(6))


#include "real_contains.h"

        end

