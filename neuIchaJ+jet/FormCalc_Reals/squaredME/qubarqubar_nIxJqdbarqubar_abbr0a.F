#if 0
* qubarqubar_nIxJqdbarqubar_abbr0a.F
* abbreviations for {-F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], -F[4, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:58
#endif

#include "qubarqubar_nIxJqdbarqubar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qubarqubar_nIxJqdbarqubar_abbr0a
        implicit none

#include "qubarqubar_nIxJqdbarqubar_vars.h"
#include "real_inline.h"


        Opt4(Cha(4),Gen(6)) = 
     &   S + T + T14 + T15 - 2*MCha2(Cha(4)) - Mf2(3,Gen(6))



        Opt2(Cha(4),Neu(3)) = T + T14 - MCha2(Cha(4)) - MNeu2(Neu(3))



        Sub157(Cha(4),Gen(1),Gen(5)) = 
     &   S45 + T14 + T15 - MCha2(Cha(4)) - Mf2(3,Gen(1)) - Mf2(4,Gen(5))



        Sub57(Cha(4),Gen(1),Neu(3)) = S34 - 2*Mf2(3,Gen(1)) + Opt2(Cha(4),Neu(3))

        Sub168(Cha(4),Gen(1),Neu(3)) = S34 - Mf2(3,Gen(1)) + Opt2(Cha(4),Neu(3))



        Sub53(Cha(4),Gen(2),Neu(3)) = 
     &   S34 + T24 + U - MCha2(Cha(4)) - 2*Mf2(3,Gen(2)) - MNeu2(Neu(3))



        Sub36(Cha(4),Gen(5),Neu(3)) = 
     &   S34 + S35 + S45 - MCha2(Cha(4)) - 2*Mf2(4,Gen(5)) - 
     &    MNeu2(Neu(3))



        Opt5(Cha(4),Gen(1),Gen(2),Gen(6)) = 
     &   S35 - 3*Mf2(3,Gen(1)) - 2*Mf2(3,Gen(2)) + Opt4(Cha(4),Gen(6))



        Opt3(Cha(4),Gen(1),Gen(5),Gen(6)) = 
     &   U - MCha2(Cha(4)) - Mf2(3,Gen(1)) - Mf2(3,Gen(6)) - Mf2(4,Gen(5))



        Sub150(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + S35 + T - Mf2(3,Gen(2)) - 3*MNeu2(Neu(3)) + 
     &    Opt3(Cha(4),Gen(1),Gen(5),Gen(6))

        Sub156(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + U - 2*Mf2(4,Gen(5)) - 3*MNeu2(Neu(3)) + 
     &    Opt5(Cha(4),Gen(1),Gen(2),Gen(6))

        Sub166(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S + T24 + T25 - 3*Mf2(3,Gen(2)) - MNeu2(Neu(3)) + 
     &    Opt3(Cha(4),Gen(1),Gen(5),Gen(6))

        Sub167(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   T24 + T25 + U - 3*(Mf2(3,Gen(1)) + Mf2(3,Gen(2))) - 
     &    2*(Mf2(4,Gen(5)) + MNeu2(Neu(3))) + Opt4(Cha(4),Gen(6))

        Sub170(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S + T15 - 3*Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - Mf2(3,Gen(6)) - 
     &    Mf2(4,Gen(5)) + Opt2(Cha(4),Neu(3))

        Sub171(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   S45 + T25 - 3*Mf2(4,Gen(5)) - 2*MNeu2(Neu(3)) + 
     &    Opt5(Cha(4),Gen(1),Gen(2),Gen(6))

        Sub179(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub36(Cha(4),Gen(5),Neu(3))/
     &       (Mf2(3,Gen(2)) + Sub167(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))
     &      + Sub53(Cha(4),Gen(2),Neu(3))/
     &     (Mf2(4,Gen(2)) + Sub171(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))

        Sub182(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub36(Cha(4),Gen(5),Neu(3))/
     &       (Mf2(3,Gen(1)) + Sub167(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))))
     &      + Sub57(Cha(4),Gen(1),Neu(3))/
     &     (Mf2(4,Gen(1)) - Sub168(Cha(4),Gen(1),Neu(3)))


#include "real_contains.h"

        end

