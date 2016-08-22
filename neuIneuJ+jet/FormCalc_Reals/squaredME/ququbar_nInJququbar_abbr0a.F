#if 0
* ququbar_nInJququbar_abbr0a.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 8.4 on 4-Aug-2016 14:37
#endif

#include "ququbar_nInJququbar_vars.h"
#include "contains.h"


        subroutine ququbar_nInJququbar_abbr0a
        implicit none

#include "ququbar_nInJququbar_vars.h"



        Sub374(Gen(1),Gen(2),Gen(5)) = 
     &   S + T15 + T25 - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - Mf2(3,Gen(5))



        Sub416(Gen(1),Gen(2),Neu(3)) = 
     &   S + T + U - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - MNeu2(Neu(3))



        Sub409(Gen(1),Gen(2),Neu(4)) = 
     &   S + T14 + T24 - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - MNeu2(Neu(4))



        Sub365(Gen(1),Gen(5),Neu(3)) = 
     &   S35 + T + T15 - Mf2(3,Gen(1)) - Mf2(3,Gen(5)) - MNeu2(Neu(3))



        Sub368(Gen(1),Gen(5),Neu(4)) = 
     &   S45 + T14 + T15 - Mf2(3,Gen(1)) - Mf2(3,Gen(5)) - MNeu2(Neu(4))



        Sub372(Gen(1),Neu(3),Neu(4)) = 
     &   S34 + T + T14 - Mf2(3,Gen(1)) - MNeu2(Neu(3)) - MNeu2(Neu(4))



        Opt4(Gen(6),Neu(3),Neu(4)) = 
     &   S + T + T14 + T15 + T25 - Mf2(3,Gen(6)) - 
     &    2*(MNeu2(Neu(3)) + MNeu2(Neu(4)))



        Opt2(Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   S34 - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - Mf2(3,Gen(5)) - 
     &    Mf2(3,Gen(6))



        Opt3(Gen(1),Gen(6),Neu(3),Neu(4)) = 
     &   T25 - Mf2(3,Gen(1)) - Mf2(3,Gen(6)) - MNeu2(Neu(3)) - 
     &    MNeu2(Neu(4))



        Sub362(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S45 + T14 + T24 - MNeu2(Neu(3)) - 3*MNeu2(Neu(4)) + 
     &    Opt2(Gen(1),Gen(2),Gen(5),Gen(6))

        Sub367(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S35 + T + U - 3*MNeu2(Neu(3)) - MNeu2(Neu(4)) + 
     &    Opt2(Gen(1),Gen(2),Gen(5),Gen(6))

        Sub377(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S35 + S45 + T15 - Mf2(3,Gen(2)) - 3*Mf2(3,Gen(5)) + 
     &    Opt3(Gen(1),Gen(6),Neu(3),Neu(4))

        Sub370(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S + T24 + U - 3*Mf2(3,Gen(2)) - Mf2(3,Gen(5)) + 
     &    Opt3(Gen(1),Gen(6),Neu(3),Neu(4))

        Sub371(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   T24 + U - 3*(Mf2(3,Gen(1)) + Mf2(3,Gen(2))) - 2*Mf2(3,Gen(5)) + 
     &    Opt4(Gen(6),Neu(3),Neu(4))

        Sub369(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S35 + S45 - 2*Mf2(3,Gen(2)) - 
     &    3*(Mf2(3,Gen(1)) + Mf2(3,Gen(5))) + Opt4(Gen(6),Neu(3),Neu(4))


#include "contains.h"

        end