#if 0
* gg_nIxJquqdbar_3645_Sq1Sq2_abbr0a.F
* abbreviations for {V[5, {Glu1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[12, {Cha(4)}], F[3, {Gen(5), Col5}], -F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:54
#endif

#include "gg_nIxJquqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine gg_nIxJquqdbar_3645_Sq1Sq2_abbr0a
        implicit none

#include "gg_nIxJquqdbar_3645_Sq1Sq2_vars.h"
#include "realOS_inline.h"

        Opt4(Gen(6)) = S + T + T14 + T15 - Mf2(4,Gen(6))


        Sub455(Cha(4),Gen(5)) = 
     &   S45 + T14 + T15 - MCha2(Cha(4)) - Mf2(3,Gen(5))



        Sub459(Cha(4),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + S35 + T + U - MCha2(Cha(4)) - Mf2(3,Gen(5)) - 
     &    Mf2(4,Gen(6)) - 3*MNeu2(Neu(3))

        Sub464(Cha(4),Gen(5),Gen(6),Neu(3)) = 
     &   S34 + S35 + U - 2*(MCha2(Cha(4)) + Mf2(3,Gen(5))) - 
     &    3*MNeu2(Neu(3)) + Opt4(Gen(6))



        Sub460(Cha(4),Gen(5),Gen(6),Neu(3),Sq1) = 
     &   1/(S45 - Sub458(Gen(5),Sq1)) - 
     &    1/(Sub456(Gen(6),Sq1) + Sub459(Cha(4),Gen(5),Gen(6),Neu(3)))

        Sub463(Cha(4),Gen(5),Gen(6),Neu(3),Sq1) = 
     &   S45 - Sub459(Cha(4),Gen(5),Gen(6),Neu(3)) - Sub461(Gen(5),Sq1) - 
     &    Sub462(Gen(6),Sq1)


#include "realOS_contains.h"

        end

