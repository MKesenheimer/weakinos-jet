#if 0
* ququbar_nInJququbar_3546_Sq1Sq2_abbr0a.F
* abbreviations for {F[3, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[3, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:19
#endif

#include "ququbar_nInJququbar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine ququbar_nInJququbar_3546_Sq1Sq2_abbr0a
        implicit none

#include "ququbar_nInJququbar_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"


        Opt1(Gen(1),Gen(2),Neu(3)) = 
     &   S34 + T14 - Mf2(3,Gen(1)) - Mf2(3,Gen(2)) - MNeu2(Neu(3))



        Sub332(Gen(1),Gen(5),Neu(3)) = 
     &   S35 + T + T15 - Mf2(3,Gen(1)) - Mf2(3,Gen(5)) - MNeu2(Neu(3))



        Sub334(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S45 + T24 - Mf2(3,Gen(5)) - Mf2(3,Gen(6)) - 3*MNeu2(Neu(4)) + 
     &    Opt1(Gen(1),Gen(2),Neu(3))


#include "realOS_contains.h"

        end

