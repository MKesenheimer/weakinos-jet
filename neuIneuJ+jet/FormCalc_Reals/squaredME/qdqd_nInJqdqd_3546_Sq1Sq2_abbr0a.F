#if 0
* qdqd_nInJqdqd_3546_Sq1Sq2_abbr0a.F
* abbreviations for {F[4, {Gen(1), Col1}], F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[4, {Gen(5), Col5}], F[4, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:16
#endif

#include "qdqd_nInJqdqd_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"
#include "realOS_contains.h"


        subroutine qdqd_nInJqdqd_3546_Sq1Sq2_abbr0a
        implicit none

#include "qdqd_nInJqdqd_3546_Sq1Sq2_vars.h"
#include "realOS_inline.h"


        Opt1(Gen(6),Neu(4)) = 
     &   S34 + S45 + T14 + T24 - Mf2(4,Gen(6)) - 3*MNeu2(Neu(4))



        Sub216(Gen(1),Gen(5),Neu(3)) = 
     &   S35 + T + T15 - Mf2(4,Gen(1)) - Mf2(4,Gen(5)) - MNeu2(Neu(3))



        Sub218(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   -Mf2(4,Gen(1)) - Mf2(4,Gen(2)) - Mf2(4,Gen(5)) - MNeu2(Neu(3)) + 
     &    Opt1(Gen(6),Neu(4))

        Sub230(Gen(1),Gen(2),Gen(5),Gen(6),Neu(3),Neu(4)) = 
     &   S + T + T15 - 3*Mf2(4,Gen(1)) - 
     &    2*(Mf2(4,Gen(2)) + Mf2(4,Gen(5)) + MNeu2(Neu(3))) + 
     &    Opt1(Gen(6),Neu(4))


#include "realOS_contains.h"

        end

