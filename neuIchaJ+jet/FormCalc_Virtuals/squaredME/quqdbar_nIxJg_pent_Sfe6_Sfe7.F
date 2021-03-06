#if 0
* quqdbar_nIxJg_pent_Sfe6_Sfe7.F
* form factors for {F[3, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], V[5, {Glu5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 11-Nov-2017 23:19
#endif

#include "quqdbar_nIxJg_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine quqdbar_nIxJg_pent_Sfe6_Sfe7
        implicit none

#include "quqdbar_nIxJg_vars.h"
        HelType tmp1

#include "virt_inline.h"

        tmp1 = -(1/9.D0*(AaE0i15(Epsi(ee002),Sfe6,Sfe7)*
     &         Sub10478(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7))) + 
     &    1/9.D0*(AaE0i15(Epsi(ee123),Sfe6,Sfe7)*
     &       Sub10528(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(Pair2*AaE0i16(Epsi(ee223),Sfe6,Sfe7)*
     &       Sub10556(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i15(Epsi(ee122),Sfe6,Sfe7)*
     &       Sub10650(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(AaE0i17(Epsi(ee003),Sfe6,Sfe7)*
     &       Sub10667(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i15(Epsi(ee224),Sfe6,Sfe7)*
     &       Sub10682(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i15(Epsi(ee133),Sfe6,Sfe7)*
     &       Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &       Sub10698(HelAll(Cha(4)),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i16(Epsi(ee002),Sfe6,Sfe7)*
     &       Sub10720(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(AaE0i16(Epsi(ee224),Sfe6,Sfe7)*
     &       Sub10810(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i18(Epsi(ee134),Sfe6,Sfe7)*
     &       Sub10874(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(AaE0i19(Epsi(ee234),Sfe6,Sfe7)*
     &       Sub10884(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    Pair3*(1/9.D0*(AaE0i15(Epsi(ee222),Sfe6,Sfe7)*MNeu(Neu(3))*
     &          Sub10265(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &          Sub10742(HelAll(Cha(4)),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       1/9.D0*(AaE0i15(Epsi(ee333),Sfe6,Sfe7)*
     &          Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &          Sub10912(HelAll(Cha(4)),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &       1/9.D0*(AaE0i18(Epsi(ee344),Sfe6,Sfe7)*
     &          Sub10951(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)))-
     &      1/9.D0*(AaE0i19(Epsi(ee223),Sfe6,Sfe7)*
     &       Sub10968(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    AaE0i13(Epsi(ee001),Sfe6,Sfe7)*
     &     Sub10342(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee113),Sfe6,Sfe7)*
     &     Sub10402(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee001),Sfe6,Sfe7)*
     &     Sub10413(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i14(Epsi(ee113),Sfe6,Sfe7)*
     &     Sub10446(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee233),Sfe6,Sfe7)*
     &     Sub10505(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee123),Sfe6,Sfe7)*
     &     Sub10539(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee114),Sfe6,Sfe7)*
     &     Sub10265(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &     Sub10540(HelAll(Cha(4)),Gen(1),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i13(Epsi(ee134),Sfe6,Sfe7)*
     &     Sub10549(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee123),Sfe6,Sfe7)*
     &     Sub10598(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i13(Epsi(ee334),Sfe6,Sfe7)*
     &     Sub10629(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee003),Sfe6,Sfe7)*
     &     Sub10660(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee334),Sfe6,Sfe7)*
     &     Sub10666(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i14(Epsi(ee003),Sfe6,Sfe7)*
     &     Sub10681(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i13(Epsi(ee133),Sfe6,Sfe7)*
     &     Sub10683(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i14(Epsi(ee134),Sfe6,Sfe7)*
     &     Sub10694(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee244),Sfe6,Sfe7)*
     &     Sub10710(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    Abb2*(AaE0i13(Epsi(ee112),Sfe6,Sfe7)*
     &        Sub10514(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee122),Sfe6,Sfe7)*
     &        Sub10728(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    AaE0i14(Epsi(ee244),Sfe6,Sfe7)*
     &     Sub10741(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee133),Sfe6,Sfe7)*
     &     Sub10750(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee234),Sfe6,Sfe7)*
     &     Sub10762(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    Abb1*(AaE0i14(Epsi(ee112),Sfe6,Sfe7)*
     &        Sub10605(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i14(Epsi(ee122),Sfe6,Sfe7)*
     &        Sub10809(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    AaE0i13(Epsi(ee124),Sfe6,Sfe7)*
     &     Sub10824(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee144),Sfe6,Sfe7)*
     &     Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &     Sub10857(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i14(Epsi(ee124),Sfe6,Sfe7)*
     &     Sub10897(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i14(Epsi(ee144),Sfe6,Sfe7)*
     &     Sub10265(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &     Sub10911(HelAll(Cha(4)),Gen(1),Neu(3),Sfe6,Sfe7)

        tmp1 = tmp1 - 1/4.D0*
     &     (AaE0i13(Epsi(ee2),Sfe6,Sfe7)*
     &       Sub17973(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/36.D0*(AaE0i17(Epsi(ee1),Sfe6,Sfe7)*
     &       Sub18868(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i15(Epsi(ee344),Sfe6,Sfe7)*
     &        Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &        Sub10971(HelAll(Cha(4)),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i16(Epsi(ee234),Sfe6,Sfe7)*
     &        Sub10976(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &        (AaE0i20(Epsi(ee224),Sfe6,Sfe7)*
     &           Sub10977(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &          AaE0i20(Epsi(ee334),Sfe6,Sfe7)*
     &           Sub11042(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7))-
     &         AaE0i15(Epsi(ee244),Sfe6,Sfe7)*
     &        Sub11056(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i16(Epsi(ee244),Sfe6,Sfe7)*
     &        Sub11069(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i16(Epsi(ee00),Sfe6,Sfe7)*
     &        Sub19383(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(-(AaE0i19(Epsi(ee00),Sfe6,Sfe7)*
     &          Sub19450(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i18(Epsi(ee12),Sfe6,Sfe7)*
     &        Sub19553(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(-(AaE0i14(Epsi(ee1),Sfe6,Sfe7)*
     &          Sub18010(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i14(Epsi(ee2),Sfe6,Sfe7)*
     &        Sub18624(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee4),Sfe6,Sfe7)*
     &        Sub18720(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee1),Sfe6,Sfe7)*
     &        Sub18958(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee12),Sfe6,Sfe7)*
     &        Sub19646(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i15(Epsi(ee00),Sfe6,Sfe7)*
     &       Sub19651(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/18.D0*(-(AaE0i20(Epsi(ee1),Sfe6,Sfe7)*
     &          Sub19309(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i17(Epsi(ee3),Sfe6,Sfe7)*
     &        Sub19681(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(AaE0i14(Epsi(ee3),Sfe6,Sfe7)*
     &       Sub19706(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(AaE0i16(Epsi(ee12),Sfe6,Sfe7)*
     &       Sub19723(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(-(AaE0i14(Epsi(ee4),Sfe6,Sfe7)*
     &          Sub19104(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i13(Epsi(ee23),Sfe6,Sfe7)*
     &        Sub19800(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(AaE0i14(Epsi(ee12),Sfe6,Sfe7)*
     &       Sub19844(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    Pair3*(1/9.D0*(AaE0i15(Epsi(ee23),Sfe6,Sfe7)*
     &          Sub19753(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       1/9.D0*(AaE0i18(Epsi(ee34),Sfe6,Sfe7)*
     &          Sub19852(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)))-
     &      1/9.D0*(AaE0i19(Epsi(ee1),Sfe6,Sfe7)*
     &       Sub19859(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i15(Epsi(ee44),Sfe6,Sfe7)*
     &       Sub19864(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/2.D0*(AaE0i13(Epsi(ee22),Sfe6,Sfe7)*
     &       Sub19876(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(AaE0i15(Epsi(ee34),Sfe6,Sfe7)*
     &       Sub19902(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    Pair2*(1/9.D0*(AaE0i19(Epsi(ee334),Sfe6,Sfe7)*
     &          Sub11005(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       1/9.D0*(AaE0i16(Epsi(ee23),Sfe6,Sfe7)*
     &          Sub19815(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &       1/9.D0*(AaE0i19(Epsi(ee33),Sfe6,Sfe7)*
     &          Sub19907(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)))-
     &      1/9.D0*(AaE0i15(Epsi(ee1),Sfe6,Sfe7)*
     &       Sub19918(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(AaE0i14(Epsi(ee11),Sfe6,Sfe7)*
     &       Sub19930(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/18.D0*(AaE0i20(Epsi(ee24),Sfe6,Sfe7)*
     &       Sub19951(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/18.D0*(AaE0i17(Epsi(ee12),Sfe6,Sfe7)*
     &       Sub20006(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    AaE0i14(Epsi(ee344),Sfe6,Sfe7)*
     &     Sub10994(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &    AaE0i13(Epsi(ee00),Sfe6,Sfe7)*
     &     Sub19376(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    AaE0i14(Epsi(ee00),Sfe6,Sfe7)*
     &     Sub19437(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)

        tmp1 = tmp1 + 1/18.D0*
     &     (-(AaE0i20(Epsi(ee12),Sfe6,Sfe7)*
     &          Sub20113(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i17(Epsi(ee22),Sfe6,Sfe7)*
     &        Sub20167(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i20(Epsi(ee22),Sfe6,Sfe7)*
     &        Sub20195(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i17(Epsi(ee44),Sfe6,Sfe7)*
     &        Sub20251(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i20(Epsi(ee33),Sfe6,Sfe7)*
     &        Sub20393(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i20(Epsi(ee44),Sfe6,Sfe7)*
     &        Sub20414(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i20(Epsi(ee34),Sfe6,Sfe7)*
     &        Sub20525(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/4.D0*(AaE0i13(Epsi(ee3),Sfe6,Sfe7)*
     &       Sub20589(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/36.D0*(-(AaE0i17(Epsi(ee23),Sfe6,Sfe7)*
     &          Sub20307(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i17(Epsi(ee33),Sfe6,Sfe7)*
     &        Sub20355(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i17(Epsi(ee34),Sfe6,Sfe7)*
     &        Sub20460(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i17(Epsi(ee13),Sfe6,Sfe7)*
     &        Sub20666(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i17(Epsi(ee24),Sfe6,Sfe7)*
     &        Sub20700(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(-(AaE0i14(Epsi(ee23),Sfe6,Sfe7)*
     &          Sub20024(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       AaE0i14(Epsi(ee22),Sfe6,Sfe7)*
     &        Sub20040(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee44),Sfe6,Sfe7)*
     &        Sub20049(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee11),Sfe6,Sfe7)*
     &        Sub20133(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i13(Epsi(ee34),Sfe6,Sfe7)*
     &        Sub20155(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i14(Epsi(ee44),Sfe6,Sfe7)*
     &        Sub20208(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i13(Epsi(ee33),Sfe6,Sfe7)*
     &        Sub20279(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i14(Epsi(ee33),Sfe6,Sfe7)*
     &        Sub20289(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i13(Epsi(ee14),Sfe6,Sfe7)*
     &        Sub20495(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i14(Epsi(ee13),Sfe6,Sfe7)*
     &        Sub20556(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i14(Epsi(ee24),Sfe6,Sfe7)*
     &        Sub20615(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i14(Epsi(ee0),Sfe6,Sfe7)*
     &        Sub20703(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/2.D0*(AaE0i14(Epsi(ee14),Sfe6,Sfe7)*
     &       Sub20724(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/9.D0*(AaE0i19(Epsi(ee11),Sfe6,Sfe7)*
     &        Sub20029(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i18(Epsi(ee11),Sfe6,Sfe7)*
     &        Sub20044(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Pair3*AaE0i18(Epsi(ee33),Sfe6,Sfe7)*
     &        Sub20117(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i19(Epsi(ee13),Sfe6,Sfe7)*
     &        Sub20204(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i18(Epsi(ee13),Sfe6,Sfe7)*
     &        Sub20216(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i18(Epsi(ee2),Sfe6,Sfe7)*
     &        Sub20241(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i19(Epsi(ee14),Sfe6,Sfe7)*
     &        Sub20268(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Pair2*AaE0i16(Epsi(ee33),Sfe6,Sfe7)*
     &        Sub20397(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i16(Epsi(ee13),Sfe6,Sfe7)*
     &        Sub20406(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i18(Epsi(ee24),Sfe6,Sfe7)*
     &        Sub20506(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i16(Epsi(ee24),Sfe6,Sfe7)*
     &        Sub20566(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       AaE0i16(Epsi(ee14),Sfe6,Sfe7)*
     &        Sub20622(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i15(Epsi(ee24),Sfe6,Sfe7)*
     &        Sub20676(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       AaE0i16(Epsi(ee4),Sfe6,Sfe7)*
     &        Sub20731(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7))

        tmp1 = tmp1 - 1/2.D0*
     &     (AaE0i13(Epsi(ee24),Sfe6,Sfe7)*
     &       Sub20758(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/18.D0*(AaE0i20(Epsi(ee23),Sfe6,Sfe7)*
     &       Sub20924(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(AaE0i13(Epsi(ee0),Sfe6,Sfe7)*
     &       Sub20936(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*Sub21493(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    1/9.D0*(-(AaE0i19(Epsi(ee23),Sfe6,Sfe7)*
     &          Sub20931(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) + 
     &       Pair3*AaE0i15(Epsi(ee0),Sfe6,Sfe7)*
     &        Sub20933(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub10317(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21449(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub10329(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21450(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub10394(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21452(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21453(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub10424(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21454(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21463(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub10468(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21467(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21470(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21471(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21472(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21474(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21475(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21476(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21477(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21479(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21480(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub10719(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21481(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21482(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21483(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21484(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21485(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21486(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Pair2*Sub21487(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &         Sfe7) - Sub21488(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &        Sfe7) + Sub21489(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &        Sfe7) + Sub21490(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &        Sfe7) - Abb1*
     &        Sub21491(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21492(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21494(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21495(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub10962(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub21496(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21497(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21498(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    (1/18.D0*Sub21462(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       1/9.D0*(-(Sub10471(HelAll(Cha(4)),Gen(1),Neu(3),Sfe6,Sfe7)*
     &             Sub21468(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &          Sub21469(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &          Sub10585(HelAll(Cha(4)),Gen(1),Neu(3),Sfe6,Sfe7)*
     &           Sub21473(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &       1/9.D0*Sub21478(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7))*
     &     Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7) + 
     &    Sub10374(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &     Sub21451(Cha(4),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &    Sub10265(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &     (-(1/9.D0*Sub21466(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &           Sfe7)) + Sub21458(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),
     &        Sfe6,Sfe7))

        tmp1 = tmp1 + 1/9.D0*
     &     (-Sub21500(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21501(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21502(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21506(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21509(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21510(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21511(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21514(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21515(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21516(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21518(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21519(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21522(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21523(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21524(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21525(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21526(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)) - 
     &    1/9.D0*(Pair3*(Sub21512(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &          Sfe7) + MNeu(Neu(3))*
     &          Sub10265(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &          Sub21536(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)))+
     &      1/18.D0*(-Sub21503(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &         Sfe7) - Sub21505(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,
     &        Sfe7) + Sub19543(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &        Sub21507(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21508(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21527(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) + 
     &       Sub21533(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub3173(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7) - 
     &       Sub21539(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub7414(HelAll(Cha(4)),Gen(1),Neu(3),Sfe6,Sfe7)) + 
     &    1/2.D0*(-Sub21520(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       Sub21538(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7)*
     &        Sub589(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7) + 
     &       AaD0i21(Epsi(dd2),Sfe7)*
     &        Sub589(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)*
     &        Sub8281(HelAll(Cha(4)),Gen(1),Neu(3),Sfe6,Sfe7)) + 
     &    (1/9.D0*Sub21499(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7) - 
     &       1/9.D0*Sub21542(HelAll(Cha(4)),Gen(1),Gen(2),Neu(3),Sfe6,Sfe7))*
     &     Sub10318(HelAll(Gen(1)),Gen(2),Sfe6,Sfe7)

        Cloop(HelAll(1)) = 
     &   Cloop(HelAll(1)) + 
     &    Alfa/(CW*MW2*SW2)*(Alfas*GS*tmp1*IndexDelta(Gen(1),Gen(2)))

#if DEBUG >= 4
        DEB("quqdbar_nIxJg_pent_Sfe6_Sfe7:Cloop(1)", Cloop(HelAll(1)))
#endif

#include "virt_contains.h"

        end

