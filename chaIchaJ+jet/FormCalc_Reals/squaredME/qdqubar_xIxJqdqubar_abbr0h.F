#if 0
* qdqubar_xIxJqdqubar_abbr0h.F
* abbreviations for {F[4, {Gen(1), Col1}], -F[3, {Gen(2), Col2}]} -> {-F[12, {Cha(3)}], F[12, {Cha(4)}], F[4, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 11:05
#endif

#include "qdqubar_xIxJqdqubar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdqubar_xIxJqdqubar_abbr0h
        implicit none

#include "qdqubar_xIxJqdqubar_vars.h"
#include "real_inline.h"

        F1 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(6,-1,1))

        F2 = ChainB0(Spinor(5,1,2),0,0,Spinor(3,-1,1))

        F3 = ChainV0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F4 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(6,-1,1))

        F5 = ChainV1(Spinor(2,-1,2),0,k(1),0,Spinor(6,-1,1))

        F6 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(6,-1,1))

        F7 = ChainB1(Spinor(2,-1,2),0,k(1),0,Spinor(6,-1,1))

        F8 = ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(6,-1,1))

        F9 = -ChainV0(Spinor(5,1,2),0,0,Spinor(3,-1,1))

        F10 = ChainB0(Spinor(4,1,2),0,0,Spinor(1,1,1))

        F11 = ChainB0(Spinor(6,1,2),0,0,Spinor(1,1,1))

        F12 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F13 = ChainB1(Spinor(6,1,2),0,k(4),0,Spinor(1,1,1))

        F14 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F15 = ChainB1(Spinor(6,1,2),0,k(2),0,Spinor(1,1,1))

        F16 = ChainV1(Spinor(6,1,2),0,k(4),0,Spinor(1,1,1))

        F17 = ChainV0(Spinor(6,1,2),0,0,Spinor(1,1,1))

        F18 = ChainV1(Spinor(6,1,2),0,k(2),0,Spinor(1,1,1))

        F19 = ChainV0(Spinor(5,1,2),0,0,Spinor(2,1,1))

        F20 = -ChainV0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F21 = ChainV1(Spinor(5,1,2),0,k(1),0,Spinor(2,1,1))

        F22 = ChainV1(Spinor(5,1,2),0,k(4),0,Spinor(2,1,1))

        F23 = ChainB1(Spinor(5,1,2),0,k(1),0,Spinor(2,1,1))

        F24 = ChainB0(Spinor(5,1,2),0,0,Spinor(2,1,1))

        F25 = ChainB1(Spinor(5,1,2),0,k(4),0,Spinor(2,1,1))

        F26 = ChainB0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F27 = ChainV1(Spinor(5,1,2),0,k(4),0,Spinor(1,1,1))

        F28 = ChainV1(Spinor(5,1,2),0,k(2),0,Spinor(1,1,1))

        F29 = ChainB0(Spinor(5,1,2),0,0,Spinor(1,1,1))

        F30 = ChainV0(Spinor(5,1,2),0,0,Spinor(1,1,1))

        F31 = ChainB1(Spinor(5,1,2),0,k(2),0,Spinor(1,1,1))

        F32 = ChainB1(Spinor(5,1,2),0,k(4),0,Spinor(1,1,1))

        F33 = ChainV0(Spinor(2,-1,2),0,1,Spinor(5,1,2))

        F34 = ChainB0(Spinor(6,-1,1),1,0,Spinor(3,-1,1))

        F35 = -ChainV0(Spinor(5,1,2),0,0,Spinor(6,-1,1))

        F36 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(3,-1,1))

        F37 = -ChainB1(Spinor(2,-1,2),0,k(1),1,Spinor(5,1,2))

        F38 = ChainV0(Spinor(6,-1,1),1,0,Spinor(3,-1,1))

        F39 = -ChainB1(Spinor(2,-1,2),0,k(4),1,Spinor(5,1,2))

        F40 = ChainV1(Spinor(5,1,2),0,k(2),0,Spinor(6,-1,1))

        F41 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(3,-1,1))

        F42 = ChainV1(Spinor(5,1,2),0,k(3),0,Spinor(6,-1,1))

        F43 = -ChainB0(Spinor(2,-1,2),0,1,Spinor(5,1,2))

        F44 = ChainB0(Spinor(5,1,2),0,0,Spinor(6,-1,1))

        F45 = ChainV1(Spinor(2,-1,2),0,k(1),1,Spinor(5,1,2))

        F46 = ChainV1(Spinor(2,-1,2),0,k(4),1,Spinor(5,1,2))

        F47 = -ChainB1(Spinor(5,1,2),0,k(2),0,Spinor(6,-1,1))

        F48 = -ChainB1(Spinor(5,1,2),0,k(3),0,Spinor(6,-1,1))

        F49 = ChainB0(Spinor(5,1,2),0,1,Spinor(6,1,2))

        F50 = -ChainV0(Spinor(1,1,1),1,0,Spinor(3,-1,1))

        F51 = ChainV1(Spinor(5,1,2),0,k(1),1,Spinor(6,1,2))

        F52 = ChainB0(Spinor(1,1,1),1,0,Spinor(3,-1,1))

        F53 = ChainB0(Spinor(6,1,2),0,0,Spinor(3,-1,1))

        F54 = ChainV1(Spinor(5,1,2),0,k(3),1,Spinor(6,1,2))

        F55 = ChainV0(Spinor(5,1,2),0,1,Spinor(6,1,2))

        F56 = -ChainV0(Spinor(6,1,2),0,0,Spinor(3,-1,1))

        F57 = ChainB1(Spinor(5,1,2),0,k(1),1,Spinor(6,1,2))

        F58 = ChainB1(Spinor(5,1,2),0,k(3),1,Spinor(6,1,2))

        F59 = ChainV1(Spinor(5,1,2),0,k(3),0,Spinor(1,1,1))

        F60 = ChainB0(Spinor(4,1,2),0,0,Spinor(3,-1,1))

        F61 = -ChainV0(Spinor(4,1,2),0,0,Spinor(3,-1,1))

        F62 = ChainB1(Spinor(5,1,2),0,k(3),0,Spinor(1,1,1))

        F63 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F64 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))

        F65 = -ChainB1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F66 = -ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(1,1,1))

        F67 = -ChainV0(Spinor(6,-1,1),1,0,Spinor(1,1,1))

        F68 = ChainV1(Spinor(2,-1,2),0,k(3),1,Spinor(5,1,2))

        F69 = ChainB0(Spinor(6,-1,1),1,0,Spinor(1,1,1))

        F70 = -ChainB1(Spinor(2,-1,2),0,k(3),1,Spinor(5,1,2))

        F71 = ChainV0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F72 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F73 = -ChainB1(Spinor(4,1,2),0,k(5),0,Spinor(3,-1,1))

        F74 = -ChainB1(Spinor(4,1,2),0,k(2),0,Spinor(3,-1,1))

        F75 = ChainV1(Spinor(4,1,2),0,k(1),0,Spinor(3,-1,1))

        F76 = ChainV1(Spinor(4,1,2),0,k(2),0,Spinor(3,-1,1))

        F77 = ChainB0(Spinor(4,1,2),0,0,Spinor(6,-1,1))

        F78 = ChainV1(Spinor(4,1,2),0,k(5),0,Spinor(3,-1,1))

        F79 = ChainV0(Spinor(5,1,2),0,1,Spinor(4,1,2))

        F80 = -ChainB1(Spinor(4,1,2),0,k(1),0,Spinor(3,-1,1))

        F81 = -ChainV0(Spinor(3,-1,1),1,0,Spinor(1,1,1))

        F82 = ChainV0(Spinor(2,-1,2),0,1,Spinor(4,1,2))

        F83 = -ChainV0(Spinor(4,1,2),0,0,Spinor(6,-1,1))

        F84 = ChainB0(Spinor(5,1,2),0,1,Spinor(4,1,2))

        F85 = -ChainB0(Spinor(2,-1,2),0,1,Spinor(4,1,2))

        F86 = ChainB0(Spinor(3,-1,1),1,0,Spinor(1,1,1))

        F87 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(6,-1,1))

        F88 = ChainB1(Spinor(2,-1,2),0,k(3),0,Spinor(6,-1,1))

        F89 = ChainV1(Spinor(5,1,2),0,k(1),0,Spinor(6,-1,1))

        F90 = ChainB0(Spinor(6,-1,1),1,1,Spinor(4,1,2))

        F91 = -ChainV0(Spinor(6,-1,1),1,1,Spinor(4,1,2))

        F92 = -ChainB1(Spinor(5,1,2),0,k(1),0,Spinor(6,-1,1))

        F93 = -ChainB1(Spinor(5,1,2),0,k(4),0,Spinor(3,-1,1))

        F94 = ChainV0(Spinor(4,1,2),0,1,Spinor(5,1,2))

        F95 = ChainB0(Spinor(4,1,2),0,1,Spinor(5,1,2))

        F96 = ChainB1(Spinor(5,1,2),0,k(3),1,Spinor(4,1,2))

        F97 = ChainV1(Spinor(5,1,2),0,k(4),0,Spinor(3,-1,1))

        F98 = ChainV1(Spinor(5,1,2),0,k(3),1,Spinor(4,1,2))

        F99 = ChainV1(Spinor(4,1,2),0,k(3),1,Spinor(5,1,2))

        F100 = ChainB1(Spinor(4,1,2),0,k(3),1,Spinor(5,1,2))

        F101 = ChainV0(Spinor(1,1,1),1,0,Spinor(2,1,1))

        F102 = ChainB1(Spinor(5,1,2),0,k(1),1,Spinor(4,1,2))

        F103 = ChainV1(Spinor(4,1,2),0,k(5),0,Spinor(1,1,1))

        F104 = ChainV0(Spinor(4,1,2),0,0,Spinor(2,1,1))

        F105 = ChainB0(Spinor(4,1,2),0,0,Spinor(2,1,1))

        F106 = ChainB0(Spinor(1,1,1),1,0,Spinor(2,1,1))

        F107 = ChainV1(Spinor(5,1,2),0,k(1),1,Spinor(4,1,2))

        F108 = ChainB1(Spinor(4,1,2),0,k(5),0,Spinor(1,1,1))

        F109 = -ChainB1(Spinor(4,1,2),0,k(1),0,Spinor(6,-1,1))

        F110 = -ChainB1(Spinor(4,1,2),0,k(2),0,Spinor(6,-1,1))

        F111 = ChainV1(Spinor(2,-1,2),0,k(5),1,Spinor(4,1,2))

        F112 = ChainV1(Spinor(2,-1,2),0,k(3),1,Spinor(4,1,2))

        F113 = ChainV1(Spinor(4,1,2),0,k(1),0,Spinor(6,-1,1))

        F114 = ChainV1(Spinor(4,1,2),0,k(2),0,Spinor(6,-1,1))

        F115 = -ChainB1(Spinor(2,-1,2),0,k(5),1,Spinor(4,1,2))

        F116 = -ChainB1(Spinor(2,-1,2),0,k(3),1,Spinor(4,1,2))

        Sub28 = F35*(F63 + F64) - F44*(F65 + F66) + 
     &    F67*(F46 + F68) - F69*(F39 + F70)

        Sub32 = F43*F67 + F33*F69 + F44*F71 + F35*F72

        Sub35 = (F4 - F6)*(F27 + F32 + F59 + F62) + Sub28

        Sub39 = F35*(F63 + F64) + F44*(F65 + F66) + 
     &    F67*(F46 + F68) + F69*(F39 + F70)

        Sub43 = (F4 + F6)*(F27 + F32 + F59 + F62) - Sub39

        Sub50 = (F13 - F15)*F2 + 
     &    (-F27 + F28 + F31 - F32)*F53 + F52*(F51 - F54)

        Sub53 = (F27 - F28 - F31 + F32)*F56 - 
     &    F50*(F57 - F58) - F16*F9 + F18*F9

        Sub58 = F101*F102 - F103*F19 + F104*(F28 + F31)

        Sub61 = F106*F107 - F108*F24 + F105*(F28 + F31)

        Sub68 = -((F43*F67 + F44*F71)*F75) - 
     &    (F32 + F62)*(F34*F82 + F36*F83) + 
     &    F2*(F1*F3 + F3*(F8 + F87 + F88)) + F34*F71*F96 + 
     &    F36*F67*F99

        Sub69 = (F43*F67 + F44*F71)*F80 - F1*F50*F84 + 
     &    F32*(F41*F77 + F38*F85) + F62*(F41*F77 + F38*F85) - 
     &    F50*F84*(F8 + F87 + F88) - (F71*F77 + F67*F85)*F93

        Sub71 = (F33*F69 + F35*F72)*F75 - F1*F52*F79 + 
     &    (F27 + F59)*(F34*F82 + F36*F83) - 
     &    F52*F79*(F8 + F87 + F88) - (F69*F82 + F72*F83)*F97

        Sub72 = F41*(F100*F69 - (F27 + F59)*F77) - 
     &    (F33*F69 + F35*F72)*F80 - F38*(F27 + F59)*F85 + 
     &    F10*(F1 + F8 + F87 + F88)*F9 + F38*F72*F98

        Sub77 = F34*(F45 - F46) - F36*(F47 - F48) + 
     &    F2*(F1 - F5 - F7 + F8)

        Sub78 = F38*(F37 - F39) + F41*(-F40 + F42) + 
     &    (F1 - F5 - F7 + F8)*F9

        Sub88 = (F115 + F116)*F67 + (F113 + F114)*F71

        Sub89 = (F111 + F112)*F69 + (F109 + F110)*F72

        Sub97 = -(1/SW2*
     &      (F33*F69 + F35*F72 - 2*SW2*(F43*F67 + F44*F71) + 
     &        2*CW2*(F33*F69 + F35*F72)))

        Sub99 = -(F40*F52) + F34*(F27 + F59)

        Sub100 = (F33*F69 + F35*F72)*F78 - 
     &    F36*(F27 + F59)*F83 + F52*(F82*F89 + (F39 + F70)*F91) - 
     &    F82*Sub99

        Sub101 = F38*(F39 + F70) + F41*(F40 + F89)

        Sub102 = (F33*F69 + F35*F72)*F73 - 
     &    (F27 + F59)*(F41*F77 + F38*F85) + F10*Sub101

        Sub104 = F34*(F46 + F68) + F36*(F47 + F92)

        Sub105 = (F43*F67 + F44*F71)*F78 - 
     &    (F32 + F62)*(F34*F82 + F36*F83) + F3*Sub104

        Sub106 = (F43*F67 + F44*F71)*F73 - 
     &    F41*(F32 + F62)*F77 - F32*F38*F85 + 
     &    (F47*F50 - F38*F62)*F85 + F50*((F46 + F68)*F90 + F85*F92)

        Sub109 = 1/SW2*
     &    (F33*F69 + F44*F71 - 4*CW2*(F33*F69 + F44*F71) + 
     &      4*SW2*(F43*F67 + F35*F72))

        Sub116 = F3*(F46 + F68) + (F63 + F64)*F79 - 
     &    (F27 + F32 + F59 + F62)*F82

        Sub117 = -((F33*F69 + F44*F71)*(F75 + F76 - F78)) + F34*Sub116

        Sub122 = F41*(F100*F69 - (F27 + F59)*F77 - 
     &       (F32 + F62)*F77) + F2*(F1*F3 + F3*F8) - 
     &    (F27 + F32 + F59 + F62)*(F34*F82 + F36*F83) + 
     &    (F1 + F8)*(F52*F79 + F50*F84) - 
     &    F38*(F27 + F32 + F59 + F62)*F85 + 
     &    (F2*F3 + F52*F79 + F50*F84)*(F87 + F88) + 
     &    F10*(F1 + F8 + F87 + F88)*F9 + (F71*F77 + F67*F85)*F93 + 
     &    F34*F71*F96 + (F69*F82 + F72*F83)*F97 + F38*F72*F98 + 
     &    F36*F67*F99 - (F75 + F80)*Sub32

        Sub123 = F2*(F71*F77 + F67*F85) + (F69*F82 + F72*F83)*F9

        Sub124 = F38*F72*F79 + F34*F71*F84 + F41*F69*F94 + F36*F67*F95

        Sub126 = F3*(F1*F2 - F34*(F46 + F68)) + 
     &    (F33*F69 + F44*F71)*F76 + 
     &    (F1*F52 - F34*(F63 + F64))*F79 + (F2*F3 + F52*F79)*F87

        Sub127 = -((F63 + F64)*F77) + F10*(F1 + F87)

        Sub128 = (F33*F69 + F44*F71)*F74 - 
     &    (F46 + F68)*F77*F81 + F50*F84*(F1 + F87) + F9*Sub127

        Sub130 = F2*(F65 + F66) + (F39 + F70)*F86

        Sub131 = (F43*F67 + F35*F72)*F76 + 
     &    (F2*F3 + F52*F79)*(F8 + F88) - F83*Sub130

        Sub132 = F10*(F39 + F70) + (F65 + F66)*F84

        Sub133 = -((F43*F67 + F35*F72)*F74) - 
     &    (F8 + F88)*(F50*F84 + F10*F9) + F38*Sub132

        Sub138 = F73 - F74 - F75 - F76 + F78

        Sub139 = (F46 + F68)*F81 + (F63 + F64)*F9

        Sub140 = -F80 + Sub138

        Sub143 = F3*(F1*F2 - F34*(F46 + F68)) - 
     &    F10*F38*(F39 + F70) + (F1*F52 - F34*(F63 + F64))*F79 + 
     &    (F2*F3 + F52*F79)*F8 - (F46 + F68)*F77*F81 - 
     &    F38*(F65 + F66)*F84 + (F2*F3 + F52*F79)*F87 + 
     &    F50*F84*(F1 + F87) + (F2*F3 + F52*F79)*F88 + 
     &    (F8 + F88)*(F50*F84 + F10*F9) + F9*Sub127 - F83*Sub130 + 
     &    (F74 + F76)*Sub32

        Sub145 = -(F47*F50) + F38*(F32 + F62)

        Sub146 = -(F41*(F32 + F62)*F77) - 
     &    F36*(F27 + F59)*F83 - (F32 + F62)*(F34*F82 + F36*F83) - 
     &    (F27 + F59)*(F41*F77 + F38*F85) + 
     &    F52*(F82*F89 + (F39 + F70)*F91) + 
     &    F50*((F46 + F68)*F90 + F85*F92) + F10*Sub101 + 
     &    F3*Sub104 - F85*Sub145 + (F73 + F78)*Sub32 - F82*Sub99

        Sub111 = F36*(F27 + F32 + F59 + F62) - Sub130

        Sub112 = (F43*F67 + F35*F72)*(F75 + F76 - F78) + F83*Sub111

        Sub113 = -((F27 + F32 + F59 + F62)*F85) + Sub132

        Sub114 = (F43*F67 + F35*F72)*(F73 - F74 - F80) + F38*Sub113

        Sub118 = F41*(F27 + F32 + F59 + F62) - Sub139

        Sub119 = (F33*F69 + F44*F71)*(F73 - F74 - F80) - F77*Sub118

        Sub141 = -(F41*(F27 + F32 + F59 + F62)*F77) - 
     &    F44*F71*F80 - F83*Sub111 + F38*Sub113 + F34*Sub116 - 
     &    F33*F69*(F80 - Sub138) + F44*F71*Sub138 + F77*Sub139 + 
     &    (F43*F67 + F35*F72)*Sub140

        Sub54(HelAll(Cha(3))) = Sub50 + (F49*F50 + F11*F9)*MCha(Cha(3))

        Sub55(HelAll(Cha(3))) = Sub53 - (F17*F2 + F52*F55)*MCha(Cha(3))

        Sub81(HelAll(Cha(3))) = Sub78 - (F33*F34 + F35*F36)*MCha(Cha(3))

        Sub82(HelAll(Cha(3))) = Sub77 - (F38*F43 + F41*F44)*MCha(Cha(3))


        Sub31(HelAll(Cha(3)),Cha(4)) = 
     &   F61*Sub29(Cha(3),Cha(4)) - F60*Sub30(Cha(3),Cha(4))

        Sub42(HelAll(Cha(3)),Cha(4)) = 
     &   F61*Sub40(Cha(3),Cha(4)) + F60*Sub41(Cha(3),Cha(4))

        Sub46(HelAll(Cha(3)),Cha(4)) = 
     &   F61*Sub44(Cha(3),Cha(4)) - F60*Sub45(Cha(3),Cha(4))

        Sub49(HelAll(Cha(3)),Cha(4)) = 
     &   F61*Sub47(Cha(3),Cha(4)) + F60*Sub48(Cha(3),Cha(4))

        Sub70(HelAll(Cha(3)),Cha(4)) = 
     &   Sub68*Sub66(Cha(3),Cha(4)) - Sub69*Sub67(Cha(3),Cha(4))

        Sub73(HelAll(Cha(3)),Cha(4)) = 
     &   Sub71*Sub66(Cha(3),Cha(4)) - Sub72*Sub67(Cha(3),Cha(4))

        Sub74(HelAll(Cha(3)),Cha(4)) = 
     &   1/SW2*(2*(CW2*((F69*F82 + F72*F83)*F9*MCha(Cha(3))) + 
     &         SW2*((F34*F71*F84 + F36*F67*F95)*MCha(Cha(4)))) + 
     &      (F69*F82 + F72*F83)*F9*MCha(Cha(3)))

        Sub75(HelAll(Cha(3)),Cha(4)) = 
     &   1/SW2*(2*SW2*(F2*(F71*F77 + F67*F85)*MCha(Cha(3))) + 
     &      (F38*F72*F79 + F41*F69*F94)*
     &       (2*CW2*MCha(Cha(4)) + MCha(Cha(4))))

        Sub76(HelAll(Cha(3)),Cha(4)) = 
     &   1/SW2*(2*CW2*Sub73(HelAll(Cha(3)),Cha(4)) + 
     &      SW2*(2*Sub70(HelAll(Cha(3)),Cha(4)) + 
     &         Sub66(Cha(3),Cha(4))*Sub74(HelAll(Cha(3)),Cha(4)) - 
     &         Sub67(Cha(3),Cha(4))*Sub75(HelAll(Cha(3)),Cha(4))) + 
     &      Sub73(HelAll(Cha(3)),Cha(4)))

        Sub95(HelAll(Cha(3)),Cha(4)) = 
     &   F61*Sub66(Cha(3),Cha(4)) + F60*Sub67(Cha(3),Cha(4))

        Sub96(HelAll(Cha(3)),Cha(4)) = 
     &   F60*Sub66(Cha(3),Cha(4)) + F61*Sub67(Cha(3),Cha(4))

        Sub98(HelAll(Cha(3)),Cha(4)) = 
     &   Sub97*(MCha(Cha(3))*Sub95(HelAll(Cha(3)),Cha(4)) - 
     &      MCha(Cha(4))*Sub96(HelAll(Cha(3)),Cha(4)))

        Sub103(HelAll(Cha(3)),Cha(4)) = 
     &   Sub100*Sub66(Cha(3),Cha(4)) + Sub102*Sub67(Cha(3),Cha(4))

        Sub107(HelAll(Cha(3)),Cha(4)) = 
     &   Sub105*Sub66(Cha(3),Cha(4)) + Sub106*Sub67(Cha(3),Cha(4))

        Sub108(HelAll(Cha(3)),Cha(4)) = 
     &   1/SW2*(2*CW2*Sub103(HelAll(Cha(3)),Cha(4)) - 
     &      SW2*(2*Sub107(HelAll(Cha(3)),Cha(4)) - 
     &         Sub98(HelAll(Cha(3)),Cha(4))) + 
     &      Sub103(HelAll(Cha(3)),Cha(4)))

        Sub110(HelAll(Cha(3)),Cha(4)) = 
     &   Sub109*(MCha(Cha(3))*Sub95(HelAll(Cha(3)),Cha(4)) - 
     &      MCha(Cha(4))*Sub96(HelAll(Cha(3)),Cha(4)))

        Sub125(HelAll(Cha(3)),Cha(4)) = 
     &   Sub122 - Sub123*MCha(Cha(3)) + Sub124*MCha(Cha(4))

        Sub129(HelAll(Cha(3)),Cha(4)) = 
     &   Sub126*Sub66(Cha(3),Cha(4)) + Sub128*Sub67(Cha(3),Cha(4))

        Sub134(HelAll(Cha(3)),Cha(4)) = 
     &   Sub131*Sub66(Cha(3),Cha(4)) - Sub133*Sub67(Cha(3),Cha(4))

        Sub135(HelAll(Cha(3)),Cha(4)) = 
     &   Sub129(HelAll(Cha(3)),Cha(4)) + 
     &    (F33*F69 + F44*F71)*MCha(Cha(3))*Sub95(HelAll(Cha(3)),Cha(4))

        Sub136(HelAll(Cha(3)),Cha(4)) = 
     &   Sub134(HelAll(Cha(3)),Cha(4)) + 
     &    (F43*F67 + F35*F72)*MCha(Cha(3))*Sub95(HelAll(Cha(3)),Cha(4))

        Sub137(HelAll(Cha(3)),Cha(4)) = 
     &   1/SW2*(-(4*CW2*Sub135(HelAll(Cha(3)),Cha(4))) + 
     &      SW2*(4*Sub136(HelAll(Cha(3)),Cha(4)) - 
     &         Sub109*MCha(Cha(4))*Sub96(HelAll(Cha(3)),Cha(4))) + 
     &      Sub135(HelAll(Cha(3)),Cha(4)))

        Sub144(HelAll(Cha(3)),Cha(4)) = 
     &   Sub143 + (F60 + F61)*Sub32*(MCha(Cha(3)) - MCha(Cha(4)))

        Sub147(HelAll(Cha(3)),Cha(4)) = 
     &   Sub146 - (F60 + F61)*Sub32*(MCha(Cha(3)) - MCha(Cha(4)))

        Sub169(HelAll(Cha(3)),Cha(4)) = 
     &   Sub31(HelAll(Cha(3)),Cha(4))/
     &     (S34 + GaugeXi(2)*(-MZ2 + cI*MZ*WZ)) + 
     &    (2*CB2*Sub46(HelAll(Cha(3)),Cha(4)))/(S2B*(MA02C - S34))

        Sub171(HelAll(Cha(3)),Cha(4)) = 
     &   -((SA*Sub42(HelAll(Cha(3)),Cha(4)))/(MHH2C - S34)) + 
     &    (CA*Sub49(HelAll(Cha(3)),Cha(4)))/(Mh02C - S34)

        Sub115(HelAll(Cha(3)),Cha(4)) = 
     &   Sub112*Sub66(Cha(3),Cha(4)) - Sub114*Sub67(Cha(3),Cha(4))

        Sub120(HelAll(Cha(3)),Cha(4)) = 
     &   Sub117*Sub66(Cha(3),Cha(4)) + Sub119*Sub67(Cha(3),Cha(4))

        Sub121(HelAll(Cha(3)),Cha(4)) = 
     &   1/SW2*(SW2*(Sub110(HelAll(Cha(3)),Cha(4)) + 
     &         4*Sub115(HelAll(Cha(3)),Cha(4))) + 
     &      (-1 + 4*CW2)*Sub120(HelAll(Cha(3)),Cha(4)))

        Sub142(HelAll(Cha(3)),Cha(4)) = 
     &   Sub141 - (F60 + F61)*Sub32*(MCha(Cha(3)) - MCha(Cha(4)))


        LOOP(Sfe7, 1,2,1)

        Sub62(HelAll(Cha(3)),Gen(6),Sfe7) = 
     &   (2*(1/2.D0*(F26*S2B*Sub59(Cha(3),Gen(6),Sfe7)) + 
     &        Sqrt(2.D0)*(CB2*F20*Mf(3,Gen(6))*USf(Sfe7,1,4,Gen(6))*
     &           VCha(Cha(3),2))))/(CB2*S2B)

        Sub63(HelAll(Cha(3)),Gen(6),Sfe7) = 
     &   (2*(Sqrt(2.D0)*(F26*SB2*Sub59(Cha(3),Gen(6),Sfe7)) + 
     &        F20*S2B*Mf(3,Gen(6))*USf(Sfe7,1,4,Gen(6))*VCha(Cha(3),2)))/
     &    (S2B*SB2)

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub85(HelAll(Cha(4)),Gen(1),Sfe7) = 
     &   (2*(1/2.D0*(F10*S2B*Sub80(Cha(4),Gen(1),Sfe7)) + 
     &        Sqrt(2.D0)*(F3*SB2*Mf(4,Gen(1))*UCha(Cha(4),2)*
     &           USf(Sfe7,1,3,Gen(1)))))/(S2B*SB2)

        Sub86(HelAll(Cha(4)),Gen(1),Sfe7) = 
     &   (2*(Sqrt(2.D0)*(CB2*F10*Sub80(Cha(4),Gen(1),Sfe7)) + 
     &        F3*S2B*Mf(4,Gen(1))*UCha(Cha(4),2)*USf(Sfe7,1,3,Gen(1))))/
     &    (CB2*S2B)

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub4(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(1/2.D0*(F10*S2B*Sub3(Cha(4),Gen(1),Sfe8)) + 
     &        Sqrt(2.D0)*(F3*SB2*Mf(4,Gen(1))*UCha(Cha(4),2)*
     &           USf(Sfe8,1,3,Gen(1)))))/(S2B*SB2)

        Sub5(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(Sqrt(2.D0)*(CB2*F10*Sub3(Cha(4),Gen(1),Sfe8)) + 
     &        F3*S2B*Mf(4,Gen(1))*UCha(Cha(4),2)*USf(Sfe8,1,3,Gen(1))))/
     &    (CB2*S2B)

        Sub18(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(1/Sqrt(2.D0)*(F3*S2B*Mf(4,Gen(1))*UCha(Cha(4),2)*
     &           USf(Sfe8,1,3,Gen(1))) + CB2*F10*Sub3(Cha(4),Gen(1),Sfe8)
     &        ))/(CB2*S2B)

        Sub20(HelAll(Cha(4)),Gen(1),Sfe8) = 
     &   (2*(1/Sqrt(2.D0)*(F10*S2B*Sub3(Cha(4),Gen(1),Sfe8)) + 
     &        2*F3*SB2*Mf(4,Gen(1))*UCha(Cha(4),2)*USf(Sfe8,1,3,Gen(1))))
     &     /(S2B*SB2)

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub91(HelAll(Cha(4)),Gen(1),Gen(5),Sfe7) = 
     &   Sqrt(2.D0)*((Sub88 + F67*F85*MCha(Cha(4)))*Mf(4,Gen(1))*
     &        UCha(Cha(4),2)*USf(Sfe7,1,3,Gen(5)))/(CB*SB) + 
     &    ((Sub89 + F69*F82*MCha(Cha(4)))*Sub90(Cha(4),Gen(5),Sfe7))/SB2

        Sub92(HelAll(Cha(4)),Gen(1),Gen(5),Sfe7) = 
     &   Sqrt(2.D0)*((Sub89 + F69*F82*MCha(Cha(4)))*
     &        Sub90(Cha(4),Gen(5),Sfe7))/(CB*SB) + 
     &    (2*(Sub88 + F67*F85*MCha(Cha(4)))*Mf(4,Gen(1))*UCha(Cha(4),2)*
     &       USf(Sfe7,1,3,Gen(5)))/CB2

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub24(HelAll(Gen(1)),Gen(5),Sfe7,Sfe8) = 
     &   (F27 - F28)*USf(Sfe8,1,4,Gen(1))*USfC(Sfe7,1,4,Gen(5)) + 
     &    (-F31 + F32)*USf(Sfe8,2,4,Gen(1))*USfC(Sfe7,2,4,Gen(5))

        Sub25(HelAll(Gen(1)),Gen(5),Sfe7,Sfe8) = 
     &   Sub24(HelAll(Gen(1)),Gen(5),Sfe7,Sfe8) + 
     &    F30*Mino3*USf(Sfe8,2,4,Gen(1))*USfC(Sfe7,1,4,Gen(5)) + 
     &    F29*Mino3C*USf(Sfe8,1,4,Gen(1))*USfC(Sfe7,2,4,Gen(5))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub11(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8) = 
     &   (F16 - F18)*USf(Sfe7,2,3,Gen(6))*USf(Sfe8,1,4,Gen(1)) + 
     &    (F13 - F15)*USf(Sfe7,1,3,Gen(6))*USf(Sfe8,2,4,Gen(1))

        Sub12(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8) = 
     &   Sub11(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8) + 
     &    F11*Mino3C*USf(Sfe7,1,3,Gen(6))*USf(Sfe8,1,4,Gen(1)) + 
     &    F17*Mino3*USf(Sfe7,2,3,Gen(6))*USf(Sfe8,2,4,Gen(1))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub17(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) = 
     &   (F23 - F25)*USfC(Sfe7,2,4,Gen(5))*USfC(Sfe8,1,3,Gen(2)) + 
     &    (F21 - F22)*USfC(Sfe7,1,4,Gen(5))*USfC(Sfe8,2,3,Gen(2))

        Sub21(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) = 
     &   F19*Mino3*USfC(Sfe7,1,4,Gen(5))*USfC(Sfe8,1,3,Gen(2)) + 
     &    F24*Mino3C*USfC(Sfe7,2,4,Gen(5))*USfC(Sfe8,2,3,Gen(2))

        Sub19(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) = 
     &   Sub17(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) - 
     &    Sub21(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8)

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub1(HelAll(Gen(2)),Gen(6),Sfe7,Sfe8) = 
     &   (F1 - F5)*USf(Sfe7,1,3,Gen(6))*USfC(Sfe8,1,3,Gen(2)) + 
     &    (-F7 + F8)*USf(Sfe7,2,3,Gen(6))*USfC(Sfe8,2,3,Gen(2))

        Sub8(HelAll(Gen(2)),Gen(6),Sfe7,Sfe8) = 
     &   F6*Mino3*USf(Sfe7,2,3,Gen(6))*USfC(Sfe8,1,3,Gen(2)) + 
     &    F4*Mino3C*USf(Sfe7,1,3,Gen(6))*USfC(Sfe8,2,3,Gen(2))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub83(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7) = 
     &   F3*Mf(4,Gen(1))*Sub79(Cha(3),Gen(1),Sfe7)*Sub81(HelAll(Cha(3)))*
     &     UCha(Cha(4),2)*USf(Sfe7,1,3,Gen(1)) + 
     &    F10*Mf(4,Gen(5))*Sub80(Cha(4),Gen(1),Sfe7)*Sub82(HelAll(Cha(3)))*
     &     UChaC(Cha(3),2)*USfC(Sfe7,1,3,Gen(1))

        Sub84(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7) = 
     &   (8*(1/2.D0*(1/Sqrt(2.D0)*
     &            (S2B**2*Sub83(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7))+
     &             CB2*F10*S2B*Sub79(Cha(3),Gen(1),Sfe7)*
     &            Sub80(Cha(4),Gen(1),Sfe7)*Sub81(HelAll(Cha(3)))) + 
     &        F3*S2B*SB2*Mf(4,Gen(1))*Mf(4,Gen(5))*Sub82(HelAll(Cha(3)))*
     &         UCha(Cha(4),2)*UChaC(Cha(3),2)*USf(Sfe7,1,3,Gen(1))*
     &         USfC(Sfe7,1,3,Gen(1))))/S2B**3

        Sub87(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7) = 
     &   F9*Sub2(Cha(3),Gen(5),Sfe7)*Sub85(HelAll(Cha(4)),Gen(1),Sfe7) + 
     &    F2*Mf(4,Gen(5))*Sub86(HelAll(Cha(4)),Gen(1),Sfe7)*
     &     UChaC(Cha(3),2)*USfC(Sfe7,1,3,Gen(5))

        Sub93(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7) = 
     &   F9*Sub2(Cha(3),Gen(5),Sfe7)*
     &     Sub91(HelAll(Cha(4)),Gen(1),Gen(5),Sfe7) + 
     &    F2*Mf(4,Gen(5))*Sub92(HelAll(Cha(4)),Gen(1),Gen(5),Sfe7)*
     &     UChaC(Cha(3),2)*USfC(Sfe7,1,3,Gen(5))

        Sub183(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7) = 
     &   -(Sub84(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7)/
     &       (Mf2(4,Gen(5)) - Sub159(Cha(3),Cha(4),Gen(1)))) + 
     &    ((F1 - F5 - F7 + F8)*
     &       Sub87(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7))/
     &     (S35 - Sub148(Gen(5),Sfe7))

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub56(HelAll(Cha(3)),Cha(4),Gen(2),Gen(6),Sfe7) = 
     &   F12*Mf(3,Gen(6))*Sub52(Cha(4),Gen(2),Sfe7)*Sub55(HelAll(Cha(3)))*
     &     USf(Sfe7,1,4,Gen(2))*VCha(Cha(3),2) - 
     &    F14*Mf(3,Gen(2))*Sub51(Cha(3),Gen(2),Sfe7)*Sub54(HelAll(Cha(3)))*
     &     USfC(Sfe7,1,4,Gen(2))*VChaC(Cha(4),2)

        Sub57(HelAll(Cha(3)),Cha(4),Gen(2),Gen(6),Sfe7) = 
     &   (8*(-(1/(2.D0*Sqrt(2.D0))*
     &           (S2B**2*Sub56(HelAll(Cha(3)),Cha(4),Gen(2),Gen(6),Sfe7)))+
     &          S2B*(1/2.D0*
     &            (F12*SB2*Sub51(Cha(3),Gen(2),Sfe7)*
     &              Sub52(Cha(4),Gen(2),Sfe7)*Sub54(HelAll(Cha(3)))) - 
     &           CB2*F14*Mf(3,Gen(2))*Mf(3,Gen(6))*Sub55(HelAll(Cha(3)))*
     &            USf(Sfe7,1,4,Gen(2))*USfC(Sfe7,1,4,Gen(2))*
     &            VCha(Cha(3),2)*VChaC(Cha(4),2))))/S2B**3

        Sub64(HelAll(Cha(3)),Cha(4),Gen(2),Gen(6),Sfe7) = 
     &   Sub58*Sub60(Cha(4),Gen(6),Sfe7)*
     &     Sub62(HelAll(Cha(3)),Gen(6),Sfe7) + 
     &    Sub61*Mf(3,Gen(2))*Sub63(HelAll(Cha(3)),Gen(6),Sfe7)*
     &     USfC(Sfe7,1,4,Gen(6))*VChaC(Cha(4),2)

        Sub65(HelAll(Cha(3)),Cha(4),Gen(2),Gen(6),Sfe7) = 
     &   F12*Sub52(Cha(4),Gen(2),Sfe7)*Sub62(HelAll(Cha(3)),Gen(6),Sfe7) + 
     &    F14*Mf(3,Gen(2))*Sub63(HelAll(Cha(3)),Gen(6),Sfe7)*
     &     USfC(Sfe7,1,4,Gen(2))*VChaC(Cha(4),2)

        ENDLOOP(Sfe7)


        Sub166(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   Sub147(HelAll(Cha(3)),Cha(4))/
     &     (Mf2(4,Gen(1)) + Sub158(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))) - 
     &    Sub125(HelAll(Cha(3)),Cha(4))/
     &     (Mf2(4,Gen(1)) - Sub159(Cha(3),Cha(4),Gen(1)))

        Sub168(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -((Sub35*Mf(3,Gen(6)))/
     &       (Mf2(3,Gen(2)) - Sub161(Gen(1),Gen(2),Gen(5)))) - 
     &    (Sub28*Mf(3,Gen(2)))/
     &     (Mf2(3,Gen(2)) + Sub162(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))

        Sub170(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   (Sub43*Mf(3,Gen(6)))/
     &     (Mf2(3,Gen(2)) - Sub161(Gen(1),Gen(2),Gen(5))) - 
     &    (Sub39*Mf(3,Gen(2)))/
     &     (Mf2(3,Gen(2)) + Sub162(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))

        Sub172(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   (SB*Sub168(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       Sub169(HelAll(Cha(3)),Cha(4)) - 
     &      Sub170(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       Sub171(HelAll(Cha(3)),Cha(4)))/SB

        Sub175(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   (Sub110(HelAll(Cha(3)),Cha(4))*
     &       Sub157(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       Sub163(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) - 
     &      T15*Sub160(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       Sub98(HelAll(Cha(3)),Cha(4)))/
     &    (T15*Sub157(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))

        Sub176(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   (2*IndexDelta(Gen(2),Gen(6))*Sub76(HelAll(Cha(3)),Cha(4)))/
     &     (Sub157(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       (Mf2(4,Gen(1)) - Sub159(Cha(3),Cha(4),Gen(1)))) - 
     &    (Sub175(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       Sub94(Gen(2),Gen(6)))/(S34 + GaugeXi(2)*(-MZ2 + cI*MZ*WZ))

        Sub165(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   Sub108(HelAll(Cha(3)),Cha(4))/
     &     (Sub157(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &       (Mf2(4,Gen(1)) + Sub158(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))))
     &      + Sub121(HelAll(Cha(3)),Cha(4))/
     &     (T15*(Mf2(3,Gen(2)) - Sub161(Gen(1),Gen(2),Gen(5))))

        Sub167(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -(Sub142(HelAll(Cha(3)),Cha(4))/
     &       (Mf2(3,Gen(2)) - Sub161(Gen(1),Gen(2),Gen(5)))) + 
     &    Sub144(HelAll(Cha(3)),Cha(4))/
     &     (Mf2(3,Gen(2)) + Sub162(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))

        Sub173(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   2/CW2*Sub137(HelAll(Cha(3)),Cha(4))/
     &      ((MZ2 - S34 - cI*MZ*WZ)*
     &        (Mf2(3,Gen(2)) + Sub162(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))
     &        ) + (3*Sqrt(2.D0))/(MW*SW2)*
     &     Sub172(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) - 
     &    (16*IndexDelta(Cha(3),Cha(4))*
     &       Sub167(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))/S34

        Sub174(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -(CW2*((8*IndexDelta(Cha(3),Cha(4))*
     &            Sub166(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))/
     &          (S34*Sub157(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))) + 
     &         Sub173(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))/T15))-
     &      (2*Sub165(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)))/
     &     (MZ2 - S34 - cI*MZ*WZ)

        Sub177(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) = 
     &   -(4*CW2*((F60 + F61)*Sub32*IndexDelta(Cha(3),Cha(4))*
     &          (MCha(Cha(3)) - MCha(Cha(4)))*
     &          Sub164(Cha(3),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))*
     &          Sub33(Gen(2),Gen(6)))/S34**2) + 
     &    IndexDelta(Gen(2),Gen(6))*
     &     Sub174(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6)) - 
     &    Sub176(HelAll(Cha(3)),Cha(4),Gen(1),Gen(2),Gen(5),Gen(6))/
     &     (MZ2 - S34 - cI*MZ*WZ)


        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub6(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7,Sfe8) = 
     &   F9*Sub2(Cha(3),Gen(5),Sfe7)*Sub4(HelAll(Cha(4)),Gen(1),Sfe8) + 
     &    F2*Mf(4,Gen(5))*Sub5(HelAll(Cha(4)),Gen(1),Sfe8)*UChaC(Cha(3),2)*
     &     USfC(Sfe7,1,3,Gen(5))

        Sub7(HelAll(Cha(3)),Cha(4),Gen(1),Gen(5),Sfe7,Sfe8) = 
     &   (2*(CB2*(1/2.D0*(F9*S2B*Sub2(Cha(3),Gen(5),Sfe7)*
     &              Sub4(HelAll(Cha(4)),Gen(1),Sfe8)) + 
     &           Sqrt(2.D0)*(F10*F2*Mf(4,Gen(5))*Sub3(Cha(4),Gen(1),Sfe8)*
     &              UChaC(Cha(3),2)*USfC(Sfe7,1,3,Gen(5)))) + 
     &        F2*F3*S2B*Mf(4,Gen(1))*Mf(4,Gen(5))*UCha(Cha(4),2)*
     &         UChaC(Cha(3),2)*USf(Sfe8,1,3,Gen(1))*USfC(Sfe7,1,3,Gen(5))
     &        ))/(CB2*S2B)

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub22(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Sfe7,Sfe8) = 
     &   Sub20(HelAll(Cha(4)),Gen(1),Sfe8)*
     &    (Sub17(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) - 
     &      Sub21(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

#include "real_contains.h"

        end

