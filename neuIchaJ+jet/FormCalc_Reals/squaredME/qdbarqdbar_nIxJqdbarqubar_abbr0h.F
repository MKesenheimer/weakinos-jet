#if 0
* qdbarqdbar_nIxJqdbarqubar_abbr0h.F
* abbreviations for {-F[4, {Gen(1), Col1}], -F[4, {Gen(2), Col2}]} -> {F[11, {Neu(3)}], -F[12, {Cha(4)}], -F[4, {Gen(5), Col5}], -F[3, {Gen(6), Col6}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:56
#endif

#include "qdbarqdbar_nIxJqdbarqubar_vars.h"
#include "real_inline.h"
#include "real_contains.h"


        subroutine qdbarqdbar_nIxJqdbarqubar_abbr0h
        implicit none

#include "qdbarqdbar_nIxJqdbarqubar_vars.h"
#include "real_inline.h"

        F1 = ChainV0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F2 = ChainB0(Spinor(3,1,2),0,0,Spinor(5,-1,1))

        F3 = -ChainV0(Spinor(4,1,2),0,0,Spinor(6,-1,1))

        F4 = -ChainV0(Spinor(3,1,2),0,0,Spinor(5,-1,1))

        F5 = -ChainB1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F6 = -ChainB1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F7 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(1,1,1))

        F8 = ChainV1(Spinor(2,-1,2),0,k(5),0,Spinor(1,1,1))

        F9 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(1,1,1))

        F10 = ChainB0(Spinor(4,1,2),0,0,Spinor(6,-1,1))

        F11 = ChainV1(Spinor(6,1,2),0,k(3),0,Spinor(5,-1,1))

        F12 = -ChainB1(Spinor(6,1,2),0,k(1),0,Spinor(5,-1,1))

        F13 = ChainB0(Spinor(6,1,2),0,0,Spinor(5,-1,1))

        F14 = ChainV1(Spinor(6,1,2),0,k(1),0,Spinor(5,-1,1))

        F15 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(3,-1,1))

        F16 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F17 = -ChainB1(Spinor(6,1,2),0,k(3),0,Spinor(5,-1,1))

        F18 = -ChainV0(Spinor(6,1,2),0,0,Spinor(5,-1,1))

        F19 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(3,-1,1))

        F20 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(4,-1,1))

        F21 = ChainV1(Spinor(2,-1,2),0,k(1),0,Spinor(5,-1,1))

        F22 = ChainB1(Spinor(2,-1,2),0,k(1),0,Spinor(5,-1,1))

        F23 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(5,-1,1))

        F24 = ChainB1(Spinor(2,-1,2),0,k(3),0,Spinor(5,-1,1))

        F25 = ChainV0(Spinor(2,-1,2),0,1,Spinor(6,1,2))

        F26 = ChainB0(Spinor(5,-1,1),1,0,Spinor(4,-1,1))

        F27 = -ChainV0(Spinor(6,1,2),0,0,Spinor(4,-1,1))

        F28 = -ChainB1(Spinor(2,-1,2),0,k(1),1,Spinor(6,1,2))

        F29 = ChainV0(Spinor(5,-1,1),1,0,Spinor(4,-1,1))

        F30 = -ChainB1(Spinor(2,-1,2),0,k(3),1,Spinor(6,1,2))

        F31 = -ChainB0(Spinor(2,-1,2),0,1,Spinor(6,1,2))

        F32 = ChainB0(Spinor(6,1,2),0,0,Spinor(4,-1,1))

        F33 = ChainV1(Spinor(2,-1,2),0,k(1),1,Spinor(6,1,2))

        F34 = ChainV1(Spinor(2,-1,2),0,k(3),1,Spinor(6,1,2))

        F35 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(5,-1,1))

        F36 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(5,-1,1))

        F37 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(4,-1,1))

        F38 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(3,-1,1))

        F39 = -ChainB1(Spinor(6,1,2),0,k(2),0,Spinor(5,-1,1))

        F40 = ChainV1(Spinor(6,1,2),0,k(2),0,Spinor(5,-1,1))

        F41 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(4,-1,1))

        F42 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(3,-1,1))

        F43 = ChainV1(Spinor(1,-1,2),0,k(2),0,Spinor(5,-1,1))

        F44 = ChainV1(Spinor(1,-1,2),0,k(3),0,Spinor(5,-1,1))

        F45 = ChainB1(Spinor(1,-1,2),0,k(2),0,Spinor(5,-1,1))

        F46 = ChainB1(Spinor(1,-1,2),0,k(3),0,Spinor(5,-1,1))

        F47 = ChainV0(Spinor(1,-1,2),0,1,Spinor(6,1,2))

        F48 = -ChainB1(Spinor(1,-1,2),0,k(2),1,Spinor(6,1,2))

        F49 = -ChainB1(Spinor(1,-1,2),0,k(3),1,Spinor(6,1,2))

        F50 = -ChainB0(Spinor(1,-1,2),0,1,Spinor(6,1,2))

        F51 = ChainV1(Spinor(1,-1,2),0,k(2),1,Spinor(6,1,2))

        F52 = ChainV1(Spinor(1,-1,2),0,k(3),1,Spinor(6,1,2))

        F53 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(5,-1,1))

        F54 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(5,-1,1))

        F55 = ChainB0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F56 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(5,-1,1))

        F57 = ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(5,-1,1))

        F58 = -ChainV0(Spinor(3,1,2),0,0,Spinor(6,-1,1))

        F59 = -ChainB0(Spinor(2,-1,2),0,0,Spinor(6,-1,1))

        F60 = ChainB0(Spinor(5,-1,1),1,0,Spinor(6,-1,1))

        F61 = ChainV0(Spinor(2,-1,2),0,1,Spinor(3,1,2))

        F62 = -ChainB1(Spinor(3,1,2),0,k(1),0,Spinor(5,-1,1))

        F63 = -ChainB1(Spinor(3,1,2),0,k(4),0,Spinor(5,-1,1))

        F64 = ChainV1(Spinor(2,-1,2),0,k(1),1,Spinor(3,1,2))

        F65 = ChainV1(Spinor(2,-1,2),0,k(4),1,Spinor(3,1,2))

        F66 = ChainV0(Spinor(5,-1,1),1,0,Spinor(6,-1,1))

        F67 = -ChainB0(Spinor(2,-1,2),0,1,Spinor(3,1,2))

        F68 = -ChainV0(Spinor(2,-1,2),0,0,Spinor(6,-1,1))

        F69 = ChainV1(Spinor(3,1,2),0,k(1),0,Spinor(5,-1,1))

        F70 = ChainV1(Spinor(3,1,2),0,k(4),0,Spinor(5,-1,1))

        F71 = -ChainB1(Spinor(2,-1,2),0,k(4),1,Spinor(3,1,2))

        F72 = -ChainB1(Spinor(2,-1,2),0,k(1),1,Spinor(3,1,2))

        F73 = ChainV1(Spinor(2,-1,2),0,k(1),0,Spinor(6,-1,1))

        F74 = ChainV1(Spinor(2,-1,2),0,k(4),0,Spinor(6,-1,1))

        F75 = ChainB1(Spinor(2,-1,2),0,k(1),0,Spinor(6,-1,1))

        F76 = ChainB1(Spinor(2,-1,2),0,k(4),0,Spinor(6,-1,1))

        F77 = ChainV1(Spinor(1,-1,2),0,k(4),0,Spinor(5,-1,1))

        F78 = ChainB1(Spinor(1,-1,2),0,k(4),0,Spinor(5,-1,1))

        F79 = -ChainB0(Spinor(1,-1,2),0,0,Spinor(6,-1,1))

        F80 = ChainV0(Spinor(1,-1,2),0,1,Spinor(3,1,2))

        F81 = -ChainB1(Spinor(3,1,2),0,k(2),0,Spinor(5,-1,1))

        F82 = ChainV1(Spinor(1,-1,2),0,k(2),1,Spinor(3,1,2))

        F83 = -ChainV0(Spinor(1,-1,2),0,0,Spinor(6,-1,1))

        F84 = -ChainB0(Spinor(1,-1,2),0,1,Spinor(3,1,2))

        F85 = ChainV1(Spinor(3,1,2),0,k(2),0,Spinor(5,-1,1))

        F86 = ChainV1(Spinor(1,-1,2),0,k(4),1,Spinor(3,1,2))

        F87 = -ChainB1(Spinor(1,-1,2),0,k(2),1,Spinor(3,1,2))

        F88 = -ChainB1(Spinor(1,-1,2),0,k(4),1,Spinor(3,1,2))

        F89 = ChainV1(Spinor(1,-1,2),0,k(2),0,Spinor(6,-1,1))

        F90 = ChainV1(Spinor(1,-1,2),0,k(4),0,Spinor(6,-1,1))

        F91 = ChainB1(Spinor(1,-1,2),0,k(4),0,Spinor(6,-1,1))

        F92 = ChainB1(Spinor(1,-1,2),0,k(2),0,Spinor(6,-1,1))

        F93 = -ChainV0(Spinor(3,1,2),0,0,Spinor(4,-1,1))

        F94 = ChainB0(Spinor(3,1,2),0,0,Spinor(4,-1,1))

        F95 = -ChainB1(Spinor(1,-1,2),0,k(3),1,Spinor(2,-1,2))

        F96 = -ChainB1(Spinor(1,-1,2),0,k(4),1,Spinor(2,-1,2))

        F97 = -ChainV1(Spinor(1,-1,2),0,k(3),1,Spinor(2,-1,2))

        F98 = -ChainV1(Spinor(1,-1,2),0,k(4),1,Spinor(2,-1,2))

        F99 = ChainV0(Spinor(1,-1,2),0,1,Spinor(2,-1,2))

        F100 = ChainV1(Spinor(3,1,2),0,k(1),0,Spinor(4,-1,1))

        F101 = ChainV1(Spinor(3,1,2),0,k(2),0,Spinor(4,-1,1))

        F102 = ChainB0(Spinor(6,-1,1),1,0,Spinor(4,-1,1))

        F103 = ChainV1(Spinor(3,1,2),0,k(5),0,Spinor(4,-1,1))

        F104 = -ChainV0(Spinor(5,-1,1),1,1,Spinor(3,1,2))

        F105 = -ChainB1(Spinor(3,1,2),0,k(1),0,Spinor(4,-1,1))

        F106 = -ChainB1(Spinor(3,1,2),0,k(2),0,Spinor(4,-1,1))

        F107 = -ChainB1(Spinor(3,1,2),0,k(5),0,Spinor(4,-1,1))

        F108 = ChainV1(Spinor(2,-1,2),0,k(3),0,Spinor(6,-1,1))

        F109 = ChainV0(Spinor(6,-1,1),1,0,Spinor(5,-1,1))

        F110 = ChainB0(Spinor(6,-1,1),1,0,Spinor(5,-1,1))

        F111 = ChainV0(Spinor(4,-1,1),1,0,Spinor(5,-1,1))

        F112 = ChainV0(Spinor(3,1,2),0,1,Spinor(2,-1,2))

        F113 = ChainV1(Spinor(1,-1,2),0,k(3),0,Spinor(4,-1,1))

        F114 = -ChainV1(Spinor(1,-1,2),0,k(5),1,Spinor(2,-1,2))

        F115 = -ChainB1(Spinor(1,-1,2),0,k(5),1,Spinor(2,-1,2))

        F116 = ChainV0(Spinor(5,-1,1),1,0,Spinor(3,-1,1))

        F117 = ChainB0(Spinor(5,-1,1),1,0,Spinor(3,-1,1))

        F118 = ChainB0(Spinor(4,-1,1),1,0,Spinor(5,-1,1))

        F119 = ChainV0(Spinor(3,-1,1),1,0,Spinor(5,-1,1))

        F120 = ChainB0(Spinor(3,-1,1),1,0,Spinor(5,-1,1))

        Sub22 = F61*(F44 + F46 + F77 + F78) - 
     &    (F23 + F56)*F80 - F104*(F95 + F96)

        Sub23 = (F100 + F101 - F103)*(F36*F79 + F60*F99) + F102*Sub22

        Sub24 = -(F41*(F23 + F56)) + 
     &    F20*(F44 + F46 + F77 + F78) - F29*(F95 + F96)

        Sub25 = (F105 + F106 - F107)*(F36*F79 + F60*F99) + F55*Sub24

        Sub28 = F61*(F44 + F77) - 
     &    (F23 + F24 + F56 + F57)*F80 + F4*(F97 + F98)

        Sub29 = (F100 + F101 - F103)*(F54*F59 + F110*F99) - F102*Sub28

        Sub30 = -(F41*(F23 + F24 + F56 + F57)) + 
     &    F20*(F44 + F77) + F111*(F97 + F98)

        Sub31 = (F105 + F106 - F107)*(F54*F59 + F110*F99) - F55*Sub30

        Sub35 = -(F108*F26) + F102*(F23 + F56) - F26*F74

        Sub36 = F37*F4*(F108 + F74) - F102*F104*(F95 + F96) + 
     &    F101*(F36*F79 + F60*F99) - F80*Sub35

        Sub37 = -(F55*(F23 + F56)) + F2*(F108 + F74)

        Sub38 = (F108 + F74)*F84 - F55*(F95 + F96)

        Sub39 = F106*(F36*F79 + F60*F99) + F41*Sub37 + F29*Sub38

        Sub42 = F59*(F4*F41 - F54*F93) + F110*(F112*F41 - F93*F99)

        Sub44 = (F113*F4 + F100*F54)*F59 + F110*(F112*F113 + F100*F99)

        Sub45 = (F110*F20 + F29*F59)*F86 + F105*(F54*F59 + F110*F99)

        Sub47 = F59*(F29*F80 - F54*F93) + F110*(F20*F80 - F93*F99)

        Sub56 = F16*(F12 - F17) - 
     &    (F21 + F22 - F23 - F24)*F32 + F26*(F33 - F34)

        Sub62 = (F11 - F14)*F20 + 
     &    (F21 + F22 - F23 - F24)*F27 + F29*(-F28 + F30)

        Sub67 = F68*(F69 - F70) - F66*(F71 - F72)

        Sub68 = F59*(F62 - F63) + F60*(F64 - F65)

        Sub74 = F83*(F70 - F85) - F66*(F87 - F88)

        Sub75 = F79*(F63 - F81) - F60*(F82 - F86)

        Sub80 = F37*(F17 - F39) + 
     &    F32*(F43 - F44 + F45 - F46) - F26*(F51 - F52)

        Sub85 = (F11 - F40)*F41 + 
     &    F27*(F43 - F44 + F45 - F46) - F29*(F48 - F49)

        Sub89 = F115*F118 + (F21 + F22)*F37 - F16*F45

        Sub90 = F111*F114 + (F21 + F22)*F41 - F20*F43

        Sub93 = F114*F26 - F22*F37 + F16*(F43 + F45)

        Sub94 = F115*F29 - F21*F41 + F20*(F43 + F45)

        Sub96 = F115*F116 - F15*F21 + F38*(F43 + F45)

        Sub97 = F114*F117 - F19*F22 + F42*(F43 + F45)

        Sub99 = F114*F119 + F15*(F21 + F22) - F38*F43

        Sub100 = F115*F120 + F19*(F21 + F22) - F42*F45

        Sub124 = F68*(F44 + F77) + F109*(F97 + F98)

        Sub127 = F59*(F46 + F78) + F110*(F95 + F96)

        Sub135 = (F23 + F56)*F83 + F66*(F95 + F96)

        Sub136 = (F24 + F57)*F79 + F60*(F97 + F98)

        Sub138 = -((F23 + F24 + F56 + F57)*F83) + Sub124

        Sub139 = -((F23 + F24 + F56 + F57)*F79) + Sub127

        Sub132 = F68*(F44 + F46 + F77 + F78) - Sub135

        Sub133 = F59*(F44 + F46 + F77 + F78) - Sub136

        Sub63(HelAll(Cha(4))) = Sub56 - (F13*F20 + F29*F31)*MCha(Cha(4))

        Sub64(HelAll(Cha(4))) = Sub62 + (F16*F18 + F25*F26)*MCha(Cha(4))

        Sub86(HelAll(Cha(4))) = Sub80 + (F13*F41 + F29*F50)*MCha(Cha(4))

        Sub87(HelAll(Cha(4))) = Sub85 + (F18*F37 + F26*F47)*MCha(Cha(4))

        Sub130(HelAll(Gen(1))) = Sub124*Mf(3,Gen(1)) - Sub127*Mf(4,Gen(1))

        Sub144(HelAll(Gen(1))) = 
     &   CB2*Sub124*Mf(3,Gen(1)) + SB2*Sub127*Mf(4,Gen(1))

        Sub137(HelAll(Gen(2))) = Sub135*Mf(3,Gen(2)) - Sub136*Mf(4,Gen(2))

        Sub143(HelAll(Gen(2))) = 
     &   CB2*Sub135*Mf(3,Gen(2)) + SB2*Sub136*Mf(4,Gen(2))


        Sub17(HelAll(Cha(4)),Neu(3)) = 
     &   F94*Sub15(Cha(4),Neu(3)) + F93*Sub16(Cha(4),Neu(3))

        Sub18(HelAll(Cha(4)),Neu(3)) = 
     &   F93*Sub15(Cha(4),Neu(3)) + F94*Sub16(Cha(4),Neu(3))

        Sub19(HelAll(Cha(4)),Neu(3)) = 
     &   MNeu(Neu(3))*Sub17(HelAll(Cha(4)),Neu(3)) - 
     &    MCha(Cha(4))*Sub18(HelAll(Cha(4)),Neu(3))

        Sub26(HelAll(Cha(4)),Neu(3)) = 
     &   Sub23*Sub15(Cha(4),Neu(3)) + Sub25*Sub16(Cha(4),Neu(3))

        Sub27(HelAll(Cha(4)),Neu(3)) = 
     &   (F36*F79 + F60*F99)*Sub19(HelAll(Cha(4)),Neu(3)) - 
     &    Sub26(HelAll(Cha(4)),Neu(3))

        Sub32(HelAll(Cha(4)),Neu(3)) = 
     &   Sub29*Sub15(Cha(4),Neu(3)) + Sub31*Sub16(Cha(4),Neu(3))

        Sub33(HelAll(Cha(4)),Neu(3)) = 
     &   (F54*F59 + F110*F99)*Sub19(HelAll(Cha(4)),Neu(3)) - 
     &    Sub32(HelAll(Cha(4)),Neu(3))

        Sub40(HelAll(Cha(4)),Neu(3)) = 
     &   Sub36*Sub15(Cha(4),Neu(3)) + Sub39*Sub16(Cha(4),Neu(3))

        Sub41(HelAll(Cha(4)),Neu(3)) = 
     &   (F36*F79 + F60*F99)*Sub19(HelAll(Cha(4)),Neu(3)) - 
     &    Sub40(HelAll(Cha(4)),Neu(3))

        Sub43(HelAll(Cha(4)),Neu(3)) = 
     &   Sub42*Sub15(Cha(4),Neu(3)) - 
     &    F94*(F54*F59 + F110*F99)*Sub16(Cha(4),Neu(3))

        Sub46(HelAll(Cha(4)),Neu(3)) = 
     &   Sub44*Sub15(Cha(4),Neu(3)) + Sub45*Sub16(Cha(4),Neu(3))

        Sub48(HelAll(Cha(4)),Neu(3)) = 
     &   F94*(F54*F59 + F110*F99)*Sub15(Cha(4),Neu(3)) - 
     &    Sub47*Sub16(Cha(4),Neu(3))

        Sub49(HelAll(Cha(4)),Neu(3)) = 
     &   MCha(Cha(4))*Sub43(HelAll(Cha(4)),Neu(3)) - 
     &    Sub46(HelAll(Cha(4)),Neu(3)) + 
     &    MNeu(Neu(3))*Sub48(HelAll(Cha(4)),Neu(3))

        Sub131(HelAll(Cha(4)),Neu(3)) = 
     &   F93*SB*Sub126(Cha(4),Neu(3)) + CB*F94*Sub129(Cha(4),Neu(3))

        Sub142(HelAll(Cha(4)),Neu(3)) = 
     &   CB*F93*Sub126(Cha(4),Neu(3)) - F94*SB*Sub129(Cha(4),Neu(3))



        Sub140(HelAll(Gen(1)),Gen(6)) = 
     &   Sub138*Mf(3,Gen(6)) - Sub139*Mf(4,Gen(1))

        Sub145(HelAll(Gen(1)),Gen(6)) = 
     &   CB2*Sub138*Mf(3,Gen(6)) + SB2*Sub139*Mf(4,Gen(1))



        Sub134(HelAll(Gen(2)),Gen(6)) = 
     &   Sub132*Mf(3,Gen(6)) - Sub133*Mf(4,Gen(2))

        Sub141(HelAll(Gen(2)),Gen(6)) = 
     &   CB2*Sub132*Mf(3,Gen(6)) + SB2*Sub133*Mf(4,Gen(2))


        LOOP(Sfe7, 1,2,1)

        Sub55(HelAll(Cha(4)),Gen(1),Sfe7) = 
     &   (1/Sqrt(2.D0)*(F41*S2B*Sub54(Cha(4),Gen(1),Sfe7)) + 
     &      2*F37*SB2*Mf(4,Gen(1))*UChaC(Cha(4),2)*USfC(Sfe7,1,3,Gen(1)))
     &     /(S2B*SB2)

        Sub112(HelAll(Cha(4)),Gen(1),Sfe7) = 
     &   (Sqrt(2.D0)*(CB2*F41*Sub54(Cha(4),Gen(1),Sfe7)) + 
     &      F37*S2B*Mf(4,Gen(1))*UChaC(Cha(4),2)*USfC(Sfe7,1,3,Gen(1)))/
     &    (CB2*S2B)

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub72(HelAll(Cha(4)),Gen(2),Sfe7) = 
     &   (1/Sqrt(2.D0)*(F20*S2B*Sub71(Cha(4),Gen(2),Sfe7)) + 
     &      2*F16*SB2*Mf(4,Gen(2))*UChaC(Cha(4),2)*USfC(Sfe7,1,3,Gen(2)))
     &     /(S2B*SB2)

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub105(HelAll(Cha(4)),Gen(2),Sfe8) = 
     &   (Sqrt(2.D0)*(CB2*F20*Sub103(Cha(4),Gen(2),Sfe8)) + 
     &      F16*S2B*Mf(4,Gen(2))*UChaC(Cha(4),2)*USfC(Sfe8,1,3,Gen(2)))/
     &    (CB2*S2B)

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub79(HelAll(Cha(4)),Gen(6),Sfe7) = 
     &   (1/Sqrt(2.D0)*(F10*S2B*Sub78(Cha(4),Gen(6),Sfe7)) + 
     &      2*CB2*F3*Mf(3,Gen(6))*USf(Sfe7,1,4,Gen(6))*VCha(Cha(4),2))/
     &    (CB2*S2B)

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub61(HelAll(Gen(1)),Neu(3),Sfe7) = 
     &   F15*Sub59(Gen(1),Neu(3),Sfe7) + F19*Sub60(Gen(1),Neu(3),Sfe7)

        Sub101(HelAll(Gen(1)),Neu(3),Sfe7) = 
     &   Sub99*Sub59(Gen(1),Neu(3),Sfe7) + Sub100*Sub60(Gen(1),Neu(3),Sfe7)

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub84(HelAll(Gen(2)),Neu(3),Sfe7) = 
     &   F38*Sub82(Gen(2),Neu(3),Sfe7) + F42*Sub83(Gen(2),Neu(3),Sfe7)

        Sub98(HelAll(Gen(2)),Neu(3),Sfe7) = 
     &   Sub96*Sub82(Gen(2),Neu(3),Sfe7) + Sub97*Sub83(Gen(2),Neu(3),Sfe7)

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub118(HelAll(Gen(2)),Neu(3),Sfe8) = 
     &   F38*Sub116(Gen(2),Neu(3),Sfe8) + F42*Sub117(Gen(2),Neu(3),Sfe8)

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub5(HelAll(Gen(5)),Neu(3),Sfe7) = 
     &   F4*Sub2(Gen(5),Neu(3),Sfe7) + F2*Sub4(Gen(5),Neu(3),Sfe7)

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub110(HelAll(Gen(5)),Neu(3),Sfe8) = 
     &   F4*Sub108(Gen(5),Neu(3),Sfe8) + F2*Sub109(Gen(5),Neu(3),Sfe8)

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub53(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   F58*Sub50(Gen(6),Neu(3),Sfe7) - F55*Sub52(Gen(6),Neu(3),Sfe7)

        Sub66(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   (F66*F67 + F2*F68)*Sub50(Gen(6),Neu(3),Sfe7) - 
     &    (F4*F59 + F60*F61)*Sub52(Gen(6),Neu(3),Sfe7)

        Sub69(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   Sub67*Sub50(Gen(6),Neu(3),Sfe7) - Sub68*Sub52(Gen(6),Neu(3),Sfe7)

        Sub70(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   MNeu(Neu(3))*Sub66(HelAll(Gen(6)),Neu(3),Sfe7) - 
     &    Sub69(HelAll(Gen(6)),Neu(3),Sfe7)

        Sub73(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   (F2*F83 + F66*F84)*Sub50(Gen(6),Neu(3),Sfe7) - 
     &    (F4*F79 + F60*F80)*Sub52(Gen(6),Neu(3),Sfe7)

        Sub76(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   Sub74*Sub50(Gen(6),Neu(3),Sfe7) - Sub75*Sub52(Gen(6),Neu(3),Sfe7)

        Sub77(HelAll(Gen(6)),Neu(3),Sfe7) = 
     &   MNeu(Neu(3))*Sub73(HelAll(Gen(6)),Neu(3),Sfe7) + 
     &    Sub76(HelAll(Gen(6)),Neu(3),Sfe7)

        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)

        Sub65(HelAll(Cha(4)),Gen(1),Gen(6),Sfe7) = 
     &   (1/Sqrt(2.D0)*(S2B*Sub57(Cha(4),Gen(1),Sfe7)*
     &         Sub63(HelAll(Cha(4)))) - 
     &      2*CB2*Mf(3,Gen(6))*Sub64(HelAll(Cha(4)))*
     &       USf(Sfe7,1,4,Gen(1))*VCha(Cha(4),2))/(CB2*S2B)

        Sub92(HelAll(Cha(4)),Gen(1),Gen(6),Sfe7) = 
     &   (1/Sqrt(2.D0)*(S2B*Sub90*Sub91(Cha(4),Gen(6),Sfe7)) + 
     &      2*SB2*Sub89*Mf(4,Gen(1))*UChaC(Cha(4),2)*
     &       USfC(Sfe7,1,3,Gen(6)))/(S2B*SB2)

        Sub123(HelAll(Cha(4)),Gen(1),Gen(6),Sfe7) = 
     &   (1/Sqrt(2.D0)*(F10*S2B*Sub57(Cha(4),Gen(1),Sfe7)) + 
     &      2*CB2*F3*Mf(3,Gen(6))*USf(Sfe7,1,4,Gen(1))*VCha(Cha(4),2))/
     &    (CB2*S2B)

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub12(HelAll(Cha(4)),Gen(1),Gen(6),Sfe8) = 
     &   (1/Sqrt(2.D0)*(F10*S2B*Sub9(Cha(4),Gen(1),Sfe8)) + 
     &      2*CB2*F3*Mf(3,Gen(6))*USf(Sfe8,1,4,Gen(1))*VCha(Cha(4),2))/
     &    (CB2*S2B)

        ENDLOOP(Sfe8)

        LOOP(Sfe7, 1,2,1)

        Sub88(HelAll(Cha(4)),Gen(2),Gen(6),Sfe7) = 
     &   (1/Sqrt(2.D0)*(S2B*Sub81(Cha(4),Gen(2),Sfe7)*
     &         Sub86(HelAll(Cha(4)))) + 
     &      2*CB2*Mf(3,Gen(6))*Sub87(HelAll(Cha(4)))*
     &       USf(Sfe7,1,4,Gen(2))*VCha(Cha(4),2))/(CB2*S2B)

        Sub95(HelAll(Cha(4)),Gen(2),Gen(6),Sfe7) = 
     &   (1/Sqrt(2.D0)*(S2B*Sub94*Sub91(Cha(4),Gen(6),Sfe7)) + 
     &      2*SB2*Sub93*Mf(4,Gen(2))*UChaC(Cha(4),2)*
     &       USfC(Sfe7,1,3,Gen(6)))/(S2B*SB2)

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)

        Sub8(HelAll(Cha(4)),Gen(2),Gen(6),Sfe8) = 
     &   (1/Sqrt(2.D0)*(F10*S2B*Sub1(Cha(4),Gen(2),Sfe8)) + 
     &      2*CB2*F3*Mf(3,Gen(6))*USf(Sfe8,1,4,Gen(2))*VCha(Cha(4),2))/
     &    (CB2*S2B)

        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub6(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8) = 
     &   (F7 + F8)*USfC(Sfe7,2,4,Gen(1))*USfC(Sfe8,1,4,Gen(2)) + 
     &    (F5 + F6)*USfC(Sfe7,1,4,Gen(1))*USfC(Sfe8,2,4,Gen(2))

        Sub7(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8) = 
     &   Sub6(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8) + 
     &    F1*Mino3*USfC(Sfe7,1,4,Gen(1))*USfC(Sfe8,1,4,Gen(2)) + 
     &    F9*Mino3C*USfC(Sfe7,2,4,Gen(1))*USfC(Sfe8,2,4,Gen(2))

        Sub10(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8) = 
     &   (F5 + F6)*USfC(Sfe7,2,4,Gen(2))*USfC(Sfe8,1,4,Gen(1)) + 
     &    (F7 + F8)*USfC(Sfe7,1,4,Gen(2))*USfC(Sfe8,2,4,Gen(1))

        Sub11(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8) = 
     &   Sub10(HelAll(Gen(1)),Gen(2),Sfe7,Sfe8) - 
     &    F1*Mino3*USfC(Sfe7,1,4,Gen(2))*USfC(Sfe8,1,4,Gen(1)) - 
     &    F9*Mino3C*USfC(Sfe7,2,4,Gen(2))*USfC(Sfe8,2,4,Gen(1))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub121(HelAll(Gen(1)),Gen(5),Sfe7,Sfe8) = 
     &   (F43 - F44)*USf(Sfe8,1,4,Gen(5))*USfC(Sfe7,1,4,Gen(1)) + 
     &    (F45 - F46)*USf(Sfe8,2,4,Gen(5))*USfC(Sfe7,2,4,Gen(1))

        Sub122(HelAll(Gen(1)),Gen(5),Sfe7,Sfe8) = 
     &   Sub121(HelAll(Gen(1)),Gen(5),Sfe7,Sfe8) - 
     &    F54*Mino3*USf(Sfe8,2,4,Gen(5))*USfC(Sfe7,1,4,Gen(1)) - 
     &    F53*Mino3C*USf(Sfe8,1,4,Gen(5))*USfC(Sfe7,2,4,Gen(1))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub102(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8) = 
     &   (F89 - F90)*USf(Sfe8,1,3,Gen(6))*USfC(Sfe7,1,4,Gen(1)) + 
     &    (-F91 + F92)*USf(Sfe8,2,3,Gen(6))*USfC(Sfe7,2,4,Gen(1))

        Sub104(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8) = 
     &   Sub102(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8) - 
     &    F83*Mino3*USf(Sfe8,2,3,Gen(6))*USfC(Sfe7,1,4,Gen(1)) - 
     &    F79*Mino3C*USf(Sfe8,1,3,Gen(6))*USfC(Sfe7,2,4,Gen(1))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub106(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) = 
     &   (F21 - F23)*USf(Sfe7,1,4,Gen(5))*USfC(Sfe8,1,4,Gen(2)) + 
     &    (F22 - F24)*USf(Sfe7,2,4,Gen(5))*USfC(Sfe8,2,4,Gen(2))

        Sub107(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) = 
     &   Sub106(HelAll(Gen(2)),Gen(5),Sfe7,Sfe8) - 
     &    F36*Mino3*USf(Sfe7,2,4,Gen(5))*USfC(Sfe8,1,4,Gen(2)) - 
     &    F35*Mino3C*USf(Sfe7,1,4,Gen(5))*USfC(Sfe8,2,4,Gen(2))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub111(HelAll(Gen(2)),Gen(6),Sfe7,Sfe8) = 
     &   (F73 - F74)*USf(Sfe7,1,3,Gen(6))*USfC(Sfe8,1,4,Gen(2)) + 
     &    (F75 - F76)*USf(Sfe7,2,3,Gen(6))*USfC(Sfe8,2,4,Gen(2))

        Sub113(HelAll(Gen(2)),Gen(6),Sfe7,Sfe8) = 
     &   Sub111(HelAll(Gen(2)),Gen(6),Sfe7,Sfe8) - 
     &    F68*Mino3*USf(Sfe7,2,3,Gen(6))*USfC(Sfe8,1,4,Gen(2)) - 
     &    F59*Mino3C*USf(Sfe7,1,3,Gen(6))*USfC(Sfe8,2,4,Gen(2))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub114(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8) = 
     &   (F12 - F17)*USf(Sfe7,2,4,Gen(5))*USf(Sfe8,1,3,Gen(6)) + 
     &    (-F11 + F14)*USf(Sfe7,1,4,Gen(5))*USf(Sfe8,2,3,Gen(6))

        Sub115(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8) = 
     &   Sub114(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8) - 
     &    F13*Mino3C*USf(Sfe7,1,4,Gen(5))*USf(Sfe8,1,3,Gen(6)) - 
     &    F18*Mino3*USf(Sfe7,2,4,Gen(5))*USf(Sfe8,2,3,Gen(6))

        Sub119(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8) = 
     &   (F11 - F40)*USf(Sfe7,2,3,Gen(6))*USf(Sfe8,1,4,Gen(5)) + 
     &    (F17 - F39)*USf(Sfe7,1,3,Gen(6))*USf(Sfe8,2,4,Gen(5))

        Sub120(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8) = 
     &   Sub119(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8) + 
     &    F13*Mino3C*USf(Sfe7,1,3,Gen(6))*USf(Sfe8,1,4,Gen(5)) + 
     &    F18*Mino3*USf(Sfe7,2,3,Gen(6))*USf(Sfe8,2,4,Gen(5))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)


        Sub176(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Neu(3)) = 
     &   -(Sub33(HelAll(Cha(4)),Neu(3))/
     &       (Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5)))) - 
     &    Sub49(HelAll(Cha(4)),Neu(3))/
     &     (Mf2(3,Gen(1)) - Sub166(Cha(4),Gen(1),Neu(3)))

        Sub177(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Neu(3)) = 
     &   2*IndexDelta(Gen(2),Gen(5))*
     &     Sub176(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Neu(3)) + 
     &    ((F54*F59 + F110*F99)*Sub19(HelAll(Cha(4)),Neu(3))*
     &       Sub21(Gen(2),Gen(5))*Sub34(Cha(4),Gen(1),Neu(3)))/
     &     ((S34 + GaugeXi(3)*(-MW2 + cI*MW*WW))*
     &       (Mf2(3,Gen(1)) - Sub166(Cha(4),Gen(1),Neu(3))))


        LOOP(Sfe7, 1,2,1)

        Sub190(HelAll(Gen(1)),Gen(2),Gen(5),Neu(3),Sfe7) = 
     &   -(Sub101(HelAll(Gen(1)),Neu(3),Sfe7)/
     &       (Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5)))) + 
     &    ((F21 + F22 - F23 - F24)*Sub61(HelAll(Gen(1)),Neu(3),Sfe7))/
     &     (T - Sub155(Gen(1),Sfe7))

        ENDLOOP(Sfe7)


        Sub167(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub140(HelAll(Gen(1)),Gen(6))/
     &       (Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5)))) - 
     &    Sub130(HelAll(Gen(1)))/
     &     (Mf2(3,Gen(1)) - Sub166(Cha(4),Gen(1),Neu(3)))

        Sub170(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub145(HelAll(Gen(1)),Gen(6))/
     &       (Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5)))) - 
     &    Sub144(HelAll(Gen(1)))/
     &     (Mf2(3,Gen(1)) - Sub166(Cha(4),Gen(1),Neu(3)))

        Sub174(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (3*(F36*F79 + F60*F99)*IndexDelta(Gen(1),Gen(5))*
     &       Sub13(Gen(2),Gen(6))*Sub173(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))
     &       )/T15 - ((F54*F59 + F110*F99)*IndexDelta(Gen(1),Gen(6))*
     &       Sub20(Gen(1),Gen(2),Gen(5),Gen(6))*Sub21(Gen(2),Gen(5)))/
     &     (T25*(Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5))))

        Sub175(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub27(HelAll(Cha(4)),Neu(3))/
     &       (Mf2(4,Gen(2)) - Sub163(Gen(1),Gen(2),Gen(5)))) - 
     &    Sub41(HelAll(Cha(4)),Neu(3))/
     &     (Mf2(3,Gen(2)) + Sub164(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))

        Sub178(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (6*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub175(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/T15 + 
     &    (IndexDelta(Gen(1),Gen(6))*
     &       Sub177(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Neu(3)))/T25 + 
     &    (Sub174(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))*
     &       Sub19(HelAll(Cha(4)),Neu(3)))/
     &     (S34 + GaugeXi(3)*(-MW2 + cI*MW*WW))

        Sub183(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   ((F36*F79 + F60*F99)*IndexDelta(Gen(1),Gen(5))*
     &       Sub13(Gen(2),Gen(6))*Sub173(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))
     &       )/T15 - (3*(F54*F59 + F110*F99)*IndexDelta(Gen(1),Gen(6))*
     &       Sub20(Gen(1),Gen(2),Gen(5),Gen(6))*Sub21(Gen(2),Gen(5)))/
     &     (T25*(Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5))))

        Sub184(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (2*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub175(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/T15 + 
     &    (3*IndexDelta(Gen(1),Gen(6))*
     &       Sub177(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Neu(3)))/T25 + 
     &    (Sub183(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))*
     &       Sub19(HelAll(Cha(4)),Neu(3)))/
     &     (S34 + GaugeXi(3)*(-MW2 + cI*MW*WW))

        Sub165(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub134(HelAll(Gen(2)),Gen(6))/
     &       (Mf2(4,Gen(2)) - Sub163(Gen(1),Gen(2),Gen(5)))) + 
     &    Sub137(HelAll(Gen(2)))/
     &     (Mf2(3,Gen(2)) + Sub164(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))

        Sub168(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (3*T25*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub165(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &      T15*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &       Sub167(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &    (T15*T25)

        Sub169(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   -(Sub141(HelAll(Gen(2)),Gen(6))/
     &       (Mf2(4,Gen(2)) - Sub163(Gen(1),Gen(2),Gen(5)))) + 
     &    Sub143(HelAll(Gen(2)))/
     &     (Mf2(3,Gen(2)) + Sub164(Cha(4),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))

        Sub171(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (3*T25*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub169(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &      T15*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &       Sub170(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &    (T15*T25)

        Sub172(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (S2B*Sub131(HelAll(Cha(4)),Neu(3))*
     &       Sub168(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &     (S34 + GaugeXi(3)*(-MW2 + cI*MW*WW)) - 
     &    (2*Sub142(HelAll(Cha(4)),Neu(3))*
     &       Sub171(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &     (MHp2C - S34)

        Sub179(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   1/MW*Sub172(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))/S2B + 
     &    Sub178(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))/
     &     (MW2 - S34 - cI*MW*WW)

        Sub180(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (T25*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub165(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &      3*T15*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &       Sub167(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &    (T15*T25)

        Sub181(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (T25*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub169(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) - 
     &      3*T15*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &       Sub170(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &    (T15*T25)

        Sub182(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   (S2B*Sub131(HelAll(Cha(4)),Neu(3))*
     &       Sub180(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &     (S34 + GaugeXi(3)*(-MW2 + cI*MW*WW)) - 
     &    (2*Sub142(HelAll(Cha(4)),Neu(3))*
     &       Sub181(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)))/
     &     (MHp2C - S34)

        Sub185(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3)) = 
     &   1/MW*Sub182(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))/S2B + 
     &    Sub184(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Neu(3))/
     &     (MW2 - S34 - cI*MW*WW)


        LOOP(Sfe7, 1,2,1)

        Sub193(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Sfe7) = 
     &   (-3*IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &       Sub92(HelAll(Cha(4)),Gen(1),Gen(6),Sfe7))/
     &     (T25*(Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5)))) - 
     &    (IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub95(HelAll(Cha(4)),Gen(2),Gen(6),Sfe7))/
     &     (T15*(Mf2(4,Gen(2)) - Sub163(Gen(1),Gen(2),Gen(5))))

        Sub199(HelAll(Cha(4)),Gen(1),Gen(2),Gen(5),Gen(6),Sfe7) = 
     &   -((IndexDelta(Gen(1),Gen(6))*IndexDelta(Gen(2),Gen(5))*
     &         Sub92(HelAll(Cha(4)),Gen(1),Gen(6),Sfe7))/
     &       (T25*(Mf2(4,Gen(1)) - Sub163(Gen(1),Gen(2),Gen(5))))) - 
     &    (3*IndexDelta(Gen(1),Gen(5))*IndexDelta(Gen(2),Gen(6))*
     &       Sub95(HelAll(Cha(4)),Gen(2),Gen(6),Sfe7))/
     &     (T15*(Mf2(4,Gen(2)) - Sub163(Gen(1),Gen(2),Gen(5))))

        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub204(HelAll(Gen(1)),Gen(5),Gen(6),Neu(3),Sfe7,Sfe8) = 
     &   (Sub104(HelAll(Gen(1)),Gen(6),Sfe7,Sfe8)*
     &       Sub5(HelAll(Gen(5)),Neu(3),Sfe7))/
     &     (S35 - Sub155(Gen(1),Sfe7)) + 
     &    (Sub115(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8)*
     &       Sub61(HelAll(Gen(1)),Neu(3),Sfe7))/(T - Sub155(Gen(1),Sfe7))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)

        Sub201(HelAll(Gen(2)),Gen(5),Gen(6),Neu(3),Sfe7,Sfe8) = 
     &   (Sub110(HelAll(Gen(5)),Neu(3),Sfe8)*
     &       Sub113(HelAll(Gen(2)),Gen(6),Sfe7,Sfe8))/
     &     (S35 - Sub156(Gen(2),Sfe8)) - 
     &    (Sub118(HelAll(Gen(2)),Neu(3),Sfe8)*
     &       Sub120(HelAll(Gen(5)),Gen(6),Sfe7,Sfe8))/
     &     (U - Sub156(Gen(2),Sfe8))

        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)

#include "real_contains.h"

        end

