#if 0
* qdg_nInJqd_abbr1h3.F
* abbreviations for {F[4, {Gen(1), Col1}], V[5, {Glu2}]} -> {F[11, {Neu(3)}], F[11, {Neu(4)}], F[4, {Gen(5), Col5}]}
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:40
#endif

#include "qdg_nInJqd_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


        subroutine qdg_nInJqd_abbr1h3
        implicit none

#include "qdg_nInJqd_vars.h"
#include "virt_inline.h"

        Sub132(HelAll(Gen(1))) = 
     &   2*S*(2*F22*F35*Pair2 + Sub130) + Sub131*(S - Mf2(4,Gen(1)))

        Sub300(HelAll(Gen(1))) = F2 - 2*F4*Pair2 - F7*Mf(4,Gen(1))

        Sub301(HelAll(Gen(1))) = 
     &   (F5 - 2*F3*Pair2)*Mf(4,Gen(1)) - F8*(S - Mf2(4,Gen(1)))

        Sub303(HelAll(Gen(1))) = 
     &   (F2 - 2*F4*Pair2)*Mf(4,Gen(1)) - F7*(S - Mf2(4,Gen(1)))

        Sub304(HelAll(Gen(1))) = F5 - 2*F3*Pair2 - F8*Mf(4,Gen(1))

        Sub320(HelAll(Gen(1))) = F11 - 2*F10*Pair2 - F16*Mf(4,Gen(1))

        Sub321(HelAll(Gen(1))) = 
     &   (F13 - 2*F12*Pair2)*Mf(4,Gen(1)) - F15*(S - Mf2(4,Gen(1)))

        Sub323(HelAll(Gen(1))) = 
     &   (F11 - 2*F10*Pair2)*Mf(4,Gen(1)) - F16*(S - Mf2(4,Gen(1)))

        Sub324(HelAll(Gen(1))) = F13 - 2*F12*Pair2 - F15*Mf(4,Gen(1))

        Sub702(HelAll(Gen(1))) = -Sub664 + Sub701*Mf(4,Gen(1))

        Sub838(HelAll(Gen(1))) = F51*Pair2 - F16*(S - Mf2(4,Gen(1)))

        Sub839(HelAll(Gen(1))) = F52*Pair2 - F15*(S - Mf2(4,Gen(1)))

        Sub841(HelAll(Gen(1))) = -2*F51*Pair2 + F16*(S - Mf2(4,Gen(1)))

        Sub842(HelAll(Gen(1))) = -2*F52*Pair2 + F15*(S - Mf2(4,Gen(1)))

        Sub849(HelAll(Gen(1))) = F50*Pair2 - F7*(S - Mf2(4,Gen(1)))

        Sub850(HelAll(Gen(1))) = F49*Pair2 - F8*(S - Mf2(4,Gen(1)))

        Sub852(HelAll(Gen(1))) = 2*F50*Pair2 - F7*(S - Mf2(4,Gen(1)))

        Sub853(HelAll(Gen(1))) = 2*F49*Pair2 - F8*(S - Mf2(4,Gen(1)))

        Sub880(HelAll(Gen(1))) = 2*F50*Pair2 + F7*(S - Mf2(4,Gen(1)))

        Sub881(HelAll(Gen(1))) = 2*F49*Pair2 + F8*(S - Mf2(4,Gen(1)))

        Sub944(HelAll(Gen(1))) = 2*F51*Pair2 + F16*(S - Mf2(4,Gen(1)))

        Sub945(HelAll(Gen(1))) = 2*F52*Pair2 + F15*(S - Mf2(4,Gen(1)))

        Sub967(HelAll(Gen(1))) = 2*F50*Pair2 - F7*Mf2(4,Gen(1))

        Sub968(HelAll(Gen(1))) = 2*F49*Pair2 - F8*Mf2(4,Gen(1))

        Sub970(HelAll(Gen(1))) = 8*F50*Pair2 - F7*Mf2(4,Gen(1))

        Sub971(HelAll(Gen(1))) = 8*F49*Pair2 - F8*Mf2(4,Gen(1))

        Sub1097(HelAll(Gen(1))) = 
     &   2*(F26*F29 - F22*F53)*Pair2 + 
     &    (F22*F23 - F26*F27)*(S - Mf2(4,Gen(1)))

        Sub1098(HelAll(Gen(1))) = 
     &   2*(F26*F32 - F3*F54)*Pair2 - 
     &    (F18*F26 - F25*F3)*(S - Mf2(4,Gen(1)))

        Sub1100(HelAll(Gen(1))) = 
     &   (F26*F29 - F22*F53)*Pair2 + 
     &    (F22*F23 - F26*F27)*(S - Mf2(4,Gen(1)))

        Sub1101(HelAll(Gen(1))) = 
     &   (F26*F32 - F3*F54)*Pair2 - 
     &    (F18*F26 - F25*F3)*(S - Mf2(4,Gen(1)))

        Sub1106(HelAll(Gen(1))) = 
     &   2*(F29*F36 - F4*F56)*Pair2 - 
     &    (F27*F36 - F34*F4)*(S - Mf2(4,Gen(1)))

        Sub1107(HelAll(Gen(1))) = 
     &   2*(F32*F36 - F19*F55)*Pair2 + 
     &    (F19*F33 - F18*F36)*(S - Mf2(4,Gen(1)))

        Sub1109(HelAll(Gen(1))) = 
     &   (F29*F36 - F4*F56)*Pair2 - 
     &    (F27*F36 - F34*F4)*(S - Mf2(4,Gen(1)))

        Sub1110(HelAll(Gen(1))) = 
     &   (F32*F36 - F19*F55)*Pair2 + 
     &    (F19*F33 - F18*F36)*(S - Mf2(4,Gen(1)))

        Sub1163(HelAll(Gen(1))) = 
     &   2*(F26*F29 - F22*F53)*Pair2 - 
     &    (F22*F23 - F26*F27)*(S - Mf2(4,Gen(1)))

        Sub1164(HelAll(Gen(1))) = 
     &   2*(F26*F32 - F3*F54)*Pair2 + 
     &    (F18*F26 - F25*F3)*(S - Mf2(4,Gen(1)))

        Sub1169(HelAll(Gen(1))) = 
     &   2*(F29*F36 - F4*F56)*Pair2 + 
     &    (F27*F36 - F34*F4)*(S - Mf2(4,Gen(1)))

        Sub1170(HelAll(Gen(1))) = 
     &   2*(F32*F36 - F19*F55)*Pair2 - 
     &    (F19*F33 - F18*F36)*(S - Mf2(4,Gen(1)))

        Sub2150(HelAll(Gen(1))) = F45 - 2*F36*Pair2 - F30*Mf(4,Gen(1))

        Sub2151(HelAll(Gen(1))) = F46 - 2*F26*Pair2 - F28*Mf(4,Gen(1))

        Sub2220(HelAll(Gen(1))) = 
     &   (F45 - 2*F36*Pair2)*Mf(4,Gen(1)) - F30*Mf2(4,Gen(1))

        Sub2221(HelAll(Gen(1))) = 
     &   (F46 - 2*F26*Pair2)*Mf(4,Gen(1)) - F28*Mf2(4,Gen(1))

        Sub5097(HelAll(Gen(1))) = F53*Pair2 - F23*(S - Mf2(4,Gen(1)))

        Sub5100(HelAll(Gen(1))) = F56*Pair2 - F34*(S - Mf2(4,Gen(1)))

        Sub5101(HelAll(Gen(1))) = -(F55*Pair2) + F33*(S - Mf2(4,Gen(1)))

        Sub5474(HelAll(Gen(1))) = 
     &   F28*F48*(S - Mf2(4,Gen(1))) + F4*F6*Pair2*Mf2(4,Gen(1))

        Sub5478(HelAll(Gen(1))) = 
     &   F28*F47*(S - Mf2(4,Gen(1))) + F19*F20*Pair2*Mf2(4,Gen(1))

        Sub5485(HelAll(Gen(1))) = 
     &   F30*F48*(S - Mf2(4,Gen(1))) + F22*F35*Pair2*Mf2(4,Gen(1))

        Sub5490(HelAll(Gen(1))) = 
     &   F30*F47*(S - Mf2(4,Gen(1))) + F1*F3*Pair2*Mf2(4,Gen(1))

        Sub5506(HelAll(Gen(1))) = 2*F53*Pair2 + F23*(S - Mf2(4,Gen(1)))

        Sub5507(HelAll(Gen(1))) = 2*F54*Pair2 + F25*(S - Mf2(4,Gen(1)))

        Sub5509(HelAll(Gen(1))) = 2*F56*Pair2 + F34*(S - Mf2(4,Gen(1)))

        Sub5510(HelAll(Gen(1))) = 2*F55*Pair2 + F33*(S - Mf2(4,Gen(1)))

        Sub8397(HelAll(Gen(1))) = F17*F29 - F4*F6*Mf2(4,Gen(1))

        Sub8399(HelAll(Gen(1))) = -(F31*F32) + F1*F3*Mf2(4,Gen(1))

        Sub8971(HelAll(Gen(1))) = F17*F29 - F6*(F127 - F4*Mf2(4,Gen(1)))

        Sub9104(HelAll(Gen(1))) = F31*F32 - F1*(F128 - F3*Mf2(4,Gen(1)))

        Sub9109(HelAll(Gen(1))) = 
     &   -(F111*F73) + F6*(F128 - F3*Mf2(4,Gen(1)))

        Sub10488(HelAll(Gen(1))) = F54*Pair2 - F25*(S - Mf2(4,Gen(1)))

        Sub10626(HelAll(Gen(1))) = F62*Pair2 - F40*(S - Mf2(4,Gen(1)))

        Sub10627(HelAll(Gen(1))) = F61*Pair2 - F42*(S - Mf2(4,Gen(1)))

        Sub10662(HelAll(Gen(1))) = 
     &   F98*(F2 - 4*F4*Pair2) - F23*F70*Mf2(4,Gen(1))

        Sub10663(HelAll(Gen(1))) = 
     &   F96*(F2 - 4*F4*Pair2) - F27*F36*Mf2(4,Gen(1))

        Sub10665(HelAll(Gen(1))) = 
     &   F98*(F5 - 4*F3*Pair2) - F18*F26*Mf2(4,Gen(1))

        Sub10666(HelAll(Gen(1))) = 
     &   F96*(F5 - 4*F3*Pair2) - F33*F73*Mf2(4,Gen(1))

        Sub10848(HelAll(Gen(1))) = -2*F56*Pair2 + F34*(S - Mf2(4,Gen(1)))

        Sub10855(HelAll(Gen(1))) = -2*F54*Pair2 + F25*(S - Mf2(4,Gen(1)))

        Sub10916(HelAll(Gen(1))) = 
     &   F116*(F11 - 4*F10*Pair2) - F63*F66*Mf2(4,Gen(1))

        Sub10917(HelAll(Gen(1))) = 
     &   F116*(F13 - 4*F12*Pair2) - F119*F26*Mf2(4,Gen(1))

        Sub10919(HelAll(Gen(1))) = 
     &   F114*(F11 - 4*F10*Pair2) - F118*F36*Mf2(4,Gen(1))

        Sub10920(HelAll(Gen(1))) = 
     &   F114*(F13 - 4*F12*Pair2) - F64*F65*Mf2(4,Gen(1))

        Sub10998(HelAll(Gen(1))) = -2*F61*Pair2 + F42*(S - Mf2(4,Gen(1)))

        Sub11010(HelAll(Gen(1))) = -2*F62*Pair2 + F40*(S - Mf2(4,Gen(1)))

        Sub11047(HelAll(Gen(1))) = 
     &   F54*(F2 - 2*F4*Pair2) - F25*F4*(S - Mf2(4,Gen(1)))

        Sub11048(HelAll(Gen(1))) = 
     &   F56*(F2 - 2*F4*Pair2) - F34*F4*(S - Mf2(4,Gen(1)))

        Sub11050(HelAll(Gen(1))) = 
     &   F54*(F5 - 2*F3*Pair2) - F25*F3*(S - Mf2(4,Gen(1)))

        Sub11051(HelAll(Gen(1))) = 
     &   F56*(F5 - 2*F3*Pair2) - F3*F34*(S - Mf2(4,Gen(1)))

        Sub11300(HelAll(Gen(1))) = 
     &   F62*(F11 - 2*F10*Pair2) - F10*F40*(S - Mf2(4,Gen(1)))

        Sub11301(HelAll(Gen(1))) = 
     &   F62*(F13 - 2*F12*Pair2) - F12*F40*(S - Mf2(4,Gen(1)))

        Sub11303(HelAll(Gen(1))) = 
     &   F61*(F11 - 2*F10*Pair2) - F10*F42*(S - Mf2(4,Gen(1)))

        Sub11304(HelAll(Gen(1))) = 
     &   F61*(F13 - 2*F12*Pair2) - F12*F42*(S - Mf2(4,Gen(1)))

        Sub12018(HelAll(Gen(1))) = F127 + F4*(S - Mf2(4,Gen(1)))

        Sub12027(HelAll(Gen(1))) = F128 + F3*(S - Mf2(4,Gen(1)))

        Sub12285(HelAll(Gen(1))) = F132 + F10*(S - Mf2(4,Gen(1)))

        Sub12291(HelAll(Gen(1))) = F133 + F12*(S - Mf2(4,Gen(1)))

        Sub13338(HelAll(Gen(1))) = 
     &   F2*F98 + (-2*F53*F70 - 4*F4*F98)*Pair2 - 
     &    F23*F70*Mf2(4,Gen(1))

        Sub13339(HelAll(Gen(1))) = 
     &   F2*F96 + (-2*F29*F36 - 4*F4*F96)*Pair2 - 
     &    F27*F36*Mf2(4,Gen(1))

        Sub13341(HelAll(Gen(1))) = 
     &   F98*(F5 - 4*F3*Pair2) - 
     &    F26*(2*F32*Pair2 + F18*Mf2(4,Gen(1)))

        Sub13342(HelAll(Gen(1))) = 
     &   F5*F96 + (-2*F55*F73 - 4*F3*F96)*Pair2 - 
     &    F33*F73*Mf2(4,Gen(1))

        Sub14043(HelAll(Gen(1))) = 
     &   F116*(F11 - 4*F10*Pair2) + 
     &    F63*(-2*F94*Pair2 - F66*Mf2(4,Gen(1)))

        Sub14044(HelAll(Gen(1))) = 
     &   F116*(F13 - 4*F12*Pair2) + 
     &    F26*(-2*F134*Pair2 - F119*Mf2(4,Gen(1)))

        Sub14046(HelAll(Gen(1))) = 
     &   F114*(F11 - 4*F10*Pair2) + 
     &    F36*(-2*F129*Pair2 - F118*Mf2(4,Gen(1)))

        Sub14047(HelAll(Gen(1))) = 
     &   F114*(F13 - 4*F12*Pair2) + 
     &    F65*(-2*F91*Pair2 - F64*Mf2(4,Gen(1)))

        Sub14135(HelAll(Gen(1))) = 2*F2*F56 - F34*F4*(S - Mf2(4,Gen(1)))

        Sub14142(HelAll(Gen(1))) = 2*F2*F54 - F25*F4*(S - Mf2(4,Gen(1)))

        Sub14146(HelAll(Gen(1))) = -2*F5*F56 + F3*F34*(S - Mf2(4,Gen(1)))

        Sub14149(HelAll(Gen(1))) = -2*F5*F54 + F25*F3*(S - Mf2(4,Gen(1)))

        Sub14221(HelAll(Gen(1))) = 
     &   -2*F11*F61 + F10*F42*(S - Mf2(4,Gen(1)))

        Sub14229(HelAll(Gen(1))) = 
     &   -2*F13*F61 + F12*F42*(S - Mf2(4,Gen(1)))

        Sub14356(HelAll(Gen(1))) = 
     &   (F54 - F98)*Pair2 - F25*(S - Mf2(4,Gen(1)))

        Sub14357(HelAll(Gen(1))) = 
     &   (F56 - F96)*Pair2 - F34*(S - Mf2(4,Gen(1)))

        Sub14466(HelAll(Gen(1))) = 
     &   (F116 - F62)*Pair2 + F40*(S - Mf2(4,Gen(1)))

        Sub14467(HelAll(Gen(1))) = 
     &   (F114 - F61)*Pair2 + F42*(S - Mf2(4,Gen(1)))

        Sub134(HelAll(Gen(1))) = 
     &   2*S*(2*F1*F3*Pair2 - Sub133) + Sub28*(S - Mf2(4,Gen(1)))

        Sub137(HelAll(Gen(1))) = 
     &   2*S*(2*F4*F6*Pair2 - Sub136) + Sub24*(S - Mf2(4,Gen(1)))

        Sub140(HelAll(Gen(1))) = 
     &   2*S*(2*F19*F20*Pair2 - Sub138) + Sub139*(S - Mf2(4,Gen(1)))

        Sub284(HelAll(Gen(1))) = 2*Abb7*F10 - F11*(S - Mf2(4,Gen(1)))

        Sub285(HelAll(Gen(1))) = 2*Abb7*F12 - F13*(S - Mf2(4,Gen(1)))

        Sub327(HelAll(Gen(1))) = 2*Abb7*F4 - F2*(S - Mf2(4,Gen(1)))

        Sub328(HelAll(Gen(1))) = 2*Abb7*F3 - F5*(S - Mf2(4,Gen(1)))

        Sub344(HelAll(Gen(1))) = 2*Abb7*F4*F6 - Sub136*(S - Mf2(4,Gen(1)))

        Sub345(HelAll(Gen(1))) = 
     &   2*Abb7*F19*F20 - Sub138*(S - Mf2(4,Gen(1)))

        Sub347(HelAll(Gen(1))) = 
     &   2*Abb7*F22*F35 + Sub130*(S - Mf2(4,Gen(1)))

        Sub348(HelAll(Gen(1))) = 2*Abb7*F1*F3 - Sub133*(S - Mf2(4,Gen(1)))

        Sub618(HelAll(Gen(1))) = Abb61 + Pair2*(S - Mf2(4,Gen(1)))

        Sub822(HelAll(Gen(1))) = 
     &   (F11 - 2*F10*Pair2)*Mf2(4,Gen(1)) + F11*(S + Mf2(4,Gen(1))) - 
     &    2*F10*(2*Pair2*S - Sub618(HelAll(Gen(1))))

        Sub823(HelAll(Gen(1))) = 
     &   (F13 - 2*F12*Pair2)*Mf2(4,Gen(1)) + F13*(S + Mf2(4,Gen(1))) - 
     &    2*F12*(2*Pair2*S - Sub618(HelAll(Gen(1))))

        Sub830(HelAll(Gen(1))) = 
     &   (F2 - 2*F4*Pair2)*Mf2(4,Gen(1)) + F2*(S + Mf2(4,Gen(1))) - 
     &    2*F4*(2*Pair2*S - Sub618(HelAll(Gen(1))))

        Sub831(HelAll(Gen(1))) = 
     &   (F5 - 2*F3*Pair2)*Mf2(4,Gen(1)) + F5*(S + Mf2(4,Gen(1))) - 
     &    2*F3*(2*Pair2*S - Sub618(HelAll(Gen(1))))

        Sub835(HelAll(Gen(1))) = Abb7*F10 - F11*(S - Mf2(4,Gen(1)))

        Sub836(HelAll(Gen(1))) = Abb7*F12 - F13*(S - Mf2(4,Gen(1)))

        Sub846(HelAll(Gen(1))) = Abb7*F4 - F2*(S - Mf2(4,Gen(1)))

        Sub847(HelAll(Gen(1))) = Abb7*F3 - F5*(S - Mf2(4,Gen(1)))

        Sub857(HelAll(Gen(1))) = 2*Abb61 - Pair2*(2*S + Mf2(4,Gen(1)))

        Sub858(HelAll(Gen(1))) = 
     &   F2*(S + Mf2(4,Gen(1))) + 2*F4*Sub857(HelAll(Gen(1)))

        Sub859(HelAll(Gen(1))) = 
     &   F5*(S + Mf2(4,Gen(1))) + 2*F3*Sub857(HelAll(Gen(1)))

        Sub877(HelAll(Gen(1))) = 4*Abb7*F4 - F2*(S - Mf2(4,Gen(1)))

        Sub878(HelAll(Gen(1))) = 4*Abb7*F3 - F5*(S - Mf2(4,Gen(1)))

        Sub933(HelAll(Gen(1))) = 
     &   F11*(S + Mf2(4,Gen(1))) + 2*F10*Sub857(HelAll(Gen(1)))

        Sub934(HelAll(Gen(1))) = 
     &   F13*(S + Mf2(4,Gen(1))) + 2*F12*Sub857(HelAll(Gen(1)))

        Sub941(HelAll(Gen(1))) = 4*Abb7*F10 - F11*(S - Mf2(4,Gen(1)))

        Sub942(HelAll(Gen(1))) = 4*Abb7*F12 - F13*(S - Mf2(4,Gen(1)))

        Sub958(HelAll(Gen(1))) = 
     &   6*F4*Pair2*S - 2*(Abb62*F4 + F2*S) + 
     &    (F2 - 2*F4*Pair2)*(S - Mf2(4,Gen(1)))

        Sub959(HelAll(Gen(1))) = 
     &   6*F3*Pair2*S - 2*(Abb62*F3 + F5*S) + 
     &    (F5 - 2*F3*Pair2)*(S - Mf2(4,Gen(1)))

        Sub974(HelAll(Gen(1))) = 
     &   F2*(S + Mf2(4,Gen(1))) + F4*(4*Abb63 - Pair2*Sub973(Gen(1)))

        Sub975(HelAll(Gen(1))) = 
     &   F5*(S + Mf2(4,Gen(1))) + F3*(4*Abb63 - Pair2*Sub973(Gen(1)))

        Sub985(HelAll(Gen(1))) = 
     &   6*F10*Pair2*S - 2*(Abb62*F10 + F11*S) + 
     &    (F11 - 2*F10*Pair2)*(S - Mf2(4,Gen(1)))

        Sub986(HelAll(Gen(1))) = 
     &   6*F12*Pair2*S - 2*(Abb62*F12 + F13*S) + 
     &    (F13 - 2*F12*Pair2)*(S - Mf2(4,Gen(1)))

        Sub1005(HelAll(Gen(1))) = 
     &   F11*(S + Mf2(4,Gen(1))) + F10*(4*Abb63 - Pair2*Sub973(Gen(1)))

        Sub1006(HelAll(Gen(1))) = 
     &   F13*(S + Mf2(4,Gen(1))) + F12*(4*Abb63 - Pair2*Sub973(Gen(1)))

        Sub1103(HelAll(Gen(1))) = Abb7*F4*F6 - Sub136*(S - Mf2(4,Gen(1)))

        Sub1104(HelAll(Gen(1))) = 
     &   Abb7*F19*F20 - Sub138*(S - Mf2(4,Gen(1)))

        Sub1112(HelAll(Gen(1))) = 
     &   Abb7*F22*F35 + Sub130*(S - Mf2(4,Gen(1)))

        Sub1113(HelAll(Gen(1))) = Abb7*F1*F3 - Sub133*(S - Mf2(4,Gen(1)))

        Sub1166(HelAll(Gen(1))) = 
     &   4*Abb7*F4*F6 - Sub136*(S - Mf2(4,Gen(1)))

        Sub1167(HelAll(Gen(1))) = 
     &   4*Abb7*F19*F20 - Sub138*(S - Mf2(4,Gen(1)))

        Sub1172(HelAll(Gen(1))) = 
     &   4*Abb7*F22*F35 + Sub130*(S - Mf2(4,Gen(1)))

        Sub1173(HelAll(Gen(1))) = 
     &   4*Abb7*F1*F3 - Sub133*(S - Mf2(4,Gen(1)))

        Sub3101(HelAll(Gen(1))) = 
     &   F49*(4*Abb165 - 4*(Abb2 + Pair2)) - F8*Sub3100(Gen(1))

        Sub3106(HelAll(Gen(1))) = 
     &   F50*(4*Abb165 - 4*(Abb2 + Pair2)) - F7*Sub3100(Gen(1))

        Sub3215(HelAll(Gen(1))) = 2*Abb68 + Abb2*(2*S + Mf2(4,Gen(1)))

        Sub3230(HelAll(Gen(1))) = Abb202 - Pair2*(T24 - Mf2(4,Gen(1)))

        Sub3310(HelAll(Gen(1))) = 
     &   2*(Abb205 + Abb165*S) + Pair2*Mf2(4,Gen(1))

        Sub3354(HelAll(Gen(1))) = 2*Abb74 + Abb1*(2*S + Mf2(4,Gen(1)))

        Sub3368(HelAll(Gen(1))) = Abb258 - Pair2*(U - Mf2(4,Gen(1)))

        Sub3474(HelAll(Gen(1))) = 
     &   F52*(-4*Abb120 + 4*(Abb1 + Pair2)) + F15*Sub3473(Gen(1))

        Sub3479(HelAll(Gen(1))) = 
     &   F51*(-4*Abb120 + 4*(Abb1 + Pair2)) + F16*Sub3473(Gen(1))

        Sub3526(HelAll(Gen(1))) = 
     &   2*(Abb261 + Abb120*S) + Pair2*Mf2(4,Gen(1))

        Sub3775(HelAll(Gen(1))) = 
     &   -2*(Abb296*F6 + F109*(Abb53 + Pair1)) + 2*F109*Pair2 - 
     &    F24*Mf2(4,Gen(1))

        Sub3782(HelAll(Gen(1))) = 
     &   -2*(Abb296*F1 + F108*(Abb53 + Pair1)) + 2*F108*Pair2 - 
     &    F38*Mf2(4,Gen(1))

        Sub4331(HelAll(Gen(1))) = -Sub4364 - F7*(T + Mf2(4,Gen(1)))

        Sub4336(HelAll(Gen(1))) = -Sub4349 - F8*(T + Mf2(4,Gen(1)))

        Sub5057(HelAll(Gen(1))) = 
     &   Abb161*F17*(F29 + F48) + F28*F48*(S - Mf2(4,Gen(1)))

        Sub5094(HelAll(Gen(1))) = Abb170*F6 + F24*(S - Mf2(4,Gen(1)))

        Sub5095(HelAll(Gen(1))) = Abb170*F20 + F21*(S - Mf2(4,Gen(1)))

        Sub5103(HelAll(Gen(1))) = Abb170*F35 + F37*(S - Mf2(4,Gen(1)))

        Sub5104(HelAll(Gen(1))) = Abb170*F1 + F38*(S - Mf2(4,Gen(1)))

        Sub6688(HelAll(Gen(1))) = 
     &   2*Abb1*F52 - F15*(S + T + U - 2*Mf2(4,Gen(1)))

        Sub6691(HelAll(Gen(1))) = 
     &   2*Abb1*F51 - F16*(S + T + U - 2*Mf2(4,Gen(1)))

        Sub6698(HelAll(Gen(1))) = 
     &   F50*(-2*Pair2 + 2*(Abb2 + Pair4)) + F7*Mf2(4,Gen(1))

        Sub6699(HelAll(Gen(1))) = 
     &   F49*(-2*Pair2 + 2*(Abb2 + Pair4)) + F8*Mf2(4,Gen(1))

        Sub6853(HelAll(Gen(1))) = Abb129 + (-Abb2 + Pair3)*Mf2(4,Gen(1))

        Sub7145(HelAll(Gen(1))) = 
     &   F51*(-2*Pair2 + 2*(Abb1 + Pair3)) + F16*Mf2(4,Gen(1))

        Sub7146(HelAll(Gen(1))) = 
     &   F52*(-2*Pair2 + 2*(Abb1 + Pair3)) + F15*Mf2(4,Gen(1))

        Sub7204(HelAll(Gen(1))) = 
     &   2*Abb2*F49 - F8*(S + T14 + T24 - 2*Mf2(4,Gen(1)))

        Sub7207(HelAll(Gen(1))) = 
     &   2*Abb2*F50 - F7*(S + T14 + T24 - 2*Mf2(4,Gen(1)))

        Sub7483(HelAll(Gen(1))) = Abb83 + (-Abb1 + Pair4)*Mf2(4,Gen(1))

        Sub10279(HelAll(Gen(1))) = 
     &   2*Abb236*F104 - F131 - 2*Sub10278 - F8*Mf2(4,Gen(1))

        Sub10303(HelAll(Gen(1))) = 
     &   2*Abb236*F103 - F130 - 2*Sub10302 - F7*Mf2(4,Gen(1))

        Sub10623(HelAll(Gen(1))) = Abb170*F9 + F41*(S - Mf2(4,Gen(1)))

        Sub10624(HelAll(Gen(1))) = Abb170*F14 + F39*(S - Mf2(4,Gen(1)))

        Sub10854(HelAll(Gen(1))) = 
     &   Abb2*F109 + F6*Pair3*(S - Mf2(4,Gen(1)))

        Sub10996(HelAll(Gen(1))) = Abb337*F9 + F41*(S - Mf2(4,Gen(1)))

        Sub10997(HelAll(Gen(1))) = 
     &   Abb1*F112 + F9*Pair4*(S - Mf2(4,Gen(1)))

        Sub11008(HelAll(Gen(1))) = Abb337*F14 + F39*(S - Mf2(4,Gen(1)))

        Sub11009(HelAll(Gen(1))) = 
     &   Abb1*F105 + F14*Pair4*(S - Mf2(4,Gen(1)))

        Sub11054(HelAll(Gen(1))) = 2*Abb173*F1 - 3*F38*(S - Mf2(4,Gen(1)))

        Sub11056(HelAll(Gen(1))) = 2*Abb173*F6 - 3*F24*(S - Mf2(4,Gen(1)))

        Sub11086(HelAll(Gen(1))) = 
     &   Abb1*F112 - F9*(2*Abb270 + Abb1*Mf2(4,Gen(1)))

        Sub11314(HelAll(Gen(1))) = 2*Abb173*F9 - 3*F41*(S - Mf2(4,Gen(1)))

        Sub11318(HelAll(Gen(1))) = 
     &   2*Abb173*F14 - 3*F39*(S - Mf2(4,Gen(1)))

        Sub11395(HelAll(Gen(1))) = 
     &   Pair2*Sub5586 + Abb2*F10*F9*Mf2(4,Gen(1))

        Sub11403(HelAll(Gen(1))) = 
     &   -(F52*(2*Abb162 - 4*Pair1 - 2*Pair2)) + F15*Mf2(4,Gen(1))

        Sub11414(HelAll(Gen(1))) = 
     &   Pair2*Sub5594 + Abb2*F12*F9*Mf2(4,Gen(1))

        Sub11416(HelAll(Gen(1))) = 
     &   4*F51*F9*Pair1 + F31*F77*(2*Abb160 + 4*Pair1) - 
     &    2*Sub11415 + (-(F30*F77) + F16*F9)*Mf2(4,Gen(1))

        Sub11417(HelAll(Gen(1))) = 
     &   -(F31*(2*Abb160 + 4*Pair1 - 2*Pair2)) + F30*Mf2(4,Gen(1))

        Sub11429(HelAll(Gen(1))) = 
     &   Pair2*Sub5604 + Abb2*F10*F14*Mf2(4,Gen(1))

        Sub11431(HelAll(Gen(1))) = 
     &   -2*Sub11430 + F14*
     &     (F52*(2*Abb162 - 4*Pair1) - F15*Mf2(4,Gen(1))) + 
     &    F78*(-4*F17*Pair1 + F28*Mf2(4,Gen(1)))

        Sub11432(HelAll(Gen(1))) = 
     &   F17*(2*Abb160 + 4*Pair1 - 2*Pair2) - F28*Mf2(4,Gen(1))

        Sub11442(HelAll(Gen(1))) = 
     &   Pair2*Sub5611 + Abb2*F12*F14*Mf2(4,Gen(1))

        Sub11449(HelAll(Gen(1))) = 
     &   -(F51*(2*Abb162 - 4*Pair1 - 2*Pair2)) + F16*Mf2(4,Gen(1))

        Sub11584(HelAll(Gen(1))) = 
     &   F49*(2*Abb162 - 4*Pair1 - 2*Pair2) - F8*Mf2(4,Gen(1))

        Sub11598(HelAll(Gen(1))) = 
     &   4*F49*F6*Pair1 + F17*F44*(2*Abb160 + 4*Pair1) - 
     &    2*Sub11597 + (-(F28*F44) + F6*F8)*Mf2(4,Gen(1))

        Sub11611(HelAll(Gen(1))) = 
     &   Pair2*Sub5300 + Abb1*F1*F3*Mf2(4,Gen(1))

        Sub11613(HelAll(Gen(1))) = 
     &   4*F1*F50*Pair1 + F31*F43*(2*Abb160 + 4*Pair1) - 
     &    2*Sub11612 + (-(F30*F43) + F1*F7)*Mf2(4,Gen(1))

        Sub11622(HelAll(Gen(1))) = 
     &   F50*(2*Abb162 - 4*Pair1 - 2*Pair2) - F7*Mf2(4,Gen(1))

        Sub11647(HelAll(Gen(1))) = 
     &   F49*(2*(Abb1 + Abb38) - 4*Pair1 - 2*Pair2) + 
     &    F8*Mf2(4,Gen(1))

        Sub11656(HelAll(Gen(1))) = 
     &   F50*(2*(Abb1 + Abb38) - 4*Pair1 - 2*Pair2) + 
     &    F7*Mf2(4,Gen(1))

        Sub11711(HelAll(Gen(1))) = 
     &   F52*(2*(Abb2 + Abb24) - 4*Pair1 - 2*Pair2) + 
     &    F15*Mf2(4,Gen(1))

        Sub11719(HelAll(Gen(1))) = 
     &   F51*(2*(Abb2 + Abb24) - 4*Pair1 - 2*Pair2) + 
     &    F16*Mf2(4,Gen(1))

        Sub11782(HelAll(Gen(1))) = 
     &   F105*Pair4 + F14*(Pair1*T24 + Abb163*Mf2(4,Gen(1)))

        Sub12065(HelAll(Gen(1))) = 
     &   F108*Pair3 + F1*(Pair1*U + Abb163*Mf2(4,Gen(1)))

        Sub12476(HelAll(Gen(1))) = 
     &   (F112*F12 + F133*F9)*Pair2 + Abb2*F12*F9*Mf2(4,Gen(1))

        Sub12489(HelAll(Gen(1))) = 
     &   F132*F14*Pair2 + F10*(F105*Pair2 + Abb2*F14*Mf2(4,Gen(1)))

        Sub13121(HelAll(Gen(1))) = 
     &   Pair1*Sub13120 + Pair2*Sub5290 + Abb1*F1*F4*Mf2(4,Gen(1))

        Sub13132(HelAll(Gen(1))) = 
     &   (F109*F4 + F127*F6)*Pair2 + 
     &    F4*F6*(Abb418 + Abb1*Mf2(4,Gen(1)))

        Sub13142(HelAll(Gen(1))) = 
     &   -(F108*F3*(2*Pair1 - Pair2)) + 
     &    F1*(F128*(Pair1 + Pair2) + Abb1*F3*Mf2(4,Gen(1)))

        Sub13153(HelAll(Gen(1))) = 
     &   Pair2*Sub5312 + Abb1*F3*F6*Mf2(4,Gen(1))

        Sub13259(HelAll(Gen(1))) = 
     &   Abb345 - Pair1*T24 - Abb356*Mf2(4,Gen(1))

        Sub13309(HelAll(Gen(1))) = Abb359 - Pair1*U - Abb392*Mf2(4,Gen(1))

        Sub13449(HelAll(Gen(1))) = 
     &   Abb2*F127 - F4*(Abb221 + Abb2*Mf2(4,Gen(1)))

        Sub14139(HelAll(Gen(1))) = Abb363 + Pair3*(S - Mf2(4,Gen(1)))

        Sub14223(HelAll(Gen(1))) = Abb326 + Pair4*(S - Mf2(4,Gen(1)))

        Sub313(HelAll(Gen(5))) = F41 - 2*F9*Pair1 - F40*Mf(4,Gen(5))

        Sub317(HelAll(Gen(5))) = F39 - 2*F14*Pair1 - F42*Mf(4,Gen(5))

        Sub364(HelAll(Gen(5))) = F38 - 2*F1*Pair1 - F25*Mf(4,Gen(5))

        Sub368(HelAll(Gen(5))) = F24 - 2*F6*Pair1 - F34*Mf(4,Gen(5))

        Sub1223(HelAll(Gen(5))) = 
     &   2*(F26*F29 - F22*F53)*Pair2 + 
     &    (F22*F23 - F26*F27)*(S - Mf2(4,Gen(5)))

        Sub1224(HelAll(Gen(5))) = 
     &   2*(F26*F32 - F3*F54)*Pair2 - 
     &    (F18*F26 - F25*F3)*(S - Mf2(4,Gen(5)))

        Sub1226(HelAll(Gen(5))) = 
     &   2*(F29*F36 - F4*F56)*Pair2 - 
     &    (F27*F36 - F34*F4)*(S - Mf2(4,Gen(5)))

        Sub1227(HelAll(Gen(5))) = 
     &   2*(F32*F36 - F19*F55)*Pair2 + 
     &    (F19*F33 - F18*F36)*(S - Mf2(4,Gen(5)))

        Sub1257(HelAll(Gen(5))) = 
     &   1/SW2*(F45 - 2*F36*Pair1 + CW2*(2*F45 - 4*F36*Pair1) + 
     &      SW2*(Sub701*Mf(4,Gen(5))))

        Sub1512(HelAll(Gen(5))) = 
     &   F40*Mf(4,Gen(5))*Sub398(Gen(5)) + 
     &    (-F41 + 2*F9*Pair1)*Sub399(Gen(5))

        Sub1513(HelAll(Gen(5))) = 
     &   (F39 - 2*F14*Pair1)*Sub398(Gen(5)) - 
     &    F42*Mf(4,Gen(5))*Sub399(Gen(5))

        Sub2229(HelAll(Gen(5))) = F45 - 2*F36*Pair1 - F28*Mf(4,Gen(5))

        Sub2230(HelAll(Gen(5))) = F46 - 2*F26*Pair1 - F30*Mf(4,Gen(5))

        Sub5237(HelAll(Gen(5))) = 
     &   F124*F31 - (F101 + F41)*F51 - 
     &    2*(F31*F77 - F51*F9)*Pair2 - 
     &    F16*(F112 - F9*(S + Mf2(4,Gen(5))))

        Sub5239(HelAll(Gen(5))) = 
     &   (F101 + F41)*F52 - 2*Pair2*Sub5238 - 
     &    F63*(F89 - F64*Mf2(4,Gen(5))) + 
     &    F15*(F112 - F9*(S + Mf2(4,Gen(5))))

        Sub5242(HelAll(Gen(5))) = 
     &   (F102 + F39)*F51 - 2*Pair2*Sub5241 - 
     &    F65*(F92 - F66*Mf2(4,Gen(5))) + 
     &    F16*(F105 - F14*(S + Mf2(4,Gen(5))))

        Sub5243(HelAll(Gen(5))) = 
     &   F123*F17 - (F102 + F39)*F52 + 
     &    2*(F14*F52 - F17*F78)*Pair2 - 
     &    F15*(F105 - F14*(S + Mf2(4,Gen(5))))

        Sub5322(HelAll(Gen(5))) = 
     &   2*F17*(Pair1 + 2*Pair2) - F28*(S - Mf2(4,Gen(5)))

        Sub5332(HelAll(Gen(5))) = 
     &   -2*F31*(Pair1 + 2*Pair2) + F30*(S - Mf2(4,Gen(5)))

        Sub5576(HelAll(Gen(5))) = 
     &   -F89 + 2*((F90 + F91)*Pair1 + F91*Pair2) + 
     &    F64*(S34 + Mf2(4,Gen(5)))

        Sub5579(HelAll(Gen(5))) = 
     &   -F92 + 2*((F93 + F94)*Pair1 + F94*Pair2) + 
     &    F66*(S34 + Mf2(4,Gen(5)))

        Sub5669(HelAll(Gen(5))) = 
     &   (F32 + F47)*(S - Mf2(4,Gen(5))) + 
     &    F32*(S - S34 + 3*Mf2(4,Gen(5)))

        Sub5694(HelAll(Gen(5))) = 
     &   4*F61*Pair1 + F42*(T - T24 + Mf2(4,Gen(5)))

        Sub5696(HelAll(Gen(5))) = 
     &   4*F62*Pair1 + F40*(T - T24 + Mf2(4,Gen(5)))

        Sub5896(HelAll(Gen(5))) = 6*F17*Pair1 - F28*Mf2(4,Gen(5))

        Sub5897(HelAll(Gen(5))) = 4*F17*Pair1 - F28*Mf2(4,Gen(5))

        Sub5920(HelAll(Gen(5))) = 2*F17*Pair2 - F28*(S - Mf2(4,Gen(5)))

        Sub5926(HelAll(Gen(5))) = -2*F31*Pair2 + F30*(S - Mf2(4,Gen(5)))

        Sub6300(HelAll(Gen(5))) = 
     &   4*F56*Pair1 + F34*(T14 - U + Mf2(4,Gen(5)))

        Sub6302(HelAll(Gen(5))) = 
     &   4*F54*Pair1 + F25*(T14 - U + Mf2(4,Gen(5)))

        Sub7419(HelAll(Gen(5))) = 
     &   2*F31*(Pair1 + Pair2) + F30*(S - Mf2(4,Gen(5)))

        Sub7421(HelAll(Gen(5))) = F31*Sub7420 - F30*(S - Mf2(4,Gen(5)))

        Sub7424(HelAll(Gen(5))) = 
     &   2*F17*(Pair1 + Pair2) + F28*(S - Mf2(4,Gen(5)))

        Sub7425(HelAll(Gen(5))) = F17*Sub7420 - F28*(S - Mf2(4,Gen(5)))

        Sub8381(HelAll(Gen(5))) = 
     &   -(F63*(F106 - F76*Mf2(4,Gen(5)))) + 
     &    F10*(F112 - F9*(S + Mf2(4,Gen(5))))

        Sub8382(HelAll(Gen(5))) = 
     &   -(F134*F31) + F12*(F112 - F9*(S + Mf2(4,Gen(5))))

        Sub8384(HelAll(Gen(5))) = 
     &   -(F129*F17) + F10*(F105 - F14*(S + Mf2(4,Gen(5))))

        Sub8385(HelAll(Gen(5))) = 
     &   -(F65*(F107 - F75*Mf2(4,Gen(5)))) + 
     &    F12*(F105 - F14*(S + Mf2(4,Gen(5))))

        Sub8403(HelAll(Gen(5))) = F112 - F9*(S + T14 + Mf2(4,Gen(5)))

        Sub8406(HelAll(Gen(5))) = F105 - F14*(S + T14 + Mf2(4,Gen(5)))

        Sub8413(HelAll(Gen(5))) = 
     &   F17*F29 - 2*F4*(F109 - F6*(S + Mf2(4,Gen(5))))

        Sub8415(HelAll(Gen(5))) = -F108 + F1*(S + Mf2(4,Gen(5)))

        Sub8489(HelAll(Gen(5))) = 
     &   F10*(F112 - F9*(S + Mf2(4,Gen(5)))) - 
     &    F63*(F106 - F76*(S34 + Mf2(4,Gen(5))))

        Sub8493(HelAll(Gen(5))) = 
     &   F12*(F105 - F14*(S + Mf2(4,Gen(5)))) - 
     &    F65*(F107 - F75*(S34 + Mf2(4,Gen(5))))

        Sub8527(HelAll(Gen(5))) = 
     &   F10*F112 - F63*(F106 - F76*Mf2(4,Gen(5)))

        Sub8532(HelAll(Gen(5))) = 
     &   F105*F12 - F65*(F107 - F75*Mf2(4,Gen(5)))

        Sub8645(HelAll(Gen(5))) = 
     &   Pair1*(F65*F75*S34 - Sub8493(HelAll(Gen(5)))) - 
     &    Pair2*Sub8493(HelAll(Gen(5)))

        Sub11191(HelAll(Gen(5))) = F49*F68 + F8*(F108 + F1*Mf2(4,Gen(5)))

        Sub11207(HelAll(Gen(5))) = F49*F67 + F8*(F109 + F6*Mf2(4,Gen(5)))

        Sub11208(HelAll(Gen(5))) = 
     &   -2*(F17*F44 + F49*F6)*(Pair1 + Pair2) + 2*Sub11206 - 
     &    Sub11207(HelAll(Gen(5)))

        Sub11218(HelAll(Gen(5))) = F50*F68 + F7*(F108 + F1*Mf2(4,Gen(5)))

        Sub11219(HelAll(Gen(5))) = 
     &   -2*(F31*F43 + F1*F50)*(Pair1 + Pair2) + 2*Sub11217 - 
     &    Sub11218(HelAll(Gen(5)))

        Sub11229(HelAll(Gen(5))) = F50*F67 + F7*(F109 + F6*Mf2(4,Gen(5)))

        Sub11882(HelAll(Gen(5))) = 
     &   F101*F52 + F15*(F112 + F9*Mf2(4,Gen(5)))

        Sub11899(HelAll(Gen(5))) = 
     &   F101*F51 + F16*(F112 + F9*Mf2(4,Gen(5)))

        Sub11900(HelAll(Gen(5))) = 
     &   -2*(F31*F77 + F51*F9)*(Pair1 + Pair2) + 2*Sub11898 - 
     &    Sub11899(HelAll(Gen(5)))

        Sub11914(HelAll(Gen(5))) = 
     &   F102*F52 + F15*(F105 + F14*Mf2(4,Gen(5)))

        Sub11915(HelAll(Gen(5))) = 
     &   -2*(F14*F52 + F17*F78)*(Pair1 + Pair2) + 2*Sub11913 - 
     &    Sub11914(HelAll(Gen(5)))

        Sub11922(HelAll(Gen(5))) = 
     &   F102*F51 + F16*(F105 + F14*Mf2(4,Gen(5)))

        Sub13409(HelAll(Gen(5))) = 
     &   F49*(F38 - 2*F1*Pair1) + F1*F8*Mf2(4,Gen(5))

        Sub13417(HelAll(Gen(5))) = -Sub12561 - F6*F8*Mf2(4,Gen(5))

        Sub13424(HelAll(Gen(5))) = -Sub12567 - F1*F7*Mf2(4,Gen(5))

        Sub13427(HelAll(Gen(5))) = 
     &   F50*(F24 - 2*F6*Pair1) + F6*F7*Mf2(4,Gen(5))

        Sub691(HelAll(Gen(5))) = 
     &   Abb59*F4*F6 + F28*(F29 + F48)*(S - Mf2(4,Gen(5)))

        Sub693(HelAll(Gen(5))) = 
     &   Abb59*F19*F20 + F28*(F32 + F47)*(S - Mf2(4,Gen(5)))

        Sub696(HelAll(Gen(5))) = 
     &   Abb59*F22*F35 + F30*(F29 + F48)*(S - Mf2(4,Gen(5)))

        Sub698(HelAll(Gen(5))) = 
     &   Abb59*F1*F3 + F30*(F32 + F47)*(S - Mf2(4,Gen(5)))

        Sub783(HelAll(Gen(5))) = Abb61 - Pair2*(S - Mf2(4,Gen(5)))

        Sub789(HelAll(Gen(5))) = Abb62 - 2*Pair2*(S - Mf2(4,Gen(5)))

        Sub990(HelAll(Gen(5))) = 
     &   2*F17*(Abb53 + Pair1 - Pair2) + F28*(S - Mf2(4,Gen(5)))

        Sub997(HelAll(Gen(5))) = 
     &   2*F31*(Abb53 + Pair1 - Pair2) + F30*(S - Mf2(4,Gen(5)))

        Sub1270(HelAll(Gen(5))) = Abb63 - Pair2*(S - Mf2(4,Gen(5)))

        Sub3632(HelAll(Gen(5))) = 
     &   -(F31*(2*(Abb53 + Pair1) - 2*Pair2)) + F30*Mf2(4,Gen(5))

        Sub3637(HelAll(Gen(5))) = 
     &   F17*(2*(Abb53 + Pair1) - 2*Pair2) - F28*Mf2(4,Gen(5))

        Sub5214(HelAll(Gen(5))) = Abb168*F4*F6 + F28*F48*Mf2(4,Gen(5))

        Sub5217(HelAll(Gen(5))) = Abb168*F19*F20 + F28*F47*Mf2(4,Gen(5))

        Sub5220(HelAll(Gen(5))) = Abb168*F22*F35 + F30*F48*Mf2(4,Gen(5))

        Sub5222(HelAll(Gen(5))) = Abb168*F1*F3 + F30*F47*Mf2(4,Gen(5))

        Sub5623(HelAll(Gen(5))) = 
     &   Abb190*F4*F6 + F28*F48*(S - Mf2(4,Gen(5)))

        Sub5627(HelAll(Gen(5))) = 
     &   Abb190*F19*F20 + F28*F47*(S - Mf2(4,Gen(5)))

        Sub5630(HelAll(Gen(5))) = 
     &   Abb190*F22*F35 + F30*F48*(S - Mf2(4,Gen(5)))

        Sub5632(HelAll(Gen(5))) = 
     &   Abb190*F1*F3 + F30*F47*(S - Mf2(4,Gen(5)))

        Sub5693(HelAll(Gen(5))) = 
     &   Abb111 + 4*Pair1*T24 - 
     &    Pair4*(-2*T24 + 2*(T + Mf2(4,Gen(5))))

        Sub6240(HelAll(Gen(5))) = 
     &   Abb188 + 2*(Pair1 + Pair2)*(S - Mf2(4,Gen(5)))

        Sub6299(HelAll(Gen(5))) = 
     &   Abb153 + 4*Pair1*U - Pair3*(-2*U + 2*(T14 + Mf2(4,Gen(5))))

        Sub10077(HelAll(Gen(5))) = 
     &   Abb439 + Pair1*(S - T14 + Mf2(4,Gen(5)))

        Sub12316(HelAll(Gen(5))) = 
     &   Pair1*Sub9772 + Abb2*F10*F9*Mf2(4,Gen(5))

        Sub12324(HelAll(Gen(5))) = 
     &   (2*F112*F12 - F133*F9)*Pair1 + Abb2*F12*F9*Mf2(4,Gen(5))

        Sub12325(HelAll(Gen(5))) = -Sub12383 - F16*F9*Mf2(4,Gen(5))

        Sub12331(HelAll(Gen(5))) = 
     &   -(F132*F14*Pair1) + 
     &    F10*(2*F105*Pair1 + Abb2*F14*Mf2(4,Gen(5)))

        Sub12332(HelAll(Gen(5))) = -Sub12390 - F14*F15*Mf2(4,Gen(5))

        Sub12336(HelAll(Gen(5))) = 
     &   Pair1*Sub9792 + Abb2*F12*F14*Mf2(4,Gen(5))

        Sub13408(HelAll(Gen(5))) = 
     &   Abb377*F4 + (-F121 - 2*Abb1*F4)*Mf2(4,Gen(5))

        Sub13422(HelAll(Gen(5))) = 
     &   Abb377*F3 + (-F122 - 2*Abb1*F3)*Mf2(4,Gen(5))

        Sub13764(HelAll(Gen(5))) = 
     &   F117*Mf2(4,Gen(5)) + 
     &    F10*(Abb333 - 2*(Abb446 + Pair4*Mf2(4,Gen(5))))

        Sub13779(HelAll(Gen(5))) = 
     &   F120*Mf2(4,Gen(5)) + 
     &    F12*(Abb333 - 2*(Abb446 + Pair4*Mf2(4,Gen(5))))

        LOOP(Gen6, 1,3,1)

        Sub1386(HelAll(Gen6)) = 
     &   Sub1385*Mf2(4,Gen6) + 
     &    (F19*F23 + F1*F7)*(T24 + U + Mf2(4,Gen6))

        Sub1390(HelAll(Gen6)) = 
     &   Sub1389*Mf2(4,Gen6) + 
     &    (F22*F33 + F6*F8)*(T24 + U + Mf2(4,Gen6))

        Sub1403(HelAll(Gen6)) = 
     &   Sub1401*(T24 + U) - Sub1402*Mf2(4,Gen6)

        Sub1416(HelAll(Gen6)) = 
     &   Sub1385*Mf2(3,Gen6) + 
     &    (F19*F23 + F1*F7)*(T24 + U + Mf2(3,Gen6))

        Sub1418(HelAll(Gen6)) = 
     &   Sub1389*Mf2(3,Gen6) + 
     &    (F22*F33 + F6*F8)*(T24 + U + Mf2(3,Gen6))

        Sub1424(HelAll(Gen6)) = 
     &   Sub1401*(T24 + U) - Sub1402*Mf2(3,Gen6)

        Sub4021(HelAll(Gen6)) = 
     &   (F17 + F31)*F59 + 
     &    F19*(2*F53*(Pair3 + Pair4) - F23*Mf2(4,Gen6)) + 
     &    F1*(2*F50*(Pair3 + Pair4) - F7*Mf2(4,Gen6))

        Sub4022(HelAll(Gen6)) = 
     &   (F17 + F31)*F60 + 
     &    F22*(2*F55*(Pair3 + Pair4) - F33*Mf2(4,Gen6)) + 
     &    F6*(2*F49*(Pair3 + Pair4) - F8*Mf2(4,Gen6))

        Sub4308(HelAll(Gen6)) = 
     &   (F17 + F31)*F59 + 
     &    F19*(2*F53*(Pair3 + Pair4) - F23*Mf2(3,Gen6)) + 
     &    F1*(2*F50*(Pair3 + Pair4) - F7*Mf2(3,Gen6))

        Sub4309(HelAll(Gen6)) = 
     &   (F17 + F31)*F60 + 
     &    F22*(2*F55*(Pair3 + Pair4) - F33*Mf2(3,Gen6)) + 
     &    F6*(2*F49*(Pair3 + Pair4) - F8*Mf2(3,Gen6))

        Sub1387(HelAll(Gen6)) = 
     &   2*F31*F43*Pair2 - 2*Sub1384 - Sub1386(HelAll(Gen6))

        Sub1391(HelAll(Gen6)) = 
     &   2*F31*F44*Pair2 - 2*Sub1388 - Sub1390(HelAll(Gen6))

        Sub1400(HelAll(Gen6)) = 
     &   Abb79 + Abb78*(T24 + U + 2*Mf2(4,Gen6))

        Sub1405(HelAll(Gen6)) = 
     &   2*F17*F44*Pair2 - 2*Sub1404 - Sub1390(HelAll(Gen6))

        Sub1407(HelAll(Gen6)) = 
     &   2*F17*F43*Pair2 - 2*Sub1406 - Sub1386(HelAll(Gen6))

        Sub1417(HelAll(Gen6)) = 
     &   2*F31*F43*Pair2 - 2*Sub1384 - Sub1416(HelAll(Gen6))

        Sub1419(HelAll(Gen6)) = 
     &   2*F31*F44*Pair2 - 2*Sub1388 - Sub1418(HelAll(Gen6))

        Sub1423(HelAll(Gen6)) = 
     &   Abb79 + Abb78*(T24 + U + 2*Mf2(3,Gen6))

        Sub1425(HelAll(Gen6)) = 
     &   2*F17*F44*Pair2 - 2*Sub1404 - Sub1418(HelAll(Gen6))

        Sub1426(HelAll(Gen6)) = 
     &   2*F17*F43*Pair2 - 2*Sub1406 - Sub1416(HelAll(Gen6))

        Sub4095(HelAll(Gen6)) = 
     &   Sub4090 + (F22*F35 + F4*F6)*(Abb53 - Pair3)*Mf2(4,Gen6)

        Sub4320(HelAll(Gen6)) = 
     &   Sub4090 + (F22*F35 + F4*F6)*(Abb53 - Pair3)*Mf2(3,Gen6)

        ENDLOOP(Gen6)

#include "virt_contains.h"

        end

