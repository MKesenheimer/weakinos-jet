      subroutine bmunu_ubaru_n1n2g(p,ampmunu)
      implicit none
#include "PhysPars.h"
      double precision pi
      parameter (pi = 4.D0*datan(1.D0))
      double precision p(0:3,5)
      double precision al(0:3), be(0:3)
      double precision eta5(0:3)
      parameter (eta5 = (/1.D0,0.D0,0.D0,0.D0/))
      integer alind, beind, i, j
      double precision ampmunu(0:3,0:3,5)
      double precision k1(0:3)
      double precision k2(0:3)
      double precision k3(0:3)
      double precision k4(0:3)
      double precision k5(0:3)
      double precision S, T, U, S34, T14, T24
      integer Sfe6,Sfe6c
      double precision Sub4(2),Sub1(2),Sub10(2),Sub9(2),Sub26,Sub25
      double precision Sub5,Sub7,Sub2(2),Sub11(2),Sub40,Sub6(2),Sub3(2)
      double precision Sub12(2),Sub8(2),Sub14(2,2),Sub15(2,2),Sub17
      double precision Sub20,Sub19,Sub21,Sub18,Sub22,Sub23,Sub24,Sub28
      double precision Sub33,Opt5,Sub30,Sub37,Sub27,Sub35,Sub38,Sub29
      double precision Sub32,Sub34,Sub39,Sub31,Sub36,Sub41

      double precision Epsilon, DotP, Den, Kronecker
      double precision momsq, momsum2sq, momsum3sq
      external Epsilon, DotP, Den, Kronecker
      external momsq, momsum2sq, momsum3sq

      ampmunu(:,:,:) = 0D0
      S   = momsum2sq(p(:,1), p(:,2))
      T   = momsum2sq(p(:,1),-p(:,3))
      U   = momsum2sq(p(:,2),-p(:,3))
      S34 = momsum2sq(p(:,3), p(:,4))
      T14 = momsum2sq(p(:,1),-p(:,4))
      T24 = momsum2sq(p(:,2),-p(:,4))

      do i=0,3
      if(i.eq.0) then
      k1(i) = p(i,1)
      k2(i) = p(i,2)
      k3(i) = p(i,3)
      k4(i) = p(i,4)
      k5(i) = p(i,5)
      else
      k1(i) = -p(i,1)
      k2(i) = -p(i,2)
      k3(i) = -p(i,3)
      k4(i) = -p(i,4)
      k5(i) = -p(i,5)
      endif
      enddo

      do alind=0,3
      do beind=0,3

      al(:) = 0D0
      if(alind.eq.0) then
      al(alind) = 1D0
      else
      al(alind) = -1D0
      endif
      be(:) = 0D0
      if(beind.eq.0) then
      be(beind) = 1D0
      else
      be(beind) = -1D0
      endif

      Sub26 = al(0)/k5(0)**3 - k5(alind)/k5(0)**4
      Sub25 = al(0)/k5(0)**2 - k5(alind)/k5(0)**3
      Sub5 = SW*ZNeu(1,1) + 3*CW*ZNeu(1,2)
      Sub7 = SW*ZNeuC(1,1) + 3*CW*ZNeuC(1,2)
      Sub40 = T*(Sub25*be(0) - Sub26*k5(beind)) - 
     -  (Sub25*be(0) - Sub26*k5(beind))*MNeu2(1)
      Sub17 = T - T24 + 2*(MNeu2(1) + MNeu2(2))
      Sub20 = k1(alind)*(k1(0) - k3(0)) - (k1(0) - k3(0))*k3(alind)
      Sub19 = k1(beind)*(k1(0) - k3(0)) - (k1(0) - k3(0))*k3(beind)
      Sub21 = (be(0)*k3(alind) + al(0)*k3(beind))/k5(0) + 
     -  ((be(0)*k1(0) + k1(beind))*k5(alind))/k5(0)**2
      Sub18 = -k1(0)**2 + 2*k1(0)*k3(0) - k3(0)**2
      Sub22 = al(0)*k1(0) + k1(alind) - al(0)*k3(0) - k3(alind)
      Sub23 = (be(0)*k3(0) + k3(beind))*k5(alind) - Sub22*k5(beind)
      Sub24 = -Sub21 + Sub23/k5(0)**2 + 
     -  (be(0)*k1(alind) + al(0)*k1(beind))/k5(0) + 
     -  (2*(k1(0) - k3(0))*k5(alind)*k5(beind))/k5(0)**3
      Sub28 = Sub24 - Sub17*(Sub25*be(0) - Sub26*k5(beind))
      Sub33 = Sub28 - 2*T*(Sub25*be(0) - Sub26*k5(beind))
      Opt5 = -2*Sub17*Sub24 + 4*k1(beind)*(k1(alind) - k3(alind)) - 
     -  4*(k1(alind) - k3(alind))*k3(beind) - 
     -  (4*Sub18*k5(alind)*k5(beind))/k5(0)**2 - 
     -  (4*(Sub19*k5(alind) + Sub20*k5(beind)))/k5(0)
      Sub30 = Sub24 - (Sub17 + T)*(Sub25*be(0) - Sub26*k5(beind))
      Sub37 = Sub24 - (Sub17 + T24)*(Sub25*be(0) - Sub26*k5(beind))
      Sub27 = Opt5 + Sub17**2*(Sub25*be(0) - Sub26*k5(beind))
      Sub35 = Sub30 - 2*T24*(Sub25*be(0) - Sub26*k5(beind))
      Sub38 = Sub37 - 2*T*(Sub25*be(0) - Sub26*k5(beind))
      Sub29 = Opt5 - 4*Sub28*T - Sub17**2*(-(Sub25*be(0)) + Sub26*k5(beind))
      Sub32 = Opt5 - 4*Sub28*T24 - Sub17**2*(-(Sub25*be(0)) + Sub26*k5(beind))
      Sub34 = Sub27 - 4*Sub28*T - 4*Sub33*T24
      Sub39 = Sub37*T - Sub38*MNeu2(1) - 
     -  2*(Sub25*be(0) - Sub26*k5(beind))*MNeu2(1)**2
      Sub31 = Sub27*T - Sub29*MNeu2(1) - 4*Sub30*MNeu2(1)**2 - 
     -  4*(Sub25*be(0) - Sub26*k5(beind))*MNeu2(1)**3
      Sub36 = Sub32*T - Sub34*MNeu2(1) - 4*Sub35*MNeu2(1)**2 - 
     -  4*(Sub25*be(0) - Sub26*k5(beind))*MNeu2(1)**3
      Sub41 = Sub31*T24 - Sub36*MNeu2(2) - 4*Sub39*MNeu2(2)**2 - 4*Sub40*MNeu2(2)**3

      do Sfe6=1,2
      Sub4(Sfe6) = 4*MW*SB*SW*USf(Sfe6,2,3,1)*ZNeu(2,1) - 
     -  3*CW*MU*USf(Sfe6,1,3,1)*ZNeu(2,4)
      Sub1(Sfe6) = 4*MW*SB*SW*USfC(Sfe6,2,3,1)*ZNeuC(1,1) - 
     -  3*CW*MU*USfC(Sfe6,1,3,1)*ZNeuC(1,4)
      Sub2(Sfe6) = MW*SB*USf(Sfe6,1,3,1)*ZNeuC(2,2) + MU*USf(Sfe6,2,3,1)*ZNeuC(2,4)
      Sub6(Sfe6) = MW*SB*Sub5*USfC(Sfe6,1,3,1) + 3*CW*MU*USfC(Sfe6,2,3,1)*ZNeu(1,4)
      Sub3(Sfe6) = 3*CW*Sub2(Sfe6) + MW*SB*SW*USf(Sfe6,1,3,1)*ZNeuC(2,1)
      enddo

      do Sfe6c=1,2
      Sub10(Sfe6c) = 4*MW*SB*SW*USf(Sfe6c,2,3,1)*ZNeu(1,1) - 
     -  3*CW*MU*USf(Sfe6c,1,3,1)*ZNeu(1,4)
      Sub9(Sfe6c) = 4*MW*SB*SW*USfC(Sfe6c,2,3,1)*ZNeuC(2,1) - 
     -  3*CW*MU*USfC(Sfe6c,1,3,1)*ZNeuC(2,4)
      Sub11(Sfe6c) = MW*SB*USfC(Sfe6c,1,3,1)*ZNeu(2,2) + MU*USfC(Sfe6c,2,3,1)*ZNeu(2,4)
      Sub12(Sfe6c) = 3*CW*Sub11(Sfe6c) + MW*SB*SW*USfC(Sfe6c,1,3,1)*ZNeu(2,1)
      Sub8(Sfe6c) = MW*SB*Sub7*USf(Sfe6c,1,3,1) + 3*CW*MU*USf(Sfe6c,2,3,1)*ZNeuC(1,4)
      enddo

      do Sfe6=1,2
      do Sfe6c=1,2
      Sub14(Sfe6,Sfe6c) = Sub1(Sfe6)*Sub10(Sfe6c) + Sub6(Sfe6)*Sub8(Sfe6c)
      Sub15(Sfe6,Sfe6c) = Sub12(Sfe6c)*Sub3(Sfe6) + Sub4(Sfe6)*Sub9(Sfe6c)
      enddo
      enddo

      do Sfe6=1,2
      do Sfe6c=1,2
      ampmunu(alind,beind,5) = ampmunu(alind,beind,5) + (4*Alfa2*Alfas*Pi**3*Sub41*Den(T,MSf2(Sfe6,3,1))*
     -    Den(T,MSf2(Sfe6c,3,1))*Den(T24,MSf2(Sfe6,3,1))*
     -    Den(T24,MSf2(Sfe6c,3,1))*Sub14(Sfe6,Sfe6c)*Sub15(Sfe6,Sfe6c))/
     -  (81.*CW2**2*MW2**2*SB2**2*SW2**2)
      enddo
      enddo

      enddo
      enddo

      end
