      subroutine born_ubaru_n1n2g(p,b)
      implicit none
#include "PhysPars.h"
#include "pwhg_math.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
      double precision p(0:3,nlegborn)
      double precision al(0:3), be(0:3)
      double precision k1(0:3), k2(0:3), k3(0:3), k4(0:3), k5(0:3)
      double precision b
      double precision S, T, U, S34, T14, T24
      integer Sfe6, Sfe6c
      integer alind, beind

      double precision Epsilon, DotP, Den
      double precision momsq, momsum2sq, momsum3sq
      external Epsilon, DotP, Den
      external momsq, momsum2sq, momsum3sq

      double precision Sub16,Sub4(2),Sub1(2),Sub10(2)
      double precision Sub9(2),Sub5,Sub7,Sub2(2)
      double precision Sub11(2),Sub6(2),Sub3(2),Sub12(2)
      double precision Sub14(2,2),Sub15(2,2),Sub8(2)

      b = 0D0

      k1(:) = p(:,1)
      k2(:) = p(:,2)
      k3(:) = p(:,3)
      k4(:) = p(:,4)
      k5(:) = p(:,5)
      S   = momsum2sq(k1(:), k2(:))
      T   = momsum2sq(k1(:),-k3(:))
      U   = momsum2sq(k2(:),-k3(:))
      S34 = momsum2sq(k3(:), k4(:))
      T14 = momsum2sq(k1(:),-k4(:))
      T24 = momsum2sq(k2(:),-k3(:))


      Sub16 = T24*(T**2 - T*MNeu2(1)) + (-T**2 + T*MNeu2(1))*MNeu2(2)
      Sub5 = SW*ZNeu(1,1) + 3*CW*ZNeu(1,2)
      Sub7 = SW*ZNeuC(1,1) + 3*CW*ZNeuC(1,2)

      do Sfe6=1,2
      Sub4(Sfe6) =         4*MW*SB*SW*USf(Sfe6,2,3,1)*ZNeu(2,1) - 
     -  3*CW*MU*USf(Sfe6,1,3,1)*ZNeu(2,4)
      Sub1(Sfe6) =         4*MW*SB*SW*USfC(Sfe6,2,3,1)*ZNeuC(1,1) - 
     -  3*CW*MU*USfC(Sfe6,1,3,1)*ZNeuC(1,4)
      Sub2(Sfe6) = MW*SB*USf(Sfe6,1,3,1)*ZNeuC(2,2) + MU*USf(Sfe6,2,3,1)*ZNeuC(2,4)
      Sub6(Sfe6) = MW*SB*Sub5*USfC(Sfe6,1,3,1) + 3*CW*MU*USfC(Sfe6,2,3,1)*ZNeu(1,4)
      Sub3(Sfe6) = 3*CW*Sub2(Sfe6) + MW*SB*SW*USf(Sfe6,1,3,1)*ZNeuC(2,1)
      enddo

      do Sfe6c=1,2
      Sub10(Sfe6c) =         4*MW*SB*SW*USf(Sfe6c,2,3,1)*ZNeu(1,1) - 
     -  3*CW*MU*USf(Sfe6c,1,3,1)*ZNeu(1,4)
      Sub9(Sfe6c) =         4*MW*SB*SW*USfC(Sfe6c,2,3,1)*ZNeuC(2,1) - 
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
      
      print*,"Sub1",Sub1
      print*,"Sub10",Sub10
      print*,"Sub6",Sub6
      print*,"Sub8",Sub8
      print*,"Sub14",Sub14
      print*,"Sub15",Sub15
      stop

      b =         (-16*Alfa2*Alfas*Pi**3*Sub16*
     -     Den(T,MSf2(1,3,1) - ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T,MSf2(1,3,1) + ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T24,MSf2(1,3,1) - ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T24,MSf2(1,3,1) + ii*MSf(1,3,1)*WSf(1,3,1))*Sub14(1,1)*
     -     Sub15(1,1))/(81.*CW2**2*MW2**2*SB2**2*SW2**2) - 
     -  (16*Alfa2*Alfas*Pi**3*Sub16*
     -     Den(T,MSf2(1,3,1) - ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T,MSf2(2,3,1) + ii*MSf(2,3,1)*WSf(2,3,1))*
     -     Den(T24,MSf2(1,3,1) - ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T24,MSf2(2,3,1) + ii*MSf(2,3,1)*WSf(2,3,1))*Sub14(1,2)*
     -     Sub15(1,2))/(81.*CW2**2*MW2**2*SB2**2*SW2**2) - 
     -  (16*Alfa2*Alfas*Pi**3*Sub16*
     -     Den(T,MSf2(1,3,1) + ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T,MSf2(2,3,1) - ii*MSf(2,3,1)*WSf(2,3,1))*
     -     Den(T24,MSf2(1,3,1) + ii*MSf(1,3,1)*WSf(1,3,1))*
     -     Den(T24,MSf2(2,3,1) - ii*MSf(2,3,1)*WSf(2,3,1))*Sub14(2,1)*
     -     Sub15(2,1))/(81.*CW2**2*MW2**2*SB2**2*SW2**2) - 
     -  (16*Alfa2*Alfas*Pi**3*Sub16*
     -     Den(T,MSf2(2,3,1) - ii*MSf(2,3,1)*WSf(2,3,1))*
     -     Den(T,MSf2(2,3,1) + ii*MSf(2,3,1)*WSf(2,3,1))*
     -     Den(T24,MSf2(2,3,1) - ii*MSf(2,3,1)*WSf(2,3,1))*
     -     Den(T24,MSf2(2,3,1) + ii*MSf(2,3,1)*WSf(2,3,1))*Sub14(2,2)*
     -     Sub15(2,2))/(81.*CW2**2*MW2**2*SB2**2*SW2**2)

      end
