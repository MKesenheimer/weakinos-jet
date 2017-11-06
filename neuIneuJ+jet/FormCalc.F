c############## FormCalc.f ############################################
c last modified by MK, 22.01.2015
c weakino pair + jet production
c Interface to the FormCalc amplitudes
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setDivergence ##############################
      subroutine setDivergence()
        implicit none
#include "virt_types.h"
#include "virt_const.h"
        double precision getdelta
        external detdelta
        Divergence = getdelta()
      end
c############### subroutine setDivergence ##############################

c############### subroutine formCalcVecSet #############################
c calculate unit vectors and set the vectors for FormCalc
      subroutine formCalcVecSet(nlegs,p,flav)
        implicit none
        integer nlegs
        double precision p(0:3,nlegs)
        integer  flav(nlegs)
        double precision mass(nlegs)
        double precision ex, ey, ez, pNorm
        integer i
        ! function to calculate the squared momentum sum
        double precision momsum3sq, momsum2sq
        external momsum2sq, momsum3sq
        ! load process dependent masses
        call set_process(nlegs,flav(1:nlegs),mass(1:nlegs))
        do i=1,nlegs
          pNorm=dsqrt(p(1,i)**2+p(2,i)**2+p(3,i)**2)
          ex=p(1,i)/pNorm
          ey=p(2,i)/pNorm
          ez=p(3,i)/pNorm
          call VecSet(i, mass(i), pNorm, ex,ey,ez)
        enddo
      end
c############### end subroutine formCalcVecSet #########################

c############### subroutine crossing ###################################
      subroutine crossing(nlegs,p,flav,l1,l2)
        implicit none
        integer nlegs
        double precision p(0:3,nlegs), ptemp(0:3)
        integer flav(nlegs),ftemp
        integer l1, l2
        integer i
        if(l1.gt.nlegs.or.l1.le.0.or.l2.gt.nlegs.or.l2.le.0) then
          print*,"error: l1 and l2 should be in (1,",nlegs,")"
          stop
        endif
        do i=0,3
          ptemp(i) = p(i,l1)
          p(i,l1) = p(i,l2)
          p(i,l2) = ptemp(i)
        enddo
        ftemp = flav(l1)
        flav(l1) = flav(l2)
        flav(l2) = ftemp
      end
c############### end subroutine crossing ###############################

c############### subroutine getBornAmps ################################
      subroutine getBornAmps(p,flav,born,bornjk,bmunu)
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegborn)
        integer flav(nlegborn)
        double precision amp(2),born
        double precision bornjk(nlegborn,nlegborn)
        double precision bmunu(0:3,0:3,nlegborn)
        integer flags
        double precision NC, CF, CA, eighth
        parameter (NC=3D0, CF=4D0/3D0, CA=3D0, eighth=1D0/8D0)
        double precision fqg, fqq
        parameter (fqq=-eighth*CF, fqg=eighth*NC**2*CF)
c#define CHECKMADFORM
#ifdef CHECKMADFORM
        integer i,gleg
        double precision bmad
#endif
        
        ! reset
        born = 0D0
        bornjk(:,:) = 0D0
        bmunu(:,:,:) = 0D0

        ! = 010d, Bit0: set mass, Bit1: reset, Bit2: loop
        flags = 2
        call getAmps(p,flav,flags,amp)
        born = amp(1)

        ! fill the color and spin correlated amplitudes
        ! F F -> F F g
        if(flav(1).ne.0.and.flav(2).ne.0) then
          bornjk(1,2) = fqq*born
          bornjk(2,1) = bornjk(1,2)
          bornjk(1,5) = fqg*born
          bornjk(5,1) = bornjk(1,5)
          bornjk(2,5) = fqg*born
          bornjk(5,2) = bornjk(2,5)
        ! g F -> F F F
        elseif(flav(1).eq.0.and.flav(2).ne.0) then
          bornjk(1,2) = fqg*born
          bornjk(2,1) = bornjk(1,2)
          bornjk(1,5) = fqg*born
          bornjk(5,1) = bornjk(1,5)
          bornjk(2,5) = fqq*born
          bornjk(5,2) = bornjk(2,5)
        ! F g -> F F F  
        elseif(flav(1).ne.0.and.flav(2).eq.0) then
          bornjk(1,2) = fqg*born
          bornjk(2,1) = bornjk(1,2)
          bornjk(1,5) = fqq*born
          bornjk(5,1) = bornjk(1,5)
          bornjk(2,5) = fqg*born
          bornjk(5,2) = bornjk(2,5)
        endif

        ! MadGraph spincorrelated amplitude
        call sborn_proc(p,flav,bmunu)
        
#ifdef CHECKMADFORM
        do i=1,nlegborn
          if(flav(i).eq.0) gleg = i
        enddo
        bmad = -bmunu(0,0,gleg)+bmunu(1,1,gleg)+bmunu(2,2,gleg)+bmunu(3,3,gleg)
        if(dsqrt(p(1,5)**2+p(2,5)**2).gt.10.and.(born-bmad)/(born+bmad).gt.1d-4) then
          print*,flav
          print*,"FormCalc = ", born
          print*,"MadGraph = ", bmad
          print*,(born-bmad)/(born+bmad)
          print*
        endif
#endif
      end
c############### end subroutine getBornAmps ############################
      
c############### subroutine getVirtAmps ################################
      subroutine getVirtAmps(p,flav,amps)
        implicit none
#include "nlegborn.h"
#include "Flags.h"
#include "pwhg_st.h"
        double precision p(0:3,nlegborn)
        integer flav(nlegborn)
        double precision amps(2)
        integer i,j,flags

        call clearcache
        call setlambda(0d0) ! Only finite part
        call setuvdiv(0d0)  ! no 1/eps_UV terms
        call setdelta(0d0)  ! set divergences to zero
        call setmudim(st_muren2) ! renormalization scale
        call setDivergence

        if(flg_fakevirtuals) then
          ! = 010d, Bit0: set mass, Bit1: reset, Bit2: loop
          flags = 2
        else
          ! = 110d, Bit0: set mass, Bit1: reset, Bit2: loop
          flags = 6
        endif

        call getAmps(p,flav,flags,amps(:))
      
c#define CHECKUV
#ifdef CHECKUV
        ! test for finiteness
        print*
        print*, "checking for UV finiteness"
        print*, "flav = ", flav
        call clearcache
        call setuvdiv(1d0)
        call setdelta(1D7)    ! Test UV finiteness
        call setDivergence

        print*,"p(:,:) = "
        print*,p(:,1)
        print*,p(:,2)
        print*,p(:,3)
        print*,p(:,4)
        print*,p(:,5)
        print*,"pt(5) = ",dsqrt(p(1,5)**2+p(2,5)**2)
        print*,"born, virt   = ", amps
        
        call getAmps(p,flav,flags,amps(:))
        print*,"born, virtUV = ", amps
        call nstop(100)
#endif
      end
c############### end subroutine getVirtAmps ############################

c############### subroutine getAmps ####################################
      subroutine getAmps(p,flav,flags,amp)
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegborn)
        integer  flav(nlegborn), flavs(nlegborn),f1,f2,f3,f4,f5
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5))
        double precision ps(0:3,nlegborn)
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2), amp(2), symfac
        logical isodd, iseven
        external isodd, iseven

        ! reset
        fc_result(:) = 0D0
        flavs(:) = flav(:)
        ps(:,:) = p(:,:)
        amp(:) = 0D0
        
        ! crossing (FormCalc provides only amplitudes for qqbar 
        ! and not for qbarq)
        if(.not.((f1.gt.0.and.f2.lt.0) .or.
     &     (f1.ne.0.and.f2.eq.0))) then
          call crossing(nlegreal,ps,flavs,1,2)
        endif

        ! helicities, symmetry factor
        ! unpolarized fermion: B01010  FC8Guid.pdf p.49
        ! unpolarized gluon: B01110
        if(f1.eq.0) then
          symfac = 96d0
          helicities = 15018314
        elseif(f2.eq.0) then
          symfac = 96d0
          helicities = 10955082
        elseif(f3.eq.0) then
          symfac = 36d0
          helicities = 1082106
        elseif(f4.eq.0) then
          symfac = 36d0
          helicities = 10824138
        elseif(f5.eq.0) then
          symfac = 36d0
          helicities = 10824014
        else
          print*, "ERROR: Wrong flavor structure in getAmps."
          print*, "flav = ", flavs
          stop
        endif

        ! symmetry factor for equal final states
        if(f3.eq.f4) then
          symfac = 2d0*symfac
        endif
        
        ! set the formCalc vectors
        call formCalcVecSet(nlegborn,ps,flavs)

#define MATRIXELEMENT
#ifdef MATRIXELEMENT
        ! F g -> F F F
        if((f1.eq.0 .or. f2.eq.0).and.(flav(5).ne.0)) then
          if((f1.gt.0.and.isodd(f1)) .or. (f2.gt.0.and.isodd(f2))) then
            if(flags.eq.6) call qdg_nInJqd_RenConst
            call qdg_nInJqd_squaredME(fc_result,helicities,flags)
            goto 20
          elseif((f1.gt.0.and.iseven(f1)) .or. (f2.gt.0.and.iseven(f2))) then
            if(flags.eq.6) call qug_nInJqu_RenConst
            call qug_nInJqu_squaredME(fc_result,helicities,flags)
            goto 20
          elseif((f1.lt.0.and.isodd(f1)) .or. (f2.lt.0.and.isodd(f2))) then
            if(flags.eq.6) call qdbarg_nInJqdbar_RenConst
            call qdbarg_nInJqdbar_squaredME(fc_result,helicities,flags)
            goto 20
          elseif((f1.lt.0.and.iseven(f1)) .or. (f2.lt.0.and.iseven(f2))) then
            if(flags.eq.6) call qubarg_nInJqubar_RenConst
            call qubarg_nInJqubar_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! F F -> F F g
        else
          if(isodd(abs(f1)).and.isodd(abs(f2))) then
            if(flags.eq.6) call qdqdbar_nInJg_RenConst
            call qdqdbar_nInJg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f1)).and.iseven(abs(f2))) then
            if(flags.eq.6) call ququbar_nInJg_RenConst
            call ququbar_nInJg_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        endif

 21     print*, "error: Wrong flavor structure in getAmps."
        print*, "flav = ", flav
        stop
 20     continue
#endif

        amp(1) = fc_result(1)/symfac
        amp(2) = fc_result(2)/symfac
      end
c############### subroutine getAmps ####################################

c############### subroutine getRealAmps ################################
      subroutine getRealAmps(p,flav,amp)
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegreal)
        integer  flav(nlegreal), flavs(nlegreal),f1,f2,f3,f4,f5,f6
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5)),(f6,flavs(6))
        double precision ps(0:3,nlegreal)
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2), amp, symfac
        logical isodd, iseven
        external isodd, iseven
        
        ! reset
        fc_result(:) = 0D0
        flavs(:) = flav(:)
        ps(:,:) = p(:,:)
        amp = 0D0
        ! = 010d, Bit0: set mass, Bit1: reset, Bit2: loop
        flags = 2
        
        ! helicities, crossing, symmetry factor
        ! unpolarized fermion: B01010  FC8Guid.pdf p.49
        ! unpolarized gluon: B01110
        ! F F -> F F F F
        if(f1.ne.0.and.f2.ne.0.and.f5.ne.0.and.f6.ne.0) then
          symfac = 36d0
          helicities = 346368330
          if(f1.lt.0.and.f2.gt.0 .or.
     &       f1.lt.0.and.f2.lt.0.and.iseven(abs(f1)).and.isodd(abs(f2)) .or.
     &       f1.gt.0.and.f2.gt.0.and.iseven(abs(f1)).and.isodd(abs(f2))) then
            call crossing(nlegreal,ps,flavs,1,2)
          endif
          if(f5.lt.0.and.f6.gt.0 .or.
     &       f5.lt.0.and.f6.lt.0.and.iseven(abs(f5)).and.isodd(abs(f6)) .or.
     &       f5.gt.0.and.f6.gt.0.and.iseven(abs(f5)).and.isodd(abs(f6))) then
            call crossing(nlegreal,ps,flavs,5,6)
          endif
        ! g F -> F F F g
        elseif(f1.eq.0.and.f2.ne.0.and.f5.ne.0.and.f6.eq.0) then
          symfac = 96d0
          helicities = 480586062
        ! F g -> F F F g
        elseif(f1.ne.0.and.f2.eq.0.and.f5.ne.0.and.f6.eq.0) then
          call crossing(nlegreal,ps,flavs,1,2)
          symfac = 96d0
          helicities = 480586062
        ! g F -> F F g F
        elseif(f1.eq.0.and.f2.ne.0.and.f5.eq.0.and.f6.ne.0) then
          call crossing(nlegreal,ps,flavs,5,6)
          symfac = 96d0
          helicities = 480586062 
        ! F g -> F F g F
        elseif(f1.ne.0.and.f2.eq.0.and.f5.eq.0.and.f6.ne.0) then
          call crossing(nlegreal,ps,flavs,1,2)
          call crossing(nlegreal,ps,flavs,5,6)
          symfac = 96d0
          helicities = 480586062
        ! F F -> F F g g
        elseif(f1.ne.0.and.f2.ne.0.and.f5.eq.0.and.f6.eq.0) then
          symfac = 36d0
          helicities = 346368462
          if(f1.lt.0.and.f2.gt.0) then
            call crossing(nlegreal,ps,flavs,1,2)
          endif
          if(f5.lt.0.and.f6.gt.0) then
            call crossing(nlegreal,ps,flavs,5,6)
          endif
        ! g g -> F F F F
        elseif(f1.eq.0.and.f2.eq.0.and.f5.ne.0.and.f6.ne.0) then
          symfac = 256d0
          if(f1.lt.0.and.f2.gt.0) then
            call crossing(nlegreal,ps,flavs,1,2)
          endif
          if(f5.lt.0.and.f6.gt.0) then
            call crossing(nlegreal,ps,flavs,5,6)
          endif
        else
          print*, "error: Wrong flavor structure in setreal."
          print*, "flav = ", flavs
          stop
        endif

        ! symmetry factor for equal final states
        if(flavs(3).eq.flavs(4)) then
          symfac = 2d0*symfac
        endif
        if(flavs(5).eq.flavs(6)) then
          symfac = 2d0*symfac
        endif
        
        ! set the formCalc vectors
        call formCalcVecSet(nlegreal,ps,flavs)

#define MATRIXELEMENT
#ifdef MATRIXELEMENT
        ! F F -> F F F F
        if(f1.ne.0.and.f2.ne.0.and.f5.ne.0.and.f6.ne.0) then
          if(isodd(abs(f1)).and.isodd(abs(f2))) then
            if(f1.lt.0.and.f2.lt.0) then
              call qdbarqdbar_nInJqdbarqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call qdqdbar_nInJqdqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.iseven(abs(f5)).and.f5.gt.0) then
              call qdqdbar_nInJququbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.gt.0) then
              call qdqd_nInJqdqd_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)).and.iseven(abs(f2))) then
            if(f1.lt.0.and.f2.lt.0) then
              call qubarqubar_nInJqubarqubar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.iseven(abs(f5)).and.f5.gt.0) then
              call ququbar_nInJququbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call ququbar_nInJqdqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.gt.0) then
              call ququ_nInJququ_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(isodd(abs(f1)).and.iseven(abs(f2))) then
            if(f1.lt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.lt.0) then
              call qdbarqubar_nInJqdbarqubar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call qdqubar_nInJqdqubar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.gt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call qdqu_nInJqdqu_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)).and.isodd(abs(f2))) then
            if(f1.gt.0.and.f2.lt.0.and.iseven(abs(f5)).and.f5.gt.0) then
              call quqdbar_nInJquqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          else
            goto 21
          endif
        ! g F -> F F F g
        elseif(f1.eq.0.and.f2.ne.0.and.f5.ne.0.and.f6.eq.0) then
          if(iseven(abs(f2)).and.f2.gt.0) then
            call gqu_nInJqug_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f2)).and.f2.lt.0) then
            call gqubar_nInJqubarg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f2)).and.f2.gt.0) then
            call gqd_nInJqdg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f2)).and.f2.lt.0) then
            call gqdbar_nInJqdbarg_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! F F -> F F g g
        elseif(f1.ne.0.and.f2.ne.0.and.f5.eq.0.and.f6.eq.0) then
          if(isodd(abs(f1)).and.f1.gt.0) then
            call qdqdbar_nInJgg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f1)).and.f1.gt.0) then
            call ququbar_nInJgg_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! g g -> F F F F
        elseif(f1.eq.0.and.f2.eq.0.and.f5.ne.0.and.f6.ne.0) then
          if(isodd(abs(f5)).and.f5.gt.0) then
            call gg_nInJqdqdbar_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f5)).and.f5.gt.0) then
            call gg_nInJququbar_squaredME(fc_result,helicities,flags)
            goto 20  
          else
            goto 21
          endif
        else
          goto 21
        endif

 21     print*, "error: Wrong flavor structure in getRealAmps."
        print*, "flav = ", flav
        stop
 20     continue
#endif

        amp = fc_result(1)/symfac
      end
c############### subroutine getRealAmps ################################

c############### subroutine getRealOSAmps ##############################
      subroutine getRealOSAmps(p,flav,ichan,ampos)
        implicit none
#include "nlegborn.h"
#include "osres.h"
        double precision p(0:3,nlegreal)
        integer  flav(nlegreal), flavs(nlegreal),f1,f2,f3,f4,f5,f6
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5)),(f6,flavs(6))
        double precision ps(0:3,nlegreal)
        integer ichan
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2), symfac
        double precision ampos
        logical isodd, iseven
        external isodd, iseven
        
        ! reset
        fc_result(:) = 0D0
        flavs(:) = flav(:)
        ps(:,:) = p(:,:)
        ampos = 0D0
        ! = 010d, Bit0: set mass, Bit1: reset, Bit2: loop
        flags = 2
        
        ! helicities, crossing, symmetry factor
        ! unpolarized fermion: B01010  FC8Guid.pdf p.49
        ! unpolarized gluon: B01110
        ! F F -> F F F F
        if(f1.ne.0.and.f2.ne.0.and.f5.ne.0.and.f6.ne.0) then
          symfac = 36d0
          helicities = 346368330
          if(f1.lt.0.and.f2.gt.0 .or.
     &       f1.lt.0.and.f2.lt.0.and.iseven(abs(f1)).and.isodd(abs(f2)) .or.
     &       f1.gt.0.and.f2.gt.0.and.iseven(abs(f1)).and.isodd(abs(f2))) then
            call crossing(nlegreal,ps,flavs,1,2)
          endif
          if(f5.lt.0.and.f6.gt.0 .or.
     &       f5.lt.0.and.f6.lt.0.and.iseven(abs(f5)).and.isodd(abs(f6)) .or.
     &       f5.gt.0.and.f6.gt.0.and.iseven(abs(f5)).and.isodd(abs(f6))) then
            call crossing(nlegreal,ps,flavs,5,6)
          endif
        ! g F -> F F F g
        elseif(f1.eq.0.and.f2.ne.0.and.f5.ne.0.and.f6.eq.0) then
          symfac = 96d0
          helicities = 480586062
        ! F g -> F F F g
        elseif(f1.ne.0.and.f2.eq.0.and.f5.ne.0.and.f6.eq.0) then
          call crossing(nlegreal,ps,flavs,1,2)
          symfac = 96d0
          helicities = 480586062
        ! g F -> F F g F
        elseif(f1.eq.0.and.f2.ne.0.and.f5.eq.0.and.f6.ne.0) then
          call crossing(nlegreal,ps,flavs,5,6)
          symfac = 96d0
          helicities = 480586062 
        ! F g -> F F g F
        elseif(f1.ne.0.and.f2.eq.0.and.f5.eq.0.and.f6.ne.0) then
          call crossing(nlegreal,ps,flavs,1,2)
          call crossing(nlegreal,ps,flavs,5,6)
          symfac = 96d0
          helicities = 480586062
        ! F F -> F F g g
        elseif(f1.ne.0.and.f2.ne.0.and.f5.eq.0.and.f6.eq.0) then
          symfac = 36d0
          helicities = 346368462
          if(f1.lt.0.and.f2.gt.0) then
            call crossing(nlegreal,ps,flavs,1,2)
          endif
          if(f5.lt.0.and.f6.gt.0) then
            call crossing(nlegreal,ps,flavs,5,6)
          endif
        ! g g -> F F F F
        elseif(f1.eq.0.and.f2.eq.0.and.f5.ne.0.and.f6.ne.0) then
          symfac = 256d0
          if(f1.lt.0.and.f2.gt.0) then
            call crossing(nlegreal,ps,flavs,1,2)
          endif
          if(f5.lt.0.and.f6.gt.0) then
            call crossing(nlegreal,ps,flavs,5,6)
          endif
        else
          print*, "error: Wrong flavor structure in setreal."
          print*, "flav = ", flavs
          stop
        endif

        ! symmetry factor for equal final states
        if(flavs(3).eq.flavs(4)) then
          symfac = 2d0*symfac
        endif
        if(flavs(5).eq.flavs(6)) then
          symfac = 2d0*symfac
        endif
        
        ! set the formCalc vectors
        call formCalcVecSet(nlegreal,ps,flavs)
        ! set the channel informations, in particular the sfermion indices
        ! which are needed by the realOS routines
        call set_channel(flav,ichan)
      
#define MATRIXELEMENT
#ifdef MATRIXELEMENT
        ! F F -> F F F F
        if(f1.ne.0.and.f2.ne.0.and.f5.ne.0.and.f6.ne.0) then
          if(isodd(abs(f1)).and.isodd(abs(f2))) then
            if(f1.lt.0.and.f2.lt.0) then
              call qdbarqdbar_nInJqdbarqdbar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call qdqdbar_nInJqdqdbar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.iseven(abs(f5)).and.f5.gt.0) then
              call qdqdbar_nInJququbar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.gt.0) then
              call qdqd_nInJqdqd_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)).and.iseven(abs(f2))) then
            if(f1.lt.0.and.f2.lt.0) then
              call qubarqubar_nInJqubarqubar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.iseven(abs(f5)).and.f5.gt.0) then
              call ququbar_nInJququbar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call ququbar_nInJqdqdbar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.gt.0) then
              call ququ_nInJququ_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(isodd(abs(f1)).and.iseven(abs(f2))) then
            if(f1.lt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.lt.0) then
              call qdbarqubar_nInJqdbarqubar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.lt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call qdqubar_nInJqdqubar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            elseif(f1.gt.0.and.f2.gt.0.and.isodd(abs(f5)).and.f5.gt.0) then
              call qdqu_nInJqdqu_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)).and.isodd(abs(f2))) then
            if(f1.gt.0.and.f2.lt.0.and.iseven(abs(f5)).and.f5.gt.0) then
              call quqdbar_nInJquqdbar_realOS_squaredME(fc_result,ichan,helicities,flags)
              goto 20
            else
              goto 21
            endif
          else
            goto 21
          endif
        ! g g -> F F F F
        elseif(f1.eq.0.and.f2.eq.0.and.f5.ne.0.and.f6.ne.0) then
          if(isodd(abs(f5)).and.f5.gt.0) then
            call gg_nInJqdqdbar_realOS_squaredME(fc_result,ichan,helicities,flags)
            goto 20
          elseif(iseven(abs(f5)).and.f5.gt.0) then
            call gg_nInJququbar_realOS_squaredME(fc_result,ichan,helicities,flags)
            goto 20  
          else
            goto 21
          endif
        else
          goto 21
        endif
        
 21     print*, "error: Wrong flavor structure in getRealOSAmps."
        print*, "flav = ", flav
        stop
 20     continue
#endif

        ampos = fc_result(1)/symfac
      end
c############### subroutine getRealOSAmps ##############################
