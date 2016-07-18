c############### Reals.f ###############################################
c last modified by MK, 15.07.2015
c weakino pair + jet production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setreal ####################################
c NOTE: with flag DSUB_II defined, this routine only calculates the
c real contributions without the resonant matrix element squared:
c |M_NR|**2 + 2 M_R* M_NR.
c The remaining terms and the on-shell counter terms are added back
c in subroutine setosresreal:
c |M_R|**2 - BW*corrfac*|M_R^OS|**2.
      subroutine setreal(p,rflav,ampreal)
        implicit none
#include "PhysPars.h"
#include "pwhg_st.h"
#include "pwhg_math.h"
#include "nlegborn.h"
#include "looptools.h"
#include "Flags.h"
#include "decl.h"
#include "osres.h"
        double precision p(0:3,nlegreal),p_os(0:3,nlegreal)
        integer rflav(nlegreal)
        ! copy of rflav and p
        double precision ps(0:3,nlegreal)
        integer  flavs(nlegreal),f1,f2,f3,f4,f5,f6
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5)),(f6,flavs(6))
        double precision amp, ampreal, ampos(2)
        ! formcalc results
        double precision fc_result(2)
        ! symmetry factor
        double precision symfac
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! check 4-momentum conservation
        logical lresult
        ! status
        logical verbose
        logical init_reals
        data init_reals/.true./
        
        ! variables for the on-shell subtraction
        character*4 chan
        ! kinematics
        double precision s12, sij
        ! breit-wigner ratios
        double precision bwratio
        ! masses
        double precision mij, mi, mj, mk
        ! indices
        integer n,i,j,k
        ! local variables
        double precision ampCT
        ! jacobian for the on-shell terms
        double precision corrfac
        external corrfac
        ! function to calculate the squared momentum sum
        double precision momsum3sq, momsum2sq
        external momsum2sq, momsum3sq
        
#ifdef DEBUGQ
#define DEBUG1
        ! debugging
        integer counter
        data counter/0/
        save counter
#endif

        if (init_reals) then
          print*
          print*, "=== Init reals... ==="
          verbose = .true.
          init_reals = .false.
        else
          verbose = .false.
        endif
        
        ! backup flavor list and momenta, so we can operate on it without
        ! affecting the rest of the program
        flavs(:) = rflav(:)
        ps(:,:) = p(:,:)
        
        ! check four momentum conservation
        call check_4conservation(p, nlegreal, 1, lresult)

        ! calculate the couplings for this PS point
        call set_ebe_couplings

        ! crossing (FormCalc provides only amplitudes for qqbar 
        ! and not for qbarq)
        if((flavs(1).gt.0 .and. flavs(2).lt.0) .or.
     &     (flavs(1).lt.0 .and. flavs(2).eq.0) .or.
     &     (flavs(1).gt.0 .and. flavs(2).eq.0)) then
          ps(:,1) = p(:,1)
          ps(:,2) = p(:,2)
        else
          ps(:,1) = p(:,2)
          ps(:,2) = p(:,1)
        endif

        ! unpolarized particles, i.e.: B01010 01010 01010 01010 01010 01010 = D346368330, FC8Guid.pdf p.49
        ! F F -> F F F F
        if(f1.ne.0 .and. f2.ne.0 .and. f5.ne.0 .and. f6.ne.0) then
          symfac = 96d0 ! symmetry factor: TODO
          helicities = 346368330
        ! F g -> F F F g
        elseif(f1.ne.0 .and. f2.eq.0 .and. f5.ne.0 .and. f6.eq.0) then
          symfac = 96d0
          helicities = 35056238
        ! F g -> F F g F
        elseif(f1.ne.0 .and. f2.eq.0 .and. f5.eq.0 .and. f6.ne.0) then
          symfac = 96d0
          helicities = 350562762       
        ! g F -> F F F g
        elseif(f1.eq.0 .and. f2.ne.0 .and. f5.ne.0 .and. f6.eq.0) then
          symfac = 96d0
          helicities = 480586062
        ! g F -> F F g F
        elseif(f1.eq.0 .and. f2.ne.0 .and. f5.eq.0 .and. f6.ne.0) then
          symfac = 96d0
          helicities = 480586186
        ! F F -> F F g g
        elseif(f1.ne.0 .and. f2.ne.0 .and. f5.eq.0 .and. f6.eq.0) then
          symfac = 96d0
          helicities = 346368462
        ! g g -> F F F F
        elseif(f1.eq.0 .and. f2.eq.0 .and. f5.ne.0 .and. f6.ne.0) then
          symfac = 96d0
          helicities = 484780362
        ! g g -> F F g g
        elseif(f1.eq.0 .and. f2.eq.0 .and. f5.eq.0 .and. f6.eq.0) then
          symfac = 96d0
          helicities = 484780494
        else
          print*, "ERROR: Wrong flavor structure in setreal."
          print*, "flav = ", flavs
          stop
        endif

        ! symmetry factor for equal final states
        if(flavs(3).eq.flavs(4)) then
          symfac = 2d0*symfac
        endif

        ! flags: Bit0 (reset) = 1, Bit1 (loop) = 0 -> B01 = D1
        flags = 1

        ! reset the FormCalc results
        fc_result(1) = 0.D0    ! real amplitude
        fc_result(2) = 0.D0 
        
#ifndef NEGLECTBMASS
          print*,"ERROR: FormCalc massive b-quark amplitudes not yet "
                 //"implemented."
          stop
#endif

        ! get the formcalc real amplitudes
        call getRAmps(ps,flavs,helicities,flags,fc_result)

        
#ifdef DEBUG2Q
        ! get the real amplitude from MadGraph...
        call sreal_proc(p,flavs,amp)

        print*
        print*,"flav",flavs
        print*,ps(:,1)
        print*,ps(:,2)
        print*,ps(:,3)
        print*,ps(:,4)
        print*,ps(:,5)
        print*,"pt",dsqrt(ps(1,5)**2+ps(2,5)**2)
        print*,"MadGraph",amp
        print*,"FormCalc",fc_result(1)/symfac
        print*,"rel",2D0*dabs(amp-fc_result(1)/symfac)/(amp+fc_result(1)/symfac)
        !stop
        !call nstop(10)
#endif
        
#ifdef DEBUGQ
        ! get the real amplitude from MadGraph...
        call sreal_proc(p,flavs,amp)

        if(2D0*dabs(amp-fc_result(1)/symfac)/(amp+fc_result(1)/symfac).gt.1d-2) then
          print*
          print*,"flav",flavs
          print*,"pt",dsqrt(ps(1,5)**2+ps(2,5)**2)
          print*,"MadGraph",amp
          print*,"FormCalc",fc_result(1)/symfac
          print*,"rel",2D0*dabs(amp-fc_result(1)/symfac)/(amp+fc_result(1)/symfac)
        endif
#endif
        
!         ! subtract the on shell contributions
!         if((flavs(1).eq.0 .or. flavs(2).eq.0).and.(flavs(5).ne.0)) then
!           do n=1,nosres          
!             ! set the channel-related indices i,j,k and masses mi,mj,mk,mij
!             call set_channel(osresID(n),i,j,k,mi,mj,mk,mij)
!             ! in case a weakino mass is negative
!             mi = dabs(mi)
!             mj = dabs(mj)
!             mk = dabs(mk)
!             mij = dabs(mij)
!             ! calculate the invariants
!             s12 = momsum2sq(ps(0:3,1),ps(0:3,2)) ! invariant mass of particle 1 + 2
!             sij = momsum2sq(ps(0:3,i),ps(0:3,j)) ! invariant mass of particle i + j
!             ampCT = 0D0
!             ! Prospino scheme 1211.0286
!             if( (s12.ge.(mij+mk)**2) .and. (mij.ge.(mi+mj)) ) then
!               bwratio = (mij*wreg)**2/((sij-mij**2)**2+(mij*wreg)**2) ! calculate the ratio of the breit wigner functions
!               !bwratio = 0D0
!               call off_to_on(ps,osresID(n),p_os)                      ! off_to_on the momenta p to on-shell momenta p_os
!               call getROSAmps(p_os,flavs,osresID(n),
!      &                                  helicities,flags,ampos)
!               ampCT = bwratio*ampos(1)                                ! multiply with the breit-wigner ratio
!               ampCT = corrfac(s12,mi,mj,mk,sij,mij)*ampCT             ! rescale the counter term to fit into the new ps
!             endif
!             fc_result(1) = fc_result(1) - ampCT
!           enddo
!         endif
        
        amp     = fc_result(1)/symfac
        ampreal = amp/(st_alpha/(2d0*pi))
        
        ! Print informations
        if (verbose) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "real = ", ampreal
          print*
        endif

        if(isnan(ampreal)) then
          !print*,"got strange value for real amplitude...", ampreal
          ampreal = 0D0
        endif
        
#ifdef DEBUG1
        if(counter < 5) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "real = ", ampreal
          counter = counter + 1
        endif
#endif
      end
c############### end subroutine setvirtual #############################

c############### subroutine getRAmps ###################################
      subroutine getRAmps(p,flav,helicities,flags,fc_result)
        implicit none
#include "nlegborn.h"
#include "osres.h"
        double precision p(0:3,nlegreal)
        integer  flav(nlegreal)
        integer  flavs(nlegreal),f1,f2,f3,f4,f5,f6
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5)),(f6,flavs(6))
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2)
        
        flavs(:) = flav(:)
        
        ! reset
        fc_result(:) = 0D0
        
        ! set the formCalc vectors
        call formCalcVecSet(p,flavs)
        
        ! F F -> F F F F
        if(f1.ne.0 .and. f2.ne.0 .and. f5.ne.0 .and. f6.ne.0) then
!           ! n1n1+jet+jet
!           if(f3.eq.1000022 .and. f4.eq.1000022) then
!           
!           elseif(f3.eq.1000022 .and. f4.eq.1000023) then
!           
!           elseif(f3.eq.1000023 .and. f4.eq.1000023) then
!           
!           elseif(f3.eq.1000023 .and. f4.eq.1000025) then
!           
!           elseif(f3.eq.1000022 .and. f4.eq.1000023) then
          
        ! F g -> F F F g
        elseif(f1.ne.0 .and. f2.eq.0 .and. f5.ne.0 .and. f6.eq.0) then

        ! F g -> F F g F
        elseif(f1.ne.0 .and. f2.eq.0 .and. f5.eq.0 .and. f6.ne.0) then

        ! g F -> F F F g
        elseif(f1.eq.0 .and. f2.ne.0 .and. f5.ne.0 .and. f6.eq.0) then

        ! g F -> F F g F
        elseif(f1.eq.0 .and. f2.ne.0 .and. f5.eq.0 .and. f6.ne.0) then

        ! F F -> F F g g
        elseif(f1.ne.0 .and. f2.ne.0 .and. f5.eq.0 .and. f6.eq.0) then

        ! g g -> F F F F
        elseif(f1.eq.0 .and. f2.eq.0 .and. f5.ne.0 .and. f6.ne.0) then

        ! g g -> F F g g
        elseif(f1.eq.0 .and. f2.eq.0 .and. f5.eq.0 .and. f6.eq.0) then

        else
          goto 21
        endif
        

 21     print*, "ERROR: Wrong flavor structure in getRAmps."
        print*, "flav = ", flavs
        stop
 20     continue
      end
c############### subroutine getRAmps ###################################

c############### subroutine getROSAmps #################################
      subroutine getROSAmps(p,flav,chan,helicities,flags,fc_result)
        implicit none
#include "nlegborn.h"
#include "osres.h"
        double precision p(0:3,nlegreal)
        integer  flav(nlegreal)
        integer  flavs(nlegreal),f1,f2,f3,f4,f5,f6
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5)),(f6,flavs(6))
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2)
        ! on-shell amplitude
        double precision ampos(2,cnosres)
        ! variable to determine the channel
        character*4 chan
        ! indices
        integer n
        
        flavs(:) = flav(:)
        
        ! reset
        fc_result(:) = 0D0
        ampos(:,:) = 0D0
        
        ! set the formCalc vectors
        call formCalcVecSet(p,flavs)
        

 21     print*, "ERROR: Wrong flavor structure in getROSAmps."
        print*, "flav = ", flavs
        stop
 20     continue
 
        do n=1,nosres
          if(osresID(n).eq.chan) then
            fc_result(:) = ampos(:,n)
          endif
        enddo
      end
c############### subroutine getROSAmps #################################