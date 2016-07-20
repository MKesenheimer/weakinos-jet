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
        
#ifdef DEBUGQ
        do i=1,nlegreal
          print*,"        flavs(",i,") = ",rflav(i)
          do j=0,3
            print*,"        ps(",j,",",i,") = ", p(j,i)
          enddo
        enddo
        stop
#endif

#ifdef DEBUGQ
         flavs(           1 ) =           -4
         ps(           0 ,           1 ) =    2433.3361567772117D0
         ps(           1 ,           1 ) =    0.0000000000000000D0     
         ps(           2 ,           1 ) =    0.0000000000000000D0     
         ps(           3 ,           1 ) =    2433.3361567772117D0     
         flavs(           2 ) =           -4
         ps(           0 ,           2 ) =    2433.3361567772117D0     
         ps(           1 ,           2 ) =    0.0000000000000000D0     
         ps(           2 ,           2 ) =    0.0000000000000000D0     
         ps(           3 ,           2 ) =   -2433.3361567772117D0     
         flavs(           3 ) =      1000022
         ps(           0 ,           3 ) =    2059.0938579668482D0     
         ps(           1 ,           3 ) =   -740.52967816895693D0     
         ps(           2 ,           3 ) =   -1915.0108015757655D0     
         ps(           3 ,           3 ) =   -121.93588030733588D0     
         flavs(           4 ) =      1000023
         ps(           0 ,           4 ) =    873.20653133569544D0     
         ps(           1 ,           4 ) =   -101.96891144413289D0     
         ps(           2 ,           4 ) =    839.58697824297883D0     
         ps(           3 ,           4 ) =   -119.96988299311880D0     
         flavs(           5 ) =           -4
         ps(           0 ,           5 ) =    729.35050620199615D0     
         ps(           1 ,           5 ) =    479.26306163459833D0     
         ps(           2 ,           5 ) =    206.30478262886419D0     
         ps(           3 ,           5 ) =   -509.60515628690052D0     
         flavs(           6 ) =           -4
         ps(           0 ,           6 ) =    1205.0214180498836D0     
         ps(           1 ,           6 ) =    363.23552797849158D0     
         ps(           2 ,           6 ) =    869.11904070392245D0     
         ps(           3 ,           6 ) =    751.51091958735549D0
#endif
        
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
          flavs(1) = rflav(2)
          flavs(2) = rflav(1)
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
        call getRealAmps(ps,flavs,helicities,flags,fc_result)

        
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
c############### end subroutine setreal ################################