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
        double precision amp, ampreal, ampos(2)
        ! status
        logical verbose
        logical init_reals
        data init_reals/.true./
        
        ! MadGraph
        double precision ampM
        
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
        
#ifdef DEBUGQ
        do i=1,nlegreal
          print*,"        rflav(",i,") = ",rflav(i)
          do j=0,3
            print*,"        p(",j,",",i,") = ", p(j,i)
          enddo
        enddo
        stop
#endif

#ifdef DEBUGQ
         rflav(           1 ) =           -4
         p(           0 ,           1 ) =    2433.3361567772117D0
         p(           1 ,           1 ) =    0.0000000000000000D0     
         p(           2 ,           1 ) =    0.0000000000000000D0     
         p(           3 ,           1 ) =    2433.3361567772117D0     
         rflav(           2 ) =           4
         p(           0 ,           2 ) =    2433.3361567772117D0     
         p(           1 ,           2 ) =    0.0000000000000000D0     
         p(           2 ,           2 ) =    0.0000000000000000D0     
         p(           3 ,           2 ) =   -2433.3361567772117D0     
         rflav(           3 ) =      1000022
         p(           0 ,           3 ) =    2059.0938579668482D0     
         p(           1 ,           3 ) =   -740.52967816895693D0     
         p(           2 ,           3 ) =   -1915.0108015757655D0     
         p(           3 ,           3 ) =   -121.93588030733588D0     
         rflav(           4 ) =      1000023
         p(           0 ,           4 ) =    873.20653133569544D0     
         p(           1 ,           4 ) =   -101.96891144413289D0     
         p(           2 ,           4 ) =    839.58697824297883D0     
         p(           3 ,           4 ) =   -119.96988299311880D0     
         rflav(           5 ) =           0
         p(           0 ,           5 ) =    729.35050620199615D0     
         p(           1 ,           5 ) =    479.26306163459833D0     
         p(           2 ,           5 ) =    206.30478262886419D0     
         p(           3 ,           5 ) =   -509.60515628690052D0     
         rflav(           6 ) =           0
         p(           0 ,           6 ) =    1205.0214180498836D0     
         p(           1 ,           6 ) =    363.23552797849158D0     
         p(           2 ,           6 ) =    869.11904070392245D0     
         p(           3 ,           6 ) =    751.51091958735549D0
#endif
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        
#ifndef NEGLECTBMASS
          print*,"ERROR: FormCalc massive b-quark amplitudes not yet "
                 //"implemented."
          stop
#endif

        ! get the formcalc real amplitudes
        call getRealAmps(p,rflav,amp)

#ifdef DEBUGQ
        !print*,rflav
        ! get the real amplitude from MadGraph...
        call sreal_proc(p,rflav,ampM)

        if(2D0*dabs(ampM-amp)/(ampM+amp).gt.1d-4 .and. 
     &    dsqrt(p(1,5)**2+p(2,5)**2).gt. 10D0 .and.
     &    dsqrt(p(1,6)**2+p(2,6)**2).gt. 10D0) then
          print*
          print*,"rflav",rflav
          print*,"pt5",dsqrt(p(1,5)**2+p(2,5)**2)
          print*,"pt6",dsqrt(p(1,6)**2+p(2,6)**2)
          print*,"MadGraph",ampM
          print*,"FormCalc",amp
          print*,"rel error",2D0*dabs(ampM-amp)/(ampM+amp)
          print*,"frac",ampM/amp, amp/ampM
          stop
        endif
#endif
        
!         ! subtract the on shell contributions
!         if((rflav(1).eq.0 .or. rflav(2).eq.0).and.(rflav(5).ne.0)) then
!           do n=1,nosres          
!             ! set the channel-related indices i,j,k and masses mi,mj,mk,mij
!             call set_channel(osresID(n),i,j,k,mi,mj,mk,mij)
!             ! in case a weakino mass is negative
!             mi = dabs(mi)
!             mj = dabs(mj)
!             mk = dabs(mk)
!             mij = dabs(mij)
!             ! calculate the invariants
!             s12 = momsum2sq(p(0:3,1),p(0:3,2)) ! invariant mass of particle 1 + 2
!             sij = momsum2sq(p(0:3,i),p(0:3,j)) ! invariant mass of particle i + j
!             ampCT = 0D0
!             ! Prospino scheme 1211.0286
!             if( (s12.ge.(mij+mk)**2) .and. (mij.ge.(mi+mj)) ) then
!               bwratio = (mij*wreg)**2/((sij-mij**2)**2+(mij*wreg)**2) ! calculate the ratio of the breit wigner functions
!               !bwratio = 0D0
!               call off_to_on(p,osresID(n),p_os)                      ! off_to_on the momenta p to on-shell momenta p_os
!               call getROSAmps(p_os,rflav,osresID(n),
!      &                                  helicities,flags,ampos)
!               ampCT = bwratio*ampos(1)                                ! multiply with the breit-wigner ratio
!               ampCT = corrfac(s12,mi,mj,mk,sij,mij)*ampCT             ! rescale the counter term to fit into the new ps
!             endif
!             fc_result(1) = fc_result(1) - ampCT
!           enddo
!         endif
        
#ifdef DEBUGQ
        ! MadGraph call to real amplitudes
        call sreal_proc(p,rflav,amp)
#endif

        ! Cancel as/(2pi) associated with amp2. It will be put back by real_ampsq
        ampreal = amp/(st_alpha/(2d0*pi))
        
        ! Print informations
        if (verbose) then
          print*, "process: ", rflav
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
          print*, "process: ", rflav
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "real = ", ampreal
          counter = counter + 1
        endif
#endif
      end
c############### end subroutine setreal ################################

c############### subroutine realcolour_lh ##############################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      subroutine realcolour_lh
        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        integer rflav(nlegreal),color(2,nlegreal)
        integer i,j
c        do i=1,nlegreal
c          rflav(i)=idup(i)
c          if (rflav(i).eq.21) rflav(i)=0
c        enddo
c        call real_color(rflav,color)
c        do i=1,2
c          do j=1,nlegreal
c            icolup(i,j)=color(i,j)
c          enddo
c        enddo
#ifdef DEBUG
        print*,"[DEBUG] in realcolour_lh"
        stop
#endif
      end
c############### end subroutine realcolour_lh ##########################