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
      subroutine setreal(p,f,ampreal)
        implicit none
#include "PhysPars.h"
#include "pwhg_st.h"
#include "nlegborn.h"
#include "osres.h"
        double precision p(0:3,nlegreal)
        integer f(nlegreal)
        double precision amp, ampreal, ampos, amposCT
        ! status
        logical verbose
        logical init_reals
        data init_reals/.true./        
        integer ichan
        ! sum only over relevant on-shell channels
        integer nos
        ! output control
        integer warncount1
        data warncount1/0/
c#define CHECKMADFORM
#ifdef CHECKMADFORM
        double precision ampM
#endif

        if (init_reals) then
          print*
          print*, "=== Init reals... ==="
          verbose = .true.
          init_reals = .false.
        else
          verbose = .false.
        endif

        ! calculate the couplings for this PS point
        call set_ebe_couplings

        ! get the formcalc real amplitudes
        call getRealAmps(p,f,amp)

#ifdef CHECKMADFORM
        call sreal_proc(p,f,ampM)
        if(dsqrt(p(1,5)**2+p(2,5)**2).gt.10.and.(amp-ampM)/(amp+ampM).gt.1d-4) then
          print*,f
          print*,"FormCalc = ", amp
          print*,"MadGraph = ", ampM
          print*,(amp-ampM)/(amp+ampM)
          print*
        endif
#endif

#if defined(DSUB_I) || defined(DSUB_II) || defined(DR_II)
        ! check if gluino single resonances can occur (sum over nosres1 + nosres2)
        ! if not, sum over double squark resonances only (sum over nosres1)
        ! NOTE: if you change the following if-statements, you should change them in
        ! sigosres.f, too.
#if defined(NINJ_JET)
        if(f(1).eq.-f(2).and.f(5).eq.-f(6).and.f(1).ne.0.and.f(5).ne.0) then
#elif defined(NIXJ_JET)
        if((f(1).lt.0.and.f(2).gt.0).or.(f(1).gt.0.and.f(2).lt.0)) then
#elif defined(XIXJ_JET)
        if((((f(5).gt.0.and.f(6).lt.0).or.(f(5).lt.0.and.f(6).gt.0))
     &          .and.mod(f(5)+f(6),2).ne.0)) then
#endif
          nos = nosres1 + nosres2
        else
          nos = nosres1
        endif
#if defined(NINJ_JET) || defined(NIXJ_JET)
        if(f(5).ne.0.and.f(6).ne.0) then
#elif defined(XIXJ_JET)
          if(((.not.((f(5).ge.0.and.f(6).ge.0).or.(f(5).le.0.and.f(6).le.0))).or.
     &       (((f(5).gt.0.and.f(6).gt.0).or.(f(5).lt.0.and.f(6).lt.0))
     &          .and.mod(f(5)+f(6),2).ne.0))) then
#endif
          do ichan=1,nos
            ampos = 0D0
#ifndef DSUB_I
            ! common for DSUB_II and DR_II: subtract all resonant amplitudes
            ! DSUB_II: move the resonant amplitudes with a special CT to a separate phase space
            call getRealOSAmps(p,f,ichan,ampos)
            amp = amp - ampos
#else
            ! DSUB_I scheme: subtract the CT without integrating over separate phase space
            ! this should give nearly the same result as with flag DSUB_II set
            ! but with a higher negative weight fraction
            call setosresreal(p,f,ichan,amposCT)
            amp = amp - amposCT
#endif
          enddo
        endif
#endif        

        ! Cancel as/(2pi) associated with amp2. It will be put back by real_ampsq
        ampreal = amp/(st_alpha/(2d0*m_pi))
        
        ! Print informations
        if(verbose) then
          print*, "process: ", f
          print*, "real = ", ampreal
          print*
        endif

        if(isnan(ampreal)) then
          if(warncount1.lt.10) then
            warncount1 = warncount1 + 1
            print*,"got strange value for real amplitude...", ampreal
          elseif(warncount1.eq.10) then
            warncount1 = 11
            print*, "setreal: Further output will be suppressed."
          endif
          ampreal = 0D0
        endif
      end
c############### end subroutine setreal ################################

c############### subroutine realcolour_lh ##############################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      subroutine realcolour_lh
        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        integer f(nlegreal),color(2,nlegreal)
        integer i,j
        do i=1,nlegreal
          f(i)=idup(i)
          if (f(i).eq.21) f(i)=0
        enddo
        call real_color(f,color)
        do i=1,2
          do j=1,nlegreal
            icolup(i,j)=color(i,j)
          enddo
        enddo
      end
c############### end subroutine realcolour_lh ##########################
