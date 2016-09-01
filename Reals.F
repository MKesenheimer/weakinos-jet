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
        double precision amp, ampreal, ampos
        ! status
        logical verbose
        logical init_reals
        data init_reals/.true./        
        ! kinematics
        double precision s12, sij, skl
        ! mass at resonance, mass of particle i,j,k,l
        double precision mij, mkl, mi, mj, mk, ml
        ! the width of the resonances
        double precision wij, wkl
        integer ichan,i,j,k,l
        ! breit-wigner ratios
        double precision bwratio
        ! local variables
        double precision ampCT
        ! jacobian for the on-shell terms
        double precision corrfac
        external corrfac
        ! function to calculate the squared momentum sum
        double precision momsum3sq, momsum2sq
        external momsum2sq, momsum3sq
#ifdef DEBUG
        double precision s35, s46
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
        call getRealAmps(p,rflav,amp)

#ifdef DEBUG
        ! subtract the on shell contributions
        if(rflav(5).ne.0 .and. rflav(6).ne.0) then
          do ichan=1,cnosres
            ! set the channel-related indices i,j,k and masses mi,mj,mk,mij
            call set_channel(rflav,ichan)
            i = osres_i
            j = osres_j
            k = osres_k
            l = osres_l
            mi = osres_mi
            mj = osres_mj
            mk = osres_mk
            ml = osres_ml
            mij = osres_mij
            mkl = osres_mkl
            wij = osres_wij
            wkl = osres_wkl

            ! calculate the invariants
            s12 = momsum2sq(p(0:3,1),p(0:3,2)) ! invariant mass of particle 1 + 2
            sij = momsum2sq(p(0:3,i),p(0:3,j)) ! invariant mass of particle i + j
            skl = momsum2sq(p(0:3,k),p(0:3,l)) ! invariant mass of particle k + l
            
            ! Prospino scheme for double resonances
            if((s12.ge.(mij+mkl)**2).and.(mij.ge.(mi+mj)).and.(mkl.ge.(mk+ml))) then
              ! calculate the ratio of the breit wigner functions
              bwratio = (mij*(wreg+wij))**2/((sij-mij**2)**2+(mij*(wreg+wij))**2)*
     &                  (mkl*(wreg+wkl))**2/((skl-mkl**2)**2+(mkl*(wreg+wkl))**2)
              ! off_to_on the momenta p to on-shell momenta p_os
              call off_to_on(p,rflav,ichan,p_OS)
              call getRealOSAmps(p_os,rflav,ichan,ampos)
              ampCT = bwratio*ampos
              ! rescale the counter term to fit into the new PS
              !ampCT = corrfac(s12,mi,mj,mk,sij,mij)*ampCT
            else
              ampCT = 0D0
            endif  
            amp = amp - ampCT
          enddo
        endif
#endif

        ! Cancel as/(2pi) associated with amp2. It will be put back by real_ampsq
        ampreal = amp/(st_alpha/(2d0*pi))
        
        ! Print informations
        if(verbose) then
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

#ifdef DEBUG
        ! fill histograms
        if(dsqrt(p(1,5)**2+p(2,5)**2).gt.10D0 .and. dsqrt(p(1,6)**2+p(2,6)**2).gt.10D0) then
          s35 = momsum2sq(p(0:3,3),p(0:3,5))
          s46 = momsum2sq(p(0:3,4),p(0:3,6))
          call histogram("sqrt(s35)",100,400D0,800D0,dsqrt(s35),ampreal)
          call histogram("sqrt(s46)",100,400D0,800D0,dsqrt(s46),ampreal)
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