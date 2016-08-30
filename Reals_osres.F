c############### Real_osres.f ##########################################
c last modified by MK, 25.11.2015
c weakino pair production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setosresreal ###############################
c this is the routine which gives back |M_R|**2 - BW*corrfac*|M_R^OS|**2
c (the real contributions with a resonant intermediate squark subtracted 
c by on-shell counter terms) evaluated at a phase space point where 
c sij = mij**2 holds.
c the corrfac factor accounts for the restriction of the pahse-space and
c modifies the jacobian.
      subroutine  setosresreal(p,rflav,ichan,ampreal)
        implicit none

#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_math.h"
#include "pwhg_st.h"
#include "Flags.h"
#include "osres.h"

        double precision p(0:3,nlegreal)
        double precision p_os(0:3,nlegreal)
        integer rflav(nlegreal)
        integer ichan
        ! the real amplitudes
        double precision ampreal
        ! local variables
        double precision ampCT, amp, ampOS
        ! function to calculate the squared momentum sum
        double precision momsum3sq, momsum2sq
        external momsum2sq, momsum3sq
        ! jacobian for the on-shell terms
        double precision corrfac
        external corrfac
        ! kinematics
        double precision s12, sij, skl
        ! mass at resonance, mass of particle i,j,k,l
        double precision wij, mij, wkl, mkl, mi, mj, mk, ml
        integer i,j,k,l
        ! breit-wigner ratios
        double precision bwratio
        ! check 4-momentum conservation
        logical lresult
        ! initialization variables
        logical verbose
        logical init_real
        data init_real/.true./
#ifdef DEBUGQ
#define DEBUG1
        ! debugging
        integer counter
        data counter/0/
        save counter
#endif
        
#if defined(DSUB_II) || defined(DSUB_II_TEST)
        if (init_real) then
           print*
           print*, "=== Init resonant real... ==="
           print*, "Computing the integral of the "//
     &             "remnant cross section consisting of "
           print*, "remaining terms from on-shell resonant "//
     &             "reals, incl. on-shell-subtraction "
           print*, "to set up the adaptive grid"
           verbose = .true.
           init_real = .false.
        else
           verbose = .false.
        endif
        
#ifndef DSUB_II_TEST
        if(flg_btilde) then
          print*, "the realresonant-routine should not cald for Btilde."
          stop
        endif
#endif

        ! check four momentum conservation
        call check_4conservation(p, nlegborn+1, 1, lresult)
        
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        
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
        s12  = momsum2sq(p(0:3,1),p(0:3,2)) ! invariant mass of particle 1 + 2
        sij  = momsum2sq(p(0:3,i),p(0:3,j)) ! invariant mass of particle i + j
        skl  = momsum2sq(p(0:3,k),p(0:3,l)) ! invariant mass of particle k + l

        ! this is |M_R(res)|**2 (still with on-shell resonances)
        call getRealOSAmps(p,rflav,ichan,amp)

        ! Prospino scheme for double resonances
        if((s12.ge.(mij+mkl)**2).and.(mij.ge.(mi+mj)).and.(mkl.ge.(mk+ml))) then
          ! calculate the ratio of the breit wigner functions
          bwratio = (mij*(wreg+wij))**2/((sij-mij**2)**2+(mij*(wreg+wij))**2)*
     &              (mkl*(wreg+wkl))**2/((skl-mkl**2)**2+(mkl*(wreg+wkl))**2)
          ! off_to_on the momenta p to on-shell momenta p_os
          call off_to_on(p,rflav,ichan,p_OS)
          call getRealOSAmps(p_os,rflav,ichan,ampos)
          ampCT = bwratio*ampos
          ! rescale the counter term to fit into the new PS
          !ampCT = corrfac(s12,mi,mj,mk,sij,mij)*ampCT
        else
          ampCT = 0D0
        endif 

        ! calculate the total resonant amplitude and substract the
        ! on-shell counter terms -> no on-shell resonances anymore
        ampreal = amp-ampCT
 
        if(isnan(ampreal)) then
          !print*,"got strange value for ampreal amplitude...",ampreal
          ampreal = 0D0
        endif
 
        if(verbose) then
          print*, "process = ", rflav
          print*, "chan = ", ichan
          print*, "s12  = ", s12
          print*, "on-shell resonant Real amplitude = ", ampreal
          print*
        endif
        
#ifdef DEBUG1
        if(counter .lt. 5) then
          print*,"real_osres = ", ampreal
          counter = counter + 1
        endif
#endif
#else
        ampreal = 0d0
#endif
      end
c############### end subroutine setosresreal ###########################