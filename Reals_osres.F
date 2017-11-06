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
      subroutine  setosresreal(p,rflav,ichan,amp)
        implicit none
#include "PhysPars.h"
#include "pwhg_st.h"
#include "nlegborn.h"
#include "Flags.h"
#include "osres.h"
        double precision p(0:3,nlegreal)
        double precision p_os(0:3,nlegreal)
        integer rflav(nlegreal)
        integer ichan
        ! the real amplitudes
        double precision amp
        ! local variables
        double precision ampCT, ampOS
        ! function to calculate the squared momentum sum
        double precision momsum2sq, momsum3sq
        external momsum2sq, momsum3sq
        ! jacobian for the on-shell terms
        double precision corrfac_ijkl, corrfac_ijk
        external corrfac_ijkl, corrfac_ijk
        ! kinematics
        double precision s12, sij, skl, sijk
        ! mass at resonance, mass of particle i,j,k,l
        double precision wijk, wij, wkl, mijk, mij, mkl, mi, mj, mk, ml
        integer i,j,k,l
        ! breit-wigner ratios
        double precision bwratio
        ! initialization variables
        logical verbose
        logical init_real
        data init_real/.true./
        ! checks
        logical check
        ! output control
        integer warncount1
        data warncount1/0/
#ifdef HISTOGRAMS
        double precision s35, s46, s356
        integer lstr
        character*50 str
#endif
        
#if defined(DSUB_I) || defined(DSUB_II)
        if(init_real) then
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
        
#ifdef DSUB_II
        if(flg_btilde) then
          print*, "setosresreal should not called for Btilde."
          stop
        endif
#endif
        
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        
        ! set the channel-related indices
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
        mijk = osres_mijk
        wijk = osres_wijk

        ! calculate the invariants
        s12 = momsum2sq(p(0:3,1),p(0:3,2)) ! invariant mass of particle 1 + 2
        sij = momsum2sq(p(0:3,i),p(0:3,j)) ! invariant mass of particle i + j
        skl = momsum2sq(p(0:3,k),p(0:3,l)) ! invariant mass of particle k + l
        sijk = momsum3sq(p(0:3,i),p(0:3,j),p(0:3,k))

#ifdef DSUB_II
        ! this is |M_R(res)|**2 (still with on-shell resonances)
        call getRealOSAmps(p,rflav,ichan,amp)
#else
        amp = 0D0
#endif

#define SUBTRACTION
#ifdef SUBTRACTION        
        ! subtraction scheme for double resonances
        if(ichan.ge.1 .and. ichan.le.8 .and. 
     &     (s12.ge.(mij+mkl)**2).and.(mij.ge.(mi+mj)).and.(mkl.ge.(mk+ml))) then
          ! with natural widths in double pole part
          bwratio = (mij**2*mkl**2*wij**2*wkl**2*
     &              (mij**4 + (-mkl**2 + sij + skl)**2 + 
     &              mij**2*(2*mkl**2 - 2*(sij + skl) + 
     &              wij**2) + 2*mij*mkl*wij*wkl + 
     &              mkl**2*wkl**2)*
     &              (mij*(wij + wreg2) + mkl*(wkl + wreg2))**2)/
     &              (((mij**2 - sij)**2 + mij**2*wij**2)*
     &              (mij*wij + mkl*wkl)**2*
     &              ((mkl**2 - skl)**2 + mkl**2*wkl**2)*
     &              (mij**4 + mkl**4 + (sij + skl)**2 + 
     &              2*mij*mkl*(wij + wreg2)*(wkl + wreg2) + 
     &              mij**2*(2*mkl**2 - 2*sij - 2*skl + 
     &              (wij + wreg2)**2) + 
     &              mkl**2*(-2*sij - 2*skl + (wkl + wreg2)**2)))
          ! off_to_on the momenta p to on-shell momenta p_os
          call off_to_on_ijkl(p,rflav,ichan,.true.,.true.,p_os,check)
          call getRealOSAmps(p_os,rflav,ichan,ampOS)
          ampCT = bwratio*ampOS
          ! rescale the counter term to fit into the new PS
          ampCT = corrfac_ijkl(s12,mi,mj,mk,ml,mij,mkl,sij,skl)*ampCT
#ifdef DSUB_II
          amp = amp - ampCT
#else
          amp = amp + ampCT
#endif
        endif

        ! subtraction scheme for single resonant gluino contributions
        if(ichan.ge.9 .and. ichan.le.16 .and. 
     &     (s12.ge.(mijk+ml)**2) .and. (mijk.ge.(mi+mj+mk))) then
          bwratio = (mijk*(wijk+wreg1))**2/((sijk-mijk**2)**2+(mijk*(wijk+wreg1))**2)
          ! off_to_on the momenta p to on-shell momenta p_os
          call off_to_on_ijk(p,rflav,ichan,p_os,check)
          call getRealOSAmps(p_os,rflav,ichan,ampOS)
          ampCT = bwratio*ampOS
          ! rescale the counter term to fit into the new PS (leave sij unconstraint)
          ampCT = corrfac_ijk(s12,mi,mj,mk,ml,mijk,dsqrt(sij),sijk,sij)*ampCT
#ifdef DSUB_II
          amp = amp - ampCT
#else
          amp = amp + ampCT
#endif
        endif
#endif

        if(verbose) then
          print*, "process = ", rflav
          print*, "chan = ", ichan
          print*, "s12  = ", s12
          print*, "on-shell resonant Real amplitude = ", amp
        endif

        if(isnan(amp)) then
          if(warncount1.lt.10) then
            warncount1 = warncount1 + 1
            print*,"got strange value for on-shell Real amplitude...", amp
          elseif(warncount1.eq.10) then
            warncount1 = 11
            print*, "setosresreal: Further output will be suppressed."
          endif
          amp = 0D0
        endif

#ifdef HISTOGRAMS
        ! fill histograms
        if(dsqrt(p(1,5)**2+p(2,5)**2).gt.10D0 .and. dsqrt(p(1,6)**2+p(2,6)**2).gt.10D0) then
          call process_name(nlegreal,rflav(1:nlegreal),str,lstr)
          s35 = momsum2sq(p(0:3,3),p(0:3,5))
          s46 = momsum2sq(p(0:3,4),p(0:3,6))
          s356 = momsum3sq(p(0:3,3),p(0:3,5),p(0:3,6))
          ! warning: don't fill more than 200 histograms!
          if(abs(abs(rflav(1))).le.2) then
            call histogram("sqrt(s35)",100,400D0,800D0,dsqrt(s35),amp)
            !call histogram("sqrt(s46)_"//str(1:lstr),100,400D0,800D0,dsqrt(s46),amp)
            call histogram("sqrt(s356)",100,400D0,800D0,dsqrt(s356),amp)
            !call histogram("sqrt(s356)_ampCT",100,400D0,800D0,dsqrt(s356),ampCT)
            call histogram("int(s35)",1,1D1,1D6,dsqrt(s35),amp)
            !call histogram("int(s46)_"//str(1:lstr),1,1D1,1D6,dsqrt(s46),amp)
            call histogram("int(s356)",1,1D1,1D6,dsqrt(s356),amp)
          endif
        endif
#endif
#else
        amp = 0D0
#endif
      end
c############### end subroutine setosresreal ###########################
