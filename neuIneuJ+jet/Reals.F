c############### Reals.f ###############################################
c last modified by MK, 22.01.2015
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
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_math.h"
#include "pwhg_st.h"
#include "Flags.h"
#include "osres.h"
        double precision p(0:3,nlegreal),ps(0:3,nlegreal)
        integer rflav(nlegreal), flavs(nlegreal)
        ! the real amplitudes
        double precision ampreal
        double precision amp
        ! indices
        integer i,j, k, l
#ifdef DSUB_II_TEST
        ! the real resonant amplitudes (testing)
        double precision amposres
        integer ichan
#endif
#ifdef SLICING
        ! kinematics
        double precision sij, skl
        double precision delta
        parameter (delta = 10D0)
#endif
        ! function to calculate the squared momentum sum
        double precision dotp, momsq, momsum3sq, momsum2sq
        external dotp, momsq, momsum2sq, momsum3sq
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
        ! variables to determine the resonant diagrams
        logical detcalled
        data detcalled/.false./

        if(init_real) then
           print*
           print*, "=== Init real... ==="
           verbose = .true.
           init_real = .false.
        else
           verbose = .false.
        endif
        
        ! determine the resonant diagrams
        if(.not.detcalled) then
          detcalled = .true.
          call deterResDiags()
        endif

        ! store flavor list and momenta
        flavs = rflav
        ps(:,:) = p(:,:)
        
        ! check four momentum conservation
        call check_4conservation(ps, nlegborn+1, 1, lresult)
        
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        
        ! get the real amplitude from MadGraph, TODO
        call sreal_proc(ps,flavs,amp)

        ! Cancel as/(2pi) associated with amp2. It will be put back by real_ampsq
        ampreal = amp/(st_alpha/(2d0*pi))

#ifdef SLICING
        ! not tested yet
        do i=3,4
        do k=3,4
          if(i.ne.k) then 
            sij = momsum2sq(ps(0:3,i), ps(0:3,5))
            skl = momsum2sq(ps(0:3,k), ps(0:3,6))
            if(dabs(dsqrt(sij)-m_avrg).lt.delta .and.
     &         dabs(dsqrt(skl)-m_avrg).lt.delta) then
              !print*,i,5,k,6,dsqrt(sij),dsqrt(skl)
              ampreal = 0D0
            endif
          endif
        enddo
        enddo
#endif

#ifdef DSUB_II_TEST
        ! test the DSUB_II scheme (here without the correct jacobian)
        ! this gives the same result as with flag DSUB_I set
        ! if the real amplitude is zero, do not subtract anything
        !if(.not.(ampreal.eq.0d0)) then
          do ichan=1,nosres
            amposres = 0D0
            ! TODO
            !call setosresreal(p,flavs,osresID(ichan),amposres)
            ! Note: amposres has the factor as/(2pi)
            ! included, so cancel as/(2pi) here
            ampreal = ampreal + amposres/(st_alpha/(2d0*pi))
          enddo
        !endif
#endif
        
        if (verbose) then
          print*, "process = ", flavs
          print*, "s12  = ", momsum2sq(ps(0:3,1),ps(0:3,2)) ! invariant mass of particle 1 + 2
          print*, "real = ", ampreal
          print*
        endif

        if(isnan(ampreal)) then
          !print*,"got strange value for ampreal amplitude...",ampreal
          ampreal = 0D0
        endif

#ifdef DEBUG1
        if(counter .lt. 5) then
          print*, "process = ", flavs
          print*, "s12  = ", momsum2sq(ps(0:3,1),ps(0:3,2))
          print*, "real = ", ampreal
          counter = counter + 1
        endif
#endif
      end
c############### end subroutine setreal ################################

c############### subroutine deterResDiags ##############################
c determine the doubly resonant diagrams
      subroutine deterResDiags()
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_flst_add.h"
        double precision p(0:3,nlegreal)
        integer rflav(nlegreal)
        ! the real amplitudes
        double precision ampreal
        ! indices
        integer i, j
        
        print*
        print*,"Determining the resonant diagrams..."
        
        do i=1,flst_nosres
          print*,"...for process: ",flst_osres(:,i)
          do j=1,nlegreal
            rflav(j) = flst_real(j,i)
          enddo
          call setreal(p,rflav,ampreal)
          print*,ampreal
        enddo
      end
c############### end subroutine deterResDiags ##########################

c############### function dsigrealTest #################################
c determine the doubly resonant diagrams
      function dsigrealTest(xreal,rflav)
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_flst.h"
        double precision dsigrealTest
        double precision xreal(ndiminteg)
        double precision p(0:3,nlegreal)
        integer rflav(nlegreal)
        ! the real amplitudes
        double precision ampreal

        

      end
c############### end function dsigrealTest #############################

!  call mint(btilde,ndiminteg,ncall2,itmx2,ifold,imode,iun, xgrid,xint,xacc,nhits,ymax,ymaxrat,sigbtl,errbtl)
