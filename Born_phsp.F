c############### Born.f ################################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### subroutine born_phsp ##################################
c fills the four-momenta of the Born-process particles,
c both in the laboratory and in the center-of-mass frame,
c the Bjorken x of the two incoming partons,
c the partonic Mandelstam variable s,
c the masses for Born and real emission,
c and the Jacobi factor for the phase space volume
      subroutine born_phsp(xborn)
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
        ! input:
        double precision xborn(ndiminteg-3)
        ! output, local variables:
        double precision vec(3),beta
        ! jacobian
        double precision jac
        double precision p12(0:3), pij(0:3)
        integer ind,nu
        ! final state arrangement
        integer i,j,k
        ! check 4-momentum conservation
        logical lresult
        ! phase space borders
        double precision sijmin
        ! function to calculate the squared momentum sum
        double precision dotp, momsq, momsum3sq, momsum2sq
        external dotp, momsq, momsum2sq, momsum3sq
        ! pseudorapidity and pt
        double precision ptjet
        ! output control
        integer warncount1
        data warncount1/0/

        ! final state arrangement
        ! (the order how the phase space is generated)
        i = 3; j = 5; k = 4

        ! reset the jacobians
        jac = 1D0
        kn_jacborn = 1d0

        ! set masses for Born and real emission
        kn_masses(1) = 0D0
        kn_masses(2) = 0D0
        kn_masses(3) = dabs(par_Fin1mass)
        kn_masses(4) = dabs(par_Fin2mass)
        kn_masses(5) = dabs(par_Fin3mass)
        kn_masses(6) = 0D0

        ! lower bound on the mass of the final state 2->3 process
        kn_minmass = kn_masses(3) + kn_masses(4) + kn_masses(5)

        ! calculate the parton momentum fractions kn_xb1, kn_xb2
        ! the squared CM energy of the Born process sborn
        ! and the jacobi factor of the PS volume
        call x1x2phspace(2,kn_sbeams,kn_minmass,xborn(1:2),kn_xb1,kn_xb2,
     &                   kn_sborn,jac)
        kn_jacborn = kn_jacborn*jac

        ! center of mass momenta
        kn_cmpborn(0,1) =  dsqrt(kn_sborn)/2d0
        kn_cmpborn(1,1) =  0d0
        kn_cmpborn(2,1) =  0d0
        kn_cmpborn(3,1) =  kn_cmpborn(0,1)
        
        kn_cmpborn(0,2) =  kn_cmpborn(0,1)
        kn_cmpborn(1,2) =  0d0
        kn_cmpborn(2,2) =  0d0
        kn_cmpborn(3,2) = -kn_cmpborn(0,1)

        p12(:) = kn_cmpborn(:,1) + kn_cmpborn(:,2)
        sijmin = (kn_masses(i)+kn_masses(j))**2
        
        ! generate four-momenta in the cms frame
        ! for the particles produced in the Born process,
        ! calculates the Jacobi factor for the phase space volume
        ! we don't want to integrate over phi, so we provide 0 for xphi
        ! (see phsp_routines.f)
        call R2phsp_s2(0,xborn(3),xborn(4),0D0,sijmin,kn_masses(k),
     &                 0D0,0D0,p12(:),kn_cmpborn(:,k),pij(:),jac)
        kn_jacborn = kn_jacborn*jac
        call R2phsp(xborn(5),xborn(6),kn_masses(i),kn_masses(j),pij(:),
     &              kn_cmpborn(:,i),kn_cmpborn(:,j),jac)
        kn_jacborn = kn_jacborn*jac

        ! generation cut for soft jets
        ! (jet has always index = 5)
        ptjet = dsqrt(kn_cmpborn(1,5)**2+kn_cmpborn(2,5)**2)
        if(ptjet.lt.gcut) then
          kn_jacborn = 0D0
          !return
        endif

        ! physical phase space jacobian ((2*Pi)**(3n-4), n=3)
        kn_jacborn = kn_jacborn/(2D0*m_pi)**5

        ! Boost to the LAB frame, starting at vector 3:
        beta = (kn_xb1-kn_xb2)/(kn_xb1+kn_xb2)
        vec(1) = 0D0
        vec(2) = 0D0
        vec(3) = 1D0
        call mboost(nlegborn-2,vec,beta,kn_cmpborn(:,3),kn_pborn(:,3))

        do nu=0,3
           kn_pborn(nu,1) = kn_xb1*kn_beams(nu,1)
           kn_pborn(nu,2) = kn_xb2*kn_beams(nu,2)
        enddo

        ! check for NaNs
        do ind=1,nlegborn
          do nu=0,3
            if(isnan(kn_pborn(nu,ind)) .or. 
     &         isnan(kn_cmpborn(nu,ind))) then
              if(warncount1.lt.10) then
                warncount1 = warncount1 + 1
                print*, "Warning in Born_phsp: NaN occured."
                print*,"kn_pborn(",nu,",",ind,")   = ",kn_pborn(nu,ind)
                print*,"kn_cmpborn(",nu,",",ind,") = ",kn_cmpborn(nu,ind)
              elseif(warncount1.eq.10) then
                warncount1 = 11
                print*, "born_phsp: Further output will be suppressed."
              endif
              kn_jacborn = 0D0
              return
            endif
          enddo
        enddo

        ! check four momentum conservation
        call check_4conservation(kn_pborn,nlegborn,2,lresult)
        if(.not.lresult) kn_jacborn = 0D0
      end
c############### end subroutine born_phsp ##############################

c############### subroutine born_suppression ###########################
      subroutine born_suppression(fact)
        implicit none
#include "nlegborn.h"
        double precision fact
        double precision powheginput
        external powheginput
        logical init
        data init/.true./
        save init
        if(init) then
           if(powheginput("#bornsuppfact").gt.0) then
              fact = powheginput("bornsuppfact")
              print*, "Using Born suppression factor ", fact
           elseif(powheginput("#ptsupp").gt.0) then
              fact = powheginput("ptsupp")
              print*, "WARNING: ptsupp is deprecated"
              print*, "Replace it with bornsuppfact"
           else
              fact = 1d0  
           endif
           init=.false.
        endif
        
        if(fact.le.0d0) fact = 1d0
      end
c############### end subroutine born_suppression #######################

c############### subroutine set_fac_ren_scales #########################
      subroutine set_fac_ren_scales(muf1,mur1)
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
#include "pwhg_st.h"
#include "pwhg_flg.h"
        ! renamed to prevent conflicts with madgraph parameter mur
        double precision muf1,mur1
        double precision muref
        double precision kn34(0:3)
        double precision ET(6), HT
        logical init
        data init/.true./
        logical runningscale
        save runningscale
        double precision powheginput
        external powheginput

        if(init) then
          if(powheginput("#runningscale").gt.0) then
            if(powheginput("runningscale").ne.1) then
              print*, "you set runningscale to ", runningscale
              print*, "this runningscale value is not allowed"
              stop
            endif
            runningscale = .true.
          else
            runningscale = .false.
          endif
        endif
       
c choose the running scale
c 1: invariant mass of weakino-pair
c 2: sum of transverse energy of final states
#define SCALE 2
        if(runningscale) then
#if SCALE == 1
           kn34(:) = kn_pborn(:,3)+kn_pborn(:,4)
           muref = dsqrt(dabs(kn34(0)**2-kn34(1)**2
     &                      -kn34(2)**2-kn34(3)**2))
#elif SCALE == 2
           if(flg_bornonly) then
             ET(3) = dsqrt(dabs(kn_pborn(1,3)**2 + kn_pborn(2,3)**2 + kn_masses(3)**2))
             ET(4) = dsqrt(dabs(kn_pborn(1,4)**2 + kn_pborn(2,4)**2 + kn_masses(4)**2))
             ET(5) = dsqrt(dabs(kn_pborn(1,5)**2 + kn_pborn(2,5)**2 + kn_masses(5)**2))
             HT = ET(3) + ET(4) + ET(5)
           else
             ET(3) = dsqrt(dabs(kn_preal(1,3)**2 + kn_preal(2,3)**2 + kn_masses(3)**2))
             ET(4) = dsqrt(dabs(kn_preal(1,4)**2 + kn_preal(2,4)**2 + kn_masses(4)**2))
             ET(5) = dsqrt(dabs(kn_preal(1,5)**2 + kn_preal(2,5)**2 + kn_masses(5)**2))
             ET(6) = dsqrt(dabs(kn_preal(1,6)**2 + kn_preal(2,6)**2 + kn_masses(6)**2))
             HT = ET(3) + ET(4) + ET(5) + ET(6)
           endif
           muref = HT/2D0
#endif
        else
           muref = dabs(par_Fin1mass) + dabs(par_Fin2mass)
        endif

        muref = max(muref,dsqrt(2d0))  ! avoid too small values of the scales

        if(init) then
          print*
          print*,"===================================================="
          print*,"Factorization and renormalization scales:"
          if(runningscale) then
            print*,"using running scale and"
#if SCALE == 1
            print*,"invariant mass of neutralinos for muref"
#elif SCALE == 2
            print*,"transverse energy of final states"
#endif
          else
            print*,"using fixed scale and"
            print*,"sum of neutralino masses for muref"
            print*,"muref = ", muref
          endif
          ! NOTE: scales will be multiplied (outside of this function) by
          ! an additional factor if renscfact or facscfact is set > 0
          ! in powheg.input
          print*,"muren = muref * ", st_renfact
          print*,"mufac = muref * ", st_facfact
          print*,"===================================================="
          init = .false.
        endif

        muf1 = muref
        mur1 = muref
      end
c############### end subroutine set_fac_ren_scales #####################
