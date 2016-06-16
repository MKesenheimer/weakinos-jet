c############### Real.f ################################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### subroutine real_phsp ##################################
c fills the four-momenta of the real-process particles,
c both in the laboratory and in the center-of-mass frame,
c the Bjorken x of the two incoming partons,
c the partonic Mandelstam variable s,
c the masses for real and real emission,
c and the Jacobi factor for the phase space volume
      subroutine real_phsp(xreal)
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
#include "pwhg_math.h"
        ! input:
        double precision xreal(ndiminteg)
        ! output, local variables:
        double precision vec(3),beta
        ! jacobian
        double precision jac
        double precision p12(0:3), pijk(0:3), pij(0:3)
        integer ind,nu
        ! final state arrangement
        integer i,j,k
        ! check 4-momentum conservation
        logical lresult
        ! phase space borders
        double precision sijmin, sijkmin
        ! mandelstams
        double precision s
        ! function to calculate the squared momentum sum
        double precision dotp, momsq, momsum3sq, momsum2sq
        external dotp, momsq, momsum2sq, momsum3sq
        ! pseudorapidity and pt
        double precision etajet, ptjet

        ! final state arrangement
        ! (the order how the phase space is generated)
        i = 3; j = 5; k = 4; l = 6

        ! reset the jacobians
        jac = 1D0
        kn_jacreal = 1d0

#ifdef DEBUGQ
        print*,"xreal(1) = ", xreal(1)
        print*,"xreal(2) = ", xreal(2)
        print*,"xreal(3) = ", xreal(3)
        print*,"xreal(4) = ", xreal(4)
        print*,"xreal(5) = ", xreal(5)
        print*,"xreal(6) = ", xreal(6)
        print*,"xreal(7) = ", xreal(7)
        print*,"xreal(8) = ", xreal(8)
        print*,"xreal(9) = ", xreal(9)
#define DEBUG1
#endif

        ! set masses for real and real emission
        kn_masses(1) = 0D0
        kn_masses(2) = 0D0
        kn_masses(3) = dabs(par_Fin1mass)
        kn_masses(4) = dabs(par_Fin2mass)
        kn_masses(5) = dabs(par_Fin3mass)
        kn_masses(6) = 0D0

        ! lower bound on the mass of the final state 2->4 process
        kn_minmass = kn_masses(3) + kn_masses(4) 
     &             + kn_masses(5) + kn_masses(6)

#ifdef DEBUG1
        print*,"minmass = ", kn_minmass
        print*,"sbeams  = ", kn_sbeams
#endif

        ! calculate the parton momentum fractions kn_xb1, kn_xb2
        ! the squared CM energy of the real process sreal
        ! and the jacobi factor of the PS volume
        call x1x2phspace(kn_sbeams,kn_minmass,xreal(1:2),kn_xb1,kn_xb2,
     &                   kn_sreal,jac)
        kn_jacreal = kn_jacreal*jac

        ! center of mass momenta
        kn_cmpreal(0,1) =  dsqrt(kn_sreal)/2d0
        kn_cmpreal(1,1) =  0d0
        kn_cmpreal(2,1) =  0d0
        kn_cmpreal(3,1) =  kn_cmpreal(0,1)
        
        kn_cmpreal(0,2) =  kn_cmpreal(0,1)
        kn_cmpreal(1,2) =  0d0
        kn_cmpreal(2,2) =  0d0
        kn_cmpreal(3,2) = -kn_cmpreal(0,1)

        p12(:) = kn_cmpreal(:,1) + kn_cmpreal(:,2)
        sijkmin = (kn_masses(i)+kn_masses(j)+kn_masses(k))**2
        sijmin = (kn_masses(i)+kn_masses(j))**2
        
        ! phase space for decay p12 -> pl + pijk
        ! R2phsp_s2(psgen,x2,xth,xphi,s2min,m1,bwmass,bwwidth,p0,p1,p2,jac)
        ! without phi integration
        call R2phsp_s2(0,xx(3),xx(4),0D0,sijkmin,kn_masses(l),0D0,0D0,
     &                 p12(:),kn_cmpreal(:,l),pijk(:),jac)
        kn_jacreal = kn_jacreal*jac
        
        ! phase space for decay pijk -> pk + pij (1 = with breit wigner)
        ! with phi integration
        call R2phsp_s2(0,xx(5),xx(6),xx(7),sijmin,kn_masses(k),0D0,0D0,
     &                 pijk(:),kn_cmpreal(:,k),pij(:),jac)
        kn_jacreal = kn_jacreal*jac

        ! phase space for decay pij -> pi + pj
        ! R2phsp(xth,xphi,m1,m2,p0,p1,p2,jac)
        call R2phsp(xx(8),xx(9),kn_masses(i),kn_masses(j),pij(:),
     &              kn_cmpreal(:,i),kn_cmpreal(:,j),jac)
        kn_jacreal = kn_jacreal*jac

        ! generation cut for soft jets
        ! (jet has always index = 5 or 6)
        ptjet5 = dsqrt(kn_cmpreal(1,5)**2+kn_cmpreal(2,5)**2)
        ptjet6 = dsqrt(kn_cmpreal(1,6)**2+kn_cmpreal(2,6)**2)
        if(ptjet5.lt.gcut .or. ptjet6.lt.gcut) then
          kn_jacreal = 0D0
        endif

        ! physical phase space jacobian ((2*Pi)**(3n-4), n=4)
        kn_jacreal = kn_jacreal/(2D0*pi)**8

        ! flux factor (the flux factor is included for the born
        ! amplitude in sigborn, but it is not implemented
        ! in sigosres for real processes, so we supply it here)
        kn_jacreal = kn_jacreal/(2D0*s12)

        ! Boost to the LAB frame, starting at vector 3:
        beta = (kn_xb1-kn_xb2)/(kn_xb1+kn_xb2)
        vec(1) = 0D0
        vec(2) = 0D0
        vec(3) = 1D0
        call mboost(nlegreal-2,vec,beta,kn_cmpreal(:,3),kn_preal(:,3))

        do nu=0,3
           kn_preal(nu,1) = kn_xb1*kn_beams(nu,1)
           kn_preal(nu,2) = kn_xb2*kn_beams(nu,2)
        enddo

        ! check for NaNs
        do ind=1,nlegreal
          do nu=0,3
            if(isnan(kn_preal(nu,ind)) .or. isnan(kn_cmpreal(nu,ind))) then
              print*, "Warning in real_phsp: NaN occured."
              print*,"kn_preal(",nu,",",ind,")   = ",kn_preal(nu,ind)
              print*,"kn_cmpreal(",nu,",",ind,") = ",kn_cmpreal(nu,ind)
              kn_jacreal = 0D0
              return
            endif
          enddo
        enddo

        ! check four momentum conservation
        call check_4conservation(kn_preal,nlegreal,2,lresult)
        if(.not.lresult) kn_jacreal = 0D0
        
#ifdef DEBUG1
        print*,"bjorken x:"
        print*,"kn_xb1 = ", kn_xb1
        print*,"kn_xb2 = ", kn_xb2
        print*,"energy and momenta in lab frame:"
        print*,"E1, pz1  = ", kn_xb1*dsqrt(kn_sbeams)/2
        print*,"E2, -pz2 = ", kn_xb2*dsqrt(kn_sbeams)/2
        print*,"invariant masses"
        print*,"sreal = ", kn_sreal
        print*,"sreal = ", kn_xb1*kn_xb2*kn_sbeams
        print*,"laboratory frame:"
        print*,"kn_preal(1) = ", kn_preal(:,1)
        print*,"kn_preal(2) = ", kn_preal(:,2)
        print*,"kn_preal(3) = ", kn_preal(:,3)
        print*,"kn_preal(4) = ", kn_preal(:,4)
        print*,"kn_preal(5) = ", kn_preal(:,5)
        print*,"center of mass frame:"
        print*,"kn_cmpreal(1) = ", kn_cmpreal(:,1)
        print*,"kn_cmpreal(2) = ", kn_cmpreal(:,2)
        print*,"kn_cmpreal(3) = ", kn_cmpreal(:,3)
        print*,"kn_cmpreal(4) = ", kn_cmpreal(:,4)
        print*,"kn_cmpreal(5) = ", kn_cmpreal(:,5)
        call check_4conservation(kn_preal,nlegreal,2,lresult)
        print*,"four momentum check in lab frame:", lresult
        call check_4conservation(kn_preal,nlegreal,2,lresult)
        print*,"four momentum check in cm frame:", lresult
#endif
      end
c############### end subroutine real_phsp ##############################