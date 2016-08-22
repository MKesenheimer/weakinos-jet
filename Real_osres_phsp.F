c############### real_osres_phsp.f #####################################
c last modified by MK, 23.05.2016
c weakino pair + jet production
c copied and modified version of disquark/Tools/Procindependent/
c Real_phsp.f

c############### subroutine real_osres_phsp_pwg ########################
c the following subroutine creates a phase-space for 2 -> 3 with
c a tan-mapping for the invariant mass between particles i and j is used
c to calculate the resonant contributions with intermediate resonant squarks.
c The numer "ichan" is an additional channel identifier "ichan" which
c selects the resonance specified in init_processes.F.
c Only perform a tan-mapping if there is a (possible) resonance, 
c otherwise: flat sampling in sij
c this subroutine uses subroutines defined in phi1_2.f (with boosts).
      subroutine real_osres_phsp(xx,ichan)
        implicit none
        
#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
#include "osres.h"

        !include integration over sij -> 6 integration-variables
        double precision xx(ndiminteg)
        ! x1-x2 phase space
        integer psgen
        double precision tau,taumin,taumax
        double precision y,tmp
        ! mandelstams
        double precision s12
        double precision sijkmin, sijmin
        ! jacobian
        double precision jac
        double precision minmass,mi,mj,mk,ml,mij,mkl
        ! indices
        integer ichan,i,j,k,l
        ! variables for pwg routines phi1_2
        double precision pijk(0:3),pij(0:3),pkl(0:3),p12(0:3)
        ! boost
        double precision vec(3),beta
        ! check 4-momentum conservation
        logical lresult
        ! constants
        double precision m_pi
        parameter (m_pi = 4.D0*datan(1.D0))
        
        ! check the channel identifier
        if( ichan.gt.cnosres .or. ichan.lt.0) then
          print*,"wrong channel number: ", ichan
          stop
        endif

        ! set the channel-related indices i,j,k and masses mi,mj,mk,mij
        call set_channel(osresID(ichan),i,j,k,l,mi,mj,mk,ml,mij)
        
        ! reset the jacobians
        jac = 1D0
        kn_jacreal = 1D0
        ! phase space boundary
        minmass = mi+mj+mk+ml
        ! generate the x1-x2-phase space
        ! x1x2phspace(sbeams,minmass,xx,x1,x2,s,jac)
        call x1x2phspace(kn_sbeams,minmass,xx(1:2),kn_x1,kn_x2,
     &                   kn_sreal,jac)
        kn_jacreal = kn_jacreal*jac
        ! for convenience
        s12 = kn_sreal

        ! kinematics of IS-particles already fixed in partonic CMS:
        kn_cmpreal(0,1) = dsqrt(s12)/2D0
        kn_cmpreal(1,1) = 0D0
        kn_cmpreal(2,1) = 0D0
        kn_cmpreal(3,1) = kn_cmpreal(0,1)

        kn_cmpreal(0,2) = kn_cmpreal(0,1)
        kn_cmpreal(1,2) = 0D0
        kn_cmpreal(2,2) = 0D0
        kn_cmpreal(3,2) = -kn_cmpreal(0,1)
  
        p12(:) = kn_cmpreal(:,1)+kn_cmpreal(:,2)
        sijkmin = (mi+mj+mk)**2
        sijmin = (mi+mj)**2
        
        ! phase space for decay p12 -> pl + pijk
        ! R2phsp_s2(psgen,x2,xth,xphi,s2min,m1,bwmass,bwwidth,p0,p1,p2,jac)
        ! without phi integration
        call R2phsp_s2(0,xx(3),xx(4),0D0,sijkmin,ml,0D0,0D0,p12(:),
     &                  kn_cmpreal(:,l),pijk(:),jac)
        kn_jacreal = kn_jacreal*jac
        
        ! phase space for decay pijk -> pk + pij (1 = with breit wigner)
        ! with phi integration
        call R2phsp_s2(1,xx(5),xx(6),xx(7),sijmin,mk,mij,WREG,pijk(:),
     &                  kn_cmpreal(:,k),pij(:),jac)
        kn_jacreal = kn_jacreal*jac

        ! phase space for decay pij -> pi + pj
        ! R2phsp(xth,xphi,m1,m2,p0,p1,p2,jac)
        call R2phsp(xx(8),xx(9),mi,mj,pij(:),kn_cmpreal(:,i),
     &               kn_cmpreal(:,j),jac)
        kn_jacreal = kn_jacreal*jac
        
        ! physical phase space jacobian ((2*Pi)**(3n-4), n=4)
        kn_jacreal = kn_jacreal/(2D0*m_pi)**8

        ! flux factor (the flux factor is included for the born
        ! amplitude in sigborn, but it is not implemented
        ! in sigosres for real processes, so we supply it here)
        kn_jacreal = kn_jacreal/(2D0*s12)

        ! Boost to the LAB frame, starting at vector 1:
        beta = (kn_x1-kn_x2)/(kn_x1+kn_x2)
        vec(1) = 0D0
        vec(2) = 0D0
        vec(3) = 1D0
        call mboost(nlegreal,vec,beta,kn_cmpreal(:,1),kn_preal(:,1))

        ! check four-momentum conservation
        if( kn_jacreal .ne. 0D0) then
          call check_4conservation(kn_preal,nlegreal,2,lresult)
          ! if momenta are not conserved, set jacobian to zero but 
          ! continue with the calculation
          if(.not.lresult) kn_jacreal = 0D0
          return
        endif
      end
c############### end subroutine real_osres_phsp ########################

c############### function pt2max_osres #################################
c this function defines the scale for the radiation from the osres-reals
c per default simply take the value of mur,muf (which are set to a 
c fixed-values if flg_osres is set!)
      double precision function pt2max_osres()
        implicit none
#include "pwhg_st.h"

        ! per default: use the scale of the hard process 
        ! (so far a "running" scale is not supported here!)
        pt2max_osres = dabs(st_mufact2)
#ifdef DEBUGQ
        print*
        print*,"DEBUG, real_osres_phsp.F"
        print*,"pt2max_osres",pt2max_osres
#endif
        return
      end
c############### end function pt2max_osres #############################