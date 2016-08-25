c############### subroutine off_to_on ##################################
c remaps p to on-shell momenta
c sufficient for diagram subtraction scheme
c this subroutine is in principal a 3 particle phase-space generator 
c with an additional condition, namely sij = mij**2
c
c kinematics:
c              pi
c            /
c           /
c          /---- pj
c   ____  /    !
c  |    |/ sij = mij**2
c  | s  |      !
c  |____|\ skl = mkl**2
c         \
c          \---- pk
c           \
c            \
c              pl
c
c restframe Rij:
c
c            pi'
c            /
c           / 
c          /_xi_ _ _ _
c         /
c        /
c       /
c      pj'
c
c
c
c laboratory frame Rh (hadronic restframe)
c
c      pi     pj
c       \     /
c        \   /
c         \ /
c ========> <=========  
c         / \
c        /   \
c       /     \
c      pk     pl
c
      subroutine off_to_on(p,chan,p_OS)
        implicit none
#include "nexternal.inc"
#include "nlegborn.h"
#include "pwhg_kn.h"
#include "osres.h"
        ! momenta from PS-generator, on-shell momenta
        double precision p(0:3,nexternal),p_OS(0:3,nexternal)
        ! mass at resonance, mass of particle i,j,k,l
        double precision mij, mkl, mi, mj, mk, ml
        ! if you choose i=3, j=5, k=4, l=6:
        ! particle 3&5 and 4&6 will generate the resonances with the 
        ! intermediate particle with mass mij and mkl
        integer i,j,k,l
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        double precision momsq, momsum2sq, momsum3sq, dotp
        external momsq, momsum2sq, momsum3sq, dotp
        ! constants
        double precision m_pi
        parameter (m_pi = 4.D0*datan(1.D0))
        ! local variables
        double precision ratio
        ! momenta in lab. frame: p12 = p1+p2, pij = pi+pj, pkl = pk+pl
        double precision p12(0:3), pij(0:3), pkl(0:3)
        ! momenta in restframe of resonant particle
        double precision piRij(0:3), pjRij(0:3)
        double precision pkRkl(0:3), plRkl(0:3)
        ! reshuffeld momenta in lab. frame (see CS-paper)
        double precision pij_tilde(0:3), pkl_tilde(0:3)
        double precision pi_tilde(0:3), pj_tilde(0:3)
        double precision pk_tilde(0:3), pl_tilde(0:3)
        ! indices
        integer sumi, sumj, mu
        ! invariants
        double precision s, sqrtS, sij, skl
        ! boost from lab. frame into rest frame of particle i,j and k,l
        double precision betaRij, vecRij(3)
        double precision betaRkl, vecRkl(3)
        ! norm
        double precision norm
        ! tests
        double precision relerror
        ! small parameters
        double precision eps
        parameter(eps=1D-8)
        ! angles of particle i and j
        double precision cosQ, phi
        ! abs. momentum in x-y-plane
        double precision pxy
        ! check 4-momentum conservation
        logical lresult
        
        ! set the channel-related indices i,j,k,l and masses mi,mj,mk,ml,mij
        call set_channel(chan,i,j,k,l,mi,mj,mk,ml,mij)
        
        if( nexternal .ne. 6) then
          print*, "error in subroutine off_to_on"
          print*, "nexternal = ", nexternal
          stop
        endif
        
        ! check momentum conservation
        call check_4conservation(p,nexternal,1,lresult)
        
        ! set invariant masses       
        s     = momsum2sq(p(:,1),p(:,2))   ! Q2 in CS-paper
        sqrtS = dsqrt(S)
        sij   = momsum2sq(p(:,i),p(:,j))
        skl   = momsum2sq(p(:,k),p(:,l))
        mkl   = dsqrt(skl)
        
        ! check if theta function was properly used
        if(sqrtS - mij - mkl .lt. 0D0) then
          print*, "error in subroutine off_to_on"
          print*, "intermediate particle is not on-shell"
          print*, "mi,mj,mk,ml =", mi, mj, mk, ml
          print*, "sqrtS    = ", sqrtS
          print*, "mij + mkl = ", mij + mkl
          print*, "Did you forget the Theta-function?"
          stop
        endif
        
        ! Catani-Seymour reshuffling for the 2->3-kinematic
        ! see paper "The Dipole Formalism for Next-to-Leading
        ! Order QCD Calculations with Massive Partons" hep-ph/0201036.
        
        ! set aux. momenta
        do mu = 0,3
          p12(mu) = p(mu,1) + p(mu,2) ! Q(mu) in CS-paper
          pij(mu) = p(mu,i) + p(mu,j)
          pkl(mu) = p(mu,k) + p(mu,l)
        enddo

        ! definition of the momenta pij_tilde and pkl_tilde in terms of  
        ! the original momenta pi, pj and pkl
        ratio = kaellenSqrt(s,mij**2,mk**2)/kaellenSqrt(s,sij,mk**2)
        do mu = 0,3
          pkl_tilde(mu)  = ratio*(pkl(mu)-dotp(p12,pkl)/s*p12(mu))
     &                    +(s+mk**2-mij**2)/(2*s)*p12(mu)
          pij_tilde(mu) = p12(mu)-pkl_tilde(mu)
        enddo
        
        ! boost into the rest frame Rij of particle i and j
        norm      = dsqrt(pij(1)**2+pij(2)**2+pij(3)**2)
        betaRij   = -norm/pij(0)
        vecRij(1) = pij(1)/norm
        vecRij(2) = pij(2)/norm
        vecRij(3) = pij(3)/norm
        
        ! boost into the rest frame Rkl of particle k and l
        norm      = dsqrt(pkl(1)**2+pkl(2)**2+pkl(3)**2)
        betaRkl   = -norm/pkl(0)
        vecRkl(1) = pkl(1)/norm
        vecRkl(2) = pkl(2)/norm
        vecRkl(3) = pkl(3)/norm
        
#define DIRECTION_I
#ifdef DIRECTION_I
        ! keep the direction of particle i
        call mboost(1,vecRij,betaRij,p(:,i),piRij(:))
        norm = dsqrt(piRij(1)**2+piRij(2)**2+piRij(3)**2)
        ! get the angular info of particle i
        if( norm .lt. eps) then
          CosQ = 1D0
          phi  = 0D0
        else
          CosQ = piRij(3)/norm
          pxy   = dsqrt(piRij(1)**2+piRij(2)**2)
          if(dabs(pxy).lt. eps) then
            phi = 0D0
          else if(piRij(1).ge.0.and.piRij(2).ge.0) then
               phi = dacos(piRij(1)/pxy)
          else if(piRij(1).ge.0.and.piRij(2).lt.0) then
               phi = 2d0*m_pi-dacos(piRij(1)/pxy)
          else if(piRij(1).lt.0.and.piRij(2).ge.0) then
               phi = m_pi-dacos(dabs(piRij(1))/pxy)
          else
               phi = m_pi+dacos(dabs(piRij(1))/pxy)
          endif
        endif
        ! construct the new momenta of the particles i and j in Rij,
        ! with restriction Q**2 = mij**2
        piRij(0) = (mij**2+mi**2-mj**2)/(2d0*mij)
        pjRij(0) = (mij**2+mj**2-mi**2)/(2d0*mij)
        ! set 3-momenta
        norm = kaellenSqrt(mij**2,mi**2,mj**2)/(2d0*mij)
        piRij(1) = norm*dsqrt(1d0-CosQ**2)*dcos(phi)
        piRij(2) = norm*dsqrt(1d0-CosQ**2)*dsin(phi)
        piRij(3) = norm*CosQ
        do sumi=1,3
          pjRij(sumi) = -piRij(sumi)
        enddo
#endif
#ifdef DIRECTION_J
        ! keep the direction of particle j
        call mboost(1,vecRij,betaRij,p(:,j),pjRij(:))
        norm = dsqrt(pjRij(1)**2+pjRij(2)**2+pjRij(3)**2)
        ! get the angular info of particle i
        if( norm .lt. eps) then
          CosQ = 1D0
          phi  = 0D0
        else
          CosQ = pjRij(3)/norm
          pxy   = dsqrt(pjRij(1)**2+pjRij(2)**2)
          if(dabs(pxy).lt. eps) then
            phi = 0D0
          else if(pjRij(1).ge.0.and.pjRij(2).ge.0) then
               phi = dacos(pjRij(1)/pxy)
          else if(pjRij(1).ge.0.and.pjRij(2).lt.0) then
               phi = 2d0*m_pi-dacos(pjRij(1)/pxy)
          else if(pjRij(1).lt.0.and.pjRij(2).ge.0) then
               phi = m_pi-dacos(dabs(pjRij(1))/pxy)
          else
               phi = m_pi+dacos(dabs(pjRij(1))/pxy)
          endif
        endif
        ! construct the new momenta of the particles i and j in Rij:
        pjRij(0) = (mij**2+mj**2-mi**2)/(2d0*mij)
        piRij(0) = (mij**2+mi**2-mj**2)/(2d0*mij)
        ! set 3-momenta
        norm = kaellenSqrt(mij**2,mi**2,mj**2)/(2d0*mij)
        pjRij(1) = norm*dsqrt(1d0-CosQ**2)*dcos(phi)
        pjRij(2) = norm*dsqrt(1d0-CosQ**2)*dsin(phi)
        pjRij(3) = norm*CosQ
        do sumi=1,3
          piRij(sumi) = -pjRij(sumi)
        enddo
#endif
#define DIRECTION_K
#ifdef DIRECTION_K
        ! keep the direction of particle k
        call mboost(1,vecRkl,betaRkl,p(:,k),pkRkl(:))
        norm = dsqrt(pkRkl(1)**2+pkRkl(2)**2+pkRkl(3)**2)
        ! get the angular info of particle k
        if( norm .lt. eps) then
          CosQ = 1D0
          phi  = 0D0
        else
          CosQ = pkRkl(3)/norm
          pxy   = dsqrt(pkRkl(1)**2+pkRkl(2)**2)
          if(dabs(pxy).lt. eps) then
            phi = 0D0
          else if(pkRkl(1).ge.0.and.pkRkl(2).ge.0) then
               phi = dacos(pkRkl(1)/pxy)
          else if(pkRkl(1).ge.0.and.pkRkl(2).lt.0) then
               phi = 2d0*m_pi-dacos(pkRkl(1)/pxy)
          else if(pkRkl(1).lt.0.and.pkRkl(2).ge.0) then
               phi = m_pi-dacos(dabs(pkRkl(1))/pxy)
          else
               phi = m_pi+dacos(dabs(pkRkl(1))/pxy)
          endif
        endif
        ! construct the new momenta of the particles k and l in Rkl,
        ! with restriction Q**2 = skl**2
        pkRkl(0) = (mkl**2+mk**2-ml**2)/(2d0*mkl)
        plRkl(0) = (mkl**2+ml**2-mk**2)/(2d0*mkl)
        ! set 3-momenta
        norm = kaellenSqrt(mkl**2,mk**2,ml**2)/(2d0*mkl)
        pkRkl(1) = norm*dsqrt(1d0-CosQ**2)*dcos(phi)
        pkRkl(2) = norm*dsqrt(1d0-CosQ**2)*dsin(phi)
        pkRkl(3) = norm*CosQ
        do sumi=1,3
          plRkl(sumi) = -pkRkl(sumi)
        enddo
#endif
#ifdef DIRECTION_L
        ! keep the direction of particle l
        call mboost(1,vecRkl,betaRkl,p(:,l),plRkl(:))
        norm = dsqrt(plRkl(1)**2+plRkl(2)**2+plRkl(3)**2)
        ! get the angular info of particle l
        if( norm .lt. eps) then
          CosQ = 1D0
          phi  = 0D0
        else
          CosQ = plRkl(3)/norm
          pxy   = dsqrt(plRkl(1)**2+plRkl(2)**2)
          if(dabs(pxy).lt. eps) then
            phi = 0D0
          else if(plRkl(1).ge.0.and.plRkl(2).ge.0) then
               phi = dacos(plRkl(1)/pxy)
          else if(plRkl(1).ge.0.and.plRkl(2).lt.0) then
               phi = 2d0*m_pi-dacos(plRkl(1)/pxy)
          else if(plRkl(1).lt.0.and.plRkl(2).ge.0) then
               phi = m_pi-dacos(dabs(plRkl(1))/pxy)
          else
               phi = m_pi+dacos(dabs(plRkl(1))/pxy)
          endif
        endif
        ! construct the new momenta of the particles k and l in Rkl,
        ! with restriction Q**2 = skl**2
        plRkl(0) = (mkl**2+ml**2-mk**2)/(2d0*mkl)
        pkRkl(0) = (mkl**2+mk**2-ml**2)/(2d0*mkl)
        ! set 3-momenta
        norm = kaellenSqrt(mkl**2,mk**2,ml**2)/(2d0*mkl)
        plRkl(1) = norm*dsqrt(1d0-CosQ**2)*dcos(phi)
        plRkl(2) = norm*dsqrt(1d0-CosQ**2)*dsin(phi)
        plRkl(3) = norm*CosQ
        do sumi=1,3
          pkRkl(sumi) = -plRkl(sumi)
        enddo
#endif
        
        ! boost back into lab. frame with reshuffeld momenta:
        norm = dsqrt(pij_tilde(1)**2+pij_tilde(2)**2+pij_tilde(3)**2)
        betaRij   = norm/pij_tilde(0)
        vecRij(1) = pij_tilde(1)/norm
        vecRij(2) = pij_tilde(2)/norm
        vecRij(3) = pij_tilde(3)/norm
        
        call mboost(1,vecRij,betaRij,piRij(:),pi_tilde(:))
        call mboost(1,vecRij,betaRij,pjRij(:),pj_tilde(:))
        
        ! boost back into lab. frame with reshuffeld momenta:
        norm = dsqrt(pkl_tilde(1)**2+pkl_tilde(2)**2+pkl_tilde(3)**2)
        betaRkl   = norm/pkl_tilde(0)
        vecRkl(1) = pkl_tilde(1)/norm
        vecRkl(2) = pkl_tilde(2)/norm
        vecRkl(3) = pkl_tilde(3)/norm
        
        call mboost(1,vecRkl,betaRkl,pkRkl(:),pk_tilde(:))
        call mboost(1,vecRkl,betaRkl,plRkl(:),pl_tilde(:))
        
        ! set the on-shell momenta
        do mu=0,3
          ! no changes for initial state particles
          p_OS(mu,1) = p(mu,1)
          p_OS(mu,2) = p(mu,2)
          p_OS(mu,i) = pi_tilde(mu)
          p_OS(mu,j) = pj_tilde(mu)
          p_OS(mu,k) = pk_tilde(mu)
          p_OS(mu,l) = pl_tilde(mu)
        enddo

        ! - checks -
        ! check on-shell condition
        relerror = (momsum2sq(p_OS(:,i),p_OS(:,j))-mij**2)
     &             /(momsum2sq(p_OS(:,i),p_OS(:,j))+mij**2) 
        if( dabs(relerror) .gt. 1D-6 ) then
          print*,"error: no on-shell condition found."
          print*,"(p_OSi+p_OSj)**2 = ",momsum2sq(p_OS(:,i),p_OS(:,j))
          print*,"mij**2           = ", mij**2
          print*,"relerror         = ", relerror
          stop
        endif  
        
        ! check if NaN's occur
        do sumi=0,3
          do sumj=1,nexternal
            if( isnan(p_OS(sumi,sumj)) ) then
              print*,"got strange value for p_OS..."
              print*,"p1",p(:,1)
              print*,"p2",p(:,2)
              print*,"pi",p(:,i)
              print*,"pj",p(:,j)
              print*,"pk",p(:,k)
              print*,"pk",p(:,l)
              print*,"p1_OS",p_OS(:,1)
              print*,"p2_OS",p_OS(:,2)
              print*,"pi_OS",p_OS(:,i)
              print*,"pj_OS",p_OS(:,j)
              print*,"pk_OS",p_OS(:,k)
              print*,"pk_OS",p_OS(:,l)
              stop
            endif
          enddo
        enddo
        
        ! check four momentum conservation
        call check_4conservation(p_OS,nlegreal,1,lresult)
      end
c############### end subroutine off_to_on ##############################

c############### function corrfac ######################################
c the remapping requires a change in the PS integration
c and every counter term which uses the on-shell momenta should be
c rescaled by this correction factor
      double precision function corrfac(shat,mi,mj,mk,sij,mij)
        implicit none
#include "nexternal.inc"
#include "nlegborn.h"
#include "pwhg_kn.h"
        !input variables
        double precision shat,mi,mj,mk,sij,mij
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        
        corrfac = (sij*kaellenSqrt(shat,mij**2,mk**2)
     &                     *kaellenSqrt(mij**2,mi**2,mj**2))
     &           /(mij**2*kaellenSqrt(shat,sij,mk**2)
     &                     *kaellenSqrt(sij,mi**2,mj**2))

#ifdef DSUB_II_TEST
        corrfac = 1D0
#endif

#ifdef DEBUGQ
        corrfac = 1D0
        !corrfac = corrfac**(-1D0)
#endif

#ifdef DEBUGQ
        print*,"corrfac",corrfac
#endif
      end  
c############### end function corrfac ##################################

c############### subroutine set_channel ################################
c this subroutine sets the indices i,j,k,l and the masses mi,mj,mk,ml,
c and mij in dependence of the chan-identifier (NOTE: chan must be a
c character-string). The arrays osresID, osreslegs and osresM must
c be initialized in init_couplings and in init_processes.
c
c              pi
c            /
c           /
c          /---- pj
c   ____  /    !
c  |    |/ sij = mij**2
c  | s  |      !
c  |____|\ skl = mkl**2
c         \
c          \---- pk
c           \
c            \
c              pl
c
      subroutine set_channel(chan,i,j,k,l,mi,mj,mk,ml,mij)
        implicit none
#include "PhysPars.h"
#include "nexternal.inc"
#include "nlegborn.h"
#include "pwhg_math.h"
#include "pwhg_kn.h"
#include "osres.h"
        ! indices
        integer i,j,k,l, ichan
        ! masses
        double precision mij,mkl, mi, mj, mk, ml
        double precision momsq, momsum2sq, momsum3sq, dotp
        external momsq, momsum2sq, momsum3sq, dotp
        integer levi_civita
        external levi_civita
        
        do ichan=1,cnosres
          if(chan.eq.osresID(ichan)) then
            i = osreslegs(ichan,1)
            j = osreslegs(ichan,2)
            k = osreslegs(ichan,3)
            l = osreslegs(ichan,4)
            mij = osresM(ichan)
            if(i.eq.3 .and. j.eq.5 .and. k.eq.4 .and. l.eq.6) then
              mi = par_Fin1mass
              mj = 0D0
              mk = par_Fin2mass
              ml = 0D0
            elseif(i.eq.3 .and. j.eq.6 .and. k.eq.4 .and. l.eq.5) then
              mi = par_Fin1mass
              mj = 0D0
              mk = par_Fin2mass
              ml = 0D0
            else
              print*,"error in set_channel: i,j,k,l",i,j,k,l
              stop
            endif
            goto 10
          endif
        enddo

 10     continue
        ! in case a weakino masses are negative
        mi = dabs(mi)
        mj = dabs(mj)
        mk = dabs(mk)
        ml = dabs(ml)
        mij = dabs(mij)

        ! checks
        if(i.eq.0 .or. j.eq.0 .or. k.eq.0 .or. l.eq.0) then
          print*,"chan ",chan
          print*,"osreslegs(1)",osreslegs(:,1)
          print*,"osreslegs(2)",osreslegs(:,2)
          print*,"osreslegs(3)",osreslegs(:,3)
          print*,"osreslegs(4)",osreslegs(:,4)
          print*,"set_channel: got strang values for i,j,k,l:",i,j,k,l
          stop
        endif
        
#ifdef DEBUGQ
        print*,"chan",chan
        print*,"i,j,k,l",i,j,k,l
        print*,"mij,mi,mj,mk,ml",mij,mi,mj,mk,ml
        print*,"par_FinMasses",par_Fin1mass,par_Fin2mass
        !stop
#endif
      end
c############### end subroutine set_channel ############################
