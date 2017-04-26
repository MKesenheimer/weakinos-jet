c############### subroutine off_to_on_ijkl #############################
c remaps p to on-shell momenta for 4 particle final state
c sufficient for diagram subtraction scheme
c this subroutine is in principal a 4 particle phase-space generator 
c with an additional condition, namely 
c sij = mij**2 (with flag_ij = true) and 
c skl = mkl**2 (with flag_kl = true)
c
c kinematics:
c              pi
c            /
c           /
c          /---- pj
c   ____  /    !
c  |    |/ sij = mij**2 (if flag_ij = true)
c  | s  |      !
c  |____|\ skl = mkl**2 (if flag_kl = true)
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
c restframe Rkl:
c
c            pk'
c            /
c           / 
c          /_xk_ _ _ _
c         /
c        /
c       /
c      pl'
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
      subroutine off_to_on_ijkl(p,flav,ichan,flag_ij,flag_kl,p_OS)
        implicit none
#include "nlegborn.h"
#include "osres.h"
        ! momenta from PS-generator, on-shell momenta
        double precision p(0:3,nlegreal),p_OS(0:3,nlegreal)
        integer ichan, flav(nlegreal)
        logical flag_ij, flag_kl
        ! mass at resonance
        double precision mij,mkl,mi,mj,mk,ml
        integer i,j,k,l
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        double precision momsq, momsum2sq, momsum3sq, dotp
        external momsq, momsum2sq, momsum3sq, dotp
        ! local variables
        double precision ratio
        ! momenta in lab. frame: p12 = p1+p2, pkl = pk+pl
        double precision p12(0:3), pkl(0:3)
        double precision pij_OS(0:3), pkl_OS(0:3)
        ! momenta in restframe of resonant particle
        double precision piRij(0:3), pjRij(0:3)
        double precision pkRkl(0:3), plRkl(0:3)
        ! reshuffeld momenta in lab. frame (see CS-paper)
        double precision pij_tilde(0:3), pkl_tilde(0:3)
        double precision pi_tilde(0:3), pj_tilde(0:3)
        double precision pk_tilde(0:3), pl_tilde(0:3)
        ! invariants
        double precision s, sqrtS, sij, skl
        ! boost from lab. frame into rest frame of particle i,j and k,l
        double precision betaRij, vecRij(3)
        double precision betaRkl, vecRkl(3)
        double precision norm
        ! check 4-momentum conservation
        logical lresult
        
        ! set the channel-related indices i,j,k,l and masses mi,mj,mk,ml,mij
        call set_channel(flav,ichan)
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
        
        if(nlegreal.ne.6) then
          print*, "error in subroutine off_to_on"
          print*, "nlegreal = ", nlegreal
          stop
        endif
        
        if(.not.flag_ij.and..not.flag_kl) then
          print*, "error in subroutine off_to_on"
          print*, "no subtraction necessary"
          print*, "flag_ij = ",flag_ij
          print*, "flag_kl = ",flag_kl
          stop
        endif
        
        ! set aux. momenta
        p12(:) = p(:,1) + p(:,2) ! Q(mu) in CS-paper
        pkl(:) = p(:,k) + p(:,l)
        ! set invariant masses       
        s     = momsum2sq(p(:,1),p(:,2))   ! Q2 in CS-paper
        sqrtS = dsqrt(S)
        sij   = momsum2sq(p(:,i),p(:,j))
        skl   = momsum2sq(p(:,k),p(:,l))
        
        ! check if theta function was properly used
        if(((s.lt.(mij+mkl)**2).and.flag_ij.and.flag_kl) .or.
     &     ((s.lt.(mij+mk+ml)**2).and.flag_ij.and..not.flag_kl) .or.
     &     ((s.lt.(mi+mj+mkl)**2).and..not.flag_ij.and.flag_kl)) then
          print*, "error in subroutine off_to_on"
          print*, "intermediate particle is not on-shell"
          print*, "sqrtS    = ", sqrtS
          print*, "mij + mkl = ", mij + mkl
          print*, "Did you forget the Theta-function?"
          stop
        endif
        
        ! decide where to apply the on-shell condition
        if(.not.flag_ij) mij = dsqrt(sij)
        if(.not.flag_kl) mkl = dsqrt(skl)
         
        ! Catani-Seymour reshuffling for the 2->4-kinematic
        ! see paper "The Dipole Formalism for Next-to-Leading
        ! Order QCD Calculations with Massive Partons" hep-ph/0201036.        
        ! definition of the momenta pij_tilde and pkl_tilde in terms of  
        ! the original momenta pi, pj, pk and pl
        ratio = kaellenSqrt(s,mij**2,mkl**2)/kaellenSqrt(s,sij,skl)
        pkl_tilde(:)  = ratio*(pkl(:)-dotp(p12(:),pkl(:))/s*p12(:))
     &                  +(s+mkl**2-mij**2)/(2*s)*p12(:)
        pij_tilde(:) = p12(:)-pkl_tilde(:)
        
        ! calculate the momenta pi and pj in cms Rij of particle i and j
        ! and apply the on-shell condition sij = mij**2
#define DIRECTION_I
#ifdef DIRECTION_I
        call mom_in_Rcms(mij**2,p(:,i),p(:,j),piRij(:),pjRij(:))
#endif
#ifdef DIRECTION_J
        call mom_in_Rcms(mij**2,p(:,j),p(:,i),pjRij(:),piRij(:))
#endif

        ! calculate the momenta pk and pl in cms Rkl of particle k and l
        ! and apply the on-shell condition skl = mkl**2
#define DIRECTION_L
#ifdef DIRECTION_K
        call mom_in_Rcms(mkl**2,p(:,k),p(:,l),pkRkl(:),plRkl(:))
#endif
#ifdef DIRECTION_L
        call mom_in_Rcms(mkl**2,p(:,l),p(:,k),plRkl(:),pkRkl(:))
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
        ! no changes for initial state particles
        p_OS(:,1) = p(:,1)
        p_OS(:,2) = p(:,2)
        p_OS(:,i) = pi_tilde(:)
        p_OS(:,j) = pj_tilde(:)
        p_OS(:,k) = pk_tilde(:)
        p_OS(:,l) = pl_tilde(:)

#ifdef DEBUGQ
        if(.not.flag_ij.or..not.flag_kl) then

        norm = dsqrt(pkl_tilde(1)**2+pkl_tilde(2)**2+pkl_tilde(3)**2)
        betaRkl   = norm/pkl_tilde(0)
        vecRkl(1) = pkl_tilde(1)/norm
        vecRkl(2) = pkl_tilde(2)/norm
        vecRkl(3) = pkl_tilde(3)/norm
        print*,vecRkl(:)
        
        norm = dsqrt(pkl(1)**2+pkl(2)**2+pkl(3)**2)
        betaRkl   = norm/pkl(0)
        vecRkl(1) = pkl(1)/norm
        vecRkl(2) = pkl(2)/norm
        vecRkl(3) = pkl(3)/norm
        print*,vecRkl(:)
        
        print*,"p1",p(:,1), dsqrt(dabs(momsq(p(:,1))))
        print*,"p2",p(:,2), dsqrt(dabs(momsq(p(:,2))))
        print*,"pi",p(:,i), dsqrt(dabs(momsq(p(:,i))))
        print*,"pj",p(:,j), dsqrt(dabs(momsq(p(:,j))))
        print*,"pk",p(:,k), dsqrt(dabs(momsq(p(:,k))))
        print*,"pl",p(:,l), dsqrt(dabs(momsq(p(:,l))))
        print*,"piRij",piRij(:), dsqrt(dabs(momsq(piRij(:))))
        print*,"pjRij",piRij(:), dsqrt(dabs(momsq(pjRij(:))))
        print*,"pkRkl",pkRkl(:), dsqrt(dabs(momsq(pkRkl(:))))
        print*,"plRkl",plRkl(:), dsqrt(dabs(momsq(plRkl(:))))
        print*,"pt(i)",dsqrt(p(1,i)**2+p(2,i)**2)
        print*,"pt(j)",dsqrt(p(1,j)**2+p(2,j)**2)
        print*,"pt(k)",dsqrt(p(1,k)**2+p(2,k)**2)
        print*,"pt(l)",dsqrt(p(1,l)**2+p(2,l)**2)
        print*,"pij_tilde",pij_tilde(:),dsqrt(dabs(momsq(pij_tilde(:))))
        print*,"pkl_tilde",pkl_tilde(:),dsqrt(dabs(momsq(pkl_tilde(:))))
        print*,"p1_OS",p_OS(:,1), dsqrt(dabs(momsq(p_OS(:,1))))
        print*,"p2_OS",p_OS(:,2), dsqrt(dabs(momsq(p_OS(:,2))))
        print*,"pi_OS",p_OS(:,i), dsqrt(dabs(momsq(p_OS(:,i))))
        print*,"pj_OS",p_OS(:,j), dsqrt(dabs(momsq(p_OS(:,j))))
        print*,"pk_OS",p_OS(:,k), dsqrt(dabs(momsq(p_OS(:,k))))
        print*,"pl_OS",p_OS(:,l), dsqrt(dabs(momsq(p_OS(:,l))))
        print*,"|pij| = ", dsqrt(dabs(momsum2sq(p(:,i),p(:,j))))
        print*,"|pkl| = ", dsqrt(dabs(momsum2sq(p(:,k),p(:,l))))
        print*,"|pij_OS| = ", dsqrt(dabs(momsum2sq(p_OS(:,i),p_OS(:,j))))
        print*,"|pkl_OS| = ", dsqrt(dabs(momsum2sq(p_OS(:,k),p_OS(:,l))))
        print*
        !stop
        endif
#endif
        
        ! - checks -
        ! check on-shell condition
        if(flag_ij) then
          pij_OS(:) = p_OS(:,i) + p_OS(:,j)
          call check_on_shell(pij_OS(:),mij)
        endif
        if(flag_kl) then
          pkl_OS(:) = p_OS(:,k) + p_OS(:,l)
          call check_on_shell(pkl_OS(:),mkl)
        endif
        ! check four momentum conservation
        call check_4conservation(p_OS,nlegreal,3,lresult)
      end
c############### end subroutine off_to_on_ijkl #########################

c############### subroutine off_to_on_ijk ##############################
c remaps p to on-shell momenta for 4 particle final state
c sufficient for diagram subtraction scheme
c this subroutine is in principal a 4 particle phase-space generator 
c with an additional condition, namely sijk = mijk**2
c
c kinematics:
c           pl
c          /
c   ____  /
c  |    |/ 
c  | s  |      !          pi 
c  |____|\ sijk=mijk**2  /
c     mijk\      pij    /
c          \------------
c           \           \
c            \           \
c             \           pj
c              pk
c
c restframe Rijk:
c
c            pij'
c            /
c           / 
c          /_xij_ _ _ _
c         /
c        /
c       /
c      pk'
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
      subroutine off_to_on_ijk(p,flav,ichan,p_OS)
        implicit none
#include "nlegborn.h"
#include "osres.h"
        ! momenta from PS-generator, on-shell momenta
        double precision p(0:3,nlegreal),p_OS(0:3,nlegreal)
        integer ichan, flav(nlegreal)
        ! mass at resonance
        double precision ml,mijk
        integer i,j,k,l
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        double precision momsum2sq,momsum3sq,dotp
        external momsum2sq,momsum3sq,dotp
        ! local variables
        double precision ratio
        ! momenta in lab. frame: p12 = p1+p2, pij = pi+pj, pkl = pk+pl
        double precision p12(0:3), pij(0:3), pl(0:3)
        double precision pijk_OS(0:3)
        ! momenta in restframe of resonant particle
        double precision pijRijk(0:3), pkRijk(0:3)
        double precision piRij(0:3), pjRij(0:3)
        ! reshuffeld momenta in lab. frame (see CS-paper)
        double precision pijk_tilde(0:3)
        double precision pij_tilde(0:3)
        double precision pi_tilde(0:3), pj_tilde(0:3)
        double precision pk_tilde(0:3), pl_tilde(0:3)
        ! invariants
        double precision s, sqrtS, sij, sijk
        ! boost from lab. frame into rest frame of particle i,j and k,l
        double precision betaRijk, vecRijk(3)
        double precision betaRij, vecRij(3)
        double precision norm
        ! check 4-momentum conservation
        logical lresult
        
        ! set the channel-related indices i,j,k,l and masses ml,mijk
        call set_channel(flav,ichan)
        i = osres_i
        j = osres_j
        k = osres_k
        l = osres_l
        ml = osres_ml
        mijk = osres_mijk
        
        if(nlegreal.ne.6) then
          print*, "error in subroutine off_to_on"
          print*, "nlegreal = ", nlegreal
          stop
        endif
        
        ! set aux. momenta
        p12(:) = p(:,1) + p(:,2) ! Q(mu) in CS-paper
        pij(:) = p(:,i) + p(:,j)
        pl(:) = p(:,l)
        ! set invariant masses       
        s     = momsum2sq(p(:,1),p(:,2))   ! Q2 in CS-paper
        sqrtS = dsqrt(S)
        sij   = momsum2sq(p(:,i),p(:,j))
        sijk  = momsum3sq(p(:,i),p(:,j),p(:,k))
        
        ! check if theta function was properly used
        if(s.lt.(mijk+ml)**2) then
          print*, "error in subroutine off_to_on"
          print*, "intermediate particle is not on-shell"
          print*, "sqrtS    = ", sqrtS
          print*, "mijk + ml = ", mijk + ml
          print*, "Did you forget the Theta-function?"
          stop
        endif
         
        ! Catani-Seymour reshuffling for the 2->4-kinematic
        ! see paper "The Dipole Formalism for Next-to-Leading
        ! Order QCD Calculations with Massive Partons" hep-ph/0201036.        
        ! definition of the momenta pijk_tilde and pk_tilde in terms of  
        ! the original momenta pi, pj, pk and pl
        ratio = kaellenSqrt(s,mijk**2,ml**2)/kaellenSqrt(s,sijk,ml**2)
        pl_tilde(:)  = ratio*(pl(:)-dotp(p12(:),pl(:))/s*p12(:))
     &                    +(s+ml**2-mijk**2)/(2*s)*p12(:)
        pijk_tilde(:) = p12(:)-pl_tilde(:)
        
        ! calculate the momenta pij and pk in cms Rijk of particle ij and k
        ! and apply the on-shell condition sijk = mijk**2
#define DIRECTION_IJ
#ifdef DIRECTION_IJ
        call mom_in_Rcms(mijk**2,pij(:),p(:,k),pijRijk(:),pkRijk(:))
#endif
#ifdef DIRECTION_K
        call mom_in_Rcms(mijk**2,p(:,k),pij(:),pkRijk(:),pijRijk(:))
#endif

        ! boost back into lab. frame with reshuffeld momenta:
        norm = dsqrt(pijk_tilde(1)**2+pijk_tilde(2)**2+pijk_tilde(3)**2)
        betaRijk   = norm/pijk_tilde(0)
        vecRijk(1) = pijk_tilde(1)/norm
        vecRijk(2) = pijk_tilde(2)/norm
        vecRijk(3) = pijk_tilde(3)/norm
        
        call mboost(1,vecRijk,betaRijk,pijRijk(:),pij_tilde(:))
        call mboost(1,vecRijk,betaRijk,pkRijk(:),pk_tilde(:))
        
        ! calculate the momenta pi and pj in cms Rij of particle i and j
        ! but don't apply the on-shell condition! (not necessary here!)
#define DIRECTION_I
#ifdef DIRECTION_I
        call mom_in_Rcms(sij,p(:,i),p(:,j),piRij(:),pjRij(:))
#endif
#ifdef DIRECTION_J
        call mom_in_Rcms(sij,p(:,j),p(:,i),pjRij(:),piRij(:))
#endif

        ! boost back into lab. frame with reshuffeld momenta:
        norm = dsqrt(pij_tilde(1)**2+pij_tilde(2)**2+pij_tilde(3)**2)
        betaRij   = norm/pij_tilde(0)
        vecRij(1) = pij_tilde(1)/norm
        vecRij(2) = pij_tilde(2)/norm
        vecRij(3) = pij_tilde(3)/norm
        
        call mboost(1,vecRij,betaRij,piRij(:),pi_tilde(:))
        call mboost(1,vecRij,betaRij,pjRij(:),pj_tilde(:))
        
        ! set the on-shell momenta
        ! no changes for initial state particles
        p_OS(:,1) = p(:,1)
        p_OS(:,2) = p(:,2)
        p_OS(:,i) = pi_tilde(:)
        p_OS(:,j) = pj_tilde(:)
        p_OS(:,k) = pk_tilde(:)
        p_OS(:,l) = pl_tilde(:)
        
        ! - checks -
        ! check on-shell condition
        pijk_OS(:) = p_OS(:,i) + p_OS(:,j) + p_OS(:,k)
        call check_on_shell(pijk_OS(:),mijk)
        ! check four momentum conservation
        call check_4conservation(p_OS,nlegreal,3,lresult)
      end
c############### end subroutine off_to_on_ijk ##########################

c############### subroutine mom_in_Rij #################################
c calculate the momenta piR and pjR in rest frame R of particles i and j
c with invariant mass squared s. Note: s is arbitrary and can be choosen
c externally, it can be choosen different to s = (pi + pj)**2.
c The direction of particle i is kept.
      subroutine mom_in_Rcms(s,pi,pj,piR,pjR)
        implicit none
        double precision beta,vec(3),pi(0:3),pj(0:3),mi,mj,s
        double precision piR(0:3),pjR(0:3)
        ! local variables
        double precision pij(0:3),CosQ,phi,norm,pxy,sqrts
        ! sumation index
        integer sumi
        ! small parameters
        double precision eps
        parameter(eps=1D-8)
        ! constants
        double precision m_pi
        parameter (m_pi = 4.D0*datan(1.D0))
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        double precision momsq, momsum2sq, momsum3sq, dotp
        external momsq, momsum2sq, momsum3sq, dotp
        
        sqrts = dsqrt(s)
        pij(:) = pi(:) + pj(:)
        mi = dsqrt(dabs(momsq(pi(:))))
        mj = dsqrt(dabs(momsq(pj(:))))
        
        ! boost into the rest frame Rij of particle i and j
        norm   = dsqrt(pij(1)**2+pij(2)**2+pij(3)**2)
        beta   = -norm/pij(0)
        vec(1) = pij(1)/norm
        vec(2) = pij(2)/norm
        vec(3) = pij(3)/norm
        
        ! keep the direction of particle i
        call mboost(1,vec(:),beta,pi(:),piR(:))
        norm = dsqrt(piR(1)**2+piR(2)**2+piR(3)**2)
        ! get the angular info of particle i
        if( norm .lt. eps) then
          CosQ = 1D0
          phi  = 0D0
        else
          CosQ = piR(3)/norm
          pxy   = dsqrt(piR(1)**2+piR(2)**2)
          if(dabs(pxy).lt. eps) then
            phi = 0D0
          else if(piR(1).ge.0.and.piR(2).ge.0) then
               phi = dacos(piR(1)/pxy)
          else if(piR(1).ge.0.and.piR(2).lt.0) then
               phi = 2d0*m_pi-dacos(piR(1)/pxy)
          else if(piR(1).lt.0.and.piR(2).ge.0) then
               phi = m_pi-dacos(dabs(piR(1))/pxy)
          else
               phi = m_pi+dacos(dabs(piR(1))/pxy)
          endif
        endif
        ! construct the new momenta of the particles i and j in R,
        ! with invariant mass s
        piR(0) = (s+mi**2-mj**2)/(2d0*sqrts)
        pjR(0) = (s+mj**2-mi**2)/(2d0*sqrts)
        ! set 3-momenta
        norm = kaellenSqrt(s,mi**2,mj**2)/(2d0*sqrts)
        piR(1) = norm*dsqrt(1d0-CosQ**2)*dcos(phi)
        piR(2) = norm*dsqrt(1d0-CosQ**2)*dsin(phi)
        piR(3) = norm*CosQ
        do sumi=1,3
          pjR(sumi) = -piR(sumi)
        enddo
      end
c############### end subroutine mom_in_Rij #############################

c############### subroutine check_on_shell #############################
      subroutine check_on_shell(p_os,m)
        implicit none
        double precision p_os(0:3),m
        double precision relerror
        double precision momsq
        external momsq
        relerror = (momsq(p_OS(:))-m**2)
     &             /(momsq(p_OS(:))+m**2) 
        if( dabs(relerror) .gt. 1D-6 ) then
          print*,"error: no on-shell condition was found."
          print*,"|p_OS| = ",dsqrt(momsq(p_OS(:)))
          print*,"m      = ", m
          print*,"relerror = ", relerror
          stop
        endif
      end
c############### end subroutine check_on_shell #########################

c TODO
c############### function corrfac_ijkl #################################
c the remapping requires a change in the PS integration
c and every counter term which uses the on-shell momenta should be
c rescaled by this correction factor
      double precision function corrfac_ijkl(shat,mi,mj,mk,ml,mij,mkl,
     &                                       sij,skl)
        implicit none
        !input variables
        double precision shat,mi,mj,mk,ml,mkl,mij,sij,skl
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        
        corrfac_ijkl = (sij*skl*kaellenSqrt(shat,mij**2,mkl**2)
     &                    *kaellenSqrt(mij**2,mi**2,mj**2)
     &                    *kaellenSqrt(mkl**2,mk**2,ml**2))
     &                 /(mij**2*mkl**2*kaellenSqrt(shat,sij,skl)
     &                    *kaellenSqrt(sij,mi**2,mj**2)
     &                    *kaellenSqrt(skl,mk**2,ml**2))
     
#ifdef DSUB_II_TEST
        corrfac_ijkl = 1D0
#endif

#ifdef DEBUGQ
        corrfac_ijkl = 1D0
#endif

#ifdef DEBUGQ
        print*,"corrfac_ijkl",corrfac_ijkl
#endif
      end  
c############### end function corrfac_ijkl #############################

c############### function corrfac_ijk ##################################
c the remapping requires a change in the PS integration
c and every counter term which uses the on-shell momenta should be
c rescaled by this correction factor
      double precision function corrfac_ijk(shat,mi,mj,mk,sij,mij)
        implicit none
        !input variables
        double precision shat,mi,mj,mk,sij,mij
        ! external functions
        double precision kaellenSqrt
        external kaellenSqrt
        
        corrfac_ijk = (sij*kaellenSqrt(shat,mij**2,mk**2)
     &                   *kaellenSqrt(mij**2,mi**2,mj**2))
     &                /(mij**2*kaellenSqrt(shat,sij,mk**2)
     &                   *kaellenSqrt(sij,mi**2,mj**2))

#ifdef DSUB_II_TEST
        corrfac_ijk = 1D0
#endif

#ifdef DEBUGQ
        corrfac_ijk = 1D0
#endif

#ifdef DEBUGQ
        print*,"corrfac_ijk",corrfac_ijk
#endif
      end  
c############### end function corrfac_ijk ##############################
