c############### Born.f ################################################
c last modified by MK, 22.01.2015
c weakino pair + jet production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setborn ####################################
c sets Born cross section
c Born contribution:
c parton parton -> weakino weakino
      subroutine setborn(p,bflav,born,bornjk,bmunu)
        implicit none
#include "PhysPars.h"
#include "pwhg_math.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
        double precision p(0:3,nlegborn), bornjk(nlegborn,nlegborn)
        integer bflav(nlegborn)
        double precision bmunu(0:3,0:3,nlegborn), born, colcf
        ! copy of bflav and p
        double precision ps(0:3,nlegborn)
        integer  flavs(nlegborn)
        ! formcalc results
        double precision fc_result(2)
        ! symmetry factor
        double precision symfac
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! indices
        integer i,j,k,nmu,nnu
        ! check 4-momentum conservation
        logical lresult
        ! function to calculate the squared momentum sum
        double precision dotp, momsq, momsum3sq, momsum2sq
        external dotp, momsq, momsum2sq, momsum3sq
        ! calculate the relative error between formcalc and maqdgraph borns
        double precision fborn, mborn
        ! initialization variables
        logical verbose
        logical init_born
        data init_born/.true./
#ifdef DEBUGQ
#define DEBUG1
        ! debqugging
        integer counter
        data counter/0/
        save counter
#endif

        if (init_born) then
           print*
           print*, "=== Init born... ==="
           verbose = .true.
           init_born = .false.
        else
           verbose = .false.
        endif
    
#if !defined(FORM_BORN) && !defined(MAD_BORN)
        print*,"Error: Preprocessor flag FORM_BORN or"
     &       //" MAD_BORN not set in Makefile."
        stop
#endif
        
#if defined(CHECK_FORM_MAD) || defined(FORM_BORN)
c ---------------------------- Formcalc Born ---------------------------
        ! store flavor list, so we can operate on it without
        ! affecting the rest of the program
        flavs(:) = bflav(:)
        ps(:,:) = p(:,:)

        ! calculate the couplings for this PS point
        call set_ebe_couplings
        
        ! crossing (FormCalc provides only amplitudes for qqbar 
        ! and not for qbarq)
        if((flavs(1).gt.0 .and. flavs(2).lt.0) .or.
     &     (flavs(1).lt.0 .and. flavs(2).eq.0) .or.
     &     (flavs(1).gt.0 .and. flavs(2).eq.0)) then
          ps(:,1) = p(:,1)
          ps(:,2) = p(:,2)
        else
          flavs(1) = bflav(2)
          flavs(2) = bflav(1)
          ps(:,1) = p(:,2)
          ps(:,2) = p(:,1)
        endif

        ! unpolarized particles: B01110 01010 01010 01010 01010 = D15018314, FC8Guid.pdf p.49
        if(flavs(1).eq.0) then
          symfac = 96d0
          helicities = 15018314
        elseif(flavs(2).eq.0) then
          symfac = 96d0
          helicities = 10955082
        elseif(flavs(3).eq.0) then
          symfac = 36d0
          helicities = 1082106
        elseif(flavs(4).eq.0) then
          symfac = 36d0
          helicities = 10824138
        elseif(flavs(5).eq.0) then
          symfac = 36d0
          helicities = 10824014
        else
          print*, "ERROR: Wrong flavor structure in setborn."
          print*, "flav = ", flavs
          stop
        endif

        ! symmetry factor for equal final states
        if(flavs(3).eq.flavs(4)) then
          symfac = 2d0*symfac
        endif

        ! flags: Bit0 (reset) = 1, Bit1 (loop) = 0 -> B01 = D1
        flags = 1

        !call clearcache
        fc_result(1) = 0D0    ! born
        fc_result(2) = 0D0    ! virtual (not needed here)

#ifndef NEGLECTBMASS
          print*,"ERROR: FormCalc massive b-quark amplitudes not yet "
     &           //"implemented."
          stop
#endif

        ! get the formcalc real amplitudes
        call getAmps(ps,flavs,helicities,flags,fc_result)
        
        born = fc_result(1)/symfac ! take spin and colour factors into account
        fborn = born

        ! spin and color correlations (TODO!)
        do j=1,nlegborn
          if(abs(flavs(j)).le.6) then
          ! gluon spin correlations as in (eq. 2.8) of 1002.2581v1
          ! (A general framework for implementing NLO calculations in [...] the POWHEG BOX)
          ! only different from zero for processes involving gluons in 
          ! the initial or final state.: Bmunu=0
            if(flavs(j).eq.0) then
              do nmu=0,3
                do nnu=0,3
                  bmunu(nmu,nnu,j) = 0D0
                enddo
              enddo
            endif  
          ! color correlations as in (eq. 2.6) of 1002.2581v1
          ! (A general framework for implementing NLO calculations in [...] the POWHEG BOX)
          ! Bij = Cf B for i != j incoming q/qbar
            do k=j+1,nlegborn
              if(abs(flavs(k)).le.6) then
                colcf = cf !4/3
              else
                colcf = 0D0
              endif
                bornjk(j,k) = born*colcf
                bornjk(k,j) = bornjk(j,k)
            enddo
          endif
        enddo
        
        if (verbose) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(ps(0:3,1), ps(0:3,2))
          print*, "t  = ", momsum2sq(ps(0:3,1),-ps(0:3,3))
          print*, "u  = ", momsum2sq(ps(0:3,2),-ps(0:3,3))
          print*, "born (formcalc) = ", fborn
#ifdef DEBUGQ
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)
          enddo
#endif
          print*
        endif
c ---------------------------- End FormCalc Born -----------------------
c endif preprocessor condition "if defined(CHECK_FORM_MAD) || defined(FORM_BORN)"
#endif


#if defined(CHECK_FORM_MAD) || defined(MAD_BORN)
c ---------------------------- MadGraph Born ---------------------------
        ! store flavor list, so we can operate on it without
        ! affecting the rest of the program
        flavs(:) = bflav(:)
        ps(:,:) = p(:,:)

        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call sborn_proc(ps,flavs,born,bornjk,bmunu)
        mborn = born
        
        if (verbose) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "born (maqdgraph) = ", mborn
#ifdef DEBUGQ
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)
          enddo
#endif
          print*
        endif
c ---------------------------- End Maqdgraph Born -----------------------
c endif preprocessor condition "if defined(CHECK_FORM_MAD) || defined(MAD_BORN)"
#endif

#ifdef CHECK_FORM_MAD
        ! compare the formcalc and maqdgraph amplitudes
        if(kn_jacborn.ne.0D0 .and.
     &     (abs((fborn - mborn)/(fborn + mborn)) > 1d-08) ) then ! the relative error is probably larger for smaller amplitudes
          print*, "Error: formcalc and maqdgraph borns are too different"
          print*, "in process: ", flavs
          do i=1,5
            print*, "p(",i,") = ", ps(:,i)
          enddo
          print*, "kn_jacborn = ",kn_jacborn
          print*, "born (formcalc) = ", fborn
          print*, "born (maqdgraph) = ", mborn
          print*, "rel. err. = ", abs((fborn - mborn)/(fborn + mborn))
          print*, "fborn/mborn = ", fborn/mborn, mborn/fborn
          print*
        endif
#endif

! If the program calculates both maqdgraph and formcalc borns (preprocessor flag CHECK_FORM_MAD)
! the user can choose which born amplitude powheg should use (formcalc or maqdgraph)
#if defined(CHECK_FORM_MAD) && defined(MAD_BORN)
        born = mborn ! (default)
#endif
#if defined(CHECK_FORM_MAD) && defined(FORM_BORN)
        born = fborn
#endif

        if(isnan(born)) then
          !print*,"got strange value for born amplitude...",born
          born = 0D0
        endif

#ifdef DEBUG1
        if(counter < 5) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*,"born = ", born
          counter = counter + 1
        endif
#endif
      end
c############### end setborn subroutine ################################