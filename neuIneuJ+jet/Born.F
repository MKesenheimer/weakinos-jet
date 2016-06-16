c############### Born.f ################################################
c last modified by MK, 22.01.2015
c weakino pair + jet production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### setborn subroutine ####################################
c sets Born cross section
c Born contribution:
c parton parton -> weakino weakino
      subroutine setborn(p,bflav,born,bornjk,bmunu)
        implicit none
#include "pwhg_math.h"
#include "nlegborn.h"
#include "PhysPars.h"
#include "pwhg_kn.h"
        integer bflav(nlegborn)
        double precision p(0:3,nlegborn), bornjk(nlegborn,nlegborn)
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
        ! quark masses, weakino masses
        double precision MassI1, MassI2, MassO1, MassO2, MassO3
        ! check 4-momentum conservation
        logical lresult
        ! phase space
        double precision ex, ey, ez, psNorm
        ! function to calculate the squared momentum sum
        double precision dotp, momsq, momsum3sq, momsum2sq
        external dotp, momsq, momsum2sq, momsum3sq
        ! calculate the relative error between formcalc and madgraph borns
        double precision fborn, mborn
        ! initialization variables
        logical verbose
        logical init_born
        data init_born/.true./
#ifdef DEBUGQ
#define DEBUG1
        ! debugging
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

        ! check four momentum conservation
        call check_4conservation(p, nlegborn, 1, lresult)
    
#if !defined(FORM_BORN) && !defined(MAD_BORN)
        print*,"Error: Preprocessor flag FORM_BORN or"
     &       //" MAD_BORN not set in Makefile."
        stop
#endif
        
#if defined(CHECK_FORM_MAD) || defined(FORM_BORN)
c ---------------------------- Formcalc Born ---------------------------
        ! store flavor list, so we can operate on it without
        ! affecting the rest of the program
        flavs = bflav
        ps = p

        ! calculate the couplings for this PS point
        call set_ebe_couplings

        ! load process dependent masses
        call set_process(flavs(1:4),MassI1,MassI2,MassO1,MassO2,MassO3)
        
        ! crossing (FormCalc provides only amplitudes for qqbar 
        ! and not for qbarq)
        if((flavs(1).gt.0 .and. flavs(2).lt.0) .or.
     &     (flavs(1).lt.0 .and. flavs(2).eq.0) .or.
     &     (flavs(1).gt.0 .and. flavs(2).eq.0)) then
          ps(:,1) = p(:,1)
          ps(:,2) = p(:,2)
        else
          ps(:,1) = p(:,2)
          ps(:,2) = p(:,1)
        endif
        
        ! calculate unit vectors and set the vectors for FormCalc
        psNorm=dsqrt(ps(1,1)**2+ps(2,1)**2+ps(3,1)**2)
        ex=ps(1,1)/psNorm
        ey=ps(2,1)/psNorm
        ez=ps(3,1)/psNorm
        call VecSet(1, MassI1, psNorm, ex,ey,ez)
        psNorm=dsqrt(ps(1,2)**2+ps(2,2)**2+ps(3,2)**2)
        ex=ps(1,2)/psNorm
        ey=ps(2,2)/psNorm
        ez=ps(3,2)/psNorm
        call VecSet(2, MassI2, psNorm, ex,ey,ez)
        psNorm=dsqrt(ps(1,3)**2+ps(2,3)**2+ps(3,3)**2)
        ex=ps(1,3)/psNorm
        ey=ps(2,3)/psNorm
        ez=ps(3,3)/psNorm
        call VecSet(3, MassO1, psNorm, ex,ey,ez)
        psNorm=dsqrt(ps(1,4)**2+ps(2,4)**2+ps(3,4)**2)
        ex=ps(1,4)/psNorm
        ey=ps(2,4)/psNorm
        ez=ps(3,4)/psNorm
        call VecSet(4, MassO2, psNorm, ex,ey,ez)
        psNorm=dsqrt(ps(1,5)**2+ps(2,5)**2+ps(3,5)**2)
        ex=ps(1,5)/psNorm
        ey=ps(2,5)/psNorm
        ez=ps(3,5)/psNorm
        call VecSet(5, MassO3, psNorm, ex,ey,ez)

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
                 //"implemented."
          stop
#endif

        call loadFormCalcAmps(flavs,helicities,flags,fc_result)
        
        born = fc_result(1)/symfac ! take spin and colour factors into account
        fborn = born

        ! spin and color correlations
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
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
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
        flavs = bflav
        ps = p

        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call sborn_proc(p,flavs,born,bornjk,bmunu)
        mborn = born
        
        if (verbose) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "born (madgraph) = ", mborn
#ifdef DEBUGQ
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)
          enddo
#endif
          print*
        endif
c ---------------------------- End Madgraph Born -----------------------
c endif preprocessor condition "if defined(CHECK_FORM_MAD) || defined(MAD_BORN)"
#endif

#ifdef CHECK_FORM_MAD
        ! compare the formcalc and madgraph amplitudes
        if(kn_jacborn.ne.0D0 .and.
     &     (abs((fborn - mborn)/(fborn + mborn)) > 1d-08) ) then ! the relative error is probably larger for smaller amplitudes
          print*, "Error: formcalc and madgraph borns are too different"
          print*, "in process: ", flavs
          do i=1,5
            print*, "p(",i,") = ", ps(:,i)
          enddo
          print*, "kn_jacborn = ",kn_jacborn
          print*, "born (formcalc) = ", fborn
          print*, "born (madgraph) = ", mborn
          print*, "rel. err. = ", abs((fborn - mborn)/(fborn + mborn))
          print*, "fborn/mborn = ", fborn/mborn, mborn/fborn
          print*
        endif
#endif

! If the program calculates both madgraph and formcalc borns (preprocessor flag CHECK_FORM_MAD)
! the user can choose which born amplitude powheg should use (formcalc or madgraph)
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

c############### subroutine loadFormCalcAmps ###########################
      subroutine loadFormCalcAmps(flav,helicities,flags,fc_result)
        implicit none
#include "nlegborn.h"
        integer flav(nlegborn)
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2)
        ! gluon in the initial state
        if((flav(1).eq.0 .or. flav(2).eq.0).and.(flav(5).ne.0)) then
          ! n1n1+jet
          if(flav(3).eq.1000022 .and. flav(4).eq.1000022) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              call dg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              call ug_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              call dg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              call ug_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              call dg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              call ubarg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              call ubarg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n1n2+jet
          elseif(flav(3).eq.1000022 .and. flav(4).eq.1000023) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              call dg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              call ug_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              call dg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              call ug_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              call dg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              call dbarg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              call ubarg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              call dbarg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              call ubarg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              call dbarg_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n2n2+jet
          elseif(flav(3).eq.1000023 .and. flav(4).eq.1000023) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n1n3+jet
          elseif(flav(3).eq.1000022 .and. flav(4).eq.1000025) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n1n4+jet
          elseif(flav(3).eq.1000022 .and. flav(4).eq.1000035) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n2n3+jet
          elseif(flav(3).eq.1000023 .and. flav(4).eq.1000025) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n2n4+jet
          elseif(flav(3).eq.1000023 .and. flav(4).eq.1000035) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n3n3+jet
          elseif(flav(3).eq.1000025 .and. flav(4).eq.1000025) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n3n4+jet
          elseif(flav(3).eq.1000025 .and. flav(4).eq.1000035) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n4n4+jet
          elseif(flav(3).eq.1000035 .and. flav(4).eq.1000035) then
            if(     flav(1).eq.1 .or. flav(2).eq.1) then
              !call dg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.2 .or. flav(2).eq.2) then
              !call ug_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.3 .or. flav(2).eq.3) then
              !call dg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.4 .or. flav(2).eq.4) then
              !call ug_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.5 .or. flav(2).eq.5) then
              !call dg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-1 .or. flav(2).eq.-1) then
              !call dbarg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-2 .or. flav(2).eq.-2) then
              !call ubarg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-3 .or. flav(2).eq.-3) then
              !call dbarg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-4 .or. flav(2).eq.-4) then
              !call ubarg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(flav(1).eq.-5 .or. flav(2).eq.-5) then
              !call dbarg_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            endif
          else
              goto 21
            endif
        ! only quarks in the initial state
        else
          ! n1n1+jet
          if(flav(3).eq.1000022 .and. flav(4).eq.1000022) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              call uubar_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              call uubar_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n1n2+jet
          elseif(flav(3).eq.1000022 .and. flav(4).eq.1000023) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              call ddbar_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              call uubar_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              call ddbar_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              call uubar_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              call ddbar_n1n2j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n2n2+jet
          elseif(flav(3).eq.1000023 .and. flav(4).eq.1000023) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n2n2j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n1n3+jet
          elseif(flav(3).eq.1000022 .and. flav(4).eq.1000025) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n1n3j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n1n4+jet
          elseif(flav(3).eq.1000022 .and. flav(4).eq.1000035) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n1n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n2n3+jet
          elseif(flav(3).eq.1000023 .and. flav(4).eq.1000025) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n2n3j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n2n4+jet
          elseif(flav(3).eq.1000023 .and. flav(4).eq.1000035) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n2n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n3n3+jet
          elseif(flav(3).eq.1000025 .and. flav(4).eq.1000025) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n3n3j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n3n4+jet
          elseif(flav(3).eq.1000025 .and. flav(4).eq.1000035) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n3n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          ! n4n4+jet
          elseif(flav(3).eq.1000035 .and. flav(4).eq.1000035) then
            if(     abs(flav(1)).eq.1 .and. abs(flav(2)).eq.1) then
              !call ddbar_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.2 .and. abs(flav(2)).eq.2) then
              !call uubar_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.3 .and. abs(flav(2)).eq.3) then
              !call ddbar_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.4 .and. abs(flav(2)).eq.4) then
              !call uubar_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            elseif(abs(flav(1)).eq.5 .and. abs(flav(2)).eq.5) then
              !call ddbar_n4n4j_squaredME(fc_result, helicities, flags)
              goto 20
            else
              goto 21
            endif
          endif
        endif


 21     print*, "ERROR: Wrong flavor structure in loadFormCalcAmps."
        print*, "flav = ", flav
        stop

 20     continue
      end
c############### subroutine loadFormCalcAmps ###########################