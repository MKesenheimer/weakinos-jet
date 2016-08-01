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

        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call getBornAmps(p,bflav,born,bornjk,bmunu)
        fborn = born
        
#ifdef DEBUGQ
          print*
          print*, "process: ", bflav
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)/fborn
          enddo
          !stop
#endif
        
        if (verbose) then
          print*, "process: ", bflav
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "born (formcalc) = ", fborn
          print*
        endif
c ---------------------------- End FormCalc Born -----------------------
c endif preprocessor condition "if defined(CHECK_FORM_MAD) || defined(FORM_BORN)"
#endif


#if defined(CHECK_FORM_MAD) || defined(MAD_BORN)
c ---------------------------- MadGraph Born ---------------------------
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call sborn_proc(p,bflav,born,bornjk,bmunu)
        mborn = born
        
#ifdef DEBUGQ
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)/mborn
          enddo
          !stop
#endif

#ifdef DEBUGQ
          print*, "process: ", bflav
          do j=1,nlegborn
          do nmu=0,3
            print*, "bmunu(",nmu,", nu,",j,") = ", bmunu(nmu,:,j)/mborn
          enddo
          print*
          enddo
          !stop
#endif
        
        if (verbose) then
          print*, "process: ", bflav
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "born (maqdgraph) = ", mborn
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
          print*, "in process: ", bflav
          do i=1,5
            print*, "p(",i,") = ", p(:,i)
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
          print*, "process: ", bflav
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*,"born = ", born
          counter = counter + 1
        endif
#endif
      end
c############### end setborn subroutine ################################

c############### subroutine borncolour_lh ##############################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      subroutine borncolour_lh
        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        integer bflav(nlegborn),color(2,nlegborn)
        integer i,j
        do i=1,nlegborn
          bflav(i)=idup(i)
          if (bflav(i).eq.21) bflav(i)=0
        enddo
        call born_color(bflav,color)
        do i=1,2
          do j=1,nlegborn
            icolup(i,j)=color(i,j)
          enddo
        enddo
#ifdef DEBUG
        print*,"[DEBUG] in borncolour_lh"
        print*,"[DEBUG] bflav",bflav
        do j=1,nlegborn
          print*,"[DEBUG] icolup(",j,")",icolup(1,j),icolup(2,j)
        enddo
        !stop
#endif
      end
c############### end subroutine finalize_lh ############################

c############### subroutine borncolour_lh ##############################
c Sets up the resonances whose mass must be preserved
c on the Les Houches interface; not needed for neutralino production.
      subroutine finalize_lh
        implicit none
      end
c############### end subroutine finalize_lh ############################