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
#include "nlegborn.h"
        double precision p(0:3,nlegborn), bornjk(nlegborn,nlegborn)
        integer bflav(nlegborn)
        double precision bmunu(0:3,0:3,nlegborn), born
        ! initialization variables
        logical verbose
        logical init_born
        data init_born/.true./
        integer i
#ifdef DEBUGQ
        double precision momsum2sq
        external momsum2sq
        double precision s35, s45
#endif

        if (init_born) then
           print*
           print*, "=== Init born... ==="
           verbose = .true.
           init_born = .false.
        else
           verbose = .false.
        endif
    
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call getBornAmps(p,bflav,born,bornjk,bmunu)
        
        if (verbose) then
          print*, "process: ", bflav
          print*, "born = ", born
          print*
        endif

        if(isnan(born)) then
          !print*,"got strange value for born amplitude...",born
          born = 0D0
        endif

#ifdef DEBUGQ
        ! fill histograms
        if(dsqrt(p(1,5)**2+p(2,5)**2).gt.10D0) then
          s35 = momsum2sq(p(0:3,3),p(0:3,5))
          s45 = momsum2sq(p(0:3,4),p(0:3,5))
          call histogram("sqrt(s35)",90,100D0,1000D0,dsqrt(s35),born)
          call histogram("sqrt(s45)",90,100D0,1000D0,dsqrt(s45),born)
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
#ifdef DEBUGQ
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
