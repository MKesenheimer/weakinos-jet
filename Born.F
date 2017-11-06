c############### Born.f ################################################
c last modified by MK, 22.01.2015
c weakino pair + jet production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setborn ####################################
c sets Born cross section
c Born contribution:
c parton parton -> weakino weakino
      subroutine setborn(p,f,born,bornjk,bmunu)
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegborn), bornjk(nlegborn,nlegborn)
        integer f(nlegborn)
        double precision bmunu(0:3,0:3,nlegborn), born
        ! initialization variables
        logical verbose
        logical init_born
        data init_born/.true./
        integer i
        ! output control
        integer warncount1
        data warncount1/0/

        if(init_born) then
          print*
          print*, "=== Init born... ==="
          verbose = .true.
          init_born = .false.
        else
          verbose = .false.
        endif
    
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call getBornAmps(p,f,born,bornjk,bmunu)
        
        if(verbose) then
          print*, "process: ", f
          print*, "born = ", born
          print*
        endif

        if(isnan(born)) then
          if(warncount1.lt.10) then
            warncount1 = warncount1 + 1
            print*,"got strange value for born amplitude...",born
          elseif(warncount1.eq.10) then
            warncount1 = 11
            print*, "setborn: Further output will be suppressed."
          endif
          born = 0D0
        endif
      end
c############### end setborn subroutine ################################

c############### subroutine borncolour_lh ##############################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      subroutine borncolour_lh
        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        integer f(nlegborn),color(2,nlegborn)
        integer i,j
        do i=1,nlegborn
          f(i)=idup(i)
          if (f(i).eq.21) f(i)=0
        enddo
        call born_color(f,color)
        do i=1,2
          do j=1,nlegborn
            icolup(i,j)=color(i,j)
          enddo
        enddo
      end
c############### end subroutine finalize_lh ############################

c############### subroutine borncolour_lh ##############################
c Sets up the resonances whose mass must be preserved
c on the Les Houches interface; not needed for neutralino production.
      subroutine finalize_lh
        implicit none
      end
c############### end subroutine finalize_lh ############################
