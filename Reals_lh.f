c############### Reals.f ###############################################
c last modified by MK, 21.01.2015
c neutralino pair production
c real emission contributions at NLO SQCD:
c parton parton -> weakino weakino + parton
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine realcolour_lh ##############################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      subroutine realcolour_lh
        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        print*,"in realcolour_lh"
        print*,"uncomment and debug"
        stop
c        integer rflav(nlegreal),color(2,nlegreal)
c        integer i,j
c        do i=1,nlegreal
c          rflav(i)=idup(i)
c          if (rflav(i).eq.21) rflav(i)=0
c        enddo
c        call real_color(rflav,color)
c        do i=1,2
c          do j=1,nlegreal
c            icolup(i,j)=color(i,j)
c          enddo
c        enddo
c#ifdef DEBUGQ
c        print*,"[DEBUG] in realcolour_lh"
c        stop
c#endif
      end
c############### end subroutine realcolour_lh ##########################