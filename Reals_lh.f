c############### Reals.f ###############################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### subroutine realcolour_lh ##############################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      subroutine realcolour_lh
        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        integer rflav(nlegreal),color(2,nlegreal)
        integer i,j
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
#ifdef DEBUG
        print*,"[DEBUG] in realcolour_lh"
        stop
#endif
      end
c############### end subroutine realcolour_lh ##########################