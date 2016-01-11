c############### Reals.f ###############################################
c last modified by MK, 21.01.2015
c neutralino pair production
c real emission contributions at NLO SQCD:
c parton parton -> neutralino neutralino + parton
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine real_color #################################
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.

      subroutine realcolour_lh

        implicit none
#include "nlegborn.h"
#include "LesHouches.h"
        
      end
      
c############### end subroutine real_color #############################

