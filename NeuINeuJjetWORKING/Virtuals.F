c############### Virtuals.f ############################################
c last modified by MK, 02.12.2015
c weakino pair production
c virtual corrections at NLO SQCD:
c parton parton -> weakino weakino
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setvirtual #################################
c returns (M_B * M_V)/(as/(2pi)) as real number,
c where M_B is the Born amplitude and
c M_V is the finite part of the virtual amplitude
c The as/(2pi) factor is attached at a later point
c A factor (4 Pi)^ep/Gamma(1-ep) with ep=(4-d)/2 is implied.
      subroutine setvirtual(p,vflav,ampvirt)
        implicit none

#include "PhysPars.h"
#include "pwhg_st.h"
#include "pwhg_math.h"
#include "nlegborn.h"
#include "looptools.h"
#include "Flags.h"

#include "decl.h"

        double precision p(0:3,nlegborn)
        integer vflav(nlegborn)
        double precision ampvirt

      end

c############### end subroutine setvirtual #############################
