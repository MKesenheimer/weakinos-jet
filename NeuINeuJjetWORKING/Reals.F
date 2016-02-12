c############### Reals.f ###############################################
c last modified by MK, 02.12.2015
c adapted from dislepton
c weakino pair production
c real emission contributions at NLO SQCD:
c parton parton -> weakino + weakino + parton

c############### subroutine setreal ####################################
c neutral current weakino pairs + jet
c q(1) qbar(2) -> Z/W -> weakino(3) weakino(4) gluon(5)
c average over incoming helicites

      subroutine setreal(p,rflav,ampreal)
        implicit none

#include "PhysPars.h"
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_math.h"
#include "pwhg_st.h"
#include "Flags.h"

        double precision p(0:3,nlegreal),p_in(0:3,nlegreal)
        integer rflav(nlegreal)
        ! the real amplitudes
        double precision ampreal

      end

c############### end subroutine setreal ################################