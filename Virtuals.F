c############### Virtuals.f ############################################
c last modified by MK, 10.06.2016
c weakino pair + jet production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setvirtual #################################
c returns (M_B * M_V)/(as/(2pi)) as real number,
c where M_B is the Born amplitude and
c M_V is the finite part of the virtual amplitude
c The as/(2pi) factor is attached at a later point
c A factor (4 Pi)^ep/Gamma(1-ep) with ep=(4-d)/2 is implied.
      subroutine setvirtual(p,vflav,virt)
        implicit none
#include "PhysPars.h"
#include "pwhg_st.h"
#include "pwhg_math.h"
#include "nlegborn.h"
c#include "looptools.h"
c#include "decl.h"
#include "Flags.h"
        double precision p(0:3,nlegborn)
        integer vflav(nlegborn)
        double precision virt, amps(2)
        double precision momsum2sq
        ! check 4-momentum conservation
        logical lresult
        ! status
        logical verbose
        logical init_virtual
        data init_virtual/.true./
#ifdef DEBUGQ
#define DEBUG1
        ! debugging
        integer counter
        data counter/0/
        save counter
#endif

        if (init_virtual) then
          print*
          print*, "=== Init virtual... ==="
          if(flg_fakevirtuals) then
            print*, "Note: Using fake virtuals"
          endif
          verbose = .true.
          init_virtual = .false.
        else
          verbose = .false.
        endif

        call set_ebe_couplings
        call getVirtAmps(p,vflav,amps)
        
        ! fake virtuals
        if(flg_fakevirtuals) then
          virt = 1d-2*amps(1)
        ! no fake virtuals
        else
          ! = 2 x Re[M_tree* x M_1-loop], FC8Guid.pdf p.48
          ! need to extract a factor N=(4Pi)^eps/Ga(1-eps)
          ! -> LoopTools does already extract this factor
          virt = amps(2)/(st_alpha/(2d0*pi)) 
        endif
        
        ! Print informations
        if (verbose) then
          print*, "process: ", vflav
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "2*Born*Virtual = ", virt
          print*
        endif

        if(isnan(virt)) then
          !print*,"got strange value for virtual amplitude...", virtual
          virt = 0D0
        endif
        
#ifdef DEBUG1
        if(counter < 5) then
          print*, "process: ", vflav
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "virt = ", virt
          counter = counter + 1
        endif
#endif
      end
c############### end subroutine setvirtual #############################