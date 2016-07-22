c############### FormCalc.f ################################################
c last modified by MK, 22.01.2015
c weakino pair + jet production
c Interface to the FormCalc amplitudes
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine formCalcVecSet #############################
c calculate unit vectors and set the vectors for FormCalc
      subroutine formCalcVecSet(nlegs,p,flav)
        implicit none
        integer nlegs
        double precision p(0:3,nlegs)
        integer  flav(nlegs)
        double precision mass(nlegs)
        double precision ex, ey, ez, pNorm
        integer i
        ! function to calculate the squared momentum sum
        double precision momsum3sq, momsum2sq, dotp
        external momsum2sq, momsum3sq, dtop
        ! load process dependent masses
        call set_process(nlegs,flav(1:nlegs),mass(1:nlegs))
        do i=1,nlegs
          pNorm=dsqrt(p(1,i)**2+p(2,i)**2+p(3,i)**2)
          ex=p(1,i)/pNorm
          ey=p(2,i)/pNorm
          ez=p(3,i)/pNorm
          call VecSet(i, mass(i), pNorm, ex,ey,ez)
        enddo
      end
c############### end subroutine formCalcVecSet #########################

c############### subroutine getAmps ####################################
      subroutine getAmps(p,flav,helicities,flags,fc_result)
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegborn)
        integer flav(nlegborn)
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2)
        logical isodd, iseven
        external isodd, iseven
        
        ! reset
        fc_result(:) = 0D0
        
        ! set the formCalc vectors
        call formCalcVecSet(nlegborn,p,flav)
        
        ! F g -> F F F
        if((flav(1).eq.0 .or. flav(2).eq.0).and.(flav(5).ne.0)) then
          if((flav(1).gt.0 .and. isodd(flav(1))) .or.
     &       (flav(2).gt.0 .and. isodd(flav(2)))) then
            call qdg_nInJqd_squaredME(fc_result,helicities,flags)
            goto 20
          elseif((flav(1).gt.0 .and. iseven(flav(1))) .or. 
     &           (flav(2).gt.0 .and. iseven(flav(2)))) then
            call qug_nInJqu_squaredME(fc_result,helicities,flags)
            goto 20
          elseif((flav(1).lt.0 .and. isodd(flav(1))) .or. 
     &           (flav(2).lt.0 .and. isodd(flav(2)))) then
            call qdbarg_nInJqdbar_squaredME(fc_result,helicities,flags)
            goto 20
          elseif((flav(1).lt.0 .and. iseven(flav(1))) .or. 
     &           (flav(2).lt.0 .and. iseven(flav(2)))) then
            call qubarg_nInJqubar_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! F F -> F F g
        else
          if(isodd(abs(flav(1))) .and. isodd(abs(flav(2)))) then
            call qdqdbar_nInJg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(flav(1))) .and. iseven(abs(flav(2)))) then
            call ququbar_nInJg_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        endif

 21     print*, "ERROR: Wrong flavor structure in getAmps."
        print*, "flav = ", flav
        stop
 20     continue
      end
c############### subroutine getAmps ####################################

c############### subroutine getRealAmps ################################
      subroutine getRealAmps(p,flav,helicities,flags,fc_result)
        implicit none
#include "nlegborn.h"
        double precision p(0:3,nlegreal)
        integer  flav(nlegreal), flavs(nlegreal),f1,f2,f3,f4,f5,f6
        equivalence (f1,flavs(1)),(f2,flavs(2)),(f3,flavs(3))
        equivalence (f4,flavs(4)),(f5,flavs(5)),(f6,flavs(6))
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! formcalc results
        double precision fc_result(2)
        logical isodd, iseven
        external isodd, iseven
        
        ! reset
        fc_result(:) = 0D0
        flavs(:) = flav(:)
        
        ! set the formCalc vectors
        call formCalcVecSet(nlegreal,p,flav)
        
        ! F F -> F F F F
        if(f1.ne.0 .and. f2.ne.0 .and. f5.ne.0 .and. f6.ne.0) then
          if(isodd(abs(f1)) .and. isodd(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              call qdqd_nInJqdqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.lt.0) then
              call qdqdbar_nInJqdbarqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.gt.0) then
              call qdqdbar_nInJqdqdbar_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.lt.0) then
              call qdbarqd_nInJqdbarqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.gt.0) then
              call qdbarqd_nInJqdqdbar_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.lt.0 .and. f2.lt.0) then
              call qdbarqdbar_nInJqdbarqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)) .and. iseven(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              call ququ_nInJququ_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.lt.0) then
              call ququbar_nInJqubarqu_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.gt.0) then
              call ququbar_nInJququbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.lt.0) then
              call qubarqu_nInJqubarqu_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.gt.0) then
              call qubarqu_nInJququbar_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.lt.0 .and. f2.lt.0) then
              call qubarqubar_nInJqubarqubar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(isodd(abs(f1)) .and. iseven(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              call qdqu_nInJqdqu_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.lt.0) then
              call qdqubar_nInJqubarqd_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.gt.0) then
              call qdqubar_nInJqdqubar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.lt.0) then
              call qdbarqu_nInJqdbarqu_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.gt.0) then
              call qdbarqu_nInJquqdbar_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.lt.0 .and. f2.lt.0) then
              call qdbarqubar_nInJqdbarqubar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)) .and. isodd(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              call quqd_nInJquqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.lt.0) then
              call quqdbar_nInJqdbarqu_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.gt.0 .and. f2.lt.0 .and. f5.gt.0) then
              call quqdbar_nInJquqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.lt.0) then
              call qubarqd_nInJqubarqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0 .and. f5.gt.0) then
              call qubarqd_nInJqdqubar_squaredME(fc_result,helicities,flags)
              goto 20  
            elseif(f1.lt.0 .and. f2.lt.0) then
              call qubarqdbar_nInJqubarqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          else
            goto 21
          endif
        ! F g -> F F F g
        elseif(f1.ne.0 .and. f2.eq.0 .and. f5.ne.0 .and. f6.eq.0) then
          if(iseven(abs(f1)) .and. f1.gt.0) then
            call qug_nInJqug_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f1)) .and. f1.lt.0) then
            call qubarg_nInJqubarg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f1)) .and. f1.gt.0) then
            call qdg_nInJqdg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f1)) .and. f1.lt.0) then
            call qdbarg_nInJqdbarg_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! F g -> F F g F
        elseif(f1.ne.0 .and. f2.eq.0 .and. f5.eq.0 .and. f6.ne.0) then
          if(iseven(abs(f1)) .and. f1.gt.0) then
            call qug_nInJgqu_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f1)) .and. f1.lt.0) then
            call qubarg_nInJgqubar_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f1)) .and. f1.gt.0) then
            call qdg_nInJgqd_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f1)) .and. f1.lt.0) then
            call qdbarg_nInJgqdbar_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! g F -> F F F g
        elseif(f1.eq.0 .and. f2.ne.0 .and. f5.ne.0 .and. f6.eq.0) then
          if(iseven(abs(f2)) .and. f2.gt.0) then
            call gqu_nInJqug_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f2)) .and. f2.lt.0) then
            call gqubar_nInJqubarg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f2)) .and. f2.gt.0) then
            call gqd_nInJqdg_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f2)) .and. f2.lt.0) then
            call gqdbar_nInJqdbarg_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! g F -> F F g F
        elseif(f1.eq.0 .and. f2.ne.0 .and. f5.eq.0 .and. f6.ne.0) then
          if(iseven(abs(f2)) .and. f2.gt.0) then
            call gqu_nInJgqu_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(iseven(abs(f2)) .and. f2.lt.0) then
            call gqubar_nInJgqubar_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f2)) .and. f2.gt.0) then
            call gqd_nInJgqd_squaredME(fc_result,helicities,flags)
            goto 20
          elseif(isodd(abs(f2)) .and. f2.lt.0) then
            call gqdbar_nInJgqdbar_squaredME(fc_result,helicities,flags)
            goto 20
          else
            goto 21
          endif
        ! F F -> F F g g
        elseif(f1.ne.0 .and. f2.ne.0 .and. f5.eq.0 .and. f6.eq.0) then
          if(isodd(abs(f1)) .and. isodd(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call qdqd_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0) then
              !call qdqdbar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0) then
              !call qdbarqd_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.lt.0) then
              !call qdbarqdbar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(isodd(abs(f1)) .and. iseven(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call qdqu_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0) then
              !call qdqubar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0) then
              !call qdbarqu_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.lt.0) then
              !call qdbarqubar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)) .and. isodd(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call quqd_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0) then
              !call quqdbar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0) then
              !call qubarqd_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.lt.0) then
              !call qubarqdbar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f1)) .and. iseven(abs(f2))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call ququ_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.gt.0 .and. f2.lt.0) then
              !call ququbar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.gt.0) then
              !call qubarqu_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f1.lt.0 .and. f2.lt.0) then
              !call qubarqubar_nInJgg_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          else
            goto 21
          endif
        ! g g -> F F F F
        elseif(f1.eq.0 .and. f2.eq.0 .and. f5.ne.0 .and. f6.ne.0) then
          if(isodd(abs(f5)) .and. isodd(abs(f6))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call gg_nInJqdqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.gt.0 .and. f6.lt.0) then
              !call gg_nInJqdqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.gt.0) then
              !call gg_nInJqdbarqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.lt.0) then
              !call gg_nInJqdbarqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(isodd(abs(f5)) .and. iseven(abs(f6))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call gg_nInJqdqu_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.gt.0 .and. f6.lt.0) then
              !call gg_nInJqdqubar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.gt.0) then
              !call gg_nInJqdbarqu_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.lt.0) then
              !call gg_nInJqdbarqubar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif  
          elseif(iseven(abs(f5)) .and. isodd(abs(f6))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call gg_nInJquqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.gt.0 .and. f6.lt.0) then
              !call gg_nInJquqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.gt.0) then
              !call gg_nInJqubarqd_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.lt.0) then
              !call gg_nInJqubarqdbar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          elseif(iseven(abs(f5)) .and. iseven(abs(f6))) then
            if(f1.gt.0 .and. f2.gt.0) then
              !call gg_nInJququ_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.gt.0 .and. f6.lt.0) then
              !call gg_nInJququbar_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.gt.0) then
              !call gg_nInJqubarqu_squaredME(fc_result,helicities,flags)
              goto 20
            elseif(f5.lt.0 .and. f6.lt.0) then
              !call gg_nInJqubarqubar_squaredME(fc_result,helicities,flags)
              goto 20
            else
              goto 21
            endif
          else
            goto 21
          endif
        ! g g -> F F g g
        elseif(f1.eq.0 .and. f2.eq.0 .and. f5.eq.0 .and. f6.eq.0) then
          !call gg_nInJgg_squaredME(fc_result,helicities,flags)
          goto 20
        else
          goto 21
        endif

 21     print*, "ERROR: Wrong flavor structure in getRealAmps."
        print*, "flav = ", flav
        stop
 20     continue
      end
c############### subroutine getRealAmps ################################