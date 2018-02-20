c############### init_processes.f ######################################
c last modified by MK, 05.12.2015
c weakino pair production

c############### subroutine init_processes #############################
c     setup Born and real subprocesses
c     set process id using slepton types
      subroutine init_processes
        implicit none
        
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "LesHouches.h"
#include "Flags.h"
#include "pwhg_st.h"
#include "pwhg_flst_add.h"
#include "osres.h"

        integer i1,i2,i3,i4,i5,i6,ii(6)
        equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),(i4,ii(4)),
     &              (i5,ii(5)),(i6,ii(6))
        integer i,j,k, temp, flst_max
        real*8 powheginput
        external powheginput
        logical check, isnInJ, isnIxJ, isxIxJ
        integer max_flav, max_flav_out, encode_pair
        logical isodd, iseven, isg, isd, isu, isdbar, isubar
        external isodd, iseven, isg, isd, isu, isdbar, isubar
        integer qgen
        external qgen
        double precision elc
        external elc
#ifdef DEBUG
        integer nflst_select
#endif

        ! set number of active flavors for incoming partons
        max_flav = int(powheginput('incomingflavors'))
        max_flav_out = int(powheginput('outgoingflavors'))
        if(max_flav.lt.1) then
          print*,"error: incomingflavors should be between 1 and 5."
          stop
        endif
        if(max_flav.gt.5) then
          print*,"error: more than 5 incoming flavors are not supported"
          stop
        endif

        ! whether to use fake virtuals or not
        if(powheginput("#fakevirtuals").gt.0) then
           if(int(powheginput("fakevirtuals")).gt.0) then
             print*,"Using fake virtuals."
             flg_fakevirtuals = .true.
           endif
        else
          flg_fakevirtuals = .false.
        endif

        ! number of light quarks
        ! this number is used for computation of pwhg_alphas
        if(powheginput("#flavorscheme").gt.0) then
          st_nlight = int(powheginput("flavorscheme"))
          if(st_nlight.gt.5 .or. st_nlight.lt.4) then
            print*, "Flavor scheme ", st_nlight, " not supported."
            stop
          endif
        else
          st_nlight = max_flav
        endif
        if(st_nlight.eq.4) then
          print*,"Working in the 4 flavor scheme. Make sure to use 4 flavor PDFs."
        elseif(st_nlight.eq.5) then
          print*,"Working in the 5 flavor scheme. Make sure to use 5 flavor PDFs."
        endif
        if(st_nlight.ne.max_flav) then
          print*,"Warning: max_flav not equal st_nlight."
        endif

        ! index of the first coloured particle in the final state
        ! (all subsequent particles are coloured)
        flst_lightpart = 5

        ! choose process
        i3 = int(powheginput("fin1"))
        i4 = int(powheginput("fin2"))

        isnInJ = .false.
        isnIxJ = .false.
        isxIxJ = .false.
        ! channels with two neutralinos in final state
        if (      ((i3.eq.1000022).and.(i4.eq.1000022))
     &       .or. ((i3.eq.1000022).and.(i4.eq.1000023))
     &       .or. ((i3.eq.1000022).and.(i4.eq.1000025))
     &       .or. ((i3.eq.1000022).and.(i4.eq.1000035))
     
     &       .or. ((i3.eq.1000023).and.(i4.eq.1000023))
     &       .or. ((i3.eq.1000023).and.(i4.eq.1000025))
     &       .or. ((i3.eq.1000023).and.(i4.eq.1000035))
     
     &       .or. ((i3.eq.1000025).and.(i4.eq.1000025))
     &       .or. ((i3.eq.1000025).and.(i4.eq.1000035))
     
     &       .or. ((i3.eq.1000035).and.(i4.eq.1000035)) ) then
          isnInJ = .true.
          
        ! channels with two charginos in final state
        else if ( ((i3.eq.1000024).and.(i4.eq.-1000024))
     &       .or. ((i3.eq.1000024).and.(i4.eq.-1000037))
     &       .or. ((i3.eq.1000037).and.(i4.eq.-1000037)) ) then
          isxIxJ = .true.

        ! channels with one neutralino and one chargino in final state
        else if ( ((i3.eq.1000022).and.(i4.eq.-1000024))
     &       .or. ((i3.eq.1000022).and.(i4.eq. 1000024))
     &       .or. ((i3.eq.1000022).and.(i4.eq.-1000037))
     &       .or. ((i3.eq.1000022).and.(i4.eq. 1000037))

     &       .or. ((i3.eq.1000023).and.(i4.eq.-1000024))
     &       .or. ((i3.eq.1000023).and.(i4.eq. 1000024))
     &       .or. ((i3.eq.1000023).and.(i4.eq.-1000037))
     &       .or. ((i3.eq.1000023).and.(i4.eq. 1000037))
     
     &       .or. ((i3.eq.1000025).and.(i4.eq.-1000024))
     &       .or. ((i3.eq.1000025).and.(i4.eq. 1000024))
     &       .or. ((i3.eq.1000025).and.(i4.eq.-1000037))
     &       .or. ((i3.eq.1000025).and.(i4.eq. 1000037))
     
     &       .or. ((i3.eq.1000035).and.(i4.eq.-1000024))
     &       .or. ((i3.eq.1000035).and.(i4.eq. 1000024))
     &       .or. ((i3.eq.1000035).and.(i4.eq.-1000037))
     &       .or. ((i3.eq.1000035).and.(i4.eq. 1000037)) ) then
          isnIxJ = .true.
        else
          print*, "requested final states not implemented"
          print*, "swap final states and try again"
          stop
        endif

        print*
        print*, "===================================================="
        print*, "     neutralino/chargino pair + jet production      "
        call print_version
        print*, "        final states: ", i3, i4
        print*
        if     (i3.eq.1000022) then
          print*, "             q                         n1           "
        elseif (i3.eq.1000023) then
          print*, "             q                         n2           "
        elseif (i3.eq.1000025) then
          print*, "             q                         n3           "
        elseif (i3.eq.1000035) then
          print*, "             q                         n4           "
        elseif (i3.eq.-1000024) then
          print*, "             q                         x1-          "
        elseif (i3.eq.1000024) then
          print*, "             q                         x1+          "
        elseif (i3.eq.-1000037) then
          print*, "             q                         x2-          "
        elseif (i3.eq.1000037) then
          print*, "             q                         x2+          "
        endif
        print*, "              \                       /             "
        print*, "               \                     /              "
        print*, "                \                   /               "
        print*, "                 \       Z/W       /                "
        print*, "                  ~~~~~~~~~~~~~~~~~                 "
        print*, "                 /                 \                "
        print*, "                /                   \               "
        print*, "               /                     \              "
        print*, "              /                       \             "
        if     (i4.eq.1000022) then
          print*, "             q*                        n1           "
        elseif (i4.eq.1000023) then
          print*, "             q*                        n2           "
        elseif (i4.eq.1000025) then
          print*, "             q*                        n3           "
        elseif (i4.eq.1000035) then
          print*, "             q*                        n4           "
        elseif (i4.eq.-1000024) then
          print*, "             q*                        x1-          "
        elseif (i4.eq.1000024) then
          print*, "             q*                        x1+          "
        elseif (i4.eq.-1000037) then
          print*, "             q*                        x2-          "
        elseif (i4.eq.1000037) then
          print*, "             q*                        x2+          "
        endif 
        print*, "                                                    "
        print*, "===================================================="
        print* 
        print*, " number of incoming quark flavors    = ", max_flav
        print*, " number of quark flavors for alpha_s = ", st_nlight

        ! set process id: encode types of slepton A and B
        lprup(1) = encode_pair(i3,i4)
        print*, " encoded final states to process ID: ", lprup(1)
        
        ! Born subprocesses
        flst_nborn = 0
        do i1 = -max_flav,max_flav
          do i2 = -max_flav,max_flav
            do i5 = -max_flav_out,max_flav_out
              check = .false.
              if(isnInJ .or. isxIxJ) then
                if(isubar(i1).and.isg(i2).and.isubar(i5).and.i1.eq.i5) check = .true. ! ubar g -> nI nJ ubar
                if(isu(i1).and.isg(i2).and.isu(i5).and.i1.eq.i5) check = .true. ! u g -> nI nJ u
                if(isdbar(i1).and.isg(i2).and.isdbar(i5).and.i1.eq.i5) check = .true. ! dbar g -> nI nJ dbar
                if(isd(i1).and.isg(i2).and.isd(i5).and.i1.eq.i5) check = .true. ! d g -> nI nJ d
                if(isu(i1).and.isubar(i2).and.isg(i5).and.i1.eq.-i2) check = .true. ! u ubar -> nI nJ g
                if(isd(i1).and.isdbar(i2).and.isg(i5).and.i1.eq.-i2) check = .true. ! d dbar -> nI nJ g
              endif
              if(isnIxJ.and.i4.lt.0) then
                if(isubar(i1).and.isd(i2).and.isg(i5).and.(i1+i2.eq.-1)) check = .true. ! ubar d -> nI xJ- g
                if(isg(i1).and.isubar(i2).and.isdbar(i5).and.(i2+1.eq.i5)) check = .true. ! g ubar -> nI xJ- dbar
                if(isg(i1).and.isd(i2).and.isu(i5).and.(i2+1.eq.i5)) check = .true. ! g d -> nI xJ- u
              elseif(isnIxJ.and.i4.gt.0) then
                if(isu(i1).and.isdbar(i2).and.isg(i5).and.(i1+i2.eq.1)) check = .true. ! u dbar -> nI xJ+ g
                if(isg(i1).and.isu(i2).and.isd(i5).and.(i2-1.eq.i5)) check = .true. ! g u -> nI xJ+ d
                if(isg(i1).and.isdbar(i2).and.isubar(i5).and.(i2-1.eq.i5)) check = .true. ! g dbar -> nI xJ+ ubar
              endif
              if(check) then
                flst_nborn=flst_nborn+1
                if(flst_nborn.gt.maxprocborn) goto 998
                do k=1,nlegborn
                  flst_born(k,flst_nborn) = ii(k)
                enddo
              endif
            enddo
          enddo
        enddo
        
        ! cross the first two initial states if they are not equal and
        ! add the new configuration to flst_born
        flst_max = flst_nborn
        do i=1,flst_max
          if(flst_born(1,i).ne.flst_born(2,i)) then
            flst_nborn = flst_nborn+1
            flst_born(1,flst_nborn) = flst_born(2,i)
            flst_born(2,flst_nborn) = flst_born(1,i)
            flst_born(3,flst_nborn) = flst_born(3,i)
            flst_born(4,flst_nborn) = flst_born(4,i)
            flst_born(5,flst_nborn) = flst_born(5,i)
          endif
        enddo
        
        call remove_duplicates(flst_born,nlegborn,flst_nborn)
        
        ! real subprocesses
        flst_nreal=0
        do i1 = -max_flav,max_flav
          do i2 = -max_flav,max_flav
            do i5 = -max_flav_out,max_flav_out
              do i6 = -max_flav_out,max_flav_out
                check = .false.
                if(isnInJ) then
                  if(i1.eq.i2.and.i1.eq.i5.and.i5.eq.i6) check = .true. ! q q -> nI nJ q q
                  if(i1.eq.-i2.and.i1.ne.0.and.i5.eq.-i6.and.i5.ge.0) check = .true. ! q qbar -> nI nJ q qbar, q qbar -> nI nJ g g (mixed flavours)
                  if((i1.eq.i5.or.i2.eq.i5).and.(i1+i2).eq.(i5+i6).and.i5.ne.0) check = .true. ! q qbar -> nI nJ q qbar
                  if((isg(i1).or.isg(i2)).and.(i1+i2).eq.(i5+i6).and.isg(i6)) check = .true. ! q g -> nI nJ q g
                  if(isg(i1).and.isg(i2).and.i5.eq.-i6.and.i5.gt.0) check = .true. ! g g -> nI nJ q qbar
                  if(isg(i1).and.isg(i2).and.isg(i5).and.isg(i6)) check = .false. ! exclude g g -> nI nJ g g
                endif
                if(isxIxJ) then
                  if(i1.eq.i2.and.i1.eq.i5.and.i5.eq.i6) check = .true. ! q q -> nI nJ q q
                  if(i1.eq.-i2.and.i1.ne.0.and.i5.eq.-i6.and.i5.ge.0) check = .true. ! q qbar -> nI nJ q qbar, q qbar -> nI nJ g g (mixed flavours)
                  if(i1.ne.0.and.i2.ne.0.and.i5.ne.0.and.i6.ne.0.and.(i1+i2).eq.(i5+i6).and.(qgen(i1)+qgen(i2)).eq.(qgen(i5)+qgen(i6))) check = .true. ! q qbar -> nI nJ q qbar
                  if(i1.ne.0.and.i2.ne.0.and.i5.ne.0.and.i6.ne.0.and.(i1+i2).eq.(i5+i6).and.qgen(i1).eq.qgen(i2).and.qgen(i5).eq.qgen(i6)) check = .true. ! q qbar -> nI nJ q qbar
                  if((isg(i1).or.isg(i2)).and.(i1+i2).eq.(i5+i6).and.isg(i6)) check = .true. ! q g -> nI nJ q g
                  if(isg(i1).and.isg(i2).and.i5.eq.-i6.and.i5.gt.0) check = .true. ! g g -> nI nJ q qbar
                  if(isg(i1).and.isg(i2).and.isg(i5).and.isg(i6)) check = .false. ! exclude g g -> nI nJ g g
                  if(qgen(i1).eq.qgen(i2).and.qgen(i5).ne.qgen(i6)) check = .false. ! sort out not allowed flavour combination
                  if(qgen(i1).ne.qgen(i2).and.qgen(i5).eq.qgen(i6)) check = .false.
                endif
                if(isnIxJ.and.i4.gt.0.and.dabs(elc(i1)+elc(i2)-elc(i5)-elc(i6)-1D0).lt.1D-3) then
                  if(isg(i1).and.isg(i2).and.isd(i5).and.isubar(i6).and.(i5+i6).eq.-1) check = .true. ! g g -> nI xJ d ubar
                  if(isg(i1).and.isdbar(i2).and.isubar(i5).and.isg(i6).and.(i2-i5).eq.1) check = .true. ! g dbar -> ubar g
                  if(isg(i1).and.isu(i2).and.isd(i5).and.isg(i6).and.(i2-i5).eq.1) check = .true. ! g u -> nI xJ d g
                  if(isu(i1).and.isdbar(i2).and.isg(i5).and.isg(i6).and.(i1+i2).eq.1) check = .true. ! u dbar -> nI xJ g g
                  if(i1.ne.0.and.i2.ne.0.and.(qgen(i1)+qgen(i2)).eq.(qgen(i5)+qgen(i6)).and.abs(abs(i1)-abs(i2)-abs(i5)+abs(i6)).le.1) check = .true. ! q q -> nI xJ q q
                  if(i1.gt.0.and.i2.lt.0.and.(qgen(i1)-qgen(i2)).eq.0.and.(qgen(i5)-qgen(i6)).eq.0) check = .true. ! q q -> nI xJ q q
                  if(qgen(i1).eq.qgen(i5).and.qgen(i2).eq.qgen(i6).and..not.(i1.eq.i5.or.i1.eq.i6.or.i2.eq.i5.or.i2.eq.i6)) check = .false. ! sort out not allowed flavour combination
                  if(qgen(i1).eq.qgen(i6).and.qgen(i2).eq.qgen(i5).and..not.(i1.eq.i5.or.i1.eq.i6.or.i2.eq.i5.or.i2.eq.i6)) check = .false.
                endif
                if(isnIxJ.and.i4.lt.0.and.dabs(elc(i1)+elc(i2)-elc(i5)-elc(i6)+1D0).lt.1D-3) then
                  if(isg(i1).and.isg(i2).and.isdbar(i5).and.isu(i6).and.(i5+i6).eq.1) check = .true. ! g g -> nI xJ dbar u
                  if(isg(i1).and.isd(i2).and.isu(i5).and.isg(i6).and.(i2-i5).eq.-1) check = .true. ! g d -> u g
                  if(isg(i1).and.isubar(i2).and.isdbar(i5).and.isg(i6).and.(i2-i5).eq.-1) check = .true. ! g ubar -> nI xJ dbar g
                  if(isubar(i1).and.isd(i2).and.isg(i5).and.isg(i6).and.(i1+i2).eq.-1) check = .true. ! ubar d -> nI xJ g g
                  if(i1.ne.0.and.i2.ne.0.and.(qgen(i1)+qgen(i2)).eq.(qgen(i5)+qgen(i6)).and.abs(abs(i1)-abs(i2)-abs(i5)+abs(i6)).le.1) check = .true. ! q q -> nI xJ q q
                  if(i1.gt.0.and.i2.lt.0.and.(qgen(i1)-qgen(i2)).eq.0.and.(qgen(i5)-qgen(i6)).eq.0) check = .true. ! q q -> nI xJ q q
                  if(qgen(i1).eq.qgen(i5).and.qgen(i2).eq.qgen(i6).and..not.(i1.eq.i5.or.i1.eq.i6.or.i2.eq.i5.or.i2.eq.i6)) check = .false. ! sort out not allowed flavour combination
                  if(qgen(i1).eq.qgen(i6).and.qgen(i2).eq.qgen(i5).and..not.(i1.eq.i5.or.i1.eq.i6.or.i2.eq.i5.or.i2.eq.i6)) check = .false.
                endif
                if(check) then
                  flst_nreal = flst_nreal+1
                  if(flst_nreal.gt.maxprocreal) goto 998
                  do k=1,nlegreal
                    flst_real(k,flst_nreal) = ii(k)
                  enddo
                endif
              enddo
            enddo
          enddo
        enddo
        
        ! cross the first two initial states if they are not equal and
        ! add the new configuration to flst_real
        flst_max = flst_nreal
        do i=1,flst_max
          if(flst_real(1,i).ne.flst_real(2,i)) then
            flst_nreal = flst_nreal+1
            flst_real(1,flst_nreal) = flst_real(2,i)
            flst_real(2,flst_nreal) = flst_real(1,i)
            flst_real(3,flst_nreal) = flst_real(3,i)
            flst_real(4,flst_nreal) = flst_real(4,i)
            flst_real(5,flst_nreal) = flst_real(5,i)
            flst_real(6,flst_nreal) = flst_real(6,i)
          endif
        enddo
        
        ! reorder the final states. This improves the speed of the 
        ! amplitudes because they are ordered in the same way -> no
        ! additional crossing needed.
        ! Rules:
        ! particles before anti-particles, then d-types before u-types,
        ! then lower numbers before higher numbers
        ! gluons come always last.
        do i=1,flst_nreal
          i5 = flst_real(5,i)
          i6 = flst_real(6,i)
          if(i5.lt.0.and.i6.gt.0 .or.
     &       i5.lt.0.and.i6.lt.0.and.iseven(abs(i5)).and.isodd(abs(i6)) .or.
     &       i5.gt.0.and.i6.gt.0.and.iseven(abs(i5)).and.isodd(abs(i6)) .or.
     &       i5.lt.0.and.i6.lt.0.and.isodd(abs(i5)).and.isodd(abs(i6))
     &         .and.(abs(i5).gt.abs(i6).or.(i5.eq.-i6.and.i5.gt.0)) .or.
     &       i5.lt.0.and.i6.lt.0.and.iseven(abs(i5)).and.iseven(abs(i6))
     &         .and.(abs(i5).gt.abs(i6).or.(i5.eq.-i6.and.i5.gt.0)) .or.
     &       i5.gt.0.and.i6.gt.0.and.isodd(abs(i5)).and.isodd(abs(i6))
     &         .and.(abs(i5).gt.abs(i6).or.(i5.eq.-i6.and.i5.gt.0)) .or.
     &       i5.gt.0.and.i6.gt.0.and.iseven(abs(i5)).and.iseven(abs(i6))
     &         .and.(abs(i5).gt.abs(i6).or.(i5.eq.-i6.and.i5.gt.0))) then
           temp = flst_real(6,i)
           flst_real(6,i) = flst_real(5,i)
           flst_real(5,i) = temp
          endif
        enddo
        
        call remove_duplicates(flst_real,nlegreal,flst_nreal)
        
#ifdef DEBUGQ
        nflst_select = 1
        ! Real process
        flst_nreal = 1
        flst_real(1,1) = flst_real(1,nflst_select)
        flst_real(2,1) = flst_real(2,nflst_select)
        flst_real(3,1) = flst_real(3,nflst_select)
        flst_real(4,1) = flst_real(4,nflst_select)
        flst_real(5,1) = flst_real(5,nflst_select)
        flst_real(6,1) = flst_real(6,nflst_select)
#endif
        
#ifdef DEBUGQ
        flst_nreal = 1
        flst_real(1,1) = -2
        flst_real(2,1) = 2
        flst_real(3,1) = 1000022
        flst_real(4,1) = 1000023
        flst_real(5,1) = 2
        flst_real(6,1) = -2
#endif

#ifdef DEBUG
        print*,' born processes',flst_nborn
        do j=1,flst_nborn
          print*,(flst_born(k,j),k=1,nlegborn)
        enddo
#endif

#ifdef DEBUG
        print*,' real processes',flst_nreal
        do j=1,flst_nreal
          print*,(flst_real(k,j),k=1,nlegreal)
        enddo
#endif

#if defined(DSUB_I) || defined(DSUB_II) || defined(DR_II)
        ! setup the flavor list of on-shell resonant diagrams
        ! reset the number of on-shell resonant diagrams
        flst_nosres = 0
        ! fill the flavor list with on-shell resonant diagrams
        do i=1,flst_nreal
          do j=1,nlegreal
            ii(j) = flst_real(j,i)
          enddo
          ! gluino and squark resonances for nInJ and nIxJ
          ! all combinations of d, dbar, u, ubar, but no gluon
          if((isnIxJ.or.isnInJ).and.i5.ne.0.and.i6.ne.0) then
            flst_nosres = flst_nosres+1 
            do j=1,nlegreal
              ! copy existing reals
              flst_osres(j,flst_nosres) = flst_real(j,i)
            enddo
          endif
          ! gluino and squark resonances for xIxJ
          ! all combinations of d, dbar, u, ubar, but no gluons
          ! and without d d, u u, dbar dbar, ubar ubar
          if(isxIxJ.and.
     &       ((.not.((i5.ge.0.and.i6.ge.0).or.(i5.le.0.and.i6.le.0))).or.
     &       (((i5.gt.0.and.i6.gt.0).or.(i5.lt.0.and.i6.lt.0)).and.mod(i5+i6,2).ne.0))) then
            flst_nosres = flst_nosres+1 
            do j=1,nlegreal
              ! copy existing reals
              flst_osres(j,flst_nosres) = flst_real(j,i)
            enddo
          endif
        enddo

#ifdef DEBUG
        print*,' on-shell resonant real processes',flst_nosres
        do j=1,flst_nosres
          print*,(flst_osres(k,j),k=1,nlegreal)
        enddo
        !stop
#endif
#endif

        return
 998    print*,'init_processes: increase maxprocreal'
        stop
 999    print*,'init_processes: increase maxprocborn'
        stop
      end      
c############### end subroutine init_processes #########################
