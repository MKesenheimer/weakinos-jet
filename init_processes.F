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

        integer i1,i2,i3,i4,i5,i6,k,ii(6)
        equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),(i4,ii(4)),
     &              (i5,ii(5)),(i6,ii(6))
        logical debug
        parameter (debug=.true.)
        integer j
        real*8 powheginput
        external powheginput
        logical condition, mixed_channels
        integer max_flav,encode_pair
        integer temp
#ifdef DEBUG
        integer nflst_select
#endif

        ! set number of active flavors for incoming partons
        max_flav = int(powheginput('incomingflavors'))
        if (max_flav.eq.5)  print*,"WARNING: ignoring sbottom mixing"
        if (max_flav.gt.5)  stop "can't handle more than 5 flavours"

        ! whether to use fake virtuals or not
        if(powheginput("#fakevirtuals").gt.0) then
           if(int(powheginput("fakevirtuals")).gt.0) then
             flg_fakevirtuals = .true.
           endif
        else
          flg_fakevirtuals = .false.
        endif
        
        ! number of light quarks:
#ifdef NEGLECTBMASS
        st_nlight = 5
#else
        st_nlight = 4
#endif
        
        ! choose process
        i3 = int(powheginput("fin1"))
        i4 = int(powheginput("fin2"))

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
          mixed_channels = .false.
          
        ! channels with two charginos in final state
        else if ( ((i3.eq.1000024).and.(i4.eq.-1000024))
     &       .or. ((i3.eq.1000024).and.(i4.eq.-1000037))
     &       .or. ((i3.eq.1000037).and.(i4.eq.-1000037)) ) then
          mixed_channels = .false.

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
          mixed_channels = .true.

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
        print*,' number of incoming flavors: ', max_flav

        ! set process id: encode types of slepton A and B
        lprup(1) = encode_pair(i3,i4)
        print*,' encoded final states to process ID: ', lprup(1)

        ! index of the first coloured particle in the final state
        ! (all subsequent particles are coloured)
        flst_lightpart=5

        ! Born subprocesses (should be 30 for n1n1)
        flst_nborn=0
        do i1=-max_flav,max_flav
          do i2=-max_flav,max_flav
            do i5=-max_flav,max_flav
              condition=.false.
              ! q qbar (with mixed flavor in the initial state, e.g. u dbar)
              ! d ubar channels
              if(mixed_channels.and.i4.lt.0) then
                if(.not.(i1.eq.0.and.i2.eq.0)
     &           .and. (mod(i1,2).eq.0 .or. mod(i1,2).eq.1)
     &           .and. (mod(i2,2).eq.0 .or. mod(i2,2).eq.1) ) then   ! exclude gg
                  if((i1.ne.0).and.(i1+i2.eq.-1).and.(i5.eq.0)) then ! q qbar -> g
                    condition=.true.
                  elseif((i1.eq.0).and.(i2.eq.i5-1)
     &               .and. (mod(i2,2).eq.0 .or. mod(i2,2).eq.1)
     &               .and. (mod(i5,2).eq.0 .or. mod(i5,2).eq.-1) ) then ! g q
                    condition=.true.
                  elseif((i2.eq.0).and.(i1.eq.i5-1)
     &               .and. (mod(i1,2).eq.0 .or. mod(i1,2).eq.1)
     &               .and. (mod(i5,2).eq.0 .or. mod(i5,2).eq.-1) ) then ! q g
                    condition=.true.
                  endif
                endif
              ! dbar u channels
              elseif(mixed_channels.and.i4.gt.0) then
                if(.not.(i1.eq.0.and.i2.eq.0)
     &           .and. (mod(i1,2).eq.-1 .or. mod(i1,2).eq.0)
     &           .and. (mod(i2,2).eq.-1 .or. mod(i2,2).eq.0) ) then ! exclude gg
                  if((i1.ne.0).and.(i1+i2.eq.1).and.(i5.eq.0)) then ! q qbar -> g
                    condition=.true.
                  elseif((i1.eq.0).and.(i2.eq.i5+1)
     &               .and. (mod(i2,2).eq.0 .or. mod(i2,2).eq.-1)
     &               .and. (mod(i5,2).eq.0 .or. mod(i5,2).eq.1) ) then ! g q
                    condition=.true.
                  elseif((i2.eq.0).and.(i1.eq.i5+1)
     &               .and. (mod(i1,2).eq.0 .or. mod(i1,2).eq.-1)
     &               .and. (mod(i5,2).eq.0 .or. mod(i5,2).eq.1) ) then ! q g
                    condition=.true.
                  endif
                endif
              ! q qbar (same flavors in the initial state)
              else
                if(.not.(i1.eq.0.and.i2.eq.0)) then                 ! exclude gg
                  if((i1.ne.0).and.(i1+i2.eq.0).and.(i5.eq.0)) then ! q qbar -> g
                    condition=.true.
                  elseif((i1.eq.0).and.(i2.eq.i5)) then ! g q
                    condition=.true.
                  elseif((i2.eq.0).and.(i1.eq.i5)) then ! q g
                    condition=.true.
                  endif
                endif
              endif
              if(condition) then
                flst_nborn=flst_nborn+1
                if(flst_nborn.gt.maxprocborn) goto 998
                do k=1,nlegborn
                  flst_born(k,flst_nborn)=ii(k)
                enddo
              endif
            enddo
          enddo
        enddo

        ! real subprocesses (should be 175 for n1n1)
        flst_nreal=0
        do i1=-max_flav,max_flav
          do i2=-max_flav,max_flav
            do i5=-max_flav,max_flav
              do i6=-max_flav,max_flav
                condition=.false.
                if(.not.mixed_channels) then ! nI & nJ, xI & xJ
                  if(i1.eq.i2 .and. i1.eq.i5 .and. i5.eq.i6) then ! q q -> n3 n4 q q
                    condition = .true.
                  endif
                  if(i1.eq.-i2 .and. i1.ne.0 .and. i5.eq.-i6 ! q -q -> n3 n4 q -q, g g
     &               .and. i5.ge.0) then
                    condition = .true.
                  endif
                  if((i1.eq.i5 .or. i2.eq.i5) .and. (i1+i2).eq.(i5+i6) ! q q -> n3 n4 q q (mixed)
     &               .and. abs(i5).lt.abs(i6) .and. i5.ne.0) then
                    condition = .true.
                  endif
                  if((i1.eq.0 .or. i2.eq.0) .and. (i1+i2).eq.(i5+i6) ! g q -> n3 n4 g q
     &               .and. i6.eq.0) then
                    condition = .true.
                  endif
                  if(i1.eq.0 .and. i2.eq.0 .and. i5.eq.-i6 ! g g -> n3 n4 q -q
     &               .and. i5.gt.0) then
                    condition = .true.
                  endif
                  if(i1.eq.0 .and. i2.eq.0 .and. i5.eq.0 ! don't allow g g -> n3 n4 g g
     &               .and. i6.eq.0) then
                    condition = .false.
                  endif
                endif
                if(condition) then
                  flst_nreal=flst_nreal+1
                  if(flst_nreal.gt.maxprocreal) goto 998
                  do k=1,nlegreal
                    flst_real(k,flst_nreal)=ii(k)
                  enddo
                endif
              enddo
            enddo
          enddo
        enddo
        
#ifdef DEBUGQ
        nflst_select = 1 ! already tested, and ok: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14
        ! 15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 33, 34
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

#ifdef DEBUGQ
        ! Born process
        flst_nborn = 1
        flst_born(1,1) = -2
        flst_born(2,1) = 2
        flst_born(3,1) = 1000022
        flst_born(4,1) = 1000023
        flst_born(5,1) = 0

        ! Real process
        flst_nreal = 1
        flst_real(1,1) = -2
        flst_real(2,1) = 2
        flst_real(3,1) = 1000022
        flst_real(4,1) = 1000023
        flst_real(5,1) = 0
        flst_real(6,1) = 0
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

        ! setup the flavor list of on-shell resonant diagrams
        call init_processes_osres
#ifdef DEBUG
        print*,' on-shell resonant real processes',flst_nosres
        do j=1,flst_nosres
          print*,(flst_osres(k,j),k=1,nlegreal)
        enddo
#endif

        !stop

        return
 998    print*,'init_processes: increase maxprocreal'
        stop
 999    print*,'init_processes: increase maxprocborn'
        stop
      end      
c############### end subroutine init_processes #########################

c############### subroutine init_osres #################################
c if the real-amplitudes are resonant split into a regular part and the 
c IR-divergent parts: this routine provides the corresponding flavor 
c lists and stores it in flst_osres
c this list should not be confused with flst_realres.
      subroutine init_processes_osres
        implicit none
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_flg.h"
#include "pwhg_flst_add.h"
#include "osres.h"
        ! local variables
        integer i1,i2
        integer k,f(6)

        ! reset the number of on-shell resonant diagrams
        flst_nosres = 0
        ! fill the flavor list with on-shell resonant diagrams
        do i1=1,flst_nreal
          do i2=1,nlegreal
            f(i2) = flst_real(i2,i1)
          enddo
          ! gluino and squark resonances
          if(f(5).ne.0 .and. f(6).ne.0) then
            flst_nosres = flst_nosres+1 
            do i2=1,nlegreal
              ! copy existing reals
              flst_osres(i2,flst_nosres) = flst_real(i2,i1)
            enddo
          endif
        enddo
      end
c############### end subroutine init_osres #############################
