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

        integer i1,i2,i3,i4,i5,k,ii(5)
        equivalence (i1,ii(1)),(i2,ii(2)),(i3,ii(3)),(i4,ii(4)),
     &              (i5,ii(5))
        logical debug
        parameter (debug=.true.)
        integer j
        real*8 powheginput
        external powheginput
        logical condition, mixed_channels
        integer max_flav,encode_pair

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

        ! channels with one neutralino and one chargino in final state
        if ( ((i3.eq.1000022).and.(i4.eq.-1000024)) ) then
          mixed_channels = .true.
        elseif ( ((i3.eq.1000022).and.(i4.eq.1000022)) ) then
          mixed_channels = .true.

        else
          print*, "requested final states not implemented"
          print*, "swap final states and try again"
          stop
        endif

        print*
        print*, "===================================================="
        print*, "        neutralino/chargino pair production         "
        print*, "      final states: ", i3, i4
        print*, "                                                    "
        if     (i3.eq.1000022) then
          print*, "             q                         n1           "
        elseif (i3.eq.-1000024) then
          print*, "             q                         x1-          "
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
        elseif (i4.eq.-1000024) then
          print*, "             q*                        x1-          "
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

        ! Born subprocesses
        flst_nborn=0
        do i1=-max_flav,max_flav
          do i2=-max_flav,max_flav
            ! q qbar (with mixed flavor in the initial state, e.g. u dbar)
            ! d ubar channels
            if(mixed_channels.and.i4.lt.0) then
              if( (i1.ne.0) .and. (i2.ne.0) .and. (i1+i2.eq.-1)
     &           .and. (mod(i1,2).eq.0 .or. mod(i1,2).eq.1)
     &           .and. (mod(i2,2).eq.0 .or. mod(i2,2).eq.1) ) then
                flst_nborn=flst_nborn+1
                if(flst_nborn.gt.maxprocborn) goto 999
                do k=1,nlegborn
                  flst_born(k,flst_nborn)=ii(k)
                enddo
              endif
            ! dbar u channels
            elseif(mixed_channels .and. i4.gt.0) then
              if( (i1.ne.0) .and. (i2.ne.0) .and. (i1+i2.eq.1)
     &           .and. (mod(i1,2).eq.-1 .or. mod(i1,2).eq.0)
     &           .and. (mod(i2,2).eq.-1 .or. mod(i2,2).eq.0) ) then
                flst_nborn=flst_nborn+1
                if(flst_nborn.gt.maxprocborn) goto 999
                do k=1,nlegborn
                  flst_born(k,flst_nborn)=ii(k)
                enddo
              endif
            ! q qbar (same flavors in the initial state)
            else
              if(i1.ne.0.and.i1+i2.eq.0) then
                flst_nborn=flst_nborn+1
                if(flst_nborn.gt.maxprocborn) goto 999
                do k=1,nlegborn
                  flst_born(k,flst_nborn)=ii(k)
                enddo
              endif
            endif
          enddo
        enddo

              flst_born(   1,   1)=          -1
      flst_born(   2,   1)=           1
      flst_born(   3,   1)=     1000022
      flst_born(   4,   1)=     1000022
 
      flst_born(   1,   2)=           1
      flst_born(   2,   2)=          -1
      flst_born(   3,   2)=     1000022
      flst_born(   4,   2)=     1000022
 
      flst_born(   1,   3)=          -2
      flst_born(   2,   3)=           2
      flst_born(   3,   3)=     1000022
      flst_born(   4,   3)=     1000022
 
      flst_born(   1,   4)=           2
      flst_born(   2,   4)=          -2
      flst_born(   3,   4)=     1000022
      flst_born(   4,   4)=     1000022
 
      flst_born(   1,   5)=          -4
      flst_born(   2,   5)=           4
      flst_born(   3,   5)=     1000022
      flst_born(   4,   5)=     1000022
 
      flst_born(   1,   6)=           4
      flst_born(   2,   6)=          -4
      flst_born(   3,   6)=     1000022
      flst_born(   4,   6)=     1000022
 
      flst_born(   1,   7)=          -3
      flst_born(   2,   7)=           3
      flst_born(   3,   7)=     1000022
      flst_born(   4,   7)=     1000022
 
      flst_born(   1,   8)=           3
      flst_born(   2,   8)=          -3
      flst_born(   3,   8)=     1000022
      flst_born(   4,   8)=     1000022
 
      flst_born(   1,   9)=          -5
      flst_born(   2,   9)=           5
      flst_born(   3,   9)=     1000022
      flst_born(   4,   9)=     1000022
 
      flst_born(   1,  10)=           5
      flst_born(   2,  10)=          -5
      flst_born(   3,  10)=     1000022
      flst_born(   4,  10)=     1000022
 
      flst_nborn=          10
        
#ifdef DEBUG
           print*,' born processes',flst_nborn
           do j=1,flst_nborn
              print*,(flst_born(k,j),k=1,nlegborn)
           enddo
#endif

        ! real subprocesses
        flst_nreal=0
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
                flst_nreal=flst_nreal+1
                if(flst_nreal.gt.maxprocreal) goto 998
                do k=1,nlegreal
                  flst_real(k,flst_nreal)=ii(k)
                enddo
              endif
            enddo
          enddo
        enddo
        
              flst_real(   1,   1)=          -1
      flst_real(   2,   1)=           1
      flst_real(   3,   1)=     1000022
      flst_real(   4,   1)=     1000022
      flst_real(   5,   1)=           0
 
      flst_real(   1,   2)=          -1
      flst_real(   2,   2)=           0
      flst_real(   3,   2)=     1000022
      flst_real(   4,   2)=     1000022
      flst_real(   5,   2)=          -1
 
      flst_real(   1,   3)=           1
      flst_real(   2,   3)=          -1
      flst_real(   3,   3)=     1000022
      flst_real(   4,   3)=     1000022
      flst_real(   5,   3)=           0
 
      flst_real(   1,   4)=           1
      flst_real(   2,   4)=           0
      flst_real(   3,   4)=     1000022
      flst_real(   4,   4)=     1000022
      flst_real(   5,   4)=           1
 
      flst_real(   1,   5)=          -2
      flst_real(   2,   5)=           2
      flst_real(   3,   5)=     1000022
      flst_real(   4,   5)=     1000022
      flst_real(   5,   5)=           0
 
      flst_real(   1,   6)=          -2
      flst_real(   2,   6)=           0
      flst_real(   3,   6)=     1000022
      flst_real(   4,   6)=     1000022
      flst_real(   5,   6)=          -2
 
      flst_real(   1,   7)=           2
      flst_real(   2,   7)=          -2
      flst_real(   3,   7)=     1000022
      flst_real(   4,   7)=     1000022
      flst_real(   5,   7)=           0
 
      flst_real(   1,   8)=           2
      flst_real(   2,   8)=           0
      flst_real(   3,   8)=     1000022
      flst_real(   4,   8)=     1000022
      flst_real(   5,   8)=           2
 
      flst_real(   1,   9)=          -4
      flst_real(   2,   9)=           4
      flst_real(   3,   9)=     1000022
      flst_real(   4,   9)=     1000022
      flst_real(   5,   9)=           0
 
      flst_real(   1,  10)=          -4
      flst_real(   2,  10)=           0
      flst_real(   3,  10)=     1000022
      flst_real(   4,  10)=     1000022
      flst_real(   5,  10)=          -4
 
      flst_real(   1,  11)=           4
      flst_real(   2,  11)=          -4
      flst_real(   3,  11)=     1000022
      flst_real(   4,  11)=     1000022
      flst_real(   5,  11)=           0
 
      flst_real(   1,  12)=           4
      flst_real(   2,  12)=           0
      flst_real(   3,  12)=     1000022
      flst_real(   4,  12)=     1000022
      flst_real(   5,  12)=           4
 
      flst_real(   1,  13)=          -3
      flst_real(   2,  13)=           3
      flst_real(   3,  13)=     1000022
      flst_real(   4,  13)=     1000022
      flst_real(   5,  13)=           0
 
      flst_real(   1,  14)=          -3
      flst_real(   2,  14)=           0
      flst_real(   3,  14)=     1000022
      flst_real(   4,  14)=     1000022
      flst_real(   5,  14)=          -3
 
      flst_real(   1,  15)=           3
      flst_real(   2,  15)=          -3
      flst_real(   3,  15)=     1000022
      flst_real(   4,  15)=     1000022
      flst_real(   5,  15)=           0
 
      flst_real(   1,  16)=           3
      flst_real(   2,  16)=           0
      flst_real(   3,  16)=     1000022
      flst_real(   4,  16)=     1000022
      flst_real(   5,  16)=           3
 
      flst_real(   1,  17)=          -5
      flst_real(   2,  17)=           5
      flst_real(   3,  17)=     1000022
      flst_real(   4,  17)=     1000022
      flst_real(   5,  17)=           0
 
      flst_real(   1,  18)=          -5
      flst_real(   2,  18)=           0
      flst_real(   3,  18)=     1000022
      flst_real(   4,  18)=     1000022
      flst_real(   5,  18)=          -5
 
      flst_real(   1,  19)=           5
      flst_real(   2,  19)=          -5
      flst_real(   3,  19)=     1000022
      flst_real(   4,  19)=     1000022
      flst_real(   5,  19)=           0
 
      flst_real(   1,  20)=           5
      flst_real(   2,  20)=           0
      flst_real(   3,  20)=     1000022
      flst_real(   4,  20)=     1000022
      flst_real(   5,  20)=           5
 
      flst_real(   1,  21)=           0
      flst_real(   2,  21)=          -1
      flst_real(   3,  21)=     1000022
      flst_real(   4,  21)=     1000022
      flst_real(   5,  21)=          -1
 
      flst_real(   1,  22)=           0
      flst_real(   2,  22)=           1
      flst_real(   3,  22)=     1000022
      flst_real(   4,  22)=     1000022
      flst_real(   5,  22)=           1
 
      flst_real(   1,  23)=           0
      flst_real(   2,  23)=          -2
      flst_real(   3,  23)=     1000022
      flst_real(   4,  23)=     1000022
      flst_real(   5,  23)=          -2
 
      flst_real(   1,  24)=           0
      flst_real(   2,  24)=           2
      flst_real(   3,  24)=     1000022
      flst_real(   4,  24)=     1000022
      flst_real(   5,  24)=           2
 
      flst_real(   1,  25)=           0
      flst_real(   2,  25)=          -4
      flst_real(   3,  25)=     1000022
      flst_real(   4,  25)=     1000022
      flst_real(   5,  25)=          -4
 
      flst_real(   1,  26)=           0
      flst_real(   2,  26)=           4
      flst_real(   3,  26)=     1000022
      flst_real(   4,  26)=     1000022
      flst_real(   5,  26)=           4
 
      flst_real(   1,  27)=           0
      flst_real(   2,  27)=          -3
      flst_real(   3,  27)=     1000022
      flst_real(   4,  27)=     1000022
      flst_real(   5,  27)=          -3
 
      flst_real(   1,  28)=           0
      flst_real(   2,  28)=           3
      flst_real(   3,  28)=     1000022
      flst_real(   4,  28)=     1000022
      flst_real(   5,  28)=           3
 
      flst_real(   1,  29)=           0
      flst_real(   2,  29)=          -5
      flst_real(   3,  29)=     1000022
      flst_real(   4,  29)=     1000022
      flst_real(   5,  29)=          -5
 
      flst_real(   1,  30)=           0
      flst_real(   2,  30)=           5
      flst_real(   3,  30)=     1000022
      flst_real(   4,  30)=     1000022
      flst_real(   5,  30)=           5
 
      flst_nreal=          30
        
#ifdef DEBUG
        print*,' real processes',flst_nreal
        do j=1,flst_nreal
          print*,(flst_real(k,j),k=1,nlegreal)
        enddo
#endif

        return
 998    print*,'init_processes: increase maxprocreal'
        stop
 999    print*,'init_processes: increase maxprocborn'
        stop
      end
      
c############### end subroutine init_processes #########################