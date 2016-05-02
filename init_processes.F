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
          mixed_channels = .false.

        else
          print*, "requested final states not implemented"
          print*, "swap final states and try again"
          stop
        endif

        print*
        print*, "===================================================="
        print*, "        neutralino/chargino pair production         "
        call print_version
        print*, "        final states: ", i3, i4
        print*
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
      flst_born(   5,   1)=           0
 
      flst_born(   1,   2)=          -1
      flst_born(   2,   2)=           0
      flst_born(   3,   2)=     1000022
      flst_born(   4,   2)=     1000022
      flst_born(   5,   2)=          -1
 
      flst_born(   1,   3)=           1
      flst_born(   2,   3)=          -1
      flst_born(   3,   3)=     1000022
      flst_born(   4,   3)=     1000022
      flst_born(   5,   3)=           0
 
      flst_born(   1,   4)=           1
      flst_born(   2,   4)=           0
      flst_born(   3,   4)=     1000022
      flst_born(   4,   4)=     1000022
      flst_born(   5,   4)=           1
 
      flst_born(   1,   5)=          -2
      flst_born(   2,   5)=           2
      flst_born(   3,   5)=     1000022
      flst_born(   4,   5)=     1000022
      flst_born(   5,   5)=           0
 
      flst_born(   1,   6)=          -2
      flst_born(   2,   6)=           0
      flst_born(   3,   6)=     1000022
      flst_born(   4,   6)=     1000022
      flst_born(   5,   6)=          -2
 
      flst_born(   1,   7)=           2
      flst_born(   2,   7)=          -2
      flst_born(   3,   7)=     1000022
      flst_born(   4,   7)=     1000022
      flst_born(   5,   7)=           0
 
      flst_born(   1,   8)=           2
      flst_born(   2,   8)=           0
      flst_born(   3,   8)=     1000022
      flst_born(   4,   8)=     1000022
      flst_born(   5,   8)=           2
 
      flst_born(   1,   9)=          -4
      flst_born(   2,   9)=           4
      flst_born(   3,   9)=     1000022
      flst_born(   4,   9)=     1000022
      flst_born(   5,   9)=           0
 
      flst_born(   1,  10)=          -4
      flst_born(   2,  10)=           0
      flst_born(   3,  10)=     1000022
      flst_born(   4,  10)=     1000022
      flst_born(   5,  10)=          -4
 
      flst_born(   1,  11)=           4
      flst_born(   2,  11)=          -4
      flst_born(   3,  11)=     1000022
      flst_born(   4,  11)=     1000022
      flst_born(   5,  11)=           0
 
      flst_born(   1,  12)=           4
      flst_born(   2,  12)=           0
      flst_born(   3,  12)=     1000022
      flst_born(   4,  12)=     1000022
      flst_born(   5,  12)=           4
 
      flst_born(   1,  13)=          -3
      flst_born(   2,  13)=           3
      flst_born(   3,  13)=     1000022
      flst_born(   4,  13)=     1000022
      flst_born(   5,  13)=           0
 
      flst_born(   1,  14)=          -3
      flst_born(   2,  14)=           0
      flst_born(   3,  14)=     1000022
      flst_born(   4,  14)=     1000022
      flst_born(   5,  14)=          -3
 
      flst_born(   1,  15)=           3
      flst_born(   2,  15)=          -3
      flst_born(   3,  15)=     1000022
      flst_born(   4,  15)=     1000022
      flst_born(   5,  15)=           0
 
      flst_born(   1,  16)=           3
      flst_born(   2,  16)=           0
      flst_born(   3,  16)=     1000022
      flst_born(   4,  16)=     1000022
      flst_born(   5,  16)=           3
 
      flst_born(   1,  17)=           0
      flst_born(   2,  17)=          -1
      flst_born(   3,  17)=     1000022
      flst_born(   4,  17)=     1000022
      flst_born(   5,  17)=          -1
 
      flst_born(   1,  18)=           0
      flst_born(   2,  18)=           1
      flst_born(   3,  18)=     1000022
      flst_born(   4,  18)=     1000022
      flst_born(   5,  18)=           1
 
      flst_born(   1,  19)=           0
      flst_born(   2,  19)=          -2
      flst_born(   3,  19)=     1000022
      flst_born(   4,  19)=     1000022
      flst_born(   5,  19)=          -2
 
      flst_born(   1,  20)=           0
      flst_born(   2,  20)=           2
      flst_born(   3,  20)=     1000022
      flst_born(   4,  20)=     1000022
      flst_born(   5,  20)=           2
 
      flst_born(   1,  21)=           0
      flst_born(   2,  21)=          -4
      flst_born(   3,  21)=     1000022
      flst_born(   4,  21)=     1000022
      flst_born(   5,  21)=          -4
 
      flst_born(   1,  22)=           0
      flst_born(   2,  22)=           4
      flst_born(   3,  22)=     1000022
      flst_born(   4,  22)=     1000022
      flst_born(   5,  22)=           4
 
      flst_born(   1,  23)=           0
      flst_born(   2,  23)=          -3
      flst_born(   3,  23)=     1000022
      flst_born(   4,  23)=     1000022
      flst_born(   5,  23)=          -3
 
      flst_born(   1,  24)=           0
      flst_born(   2,  24)=           3
      flst_born(   3,  24)=     1000022
      flst_born(   4,  24)=     1000022
      flst_born(   5,  24)=           3
 
      flst_nborn=          24
        
#ifdef DEBUG
           print*,' born processes',flst_nborn
           do j=1,flst_nborn
              print*,(flst_born(k,j),k=1,nlegborn)
           enddo
#endif

    
        
 
 
      flst_real(   1,   1)=          -2
      flst_real(   2,   1)=          -2
      flst_real(   3,   1)=     1000022
      flst_real(   4,   1)=     1000022
      flst_real(   5,   1)=          -2
      flst_real(   6,   1)=          -2
 
      flst_real(   1,   2)=          -2
      flst_real(   2,   2)=           2
      flst_real(   3,   2)=     1000022
      flst_real(   4,   2)=     1000022
      flst_real(   5,   2)=           2
      flst_real(   6,   2)=          -2
 
      flst_real(   1,   3)=          -2
      flst_real(   2,   3)=           2
      flst_real(   3,   3)=     1000022
      flst_real(   4,   3)=     1000022
      flst_real(   5,   3)=           1
      flst_real(   6,   3)=          -1
 
      flst_real(   1,   4)=          -2
      flst_real(   2,   4)=           2
      flst_real(   3,   4)=     1000022
      flst_real(   4,   4)=     1000022
      flst_real(   5,   4)=           4
      flst_real(   6,   4)=          -4
 
      flst_real(   1,   5)=          -2
      flst_real(   2,   5)=           2
      flst_real(   3,   5)=     1000022
      flst_real(   4,   5)=     1000022
      flst_real(   5,   5)=           3
      flst_real(   6,   5)=          -3
 
      flst_real(   1,   6)=          -2
      flst_real(   2,   6)=           2
      flst_real(   3,   6)=     1000022
      flst_real(   4,   6)=     1000022
      flst_real(   5,   6)=           0
      flst_real(   6,   6)=           0
 
      flst_real(   1,   7)=          -2
      flst_real(   2,   7)=          -1
      flst_real(   3,   7)=     1000022
      flst_real(   4,   7)=     1000022
      flst_real(   5,   7)=          -2
      flst_real(   6,   7)=          -1
 
      flst_real(   1,   8)=          -2
      flst_real(   2,   8)=           1
      flst_real(   3,   8)=     1000022
      flst_real(   4,   8)=     1000022
      flst_real(   5,   8)=          -2
      flst_real(   6,   8)=           1
 
      flst_real(   1,   9)=          -2
      flst_real(   2,   9)=          -4
      flst_real(   3,   9)=     1000022
      flst_real(   4,   9)=     1000022
      flst_real(   5,   9)=          -2
      flst_real(   6,   9)=          -4
 
      flst_real(   1,  10)=          -2
      flst_real(   2,  10)=           4
      flst_real(   3,  10)=     1000022
      flst_real(   4,  10)=     1000022
      flst_real(   5,  10)=          -2
      flst_real(   6,  10)=           4
 
      flst_real(   1,  11)=          -2
      flst_real(   2,  11)=          -3
      flst_real(   3,  11)=     1000022
      flst_real(   4,  11)=     1000022
      flst_real(   5,  11)=          -2
      flst_real(   6,  11)=          -3
 
      flst_real(   1,  12)=          -2
      flst_real(   2,  12)=           3
      flst_real(   3,  12)=     1000022
      flst_real(   4,  12)=     1000022
      flst_real(   5,  12)=          -2
      flst_real(   6,  12)=           3
 
      flst_real(   1,  13)=          -2
      flst_real(   2,  13)=           0
      flst_real(   3,  13)=     1000022
      flst_real(   4,  13)=     1000022
      flst_real(   5,  13)=          -2
      flst_real(   6,  13)=           0
 
      flst_real(   1,  14)=           2
      flst_real(   2,  14)=          -2
      flst_real(   3,  14)=     1000022
      flst_real(   4,  14)=     1000022
      flst_real(   5,  14)=           2
      flst_real(   6,  14)=          -2
 
      flst_real(   1,  15)=           2
      flst_real(   2,  15)=          -2
      flst_real(   3,  15)=     1000022
      flst_real(   4,  15)=     1000022
      flst_real(   5,  15)=           1
      flst_real(   6,  15)=          -1
 
      flst_real(   1,  16)=           2
      flst_real(   2,  16)=          -2
      flst_real(   3,  16)=     1000022
      flst_real(   4,  16)=     1000022
      flst_real(   5,  16)=           4
      flst_real(   6,  16)=          -4
 
      flst_real(   1,  17)=           2
      flst_real(   2,  17)=          -2
      flst_real(   3,  17)=     1000022
      flst_real(   4,  17)=     1000022
      flst_real(   5,  17)=           3
      flst_real(   6,  17)=          -3
 
      flst_real(   1,  18)=           2
      flst_real(   2,  18)=          -2
      flst_real(   3,  18)=     1000022
      flst_real(   4,  18)=     1000022
      flst_real(   5,  18)=           0
      flst_real(   6,  18)=           0
 
      flst_real(   1,  19)=           2
      flst_real(   2,  19)=           2
      flst_real(   3,  19)=     1000022
      flst_real(   4,  19)=     1000022
      flst_real(   5,  19)=           2
      flst_real(   6,  19)=           2
 
      flst_real(   1,  20)=           2
      flst_real(   2,  20)=          -1
      flst_real(   3,  20)=     1000022
      flst_real(   4,  20)=     1000022
      flst_real(   5,  20)=           2
      flst_real(   6,  20)=          -1
 
      flst_real(   1,  21)=           2
      flst_real(   2,  21)=           1
      flst_real(   3,  21)=     1000022
      flst_real(   4,  21)=     1000022
      flst_real(   5,  21)=           2
      flst_real(   6,  21)=           1
 
      flst_real(   1,  22)=           2
      flst_real(   2,  22)=          -4
      flst_real(   3,  22)=     1000022
      flst_real(   4,  22)=     1000022
      flst_real(   5,  22)=           2
      flst_real(   6,  22)=          -4
 
      flst_real(   1,  23)=           2
      flst_real(   2,  23)=           4
      flst_real(   3,  23)=     1000022
      flst_real(   4,  23)=     1000022
      flst_real(   5,  23)=           2
      flst_real(   6,  23)=           4
 
      flst_real(   1,  24)=           2
      flst_real(   2,  24)=          -3
      flst_real(   3,  24)=     1000022
      flst_real(   4,  24)=     1000022
      flst_real(   5,  24)=           2
      flst_real(   6,  24)=          -3
 
      flst_real(   1,  25)=           2
      flst_real(   2,  25)=           3
      flst_real(   3,  25)=     1000022
      flst_real(   4,  25)=     1000022
      flst_real(   5,  25)=           2
      flst_real(   6,  25)=           3
 
      flst_real(   1,  26)=           2
      flst_real(   2,  26)=           0
      flst_real(   3,  26)=     1000022
      flst_real(   4,  26)=     1000022
      flst_real(   5,  26)=           2
      flst_real(   6,  26)=           0
 
      flst_real(   1,  27)=          -1
      flst_real(   2,  27)=          -2
      flst_real(   3,  27)=     1000022
      flst_real(   4,  27)=     1000022
      flst_real(   5,  27)=          -2
      flst_real(   6,  27)=          -1
 
      flst_real(   1,  28)=          -1
      flst_real(   2,  28)=           2
      flst_real(   3,  28)=     1000022
      flst_real(   4,  28)=     1000022
      flst_real(   5,  28)=           2
      flst_real(   6,  28)=          -1
 
      flst_real(   1,  29)=          -1
      flst_real(   2,  29)=          -1
      flst_real(   3,  29)=     1000022
      flst_real(   4,  29)=     1000022
      flst_real(   5,  29)=          -1
      flst_real(   6,  29)=          -1
 
      flst_real(   1,  30)=          -1
      flst_real(   2,  30)=           1
      flst_real(   3,  30)=     1000022
      flst_real(   4,  30)=     1000022
      flst_real(   5,  30)=           2
      flst_real(   6,  30)=          -2
 
      flst_real(   1,  31)=          -1
      flst_real(   2,  31)=           1
      flst_real(   3,  31)=     1000022
      flst_real(   4,  31)=     1000022
      flst_real(   5,  31)=           1
      flst_real(   6,  31)=          -1
 
      flst_real(   1,  32)=          -1
      flst_real(   2,  32)=           1
      flst_real(   3,  32)=     1000022
      flst_real(   4,  32)=     1000022
      flst_real(   5,  32)=           4
      flst_real(   6,  32)=          -4
 
      flst_real(   1,  33)=          -1
      flst_real(   2,  33)=           1
      flst_real(   3,  33)=     1000022
      flst_real(   4,  33)=     1000022
      flst_real(   5,  33)=           3
      flst_real(   6,  33)=          -3
 
      flst_real(   1,  34)=          -1
      flst_real(   2,  34)=           1
      flst_real(   3,  34)=     1000022
      flst_real(   4,  34)=     1000022
      flst_real(   5,  34)=           0
      flst_real(   6,  34)=           0
 
      flst_real(   1,  35)=          -1
      flst_real(   2,  35)=          -4
      flst_real(   3,  35)=     1000022
      flst_real(   4,  35)=     1000022
      flst_real(   5,  35)=          -1
      flst_real(   6,  35)=          -4
 
      flst_real(   1,  36)=          -1
      flst_real(   2,  36)=           4
      flst_real(   3,  36)=     1000022
      flst_real(   4,  36)=     1000022
      flst_real(   5,  36)=          -1
      flst_real(   6,  36)=           4
 
      flst_real(   1,  37)=          -1
      flst_real(   2,  37)=          -3
      flst_real(   3,  37)=     1000022
      flst_real(   4,  37)=     1000022
      flst_real(   5,  37)=          -1
      flst_real(   6,  37)=          -3
 
      flst_real(   1,  38)=          -1
      flst_real(   2,  38)=           3
      flst_real(   3,  38)=     1000022
      flst_real(   4,  38)=     1000022
      flst_real(   5,  38)=          -1
      flst_real(   6,  38)=           3
 
      flst_real(   1,  39)=          -1
      flst_real(   2,  39)=           0
      flst_real(   3,  39)=     1000022
      flst_real(   4,  39)=     1000022
      flst_real(   5,  39)=          -1
      flst_real(   6,  39)=           0
 
      flst_real(   1,  40)=           1
      flst_real(   2,  40)=          -2
      flst_real(   3,  40)=     1000022
      flst_real(   4,  40)=     1000022
      flst_real(   5,  40)=          -2
      flst_real(   6,  40)=           1
 
      flst_real(   1,  41)=           1
      flst_real(   2,  41)=           2
      flst_real(   3,  41)=     1000022
      flst_real(   4,  41)=     1000022
      flst_real(   5,  41)=           2
      flst_real(   6,  41)=           1
 
      flst_real(   1,  42)=           1
      flst_real(   2,  42)=          -1
      flst_real(   3,  42)=     1000022
      flst_real(   4,  42)=     1000022
      flst_real(   5,  42)=           2
      flst_real(   6,  42)=          -2
 
      flst_real(   1,  43)=           1
      flst_real(   2,  43)=          -1
      flst_real(   3,  43)=     1000022
      flst_real(   4,  43)=     1000022
      flst_real(   5,  43)=           1
      flst_real(   6,  43)=          -1
 
      flst_real(   1,  44)=           1
      flst_real(   2,  44)=          -1
      flst_real(   3,  44)=     1000022
      flst_real(   4,  44)=     1000022
      flst_real(   5,  44)=           4
      flst_real(   6,  44)=          -4
 
      flst_real(   1,  45)=           1
      flst_real(   2,  45)=          -1
      flst_real(   3,  45)=     1000022
      flst_real(   4,  45)=     1000022
      flst_real(   5,  45)=           3
      flst_real(   6,  45)=          -3
 
      flst_real(   1,  46)=           1
      flst_real(   2,  46)=          -1
      flst_real(   3,  46)=     1000022
      flst_real(   4,  46)=     1000022
      flst_real(   5,  46)=           0
      flst_real(   6,  46)=           0
 
      flst_real(   1,  47)=           1
      flst_real(   2,  47)=           1
      flst_real(   3,  47)=     1000022
      flst_real(   4,  47)=     1000022
      flst_real(   5,  47)=           1
      flst_real(   6,  47)=           1
 
      flst_real(   1,  48)=           1
      flst_real(   2,  48)=          -4
      flst_real(   3,  48)=     1000022
      flst_real(   4,  48)=     1000022
      flst_real(   5,  48)=           1
      flst_real(   6,  48)=          -4
 
      flst_real(   1,  49)=           1
      flst_real(   2,  49)=           4
      flst_real(   3,  49)=     1000022
      flst_real(   4,  49)=     1000022
      flst_real(   5,  49)=           1
      flst_real(   6,  49)=           4
 
      flst_real(   1,  50)=           1
      flst_real(   2,  50)=          -3
      flst_real(   3,  50)=     1000022
      flst_real(   4,  50)=     1000022
      flst_real(   5,  50)=           1
      flst_real(   6,  50)=          -3
 
      flst_real(   1,  51)=           1
      flst_real(   2,  51)=           3
      flst_real(   3,  51)=     1000022
      flst_real(   4,  51)=     1000022
      flst_real(   5,  51)=           1
      flst_real(   6,  51)=           3
 
      flst_real(   1,  52)=           1
      flst_real(   2,  52)=           0
      flst_real(   3,  52)=     1000022
      flst_real(   4,  52)=     1000022
      flst_real(   5,  52)=           1
      flst_real(   6,  52)=           0
 
      flst_real(   1,  53)=          -4
      flst_real(   2,  53)=          -2
      flst_real(   3,  53)=     1000022
      flst_real(   4,  53)=     1000022
      flst_real(   5,  53)=          -2
      flst_real(   6,  53)=          -4
 
      flst_real(   1,  54)=          -4
      flst_real(   2,  54)=           2
      flst_real(   3,  54)=     1000022
      flst_real(   4,  54)=     1000022
      flst_real(   5,  54)=           2
      flst_real(   6,  54)=          -4
 
      flst_real(   1,  55)=          -4
      flst_real(   2,  55)=          -1
      flst_real(   3,  55)=     1000022
      flst_real(   4,  55)=     1000022
      flst_real(   5,  55)=          -1
      flst_real(   6,  55)=          -4
 
      flst_real(   1,  56)=          -4
      flst_real(   2,  56)=           1
      flst_real(   3,  56)=     1000022
      flst_real(   4,  56)=     1000022
      flst_real(   5,  56)=           1
      flst_real(   6,  56)=          -4
 
      flst_real(   1,  57)=          -4
      flst_real(   2,  57)=          -4
      flst_real(   3,  57)=     1000022
      flst_real(   4,  57)=     1000022
      flst_real(   5,  57)=          -4
      flst_real(   6,  57)=          -4
 
      flst_real(   1,  58)=          -4
      flst_real(   2,  58)=           4
      flst_real(   3,  58)=     1000022
      flst_real(   4,  58)=     1000022
      flst_real(   5,  58)=           2
      flst_real(   6,  58)=          -2
 
      flst_real(   1,  59)=          -4
      flst_real(   2,  59)=           4
      flst_real(   3,  59)=     1000022
      flst_real(   4,  59)=     1000022
      flst_real(   5,  59)=           1
      flst_real(   6,  59)=          -1
 
      flst_real(   1,  60)=          -4
      flst_real(   2,  60)=           4
      flst_real(   3,  60)=     1000022
      flst_real(   4,  60)=     1000022
      flst_real(   5,  60)=           4
      flst_real(   6,  60)=          -4
 
      flst_real(   1,  61)=          -4
      flst_real(   2,  61)=           4
      flst_real(   3,  61)=     1000022
      flst_real(   4,  61)=     1000022
      flst_real(   5,  61)=           3
      flst_real(   6,  61)=          -3
 
      flst_real(   1,  62)=          -4
      flst_real(   2,  62)=           4
      flst_real(   3,  62)=     1000022
      flst_real(   4,  62)=     1000022
      flst_real(   5,  62)=           0
      flst_real(   6,  62)=           0
 
      flst_real(   1,  63)=          -4
      flst_real(   2,  63)=          -3
      flst_real(   3,  63)=     1000022
      flst_real(   4,  63)=     1000022
      flst_real(   5,  63)=          -4
      flst_real(   6,  63)=          -3
 
      flst_real(   1,  64)=          -4
      flst_real(   2,  64)=           3
      flst_real(   3,  64)=     1000022
      flst_real(   4,  64)=     1000022
      flst_real(   5,  64)=          -4
      flst_real(   6,  64)=           3
 
      flst_real(   1,  65)=          -4
      flst_real(   2,  65)=           0
      flst_real(   3,  65)=     1000022
      flst_real(   4,  65)=     1000022
      flst_real(   5,  65)=          -4
      flst_real(   6,  65)=           0
 
      flst_real(   1,  66)=           4
      flst_real(   2,  66)=          -2
      flst_real(   3,  66)=     1000022
      flst_real(   4,  66)=     1000022
      flst_real(   5,  66)=          -2
      flst_real(   6,  66)=           4
 
      flst_real(   1,  67)=           4
      flst_real(   2,  67)=           2
      flst_real(   3,  67)=     1000022
      flst_real(   4,  67)=     1000022
      flst_real(   5,  67)=           2
      flst_real(   6,  67)=           4
 
      flst_real(   1,  68)=           4
      flst_real(   2,  68)=          -1
      flst_real(   3,  68)=     1000022
      flst_real(   4,  68)=     1000022
      flst_real(   5,  68)=          -1
      flst_real(   6,  68)=           4
 
      flst_real(   1,  69)=           4
      flst_real(   2,  69)=           1
      flst_real(   3,  69)=     1000022
      flst_real(   4,  69)=     1000022
      flst_real(   5,  69)=           1
      flst_real(   6,  69)=           4
 
      flst_real(   1,  70)=           4
      flst_real(   2,  70)=          -4
      flst_real(   3,  70)=     1000022
      flst_real(   4,  70)=     1000022
      flst_real(   5,  70)=           2
      flst_real(   6,  70)=          -2
 
      flst_real(   1,  71)=           4
      flst_real(   2,  71)=          -4
      flst_real(   3,  71)=     1000022
      flst_real(   4,  71)=     1000022
      flst_real(   5,  71)=           1
      flst_real(   6,  71)=          -1
 
      flst_real(   1,  72)=           4
      flst_real(   2,  72)=          -4
      flst_real(   3,  72)=     1000022
      flst_real(   4,  72)=     1000022
      flst_real(   5,  72)=           4
      flst_real(   6,  72)=          -4
 
      flst_real(   1,  73)=           4
      flst_real(   2,  73)=          -4
      flst_real(   3,  73)=     1000022
      flst_real(   4,  73)=     1000022
      flst_real(   5,  73)=           3
      flst_real(   6,  73)=          -3
 
      flst_real(   1,  74)=           4
      flst_real(   2,  74)=          -4
      flst_real(   3,  74)=     1000022
      flst_real(   4,  74)=     1000022
      flst_real(   5,  74)=           0
      flst_real(   6,  74)=           0
 
      flst_real(   1,  75)=           4
      flst_real(   2,  75)=           4
      flst_real(   3,  75)=     1000022
      flst_real(   4,  75)=     1000022
      flst_real(   5,  75)=           4
      flst_real(   6,  75)=           4
 
      flst_real(   1,  76)=           4
      flst_real(   2,  76)=          -3
      flst_real(   3,  76)=     1000022
      flst_real(   4,  76)=     1000022
      flst_real(   5,  76)=           4
      flst_real(   6,  76)=          -3
 
      flst_real(   1,  77)=           4
      flst_real(   2,  77)=           3
      flst_real(   3,  77)=     1000022
      flst_real(   4,  77)=     1000022
      flst_real(   5,  77)=           4
      flst_real(   6,  77)=           3
 
      flst_real(   1,  78)=           4
      flst_real(   2,  78)=           0
      flst_real(   3,  78)=     1000022
      flst_real(   4,  78)=     1000022
      flst_real(   5,  78)=           4
      flst_real(   6,  78)=           0
 
      flst_real(   1,  79)=          -3
      flst_real(   2,  79)=          -2
      flst_real(   3,  79)=     1000022
      flst_real(   4,  79)=     1000022
      flst_real(   5,  79)=          -2
      flst_real(   6,  79)=          -3
 
      flst_real(   1,  80)=          -3
      flst_real(   2,  80)=           2
      flst_real(   3,  80)=     1000022
      flst_real(   4,  80)=     1000022
      flst_real(   5,  80)=           2
      flst_real(   6,  80)=          -3
 
      flst_real(   1,  81)=          -3
      flst_real(   2,  81)=          -1
      flst_real(   3,  81)=     1000022
      flst_real(   4,  81)=     1000022
      flst_real(   5,  81)=          -1
      flst_real(   6,  81)=          -3
 
      flst_real(   1,  82)=          -3
      flst_real(   2,  82)=           1
      flst_real(   3,  82)=     1000022
      flst_real(   4,  82)=     1000022
      flst_real(   5,  82)=           1
      flst_real(   6,  82)=          -3
 
      flst_real(   1,  83)=          -3
      flst_real(   2,  83)=          -4
      flst_real(   3,  83)=     1000022
      flst_real(   4,  83)=     1000022
      flst_real(   5,  83)=          -4
      flst_real(   6,  83)=          -3
 
      flst_real(   1,  84)=          -3
      flst_real(   2,  84)=           4
      flst_real(   3,  84)=     1000022
      flst_real(   4,  84)=     1000022
      flst_real(   5,  84)=           4
      flst_real(   6,  84)=          -3
 
      flst_real(   1,  85)=          -3
      flst_real(   2,  85)=          -3
      flst_real(   3,  85)=     1000022
      flst_real(   4,  85)=     1000022
      flst_real(   5,  85)=          -3
      flst_real(   6,  85)=          -3
 
      flst_real(   1,  86)=          -3
      flst_real(   2,  86)=           3
      flst_real(   3,  86)=     1000022
      flst_real(   4,  86)=     1000022
      flst_real(   5,  86)=           2
      flst_real(   6,  86)=          -2
 
      flst_real(   1,  87)=          -3
      flst_real(   2,  87)=           3
      flst_real(   3,  87)=     1000022
      flst_real(   4,  87)=     1000022
      flst_real(   5,  87)=           1
      flst_real(   6,  87)=          -1
 
      flst_real(   1,  88)=          -3
      flst_real(   2,  88)=           3
      flst_real(   3,  88)=     1000022
      flst_real(   4,  88)=     1000022
      flst_real(   5,  88)=           4
      flst_real(   6,  88)=          -4
 
      flst_real(   1,  89)=          -3
      flst_real(   2,  89)=           3
      flst_real(   3,  89)=     1000022
      flst_real(   4,  89)=     1000022
      flst_real(   5,  89)=           3
      flst_real(   6,  89)=          -3
 
      flst_real(   1,  90)=          -3
      flst_real(   2,  90)=           3
      flst_real(   3,  90)=     1000022
      flst_real(   4,  90)=     1000022
      flst_real(   5,  90)=           0
      flst_real(   6,  90)=           0
 
      flst_real(   1,  91)=          -3
      flst_real(   2,  91)=           0
      flst_real(   3,  91)=     1000022
      flst_real(   4,  91)=     1000022
      flst_real(   5,  91)=          -3
      flst_real(   6,  91)=           0
 
      flst_real(   1,  92)=           3
      flst_real(   2,  92)=          -2
      flst_real(   3,  92)=     1000022
      flst_real(   4,  92)=     1000022
      flst_real(   5,  92)=          -2
      flst_real(   6,  92)=           3
 
      flst_real(   1,  93)=           3
      flst_real(   2,  93)=           2
      flst_real(   3,  93)=     1000022
      flst_real(   4,  93)=     1000022
      flst_real(   5,  93)=           2
      flst_real(   6,  93)=           3
 
      flst_real(   1,  94)=           3
      flst_real(   2,  94)=          -1
      flst_real(   3,  94)=     1000022
      flst_real(   4,  94)=     1000022
      flst_real(   5,  94)=          -1
      flst_real(   6,  94)=           3
 
      flst_real(   1,  95)=           3
      flst_real(   2,  95)=           1
      flst_real(   3,  95)=     1000022
      flst_real(   4,  95)=     1000022
      flst_real(   5,  95)=           1
      flst_real(   6,  95)=           3
 
      flst_real(   1,  96)=           3
      flst_real(   2,  96)=          -4
      flst_real(   3,  96)=     1000022
      flst_real(   4,  96)=     1000022
      flst_real(   5,  96)=          -4
      flst_real(   6,  96)=           3
 
      flst_real(   1,  97)=           3
      flst_real(   2,  97)=           4
      flst_real(   3,  97)=     1000022
      flst_real(   4,  97)=     1000022
      flst_real(   5,  97)=           4
      flst_real(   6,  97)=           3
 
      flst_real(   1,  98)=           3
      flst_real(   2,  98)=          -3
      flst_real(   3,  98)=     1000022
      flst_real(   4,  98)=     1000022
      flst_real(   5,  98)=           2
      flst_real(   6,  98)=          -2
 
      flst_real(   1,  99)=           3
      flst_real(   2,  99)=          -3
      flst_real(   3,  99)=     1000022
      flst_real(   4,  99)=     1000022
      flst_real(   5,  99)=           1
      flst_real(   6,  99)=          -1
 
      flst_real(   1, 100)=           3
      flst_real(   2, 100)=          -3
      flst_real(   3, 100)=     1000022
      flst_real(   4, 100)=     1000022
      flst_real(   5, 100)=           4
      flst_real(   6, 100)=          -4
 
      flst_real(   1, 101)=           3
      flst_real(   2, 101)=          -3
      flst_real(   3, 101)=     1000022
      flst_real(   4, 101)=     1000022
      flst_real(   5, 101)=           3
      flst_real(   6, 101)=          -3
 
      flst_real(   1, 102)=           3
      flst_real(   2, 102)=          -3
      flst_real(   3, 102)=     1000022
      flst_real(   4, 102)=     1000022
      flst_real(   5, 102)=           0
      flst_real(   6, 102)=           0
 
      flst_real(   1, 103)=           3
      flst_real(   2, 103)=           3
      flst_real(   3, 103)=     1000022
      flst_real(   4, 103)=     1000022
      flst_real(   5, 103)=           3
      flst_real(   6, 103)=           3
 
      flst_real(   1, 104)=           3
      flst_real(   2, 104)=           0
      flst_real(   3, 104)=     1000022
      flst_real(   4, 104)=     1000022
      flst_real(   5, 104)=           3
      flst_real(   6, 104)=           0
 
      flst_real(   1, 105)=           0
      flst_real(   2, 105)=          -2
      flst_real(   3, 105)=     1000022
      flst_real(   4, 105)=     1000022
      flst_real(   5, 105)=          -2
      flst_real(   6, 105)=           0
 
      flst_real(   1, 106)=           0
      flst_real(   2, 106)=           2
      flst_real(   3, 106)=     1000022
      flst_real(   4, 106)=     1000022
      flst_real(   5, 106)=           2
      flst_real(   6, 106)=           0
 
      flst_real(   1, 107)=           0
      flst_real(   2, 107)=          -1
      flst_real(   3, 107)=     1000022
      flst_real(   4, 107)=     1000022
      flst_real(   5, 107)=          -1
      flst_real(   6, 107)=           0
 
      flst_real(   1, 108)=           0
      flst_real(   2, 108)=           1
      flst_real(   3, 108)=     1000022
      flst_real(   4, 108)=     1000022
      flst_real(   5, 108)=           1
      flst_real(   6, 108)=           0
 
      flst_real(   1, 109)=           0
      flst_real(   2, 109)=          -4
      flst_real(   3, 109)=     1000022
      flst_real(   4, 109)=     1000022
      flst_real(   5, 109)=          -4
      flst_real(   6, 109)=           0
 
      flst_real(   1, 110)=           0
      flst_real(   2, 110)=           4
      flst_real(   3, 110)=     1000022
      flst_real(   4, 110)=     1000022
      flst_real(   5, 110)=           4
      flst_real(   6, 110)=           0
 
      flst_real(   1, 111)=           0
      flst_real(   2, 111)=          -3
      flst_real(   3, 111)=     1000022
      flst_real(   4, 111)=     1000022
      flst_real(   5, 111)=          -3
      flst_real(   6, 111)=           0
 
      flst_real(   1, 112)=           0
      flst_real(   2, 112)=           3
      flst_real(   3, 112)=     1000022
      flst_real(   4, 112)=     1000022
      flst_real(   5, 112)=           3
      flst_real(   6, 112)=           0
 
      flst_real(   1, 113)=           0
      flst_real(   2, 113)=           0
      flst_real(   3, 113)=     1000022
      flst_real(   4, 113)=     1000022
      flst_real(   5, 113)=           2
      flst_real(   6, 113)=          -2
 
      flst_real(   1, 114)=           0
      flst_real(   2, 114)=           0
      flst_real(   3, 114)=     1000022
      flst_real(   4, 114)=     1000022
      flst_real(   5, 114)=           1
      flst_real(   6, 114)=          -1
 
      flst_real(   1, 115)=           0
      flst_real(   2, 115)=           0
      flst_real(   3, 115)=     1000022
      flst_real(   4, 115)=     1000022
      flst_real(   5, 115)=           4
      flst_real(   6, 115)=          -4
 
      flst_real(   1, 116)=           0
      flst_real(   2, 116)=           0
      flst_real(   3, 116)=     1000022
      flst_real(   4, 116)=     1000022
      flst_real(   5, 116)=           3
      flst_real(   6, 116)=          -3
 
      flst_nreal=         116
        
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