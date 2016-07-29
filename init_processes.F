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
        flst_nborn = 1
        flst_born(1,1) = -2
        flst_born(2,1) = 0
        flst_born(3,1) = 1000022
        flst_born(4,1) = 1000022
        flst_born(5,1) = -2

        flst_nreal = 1
        flst_real(1,1) = -1
        flst_real(2,1) =  0
        flst_real(3,1) = 1000022
        flst_real(4,1) = 1000022
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
        !stop
#endif

        ! setup the flavor list of on-shell resonant diagrams
        call init_processes_osres
#ifdef DEBUG
        print*,' on-shell resonant real processes',flst_nosres
        do j=1,flst_nosres
          print*,(flst_osres(k,j),k=1,nlegreal)
        enddo
#endif

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
c dl35: left handed up-type squark gets resonant with particles 3 & 5
c dl45: left handed up-type squark gets resonant with particles 4 & 5
c ul35: left handed down-type squark gets resonant with particles 3 & 5
c ul45: left handed down-type squark gets resonant with particles 4 & 5
c (only required for neutralino pair production:)
c dr35: right handed up-type squark gets resonant with particles 3 & 5
c dr45: right handed up-type squark gets resonant with particles 4 & 5
c ur35: right handed down-type squark gets resonant with particles 3 & 5
c ur45: right handed down-type squark gets resonant with particles 4 & 5
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
          if(f(5).ne.0 .and. f(6).ne.0) then
            flst_nosres=flst_nosres+1 
            do i2=1,nlegreal
              flst_osres(i2,flst_nosres) = flst_real(i2,i1) ! copy existing real
            enddo
          endif
        enddo

        ! fill the global variables for treating the on-shell resonances
#if defined(NIXJ_JET) || defined(XIXJ_JET)
        ! number of resonances
        nosres = 8 ! change cnosres in osres.h as well
#elif defined(NINJ_JET)
        nosres = 16
#else
        print*,"error: preprocessor flag NIXJ_JET, NINJ_JET or "//
     &         "XIXJ_JET not set."
#endif
        
        if(nosres.ne.cnosres) then
          print*,"error in init_processes.F: nosres and narraylngth not"
          print*,"equal: ", nosres, cnosres
          print*,"change nosres in init_processes or"//
     &           "cnosres in osres.h"
          stop
        endif

        ! the human readable identifiers (TODO)
        osresID(1) = "dl35"
        osresID(2) = "dl45"
        osresID(3) = "ul35"
        osresID(4) = "ul45"
        osresID(5) = "dl36"
        osresID(6) = "dl46"
        osresID(7) = "ul36"
        osresID(8) = "ul46"
#ifdef NINJ_JET
        osresID(9)  = "dr35"
        osresID(10) = "dr45"
        osresID(11) = "ur35"
        osresID(12) = "ur45"
        osresID(13) = "dr36"
        osresID(14) = "dr46"
        osresID(15) = "ur36"
        osresID(16) = "ur46"
#endif

        do k=1,nosres
          if(len(osresID(k)).gt.4) then
            print*,"error in init_processes.F: array osresID not set"
            print*,"correctly. Increase field length."
            stop
          endif
        enddo

        ! save which final state particles get resonant in an 
        ! additional array. This will be helpful to generate the 
        ! on-shell real phase space and to calculate the matrix elements
        ! in subroutine setosresreal
        osreslegs(1,1) = 3
        osreslegs(1,2) = 5
        osreslegs(1,3) = 4
        osreslegs(1,4) = 6
        
        osreslegs(2,1) = 4
        osreslegs(2,2) = 5
        osreslegs(2,3) = 3
        osreslegs(2,4) = 6
        
        osreslegs(3,1) = 3
        osreslegs(3,2) = 5
        osreslegs(3,3) = 4
        osreslegs(3,4) = 6
        
        osreslegs(4,1) = 4
        osreslegs(4,2) = 5
        osreslegs(4,3) = 3
        osreslegs(4,4) = 6


        osreslegs(5,1) = 3
        osreslegs(5,2) = 6
        osreslegs(5,3) = 4
        osreslegs(5,4) = 5
        
        osreslegs(6,1) = 4
        osreslegs(6,2) = 6
        osreslegs(6,3) = 3
        osreslegs(6,4) = 5
        
        osreslegs(7,1) = 3
        osreslegs(7,2) = 6
        osreslegs(7,3) = 4
        osreslegs(7,4) = 5
        
        osreslegs(8,1) = 4
        osreslegs(8,2) = 6
        osreslegs(8,3) = 3
        osreslegs(8,4) = 5

#ifdef NINJ_JET
        osreslegs(9,1) = 3
        osreslegs(9,2) = 5
        osreslegs(9,3) = 4
        osreslegs(9,4) = 6
        
        osreslegs(10,1) = 4
        osreslegs(10,2) = 5
        osreslegs(10,3) = 3
        osreslegs(10,4) = 6
        
        osreslegs(11,1) = 3
        osreslegs(11,2) = 5
        osreslegs(11,3) = 4
        osreslegs(11,4) = 6
        
        osreslegs(12,1) = 4
        osreslegs(12,2) = 5
        osreslegs(12,3) = 3
        osreslegs(12,4) = 6


        osreslegs(13,1) = 3
        osreslegs(13,2) = 6
        osreslegs(13,3) = 4
        osreslegs(13,4) = 5
        
        osreslegs(14,1) = 4
        osreslegs(14,2) = 6
        osreslegs(14,3) = 3
        osreslegs(14,4) = 5
        
        osreslegs(15,1) = 3
        osreslegs(15,2) = 6
        osreslegs(15,3) = 4
        osreslegs(15,4) = 5
        
        osreslegs(16,1) = 4
        osreslegs(16,2) = 6
        osreslegs(16,3) = 3
        osreslegs(16,4) = 5
#endif

        ! the rest of the variables defined in osres.h are set
        ! in init_couplings.f
      end
c############### end subroutine init_osres #############################
