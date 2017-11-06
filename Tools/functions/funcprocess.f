c############### funcprocess.f #########################################
c last modified by MK, 19.12.2014
c adapted from dislepton
c various shared helpers
c 2012-07 AvM

c############### decode_pair subroutine ###############################
c decode two PDG ids for MSSM fermions from a single integer
      subroutine decode_pair(combination,ida,idb)
        implicit none
        integer combination,ida,idb,idaa,idbb
        idaa = mod(abs(combination),1000)
        idbb = abs(combination)/1000
        ida =  (idaa/100)*1000000 + mod(idaa,100)
        if( combination .gt. 0D0) then
          idb =  (idbb/100)*1000000 + mod(idbb,100)
        else if (combination .lt. 0D0) then
          idb =  -(idbb/100)*1000000 - mod(idbb,100)
        else 
          print*,"error in decode_pair."
          stop
        endif  
      end
c############### end decode_pair subroutine ############################

c############### encode_pair subroutine ################################
c encode two PDG ids for MSSM fermions into a single integer
c (encodes 1000022,1000022 to 122122 or 1000024,-1000024 to -124124 etc.)
      integer function encode_pair(ida,idb)
        implicit none
        integer ida,idb,idaa,idbb,combination,idar,idbr
        idaa = (ida / 1000000)*100 + mod(ida,100)
        idbb = (idb / 1000000)*100 + mod(idb,100)
        combination = abs(idaa) + 1000*abs(idbb)
        ! encode unequal charge of particles
        if( (idaa .lt. 0D0) .or. (idbb .lt. 0D0) ) then
          combination = -combination
        endif
        ! check if reconstruction works, then we are fine in any case
        call decode_pair(combination,idar,idbr)
        if ((ida.ne.idar).or.(idb.ne.idbr)) then
          print*,"invalid particle ID"
          print*,"combination",combination
          print*,"ida,idaa,idar",ida,idaa,idar
          print*,"idb,idbb,idbr",idb,idbb,idbr
          stop  
        endif
        encode_pair = combination
      end
c############### end encode_pair subroutine ############################

c############### check_4conservation subroutine ########################
c check if 4 momentum conservation is fulfilled. If yes set lresult
c true, if not false.
c verbose = 0: Throw only once a warning with less output (could be dangerous)
c verbose = 1: Throw always a warning with less output
c verbose = 2: Throw always a warning with more output
c verbose = 3: hard check. Throw error and show all output
c verbose = 4: No output at all, but set the variable lresult
      subroutine check_4conservation(p,nleg,verbose,lresult)
        implicit none
        integer nleg,i,j
        double precision p(0:3,nleg) ! momentum vectors
        double precision pi(0:3) ! sum of incoming momenta
        double precision pf(0:3) ! sum of outgoing momenta
        double precision momsq, momsum2sq, momsum3sq, dotp
        external momsq, momsum2sq, momsum3sq, dotp
        double precision eps, rel(0:3) ! rel. err.
        double precision xi ! regulator for comparing small values
        parameter (eps=1D-4, xi=1D2)
        logical lresult
        integer verbose
        ! output control
        integer warncount1, warncount2
        data warncount1/0/, warncount2/0/

        ! reset lresult
        lresult = .true.

        ! first check if NaNs occured
        do i=1,nleg
          do j=0,3
            if( isnan(p(j,i)) ) then
              if(verbose.ge.2.and.verbose.lt.4) then
                if(warncount1.lt.10) then
                  warncount1 = warncount1 + 1
                  print*,"warning: Nan occured"
                  print*,"p1 = ", p(:,1)
                  print*,"p2 = ", p(:,2)
                  print*,"p3 = ", p(:,3)
                  print*,"p4 = ", p(:,4)
                  if(nleg.eq.5 .or. nleg.eq.6) print*,"p5 = ", p(:,5)
                  if(nleg.eq.6) print*,"p6 = ", p(:,6)
                elseif(warncount1.eq.10) then
                  warncount1 = 11
                  print*, "check_4conservation: Further output "//
     &                    "will be suppressed."
                endif
              endif
              p(j,i) = 0D0 ! overwrite NaN with 0
            endif  
          enddo  
        enddo

        pi(:) = 0d0
        pf(:) = 0d0
        do i=1,2
          do j=0,3
            pi(j) = pi(j) + p(j,i)
          enddo  
        enddo
        do i=3,nleg
          do j=0,3
            pf(j) = pf(j) + p(j,i)
          enddo  
        enddo
        
        if(.not. (verbose.le.4 .and. verbose.ge.0) ) then
          print*, "Error: not supported verbosity level ", verbose
          stop
        endif
        
        ! calculate the relative error
        ! we have to introduce a regulator xi here if we want to
        ! compare a value with the true value zero.
        ! In this case the rel. error would be rel = 2*(0-10^-5)/(0+10^-5) = 2
        ! With the regulator xi = 1: rel = 2*(0-10^-5)/(0+10^-5+xi) = 10^-5
        ! The regulator defines a range where comparing small values should give small
        ! relative errors.
        do i=0,3
          rel(i) = 2D0*dabs(pi(i)-pf(i))/dabs(pi(i)+pf(i)+xi)
        enddo

        if(rel(0) .gt. eps .or.
     &     rel(1) .gt. eps .or.
     &     rel(2) .gt. eps .or.
     &     rel(3) .gt. eps)  then
          if(warncount2.lt.10) then
            warncount2 = warncount2 + 1
            if(verbose.eq.0) warncount2 = 10 ! show the output only once (could be dangerous)
            if(verbose.eq.3) then
              print*, "Error: four momentum not conserved."
            else
              print*, "Warning: four momentum not conserved."
            endif
            print*, "Sum p in  = ", pi(:)
            print*, "Sum p out = ", pf(:)
            print*, "rel. err. = ",rel(:)
            if(verbose.eq.3) then
              print*,"p1, m1 = ", p(:,1), dsqrt(dabs(dotp(p(:,1),p(:,1))))
              print*,"p2, m2 = ", p(:,2), dsqrt(dabs(dotp(p(:,2),p(:,2))))
              print*,"p3, m3 = ", p(:,3), dsqrt(dabs(dotp(p(:,3),p(:,3))))
              print*,"p4, m4 = ", p(:,4), dsqrt(dabs(dotp(p(:,4),p(:,4))))
              if(nleg.eq.5 .or. nleg.eq.6) then
                print*,"p5, m5 = ", p(:,5), dsqrt(dabs(dotp(p(:,5),p(:,5))))
              endif  
              if(nleg.eq.6) then
                print*,"p6, m6 = ", p(:,6), dsqrt(dabs(dotp(p(:,6),p(:,6))))
              endif  
            endif
            if(verbose.eq.3) stop
          elseif(warncount2.eq.10) then
            warncount2 = 11
            print*,"check_4conservation: Further output will"//
     &             " be suppressed."
          endif
          lresult = .false.
        endif
      end
c############### end check_4conservation subroutine ####################

c############### subroutine set_process ################################
c pick SUSY masses relevant for specific initial state
      subroutine set_process(nlegs, flav, M)
        implicit none
#include "PhysPars.h"
#include "nlegborn.h"
#include "indices.h"
        integer nlegs
        integer flav(nlegs), i
        double precision M(nlegs)

        Gen(:) = 0
        Neu(:) = 0
        Cha(:) = 0
        
        do i=1,nlegs
          select case(abs(flav(i)))
          case(1) ! d
            M(i) = par_MD
            Gen(i) = 1
          case(2) ! u
            M(i) = par_MU
            Gen(i) = 1
          case(3) ! s
            M(i) = par_MS
            Gen(i) = 2
          case(4) ! c
            M(i) = par_MC
            Gen(i) = 2
          case(5) ! b
            M(i) = par_MB
            Gen(i) = 3
          case(6) ! t
            M(i) = par_MT
            Gen(i) = 3
          case(0) ! gluon
            M(i) = 0D0
          case(1000022)
            M(i) = MNeu(1)
            Neu(i) = 1
          case(1000023)
            M(i) = MNeu(2)
            Neu(i) = 2
          case(1000025)
            M(i) = MNeu(3)
            Neu(i) = 3
          case (1000035)
            M(i) = MNeu(4)
            Neu(i) = 4
          case(1000024)
            M(i) = MCha(1)
            Cha(i) = 1
          case(1000037)
            M(i) = MCha(2)
            Cha(i) = 2
          case default
            print*, "funcprocess.f: encountered unhandled "//
     &              "incoming ID ", flav(i)
            stop
          endselect
        enddo
      end
c############### end subroutine set_process ############################

c############### subroutine process_name ###############################
c convert the powheg flavor string to an character string
      subroutine process_name(nlegs,flav,str,lstr)
        implicit none
        integer nlegs, lstr, i
        integer flav(nlegs)
        character*(*) str
        
        lstr = 0
        do i=1,nlegs
        if(i.eq.3) then
          str(lstr+1:lstr+1) = "_"
          lstr = lstr + 1
        endif
        select case(flav(i))
          case(0)
            str(lstr+1:lstr+1) = "g"
            lstr = lstr + 1
          case(-1)
            str(lstr+1:lstr+4) = "dbar"
            lstr = lstr + 4
          case(1)
            str(lstr+1:lstr+1) = "d"
            lstr = lstr + 1
          case(-2)
            str(lstr+1:lstr+4) = "ubar"
            lstr = lstr + 4
          case(2)
            str(lstr+1:lstr+1) = "u"
            lstr = lstr + 1
          case(-3)
            str(lstr+1:lstr+4) = "sbar"
            lstr = lstr + 4
          case(3)
            str(lstr+1:lstr+1) = "s"
            lstr = lstr + 1
          case(-4)
            str(lstr+1:lstr+4) = "cbar"
            lstr = lstr + 4
          case(4)
            str(lstr+1:lstr+1) = "c"
            lstr = lstr + 1
          case(-5)
            str(lstr+1:lstr+4) = "bbar"
            lstr = lstr + 4
          case(5)
            str(lstr+1:lstr+1) = "b"
            lstr = lstr + 1
          case(-6)
            str(lstr+1:lstr+4) = "tbar"
            lstr = lstr + 4
          case(6)
            str(lstr+1:lstr+1) = "t"
            lstr = lstr + 1
          case(1000022)
            str(lstr+1:lstr+2) = "n1"
            lstr = lstr + 2
          case(1000023)
            str(lstr+1:lstr+2) = "n2"
            lstr = lstr + 2
          case(1000025)
            str(lstr+1:lstr+2) = "n3"
            lstr = lstr + 2
          case(1000035)
            str(lstr+1:lstr+2) = "n4"
            lstr = lstr + 2
          case(1000024)
            str(lstr+1:lstr+3) = "x1+"
            lstr = lstr + 3
          case(-1000024)
            str(lstr+1:lstr+3) = "x1-"
            lstr = lstr + 3
          case(1000037)
            str(lstr+1:lstr+3) = "x2+"
            lstr = lstr + 3
          case(-1000037)
            str(lstr+1:lstr+3) = "x2-"
            lstr = lstr + 3
          case default
            str(lstr+1:lstr+1) = "_"
            lstr = lstr + 1
        endselect
        enddo
      end
c############### end subroutine process_name ###########################
