c Add large logarithms to AlphaS to decouple hevay particles from AlphaS running
c Copyright (C) Matthias Kesenheimer - All Rights Reserved
c Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2018

      subroutine add_logs(dZgs)
        implicit none
#include "pwhg_st.h"
#include "PhysPars.h"
        double complex dZgs
        double precision MUR2
        integer i,j,k
        ! constants
        double precision pi
        parameter (pi = 4.D0*datan(1.D0))

        MUR2 = st_muren2

        ! MK: decoupling of heavy particles, see hep-ph/0509048
        ! Quarks
        if(st_nlight .eq. 5) then
          dZgs = dZgs - Alfas/(8.D0*PI) * (
     &            + 2/3.D0*dlog(MT2/MUR2) )
        elseif(st_nlight .eq. 4) then
          dZgs = dZgs - Alfas/(8.D0*PI) * (
     &            + 2/3.D0*dlog(MT2/MUR2)
     &            + 2/3.D0*dlog(MB2/MUR2) )
        elseif(st_nlight .eq. 3) then
          dZgs = dZgs - Alfas/(8.D0*PI) * (
     &            + 2/3.D0*dlog(MT2/MUR2)
     &            + 2/3.D0*dlog(MB2/MUR2)
     &            + 2/3.D0*dlog(MC2/MUR2) )
        elseif(st_nlight .eq. 2) then
          dZgs = dZgs - Alfas/(8.D0*PI) * (
     &            + 2/3.D0*dlog(MT2/MUR2) 
     &            + 2/3.D0*dlog(MB2/MUR2) 
     &            + 2/3.D0*dlog(MC2/MUR2) 
     &            + 2/3.D0*dlog(MS2/MUR2) )
        elseif(st_nlight .eq. 1) then
          dZgs = dZgs - Alfas/(8.D0*PI) * (
     &            + 2/3.D0*dlog(MT2/MUR2) 
     &            + 2/3.D0*dlog(MB2/MUR2) 
     &            + 2/3.D0*dlog(MC2/MUR2) 
     &            + 2/3.D0*dlog(MS2/MUR2) 
     &            + 2/3.D0*dlog(MD2/MUR2) )
        else
          print*,"st_nlight = ", st_nlight, " not supported."
          stop
        endif

        ! Gluinos
        dZgs = dZgs - Alfas/(8.D0*PI) * 2D0*dlog(MGL2/MUR2)

        ! Squarks
        do i=1,2 ! sfermion index s
          do j=3,4 ! type (nu,e,u,d)
            do k=1,3 ! generation
              dZgs = dZgs - Alfas/(8.D0*PI) * 1/6.D0*dlog(MSf2(i,j,k)/MUR2)
            enddo
          enddo
        enddo

      end
