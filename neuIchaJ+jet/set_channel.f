c############### subroutine set_channel ################################
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t
c Copyright (C) Matthias Kesenheimer - All Rights Reserved
c Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017
c
c par_MSf(chir,type,gen)
c chir = 1: left
c chir = 2: right
c type = 3: up-type
c type = 4: down-type
c gen = 1: u or d
c gen = 2: c or s
c gen = 3: t or b
c
c this subroutine returns all relevant informations of the on-shell 
c resonance.
c
c layout of double squark resonances
c
c              pi
c            /
c           /
c          /---- pj
c   ____  /   !
c  |    |/ sij=mij**2
c  | s  |     !
c  |____|\ skl=mkl**2
c         \
c          \---- pk
c           \
c            \
c              pl
c
c layout of single gluino resonances
c
c           pl
c          /
c   ____  /
c  |    |/ 
c  | s  |      !          pi 
c  |____|\ sijk=mijk**2  /
c     mijk\      pij    /
c          \------------
c           \           \
c            \           \
c             \           pj
c              pk
c
      subroutine set_channel(flav,ichan)
        implicit none
#include "nlegborn.h"
#include "PhysPars.h"
#include "osres.h"
        integer ichan,flav(nlegreal)

        ! set the indices
        if(ichan.ge.1 .and. ichan.le.4) then
          osres_i = 3
          osres_j = 5
          osres_k = 4
          osres_l = 6
        elseif(ichan.ge.5 .and. ichan.le.8) then
          osres_i = 3
          osres_j = 6
          osres_k = 4
          osres_l = 5
        elseif(ichan.eq.9 .or. ichan.eq.10) then
          osres_i = 3
          osres_j = 5
          osres_k = 6
          osres_l = 4
        elseif(ichan.eq.11 .or. ichan.eq.12) then
          osres_i = 3
          osres_j = 6
          osres_k = 5
          osres_l = 4
        elseif(ichan.eq.13 .or. ichan.eq.14) then
          osres_i = 4
          osres_j = 5
          osres_k = 6
          osres_l = 3
        elseif(ichan.eq.15 .or. ichan.eq.16) then
          osres_i = 4
          osres_j = 6
          osres_k = 5
          osres_l = 3
        else
          print*,"error in set_channel: ichan =",ichan
          stop
        endif
        
        ! set the chirality of the intermediate squark
        ! squark double resonances
        if(ichan.ge.1 .and. ichan.le.8) then
          if(mod(ichan-1,4).eq.0) then ! 1, 5
            osres_sfeij = 1
            osres_sfekl = 1
          elseif(mod(ichan-1,4).eq.1) then ! 2, 6
            osres_sfeij = 2
            osres_sfekl = 2
          elseif(mod(ichan-1,4).eq.2) then ! 3, 7
            osres_sfeij = 1
            osres_sfekl = 2
          elseif(mod(ichan-1,4).eq.3) then ! 4, 8
            osres_sfeij = 2
            osres_sfekl = 1
          else
            print*,"error in set_channel: ichan =",ichan
            stop
          endif
        ! gluino single resonances  
        else if(ichan.ge.9 .and. ichan.le.16) then
          if(mod(ichan-1,2).eq.0) then ! 9, 11, 13, 15, 17
            osres_sfeij = 1
          elseif(mod(ichan-1,2).eq.1) then ! 10, 12, 14, 16, 18
            osres_sfeij = 2
          else
            print*,"error in set_channel: ichan =",ichan
            stop
          endif
        endif
        
        ! set the masses, squark double resonances
        if(ichan.ge.1 .and. ichan.le.8) then
          osres_mi = par_Fin1mass
          osres_mk = par_Fin2mass
          osres_mijk = 0D0
          osres_wijk = 0D0
          select case(abs(flav(osres_j)))
            case(1)
              osres_mj = par_MD
              osres_mij = par_MSf(osres_sfeij,4,1)
              osres_wij = par_WSf(osres_sfeij,4,1)  
            case(2)
              osres_mj = par_MU
              osres_mij = par_MSf(osres_sfeij,3,1)
              osres_wij = par_WSf(osres_sfeij,3,1)
            case(3)
              osres_mj = par_MS
              osres_mij = par_MSf(osres_sfeij,4,2)
              osres_wij = par_WSf(osres_sfeij,4,2)
            case(4)
              osres_mj = par_MC
              osres_mij = par_MSf(osres_sfeij,3,2)
              osres_wij = par_WSf(osres_sfeij,3,2)
            case(5)
              osres_mj = par_MB
              osres_mij = par_MSf(osres_sfeij,4,3)
              osres_wij = par_WSf(osres_sfeij,4,3)
            case default
              print*, "error in set_channel: flav(j)", flav(osres_j)
              stop
          endselect
          select case(abs(flav(osres_l)))
            case(1)
              osres_ml = par_MD
              osres_mkl = par_MSf(osres_sfekl,3,1) ! flavor change
              osres_wkl = par_WSf(osres_sfekl,3,1)
            case(2)
              osres_ml = par_MU
              osres_mkl = par_MSf(osres_sfekl,4,1) ! flavor change
              osres_wkl = par_WSf(osres_sfekl,4,1)
            case(3)
              osres_ml = par_MS
              osres_mkl = par_MSf(osres_sfekl,3,2) ! flavor change
              osres_wkl = par_WSf(osres_sfekl,3,2)
            case(4)
              osres_ml = par_MC
              osres_mkl = par_MSf(osres_sfekl,4,2) ! flavor change
              osres_wkl = par_WSf(osres_sfekl,4,2)
            case(5)
              osres_ml = par_MB
              osres_mkl = par_MSf(osres_sfekl,3,3) ! flavor change
              osres_wkl = par_WSf(osres_sfekl,3,3)
            case default
              print*, "error in set_channel: flav(l)", flav(osres_l)
              stop
          endselect
        
        ! gluino single resonances
        elseif(ichan.ge.9 .and. ichan.le.16) then
          osres_mijk = par_MGl
          osres_wijk = par_WGl
          osres_mkl = 0D0
          osres_wkl = 0D0
          if(ichan.ge.9 .and. ichan.le.12) then
            osres_mi = par_Fin1mass
            osres_ml = par_Fin2mass
            select case(abs(flav(osres_j)))
              case(1)
                osres_mj = par_MD
                osres_mij = par_MSf(osres_sfeij,4,1)
                osres_wij = par_WSf(osres_sfeij,4,1)  
              case(2)
                osres_mj = par_MU
                osres_mij = par_MSf(osres_sfeij,3,1)
                osres_wij = par_WSf(osres_sfeij,3,1)
              case(3)
                osres_mj = par_MS
                osres_mij = par_MSf(osres_sfeij,4,2)
                osres_wij = par_WSf(osres_sfeij,4,2)
              case(4)
                osres_mj = par_MC
                osres_mij = par_MSf(osres_sfeij,3,2)
                osres_wij = par_WSf(osres_sfeij,3,2)
              case(5)
                osres_mj = par_MB
                osres_mij = par_MSf(osres_sfeij,4,3)
                osres_wij = par_WSf(osres_sfeij,4,3)
              case default
                print*, "error in set_channel: flav(j)", flav(osres_j)
                stop
            endselect
          elseif(ichan.ge.13 .and. ichan.le.16) then
            osres_mi = par_Fin2mass
            osres_ml = par_Fin1mass
            select case(abs(flav(osres_j)))
              case(1)
                osres_mj = par_MD
                osres_mij = par_MSf(osres_sfeij,3,1) ! flavor change
                osres_wij = par_WSf(osres_sfeij,3,1)  
              case(2)
                osres_mj = par_MU
                osres_mij = par_MSf(osres_sfeij,4,1) ! flavor change
                osres_wij = par_WSf(osres_sfeij,4,1)
              case(3)
                osres_mj = par_MS
                osres_mij = par_MSf(osres_sfeij,3,2) ! flavor change
                osres_wij = par_WSf(osres_sfeij,3,2)
              case(4)
                osres_mj = par_MC
                osres_mij = par_MSf(osres_sfeij,4,2) ! flavor change
                osres_wij = par_WSf(osres_sfeij,4,2)
              case(5)
                osres_mj = par_MB
                osres_mij = par_MSf(osres_sfeij,3,3) ! flavor change
                osres_wij = par_WSf(osres_sfeij,3,3)
              case default
                print*, "error in set_channel: flav(j)", flav(osres_j)
                stop
            endselect
          endif
          select case(abs(flav(osres_k)))
            case(1)
              osres_mk = par_MD
            case(2)
              osres_mk = par_MU
            case(3)
              osres_mk = par_MS
            case(4)
              osres_mk = par_MC
            case(5)
              osres_mk = par_MB
            case default
              print*, "error in set_channel: flav(k)", flav(osres_k)
              stop
          endselect
        endif

        ! in case some masses are negative
        osres_mi = dabs(osres_mi)
        osres_mj = dabs(osres_mj)
        osres_mk = dabs(osres_mk)
        osres_ml = dabs(osres_ml)
        osres_mijk = dabs(osres_mijk)
        osres_mij  = dabs(osres_mij)
        osres_mkl  = dabs(osres_mkl)
      end
c############### end subroutine set_channel ############################
