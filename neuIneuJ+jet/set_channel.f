c############### subroutine set_channel ################################
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t
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
c              pi
c            /
c           /
c          /---- pj
c   ____  /    !
c  |    |/ sij = mij**2
c  | s  |      !
c  |____|\ skl = mkl**2
c         \
c          \---- pk
c           \
c            \
c              pl
c
c channel convention (this must be equal to the definitions made in the
c mathematica scripts):
c ichan =
c 1: left squark resonant with legs 3&5, and left squark resonant with legs 4&6
c 2: right squark resonant with legs 3&5, and left squark resonant with legs 4&6
c 3: left squark resonant with legs 3&5, and right squark resonant with legs 4&6
c 4: right squark resonant with legs 3&5, and right squark resonant with legs 4&6
c 5: left squark resonant with legs 3&6, and left squark resonant with legs 4&5
c 6: right squark resonant with legs 3&6, and left squark resonant with legs 4&5
c 7: left squark resonant with legs 3&6, and right squark resonant with legs 4&5
c 8: right squark resonant with legs 3&6, and right squark resonant with legs 4&5

      subroutine set_channel(flav,ichan)
        implicit none
#include "nlegborn.h"
#include "PhysPars.h"
#include "osres.h"
        ! indices
        integer ichan,flav(nlegreal)
        logical isodd, iseven
        external isodd, iseven

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
        else
          print*,"error in set_channel: ichan =",ichan
          stop
        endif
         
        osres_mi = par_Fin1mass
        osres_mk = par_Fin2mass
        select case(abs(flav(osres_j)))
          case(1)
            osres_mj = par_MD
            if(isodd(ichan)) then
              osres_sfeij = 1 ! left squark
              osres_mij = par_MSf(1,4,1)
              osres_wij = par_Sfwidth(1,4,1)
            else
              osres_sfeij = 2 ! right
              osres_mij = par_MSf(2,4,1)
              osres_wij = par_Sfwidth(2,4,1)
            endif    
          case(2)
            osres_mj = par_MU
            if(isodd(ichan)) then
              osres_sfeij = 1
              osres_mij = par_MSf(1,3,1)
              osres_wij = par_Sfwidth(1,3,1)
            else
              osres_sfeij = 2
              osres_mij = par_MSf(2,3,1)
              osres_wij = par_Sfwidth(2,3,1)
            endif
          case(3)
            osres_mj = par_MS
            if(isodd(ichan)) then
              osres_sfeij = 1
              osres_mij = par_MSf(1,4,2)
              osres_wij = par_Sfwidth(1,4,2)
            else
              osres_sfeij = 2
              osres_mij = par_MSf(2,4,2)
              osres_wij = par_Sfwidth(2,4,2)
            endif
          case(4)
            osres_mj = par_MC
            if(isodd(ichan)) then
              osres_sfeij = 1
              osres_mij = par_MSf(1,3,2)
              osres_wij = par_Sfwidth(1,3,2)
            else
              osres_sfeij = 2
              osres_mij = par_MSf(2,3,2)
              osres_wij = par_Sfwidth(2,3,2)
            endif
          case(5)
            osres_mj = par_MB
            if(isodd(ichan)) then
              osres_sfeij = 1
              osres_mij = par_MSf(1,4,3)
              osres_wij = par_Sfwidth(1,4,3)
            else
              osres_sfeij = 2
              osres_mij = par_MSf(2,4,3)
              osres_wij = par_Sfwidth(2,4,3)
            endif
          case default
            print*, "error in set_channel: flav(4)", flav(4)
            stop
        endselect
    
        select case(abs(flav(osres_l)))
          case(1)
            osres_ml = par_MD
            if(isodd(ichan)) then
              osres_sfekl = 1 ! left squark
              osres_mkl = par_MSf(1,4,1)
              osres_wkl = par_Sfwidth(1,4,1)
            else
              osres_sfekl = 2 ! right
              osres_mkl = par_MSf(2,4,1)
              osres_wkl = par_Sfwidth(2,4,1)
            endif    
          case(2)
            osres_ml = par_MU
            if(isodd(ichan)) then
              osres_sfekl = 1
              osres_mkl = par_MSf(1,3,1)
              osres_wkl = par_Sfwidth(1,3,1)
            else
              osres_sfekl = 2
              osres_mkl = par_MSf(2,3,1)
              osres_wkl = par_Sfwidth(2,3,1)
            endif
          case(3)
            osres_ml = par_MS
            if(isodd(ichan)) then
              osres_sfekl = 1
              osres_mkl = par_MSf(1,4,2)
              osres_wkl = par_Sfwidth(1,4,2)
            else
              osres_sfekl = 2
              osres_mkl = par_MSf(2,4,2)
              osres_wkl = par_Sfwidth(2,4,2)
            endif
          case(4)
            osres_ml = par_MC
            if(isodd(ichan)) then
              osres_sfekl = 1
              osres_mkl = par_MSf(1,3,2)
              osres_wkl = par_Sfwidth(1,3,2)
            else
              osres_sfekl = 2
              osres_mkl = par_MSf(2,3,2)
              osres_wkl = par_Sfwidth(2,3,2)
            endif
          case(5)
            osres_ml = par_MB
            if(isodd(ichan)) then
              osres_sfekl = 1
              osres_mkl = par_MSf(1,4,3)
              osres_wkl = par_Sfwidth(1,4,3)
            else
              osres_sfekl = 2
              osres_mkl = par_MSf(2,4,3)
              osres_wkl = par_Sfwidth(2,4,3)
            endif
          case default
            print*, "error in set_channel: flav(4)", flav(4)
            stop
        endselect

#ifdef DEBUG
            osres_mj = par_MD
            osres_sfeij = 2
            osres_mij = par_MSf(2,4,1)
            osres_wij = par_Sfwidth(2,4,1)
            osres_ml = par_MD
            osres_sfekl = 2
            osres_mkl = par_MSf(2,4,1)
            osres_wkl = par_Sfwidth(2,4,1)
#endif
            
        
        ! in case some masses are negative
        osres_mi = dabs(osres_mi)
        osres_mj = dabs(osres_mj)
        osres_mk = dabs(osres_mk)
        osres_ml = dabs(osres_ml)
        osres_mij = dabs(osres_mij)
        osres_mkl = dabs(osres_mkl)
        
#ifdef DEBUGQ
        print*,"ichan",ichan
        print*,"flav",flav
        print*,"i,j,k,l",osres_i,osres_j,osres_k,osres_l
        print*,"mi,mj,mk,ml",osres_mi,osres_mj,osres_mk,osres_ml
        print*,"mij,mkl",osres_mij,osres_mkl
        print*,"wij,wkl",osres_wij,osres_wkl
        print*,"par_FinMasses",par_Fin1mass,par_Fin2mass
        stop
#endif
      end
c############### end subroutine set_channel ############################
