c############### subroutine set_channel ################################
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
      subroutine set_channel(flav,ichan)
        implicit none
#include "nlegborn.h"
#include "PhysPars.h"
#include "osres.h"
        ! indices
        integer ichan,flav(nlegreal)

        ! TODO: set i,j,k,l,mi,mj,mk,ml,mij,mkl,sfeij,sfekl in dependence on ichan and flav
        osres_i = 3
        osres_j = 5
        osres_k = 4
        osres_l = 6
        osres_mi = 100D0
        osres_mj = 120D0
        osres_mk = 200D0
        osres_ml = 100D0
        osres_mij = 300D0
        osres_mkl = 300D0
        osres_sfeij = 1
        osres_sfekl = 1
        ! in case weakino masses are negative
        osres_mi = dabs(osres_mi)
        osres_mj = dabs(osres_mj)
        osres_mk = dabs(osres_mk)
        osres_ml = dabs(osres_ml)
        osres_mij = dabs(osres_mij)
        osres_mkl = dabs(osres_mkl)

        ! checks
        if(osres_i.eq.0 .or. osres_j.eq.0 .or. osres_k.eq.0 .or. osres_l.eq.0) then
          print*,"ichan ",ichan
          print*,"set_channel: got strang values for i,j,k,l:",osres_i,osres_j,osres_k,osres_l
          stop
        endif
        
#ifdef DEBUGQ
        print*,"ichan",ichan
        print*,"i,j,k,l",osres_i,osres_j,osres_k,osres_l
        print*,"mij,mkl,mi,mj,mk,ml",osres_mij,osres_mkl,osres_mi,osres_mj,osres_mk,osres_ml
        print*,"par_FinMasses",par_Fin1mass,par_Fin2mass
        !stop
#endif
      end
c############### end subroutine set_channel ############################
