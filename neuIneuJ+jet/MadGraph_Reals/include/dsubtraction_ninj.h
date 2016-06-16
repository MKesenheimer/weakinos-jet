c############### dsubtraction.h ########################################
c Variables to apply the diagram subtractions scheme

#ifndef DSUBTRACTION_H
#define DSUBTRACTION_H

      character*4 CHAN                       ! distuinguish between u-, t-channels, left and right
      double complex AMPR(NGRAPHS)           ! NGRAPHS was defined before in the madgraph .f files
      double complex JAMPR(NCOLOR)           ! sum of resonant diagrams, NCOLOR was defined before
      double complex JAMPHR(-1:1,NCOLOR)     ! sum of resonant diagrams for a certain helicity
      double precision MATRIX_RES            ! sum of resonant diagrams squared
      double complex CMATRIX_RES             ! complex sum of resonant diagrams squared (for helicity sums)
      double precision BORNTILDE_RES         ! ?
      double precision ampr2(maxamps)        ! save the resonant amplitudes for later
      double precision jampr2(0:maxamps)     ! maxamps was defined before
      
#if defined(DSUB_I)
      double precision RATIO35L              ! the ratio of the Breit-Wigner functions if the left handed u-channel diagrams get resonant
      double precision RATIO35R              ! "-" right handed u-channel diagrams get resonant
      double precision RATIO45L              ! "-" left handed t-channel diagrams get resonant
      double precision RATIO45R              ! "-" right handed t-channel diagrams get resonant
      double precision THETA35L              ! the theta functions
      double precision THETA35R
      double precision THETA45L
      double precision THETA45R
      double precision COUNTER35L            ! counter term for left handed diagram with resonance at s35 = msq^2
      double precision COUNTER35R            ! counter term for right handed diagram with resonance at s35 = msq^2
      double precision COUNTER45L            ! counter term for left handed diagram with resonance at s45 = msq^2
      double precision COUNTER45R            ! counter term for right handed diagram with resonance at s45 = msq^2
      double precision P_OS(0:3,NEXTERNAL)   ! remapped momenta to on-shell kinematic
      double precision S,S35,S45             ! invariants
      double precision  momsum2sq            ! external function to calculate (pi+pj)^2
      external momsum2sq
#endif

#if defined(DSUB_I) || defined(DSUB_II) || defined(DSUB_II_TEST)
#ifndef _EXTFUNCT_               
      double precision B_CL_002_001_RES      ! gives back matrix element squared
      external B_CL_002_001_RES              ! from resonant diagrams in
      double complex B_CL_002_002_RES        ! dependence on CHAN:
      external B_CL_002_002_RES              ! u channel diagrams (s35 = msq^2), CHAN=0: left, CHAN=1: right
      double precision B_CL_002_003_RES      ! t channel diagrams (s45 = msq^2), CHAN=2: left, CHAN=3: right
      external B_CL_002_003_RES

      double precision B_CL_004_001_RES
      external B_CL_004_001_RES
      double complex B_CL_004_002_RES
      external B_CL_004_002_RES
      double precision B_CL_004_003_RES
      external B_CL_004_003_RES

      double precision B_CL_006_001_RES
      external B_CL_006_001_RES
      double complex B_CL_006_002_RES
      external B_CL_006_002_RES
      double precision B_CL_006_003_RES
      external B_CL_006_003_RES

      double precision B_CL_008_001_RES
      external B_CL_008_001_RES
      double complex B_CL_008_002_RES
      external B_CL_008_002_RES
      double precision B_CL_008_003_RES
      external B_CL_008_003_RES

      double complex B_CL_021_001_RES
      external B_CL_021_001_RES
      double precision B_CL_021_002_RES
      external B_CL_021_002_RES
      double precision B_CL_021_003_RES
      external B_CL_021_003_RES

      double complex B_CL_022_001_RES
      external B_CL_022_001_RES
      double precision B_CL_022_002_RES
      external B_CL_022_002_RES
      double precision B_CL_022_003_RES
      external B_CL_022_003_RES

      double complex B_CL_023_001_RES
      external B_CL_023_001_RES
      double precision B_CL_023_002_RES
      external B_CL_023_002_RES
      double precision B_CL_023_003_RES
      external B_CL_023_003_RES

      double complex B_CL_024_001_RES
      external B_CL_024_001_RES
      double precision B_CL_024_002_RES
      external B_CL_024_002_RES
      double precision B_CL_024_003_RES
      external B_CL_024_003_RES
#endif
#endif


c WREG is now defined in osres.h
#include "osres.h"

#endif

c############### end dsubtraction.h ####################################
