c############### dsubtraction.h ########################################
c Variables to apply the diagram subtractions scheme
#ifndef DSUBTRACTION_H
#define DSUBTRACTION_H

      character*4 CHAN                       ! distuinguish between u-, t-channels, left and right
      double complex AMPR(NGRAPHS)           ! NGRAPHS was defined before in the madgraph .f files
      double precision MATRIX_RES            ! sum of resonant diagrams squared
      
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

#endif

c WREG is now defined in osres.h
#include "osres.h"

#endif
c############### end dsubtraction.h ####################################
