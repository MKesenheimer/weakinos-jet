c############### model_mssm.h ##########################################
* model_mssm.h
* common blocks for the model parameters
* this file is part of FormCalc
* last modified 17 Dec 14 th by MK

c#include "types.h"

        double complex UCha(2,2), VCha(2,2), ZNeu(4,4)
        double precision MNeu(4), MNeu2(4), MCha(2), MCha2(2)
        double complex USf(2,2,4,3)
        double precision SB, SB2, CB, CB2, TB, TB2, C2B, S2B
        double precision SA, SA2, CA, CA2, TA, TA2, C2A, S2A
        double precision CAB, SAB, SBA, SBA2, CBA, CBA2
        
        double precision MSf(2,4,3)
        double precision MSf2(2,4,3)
        double precision MGl, MGl2
        double precision WSf(2,4,3), WGl
        double complex Mino3
        double complex MUE
        double complex Af(2:4,3,3)
        double precision Mh0, MHH, MA0, MHp
        double precision Mh02, MHH2, MA02, MHp2

#ifndef USfC
#define USfC(i,j,t,g) Conjugate(USf(i,j,t,g))
#define VChaC(i,j) Conjugate(VCha(i,j))
#define UChaC(i,j) Conjugate(UCha(i,j))
#define ZNeuC(i,j) Conjugate(ZNeu(i,j))
#define Mino3C Conjugate(Mino3)
#define MUEC Conjugate(MUE)
#define AfC(t,g1,g2) Conjugate(Af(t,g1,g2))
#endif

        common /mssmpara/ UCha, VCha, ZNeu           ! Chargino & Neutralino mixing matrices
        common /mssmpara/ MNeu, MNeu2                ! Neutralino masses
        common /mssmpara/ MCha, MCha2                ! Chargino masses   
        common /mssmpara/ USf                        ! Sfermion mixing matrix
        common /mssmpara/ SB, SB2, CB, CB2, TB, TB2  ! sin(beta), cos(beta), tan(beta)
        common /mssmpara/ C2B, S2B                   ! shorts
        common /mssmpara/ MSf, MSf2, MGl, MGl2       ! Sfermion & Gluino masses
        common /mssmpara/ WSf, WGl                   ! Sfermion & Gluino widths
        common /mssmpara/ Mino3                      ! Gaugino mass parameter
        common /mssmpara/ MUE                        ! mass term mixing the 2 Higgs doublets
        common /mssmpara/ Af                         ! trilinear coupling
        common /mssmpara/ SA, SA2, CA, CA2, TA       ! Higgs parameters 
        common /mssmpara/ TA2, C2A, S2A
        common /mssmpara/ CAB, SAB, SBA, SBA2, CBA, CBA2
        common /mssmpara/ Mh0, MHH, MA0, MHp
        common /mssmpara/ Mh02, MHH2, MA02, MHp2

c############### end model_mssm.h ######################################
