c############### model_mssm.h ##########################################
* model_mssm.h
* common blocks for the model parameters
* this file is part of FormCalc
* last modified 17 Dec 14 th by MK
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

        ! complex parameters for loop integrals
        double complex USfC(2,2,4,3), VChaC(2,2), UChaC(2,2)
        double complex ZNeuC(4,4)
        double complex Mino3C, MUEC, AfC(2:4,3,3)
        double complex MNeuC(4), MNeu2C(4), MChaC(2), MCha2C(2)
        double complex MSfC(2,4,3), MSf2C(2,4,3), MGlC, MGl2C
        double complex Mh0C, MHHC, MA0C, MHpC
        double complex Mh02C, MHH2C, MA02C, MHp2C
        ! complex masses with particle widths included
        double complex MSf2W(2,4,3), MGl2W

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
        common /mssmpara/ USfC, VChaC, UChaC, ZNeuC  ! complexified parameters for loop integrals
        common /mssmpara/ Mino3C, MUEC, AfC
        common /mssmpara/ MNeuC, MNeu2C, MChaC, MCha2C
        common /mssmpara/ MSfC, MSf2C, MGlC, MGl2C
        common /mssmpara/ Mh0C, MHHC, MA0C, MHpC
        common /mssmpara/ Mh02C, MHH2C, MA02C, MHp2C
        common /mssmpara/ MSf2W, MGl2W
c############### end model_mssm.h ######################################
