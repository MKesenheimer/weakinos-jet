c############### model_sm.h ############################################
* model_sm.h
* common blocks for the model parameters
* this file is part of FormCalc
* last modified 23 Dec 14 th by MK
        !Z mass, W mass, cos(theta_w), sin(theta_w)
        double precision MZ, MZ2, MW, MW2, CW, CW2, SW, SW2
        double precision WZ, WW
        ! Fermi constant, fine structure, fine structure constant at MZ, strong coupling
        double precision GF, Alfa, Alfa2, AlfaMZ, AlfasMZ, Alfas, Alfas2
        ! Quark masses
        double precision MU, MU2, MC, MC2, MT, MT2
        double precision MD, MD2, MS, MS2, MB, MB2
        double precision Mf(4,3), Mf2(4,3)
        ! strong and e.m. coupling constants
        double precision EL, GS

        ! complex parameters for loop integrals
        double complex MZC, MZ2C, MWC, MW2C
        double complex MUC, MU2C, MCC, MC2C, MTC, MT2C
        double complex MDC, MD2C, MSC, MS2C, MBC, MB2C
        double complex MfC(4,3), Mf2C(4,3)
        ! complex masses with widths included 
        double complex MZ2W, MW2W
        
        ! gauge parameters
        double precision GaugeXi(4)

        common /smpara/ MZ, MZ2, MW, MW2, CW, CW2, SW, SW2
        common /smpara/ WZ, WW
        common /smpara/ GF, Alfa, Alfa2, AlfaMZ, AlfasMZ, Alfas, Alfas2
        common /smpara/ MU, MU2, MC, MC2, MT, MT2
        common /smpara/ MD, MD2, MS, MS2, MB, MB2
        common /smpara/ Mf,Mf2
        common /smpara/ EL, GS
        common /smpara/ MZC, MZ2C, MWC, MW2C
        common /smpara/ MUC, MU2C, MCC, MC2C, MTC, MT2C 
        common /smpara/ MDC, MD2C, MSC, MS2C, MBC, MB2C
        common /smpara/ MfC, Mf2C
        common /smpara/ MZ2W, MW2W
        common /smpara/ GaugeXi
c############### end model_sm.h ########################################
