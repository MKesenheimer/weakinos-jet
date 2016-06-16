      SUBROUTINE SMATRIX_BXSX_NINJSXBX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : b~ s~ -> n1 n2 s~ b~  
C  
C Crossing   1 is b~ s~ -> n1 n2 s~ b~  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      Include "genps.inc"
      Include "nexternal.inc"
      Include "maxamps.inc"
      INTEGER                 NCOMB,     NCROSS         
      PARAMETER(             NCOMB=  64, NCROSS=  1)
      INTEGER    THEL
      PARAMETER(THEL=NCOMB*NCROSS)
C
C FINAL STATES
#include "finalstate.h"
C  
C ARGUMENTS 
C  
      REAL*8 P1(0:3,NEXTERNAL),ANS(NCROSS)
C  
C LOCAL VARIABLES 
C  
      INTEGER NHEL(NEXTERNAL,NCOMB),NTRY
      REAL*8 T, P(0:3,NEXTERNAL)
      REAL*8 MATRIX_BXSX_NINJSXBX
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL), I
      LOGICAL GOODHEL(NCOMB,NCROSS)
      INTEGER NGRAPHS
      REAL*8 hwgt, xtot, xtry, xrej, xr, yfrac(0:ncomb)
      INTEGER idum, ngood, igood(ncomb), jhel, j, jj
      LOGICAL warned


C  
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxflow)
      common/to_Ramps_bxsx_nInJsxbx/  amp2,       jamp2

      character*79         hel_buff
      !common/to_helicity/  hel_buff

      REAL*8 POL(2)
      !common/to_polarization/ POL

      integer          isum_hel
      logical                    multi_channel
      !common/to_matrix/isum_hel, multi_channel
      INTEGER MAPCONFIG(0:LMAXCONFIGS), ICONFIG
      !common/to_mconfigs/mapconfig, iconfig
      DATA NTRY,IDUM /0,-1/
      DATA xtry, xrej, ngood /0,0,0/
      DATA warned, isum_hel/.false.,0/
      DATA multi_channel/.true./
      SAVE yfrac, igood, jhel
      DATA NGRAPHS /   66/          
      DATA jamp2(0) /   1/          
      DATA GOODHEL/THEL*.FALSE./
      DATA(NHEL(IHEL,   1),IHEL=1, 6) /-1,-1,-1,-1,-1,-1/
      DATA(NHEL(IHEL,   2),IHEL=1, 6) /-1,-1,-1,-1,-1, 1/
      DATA(NHEL(IHEL,   3),IHEL=1, 6) /-1,-1,-1,-1, 1,-1/
      DATA(NHEL(IHEL,   4),IHEL=1, 6) /-1,-1,-1,-1, 1, 1/
      DATA(NHEL(IHEL,   5),IHEL=1, 6) /-1,-1,-1, 1,-1,-1/
      DATA(NHEL(IHEL,   6),IHEL=1, 6) /-1,-1,-1, 1,-1, 1/
      DATA(NHEL(IHEL,   7),IHEL=1, 6) /-1,-1,-1, 1, 1,-1/
      DATA(NHEL(IHEL,   8),IHEL=1, 6) /-1,-1,-1, 1, 1, 1/
      DATA(NHEL(IHEL,   9),IHEL=1, 6) /-1,-1, 1,-1,-1,-1/
      DATA(NHEL(IHEL,  10),IHEL=1, 6) /-1,-1, 1,-1,-1, 1/
      DATA(NHEL(IHEL,  11),IHEL=1, 6) /-1,-1, 1,-1, 1,-1/
      DATA(NHEL(IHEL,  12),IHEL=1, 6) /-1,-1, 1,-1, 1, 1/
      DATA(NHEL(IHEL,  13),IHEL=1, 6) /-1,-1, 1, 1,-1,-1/
      DATA(NHEL(IHEL,  14),IHEL=1, 6) /-1,-1, 1, 1,-1, 1/
      DATA(NHEL(IHEL,  15),IHEL=1, 6) /-1,-1, 1, 1, 1,-1/
      DATA(NHEL(IHEL,  16),IHEL=1, 6) /-1,-1, 1, 1, 1, 1/
      DATA(NHEL(IHEL,  17),IHEL=1, 6) /-1, 1,-1,-1,-1,-1/
      DATA(NHEL(IHEL,  18),IHEL=1, 6) /-1, 1,-1,-1,-1, 1/
      DATA(NHEL(IHEL,  19),IHEL=1, 6) /-1, 1,-1,-1, 1,-1/
      DATA(NHEL(IHEL,  20),IHEL=1, 6) /-1, 1,-1,-1, 1, 1/
      DATA(NHEL(IHEL,  21),IHEL=1, 6) /-1, 1,-1, 1,-1,-1/
      DATA(NHEL(IHEL,  22),IHEL=1, 6) /-1, 1,-1, 1,-1, 1/
      DATA(NHEL(IHEL,  23),IHEL=1, 6) /-1, 1,-1, 1, 1,-1/
      DATA(NHEL(IHEL,  24),IHEL=1, 6) /-1, 1,-1, 1, 1, 1/
      DATA(NHEL(IHEL,  25),IHEL=1, 6) /-1, 1, 1,-1,-1,-1/
      DATA(NHEL(IHEL,  26),IHEL=1, 6) /-1, 1, 1,-1,-1, 1/
      DATA(NHEL(IHEL,  27),IHEL=1, 6) /-1, 1, 1,-1, 1,-1/
      DATA(NHEL(IHEL,  28),IHEL=1, 6) /-1, 1, 1,-1, 1, 1/
      DATA(NHEL(IHEL,  29),IHEL=1, 6) /-1, 1, 1, 1,-1,-1/
      DATA(NHEL(IHEL,  30),IHEL=1, 6) /-1, 1, 1, 1,-1, 1/
      DATA(NHEL(IHEL,  31),IHEL=1, 6) /-1, 1, 1, 1, 1,-1/
      DATA(NHEL(IHEL,  32),IHEL=1, 6) /-1, 1, 1, 1, 1, 1/
      DATA(NHEL(IHEL,  33),IHEL=1, 6) / 1,-1,-1,-1,-1,-1/
      DATA(NHEL(IHEL,  34),IHEL=1, 6) / 1,-1,-1,-1,-1, 1/
      DATA(NHEL(IHEL,  35),IHEL=1, 6) / 1,-1,-1,-1, 1,-1/
      DATA(NHEL(IHEL,  36),IHEL=1, 6) / 1,-1,-1,-1, 1, 1/
      DATA(NHEL(IHEL,  37),IHEL=1, 6) / 1,-1,-1, 1,-1,-1/
      DATA(NHEL(IHEL,  38),IHEL=1, 6) / 1,-1,-1, 1,-1, 1/
      DATA(NHEL(IHEL,  39),IHEL=1, 6) / 1,-1,-1, 1, 1,-1/
      DATA(NHEL(IHEL,  40),IHEL=1, 6) / 1,-1,-1, 1, 1, 1/
      DATA(NHEL(IHEL,  41),IHEL=1, 6) / 1,-1, 1,-1,-1,-1/
      DATA(NHEL(IHEL,  42),IHEL=1, 6) / 1,-1, 1,-1,-1, 1/
      DATA(NHEL(IHEL,  43),IHEL=1, 6) / 1,-1, 1,-1, 1,-1/
      DATA(NHEL(IHEL,  44),IHEL=1, 6) / 1,-1, 1,-1, 1, 1/
      DATA(NHEL(IHEL,  45),IHEL=1, 6) / 1,-1, 1, 1,-1,-1/
      DATA(NHEL(IHEL,  46),IHEL=1, 6) / 1,-1, 1, 1,-1, 1/
      DATA(NHEL(IHEL,  47),IHEL=1, 6) / 1,-1, 1, 1, 1,-1/
      DATA(NHEL(IHEL,  48),IHEL=1, 6) / 1,-1, 1, 1, 1, 1/
      DATA(NHEL(IHEL,  49),IHEL=1, 6) / 1, 1,-1,-1,-1,-1/
      DATA(NHEL(IHEL,  50),IHEL=1, 6) / 1, 1,-1,-1,-1, 1/
      DATA(NHEL(IHEL,  51),IHEL=1, 6) / 1, 1,-1,-1, 1,-1/
      DATA(NHEL(IHEL,  52),IHEL=1, 6) / 1, 1,-1,-1, 1, 1/
      DATA(NHEL(IHEL,  53),IHEL=1, 6) / 1, 1,-1, 1,-1,-1/
      DATA(NHEL(IHEL,  54),IHEL=1, 6) / 1, 1,-1, 1,-1, 1/
      DATA(NHEL(IHEL,  55),IHEL=1, 6) / 1, 1,-1, 1, 1,-1/
      DATA(NHEL(IHEL,  56),IHEL=1, 6) / 1, 1,-1, 1, 1, 1/
      DATA(NHEL(IHEL,  57),IHEL=1, 6) / 1, 1, 1,-1,-1,-1/
      DATA(NHEL(IHEL,  58),IHEL=1, 6) / 1, 1, 1,-1,-1, 1/
      DATA(NHEL(IHEL,  59),IHEL=1, 6) / 1, 1, 1,-1, 1,-1/
      DATA(NHEL(IHEL,  60),IHEL=1, 6) / 1, 1, 1,-1, 1, 1/
      DATA(NHEL(IHEL,  61),IHEL=1, 6) / 1, 1, 1, 1,-1,-1/
      DATA(NHEL(IHEL,  62),IHEL=1, 6) / 1, 1, 1, 1,-1, 1/
      DATA(NHEL(IHEL,  63),IHEL=1, 6) / 1, 1, 1, 1, 1,-1/
      DATA(NHEL(IHEL,  64),IHEL=1, 6) / 1, 1, 1, 1, 1, 1/
      DATA(  IC(IHEL,  1),IHEL=1, 6) / 1, 2, 3, 4, 5, 6/
      DATA(IDEN(IHEL),IHEL=  1,  1) /  36/
C ----------
C BEGIN CODE
C ----------
      NTRY=NTRY+1
      DO IPROC=1,NCROSS
      CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL)
      DO IHEL=1,NEXTERNAL
         JC(IHEL) = +1
      ENDDO
       
          DO IHEL=1,NGRAPHS
              amp2(ihel)=0d0
          ENDDO
          DO IHEL=1,int(jamp2(0))
              jamp2(ihel)=0d0
          ENDDO
      ANS(IPROC) = 0D0
          DO IHEL=1,NCOMB
             IF(GOODHEL(IHEL,IPROC) .OR. NTRY .LT. 2) THEN
                 T=MATRIX_BXSX_NINJSXBX(P,NHEL(1,IHEL),JC(1))            
               ANS(IPROC)=ANS(IPROC)+T
               IF(T .NE. 0D0 .AND. .NOT.    GOODHEL(IHEL,IPROC)) THEN
                   GOODHEL(IHEL,IPROC)=.TRUE.
                   NGOOD = NGOOD +1
                   IGOOD(NGOOD) = IHEL
               ENDIF
             ENDIF
          ENDDO
      
      if(final1.eq.final2) then ! equal final states
        ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC)*2)
      else
        ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))
      endif
      ENDDO
      END
       
       
      REAL*8 FUNCTION MATRIX_BXSX_NINJSXBX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : b~ s~ -> n1 n2 s~ b~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS=  66,NEIGEN=  1) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS=  96, NCOLOR=   1) 
      REAL*8     ZERO
      PARAMETER(ZERO=0D0)
C  
C ARGUMENTS 
C  
      REAL*8 P(0:3,NEXTERNAL)
      INTEGER NHEL(NEXTERNAL), IC(NEXTERNAL)
C  
C LOCAL VARIABLES 
C  
      INTEGER I,J
      COMPLEX*16 ZTEMP
      REAL*8 DENOM(NCOLOR), CF(NCOLOR,NCOLOR)
      COMPLEX*16 AMP(NGRAPHS), JAMP(NCOLOR)
      COMPLEX*16 W(18,NWAVEFUNCS)
C  
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxflow)
      common/to_Ramps_bxsx_nInJsxbx/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            1/                                       
      DATA(CF(i,1),i=1,1) /     2/                                  
C               T[ 2, 6]T[ 1, 5]                                           
C ----------
C BEGIN CODE
C ----------
      CALL OXXXXX(P(0,1),BMASS,NHEL(1),-1*IC(1),W(1,1))       
      CALL OXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))        
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),ZERO,NHEL(5),-1*IC(5),W(1,5))        
      CALL IXXXXX(P(0,6),BMASS,NHEL(6),-1*IC(6),W(1,6))       
      CALL HIOXXX(W(1,4),W(1,1),GB1N2P,MBL,WB1,W(1,7))                                                          
      CALL JIOXXX(W(1,5),W(1,2),GG,ZERO,ZERO,W(1,8))     
      CALL HVSXXX(W(1,8),W(1,7),GC,MBL,WB1,W(1,9))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,9),GB1N1M,AMP(1))         
      CALL HIOXXX(W(1,4),W(1,1),GB2N2P,MBR,WB2,W(1,10))                                                          
      CALL HVSXXX(W(1,8),W(1,10),GC,MBR,WB2,W(1,11))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,11),GB2N1M,AMP(2))         
      CALL FSOXXX(W(1,3),W(1,7),GB1N1M,BMASS,ZERO,W(1,12))                                                          
      CALL IOVXXX(W(1,6),W(1,12),W(1,8),GG,AMP(3))             
      CALL FSOXXX(W(1,3),W(1,10),GB2N1M,BMASS,ZERO,W(1,13))                                                          
      CALL IOVXXX(W(1,6),W(1,13),W(1,8),GG,AMP(4))             
      CALL FVOXXX(W(1,1),W(1,8),GG,BMASS,ZERO,W(1,14))     
      CALL HIOXXX(W(1,4),W(1,14),GB1N2P,MBL,WB1,W(1,15))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,15),GB1N1M,AMP(5))         
      CALL HIOXXX(W(1,4),W(1,14),GB2N2P,MBR,WB2,W(1,16))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,16),GB2N1M,AMP(6))         
      CALL HIOXXX(W(1,5),W(1,3),GDLNIM,MSL,WSL,W(1,17))                                                          
      CALL FSOXXX(W(1,2),W(1,17),GQLGOP,MGO,WGO,W(1,18))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,7),GB1GOM,AMP(7))         
      CALL HIOXXX(W(1,5),W(1,3),GDRNIM,MSR,WSR,W(1,19))                                                          
      CALL FSOXXX(W(1,2),W(1,19),GQRGOP,MGO,WGO,W(1,20))                                                          
      CALL IOSXXX(W(1,6),W(1,20),W(1,7),GB1GOM,AMP(8))         
      CALL IOSXXX(W(1,6),W(1,18),W(1,10),GB2GOM,AMP(9))         
      CALL IOSXXX(W(1,6),W(1,20),W(1,10),GB2GOM,AMP(10))         
      CALL HIOXXX(W(1,4),W(1,2),GDLNJP,MSL,WSL,W(1,21))                                                          
      CALL FSOXXX(W(1,3),W(1,21),GDLNIM,ZERO,ZERO,W(1,22))                                                          
      CALL JIOXXX(W(1,5),W(1,22),GG,ZERO,ZERO,W(1,23))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,23),GG,AMP(11))             
      CALL HIOXXX(W(1,4),W(1,2),GDRNJP,MSR,WSR,W(1,24))                                                          
      CALL FSOXXX(W(1,3),W(1,24),GDRNIM,ZERO,ZERO,W(1,25))                                                          
      CALL JIOXXX(W(1,5),W(1,25),GG,ZERO,ZERO,W(1,26))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,26),GG,AMP(12))             
      CALL JSSXXX(W(1,21),W(1,17),GC,ZERO,ZERO,W(1,27))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,27),GG,AMP(13))             
      CALL JSSXXX(W(1,24),W(1,19),GC,ZERO,ZERO,W(1,28))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,28),GG,AMP(14))             
      CALL FSIXXX(W(1,4),W(1,17),GDLNJP,ZERO,ZERO,W(1,29))                                                          
      CALL JIOXXX(W(1,29),W(1,2),GG,ZERO,ZERO,W(1,30))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,30),GG,AMP(15))             
      CALL FSIXXX(W(1,4),W(1,19),GDRNJP,ZERO,ZERO,W(1,31))                                                          
      CALL JIOXXX(W(1,31),W(1,2),GG,ZERO,ZERO,W(1,32))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,32),GG,AMP(16))             
      CALL FSIXXX(W(1,5),W(1,21),GQLGOM,MGO,WGO,W(1,33))                                                          
      CALL HIOXXX(W(1,33),W(1,1),GB1GOP,MBL,WB1,W(1,34))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,34),GB1N1M,AMP(17))         
      CALL HIOXXX(W(1,33),W(1,1),GB2GOP,MBR,WB2,W(1,35))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,35),GB2N1M,AMP(18))         
      CALL FSIXXX(W(1,5),W(1,24),GQRGOM,MGO,WGO,W(1,36))                                                          
      CALL HIOXXX(W(1,36),W(1,1),GB1GOP,MBL,WB1,W(1,37))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,37),GB1N1M,AMP(19))         
      CALL HIOXXX(W(1,36),W(1,1),GB2GOP,MBR,WB2,W(1,38))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,38),GB2N1M,AMP(20))         
      CALL JIOCXX(W(1,4),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,39))                                                          
      CALL FVOXXX(W(1,2),W(1,39),GZD,ZERO,ZERO,W(1,40))    
      CALL JIOXXX(W(1,5),W(1,40),GG,ZERO,ZERO,W(1,41))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,41),GG,AMP(21))             
      CALL IOVXXX(W(1,6),W(1,14),W(1,39),GZD,AMP(22))            
      CALL HIOCXX(W(1,4),W(1,3),GH1NIJ,MH1,WH1,W(1,      
     &     42))                                                          
      CALL IOSXXX(W(1,6),W(1,14),W(1,42),GH1BB,AMP(23))          
      CALL HIOCXX(W(1,4),W(1,3),GH2NIJ,MH2,WH2,W(1,      
     &     43))                                                          
      CALL IOSXXX(W(1,6),W(1,14),W(1,43),GH2BB,AMP(24))          
      CALL HIOCXX(W(1,4),W(1,3),GH3NIJ,MH3,WH3,W(1,      
     &     44))                                                          
      CALL IOSXXX(W(1,6),W(1,14),W(1,44),GH3BB,AMP(25))          
      CALL FVOXXX(W(1,1),W(1,39),GZD,BMASS,ZERO,W(1,45))    
      CALL IOVXXX(W(1,6),W(1,45),W(1,8),GG,AMP(26))             
      CALL FSOXXX(W(1,1),W(1,42),GH1BB,BMASS,ZERO,W(1, 46))                                                          
      CALL IOVXXX(W(1,6),W(1,46),W(1,8),GG,AMP(27))             
      CALL FSOXXX(W(1,1),W(1,43),GH2BB,BMASS,ZERO,W(1, 47))                                                          
      CALL IOVXXX(W(1,6),W(1,47),W(1,8),GG,AMP(28))             
      CALL FSOXXX(W(1,1),W(1,44),GH3BB,BMASS,ZERO,W(1, 48))                                                          
      CALL IOVXXX(W(1,6),W(1,48),W(1,8),GG,AMP(29))             
      CALL FVIXXX(W(1,5),W(1,39),GZD,ZERO,ZERO,W(1,49))    
      CALL JIOXXX(W(1,49),W(1,2),GG,ZERO,ZERO,W(1,50))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,50),GG,AMP(30))             
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,51))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,52))         
      CALL HIOXXX(W(1,51),W(1,2),GDLNIP,MSL,WSL,W(1,53))                                                          
      CALL FSOXXX(W(1,52),W(1,53),GDLNJM,ZERO,ZERO,W(1,54))                                                          
      CALL JIOXXX(W(1,5),W(1,54),GG,ZERO,ZERO,W(1,55))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,55),GG,AMP(31))             
      CALL HIOXXX(W(1,51),W(1,2),GDRNIP,MSR,WSR,W(1,56))                                                          
      CALL FSOXXX(W(1,52),W(1,56),GDRNJM,ZERO,ZERO,W(1,57))                                                          
      CALL JIOXXX(W(1,5),W(1,57),GG,ZERO,ZERO,W(1,58))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,58),GG,AMP(32))             
      CALL HIOXXX(W(1,5),W(1,52),GDLNJM,MSL,WSL,W(1,59))                                                          
      CALL JSSXXX(W(1,53),W(1,59),GC,ZERO,ZERO,W(1,60))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,60),GG,AMP(33))             
      CALL HIOXXX(W(1,5),W(1,52),GDRNJM,MSR,WSR,W(1,61))                                                          
      CALL JSSXXX(W(1,56),W(1,61),GC,ZERO,ZERO,W(1,62))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,62),GG,AMP(34))             
      CALL FSIXXX(W(1,51),W(1,59),GDLNIP,ZERO,ZERO,W(1,63))                                                          
      CALL JIOXXX(W(1,63),W(1,2),GG,ZERO,ZERO,W(1,64))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,64),GG,AMP(35))             
      CALL FSIXXX(W(1,51),W(1,61),GDRNIP,ZERO,ZERO,W(1,65))                                                          
      CALL JIOXXX(W(1,65),W(1,2),GG,ZERO,ZERO,W(1,66))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,66),GG,AMP(36))             
      CALL FSIXXX(W(1,5),W(1,53),GQLGOM,MGO,WGO,W(1,67))                                                          
      CALL HIOXXX(W(1,67),W(1,1),GB1GOP,MBL,WB1,W(1,68))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,68),GB1N2M,AMP(37))         
      CALL HIOXXX(W(1,67),W(1,1),GB2GOP,MBR,WB2,W(1,69))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,69),GB2N2M,AMP(38))         
      CALL FSIXXX(W(1,5),W(1,56),GQRGOM,MGO,WGO,W(1,70))                                                          
      CALL HIOXXX(W(1,70),W(1,1),GB1GOP,MBL,WB1,W(1,71))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,71),GB1N2M,AMP(39))         
      CALL HIOXXX(W(1,70),W(1,1),GB2GOP,MBR,WB2,W(1,72))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,72),GB2N2M,AMP(40))         
      CALL HIOXXX(W(1,51),W(1,1),GB1N1P,MBL,WB1,W(1,73))                                                          
      CALL HVSXXX(W(1,8),W(1,73),GC,MBL,WB1,W(1,74))     
      CALL IOSXXX(W(1,6),W(1,52),W(1,74),GB1N2M,AMP(41))         
      CALL HIOXXX(W(1,51),W(1,1),GB2N1P,MBR,WB2,W(1,75))                                                          
      CALL HVSXXX(W(1,8),W(1,75),GC,MBR,WB2,W(1,76))     
      CALL IOSXXX(W(1,6),W(1,52),W(1,76),GB2N2M,AMP(42))         
      CALL FSOXXX(W(1,52),W(1,73),GB1N2M,BMASS,ZERO,W(1,77))                                                          
      CALL IOVXXX(W(1,6),W(1,77),W(1,8),GG,AMP(43))             
      CALL FSOXXX(W(1,52),W(1,75),GB2N2M,BMASS,ZERO,W(1,78))                                                          
      CALL IOVXXX(W(1,6),W(1,78),W(1,8),GG,AMP(44))             
      CALL HIOXXX(W(1,51),W(1,14),GB1N1P,MBL,WB1,W(1,79))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,79),GB1N2M,AMP(45))         
      CALL HIOXXX(W(1,51),W(1,14),GB2N1P,MBR,WB2,W(1,80))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,80),GB2N2M,AMP(46))         
      CALL FSOXXX(W(1,2),W(1,59),GQLGOP,MGO,WGO,W(1,81))                                                          
      CALL IOSXXX(W(1,6),W(1,81),W(1,73),GB1GOM,AMP(47))         
      CALL FSOXXX(W(1,2),W(1,61),GQRGOP,MGO,WGO,W(1,82))                                                          
      CALL IOSXXX(W(1,6),W(1,82),W(1,73),GB1GOM,AMP(48))         
      CALL IOSXXX(W(1,6),W(1,81),W(1,75),GB2GOM,AMP(49))         
      CALL IOSXXX(W(1,6),W(1,82),W(1,75),GB2GOM,AMP(50))         
      CALL OXXXXX(P(0,6),BMASS,NHEL(6),+1*IC(6),W(1,83))       
      CALL IOSCXX(W(1,67),W(1,83),W(1,7),GB1GOM,AMP(51))        
      CALL IOSCXX(W(1,67),W(1,83),W(1,10),GB2GOM,AMP(52))        
      CALL IOSCXX(W(1,70),W(1,83),W(1,7),GB1GOM,AMP(53))        
      CALL IOSCXX(W(1,70),W(1,83),W(1,10),GB2GOM,AMP(54))        
      CALL IOSCXX(W(1,33),W(1,83),W(1,73),GB1GOM,AMP(55))        
      CALL IOSCXX(W(1,36),W(1,83),W(1,73),GB1GOM,AMP(56))        
      CALL IOSCXX(W(1,33),W(1,83),W(1,75),GB2GOM,AMP(57))        
      CALL IOSCXX(W(1,36),W(1,83),W(1,75),GB2GOM,AMP(58))        
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,84))        
      CALL FSICXX(W(1,84),W(1,17),GQLGOP,MGO,WGO,W(1,      
     &     85))                                                          
      CALL HIOXXX(W(1,85),W(1,1),GB1GOP,MBL,WB1,W(1,86))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,86),GB1N2M,AMP(59))         
      CALL HIOXXX(W(1,85),W(1,1),GB2GOP,MBR,WB2,W(1,87))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,87),GB2N2M,AMP(60))         
      CALL FSICXX(W(1,84),W(1,19),GQRGOP,MGO,WGO,W(1,      
     &     88))                                                          
      CALL HIOXXX(W(1,88),W(1,1),GB1GOP,MBL,WB1,W(1,89))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,89),GB1N2M,AMP(61))         
      CALL HIOXXX(W(1,88),W(1,1),GB2GOP,MBR,WB2,W(1,90))                                                          
      CALL IOSXXX(W(1,6),W(1,52),W(1,90),GB2N2M,AMP(62))         
      CALL FSICXX(W(1,84),W(1,59),GQLGOP,MGO,WGO,W(1,      
     &     91))                                                          
      CALL HIOXXX(W(1,91),W(1,1),GB1GOP,MBL,WB1,W(1,92))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,92),GB1N1M,AMP(63))         
      CALL HIOXXX(W(1,91),W(1,1),GB2GOP,MBR,WB2,W(1,93))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,93),GB2N1M,AMP(64))         
      CALL FSICXX(W(1,84),W(1,61),GQRGOP,MGO,WGO,W(1,      
     &     94))                                                          
      CALL HIOXXX(W(1,94),W(1,1),GB1GOP,MBL,WB1,W(1,95))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,95),GB1N1M,AMP(65))         
      CALL HIOXXX(W(1,94),W(1,1),GB2GOP,MBR,WB2,W(1,96))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,96),GB2N1M,AMP(66))         
      JAMP(   1) = +AMP(   1)+AMP(   2)+AMP(   3)+AMP(   4)+AMP(   5)
     &             +AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             +AMP(  11)+AMP(  12)+AMP(  13)+AMP(  14)+AMP(  15)
     &             +AMP(  16)-AMP(  17)-AMP(  18)-AMP(  19)-AMP(  20)
     &             -AMP(  21)-AMP(  22)-AMP(  23)-AMP(  24)-AMP(  25)
     &             -AMP(  26)-AMP(  27)-AMP(  28)-AMP(  29)-AMP(  30)
     &             -AMP(  31)-AMP(  32)-AMP(  33)-AMP(  34)-AMP(  35)
     &             -AMP(  36)+AMP(  37)+AMP(  38)+AMP(  39)+AMP(  40)
     &             -AMP(  41)-AMP(  42)-AMP(  43)-AMP(  44)-AMP(  45)
     &             -AMP(  46)+AMP(  47)+AMP(  48)+AMP(  49)+AMP(  50)
     &             +AMP(  51)+AMP(  52)+AMP(  53)+AMP(  54)-AMP(  55)
     &             -AMP(  56)-AMP(  57)-AMP(  58)+AMP(  59)+AMP(  60)
     &             +AMP(  61)+AMP(  62)-AMP(  63)-AMP(  64)-AMP(  65)
     &             -AMP(  66)
      MATRIX_BXSX_NINJSXBX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_BXSX_NINJSXBX =MATRIX_BXSX_NINJSXBX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
