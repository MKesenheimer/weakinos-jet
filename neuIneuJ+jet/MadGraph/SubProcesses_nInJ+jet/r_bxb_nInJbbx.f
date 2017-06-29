      SUBROUTINE SMATRIX_BXB_NINJBBX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : b~ b -> n1 n2 b b~  
C  
C Crossing   1 is b~ b -> n1 n2 b b~  
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
C
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
      REAL*8 MATRIX_BXB_NINJBBX
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
      common/to_Ramps_bxb_nInJbbx/  amp2,       jamp2

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
      DATA NGRAPHS /  144/          
      DATA jamp2(0) /   2/          
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
      !CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL)
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
                 T=MATRIX_BXB_NINJBBX(P1,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_BXB_NINJBBX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : b~ b -> n1 n2 b b~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS= 144,NEIGEN=  2) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS= 167, NCOLOR=   2) 
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
      common/to_Ramps_bxb_nInJbbx/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            3/                                       
      DATA(CF(i,1),i=1,2) /     6,   -2/                            
C               T[ 5, 6]T[ 1, 2]                                           
      DATA Denom(2)/            3/                                       
      DATA(CF(i,2),i=1,2) /    -2,    6/                            
C               T[ 5, 2]T[ 1, 6]                                           
C ----------
C BEGIN CODE
C ----------
      CALL OXXXXX(P(0,1),BMASS,NHEL(1),-1*IC(1),W(1,1))       
      CALL IXXXXX(P(0,2),BMASS,NHEL(2),+1*IC(2),W(1,2))       
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),BMASS,NHEL(5),-1*IC(5),W(1,5))       
      CALL IXXXXX(P(0,6),BMASS,NHEL(6),-1*IC(6),W(1,6))       
      CALL HIOXXX(W(1,2),W(1,3),GB1NIM,MBL,WBL,W(1,7))                                                          
      CALL FSOCXX(W(1,4),W(1,7),GB1NJP,BMASS,ZERO,W(1,8))                                                          
      CALL JIOCXX(W(1,5),W(1,8),GG,ZERO,ZERO,W(1,9))    
      CALL IOVXXX(W(1,6),W(1,1),W(1,9),GG,AMP(1))             
      CALL HIOXXX(W(1,2),W(1,3),GB2NIM,MBR,WBR,W(1,10))                                                          
      CALL FSOCXX(W(1,4),W(1,10),GB2NJP,BMASS,ZERO,W(1,11))                                                          
      CALL JIOCXX(W(1,5),W(1,11),GG,ZERO,ZERO,W(1,12))    
      CALL IOVXXX(W(1,6),W(1,1),W(1,12),GG,AMP(2))             
      CALL HIOCXX(W(1,5),W(1,4),GB1NJP,MBL,WBL,W(1,13))                                                          
      CALL JSSXXX(W(1,13),W(1,7),GC,ZERO,ZERO,W(1,14))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,14),GG,AMP(3))             
      CALL HIOCXX(W(1,5),W(1,4),GB2NJP,MBR,WBR,W(1,15))                                                          
      CALL JSSXXX(W(1,15),W(1,10),GC,ZERO,ZERO,W(1,16))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,16),GG,AMP(4))             
      CALL FSOXXX(W(1,1),W(1,7),GB1GOP,MGO,WGO,W(1,17))                                                          
      CALL IOSXXX(W(1,6),W(1,17),W(1,13),GB1GOM,AMP(5))         
      CALL IOSXXX(W(1,6),W(1,17),W(1,15),GB2GOM,AMP(6))         
      CALL FSOXXX(W(1,1),W(1,10),GB2GOP,MGO,WGO,W(1,18))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,13),GB1GOM,AMP(7))         
      CALL IOSXXX(W(1,6),W(1,18),W(1,15),GB2GOM,AMP(8))         
      CALL FSOXXX(W(1,3),W(1,13),GB1NIM,BMASS,ZERO,W(1,19))                                                          
      CALL JIOXXX(W(1,2),W(1,19),GG,ZERO,ZERO,W(1,20))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,20),GG,AMP(9))             
      CALL FSOXXX(W(1,3),W(1,15),GB2NIM,BMASS,ZERO,W(1,21))                                                          
      CALL JIOXXX(W(1,2),W(1,21),GG,ZERO,ZERO,W(1,22))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,22),GG,AMP(10))             
      CALL HIOCXX(W(1,5),W(1,3),GB1NIP,MBL,WBL,W(1,23))                                                          
      CALL FSIXXX(W(1,2),W(1,23),GB1GOM,MGO,WGO,W(1,24))                                                          
      CALL HIOXXX(W(1,24),W(1,1),GB1GOP,MBL,WBL,W(1,25))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,25),GB1NJM,AMP(11))         
      CALL HIOXXX(W(1,24),W(1,1),GB2GOP,MBR,WBR,W(1,26))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,26),GB2NJM,AMP(12))         
      CALL HIOCXX(W(1,5),W(1,3),GB2NIP,MBR,WBR,W(1,27))                                                          
      CALL FSIXXX(W(1,2),W(1,27),GB2GOM,MGO,WGO,W(1,28))                                                          
      CALL HIOXXX(W(1,28),W(1,1),GB1GOP,MBL,WBL,W(1,29))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,29),GB1NJM,AMP(13))         
      CALL HIOXXX(W(1,28),W(1,1),GB2GOP,MBR,WBR,W(1,30))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,30),GB2NJM,AMP(14))         
      CALL JIOXXX(W(1,2),W(1,1),GG,ZERO,ZERO,W(1,31))     
      CALL HVSXXX(W(1,31),W(1,23),GC,MBL,WBL,W(1,32))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,32),GB1NJM,AMP(15))         
      CALL HVSXXX(W(1,31),W(1,27),GC,MBR,WBR,W(1,33))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,33),GB2NJM,AMP(16))         
      CALL FSOXXX(W(1,4),W(1,23),GB1NJM,BMASS,ZERO,W(1,34))                                                          
      CALL IOVXXX(W(1,6),W(1,34),W(1,31),GG,AMP(17))             
      CALL FSOXXX(W(1,4),W(1,27),GB2NJM,BMASS,ZERO,W(1,35))                                                          
      CALL IOVXXX(W(1,6),W(1,35),W(1,31),GG,AMP(18))             
      CALL FVICXX(W(1,5),W(1,31),GG,BMASS,ZERO,W(1,36))    
      CALL HIOCXX(W(1,36),W(1,3),GB1NIP,MBL,WBL,W(1,37))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,37),GB1NJM,AMP(19))         
      CALL HIOCXX(W(1,36),W(1,3),GB2NIP,MBR,WBR,W(1,38))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,38),GB2NJM,AMP(20))         
      CALL HIOCXX(W(1,5),W(1,17),GB1GOP,MBL,WBL,W(1,39))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,39),GB1NJM,AMP(21))         
      CALL HIOCXX(W(1,5),W(1,17),GB2GOP,MBR,WBR,W(1,40))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,40),GB2NJM,AMP(22))         
      CALL HIOCXX(W(1,5),W(1,18),GB1GOP,MBL,WBL,W(1,41))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,41),GB1NJM,AMP(23))         
      CALL HIOCXX(W(1,5),W(1,18),GB2GOP,MBR,WBR,W(1,42))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,42),GB2NJM,AMP(24))         
      CALL FSICXX(W(1,5),W(1,7),GB1GOP,MGO,WGO,W(1,43))                                                          
      CALL HIOXXX(W(1,43),W(1,1),GB1GOP,MBL,WBL,W(1,44))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,44),GB1NJM,AMP(25))         
      CALL HIOXXX(W(1,43),W(1,1),GB2GOP,MBR,WBR,W(1,45))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,45),GB2NJM,AMP(26))         
      CALL FSICXX(W(1,5),W(1,10),GB2GOP,MGO,WGO,W(1,46))                                                          
      CALL HIOXXX(W(1,46),W(1,1),GB1GOP,MBL,WBL,W(1,47))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,47),GB1NJM,AMP(27))         
      CALL HIOXXX(W(1,46),W(1,1),GB2GOP,MBR,WBR,W(1,48))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,48),GB2NJM,AMP(28))         
      CALL HIOXXX(W(1,2),W(1,4),GB1NJM,MBL,WBL,W(1,49))                                                          
      CALL FSOCXX(W(1,3),W(1,49),GB1NIP,BMASS,ZERO,W(1,50))                                                          
      CALL JIOCXX(W(1,5),W(1,50),GG,ZERO,ZERO,W(1,51))    
      CALL IOVXXX(W(1,6),W(1,1),W(1,51),GG,AMP(29))             
      CALL HIOXXX(W(1,2),W(1,4),GB2NJM,MBR,WBR,W(1,52))                                                          
      CALL FSOCXX(W(1,3),W(1,52),GB2NIP,BMASS,ZERO,W(1,53))                                                          
      CALL JIOCXX(W(1,5),W(1,53),GG,ZERO,ZERO,W(1,54))    
      CALL IOVXXX(W(1,6),W(1,1),W(1,54),GG,AMP(30))             
      CALL JSSXXX(W(1,23),W(1,49),GC,ZERO,ZERO,W(1,55))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,55),GG,AMP(31))             
      CALL JSSXXX(W(1,27),W(1,52),GC,ZERO,ZERO,W(1,56))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,56),GG,AMP(32))             
      CALL FSOXXX(W(1,1),W(1,49),GB1GOP,MGO,WGO,W(1,57))                                                          
      CALL IOSXXX(W(1,6),W(1,57),W(1,23),GB1GOM,AMP(33))         
      CALL IOSXXX(W(1,6),W(1,57),W(1,27),GB2GOM,AMP(34))         
      CALL FSOXXX(W(1,1),W(1,52),GB2GOP,MGO,WGO,W(1,58))                                                          
      CALL IOSXXX(W(1,6),W(1,58),W(1,23),GB1GOM,AMP(35))         
      CALL IOSXXX(W(1,6),W(1,58),W(1,27),GB2GOM,AMP(36))         
      CALL JIOXXX(W(1,2),W(1,34),GG,ZERO,ZERO,W(1,59))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,59),GG,AMP(37))             
      CALL JIOXXX(W(1,2),W(1,35),GG,ZERO,ZERO,W(1,60))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,60),GG,AMP(38))             
      CALL HIOCXX(W(1,5),W(1,57),GB1GOP,MBL,WBL,W(1,61))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,61),GB1NIM,AMP(39))         
      CALL HIOCXX(W(1,5),W(1,57),GB2GOP,MBR,WBR,W(1,62))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,62),GB2NIM,AMP(40))         
      CALL HIOCXX(W(1,5),W(1,58),GB1GOP,MBL,WBL,W(1,63))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,63),GB1NIM,AMP(41))         
      CALL HIOCXX(W(1,5),W(1,58),GB2GOP,MBR,WBR,W(1,64))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,64),GB2NIM,AMP(42))         
      CALL FSICXX(W(1,5),W(1,49),GB1GOP,MGO,WGO,W(1,65))                                                          
      CALL HIOXXX(W(1,65),W(1,1),GB1GOP,MBL,WBL,W(1,66))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,66),GB1NIM,AMP(43))         
      CALL HIOXXX(W(1,65),W(1,1),GB2GOP,MBR,WBR,W(1,67))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,67),GB2NIM,AMP(44))         
      CALL FSICXX(W(1,5),W(1,52),GB2GOP,MGO,WGO,W(1,68))                                                          
      CALL HIOXXX(W(1,68),W(1,1),GB1GOP,MBL,WBL,W(1,69))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,69),GB1NIM,AMP(45))         
      CALL HIOXXX(W(1,68),W(1,1),GB2GOP,MBR,WBR,W(1,70))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,70),GB2NIM,AMP(46))         
      CALL FSIXXX(W(1,2),W(1,13),GB1GOM,MGO,WGO,W(1,71))                                                          
      CALL HIOXXX(W(1,71),W(1,1),GB1GOP,MBL,WBL,W(1,72))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,72),GB1NIM,AMP(47))         
      CALL HIOXXX(W(1,71),W(1,1),GB2GOP,MBR,WBR,W(1,73))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,73),GB2NIM,AMP(48))         
      CALL FSIXXX(W(1,2),W(1,15),GB2GOM,MGO,WGO,W(1,74))                                                          
      CALL HIOXXX(W(1,74),W(1,1),GB1GOP,MBL,WBL,W(1,75))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,75),GB1NIM,AMP(49))         
      CALL HIOXXX(W(1,74),W(1,1),GB2GOP,MBR,WBR,W(1,76))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,76),GB2NIM,AMP(50))         
      CALL HVSXXX(W(1,31),W(1,13),GC,MBL,WBL,W(1,77))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,77),GB1NIM,AMP(51))         
      CALL HVSXXX(W(1,31),W(1,15),GC,MBR,WBR,W(1,78))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,78),GB2NIM,AMP(52))         
      CALL IOVXXX(W(1,6),W(1,19),W(1,31),GG,AMP(53))             
      CALL IOVXXX(W(1,6),W(1,21),W(1,31),GG,AMP(54))             
      CALL HIOCXX(W(1,36),W(1,4),GB1NJP,MBL,WBL,W(1,79))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,79),GB1NIM,AMP(55))         
      CALL HIOCXX(W(1,36),W(1,4),GB2NJP,MBR,WBR,W(1,80))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,80),GB2NIM,AMP(56))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,81))         
      CALL OXXXXX(P(0,5),BMASS,NHEL(5),+1*IC(5),W(1,82))       
      CALL FSIXXX(W(1,81),W(1,7),GB1NJP,BMASS,ZERO,W(1,83))                                                          
      CALL JIOXXX(W(1,83),W(1,1),GG,ZERO,ZERO,W(1,84))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,84),GG,AMP(57))             
      CALL FSIXXX(W(1,81),W(1,10),GB2NJP,BMASS,ZERO,W(1,85))                                                          
      CALL JIOXXX(W(1,85),W(1,1),GG,ZERO,ZERO,W(1,86))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,86),GG,AMP(58))             
      CALL HIOXXX(W(1,81),W(1,1),GB1NJP,MBL,WBL,W(1,87))                                                          
      CALL FSOXXX(W(1,3),W(1,87),GB1NIM,BMASS,ZERO,W(1,88))                                                          
      CALL JIOXXX(W(1,2),W(1,88),GG,ZERO,ZERO,W(1,89))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,89),GG,AMP(59))             
      CALL HIOXXX(W(1,81),W(1,1),GB2NJP,MBR,WBR,W(1,90))                                                          
      CALL FSOXXX(W(1,3),W(1,90),GB2NIM,BMASS,ZERO,W(1,91))                                                          
      CALL JIOXXX(W(1,2),W(1,91),GG,ZERO,ZERO,W(1,92))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,92),GG,AMP(60))             
      CALL JSSXXX(W(1,87),W(1,7),GC,ZERO,ZERO,W(1,93))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,93),GG,AMP(61))             
      CALL JSSXXX(W(1,90),W(1,10),GC,ZERO,ZERO,W(1,94))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,94),GG,AMP(62))             
      CALL FSOXXX(W(1,82),W(1,7),GB1GOP,MGO,WGO,W(1,95))                                                          
      CALL IOSXXX(W(1,6),W(1,95),W(1,87),GB1GOM,AMP(63))         
      CALL IOSXXX(W(1,6),W(1,95),W(1,90),GB2GOM,AMP(64))         
      CALL FSOXXX(W(1,82),W(1,10),GB2GOP,MGO,WGO,W(1,96))                                                          
      CALL IOSXXX(W(1,6),W(1,96),W(1,87),GB1GOM,AMP(65))         
      CALL IOSXXX(W(1,6),W(1,96),W(1,90),GB2GOM,AMP(66))         
      CALL JIOXXX(W(1,2),W(1,82),GG,ZERO,ZERO,W(1,97))     
      CALL HVSXXX(W(1,97),W(1,87),GC,MBL,WBL,W(1,98))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,98),GB1NIM,AMP(67))         
      CALL HVSXXX(W(1,97),W(1,90),GC,MBR,WBR,W(1,99))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,99),GB2NIM,AMP(68))         
      CALL IOVXXX(W(1,6),W(1,88),W(1,97),GG,AMP(69))             
      CALL IOVXXX(W(1,6),W(1,91),W(1,97),GG,AMP(70))             
      CALL FSIXXX(W(1,2),W(1,87),GB1GOM,MGO,WGO,W(1,100))                                                          
      CALL HIOXXX(W(1,100),W(1,82),GB1GOP,MBL,WBL,W(1,101))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,101),GB1NIM,AMP(71))         
      CALL HIOXXX(W(1,100),W(1,82),GB2GOP,MBR,WBR,W(1,102))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,102),GB2NIM,AMP(72))         
      CALL FSIXXX(W(1,2),W(1,90),GB2GOM,MGO,WGO,W(1,103))                                                          
      CALL HIOXXX(W(1,103),W(1,82),GB1GOP,MBL,WBL,W(1,104))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,104),GB1NIM,AMP(73))         
      CALL HIOXXX(W(1,103),W(1,82),GB2GOP,MBR,WBR,W(1,105))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,105),GB2NIM,AMP(74))         
      CALL FVOXXX(W(1,1),W(1,97),GG,BMASS,ZERO,W(1,106))     
      CALL HIOXXX(W(1,81),W(1,106),GB1NJP,MBL,WBL,W(1,107))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,107),GB1NIM,AMP(75))         
      CALL HIOXXX(W(1,81),W(1,106),GB2NJP,MBR,WBR,W(1,108))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,108),GB2NIM,AMP(76))         
      CALL JIOCXX(W(1,81),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,109))                                                          
      CALL FVIXXX(W(1,2),W(1,109),GZD,BMASS,ZERO,W(1,110))    
      CALL JIOXXX(W(1,110),W(1,82),GG,ZERO,ZERO,W(1,111))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,111),GG,AMP(77))             
      CALL HIOCXX(W(1,81),W(1,3),GH1NIJ,MH1,WH1,W(1,112))                                                          
      CALL FSIXXX(W(1,2),W(1,112),GH1BB,BMASS,ZERO,W(1, 113))                                                          
      CALL JIOXXX(W(1,113),W(1,82),GG,ZERO,ZERO,W(1,114))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,114),GG,AMP(78))             
      CALL HIOCXX(W(1,81),W(1,3),GH2NIJ,MH2,WH2,W(1,115))                                                          
      CALL FSIXXX(W(1,2),W(1,115),GH2BB,BMASS,ZERO,W(1, 116))                                                          
      CALL JIOXXX(W(1,116),W(1,82),GG,ZERO,ZERO,W(1,117))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,117),GG,AMP(79))             
      CALL HIOCXX(W(1,81),W(1,3),GH3NIJ,MH3,WH3,W(1,118))                                                          
      CALL FSIXXX(W(1,2),W(1,118),GH3BB,BMASS,ZERO,W(1, 119))                                                          
      CALL JIOXXX(W(1,119),W(1,82),GG,ZERO,ZERO,W(1,120))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,120),GG,AMP(80))             
      CALL IOVXXX(W(1,6),W(1,106),W(1,109),GZD,AMP(81))            
      CALL IOSXXX(W(1,6),W(1,106),W(1,112),GH1BB,AMP(82))          
      CALL IOSXXX(W(1,6),W(1,106),W(1,115),GH2BB,AMP(83))          
      CALL IOSXXX(W(1,6),W(1,106),W(1,118),GH3BB,AMP(84))          
      CALL FVOXXX(W(1,1),W(1,109),GZD,BMASS,ZERO,W(1,121))    
      CALL IOVXXX(W(1,6),W(1,121),W(1,97),GG,AMP(85))             
      CALL FSOXXX(W(1,1),W(1,112),GH1BB,BMASS,ZERO,W(1, 122))                                                          
      CALL IOVXXX(W(1,6),W(1,122),W(1,97),GG,AMP(86))             
      CALL FSOXXX(W(1,1),W(1,115),GH2BB,BMASS,ZERO,W(1, 123))                                                          
      CALL IOVXXX(W(1,6),W(1,123),W(1,97),GG,AMP(87))             
      CALL FSOXXX(W(1,1),W(1,118),GH3BB,BMASS,ZERO,W(1, 124))                                                          
      CALL IOVXXX(W(1,6),W(1,124),W(1,97),GG,AMP(88))             
      CALL FVOXXX(W(1,82),W(1,109),GZD,BMASS,ZERO,W(1,125))    
      CALL JIOXXX(W(1,2),W(1,125),GG,ZERO,ZERO,W(1,126))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,126),GG,AMP(89))             
      CALL FSOXXX(W(1,82),W(1,112),GH1BB,BMASS,ZERO,W(1, 127))                                                          
      CALL JIOXXX(W(1,2),W(1,127),GG,ZERO,ZERO,W(1,128))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,128),GG,AMP(90))             
      CALL FSOXXX(W(1,82),W(1,115),GH2BB,BMASS,ZERO,W(1, 129))                                                          
      CALL JIOXXX(W(1,2),W(1,129),GG,ZERO,ZERO,W(1,130))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,130),GG,AMP(91))             
      CALL FSOXXX(W(1,82),W(1,118),GH3BB,BMASS,ZERO,W(1, 131))                                                          
      CALL JIOXXX(W(1,2),W(1,131),GG,ZERO,ZERO,W(1,132))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,132),GG,AMP(92))             
      CALL JIOXXX(W(1,110),W(1,1),GG,ZERO,ZERO,W(1,133))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,133),GG,AMP(93))             
      CALL JIOXXX(W(1,113),W(1,1),GG,ZERO,ZERO,W(1,134))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,134),GG,AMP(94))             
      CALL JIOXXX(W(1,116),W(1,1),GG,ZERO,ZERO,W(1,135))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,135),GG,AMP(95))             
      CALL JIOXXX(W(1,119),W(1,1),GG,ZERO,ZERO,W(1,136))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,136),GG,AMP(96))             
      CALL JIOXXX(W(1,2),W(1,121),GG,ZERO,ZERO,W(1,137))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,137),GG,AMP(97))             
      CALL JIOXXX(W(1,2),W(1,122),GG,ZERO,ZERO,W(1,138))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,138),GG,AMP(98))             
      CALL JIOXXX(W(1,2),W(1,123),GG,ZERO,ZERO,W(1,139))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,139),GG,AMP(99))             
      CALL JIOXXX(W(1,2),W(1,124),GG,ZERO,ZERO,W(1,140))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,140),GG,AMP(100))             
      CALL IOVXXX(W(1,6),W(1,125),W(1,31),GG,AMP(101))             
      CALL IOVXXX(W(1,6),W(1,127),W(1,31),GG,AMP(102))             
      CALL IOVXXX(W(1,6),W(1,129),W(1,31),GG,AMP(103))             
      CALL IOVXXX(W(1,6),W(1,131),W(1,31),GG,AMP(104))             
      CALL FVOXXX(W(1,82),W(1,31),GG,BMASS,ZERO,W(1,141))     
      CALL IOVXXX(W(1,6),W(1,141),W(1,109),GZD,AMP(105))            
      CALL IOSXXX(W(1,6),W(1,141),W(1,112),GH1BB,AMP(106))          
      CALL IOSXXX(W(1,6),W(1,141),W(1,115),GH2BB,AMP(107))          
      CALL IOSXXX(W(1,6),W(1,141),W(1,118),GH3BB,AMP(108))          
      CALL OXXXXX(P(0,6),BMASS,NHEL(6),+1*IC(6),W(1,142))       
      CALL IOSCXX(W(1,24),W(1,142),W(1,87),GB1GOM,AMP(109))        
      CALL IOSCXX(W(1,28),W(1,142),W(1,87),GB1GOM,AMP(110))        
      CALL IOSCXX(W(1,24),W(1,142),W(1,90),GB2GOM,AMP(111))        
      CALL IOSCXX(W(1,28),W(1,142),W(1,90),GB2GOM,AMP(112))        
      CALL IOSCXX(W(1,100),W(1,142),W(1,23),GB1GOM,AMP(113))        
      CALL IOSCXX(W(1,100),W(1,142),W(1,27),GB2GOM,AMP(114))        
      CALL IOSCXX(W(1,103),W(1,142),W(1,23),GB1GOM,AMP(115))        
      CALL IOSCXX(W(1,103),W(1,142),W(1,27),GB2GOM,AMP(116))        
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,143))         
      CALL FSIXXX(W(1,143),W(1,49),GB1NIP,BMASS,ZERO,W(1,144))                                                          
      CALL JIOXXX(W(1,144),W(1,1),GG,ZERO,ZERO,W(1,145))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,145),GG,AMP(117))             
      CALL FSIXXX(W(1,143),W(1,52),GB2NIP,BMASS,ZERO,W(1,146))                                                          
      CALL JIOXXX(W(1,146),W(1,1),GG,ZERO,ZERO,W(1,147))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,147),GG,AMP(118))             
      CALL HIOXXX(W(1,143),W(1,1),GB1NIP,MBL,WBL,W(1,148))                                                          
      CALL FSOXXX(W(1,4),W(1,148),GB1NJM,BMASS,ZERO,W(1,149))                                                          
      CALL JIOXXX(W(1,2),W(1,149),GG,ZERO,ZERO,W(1,150))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,150),GG,AMP(119))             
      CALL HIOXXX(W(1,143),W(1,1),GB2NIP,MBR,WBR,W(1,151))                                                          
      CALL FSOXXX(W(1,4),W(1,151),GB2NJM,BMASS,ZERO,W(1,152))                                                          
      CALL JIOXXX(W(1,2),W(1,152),GG,ZERO,ZERO,W(1,153))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,153),GG,AMP(120))             
      CALL JSSXXX(W(1,148),W(1,49),GC,ZERO,ZERO,W(1,154))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,154),GG,AMP(121))             
      CALL JSSXXX(W(1,151),W(1,52),GC,ZERO,ZERO,W(1,155))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,155),GG,AMP(122))             
      CALL FSOXXX(W(1,82),W(1,49),GB1GOP,MGO,WGO,W(1,156))                                                          
      CALL IOSXXX(W(1,6),W(1,156),W(1,148),GB1GOM,AMP(123))         
      CALL FSOXXX(W(1,82),W(1,52),GB2GOP,MGO,WGO,W(1,157))                                                          
      CALL IOSXXX(W(1,6),W(1,157),W(1,148),GB1GOM,AMP(124))         
      CALL IOSXXX(W(1,6),W(1,156),W(1,151),GB2GOM,AMP(125))         
      CALL IOSXXX(W(1,6),W(1,157),W(1,151),GB2GOM,AMP(126))         
      CALL HVSXXX(W(1,97),W(1,148),GC,MBL,WBL,W(1,158))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,158),GB1NJM,AMP(127))         
      CALL HVSXXX(W(1,97),W(1,151),GC,MBR,WBR,W(1,159))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,159),GB2NJM,AMP(128))         
      CALL IOVXXX(W(1,6),W(1,149),W(1,97),GG,AMP(129))             
      CALL IOVXXX(W(1,6),W(1,152),W(1,97),GG,AMP(130))             
      CALL FSIXXX(W(1,2),W(1,148),GB1GOM,MGO,WGO,W(1,160))                                                          
      CALL HIOXXX(W(1,160),W(1,82),GB1GOP,MBL,WBL,W(1,161))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,161),GB1NJM,AMP(131))         
      CALL HIOXXX(W(1,160),W(1,82),GB2GOP,MBR,WBR,W(1,162))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,162),GB2NJM,AMP(132))         
      CALL FSIXXX(W(1,2),W(1,151),GB2GOM,MGO,WGO,W(1,163))                                                          
      CALL HIOXXX(W(1,163),W(1,82),GB1GOP,MBL,WBL,W(1,164))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,164),GB1NJM,AMP(133))         
      CALL HIOXXX(W(1,163),W(1,82),GB2GOP,MBR,WBR,W(1,165))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,165),GB2NJM,AMP(134))         
      CALL HIOXXX(W(1,143),W(1,106),GB1NIP,MBL,WBL,W(1,166))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,166),GB1NJM,AMP(135))         
      CALL HIOXXX(W(1,143),W(1,106),GB2NIP,MBR,WBR,W(1,167))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,167),GB2NJM,AMP(136))         
      CALL IOSCXX(W(1,71),W(1,142),W(1,148),GB1GOM,AMP(137))        
      CALL IOSCXX(W(1,74),W(1,142),W(1,148),GB1GOM,AMP(138))        
      CALL IOSCXX(W(1,71),W(1,142),W(1,151),GB2GOM,AMP(139))        
      CALL IOSCXX(W(1,74),W(1,142),W(1,151),GB2GOM,AMP(140))        
      CALL IOSCXX(W(1,160),W(1,142),W(1,13),GB1GOM,AMP(141))        
      CALL IOSCXX(W(1,160),W(1,142),W(1,15),GB2GOM,AMP(142))        
      CALL IOSCXX(W(1,163),W(1,142),W(1,13),GB1GOM,AMP(143))        
      CALL IOSCXX(W(1,163),W(1,142),W(1,15),GB2GOM,AMP(144))        
      JAMP(   1) = +AMP(   1)+AMP(   2)+AMP(   3)+AMP(   4)+AMP(   9)
     &             +AMP(  10)+AMP(  11)+AMP(  12)+AMP(  13)+AMP(  14)
     &             -AMP(  25)-AMP(  26)-AMP(  27)-AMP(  28)-AMP(  29)
     &             -AMP(  30)-AMP(  31)-AMP(  32)-AMP(  37)-AMP(  38)
     &             +AMP(  43)+AMP(  44)+AMP(  45)+AMP(  46)-AMP(  47)
     &             -AMP(  48)-AMP(  49)-AMP(  50)+AMP(  63)+AMP(  64)
     &             +AMP(  65)+AMP(  66)-AMP(  67)-AMP(  68)-AMP(  69)
     &             -AMP(  70)-AMP(  75)-AMP(  76)+AMP(  77)+AMP(  78)
     &             +AMP(  79)+AMP(  80)+AMP(  81)+AMP(  82)+AMP(  83)
     &             +AMP(  84)+AMP(  85)+AMP(  86)+AMP(  87)+AMP(  88)
     &             +AMP(  89)+AMP(  90)+AMP(  91)+AMP(  92)+AMP( 109)
     &             +AMP( 110)+AMP( 111)+AMP( 112)-AMP( 123)-AMP( 124)
     &             -AMP( 125)-AMP( 126)+AMP( 127)+AMP( 128)+AMP( 129)
     &             +AMP( 130)+AMP( 135)+AMP( 136)-AMP( 137)-AMP( 138)
     &             -AMP( 139)-AMP( 140)
      JAMP(   2) = +AMP(   5)+AMP(   6)+AMP(   7)+AMP(   8)+AMP(  15)
     &             +AMP(  16)+AMP(  17)+AMP(  18)+AMP(  19)+AMP(  20)
     &             -AMP(  21)-AMP(  22)-AMP(  23)-AMP(  24)-AMP(  33)
     &             -AMP(  34)-AMP(  35)-AMP(  36)+AMP(  39)+AMP(  40)
     &             +AMP(  41)+AMP(  42)-AMP(  51)-AMP(  52)-AMP(  53)
     &             -AMP(  54)-AMP(  55)-AMP(  56)+AMP(  57)+AMP(  58)
     &             +AMP(  59)+AMP(  60)+AMP(  61)+AMP(  62)-AMP(  71)
     &             -AMP(  72)-AMP(  73)-AMP(  74)-AMP(  93)-AMP(  94)
     &             -AMP(  95)-AMP(  96)-AMP(  97)-AMP(  98)-AMP(  99)
     &             -AMP( 100)-AMP( 101)-AMP( 102)-AMP( 103)-AMP( 104)
     &             -AMP( 105)-AMP( 106)-AMP( 107)-AMP( 108)+AMP( 113)
     &             +AMP( 114)+AMP( 115)+AMP( 116)-AMP( 117)-AMP( 118)
     &             -AMP( 119)-AMP( 120)-AMP( 121)-AMP( 122)+AMP( 131)
     &             +AMP( 132)+AMP( 133)+AMP( 134)-AMP( 141)-AMP( 142)
     &             -AMP( 143)-AMP( 144)
      MATRIX_BXB_NINJBBX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_BXB_NINJBBX =MATRIX_BXB_NINJBBX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       