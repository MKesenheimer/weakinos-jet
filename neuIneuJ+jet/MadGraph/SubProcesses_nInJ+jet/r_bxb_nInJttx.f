      SUBROUTINE SMATRIX_BXB_NINJTTX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : b~ b -> n1 n2 t t~  
C  
C Crossing   1 is b~ b -> n1 n2 t t~  
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
      REAL*8 MATRIX_BXB_NINJTTX
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
      common/to_Ramps_bxb_nInJttx/  amp2,       jamp2

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
      DATA NGRAPHS /   72/          
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
                 T=MATRIX_BXB_NINJTTX(P,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_BXB_NINJTTX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : b~ b -> n1 n2 t t~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS=  72,NEIGEN=  1) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS= 105, NCOLOR=   1) 
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
      common/to_Ramps_bxb_nInJttx/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            1/                                       
      DATA(CF(i,1),i=1,1) /     2/                                  
C               T[ 5, 2]T[ 1, 6]                                           
C ----------
C BEGIN CODE
C ----------
      CALL OXXXXX(P(0,1),BMASS,NHEL(1),-1*IC(1),W(1,1))       
      CALL IXXXXX(P(0,2),BMASS,NHEL(2),+1*IC(2),W(1,2))       
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),TMASS,NHEL(5),-1*IC(5),W(1,5))       
      CALL IXXXXX(P(0,6),TMASS,NHEL(6),-1*IC(6),W(1,6))       
      CALL HIOXXX(W(1,2),W(1,3),GB1NIM,MBL,WBL,W(1,7))                                                          
      CALL FSOXXX(W(1,1),W(1,7),GB1GOP,MGO,WGO,W(1,8))                                                          
      CALL HIOCXX(W(1,5),W(1,8),GT1GOP,MTL,WTL,W(1,9))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,9),GT1NJM,AMP(1))         
      CALL HIOCXX(W(1,5),W(1,8),GT2GOP,MTR,WTR,W(1,10))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,10),GT2NJM,AMP(2))         
      CALL HIOXXX(W(1,2),W(1,3),GB2NIM,MBR,WBR,W(1,11))                                                          
      CALL FSOXXX(W(1,1),W(1,11),GB2GOP,MGO,WGO,W(1,12))                                                          
      CALL HIOCXX(W(1,5),W(1,12),GT1GOP,MTL,WTL,W(1,13))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,13),GT1NJM,AMP(3))         
      CALL HIOCXX(W(1,5),W(1,12),GT2GOP,MTR,WTR,W(1,14))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,14),GT2NJM,AMP(4))         
      CALL HIOCXX(W(1,5),W(1,4),GT1NJP,MTL,WTL,W(1,15))                                                          
      CALL IOSXXX(W(1,6),W(1,8),W(1,15),GT1GOM,AMP(5))         
      CALL HIOCXX(W(1,5),W(1,4),GT2NJP,MTR,WTR,W(1,16))                                                          
      CALL IOSXXX(W(1,6),W(1,8),W(1,16),GT2GOM,AMP(6))         
      CALL IOSXXX(W(1,6),W(1,12),W(1,15),GT1GOM,AMP(7))         
      CALL IOSXXX(W(1,6),W(1,12),W(1,16),GT2GOM,AMP(8))         
      CALL HIOXXX(W(1,2),W(1,4),GB1NJM,MBL,WBL,W(1,17))                                                          
      CALL FSOXXX(W(1,1),W(1,17),GB1GOP,MGO,WGO,W(1,18))                                                          
      CALL HIOCXX(W(1,5),W(1,18),GT1GOP,MTL,WTL,W(1,19))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,19),GT1NIM,AMP(9))         
      CALL HIOCXX(W(1,5),W(1,18),GT2GOP,MTR,WTR,W(1,20))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,20),GT2NIM,AMP(10))         
      CALL HIOXXX(W(1,2),W(1,4),GB2NJM,MBR,WBR,W(1,21))                                                          
      CALL FSOXXX(W(1,1),W(1,21),GB2GOP,MGO,WGO,W(1,22))                                                          
      CALL HIOCXX(W(1,5),W(1,22),GT1GOP,MTL,WTL,W(1,23))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,23),GT1NIM,AMP(11))         
      CALL HIOCXX(W(1,5),W(1,22),GT2GOP,MTR,WTR,W(1,24))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,24),GT2NIM,AMP(12))         
      CALL HIOCXX(W(1,5),W(1,3),GT1NIP,MTL,WTL,W(1,25))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,25),GT1GOM,AMP(13))         
      CALL HIOCXX(W(1,5),W(1,3),GT2NIP,MTR,WTR,W(1,26))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,26),GT2GOM,AMP(14))         
      CALL IOSXXX(W(1,6),W(1,22),W(1,25),GT1GOM,AMP(15))         
      CALL IOSXXX(W(1,6),W(1,22),W(1,26),GT2GOM,AMP(16))         
      CALL JIOXXX(W(1,2),W(1,1),GG,ZERO,ZERO,W(1,27))     
      CALL HVSXXX(W(1,27),W(1,25),GC,MTL,WTL,W(1,28))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,28),GT1NJM,AMP(17))         
      CALL HVSXXX(W(1,27),W(1,26),GC,MTR,WTR,W(1,29))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,29),GT2NJM,AMP(18))         
      CALL FSOXXX(W(1,4),W(1,25),GT1NJM,TMASS,TWIDTH,W(1,30))                                                          
      CALL IOVXXX(W(1,6),W(1,30),W(1,27),GG,AMP(19))             
      CALL FSOXXX(W(1,4),W(1,26),GT2NJM,TMASS,TWIDTH,W(1,31))                                                          
      CALL IOVXXX(W(1,6),W(1,31),W(1,27),GG,AMP(20))             
      CALL FVICXX(W(1,5),W(1,27),GG,TMASS,TWIDTH,W(1,32))    
      CALL HIOCXX(W(1,32),W(1,3),GT1NIP,MTL,WTL,W(1,33))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,33),GT1NJM,AMP(21))         
      CALL HIOCXX(W(1,32),W(1,3),GT2NIP,MTR,WTR,W(1,34))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,34),GT2NJM,AMP(22))         
      CALL HVSXXX(W(1,27),W(1,15),GC,MTL,WTL,W(1,35))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,35),GT1NIM,AMP(23))         
      CALL HVSXXX(W(1,27),W(1,16),GC,MTR,WTR,W(1,36))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,36),GT2NIM,AMP(24))         
      CALL FSOXXX(W(1,3),W(1,15),GT1NIM,TMASS,TWIDTH,W(1,37))                                                          
      CALL IOVXXX(W(1,6),W(1,37),W(1,27),GG,AMP(25))             
      CALL FSOXXX(W(1,3),W(1,16),GT2NIM,TMASS,TWIDTH,W(1,38))                                                          
      CALL IOVXXX(W(1,6),W(1,38),W(1,27),GG,AMP(26))             
      CALL HIOCXX(W(1,32),W(1,4),GT1NJP,MTL,WTL,W(1,39))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,39),GT1NIM,AMP(27))         
      CALL HIOCXX(W(1,32),W(1,4),GT2NJP,MTR,WTR,W(1,40))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,40),GT2NIM,AMP(28))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,41))         
      CALL OXXXXX(P(0,5),TMASS,NHEL(5),+1*IC(5),W(1,42))       
      CALL FSIXXX(W(1,41),W(1,7),GB1NJP,BMASS,ZERO,W(1,43))                                                          
      CALL JIOXXX(W(1,43),W(1,1),GG,ZERO,ZERO,W(1,44))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,44),GG,AMP(29))             
      CALL FSIXXX(W(1,41),W(1,11),GB2NJP,BMASS,ZERO,W(1,45))                                                          
      CALL JIOXXX(W(1,45),W(1,1),GG,ZERO,ZERO,W(1,46))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,46),GG,AMP(30))             
      CALL HIOXXX(W(1,41),W(1,1),GB1NJP,MBL,WBL,W(1,47))                                                          
      CALL FSOXXX(W(1,3),W(1,47),GB1NIM,BMASS,ZERO,W(1,48))                                                          
      CALL JIOXXX(W(1,2),W(1,48),GG,ZERO,ZERO,W(1,49))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,49),GG,AMP(31))             
      CALL HIOXXX(W(1,41),W(1,1),GB2NJP,MBR,WBR,W(1,50))                                                          
      CALL FSOXXX(W(1,3),W(1,50),GB2NIM,BMASS,ZERO,W(1,51))                                                          
      CALL JIOXXX(W(1,2),W(1,51),GG,ZERO,ZERO,W(1,52))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,52),GG,AMP(32))             
      CALL JSSXXX(W(1,47),W(1,7),GC,ZERO,ZERO,W(1,53))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,53),GG,AMP(33))             
      CALL JSSXXX(W(1,50),W(1,11),GC,ZERO,ZERO,W(1,54))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,54),GG,AMP(34))             
      CALL FSIXXX(W(1,2),W(1,47),GB1GOM,MGO,WGO,W(1,55))                                                          
      CALL HIOXXX(W(1,55),W(1,42),GT1GOP,MTL,WTL,W(1,56))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,56),GT1NIM,AMP(35))         
      CALL HIOXXX(W(1,55),W(1,42),GT2GOP,MTR,WTR,W(1,57))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,57),GT2NIM,AMP(36))         
      CALL FSIXXX(W(1,2),W(1,50),GB2GOM,MGO,WGO,W(1,58))                                                          
      CALL HIOXXX(W(1,58),W(1,42),GT1GOP,MTL,WTL,W(1,59))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,59),GT1NIM,AMP(37))         
      CALL HIOXXX(W(1,58),W(1,42),GT2GOP,MTR,WTR,W(1,60))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,60),GT2NIM,AMP(38))         
      CALL JIOCXX(W(1,41),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,61))                                                          
      CALL FVIXXX(W(1,2),W(1,61),GZD,BMASS,ZERO,W(1,62))    
      CALL JIOXXX(W(1,62),W(1,1),GG,ZERO,ZERO,W(1,63))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,63),GG,AMP(39))             
      CALL HIOCXX(W(1,41),W(1,3),GH1NIJ,MH1,WH1,W(1,64))                                                          
      CALL FSIXXX(W(1,2),W(1,64),GH1BB,BMASS,ZERO,W(1, 65))                                                          
      CALL JIOXXX(W(1,65),W(1,1),GG,ZERO,ZERO,W(1,66))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,66),GG,AMP(40))             
      CALL HIOCXX(W(1,41),W(1,3),GH2NIJ,MH2,WH2,W(1,67))                                                          
      CALL FSIXXX(W(1,2),W(1,67),GH2BB,BMASS,ZERO,W(1, 68))                                                          
      CALL JIOXXX(W(1,68),W(1,1),GG,ZERO,ZERO,W(1,69))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,69),GG,AMP(41))             
      CALL HIOCXX(W(1,41),W(1,3),GH3NIJ,MH3,WH3,W(1,70))                                                          
      CALL FSIXXX(W(1,2),W(1,70),GH3BB,BMASS,ZERO,W(1, 71))                                                          
      CALL JIOXXX(W(1,71),W(1,1),GG,ZERO,ZERO,W(1,72))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,72),GG,AMP(42))             
      CALL FVOXXX(W(1,1),W(1,61),GZD,BMASS,ZERO,W(1,73))    
      CALL JIOXXX(W(1,2),W(1,73),GG,ZERO,ZERO,W(1,74))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,74),GG,AMP(43))             
      CALL FSOXXX(W(1,1),W(1,64),GH1BB,BMASS,ZERO,W(1, 75))                                                          
      CALL JIOXXX(W(1,2),W(1,75),GG,ZERO,ZERO,W(1,76))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,76),GG,AMP(44))             
      CALL FSOXXX(W(1,1),W(1,67),GH2BB,BMASS,ZERO,W(1, 77))                                                          
      CALL JIOXXX(W(1,2),W(1,77),GG,ZERO,ZERO,W(1,78))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,78),GG,AMP(45))             
      CALL FSOXXX(W(1,1),W(1,70),GH3BB,BMASS,ZERO,W(1, 79))                                                          
      CALL JIOXXX(W(1,2),W(1,79),GG,ZERO,ZERO,W(1,80))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,80),GG,AMP(46))             
      CALL FVOXXX(W(1,42),W(1,61),GZU,TMASS,TWIDTH,W(1,81))    
      CALL IOVXXX(W(1,6),W(1,81),W(1,27),GG,AMP(47))             
      CALL FSOXXX(W(1,42),W(1,64),GH1TT,TMASS,TWIDTH,W(1, 82))                                                          
      CALL IOVXXX(W(1,6),W(1,82),W(1,27),GG,AMP(48))             
      CALL FSOXXX(W(1,42),W(1,67),GH2TT,TMASS,TWIDTH,W(1, 83))                                                          
      CALL IOVXXX(W(1,6),W(1,83),W(1,27),GG,AMP(49))             
      CALL FSOXXX(W(1,42),W(1,70),GH3TT,TMASS,TWIDTH,W(1, 84))                                                          
      CALL IOVXXX(W(1,6),W(1,84),W(1,27),GG,AMP(50))             
      CALL FVOXXX(W(1,42),W(1,27),GG,TMASS,TWIDTH,W(1,85))     
      CALL IOVXXX(W(1,6),W(1,85),W(1,61),GZU,AMP(51))            
      CALL IOSXXX(W(1,6),W(1,85),W(1,64),GH1TT,AMP(52))          
      CALL IOSXXX(W(1,6),W(1,85),W(1,67),GH2TT,AMP(53))          
      CALL IOSXXX(W(1,6),W(1,85),W(1,70),GH3TT,AMP(54))          
      CALL OXXXXX(P(0,6),TMASS,NHEL(6),+1*IC(6),W(1,86))       
      CALL IOSCXX(W(1,55),W(1,86),W(1,25),GT1GOM,AMP(55))        
      CALL IOSCXX(W(1,55),W(1,86),W(1,26),GT2GOM,AMP(56))        
      CALL IOSCXX(W(1,58),W(1,86),W(1,25),GT1GOM,AMP(57))        
      CALL IOSCXX(W(1,58),W(1,86),W(1,26),GT2GOM,AMP(58))        
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,87))         
      CALL FSIXXX(W(1,87),W(1,17),GB1NIP,BMASS,ZERO,W(1,88))                                                          
      CALL JIOXXX(W(1,88),W(1,1),GG,ZERO,ZERO,W(1,89))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,89),GG,AMP(59))             
      CALL FSIXXX(W(1,87),W(1,21),GB2NIP,BMASS,ZERO,W(1,90))                                                          
      CALL JIOXXX(W(1,90),W(1,1),GG,ZERO,ZERO,W(1,91))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,91),GG,AMP(60))             
      CALL HIOXXX(W(1,87),W(1,1),GB1NIP,MBL,WBL,W(1,92))                                                          
      CALL FSOXXX(W(1,4),W(1,92),GB1NJM,BMASS,ZERO,W(1,93))                                                          
      CALL JIOXXX(W(1,2),W(1,93),GG,ZERO,ZERO,W(1,94))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,94),GG,AMP(61))             
      CALL HIOXXX(W(1,87),W(1,1),GB2NIP,MBR,WBR,W(1,95))                                                          
      CALL FSOXXX(W(1,4),W(1,95),GB2NJM,BMASS,ZERO,W(1,96))                                                          
      CALL JIOXXX(W(1,2),W(1,96),GG,ZERO,ZERO,W(1,97))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,97),GG,AMP(62))             
      CALL JSSXXX(W(1,92),W(1,17),GC,ZERO,ZERO,W(1,98))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,98),GG,AMP(63))             
      CALL JSSXXX(W(1,95),W(1,21),GC,ZERO,ZERO,W(1,99))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,99),GG,AMP(64))             
      CALL FSIXXX(W(1,2),W(1,92),GB1GOM,MGO,WGO,W(1,100))                                                          
      CALL HIOXXX(W(1,100),W(1,42),GT1GOP,MTL,WTL,W(1,101))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,101),GT1NJM,AMP(65))         
      CALL HIOXXX(W(1,100),W(1,42),GT2GOP,MTR,WTR,W(1,102))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,102),GT2NJM,AMP(66))         
      CALL FSIXXX(W(1,2),W(1,95),GB2GOM,MGO,WGO,W(1,103))                                                          
      CALL HIOXXX(W(1,103),W(1,42),GT1GOP,MTL,WTL,W(1,104))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,104),GT1NJM,AMP(67))         
      CALL HIOXXX(W(1,103),W(1,42),GT2GOP,MTR,WTR,W(1,105))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,105),GT2NJM,AMP(68))         
      CALL IOSCXX(W(1,100),W(1,86),W(1,15),GT1GOM,AMP(69))        
      CALL IOSCXX(W(1,100),W(1,86),W(1,16),GT2GOM,AMP(70))        
      CALL IOSCXX(W(1,103),W(1,86),W(1,15),GT1GOM,AMP(71))        
      CALL IOSCXX(W(1,103),W(1,86),W(1,16),GT2GOM,AMP(72))        
      JAMP(   1) = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)+AMP(   5)
     &             +AMP(   6)+AMP(   7)+AMP(   8)+AMP(   9)+AMP(  10)
     &             +AMP(  11)+AMP(  12)-AMP(  13)-AMP(  14)-AMP(  15)
     &             -AMP(  16)+AMP(  17)+AMP(  18)+AMP(  19)+AMP(  20)
     &             +AMP(  21)+AMP(  22)-AMP(  23)-AMP(  24)-AMP(  25)
     &             -AMP(  26)-AMP(  27)-AMP(  28)+AMP(  29)+AMP(  30)
     &             +AMP(  31)+AMP(  32)+AMP(  33)+AMP(  34)-AMP(  35)
     &             -AMP(  36)-AMP(  37)-AMP(  38)-AMP(  39)-AMP(  40)
     &             -AMP(  41)-AMP(  42)-AMP(  43)-AMP(  44)-AMP(  45)
     &             -AMP(  46)-AMP(  47)-AMP(  48)-AMP(  49)-AMP(  50)
     &             -AMP(  51)-AMP(  52)-AMP(  53)-AMP(  54)+AMP(  55)
     &             +AMP(  56)+AMP(  57)+AMP(  58)-AMP(  59)-AMP(  60)
     &             -AMP(  61)-AMP(  62)-AMP(  63)-AMP(  64)+AMP(  65)
     &             +AMP(  66)+AMP(  67)+AMP(  68)-AMP(  69)-AMP(  70)
     &             -AMP(  71)-AMP(  72)
      MATRIX_BXB_NINJTTX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_BXB_NINJTTX =MATRIX_BXB_NINJTTX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
