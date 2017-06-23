      SUBROUTINE SMATRIX_BXB_NIXJBTX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : b~ b -> n1 x2+ b t~  
C  
C Crossing   1 is b~ b -> n1 x2+ b t~  
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
      REAL*8 MATRIX_BXB_NIXJBTX
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
      common/to_Ramps_bxb_nIxJbtx/  amp2,       jamp2

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
      DATA NGRAPHS /   64/          
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
                 T=MATRIX_BXB_NIXJBTX(P,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_BXB_NIXJBTX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : b~ b -> n1 x2+ b t~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS=  64,NEIGEN=  2) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS=  80, NCOLOR=   2) 
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
      common/to_Ramps_bxb_nIxJbtx/  amp2,       jamp2
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
      CALL IXXXXX(P(0,4),MXJ,NHEL(4),-1*IC(4),W(1,4))         
      CALL OXXXXX(P(0,5),BMASS,NHEL(5),+1*IC(5),W(1,5))       
      CALL IXXXXX(P(0,6),TMASS,NHEL(6),-1*IC(6),W(1,6))       
      CALL HIOXXX(W(1,4),W(1,1),GT1XJP,MTL,WTL,W(1,7))                                                          
      CALL JIOXXX(W(1,2),W(1,5),GG,ZERO,ZERO,W(1,8))     
      CALL HVSXXX(W(1,8),W(1,7),GC,MTL,WTL,W(1,9))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,9),GT1NIM,AMP(1))         
      CALL HIOXXX(W(1,4),W(1,1),GT2XJP,MTR,WTR,W(1,10))                                                          
      CALL HVSXXX(W(1,8),W(1,10),GC,MTR,WTR,W(1,11))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,11),GT2NIM,AMP(2))         
      CALL FSOXXX(W(1,3),W(1,7),GT1NIM,TMASS,TWIDTH,W(1,12))                                                          
      CALL IOVXXX(W(1,6),W(1,12),W(1,8),GG,AMP(3))             
      CALL FSOXXX(W(1,3),W(1,10),GT2NIM,TMASS,TWIDTH,W(1,13))                                                          
      CALL IOVXXX(W(1,6),W(1,13),W(1,8),GG,AMP(4))             
      CALL FVOXXX(W(1,1),W(1,8),GG,BMASS,ZERO,W(1,14))     
      CALL HIOXXX(W(1,4),W(1,14),GT1XJP,MTL,WTL,W(1,15))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,15),GT1NIM,AMP(5))         
      CALL HIOXXX(W(1,4),W(1,14),GT2XJP,MTR,WTR,W(1,16))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,16),GT2NIM,AMP(6))         
      CALL HIOXXX(W(1,2),W(1,3),GB1NIM,MBL,WBL,W(1,17))                                                          
      CALL FSOXXX(W(1,1),W(1,17),GB1GOP,MGO,WGO,W(1,18))                                                          
      CALL HIOXXX(W(1,4),W(1,5),GT1XJP,MTL,WTL,W(1,19))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,19),GT1GOM,AMP(7))         
      CALL HIOXXX(W(1,4),W(1,5),GT2XJP,MTR,WTR,W(1,20))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,20),GT2GOM,AMP(8))         
      CALL HIOXXX(W(1,2),W(1,3),GB2NIM,MBR,WBR,W(1,21))                                                          
      CALL FSOXXX(W(1,1),W(1,21),GB2GOP,MGO,WGO,W(1,22))                                                          
      CALL IOSXXX(W(1,6),W(1,22),W(1,19),GT1GOM,AMP(9))         
      CALL IOSXXX(W(1,6),W(1,22),W(1,20),GT2GOM,AMP(10))         
      CALL FSOXXX(W(1,5),W(1,17),GB1GOP,MGO,WGO,W(1,23))                                                          
      CALL IOSXXX(W(1,6),W(1,23),W(1,7),GT1GOM,AMP(11))         
      CALL IOSXXX(W(1,6),W(1,23),W(1,10),GT2GOM,AMP(12))         
      CALL FSOXXX(W(1,5),W(1,21),GB2GOP,MGO,WGO,W(1,24))                                                          
      CALL IOSXXX(W(1,6),W(1,24),W(1,7),GT1GOM,AMP(13))         
      CALL IOSXXX(W(1,6),W(1,24),W(1,10),GT2GOM,AMP(14))         
      CALL JIOXXX(W(1,4),W(1,3),GWXJNI,WMASS,WWIDTH,W(1,25))                                                          
      CALL IOVXXX(W(1,6),W(1,14),W(1,25),GWF,AMP(15))            
      CALL HIOXXX(W(1,4),W(1,3),GHXJNI,MHC,WHC,W(1,26))                                                          
      CALL IOSXXX(W(1,6),W(1,14),W(1,26),GHPQ,AMP(16))           
      CALL FVOXXX(W(1,1),W(1,25),GWF,TMASS,TWIDTH,W(1,27))    
      CALL IOVXXX(W(1,6),W(1,27),W(1,8),GG,AMP(17))             
      CALL FSOXXX(W(1,1),W(1,26),GHPQ,TMASS,TWIDTH,W(1,  28))                                                          
      CALL IOVXXX(W(1,6),W(1,28),W(1,8),GG,AMP(18))             
      CALL JIOXXX(W(1,2),W(1,1),GG,ZERO,ZERO,W(1,29))     
      CALL HVSXXX(W(1,29),W(1,19),GC,MTL,WTL,W(1,30))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,30),GT1NIM,AMP(19))         
      CALL HVSXXX(W(1,29),W(1,20),GC,MTR,WTR,W(1,31))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,31),GT2NIM,AMP(20))         
      CALL FSOXXX(W(1,3),W(1,19),GT1NIM,TMASS,TWIDTH,W(1,32))                                                          
      CALL IOVXXX(W(1,6),W(1,32),W(1,29),GG,AMP(21))             
      CALL FSOXXX(W(1,3),W(1,20),GT2NIM,TMASS,TWIDTH,W(1,33))                                                          
      CALL IOVXXX(W(1,6),W(1,33),W(1,29),GG,AMP(22))             
      CALL FVOXXX(W(1,5),W(1,29),GG,BMASS,ZERO,W(1,34))     
      CALL HIOXXX(W(1,4),W(1,34),GT1XJP,MTL,WTL,W(1,35))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,35),GT1NIM,AMP(23))         
      CALL HIOXXX(W(1,4),W(1,34),GT2XJP,MTR,WTR,W(1,36))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,36),GT2NIM,AMP(24))         
      CALL FVOXXX(W(1,5),W(1,25),GWF,TMASS,TWIDTH,W(1,37))    
      CALL IOVXXX(W(1,6),W(1,37),W(1,29),GG,AMP(25))             
      CALL FSOXXX(W(1,5),W(1,26),GHPQ,TMASS,TWIDTH,W(1,  38))                                                          
      CALL IOVXXX(W(1,6),W(1,38),W(1,29),GG,AMP(26))             
      CALL IOVXXX(W(1,6),W(1,34),W(1,25),GWF,AMP(27))            
      CALL IOSXXX(W(1,6),W(1,34),W(1,26),GHPQ,AMP(28))           
      CALL IXXXXX(P(0,5),BMASS,NHEL(5),-1*IC(5),W(1,39))       
      CALL OXXXXX(P(0,6),TMASS,NHEL(6),+1*IC(6),W(1,40))       
      CALL HIOCXX(W(1,39),W(1,3),GB1NIP,MBL,WBL,W(1,41))                                                          
      CALL FSIXXX(W(1,2),W(1,41),GB1GOM,MGO,WGO,W(1,42))                                                          
      CALL HIOXXX(W(1,42),W(1,1),GB1GOP,MBL,WBL,W(1,43))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,43),GB1XJM,AMP(29))        
      CALL HIOXXX(W(1,42),W(1,1),GB2GOP,MBR,WBR,W(1,44))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,44),GB2XJM,AMP(30))        
      CALL HIOCXX(W(1,39),W(1,3),GB2NIP,MBR,WBR,W(1,45))                                                          
      CALL FSIXXX(W(1,2),W(1,45),GB2GOM,MGO,WGO,W(1,46))                                                          
      CALL HIOXXX(W(1,46),W(1,1),GB1GOP,MBL,WBL,W(1,47))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,47),GB1XJM,AMP(31))        
      CALL HIOXXX(W(1,46),W(1,1),GB2GOP,MBR,WBR,W(1,48))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,48),GB2XJM,AMP(32))        
      CALL HVSXXX(W(1,29),W(1,41),GC,MBL,WBL,W(1,49))     
      CALL IOSCXX(W(1,4),W(1,40),W(1,49),GB1XJM,AMP(33))        
      CALL HVSXXX(W(1,29),W(1,45),GC,MBR,WBR,W(1,50))     
      CALL IOSCXX(W(1,4),W(1,40),W(1,50),GB2XJM,AMP(34))        
      CALL FSICXX(W(1,4),W(1,41),GB1XJM,TMASS,TWIDTH,W(1,51))                                                          
      CALL IOVCXX(W(1,51),W(1,40),W(1,29),GG,AMP(35))            
      CALL FSICXX(W(1,4),W(1,45),GB2XJM,TMASS,TWIDTH,W(1,52))                                                          
      CALL IOVCXX(W(1,52),W(1,40),W(1,29),GG,AMP(36))            
      CALL FVICXX(W(1,39),W(1,29),GG,BMASS,ZERO,W(1,53))    
      CALL HIOCXX(W(1,53),W(1,3),GB1NIP,MBL,WBL,W(1,54))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,54),GB1XJM,AMP(37))        
      CALL HIOCXX(W(1,53),W(1,3),GB2NIP,MBR,WBR,W(1,55))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,55),GB2XJM,AMP(38))        
      CALL IOSCXX(W(1,42),W(1,40),W(1,7),GT1GOM,AMP(39))        
      CALL IOSCXX(W(1,46),W(1,40),W(1,7),GT1GOM,AMP(40))        
      CALL IOSCXX(W(1,42),W(1,40),W(1,10),GT2GOM,AMP(41))        
      CALL IOSCXX(W(1,46),W(1,40),W(1,10),GT2GOM,AMP(42))        
      CALL HIOCXX(W(1,39),W(1,18),GB1GOP,MBL,WBL,W(1,56))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,56),GB1XJM,AMP(43))        
      CALL HIOCXX(W(1,39),W(1,18),GB2GOP,MBR,WBR,W(1,57))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,57),GB2XJM,AMP(44))        
      CALL HIOCXX(W(1,39),W(1,22),GB1GOP,MBL,WBL,W(1,58))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,58),GB1XJM,AMP(45))        
      CALL HIOCXX(W(1,39),W(1,22),GB2GOP,MBR,WBR,W(1,59))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,59),GB2XJM,AMP(46))        
      CALL FSICXX(W(1,39),W(1,17),GB1GOP,MGO,WGO,W(1,60))                                                          
      CALL HIOXXX(W(1,60),W(1,1),GB1GOP,MBL,WBL,W(1,61))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,61),GB1XJM,AMP(47))        
      CALL HIOXXX(W(1,60),W(1,1),GB2GOP,MBR,WBR,W(1,62))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,62),GB2XJM,AMP(48))        
      CALL FSICXX(W(1,39),W(1,21),GB2GOP,MGO,WGO,W(1,63))                                                          
      CALL HIOXXX(W(1,63),W(1,1),GB1GOP,MBL,WBL,W(1,64))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,64),GB1XJM,AMP(49))        
      CALL HIOXXX(W(1,63),W(1,1),GB2GOP,MBR,WBR,W(1,65))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,65),GB2XJM,AMP(50))        
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,66))         
      CALL HIOXXX(W(1,66),W(1,1),GB1NIP,MBL,WBL,W(1,67))                                                          
      CALL HVSXXX(W(1,8),W(1,67),GC,MBL,WBL,W(1,68))     
      CALL IOSCXX(W(1,4),W(1,40),W(1,68),GB1XJM,AMP(51))        
      CALL HIOXXX(W(1,66),W(1,1),GB2NIP,MBR,WBR,W(1,69))                                                          
      CALL HVSXXX(W(1,8),W(1,69),GC,MBR,WBR,W(1,70))     
      CALL IOSCXX(W(1,4),W(1,40),W(1,70),GB2XJM,AMP(52))        
      CALL FSICXX(W(1,4),W(1,67),GB1XJM,TMASS,TWIDTH,W(1,71))                                                          
      CALL IOVCXX(W(1,71),W(1,40),W(1,8),GG,AMP(53))            
      CALL FSICXX(W(1,4),W(1,69),GB2XJM,TMASS,TWIDTH,W(1,72))                                                          
      CALL IOVCXX(W(1,72),W(1,40),W(1,8),GG,AMP(54))            
      CALL FSIXXX(W(1,2),W(1,67),GB1GOM,MGO,WGO,W(1,73))                                                          
      CALL HIOXXX(W(1,73),W(1,5),GB1GOP,MBL,WBL,W(1,74))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,74),GB1XJM,AMP(55))        
      CALL HIOXXX(W(1,73),W(1,5),GB2GOP,MBR,WBR,W(1,75))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,75),GB2XJM,AMP(56))        
      CALL FSIXXX(W(1,2),W(1,69),GB2GOM,MGO,WGO,W(1,76))                                                          
      CALL HIOXXX(W(1,76),W(1,5),GB1GOP,MBL,WBL,W(1,77))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,77),GB1XJM,AMP(57))        
      CALL HIOXXX(W(1,76),W(1,5),GB2GOP,MBR,WBR,W(1,78))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,78),GB2XJM,AMP(58))        
      CALL HIOXXX(W(1,66),W(1,14),GB1NIP,MBL,WBL,W(1,79))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,79),GB1XJM,AMP(59))        
      CALL HIOXXX(W(1,66),W(1,14),GB2NIP,MBR,WBR,W(1,80))                                                          
      CALL IOSCXX(W(1,4),W(1,40),W(1,80),GB2XJM,AMP(60))        
      CALL IOSCXX(W(1,73),W(1,40),W(1,19),GT1GOM,AMP(61))        
      CALL IOSCXX(W(1,73),W(1,40),W(1,20),GT2GOM,AMP(62))        
      CALL IOSCXX(W(1,76),W(1,40),W(1,19),GT1GOM,AMP(63))        
      CALL IOSCXX(W(1,76),W(1,40),W(1,20),GT2GOM,AMP(64))        
      JAMP(   1) = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)+AMP(  11)+AMP(  12)+AMP(  13)+AMP(  14)
     &             +AMP(  15)+AMP(  16)+AMP(  17)+AMP(  18)-AMP(  29)
     &             -AMP(  30)-AMP(  31)-AMP(  32)+AMP(  39)+AMP(  40)
     &             +AMP(  41)+AMP(  42)+AMP(  47)+AMP(  48)+AMP(  49)
     &             +AMP(  50)-AMP(  51)-AMP(  52)-AMP(  53)-AMP(  54)
     &             -AMP(  59)-AMP(  60)
      JAMP(   2) = -AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)+AMP(  19)
     &             +AMP(  20)+AMP(  21)+AMP(  22)+AMP(  23)+AMP(  24)
     &             -AMP(  25)-AMP(  26)-AMP(  27)-AMP(  28)-AMP(  33)
     &             -AMP(  34)-AMP(  35)-AMP(  36)-AMP(  37)-AMP(  38)
     &             +AMP(  43)+AMP(  44)+AMP(  45)+AMP(  46)-AMP(  55)
     &             -AMP(  56)-AMP(  57)-AMP(  58)+AMP(  61)+AMP(  62)
     &             +AMP(  63)+AMP(  64)
      MATRIX_BXB_NIXJBTX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_BXB_NIXJBTX =MATRIX_BXB_NIXJBTX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
