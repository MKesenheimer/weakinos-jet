      SUBROUTINE SMATRIX_BG_XIXJBG(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : b g -> x1+ x2- b g  
C  
C Crossing   1 is b g -> x1+ x2- b g  
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
      REAL*8 MATRIX_BG_XIXJBG
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
      common/to_Ramps_bg_xIxJbg/  amp2,       jamp2

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
      DATA(IDEN(IHEL),IHEL=  1,  1) /  96/
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
                 T=MATRIX_BG_XIXJBG(P,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_BG_XIXJBG(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : b g -> x1+ x2- b g  
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
      PARAMETER(NWAVEFUNCS=  78, NCOLOR=   2) 
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
      common/to_Ramps_bg_xIxJbg/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            3/                                       
      DATA(CF(i,1),i=1,2) /    16,   -2/                            
C               T[ 5, 1, 2, 6]                                             
      DATA Denom(2)/            3/                                       
      DATA(CF(i,2),i=1,2) /    -2,   16/                            
C               T[ 5, 1, 6, 2]                                             
C ----------
C BEGIN CODE
C ----------
      CALL IXXXXX(P(0,1),BMASS,NHEL(1),+1*IC(1),W(1,1))       
      CALL VXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))        
      CALL IXXXXX(P(0,3),MXI,NHEL(3),-1*IC(3),W(1,3))         
      CALL OXXXXX(P(0,4),MXJ,NHEL(4),+1*IC(4),W(1,4))         
      CALL OXXXXX(P(0,5),BMASS,NHEL(5),+1*IC(5),W(1,5))       
      CALL VXXXXX(P(0,6),ZERO,NHEL(6),+1*IC(6),W(1,6))        
      CALL FVOXXX(W(1,5),W(1,2),GG,BMASS,ZERO,W(1,7))     
      CALL HIOXXX(W(1,3),W(1,7),GT1XIP,MTL,WTL,W(1,8))                                                          
      CALL FSOXXX(W(1,4),W(1,8),GT1XJM,BMASS,ZERO,W(1,9))                                                          
      CALL IOVXXX(W(1,1),W(1,9),W(1,6),GG,AMP(1))             
      CALL HIOXXX(W(1,3),W(1,7),GT2XIP,MTR,WTR,W(1,10))                                                          
      CALL FSOXXX(W(1,4),W(1,10),GT2XJM,BMASS,ZERO,W(1,11))                                                          
      CALL IOVXXX(W(1,1),W(1,11),W(1,6),GG,AMP(2))             
      CALL HIOXXX(W(1,1),W(1,4),GT1XJM,MTL,WTL,W(1,12))                                                          
      CALL FSIXXX(W(1,3),W(1,12),GT1XIP,BMASS,ZERO,W(1,13))                                                          
      CALL JIOXXX(W(1,13),W(1,5),GG,ZERO,ZERO,W(1,14))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,14),G,AMP(3))              
      CALL HIOXXX(W(1,1),W(1,4),GT2XJM,MTR,WTR,W(1,15))                                                          
      CALL FSIXXX(W(1,3),W(1,15),GT2XIP,BMASS,ZERO,W(1,16))                                                          
      CALL JIOXXX(W(1,16),W(1,5),GG,ZERO,ZERO,W(1,17))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,17),G,AMP(4))              
      CALL FVIXXX(W(1,13),W(1,2),GG,BMASS,ZERO,W(1,18))     
      CALL IOVXXX(W(1,18),W(1,5),W(1,6),GG,AMP(5))             
      CALL FVIXXX(W(1,16),W(1,2),GG,BMASS,ZERO,W(1,19))     
      CALL IOVXXX(W(1,19),W(1,5),W(1,6),GG,AMP(6))             
      CALL VSSXXX(W(1,6),W(1,8),W(1,12),GC,AMP(7))             
      CALL VSSXXX(W(1,6),W(1,10),W(1,15),GC,AMP(8))             
      CALL IOVXXX(W(1,13),W(1,7),W(1,6),GG,AMP(9))             
      CALL IOVXXX(W(1,16),W(1,7),W(1,6),GG,AMP(10))             
      CALL HIOXXX(W(1,3),W(1,5),GT1XIP,MTL,WTL,W(1,20))                                                          
      CALL HVSXXX(W(1,2),W(1,20),GC,MTL,WTL,W(1,21))     
      CALL FSOXXX(W(1,4),W(1,21),GT1XJM,BMASS,ZERO,W(1,22))                                                          
      CALL IOVXXX(W(1,1),W(1,22),W(1,6),GG,AMP(11))             
      CALL HIOXXX(W(1,3),W(1,5),GT2XIP,MTR,WTR,W(1,23))                                                          
      CALL HVSXXX(W(1,2),W(1,23),GC,MTR,WTR,W(1,24))     
      CALL FSOXXX(W(1,4),W(1,24),GT2XJM,BMASS,ZERO,W(1,25))                                                          
      CALL IOVXXX(W(1,1),W(1,25),W(1,6),GG,AMP(12))             
      CALL JSSXXX(W(1,20),W(1,12),GC,ZERO,ZERO,W(1,26))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,26),G,AMP(13))              
      CALL JSSXXX(W(1,23),W(1,15),GC,ZERO,ZERO,W(1,27))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,27),G,AMP(14))              
      CALL HVSXXX(W(1,2),W(1,12),-GC,MTL,WTL,W(1,28))    
      CALL FSIXXX(W(1,3),W(1,28),GT1XIP,BMASS,ZERO,W(1,29))                                                          
      CALL IOVXXX(W(1,29),W(1,5),W(1,6),GG,AMP(15))             
      CALL HVSXXX(W(1,2),W(1,15),-GC,MTR,WTR,W(1,30))    
      CALL FSIXXX(W(1,3),W(1,30),GT2XIP,BMASS,ZERO,W(1,31))                                                          
      CALL IOVXXX(W(1,31),W(1,5),W(1,6),GG,AMP(16))             
      CALL VVSSXX(W(1,2),W(1,6),W(1,20),W(1,12),G2C,     AMP(17))                                                      
      CALL VVSSXX(W(1,2),W(1,6),W(1,23),W(1,15),G2C,     AMP(18))                                                      
      CALL VSSXXX(W(1,6),W(1,21),W(1,12),GC,AMP(19))             
      CALL VSSXXX(W(1,6),W(1,24),W(1,15),GC,AMP(20))             
      CALL VSSXXX(W(1,6),W(1,20),W(1,28),GC,AMP(21))             
      CALL VSSXXX(W(1,6),W(1,23),W(1,30),GC,AMP(22))             
      CALL FSOXXX(W(1,4),W(1,20),GT1XJM,BMASS,ZERO,W(1,32))                                                          
      CALL FVOXXX(W(1,32),W(1,2),GG,BMASS,ZERO,W(1,33))     
      CALL IOVXXX(W(1,1),W(1,33),W(1,6),GG,AMP(23))             
      CALL FSOXXX(W(1,4),W(1,23),GT2XJM,BMASS,ZERO,W(1,34))                                                          
      CALL FVOXXX(W(1,34),W(1,2),GG,BMASS,ZERO,W(1,35))     
      CALL IOVXXX(W(1,1),W(1,35),W(1,6),GG,AMP(24))             
      CALL JIOXXX(W(1,1),W(1,32),GG,ZERO,ZERO,W(1,36))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,36),G,AMP(25))              
      CALL JIOXXX(W(1,1),W(1,34),GG,ZERO,ZERO,W(1,37))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,37),G,AMP(26))              
      CALL FVIXXX(W(1,1),W(1,2),GG,BMASS,ZERO,W(1,38))     
      CALL HIOXXX(W(1,38),W(1,4),GT1XJM,MTL,WTL,W(1,39))                                                          
      CALL FSIXXX(W(1,3),W(1,39),GT1XIP,BMASS,ZERO,W(1,40))                                                          
      CALL IOVXXX(W(1,40),W(1,5),W(1,6),GG,AMP(27))             
      CALL HIOXXX(W(1,38),W(1,4),GT2XJM,MTR,WTR,W(1,41))                                                          
      CALL FSIXXX(W(1,3),W(1,41),GT2XIP,BMASS,ZERO,W(1,42))                                                          
      CALL IOVXXX(W(1,42),W(1,5),W(1,6),GG,AMP(28))             
      CALL IOVXXX(W(1,38),W(1,32),W(1,6),GG,AMP(29))             
      CALL IOVXXX(W(1,38),W(1,34),W(1,6),GG,AMP(30))             
      CALL VSSXXX(W(1,6),W(1,20),W(1,39),GC,AMP(31))             
      CALL VSSXXX(W(1,6),W(1,23),W(1,41),GC,AMP(32))             
      CALL JIOXXX(W(1,3),W(1,4),GZXIJ,ZMASS,ZWIDTH,W(1, 43))                                                          
      CALL FVOXXX(W(1,7),W(1,43),GZD,BMASS,ZERO,W(1,44))    
      CALL IOVXXX(W(1,1),W(1,44),W(1,6),GG,AMP(33))             
      CALL HIOXXX(W(1,3),W(1,4),GH1XIJ,MH1,WH1,W(1,45))                                                          
      CALL FSOXXX(W(1,7),W(1,45),GH1BB,BMASS,ZERO,W(1, 46))                                                          
      CALL IOVXXX(W(1,1),W(1,46),W(1,6),GG,AMP(34))             
      CALL HIOXXX(W(1,3),W(1,4),GH2XIJ,MH2,WH2,W(1,47))                                                          
      CALL FSOXXX(W(1,7),W(1,47),GH2BB,BMASS,ZERO,W(1, 48))                                                          
      CALL IOVXXX(W(1,1),W(1,48),W(1,6),GG,AMP(35))             
      CALL HIOXXX(W(1,3),W(1,4),GH3XIJ,MH3,WH3,W(1,49))                                                          
      CALL FSOXXX(W(1,7),W(1,49),GH3BB,BMASS,ZERO,W(1, 50))                                                          
      CALL IOVXXX(W(1,1),W(1,50),W(1,6),GG,AMP(36))             
      CALL FVIXXX(W(1,1),W(1,43),GZD,BMASS,ZERO,W(1,51))    
      CALL JIOXXX(W(1,51),W(1,5),GG,ZERO,ZERO,W(1,52))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,52),G,AMP(37))              
      CALL FSIXXX(W(1,1),W(1,45),GH1BB,BMASS,ZERO,W(1, 53))                                                          
      CALL JIOXXX(W(1,53),W(1,5),GG,ZERO,ZERO,W(1,54))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,54),G,AMP(38))              
      CALL FSIXXX(W(1,1),W(1,47),GH2BB,BMASS,ZERO,W(1, 55))                                                          
      CALL JIOXXX(W(1,55),W(1,5),GG,ZERO,ZERO,W(1,56))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,56),G,AMP(39))              
      CALL FSIXXX(W(1,1),W(1,49),GH3BB,BMASS,ZERO,W(1, 57))                                                          
      CALL JIOXXX(W(1,57),W(1,5),GG,ZERO,ZERO,W(1,58))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,58),G,AMP(40))              
      CALL FVIXXX(W(1,51),W(1,2),GG,BMASS,ZERO,W(1,59))     
      CALL IOVXXX(W(1,59),W(1,5),W(1,6),GG,AMP(41))             
      CALL FVIXXX(W(1,53),W(1,2),GG,BMASS,ZERO,W(1,60))     
      CALL IOVXXX(W(1,60),W(1,5),W(1,6),GG,AMP(42))             
      CALL FVIXXX(W(1,55),W(1,2),GG,BMASS,ZERO,W(1,61))     
      CALL IOVXXX(W(1,61),W(1,5),W(1,6),GG,AMP(43))             
      CALL FVIXXX(W(1,57),W(1,2),GG,BMASS,ZERO,W(1,62))     
      CALL IOVXXX(W(1,62),W(1,5),W(1,6),GG,AMP(44))             
      CALL IOVXXX(W(1,51),W(1,7),W(1,6),GG,AMP(45))             
      CALL IOVXXX(W(1,53),W(1,7),W(1,6),GG,AMP(46))             
      CALL IOVXXX(W(1,55),W(1,7),W(1,6),GG,AMP(47))             
      CALL IOVXXX(W(1,57),W(1,7),W(1,6),GG,AMP(48))             
      CALL FVOXXX(W(1,5),W(1,43),GZD,BMASS,ZERO,W(1,63))    
      CALL FVOXXX(W(1,63),W(1,2),GG,BMASS,ZERO,W(1,64))     
      CALL IOVXXX(W(1,1),W(1,64),W(1,6),GG,AMP(49))             
      CALL FSOXXX(W(1,5),W(1,45),GH1BB,BMASS,ZERO,W(1, 65))                                                          
      CALL FVOXXX(W(1,65),W(1,2),GG,BMASS,ZERO,W(1,66))     
      CALL IOVXXX(W(1,1),W(1,66),W(1,6),GG,AMP(50))             
      CALL FSOXXX(W(1,5),W(1,47),GH2BB,BMASS,ZERO,W(1, 67))                                                          
      CALL FVOXXX(W(1,67),W(1,2),GG,BMASS,ZERO,W(1,68))     
      CALL IOVXXX(W(1,1),W(1,68),W(1,6),GG,AMP(51))             
      CALL FSOXXX(W(1,5),W(1,49),GH3BB,BMASS,ZERO,W(1, 69))                                                          
      CALL FVOXXX(W(1,69),W(1,2),GG,BMASS,ZERO,W(1,70))     
      CALL IOVXXX(W(1,1),W(1,70),W(1,6),GG,AMP(52))             
      CALL JIOXXX(W(1,1),W(1,63),GG,ZERO,ZERO,W(1,71))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,71),G,AMP(53))              
      CALL JIOXXX(W(1,1),W(1,65),GG,ZERO,ZERO,W(1,72))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,72),G,AMP(54))              
      CALL JIOXXX(W(1,1),W(1,67),GG,ZERO,ZERO,W(1,73))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,73),G,AMP(55))              
      CALL JIOXXX(W(1,1),W(1,69),GG,ZERO,ZERO,W(1,74))     
      CALL VVVXXX(W(1,6),W(1,2),W(1,74),G,AMP(56))              
      CALL FVIXXX(W(1,38),W(1,43),GZD,BMASS,ZERO,W(1,75))    
      CALL IOVXXX(W(1,75),W(1,5),W(1,6),GG,AMP(57))             
      CALL FSIXXX(W(1,38),W(1,45),GH1BB,BMASS,ZERO,W(1, 76))                                                          
      CALL IOVXXX(W(1,76),W(1,5),W(1,6),GG,AMP(58))             
      CALL FSIXXX(W(1,38),W(1,47),GH2BB,BMASS,ZERO,W(1, 77))                                                          
      CALL IOVXXX(W(1,77),W(1,5),W(1,6),GG,AMP(59))             
      CALL FSIXXX(W(1,38),W(1,49),GH3BB,BMASS,ZERO,W(1, 78))                                                          
      CALL IOVXXX(W(1,78),W(1,5),W(1,6),GG,AMP(60))             
      CALL IOVXXX(W(1,38),W(1,63),W(1,6),GG,AMP(61))             
      CALL IOVXXX(W(1,38),W(1,65),W(1,6),GG,AMP(62))             
      CALL IOVXXX(W(1,38),W(1,67),W(1,6),GG,AMP(63))             
      CALL IOVXXX(W(1,38),W(1,69),W(1,6),GG,AMP(64))             
      JAMP(   1) = -AMP(   1)-AMP(   2)+AMP(   3)+AMP(   4)-AMP(   7)
     &             -AMP(   8)-AMP(   9)-AMP(  10)-AMP(  11)-AMP(  12)
     &             +AMP(  13)+AMP(  14)-AMP(  17)-AMP(  18)-AMP(  19)
     &             -AMP(  20)-AMP(  23)-AMP(  24)+AMP(  25)+AMP(  26)
     &             +AMP(  33)+AMP(  34)+AMP(  35)+AMP(  36)-AMP(  37)
     &             -AMP(  38)-AMP(  39)-AMP(  40)+AMP(  45)+AMP(  46)
     &             +AMP(  47)+AMP(  48)+AMP(  49)+AMP(  50)+AMP(  51)
     &             +AMP(  52)-AMP(  53)-AMP(  54)-AMP(  55)-AMP(  56)
      JAMP(   2) = -AMP(   3)-AMP(   4)-AMP(   5)-AMP(   6)-AMP(  13)
     &             -AMP(  14)-AMP(  15)-AMP(  16)-AMP(  17)-AMP(  18)
     &             -AMP(  21)-AMP(  22)-AMP(  25)-AMP(  26)-AMP(  27)
     &             -AMP(  28)-AMP(  29)-AMP(  30)-AMP(  31)-AMP(  32)
     &             +AMP(  37)+AMP(  38)+AMP(  39)+AMP(  40)+AMP(  41)
     &             +AMP(  42)+AMP(  43)+AMP(  44)+AMP(  53)+AMP(  54)
     &             +AMP(  55)+AMP(  56)+AMP(  57)+AMP(  58)+AMP(  59)
     &             +AMP(  60)+AMP(  61)+AMP(  62)+AMP(  63)+AMP(  64)
      MATRIX_BG_XIXJBG = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_BG_XIXJBG =MATRIX_BG_XIXJBG+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
