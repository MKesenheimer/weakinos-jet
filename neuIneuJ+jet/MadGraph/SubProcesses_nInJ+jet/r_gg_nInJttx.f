      SUBROUTINE SMATRIX_GG_NINJTTX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : g g -> n1 n2 t t~  
C  
C Crossing   1 is g g -> n1 n2 t t~  
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
      REAL*8 MATRIX_GG_NINJTTX
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
      common/to_Ramps_gg_nInJttx/  amp2,       jamp2

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
      DATA NGRAPHS /   96/          
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
      DATA(IDEN(IHEL),IHEL=  1,  1) / 256/
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
                 T=MATRIX_GG_NINJTTX(P1,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_GG_NINJTTX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : g g -> n1 n2 t t~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS=  96,NEIGEN=  2) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS= 127, NCOLOR=   2) 
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
      common/to_Ramps_gg_nInJttx/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            3/                                       
      DATA(CF(i,1),i=1,2) /    16,   -2/                            
C               T[ 5, 6, 1, 2]                                             
      DATA Denom(2)/            3/                                       
      DATA(CF(i,2),i=1,2) /    -2,   16/                            
C               T[ 5, 6, 2, 1]                                             
C ----------
C BEGIN CODE
C ----------
      CALL VXXXXX(P(0,1),ZERO,NHEL(1),-1*IC(1),W(1,1))        
      CALL VXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))        
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),TMASS,NHEL(5),-1*IC(5),W(1,5))       
      CALL IXXXXX(P(0,6),TMASS,NHEL(6),-1*IC(6),W(1,6))       
      CALL FVICXX(W(1,5),W(1,1),GG,TMASS,TWIDTH,W(1,7))    
      CALL HIOCXX(W(1,7),W(1,4),GT1NJP,MTL,WTL,W(1,8))                                                          
      CALL FSOXXX(W(1,3),W(1,8),GT1NIM,TMASS,TWIDTH,W(1,9))                                                          
      CALL IOVXXX(W(1,6),W(1,9),W(1,2),GG,AMP(1))             
      CALL HIOCXX(W(1,7),W(1,4),GT2NJP,MTR,WTR,W(1,10))                                                          
      CALL FSOXXX(W(1,3),W(1,10),GT2NIM,TMASS,TWIDTH,W(1,11))                                                          
      CALL IOVXXX(W(1,6),W(1,11),W(1,2),GG,AMP(2))             
      CALL HVSXXX(W(1,2),W(1,8),GC,MTL,WTL,W(1,12))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,12),GT1NIM,AMP(3))         
      CALL HVSXXX(W(1,2),W(1,10),GC,MTR,WTR,W(1,13))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,13),GT2NIM,AMP(4))         
      CALL HIOCXX(W(1,5),W(1,4),GT1NJP,MTL,WTL,W(1,14))                                                          
      CALL HVSXXX(W(1,1),W(1,14),GC,MTL,WTL,W(1,15))     
      CALL FSOXXX(W(1,3),W(1,15),GT1NIM,TMASS,TWIDTH,W(1,16))                                                          
      CALL IOVXXX(W(1,6),W(1,16),W(1,2),GG,AMP(5))             
      CALL HIOCXX(W(1,5),W(1,4),GT2NJP,MTR,WTR,W(1,17))                                                          
      CALL HVSXXX(W(1,1),W(1,17),GC,MTR,WTR,W(1,18))     
      CALL FSOXXX(W(1,3),W(1,18),GT2NIM,TMASS,TWIDTH,W(1,19))                                                          
      CALL IOVXXX(W(1,6),W(1,19),W(1,2),GG,AMP(6))             
      CALL HVSXXX(W(1,2),W(1,15),GC,MTL,WTL,W(1,20))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,20),GT1NIM,AMP(7))         
      CALL HVSXXX(W(1,2),W(1,18),GC,MTR,WTR,W(1,21))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,21),GT2NIM,AMP(8))         
      CALL FVICXX(W(1,5),W(1,2),GG,TMASS,TWIDTH,W(1,22))    
      CALL HIOCXX(W(1,22),W(1,4),GT1NJP,MTL,WTL,W(1,23))                                                          
      CALL FSOXXX(W(1,3),W(1,23),GT1NIM,TMASS,TWIDTH,W(1,24))                                                          
      CALL IOVXXX(W(1,6),W(1,24),W(1,1),GG,AMP(9))             
      CALL HIOCXX(W(1,22),W(1,4),GT2NJP,MTR,WTR,W(1,25))                                                          
      CALL FSOXXX(W(1,3),W(1,25),GT2NIM,TMASS,TWIDTH,W(1,26))                                                          
      CALL IOVXXX(W(1,6),W(1,26),W(1,1),GG,AMP(10))             
      CALL HVSXXX(W(1,1),W(1,23),GC,MTL,WTL,W(1,27))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,27),GT1NIM,AMP(11))         
      CALL HVSXXX(W(1,1),W(1,25),GC,MTR,WTR,W(1,28))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,28),GT2NIM,AMP(12))         
      CALL HVSXXX(W(1,2),W(1,14),GC,MTL,WTL,W(1,29))     
      CALL FSOXXX(W(1,3),W(1,29),GT1NIM,TMASS,TWIDTH,W(1,30))                                                          
      CALL IOVXXX(W(1,6),W(1,30),W(1,1),GG,AMP(13))             
      CALL HVSXXX(W(1,2),W(1,17),GC,MTR,WTR,W(1,31))     
      CALL FSOXXX(W(1,3),W(1,31),GT2NIM,TMASS,TWIDTH,W(1,32))                                                          
      CALL IOVXXX(W(1,6),W(1,32),W(1,1),GG,AMP(14))             
      CALL HVSXXX(W(1,1),W(1,29),GC,MTL,WTL,W(1,33))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,33),GT1NIM,AMP(15))         
      CALL HVSXXX(W(1,1),W(1,31),GC,MTR,WTR,W(1,34))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,34),GT2NIM,AMP(16))         
      CALL FVICXX(W(1,7),W(1,2),GG,TMASS,TWIDTH,W(1,35))    
      CALL HIOCXX(W(1,35),W(1,4),GT1NJP,MTL,WTL,W(1,36))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,36),GT1NIM,AMP(17))         
      CALL HIOCXX(W(1,35),W(1,4),GT2NJP,MTR,WTR,W(1,37))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,37),GT2NIM,AMP(18))         
      CALL FVICXX(W(1,22),W(1,1),GG,TMASS,TWIDTH,W(1,38))    
      CALL HIOCXX(W(1,38),W(1,4),GT1NJP,MTL,WTL,W(1,39))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,39),GT1NIM,AMP(19))         
      CALL HIOCXX(W(1,38),W(1,4),GT2NJP,MTR,WTR,W(1,40))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,40),GT2NIM,AMP(20))         
      CALL FSOXXX(W(1,3),W(1,14),GT1NIM,TMASS,TWIDTH,W(1,41))                                                          
      CALL FVOXXX(W(1,41),W(1,2),GG,TMASS,TWIDTH,W(1,42))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,1),GG,AMP(21))             
      CALL FSOXXX(W(1,3),W(1,17),GT2NIM,TMASS,TWIDTH,W(1,43))                                                          
      CALL FVOXXX(W(1,43),W(1,2),GG,TMASS,TWIDTH,W(1,44))     
      CALL IOVXXX(W(1,6),W(1,44),W(1,1),GG,AMP(22))             
      CALL FVOXXX(W(1,41),W(1,1),GG,TMASS,TWIDTH,W(1,45))     
      CALL IOVXXX(W(1,6),W(1,45),W(1,2),GG,AMP(23))             
      CALL FVOXXX(W(1,43),W(1,1),GG,TMASS,TWIDTH,W(1,46))     
      CALL IOVXXX(W(1,6),W(1,46),W(1,2),GG,AMP(24))             
      CALL JVVXXX(W(1,1),W(1,2),G,ZERO,ZERO,W(1,47))      
      CALL HVSXXX(W(1,47),W(1,14),GC,MTL,WTL,W(1,48))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,48),GT1NIM,AMP(25))         
      CALL HVSXXX(W(1,47),W(1,17),GC,MTR,WTR,W(1,49))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,49),GT2NIM,AMP(26))         
      CALL IOVXXX(W(1,6),W(1,41),W(1,47),GG,AMP(27))             
      CALL IOVXXX(W(1,6),W(1,43),W(1,47),GG,AMP(28))             
      CALL FVICXX(W(1,5),W(1,47),GG,TMASS,TWIDTH,W(1,50))    
      CALL HIOCXX(W(1,50),W(1,4),GT1NJP,MTL,WTL,W(1,51))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,51),GT1NIM,AMP(29))         
      CALL HIOCXX(W(1,50),W(1,4),GT2NJP,MTR,WTR,W(1,52))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,52),GT2NIM,AMP(30))         
      CALL HVVSXX(W(1,1),W(1,2),W(1,14),G2C,MTL,WTL,    
     &     W(1,53))                                                      
      CALL IOSXXX(W(1,6),W(1,3),W(1,53),GT1NIM,AMP(31))         
      CALL HVVSXX(W(1,1),W(1,2),W(1,17),G2C,MTR,WTR,    
     &     W(1,54))                                                      
      CALL IOSXXX(W(1,6),W(1,3),W(1,54),GT2NIM,AMP(32))         
      CALL HIOCXX(W(1,22),W(1,3),GT1NIP,MTL,WTL,W(1,55))                                                          
      CALL FSOXXX(W(1,4),W(1,55),GT1NJM,TMASS,TWIDTH,W(1,56))                                                          
      CALL IOVXXX(W(1,6),W(1,56),W(1,1),GG,AMP(33))             
      CALL HIOCXX(W(1,22),W(1,3),GT2NIP,MTR,WTR,W(1,57))                                                          
      CALL FSOXXX(W(1,4),W(1,57),GT2NJM,TMASS,TWIDTH,W(1,58))                                                          
      CALL IOVXXX(W(1,6),W(1,58),W(1,1),GG,AMP(34))             
      CALL HVSXXX(W(1,1),W(1,55),GC,MTL,WTL,W(1,59))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,59),GT1NJM,AMP(35))         
      CALL HVSXXX(W(1,1),W(1,57),GC,MTR,WTR,W(1,60))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,60),GT2NJM,AMP(36))         
      CALL HIOCXX(W(1,5),W(1,3),GT1NIP,MTL,WTL,W(1,61))                                                          
      CALL HVSXXX(W(1,2),W(1,61),GC,MTL,WTL,W(1,62))     
      CALL FSOXXX(W(1,4),W(1,62),GT1NJM,TMASS,TWIDTH,W(1,63))                                                          
      CALL IOVXXX(W(1,6),W(1,63),W(1,1),GG,AMP(37))             
      CALL HIOCXX(W(1,5),W(1,3),GT2NIP,MTR,WTR,W(1,64))                                                          
      CALL HVSXXX(W(1,2),W(1,64),GC,MTR,WTR,W(1,65))     
      CALL FSOXXX(W(1,4),W(1,65),GT2NJM,TMASS,TWIDTH,W(1,66))                                                          
      CALL IOVXXX(W(1,6),W(1,66),W(1,1),GG,AMP(38))             
      CALL HVSXXX(W(1,1),W(1,62),GC,MTL,WTL,W(1,67))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,67),GT1NJM,AMP(39))         
      CALL HVSXXX(W(1,1),W(1,65),GC,MTR,WTR,W(1,68))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,68),GT2NJM,AMP(40))         
      CALL HIOCXX(W(1,7),W(1,3),GT1NIP,MTL,WTL,W(1,69))                                                          
      CALL FSOXXX(W(1,4),W(1,69),GT1NJM,TMASS,TWIDTH,W(1,70))                                                          
      CALL IOVXXX(W(1,6),W(1,70),W(1,2),GG,AMP(41))             
      CALL HIOCXX(W(1,7),W(1,3),GT2NIP,MTR,WTR,W(1,71))                                                          
      CALL FSOXXX(W(1,4),W(1,71),GT2NJM,TMASS,TWIDTH,W(1,72))                                                          
      CALL IOVXXX(W(1,6),W(1,72),W(1,2),GG,AMP(42))             
      CALL HVSXXX(W(1,2),W(1,69),GC,MTL,WTL,W(1,73))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,73),GT1NJM,AMP(43))         
      CALL HVSXXX(W(1,2),W(1,71),GC,MTR,WTR,W(1,74))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,74),GT2NJM,AMP(44))         
      CALL HVSXXX(W(1,1),W(1,61),GC,MTL,WTL,W(1,75))     
      CALL FSOXXX(W(1,4),W(1,75),GT1NJM,TMASS,TWIDTH,W(1,76))                                                          
      CALL IOVXXX(W(1,6),W(1,76),W(1,2),GG,AMP(45))             
      CALL HVSXXX(W(1,1),W(1,64),GC,MTR,WTR,W(1,77))     
      CALL FSOXXX(W(1,4),W(1,77),GT2NJM,TMASS,TWIDTH,W(1,78))                                                          
      CALL IOVXXX(W(1,6),W(1,78),W(1,2),GG,AMP(46))             
      CALL HVSXXX(W(1,2),W(1,75),GC,MTL,WTL,W(1,79))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,79),GT1NJM,AMP(47))         
      CALL HVSXXX(W(1,2),W(1,77),GC,MTR,WTR,W(1,80))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,80),GT2NJM,AMP(48))         
      CALL HIOCXX(W(1,35),W(1,3),GT1NIP,MTL,WTL,W(1,81))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,81),GT1NJM,AMP(49))         
      CALL HIOCXX(W(1,35),W(1,3),GT2NIP,MTR,WTR,W(1,82))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,82),GT2NJM,AMP(50))         
      CALL HIOCXX(W(1,38),W(1,3),GT1NIP,MTL,WTL,W(1,83))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,83),GT1NJM,AMP(51))         
      CALL HIOCXX(W(1,38),W(1,3),GT2NIP,MTR,WTR,W(1,84))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,84),GT2NJM,AMP(52))         
      CALL FSOXXX(W(1,4),W(1,61),GT1NJM,TMASS,TWIDTH,W(1,85))                                                          
      CALL FVOXXX(W(1,85),W(1,2),GG,TMASS,TWIDTH,W(1,86))     
      CALL IOVXXX(W(1,6),W(1,86),W(1,1),GG,AMP(53))             
      CALL FSOXXX(W(1,4),W(1,64),GT2NJM,TMASS,TWIDTH,W(1,87))                                                          
      CALL FVOXXX(W(1,87),W(1,2),GG,TMASS,TWIDTH,W(1,88))     
      CALL IOVXXX(W(1,6),W(1,88),W(1,1),GG,AMP(54))             
      CALL FVOXXX(W(1,85),W(1,1),GG,TMASS,TWIDTH,W(1,89))     
      CALL IOVXXX(W(1,6),W(1,89),W(1,2),GG,AMP(55))             
      CALL FVOXXX(W(1,87),W(1,1),GG,TMASS,TWIDTH,W(1,90))     
      CALL IOVXXX(W(1,6),W(1,90),W(1,2),GG,AMP(56))             
      CALL HVSXXX(W(1,47),W(1,61),GC,MTL,WTL,W(1,91))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,91),GT1NJM,AMP(57))         
      CALL HVSXXX(W(1,47),W(1,64),GC,MTR,WTR,W(1,92))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,92),GT2NJM,AMP(58))         
      CALL IOVXXX(W(1,6),W(1,85),W(1,47),GG,AMP(59))             
      CALL IOVXXX(W(1,6),W(1,87),W(1,47),GG,AMP(60))             
      CALL HIOCXX(W(1,50),W(1,3),GT1NIP,MTL,WTL,W(1,93))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,93),GT1NJM,AMP(61))         
      CALL HIOCXX(W(1,50),W(1,3),GT2NIP,MTR,WTR,W(1,94))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,94),GT2NJM,AMP(62))         
      CALL HVVSXX(W(1,1),W(1,2),W(1,61),G2C,MTL,WTL,    
     &     W(1,95))                                                      
      CALL IOSXXX(W(1,6),W(1,4),W(1,95),GT1NJM,AMP(63))         
      CALL HVVSXX(W(1,1),W(1,2),W(1,64),G2C,MTR,WTR,    
     &     W(1,96))                                                      
      CALL IOSXXX(W(1,6),W(1,4),W(1,96),GT2NJM,AMP(64))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,97))         
      CALL OXXXXX(P(0,5),TMASS,NHEL(5),+1*IC(5),W(1,98))       
      CALL JIOCXX(W(1,97),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,99))                                                          
      CALL FVOXXX(W(1,98),W(1,1),GG,TMASS,TWIDTH,W(1,100))     
      CALL FVOXXX(W(1,100),W(1,99),GZU,TMASS,TWIDTH,W(1,101))    
      CALL IOVXXX(W(1,6),W(1,101),W(1,2),GG,AMP(65))             
      CALL HIOCXX(W(1,97),W(1,3),GH1NIJ,MH1,WH1,W(1,102))                                                          
      CALL FSOXXX(W(1,100),W(1,102),GH1TT,TMASS,TWIDTH,W(1, 103))                                                          
      CALL IOVXXX(W(1,6),W(1,103),W(1,2),GG,AMP(66))             
      CALL HIOCXX(W(1,97),W(1,3),GH2NIJ,MH2,WH2,W(1,104))                                                          
      CALL FSOXXX(W(1,100),W(1,104),GH2TT,TMASS,TWIDTH,W(1, 105))                                                          
      CALL IOVXXX(W(1,6),W(1,105),W(1,2),GG,AMP(67))             
      CALL HIOCXX(W(1,97),W(1,3),GH3NIJ,MH3,WH3,W(1,106))                                                          
      CALL FSOXXX(W(1,100),W(1,106),GH3TT,TMASS,TWIDTH,W(1, 107))                                                          
      CALL IOVXXX(W(1,6),W(1,107),W(1,2),GG,AMP(68))             
      CALL FVOXXX(W(1,100),W(1,2),GG,TMASS,TWIDTH,W(1,108))     
      CALL IOVXXX(W(1,6),W(1,108),W(1,99),GZU,AMP(69))            
      CALL IOSXXX(W(1,6),W(1,108),W(1,102),GH1TT,AMP(70))          
      CALL IOSXXX(W(1,6),W(1,108),W(1,104),GH2TT,AMP(71))          
      CALL IOSXXX(W(1,6),W(1,108),W(1,106),GH3TT,AMP(72))          
      CALL FVOXXX(W(1,98),W(1,2),GG,TMASS,TWIDTH,W(1,109))     
      CALL FVOXXX(W(1,109),W(1,99),GZU,TMASS,TWIDTH,W(1,110))    
      CALL IOVXXX(W(1,6),W(1,110),W(1,1),GG,AMP(73))             
      CALL FSOXXX(W(1,109),W(1,102),GH1TT,TMASS,TWIDTH,W(1, 111))                                                          
      CALL IOVXXX(W(1,6),W(1,111),W(1,1),GG,AMP(74))             
      CALL FSOXXX(W(1,109),W(1,104),GH2TT,TMASS,TWIDTH,W(1, 112))                                                          
      CALL IOVXXX(W(1,6),W(1,112),W(1,1),GG,AMP(75))             
      CALL FSOXXX(W(1,109),W(1,106),GH3TT,TMASS,TWIDTH,W(1, 113))                                                          
      CALL IOVXXX(W(1,6),W(1,113),W(1,1),GG,AMP(76))             
      CALL FVOXXX(W(1,109),W(1,1),GG,TMASS,TWIDTH,W(1,114))     
      CALL IOVXXX(W(1,6),W(1,114),W(1,99),GZU,AMP(77))            
      CALL IOSXXX(W(1,6),W(1,114),W(1,102),GH1TT,AMP(78))          
      CALL IOSXXX(W(1,6),W(1,114),W(1,104),GH2TT,AMP(79))          
      CALL IOSXXX(W(1,6),W(1,114),W(1,106),GH3TT,AMP(80))          
      CALL FVOXXX(W(1,98),W(1,99),GZU,TMASS,TWIDTH,W(1,115))    
      CALL FVOXXX(W(1,115),W(1,2),GG,TMASS,TWIDTH,W(1,116))     
      CALL IOVXXX(W(1,6),W(1,116),W(1,1),GG,AMP(81))             
      CALL FSOXXX(W(1,98),W(1,102),GH1TT,TMASS,TWIDTH,W(1, 117))                                                          
      CALL FVOXXX(W(1,117),W(1,2),GG,TMASS,TWIDTH,W(1,118))     
      CALL IOVXXX(W(1,6),W(1,118),W(1,1),GG,AMP(82))             
      CALL FSOXXX(W(1,98),W(1,104),GH2TT,TMASS,TWIDTH,W(1, 119))                                                          
      CALL FVOXXX(W(1,119),W(1,2),GG,TMASS,TWIDTH,W(1,120))     
      CALL IOVXXX(W(1,6),W(1,120),W(1,1),GG,AMP(83))             
      CALL FSOXXX(W(1,98),W(1,106),GH3TT,TMASS,TWIDTH,W(1, 121))                                                          
      CALL FVOXXX(W(1,121),W(1,2),GG,TMASS,TWIDTH,W(1,122))     
      CALL IOVXXX(W(1,6),W(1,122),W(1,1),GG,AMP(84))             
      CALL FVOXXX(W(1,115),W(1,1),GG,TMASS,TWIDTH,W(1,123))     
      CALL IOVXXX(W(1,6),W(1,123),W(1,2),GG,AMP(85))             
      CALL FVOXXX(W(1,117),W(1,1),GG,TMASS,TWIDTH,W(1,124))     
      CALL IOVXXX(W(1,6),W(1,124),W(1,2),GG,AMP(86))             
      CALL FVOXXX(W(1,119),W(1,1),GG,TMASS,TWIDTH,W(1,125))     
      CALL IOVXXX(W(1,6),W(1,125),W(1,2),GG,AMP(87))             
      CALL FVOXXX(W(1,121),W(1,1),GG,TMASS,TWIDTH,W(1,126))     
      CALL IOVXXX(W(1,6),W(1,126),W(1,2),GG,AMP(88))             
      CALL IOVXXX(W(1,6),W(1,115),W(1,47),GG,AMP(89))             
      CALL IOVXXX(W(1,6),W(1,117),W(1,47),GG,AMP(90))             
      CALL IOVXXX(W(1,6),W(1,119),W(1,47),GG,AMP(91))             
      CALL IOVXXX(W(1,6),W(1,121),W(1,47),GG,AMP(92))             
      CALL FVOXXX(W(1,98),W(1,47),GG,TMASS,TWIDTH,W(1,127))     
      CALL IOVXXX(W(1,6),W(1,127),W(1,99),GZU,AMP(93))            
      CALL IOSXXX(W(1,6),W(1,127),W(1,102),GH1TT,AMP(94))          
      CALL IOSXXX(W(1,6),W(1,127),W(1,104),GH2TT,AMP(95))          
      CALL IOSXXX(W(1,6),W(1,127),W(1,106),GH3TT,AMP(96))          
      JAMP(   1) = +AMP(   1)+AMP(   2)+AMP(   3)+AMP(   4)+AMP(   5)
     &             +AMP(   6)+AMP(   7)+AMP(   8)+AMP(  17)+AMP(  18)
     &             +AMP(  23)+AMP(  24)+AMP(  25)+AMP(  26)+AMP(  27)
     &             +AMP(  28)+AMP(  29)+AMP(  30)+AMP(  31)+AMP(  32)
     &             -AMP(  41)-AMP(  42)-AMP(  43)-AMP(  44)-AMP(  45)
     &             -AMP(  46)-AMP(  47)-AMP(  48)-AMP(  49)-AMP(  50)
     &             -AMP(  55)-AMP(  56)-AMP(  57)-AMP(  58)-AMP(  59)
     &             -AMP(  60)-AMP(  61)-AMP(  62)-AMP(  63)-AMP(  64)
     &             +AMP(  65)+AMP(  66)+AMP(  67)+AMP(  68)+AMP(  69)
     &             +AMP(  70)+AMP(  71)+AMP(  72)+AMP(  85)+AMP(  86)
     &             +AMP(  87)+AMP(  88)+AMP(  89)+AMP(  90)+AMP(  91)
     &             +AMP(  92)+AMP(  93)+AMP(  94)+AMP(  95)+AMP(  96)
      JAMP(   2) = +AMP(   9)+AMP(  10)+AMP(  11)+AMP(  12)+AMP(  13)
     &             +AMP(  14)+AMP(  15)+AMP(  16)+AMP(  19)+AMP(  20)
     &             +AMP(  21)+AMP(  22)-AMP(  25)-AMP(  26)-AMP(  27)
     &             -AMP(  28)-AMP(  29)-AMP(  30)+AMP(  31)+AMP(  32)
     &             -AMP(  33)-AMP(  34)-AMP(  35)-AMP(  36)-AMP(  37)
     &             -AMP(  38)-AMP(  39)-AMP(  40)-AMP(  51)-AMP(  52)
     &             -AMP(  53)-AMP(  54)+AMP(  57)+AMP(  58)+AMP(  59)
     &             +AMP(  60)+AMP(  61)+AMP(  62)-AMP(  63)-AMP(  64)
     &             +AMP(  73)+AMP(  74)+AMP(  75)+AMP(  76)+AMP(  77)
     &             +AMP(  78)+AMP(  79)+AMP(  80)+AMP(  81)+AMP(  82)
     &             +AMP(  83)+AMP(  84)-AMP(  89)-AMP(  90)-AMP(  91)
     &             -AMP(  92)-AMP(  93)-AMP(  94)-AMP(  95)-AMP(  96)
      MATRIX_GG_NINJTTX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_GG_NINJTTX =MATRIX_GG_NINJTTX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       