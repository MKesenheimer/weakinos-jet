      SUBROUTINE SMATRIX_SXS_NINJBBX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : s~ s -> n1 n2 b b~  
C  
C Crossing   1 is s~ s -> n1 n2 b b~  
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
      REAL*8 MATRIX_SXS_NINJBBX
      INTEGER IHEL,IDEN(NCROSS),IC(NEXTERNAL,NCROSS)
      INTEGER IPROC,JC(NEXTERNAL), I
      LOGICAL GOODHEL(NCOMB,NCROSS)
      INTEGER NGRAPHS
      REAL*8 hwgt, xtot, xtry, xrej, xr, yfrac(0:ncomb)
      INTEGER idum, ngood, igood(ncomb), jhel, j, jj
      LOGICAL warned


C  

C
C VARIABLES TO APPLY THE DIAGRAM SUBTRACTION SCHEMES
C
#include "dsubtraction.h"
C
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxflow)
      common/to_Ramps_sxs_nInJbbx/  amp2,       jamp2

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
                 T=MATRIX_SXS_NINJBBX(P,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_SXS_NINJBBX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : s~ s -> n1 n2 b b~  
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
      PARAMETER(NWAVEFUNCS=  93, NCOLOR=   1) 
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

C
C VARIABLES TO APPLY THE DIAGRAM SUBTRACTION SCHEMES
C
#include "dsubtraction.h"
C
C GLOBAL VARIABLES
C  
      Double Precision amp2(maxamps), jamp2(0:maxflow)
      common/to_Ramps_sxs_nInJbbx/  amp2,       jamp2
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
      CALL OXXXXX(P(0,1),ZERO,NHEL(1),-1*IC(1),W(1,1))        
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,2))        
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),BMASS,NHEL(5),-1*IC(5),W(1,5))       
      CALL IXXXXX(P(0,6),BMASS,NHEL(6),-1*IC(6),W(1,6))       
      CALL HIOXXX(W(1,2),W(1,3),GDLNIM,MSL,WSL,W(1,7))                                                          
      CALL FSOXXX(W(1,1),W(1,7),GQLGOP,MGO,WGO,W(1,8))                                                          
      CALL HIOCXX(W(1,5),W(1,8),GB1GOP,MBL,WB1,W(1,      
     &     9))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,9),GB1N2M,AMP(1))         
      CALL HIOCXX(W(1,5),W(1,8),GB2GOP,MBR,WB2,W(1,      
     &     10))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,10),GB2N2M,AMP(2))         
      CALL HIOXXX(W(1,2),W(1,3),GDRNIM,MSR,WSR,W(1,11))                                                          
      CALL FSOXXX(W(1,1),W(1,11),GQRGOP,MGO,WGO,W(1,12))                                                          
      CALL HIOCXX(W(1,5),W(1,12),GB1GOP,MBL,WB1,W(1,      
     &     13))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,13),GB1N2M,AMP(3))         
      CALL HIOCXX(W(1,5),W(1,12),GB2GOP,MBR,WB2,W(1,      
     &     14))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,14),GB2N2M,AMP(4))         
      CALL HIOCXX(W(1,5),W(1,4),GB1N2P,MBL,WB1,W(1,      
     &     15))                                                          
      CALL IOSXXX(W(1,6),W(1,8),W(1,15),GB1GOM,AMP(5))         
      CALL HIOCXX(W(1,5),W(1,4),GB2N2P,MBR,WB2,W(1,      
     &     16))                                                          
      CALL IOSXXX(W(1,6),W(1,8),W(1,16),GB2GOM,AMP(6))         
      CALL IOSXXX(W(1,6),W(1,12),W(1,15),GB1GOM,AMP(7))         
      CALL IOSXXX(W(1,6),W(1,12),W(1,16),GB2GOM,AMP(8))         
      CALL HIOXXX(W(1,2),W(1,4),GDLNJM,MSL,WSL,W(1,17))                                                          
      CALL FSOXXX(W(1,1),W(1,17),GQLGOP,MGO,WGO,W(1,18))                                                          
      CALL HIOCXX(W(1,5),W(1,18),GB1GOP,MBL,WB1,W(1,      
     &     19))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,19),GB1N1M,AMP(9))         
      CALL HIOCXX(W(1,5),W(1,18),GB2GOP,MBR,WB2,W(1,      
     &     20))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,20),GB2N1M,AMP(10))         
      CALL HIOXXX(W(1,2),W(1,4),GDRNJM,MSR,WSR,W(1,21))                                                          
      CALL FSOXXX(W(1,1),W(1,21),GQRGOP,MGO,WGO,W(1,22))                                                          
      CALL HIOCXX(W(1,5),W(1,22),GB1GOP,MBL,WB1,W(1,      
     &     23))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,23),GB1N1M,AMP(11))         
      CALL HIOCXX(W(1,5),W(1,22),GB2GOP,MBR,WB2,W(1,      
     &     24))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,24),GB2N1M,AMP(12))         
      CALL HIOCXX(W(1,5),W(1,3),GB1N1P,MBL,WB1,W(1,      
     &     25))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,25),GB1GOM,AMP(13))         
      CALL HIOCXX(W(1,5),W(1,3),GB2N1P,MBR,WB2,W(1,      
     &     26))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,26),GB2GOM,AMP(14))         
      CALL IOSXXX(W(1,6),W(1,22),W(1,25),GB1GOM,AMP(15))         
      CALL IOSXXX(W(1,6),W(1,22),W(1,26),GB2GOM,AMP(16))         
      CALL JIOXXX(W(1,2),W(1,1),GG,ZERO,ZERO,W(1,27))     
      CALL HVSXXX(W(1,27),W(1,25),GC,MBL,WB1,W(1,28))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,28),GB1N2M,AMP(17))         
      CALL HVSXXX(W(1,27),W(1,26),GC,MBR,WB2,W(1,29))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,29),GB2N2M,AMP(18))         
      CALL FSOXXX(W(1,4),W(1,25),GB1N2M,BMASS,ZERO,W(1,30))                                                          
      CALL IOVXXX(W(1,6),W(1,30),W(1,27),GG,AMP(19))             
      CALL FSOXXX(W(1,4),W(1,26),GB2N2M,BMASS,ZERO,W(1,31))                                                          
      CALL IOVXXX(W(1,6),W(1,31),W(1,27),GG,AMP(20))             
      CALL FVICXX(W(1,5),W(1,27),GG,BMASS,ZERO,W(1,32))    
      CALL HIOCXX(W(1,32),W(1,3),GB1N1P,MBL,WB1,W(1,      
     &     33))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,33),GB1N2M,AMP(21))         
      CALL HIOCXX(W(1,32),W(1,3),GB2N1P,MBR,WB2,W(1,      
     &     34))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,34),GB2N2M,AMP(22))         
      CALL HVSXXX(W(1,27),W(1,15),GC,MBL,WB1,W(1,35))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,35),GB1N1M,AMP(23))         
      CALL HVSXXX(W(1,27),W(1,16),GC,MBR,WB2,W(1,36))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,36),GB2N1M,AMP(24))         
      CALL FSOXXX(W(1,3),W(1,15),GB1N1M,BMASS,ZERO,W(1,37))                                                          
      CALL IOVXXX(W(1,6),W(1,37),W(1,27),GG,AMP(25))             
      CALL FSOXXX(W(1,3),W(1,16),GB2N1M,BMASS,ZERO,W(1,38))                                                          
      CALL IOVXXX(W(1,6),W(1,38),W(1,27),GG,AMP(26))             
      CALL HIOCXX(W(1,32),W(1,4),GB1N2P,MBL,WB1,W(1,      
     &     39))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,39),GB1N1M,AMP(27))         
      CALL HIOCXX(W(1,32),W(1,4),GB2N2P,MBR,WB2,W(1,      
     &     40))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,40),GB2N1M,AMP(28))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,41))         
      CALL OXXXXX(P(0,5),BMASS,NHEL(5),+1*IC(5),W(1,42))       
      CALL FSIXXX(W(1,41),W(1,7),GDLNJP,ZERO,ZERO,W(1,43))                                                          
      CALL JIOXXX(W(1,43),W(1,1),GG,ZERO,ZERO,W(1,44))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,44),GG,AMP(29))             
      CALL FSIXXX(W(1,41),W(1,11),GDRNJP,ZERO,ZERO,W(1,45))                                                          
      CALL JIOXXX(W(1,45),W(1,1),GG,ZERO,ZERO,W(1,46))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,46),GG,AMP(30))             
      CALL HIOXXX(W(1,41),W(1,1),GDLNJP,MSL,WSL,W(1,47))                                                          
      CALL FSOXXX(W(1,3),W(1,47),GDLNIM,ZERO,ZERO,W(1,48))                                                          
      CALL JIOXXX(W(1,2),W(1,48),GG,ZERO,ZERO,W(1,49))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,49),GG,AMP(31))             
      CALL HIOXXX(W(1,41),W(1,1),GDRNJP,MSR,WSR,W(1,50))                                                          
      CALL FSOXXX(W(1,3),W(1,50),GDRNIM,ZERO,ZERO,W(1,51))                                                          
      CALL JIOXXX(W(1,2),W(1,51),GG,ZERO,ZERO,W(1,52))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,52),GG,AMP(32))             
      CALL JSSXXX(W(1,47),W(1,7),GC,ZERO,ZERO,W(1,53))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,53),GG,AMP(33))             
      CALL JSSXXX(W(1,50),W(1,11),GC,ZERO,ZERO,W(1,54))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,54),GG,AMP(34))             
      CALL FSIXXX(W(1,2),W(1,47),GQLGOM,MGO,WGO,W(1,55))                                                          
      CALL HIOXXX(W(1,55),W(1,42),GB1GOP,MBL,WB1,W(1,56))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,56),GB1N1M,AMP(35))         
      CALL HIOXXX(W(1,55),W(1,42),GB2GOP,MBR,WB2,W(1,57))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,57),GB2N1M,AMP(36))         
      CALL FSIXXX(W(1,2),W(1,50),GQRGOM,MGO,WGO,W(1,58))                                                          
      CALL HIOXXX(W(1,58),W(1,42),GB1GOP,MBL,WB1,W(1,59))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,59),GB1N1M,AMP(37))         
      CALL HIOXXX(W(1,58),W(1,42),GB2GOP,MBR,WB2,W(1,60))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,60),GB2N1M,AMP(38))         
      CALL JIOCXX(W(1,41),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,61))                                                          
      CALL FVIXXX(W(1,2),W(1,61),GZD,ZERO,ZERO,W(1,62))    
      CALL JIOXXX(W(1,62),W(1,1),GG,ZERO,ZERO,W(1,63))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,63),GG,AMP(39))             
      CALL FVOXXX(W(1,1),W(1,61),GZD,ZERO,ZERO,W(1,64))    
      CALL JIOXXX(W(1,2),W(1,64),GG,ZERO,ZERO,W(1,65))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,65),GG,AMP(40))             
      CALL FVOXXX(W(1,42),W(1,61),GZD,BMASS,ZERO,W(1,66))    
      CALL IOVXXX(W(1,6),W(1,66),W(1,27),GG,AMP(41))             
      CALL HIOCXX(W(1,41),W(1,3),GH1NIJ,MH1,WH1,W(1,      
     &     67))                                                          
      CALL FSOXXX(W(1,42),W(1,67),GH1BB,BMASS,ZERO,W(1, 68))                                                          
      CALL IOVXXX(W(1,6),W(1,68),W(1,27),GG,AMP(42))             
      CALL HIOCXX(W(1,41),W(1,3),GH2NIJ,MH2,WH2,W(1,      
     &     69))                                                          
      CALL FSOXXX(W(1,42),W(1,69),GH2BB,BMASS,ZERO,W(1, 70))                                                          
      CALL IOVXXX(W(1,6),W(1,70),W(1,27),GG,AMP(43))             
      CALL HIOCXX(W(1,41),W(1,3),GH3NIJ,MH3,WH3,W(1,      
     &     71))                                                          
      CALL FSOXXX(W(1,42),W(1,71),GH3BB,BMASS,ZERO,W(1, 72))                                                          
      CALL IOVXXX(W(1,6),W(1,72),W(1,27),GG,AMP(44))             
      CALL FVOXXX(W(1,42),W(1,27),GG,BMASS,ZERO,W(1,73))     
      CALL IOVXXX(W(1,6),W(1,73),W(1,61),GZD,AMP(45))            
      CALL IOSXXX(W(1,6),W(1,73),W(1,67),GH1BB,AMP(46))          
      CALL IOSXXX(W(1,6),W(1,73),W(1,69),GH2BB,AMP(47))          
      CALL IOSXXX(W(1,6),W(1,73),W(1,71),GH3BB,AMP(48))          
      CALL OXXXXX(P(0,6),BMASS,NHEL(6),+1*IC(6),W(1,74))       
      CALL IOSCXX(W(1,55),W(1,74),W(1,25),GB1GOM,AMP(49))        
      CALL IOSCXX(W(1,55),W(1,74),W(1,26),GB2GOM,AMP(50))        
      CALL IOSCXX(W(1,58),W(1,74),W(1,25),GB1GOM,AMP(51))        
      CALL IOSCXX(W(1,58),W(1,74),W(1,26),GB2GOM,AMP(52))        
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,75))         
      CALL FSIXXX(W(1,75),W(1,17),GDLNIP,ZERO,ZERO,W(1,76))                                                          
      CALL JIOXXX(W(1,76),W(1,1),GG,ZERO,ZERO,W(1,77))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,77),GG,AMP(53))             
      CALL FSIXXX(W(1,75),W(1,21),GDRNIP,ZERO,ZERO,W(1,78))                                                          
      CALL JIOXXX(W(1,78),W(1,1),GG,ZERO,ZERO,W(1,79))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,79),GG,AMP(54))             
      CALL HIOXXX(W(1,75),W(1,1),GDLNIP,MSL,WSL,W(1,80))                                                          
      CALL FSOXXX(W(1,4),W(1,80),GDLNJM,ZERO,ZERO,W(1,81))                                                          
      CALL JIOXXX(W(1,2),W(1,81),GG,ZERO,ZERO,W(1,82))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,82),GG,AMP(55))             
      CALL HIOXXX(W(1,75),W(1,1),GDRNIP,MSR,WSR,W(1,83))                                                          
      CALL FSOXXX(W(1,4),W(1,83),GDRNJM,ZERO,ZERO,W(1,84))                                                          
      CALL JIOXXX(W(1,2),W(1,84),GG,ZERO,ZERO,W(1,85))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,85),GG,AMP(56))             
      CALL JSSXXX(W(1,80),W(1,17),GC,ZERO,ZERO,W(1,86))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,86),GG,AMP(57))             
      CALL JSSXXX(W(1,83),W(1,21),GC,ZERO,ZERO,W(1,87))     
      CALL IOVXXX(W(1,6),W(1,42),W(1,87),GG,AMP(58))             
      CALL FSIXXX(W(1,2),W(1,80),GQLGOM,MGO,WGO,W(1,88))                                                          
      CALL HIOXXX(W(1,88),W(1,42),GB1GOP,MBL,WB1,W(1,89))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,89),GB1N2M,AMP(59))         
      CALL HIOXXX(W(1,88),W(1,42),GB2GOP,MBR,WB2,W(1,90))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,90),GB2N2M,AMP(60))         
      CALL FSIXXX(W(1,2),W(1,83),GQRGOM,MGO,WGO,W(1,91))                                                          
      CALL HIOXXX(W(1,91),W(1,42),GB1GOP,MBL,WB1,W(1,92))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,92),GB1N2M,AMP(61))         
      CALL HIOXXX(W(1,91),W(1,42),GB2GOP,MBR,WB2,W(1,93))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,93),GB2N2M,AMP(62))         
      CALL IOSCXX(W(1,88),W(1,74),W(1,15),GB1GOM,AMP(63))        
      CALL IOSCXX(W(1,88),W(1,74),W(1,16),GB2GOM,AMP(64))        
      CALL IOSCXX(W(1,91),W(1,74),W(1,15),GB1GOM,AMP(65))        
      CALL IOSCXX(W(1,91),W(1,74),W(1,16),GB2GOM,AMP(66))        
      JAMP(   1) = -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)+AMP(   5)
     &             +AMP(   6)+AMP(   7)+AMP(   8)+AMP(   9)+AMP(  10)
     &             +AMP(  11)+AMP(  12)-AMP(  13)-AMP(  14)-AMP(  15)
     &             -AMP(  16)+AMP(  17)+AMP(  18)+AMP(  19)+AMP(  20)
     &             +AMP(  21)+AMP(  22)-AMP(  23)-AMP(  24)-AMP(  25)
     &             -AMP(  26)-AMP(  27)-AMP(  28)+AMP(  29)+AMP(  30)
     &             +AMP(  31)+AMP(  32)+AMP(  33)+AMP(  34)-AMP(  35)
     &             -AMP(  36)-AMP(  37)-AMP(  38)-AMP(  39)-AMP(  40)
     &             -AMP(  41)-AMP(  42)-AMP(  43)-AMP(  44)-AMP(  45)
     &             -AMP(  46)-AMP(  47)-AMP(  48)+AMP(  49)+AMP(  50)
     &             +AMP(  51)+AMP(  52)-AMP(  53)-AMP(  54)-AMP(  55)
     &             -AMP(  56)-AMP(  57)-AMP(  58)+AMP(  59)+AMP(  60)
     &             +AMP(  61)+AMP(  62)-AMP(  63)-AMP(  64)-AMP(  65)
     &             -AMP(  66)
      MATRIX_SXS_NINJBBX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_SXS_NINJBBX =MATRIX_SXS_NINJBBX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
