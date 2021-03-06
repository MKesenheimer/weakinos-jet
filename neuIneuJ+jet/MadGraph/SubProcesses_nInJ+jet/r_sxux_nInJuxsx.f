      SUBROUTINE SMATRIX_SXUX_NINJUXSX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : s~ u~ -> n1 n2 u~ s~  
C  
C Crossing   1 is s~ u~ -> n1 n2 u~ s~  
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
      REAL*8 MATRIX_SXUX_NINJUXSX
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
      common/to_Ramps_sxux_nInJuxsx/  amp2,       jamp2

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
      DATA NGRAPHS /   60/          
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
                 T=MATRIX_SXUX_NINJUXSX(P1,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_SXUX_NINJUXSX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : s~ u~ -> n1 n2 u~ s~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS=  60,NEIGEN=  1) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS=  90, NCOLOR=   1) 
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
      common/to_Ramps_sxux_nInJuxsx/  amp2,       jamp2
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
      CALL OXXXXX(P(0,1),ZERO,NHEL(1),-1*IC(1),W(1,1))        
      CALL OXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))        
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),ZERO,NHEL(5),-1*IC(5),W(1,5))        
      CALL IXXXXX(P(0,6),ZERO,NHEL(6),-1*IC(6),W(1,6))        
      CALL HIOXXX(W(1,4),W(1,1),GDLNJP,MSL,WSL,W(1,7))                                                          
      CALL JIOXXX(W(1,5),W(1,2),GG,ZERO,ZERO,W(1,8))     
      CALL HVSXXX(W(1,8),W(1,7),GC,MSL,WSL,W(1,9))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,9),GDLNIM,AMP(1))         
      CALL HIOXXX(W(1,4),W(1,1),GDRNJP,MSR,WSR,W(1,10))                                                          
      CALL HVSXXX(W(1,8),W(1,10),GC,MSR,WSR,W(1,11))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,11),GDRNIM,AMP(2))         
      CALL FSOXXX(W(1,3),W(1,7),GDLNIM,ZERO,ZERO,W(1,12))                                                          
      CALL IOVXXX(W(1,6),W(1,12),W(1,8),GG,AMP(3))             
      CALL FSOXXX(W(1,3),W(1,10),GDRNIM,ZERO,ZERO,W(1,13))                                                          
      CALL IOVXXX(W(1,6),W(1,13),W(1,8),GG,AMP(4))             
      CALL FVOXXX(W(1,1),W(1,8),GG,ZERO,ZERO,W(1,14))     
      CALL HIOXXX(W(1,4),W(1,14),GDLNJP,MSL,WSL,W(1,15))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,15),GDLNIM,AMP(5))         
      CALL HIOXXX(W(1,4),W(1,14),GDRNJP,MSR,WSR,W(1,16))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,16),GDRNIM,AMP(6))         
      CALL HIOXXX(W(1,5),W(1,3),GULNIM,MUL,WUL,W(1,17))                                                          
      CALL FSOXXX(W(1,2),W(1,17),GQLGOP,MGO,WGO,W(1,18))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,7),GQLGOM,AMP(7))         
      CALL HIOXXX(W(1,5),W(1,3),GURNIM,MUR,WUR,W(1,19))                                                          
      CALL FSOXXX(W(1,2),W(1,19),GQRGOP,MGO,WGO,W(1,20))                                                          
      CALL IOSXXX(W(1,6),W(1,20),W(1,7),GQLGOM,AMP(8))         
      CALL IOSXXX(W(1,6),W(1,18),W(1,10),GQRGOM,AMP(9))         
      CALL IOSXXX(W(1,6),W(1,20),W(1,10),GQRGOM,AMP(10))         
      CALL HIOXXX(W(1,4),W(1,2),GULNJP,MUL,WUL,W(1,21))                                                          
      CALL FSOXXX(W(1,3),W(1,21),GULNIM,ZERO,ZERO,W(1,22))                                                          
      CALL JIOXXX(W(1,5),W(1,22),GG,ZERO,ZERO,W(1,23))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,23),GG,AMP(11))             
      CALL HIOXXX(W(1,4),W(1,2),GURNJP,MUR,WUR,W(1,24))                                                          
      CALL FSOXXX(W(1,3),W(1,24),GURNIM,ZERO,ZERO,W(1,25))                                                          
      CALL JIOXXX(W(1,5),W(1,25),GG,ZERO,ZERO,W(1,26))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,26),GG,AMP(12))             
      CALL JSSXXX(W(1,21),W(1,17),GC,ZERO,ZERO,W(1,27))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,27),GG,AMP(13))             
      CALL JSSXXX(W(1,24),W(1,19),GC,ZERO,ZERO,W(1,28))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,28),GG,AMP(14))             
      CALL FSIXXX(W(1,4),W(1,17),GULNJP,ZERO,ZERO,W(1,29))                                                          
      CALL JIOXXX(W(1,29),W(1,2),GG,ZERO,ZERO,W(1,30))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,30),GG,AMP(15))             
      CALL FSIXXX(W(1,4),W(1,19),GURNJP,ZERO,ZERO,W(1,31))                                                          
      CALL JIOXXX(W(1,31),W(1,2),GG,ZERO,ZERO,W(1,32))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,32),GG,AMP(16))             
      CALL FSIXXX(W(1,5),W(1,21),GQLGOM,MGO,WGO,W(1,33))                                                          
      CALL HIOXXX(W(1,33),W(1,1),GQLGOP,MSL,WSL,W(1,34))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,34),GDLNIM,AMP(17))         
      CALL HIOXXX(W(1,33),W(1,1),GQRGOP,MSR,WSR,W(1,35))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,35),GDRNIM,AMP(18))         
      CALL FSIXXX(W(1,5),W(1,24),GQRGOM,MGO,WGO,W(1,36))                                                          
      CALL HIOXXX(W(1,36),W(1,1),GQLGOP,MSL,WSL,W(1,37))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,37),GDLNIM,AMP(19))         
      CALL HIOXXX(W(1,36),W(1,1),GQRGOP,MSR,WSR,W(1,38))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,38),GDRNIM,AMP(20))         
      CALL JIOCXX(W(1,4),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,39))                                                          
      CALL FVOXXX(W(1,2),W(1,39),GZU,ZERO,ZERO,W(1,40))    
      CALL JIOXXX(W(1,5),W(1,40),GG,ZERO,ZERO,W(1,41))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,41),GG,AMP(21))             
      CALL IOVXXX(W(1,6),W(1,14),W(1,39),GZD,AMP(22))            
      CALL FVOXXX(W(1,1),W(1,39),GZD,ZERO,ZERO,W(1,42))    
      CALL IOVXXX(W(1,6),W(1,42),W(1,8),GG,AMP(23))             
      CALL FVIXXX(W(1,5),W(1,39),GZU,ZERO,ZERO,W(1,43))    
      CALL JIOXXX(W(1,43),W(1,2),GG,ZERO,ZERO,W(1,44))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,44),GG,AMP(24))             
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,45))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,46))         
      CALL HIOXXX(W(1,45),W(1,2),GULNIP,MUL,WUL,W(1,47))                                                          
      CALL FSOXXX(W(1,46),W(1,47),GULNJM,ZERO,ZERO,W(1,48))                                                          
      CALL JIOXXX(W(1,5),W(1,48),GG,ZERO,ZERO,W(1,49))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,49),GG,AMP(25))             
      CALL HIOXXX(W(1,45),W(1,2),GURNIP,MUR,WUR,W(1,50))                                                          
      CALL FSOXXX(W(1,46),W(1,50),GURNJM,ZERO,ZERO,W(1,51))                                                          
      CALL JIOXXX(W(1,5),W(1,51),GG,ZERO,ZERO,W(1,52))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,52),GG,AMP(26))             
      CALL HIOXXX(W(1,5),W(1,46),GULNJM,MUL,WUL,W(1,53))                                                          
      CALL JSSXXX(W(1,47),W(1,53),GC,ZERO,ZERO,W(1,54))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,54),GG,AMP(27))             
      CALL HIOXXX(W(1,5),W(1,46),GURNJM,MUR,WUR,W(1,55))                                                          
      CALL JSSXXX(W(1,50),W(1,55),GC,ZERO,ZERO,W(1,56))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,56),GG,AMP(28))             
      CALL FSIXXX(W(1,45),W(1,53),GULNIP,ZERO,ZERO,W(1,57))                                                          
      CALL JIOXXX(W(1,57),W(1,2),GG,ZERO,ZERO,W(1,58))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,58),GG,AMP(29))             
      CALL FSIXXX(W(1,45),W(1,55),GURNIP,ZERO,ZERO,W(1,59))                                                          
      CALL JIOXXX(W(1,59),W(1,2),GG,ZERO,ZERO,W(1,60))     
      CALL IOVXXX(W(1,6),W(1,1),W(1,60),GG,AMP(30))             
      CALL FSIXXX(W(1,5),W(1,47),GQLGOM,MGO,WGO,W(1,61))                                                          
      CALL HIOXXX(W(1,61),W(1,1),GQLGOP,MSL,WSL,W(1,62))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,62),GDLNJM,AMP(31))         
      CALL HIOXXX(W(1,61),W(1,1),GQRGOP,MSR,WSR,W(1,63))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,63),GDRNJM,AMP(32))         
      CALL FSIXXX(W(1,5),W(1,50),GQRGOM,MGO,WGO,W(1,64))                                                          
      CALL HIOXXX(W(1,64),W(1,1),GQLGOP,MSL,WSL,W(1,65))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,65),GDLNJM,AMP(33))         
      CALL HIOXXX(W(1,64),W(1,1),GQRGOP,MSR,WSR,W(1,66))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,66),GDRNJM,AMP(34))         
      CALL HIOXXX(W(1,45),W(1,1),GDLNIP,MSL,WSL,W(1,67))                                                          
      CALL HVSXXX(W(1,8),W(1,67),GC,MSL,WSL,W(1,68))     
      CALL IOSXXX(W(1,6),W(1,46),W(1,68),GDLNJM,AMP(35))         
      CALL HIOXXX(W(1,45),W(1,1),GDRNIP,MSR,WSR,W(1,69))                                                          
      CALL HVSXXX(W(1,8),W(1,69),GC,MSR,WSR,W(1,70))     
      CALL IOSXXX(W(1,6),W(1,46),W(1,70),GDRNJM,AMP(36))         
      CALL FSOXXX(W(1,46),W(1,67),GDLNJM,ZERO,ZERO,W(1,71))                                                          
      CALL IOVXXX(W(1,6),W(1,71),W(1,8),GG,AMP(37))             
      CALL FSOXXX(W(1,46),W(1,69),GDRNJM,ZERO,ZERO,W(1,72))                                                          
      CALL IOVXXX(W(1,6),W(1,72),W(1,8),GG,AMP(38))             
      CALL HIOXXX(W(1,45),W(1,14),GDLNIP,MSL,WSL,W(1,73))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,73),GDLNJM,AMP(39))         
      CALL HIOXXX(W(1,45),W(1,14),GDRNIP,MSR,WSR,W(1,74))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,74),GDRNJM,AMP(40))         
      CALL FSOXXX(W(1,2),W(1,53),GQLGOP,MGO,WGO,W(1,75))                                                          
      CALL IOSXXX(W(1,6),W(1,75),W(1,67),GQLGOM,AMP(41))         
      CALL FSOXXX(W(1,2),W(1,55),GQRGOP,MGO,WGO,W(1,76))                                                          
      CALL IOSXXX(W(1,6),W(1,76),W(1,67),GQLGOM,AMP(42))         
      CALL IOSXXX(W(1,6),W(1,75),W(1,69),GQRGOM,AMP(43))         
      CALL IOSXXX(W(1,6),W(1,76),W(1,69),GQRGOM,AMP(44))         
      CALL OXXXXX(P(0,6),ZERO,NHEL(6),+1*IC(6),W(1,77))        
      CALL IOSCXX(W(1,61),W(1,77),W(1,7),GQLGOM,AMP(45))        
      CALL IOSCXX(W(1,61),W(1,77),W(1,10),GQRGOM,AMP(46))        
      CALL IOSCXX(W(1,64),W(1,77),W(1,7),GQLGOM,AMP(47))        
      CALL IOSCXX(W(1,64),W(1,77),W(1,10),GQRGOM,AMP(48))        
      CALL IOSCXX(W(1,33),W(1,77),W(1,67),GQLGOM,AMP(49))        
      CALL IOSCXX(W(1,36),W(1,77),W(1,67),GQLGOM,AMP(50))        
      CALL IOSCXX(W(1,33),W(1,77),W(1,69),GQRGOM,AMP(51))        
      CALL IOSCXX(W(1,36),W(1,77),W(1,69),GQRGOM,AMP(52))        
      CALL IXXXXX(P(0,2),ZERO,NHEL(2),+1*IC(2),W(1,78))        
      CALL FSICXX(W(1,78),W(1,17),GQLGOP,MGO,WGO,W(1,79))                                                          
      CALL HIOXXX(W(1,79),W(1,1),GQLGOP,MSL,WSL,W(1,80))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,80),GDLNJM,AMP(53))         
      CALL HIOXXX(W(1,79),W(1,1),GQRGOP,MSR,WSR,W(1,81))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,81),GDRNJM,AMP(54))         
      CALL FSICXX(W(1,78),W(1,19),GQRGOP,MGO,WGO,W(1,82))                                                          
      CALL HIOXXX(W(1,82),W(1,1),GQLGOP,MSL,WSL,W(1,83))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,83),GDLNJM,AMP(55))         
      CALL HIOXXX(W(1,82),W(1,1),GQRGOP,MSR,WSR,W(1,84))                                                          
      CALL IOSXXX(W(1,6),W(1,46),W(1,84),GDRNJM,AMP(56))         
      CALL FSICXX(W(1,78),W(1,53),GQLGOP,MGO,WGO,W(1,85))                                                          
      CALL HIOXXX(W(1,85),W(1,1),GQLGOP,MSL,WSL,W(1,86))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,86),GDLNIM,AMP(57))         
      CALL HIOXXX(W(1,85),W(1,1),GQRGOP,MSR,WSR,W(1,87))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,87),GDRNIM,AMP(58))         
      CALL FSICXX(W(1,78),W(1,55),GQRGOP,MGO,WGO,W(1,88))                                                          
      CALL HIOXXX(W(1,88),W(1,1),GQLGOP,MSL,WSL,W(1,89))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,89),GDLNIM,AMP(59))         
      CALL HIOXXX(W(1,88),W(1,1),GQRGOP,MSR,WSR,W(1,90))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,90),GDRNIM,AMP(60))         
      JAMP(   1) = +AMP(   1)+AMP(   2)+AMP(   3)+AMP(   4)+AMP(   5)
     &             +AMP(   6)-AMP(   7)-AMP(   8)-AMP(   9)-AMP(  10)
     &             +AMP(  11)+AMP(  12)+AMP(  13)+AMP(  14)+AMP(  15)
     &             +AMP(  16)-AMP(  17)-AMP(  18)-AMP(  19)-AMP(  20)
     &             -AMP(  21)-AMP(  22)-AMP(  23)-AMP(  24)-AMP(  25)
     &             -AMP(  26)-AMP(  27)-AMP(  28)-AMP(  29)-AMP(  30)
     &             +AMP(  31)+AMP(  32)+AMP(  33)+AMP(  34)-AMP(  35)
     &             -AMP(  36)-AMP(  37)-AMP(  38)-AMP(  39)-AMP(  40)
     &             +AMP(  41)+AMP(  42)+AMP(  43)+AMP(  44)+AMP(  45)
     &             +AMP(  46)+AMP(  47)+AMP(  48)-AMP(  49)-AMP(  50)
     &             -AMP(  51)-AMP(  52)+AMP(  53)+AMP(  54)+AMP(  55)
     &             +AMP(  56)-AMP(  57)-AMP(  58)-AMP(  59)-AMP(  60)
      MATRIX_SXUX_NINJUXSX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_SXUX_NINJUXSX =MATRIX_SXUX_NINJUXSX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
