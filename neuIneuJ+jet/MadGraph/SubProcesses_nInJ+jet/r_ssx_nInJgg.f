      SUBROUTINE SMATRIX_SSX_NINJGG(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : s s~ -> n1 n2 g g  
C  
C Crossing   1 is s s~ -> n1 n2 g g  
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
      REAL*8 MATRIX_SSX_NINJGG
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
      common/to_Ramps_ssx_nInJgg/  amp2,       jamp2

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
      DATA(IDEN(IHEL),IHEL=  1,  1) /  72/
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
                 T=MATRIX_SSX_NINJGG(P1,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_SSX_NINJGG(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : s s~ -> n1 n2 g g  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS=  72,NEIGEN=  2) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS=  87, NCOLOR=   2) 
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
      common/to_Ramps_ssx_nInJgg/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            3/                                       
      DATA(CF(i,1),i=1,2) /    16,   -2/                            
C               T[ 2, 1, 5, 6]                                             
      DATA Denom(2)/            3/                                       
      DATA(CF(i,2),i=1,2) /    -2,   16/                            
C               T[ 2, 1, 6, 5]                                             
C ----------
C BEGIN CODE
C ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))        
      CALL OXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))        
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,4))         
      CALL VXXXXX(P(0,5),ZERO,NHEL(5),+1*IC(5),W(1,5))        
      CALL VXXXXX(P(0,6),ZERO,NHEL(6),+1*IC(6),W(1,6))        
      CALL HIOXXX(W(1,4),W(1,2),GDLNJP,MSL,WSL,W(1,7))                                                          
      CALL FSOXXX(W(1,3),W(1,7),GDLNIM,ZERO,ZERO,W(1,8))                                                          
      CALL FVOXXX(W(1,8),W(1,5),GG,ZERO,ZERO,W(1,9))     
      CALL IOVXXX(W(1,1),W(1,9),W(1,6),GG,AMP(1))             
      CALL HIOXXX(W(1,4),W(1,2),GDRNJP,MSR,WSR,W(1,10))                                                          
      CALL FSOXXX(W(1,3),W(1,10),GDRNIM,ZERO,ZERO,W(1,11))                                                          
      CALL FVOXXX(W(1,11),W(1,5),GG,ZERO,ZERO,W(1,12))     
      CALL IOVXXX(W(1,1),W(1,12),W(1,6),GG,AMP(2))             
      CALL FVIXXX(W(1,1),W(1,5),GG,ZERO,ZERO,W(1,13))     
      CALL HIOXXX(W(1,13),W(1,3),GDLNIM,MSL,WSL,W(1,14))                                                          
      CALL FSIXXX(W(1,4),W(1,14),GDLNJP,ZERO,ZERO,W(1,15))                                                          
      CALL IOVXXX(W(1,15),W(1,2),W(1,6),GG,AMP(3))             
      CALL HIOXXX(W(1,13),W(1,3),GDRNIM,MSR,WSR,W(1,16))                                                          
      CALL FSIXXX(W(1,4),W(1,16),GDRNJP,ZERO,ZERO,W(1,17))                                                          
      CALL IOVXXX(W(1,17),W(1,2),W(1,6),GG,AMP(4))             
      CALL JIOXXX(W(1,1),W(1,8),GG,ZERO,ZERO,W(1,18))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,18),G,AMP(5))              
      CALL JIOXXX(W(1,1),W(1,11),GG,ZERO,ZERO,W(1,19))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,19),G,AMP(6))              
      CALL VSSXXX(W(1,6),W(1,7),W(1,14),GC,AMP(7))             
      CALL VSSXXX(W(1,6),W(1,10),W(1,16),GC,AMP(8))             
      CALL IOVXXX(W(1,13),W(1,8),W(1,6),GG,AMP(9))             
      CALL IOVXXX(W(1,13),W(1,11),W(1,6),GG,AMP(10))             
      CALL FVOXXX(W(1,2),W(1,5),GG,ZERO,ZERO,W(1,20))     
      CALL HIOXXX(W(1,4),W(1,20),GDLNJP,MSL,WSL,W(1,21))                                                          
      CALL FSOXXX(W(1,3),W(1,21),GDLNIM,ZERO,ZERO,W(1,22))                                                          
      CALL IOVXXX(W(1,1),W(1,22),W(1,6),GG,AMP(11))             
      CALL HIOXXX(W(1,4),W(1,20),GDRNJP,MSR,WSR,W(1,23))                                                          
      CALL FSOXXX(W(1,3),W(1,23),GDRNIM,ZERO,ZERO,W(1,24))                                                          
      CALL IOVXXX(W(1,1),W(1,24),W(1,6),GG,AMP(12))             
      CALL HIOXXX(W(1,1),W(1,3),GDLNIM,MSL,WSL,W(1,25))                                                          
      CALL FSIXXX(W(1,4),W(1,25),GDLNJP,ZERO,ZERO,W(1,26))                                                          
      CALL FVIXXX(W(1,26),W(1,5),GG,ZERO,ZERO,W(1,27))     
      CALL IOVXXX(W(1,27),W(1,2),W(1,6),GG,AMP(13))             
      CALL HIOXXX(W(1,1),W(1,3),GDRNIM,MSR,WSR,W(1,28))                                                          
      CALL FSIXXX(W(1,4),W(1,28),GDRNJP,ZERO,ZERO,W(1,29))                                                          
      CALL FVIXXX(W(1,29),W(1,5),GG,ZERO,ZERO,W(1,30))     
      CALL IOVXXX(W(1,30),W(1,2),W(1,6),GG,AMP(14))             
      CALL JIOXXX(W(1,26),W(1,2),GG,ZERO,ZERO,W(1,31))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,31),G,AMP(15))              
      CALL JIOXXX(W(1,29),W(1,2),GG,ZERO,ZERO,W(1,32))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,32),G,AMP(16))              
      CALL VSSXXX(W(1,6),W(1,21),W(1,25),GC,AMP(17))             
      CALL VSSXXX(W(1,6),W(1,23),W(1,28),GC,AMP(18))             
      CALL IOVXXX(W(1,26),W(1,20),W(1,6),GG,AMP(19))             
      CALL IOVXXX(W(1,29),W(1,20),W(1,6),GG,AMP(20))             
      CALL HVSXXX(W(1,5),W(1,7),GC,MSL,WSL,W(1,33))     
      CALL FSOXXX(W(1,3),W(1,33),GDLNIM,ZERO,ZERO,W(1,34))                                                          
      CALL IOVXXX(W(1,1),W(1,34),W(1,6),GG,AMP(21))             
      CALL HVSXXX(W(1,5),W(1,10),GC,MSR,WSR,W(1,35))     
      CALL FSOXXX(W(1,3),W(1,35),GDRNIM,ZERO,ZERO,W(1,36))                                                          
      CALL IOVXXX(W(1,1),W(1,36),W(1,6),GG,AMP(22))             
      CALL HVSXXX(W(1,5),W(1,25),-GC,MSL,WSL,W(1,37))    
      CALL FSIXXX(W(1,4),W(1,37),GDLNJP,ZERO,ZERO,W(1,38))                                                          
      CALL IOVXXX(W(1,38),W(1,2),W(1,6),GG,AMP(23))             
      CALL HVSXXX(W(1,5),W(1,28),-GC,MSR,WSR,W(1,39))    
      CALL FSIXXX(W(1,4),W(1,39),GDRNJP,ZERO,ZERO,W(1,40))                                                          
      CALL IOVXXX(W(1,40),W(1,2),W(1,6),GG,AMP(24))             
      CALL JSSXXX(W(1,7),W(1,25),GC,ZERO,ZERO,W(1,41))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,41),G,AMP(25))              
      CALL JSSXXX(W(1,10),W(1,28),GC,ZERO,ZERO,W(1,42))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,42),G,AMP(26))              
      CALL VVSSXX(W(1,5),W(1,6),W(1,7),W(1,25),G2C,     AMP(27))                                                      
      CALL VVSSXX(W(1,5),W(1,6),W(1,10),W(1,28),G2C,     AMP(28))                                                      
      CALL VSSXXX(W(1,6),W(1,33),W(1,25),GC,AMP(29))             
      CALL VSSXXX(W(1,6),W(1,35),W(1,28),GC,AMP(30))             
      CALL VSSXXX(W(1,6),W(1,7),W(1,37),GC,AMP(31))             
      CALL VSSXXX(W(1,6),W(1,10),W(1,39),GC,AMP(32))             
      CALL JIOCXX(W(1,4),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,43))                                                          
      CALL FVOXXX(W(1,2),W(1,43),GZD,ZERO,ZERO,W(1,44))    
      CALL FVOXXX(W(1,44),W(1,5),GG,ZERO,ZERO,W(1,45))     
      CALL IOVXXX(W(1,1),W(1,45),W(1,6),GG,AMP(33))             
      CALL FVIXXX(W(1,13),W(1,43),GZD,ZERO,ZERO,W(1,46))    
      CALL IOVXXX(W(1,46),W(1,2),W(1,6),GG,AMP(34))             
      CALL JIOXXX(W(1,1),W(1,44),GG,ZERO,ZERO,W(1,47))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,47),G,AMP(35))              
      CALL IOVXXX(W(1,13),W(1,44),W(1,6),GG,AMP(36))             
      CALL FVOXXX(W(1,20),W(1,43),GZD,ZERO,ZERO,W(1,48))    
      CALL IOVXXX(W(1,1),W(1,48),W(1,6),GG,AMP(37))             
      CALL FVIXXX(W(1,1),W(1,43),GZD,ZERO,ZERO,W(1,49))    
      CALL FVIXXX(W(1,49),W(1,5),GG,ZERO,ZERO,W(1,50))     
      CALL IOVXXX(W(1,50),W(1,2),W(1,6),GG,AMP(38))             
      CALL JIOXXX(W(1,49),W(1,2),GG,ZERO,ZERO,W(1,51))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,51),G,AMP(39))              
      CALL IOVXXX(W(1,49),W(1,20),W(1,6),GG,AMP(40))             
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,52))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,53))         
      CALL HIOXXX(W(1,52),W(1,2),GDLNIP,MSL,WSL,W(1,54))                                                          
      CALL FSOXXX(W(1,53),W(1,54),GDLNJM,ZERO,ZERO,W(1,55))                                                          
      CALL FVOXXX(W(1,55),W(1,5),GG,ZERO,ZERO,W(1,56))     
      CALL IOVXXX(W(1,1),W(1,56),W(1,6),GG,AMP(41))             
      CALL HIOXXX(W(1,52),W(1,2),GDRNIP,MSR,WSR,W(1,57))                                                          
      CALL FSOXXX(W(1,53),W(1,57),GDRNJM,ZERO,ZERO,W(1,58))                                                          
      CALL FVOXXX(W(1,58),W(1,5),GG,ZERO,ZERO,W(1,59))     
      CALL IOVXXX(W(1,1),W(1,59),W(1,6),GG,AMP(42))             
      CALL HIOXXX(W(1,13),W(1,53),GDLNJM,MSL,WSL,W(1,60))                                                          
      CALL FSIXXX(W(1,52),W(1,60),GDLNIP,ZERO,ZERO,W(1,61))                                                          
      CALL IOVXXX(W(1,61),W(1,2),W(1,6),GG,AMP(43))             
      CALL HIOXXX(W(1,13),W(1,53),GDRNJM,MSR,WSR,W(1,62))                                                          
      CALL FSIXXX(W(1,52),W(1,62),GDRNIP,ZERO,ZERO,W(1,63))                                                          
      CALL IOVXXX(W(1,63),W(1,2),W(1,6),GG,AMP(44))             
      CALL JIOXXX(W(1,1),W(1,55),GG,ZERO,ZERO,W(1,64))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,64),G,AMP(45))              
      CALL JIOXXX(W(1,1),W(1,58),GG,ZERO,ZERO,W(1,65))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,65),G,AMP(46))              
      CALL VSSXXX(W(1,6),W(1,54),W(1,60),GC,AMP(47))             
      CALL VSSXXX(W(1,6),W(1,57),W(1,62),GC,AMP(48))             
      CALL IOVXXX(W(1,13),W(1,55),W(1,6),GG,AMP(49))             
      CALL IOVXXX(W(1,13),W(1,58),W(1,6),GG,AMP(50))             
      CALL HIOXXX(W(1,52),W(1,20),GDLNIP,MSL,WSL,W(1,66))                                                          
      CALL FSOXXX(W(1,53),W(1,66),GDLNJM,ZERO,ZERO,W(1,67))                                                          
      CALL IOVXXX(W(1,1),W(1,67),W(1,6),GG,AMP(51))             
      CALL HIOXXX(W(1,52),W(1,20),GDRNIP,MSR,WSR,W(1,68))                                                          
      CALL FSOXXX(W(1,53),W(1,68),GDRNJM,ZERO,ZERO,W(1,69))                                                          
      CALL IOVXXX(W(1,1),W(1,69),W(1,6),GG,AMP(52))             
      CALL HIOXXX(W(1,1),W(1,53),GDLNJM,MSL,WSL,W(1,70))                                                          
      CALL FSIXXX(W(1,52),W(1,70),GDLNIP,ZERO,ZERO,W(1,71))                                                          
      CALL FVIXXX(W(1,71),W(1,5),GG,ZERO,ZERO,W(1,72))     
      CALL IOVXXX(W(1,72),W(1,2),W(1,6),GG,AMP(53))             
      CALL HIOXXX(W(1,1),W(1,53),GDRNJM,MSR,WSR,W(1,73))                                                          
      CALL FSIXXX(W(1,52),W(1,73),GDRNIP,ZERO,ZERO,W(1,74))                                                          
      CALL FVIXXX(W(1,74),W(1,5),GG,ZERO,ZERO,W(1,75))     
      CALL IOVXXX(W(1,75),W(1,2),W(1,6),GG,AMP(54))             
      CALL JIOXXX(W(1,71),W(1,2),GG,ZERO,ZERO,W(1,76))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,76),G,AMP(55))              
      CALL JIOXXX(W(1,74),W(1,2),GG,ZERO,ZERO,W(1,77))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,77),G,AMP(56))              
      CALL VSSXXX(W(1,6),W(1,66),W(1,70),GC,AMP(57))             
      CALL VSSXXX(W(1,6),W(1,68),W(1,73),GC,AMP(58))             
      CALL IOVXXX(W(1,71),W(1,20),W(1,6),GG,AMP(59))             
      CALL IOVXXX(W(1,74),W(1,20),W(1,6),GG,AMP(60))             
      CALL HVSXXX(W(1,5),W(1,54),GC,MSL,WSL,W(1,78))     
      CALL FSOXXX(W(1,53),W(1,78),GDLNJM,ZERO,ZERO,W(1,79))                                                          
      CALL IOVXXX(W(1,1),W(1,79),W(1,6),GG,AMP(61))             
      CALL HVSXXX(W(1,5),W(1,57),GC,MSR,WSR,W(1,80))     
      CALL FSOXXX(W(1,53),W(1,80),GDRNJM,ZERO,ZERO,W(1,81))                                                          
      CALL IOVXXX(W(1,1),W(1,81),W(1,6),GG,AMP(62))             
      CALL HVSXXX(W(1,5),W(1,70),-GC,MSL,WSL,W(1,82))    
      CALL FSIXXX(W(1,52),W(1,82),GDLNIP,ZERO,ZERO,W(1,83))                                                          
      CALL IOVXXX(W(1,83),W(1,2),W(1,6),GG,AMP(63))             
      CALL HVSXXX(W(1,5),W(1,73),-GC,MSR,WSR,W(1,84))    
      CALL FSIXXX(W(1,52),W(1,84),GDRNIP,ZERO,ZERO,W(1,85))                                                          
      CALL IOVXXX(W(1,85),W(1,2),W(1,6),GG,AMP(64))             
      CALL JSSXXX(W(1,54),W(1,70),GC,ZERO,ZERO,W(1,86))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,86),G,AMP(65))              
      CALL JSSXXX(W(1,57),W(1,73),GC,ZERO,ZERO,W(1,87))     
      CALL VVVXXX(W(1,6),W(1,5),W(1,87),G,AMP(66))              
      CALL VVSSXX(W(1,5),W(1,6),W(1,54),W(1,70),G2C,     AMP(67))                                                      
      CALL VVSSXX(W(1,5),W(1,6),W(1,57),W(1,73),G2C,     AMP(68))                                                      
      CALL VSSXXX(W(1,6),W(1,54),W(1,82),GC,AMP(69))             
      CALL VSSXXX(W(1,6),W(1,57),W(1,84),GC,AMP(70))             
      CALL VSSXXX(W(1,6),W(1,78),W(1,70),GC,AMP(71))             
      CALL VSSXXX(W(1,6),W(1,80),W(1,73),GC,AMP(72))             
      JAMP(   1) = +AMP(   1)+AMP(   2)-AMP(   5)-AMP(   6)+AMP(  11)
     &             +AMP(  12)-AMP(  15)-AMP(  16)+AMP(  17)+AMP(  18)
     &             +AMP(  19)+AMP(  20)+AMP(  21)+AMP(  22)-AMP(  25)
     &             -AMP(  26)+AMP(  27)+AMP(  28)+AMP(  29)+AMP(  30)
     &             -AMP(  33)+AMP(  35)-AMP(  37)+AMP(  39)-AMP(  40)
     &             -AMP(  41)-AMP(  42)+AMP(  45)+AMP(  46)-AMP(  51)
     &             -AMP(  52)+AMP(  55)+AMP(  56)-AMP(  57)-AMP(  58)
     &             -AMP(  59)-AMP(  60)-AMP(  61)-AMP(  62)+AMP(  65)
     &             +AMP(  66)-AMP(  67)-AMP(  68)-AMP(  71)-AMP(  72)
      JAMP(   2) = +AMP(   3)+AMP(   4)+AMP(   5)+AMP(   6)+AMP(   7)
     &             +AMP(   8)+AMP(   9)+AMP(  10)+AMP(  13)+AMP(  14)
     &             +AMP(  15)+AMP(  16)+AMP(  23)+AMP(  24)+AMP(  25)
     &             +AMP(  26)+AMP(  27)+AMP(  28)+AMP(  31)+AMP(  32)
     &             -AMP(  34)-AMP(  35)-AMP(  36)-AMP(  38)-AMP(  39)
     &             -AMP(  43)-AMP(  44)-AMP(  45)-AMP(  46)-AMP(  47)
     &             -AMP(  48)-AMP(  49)-AMP(  50)-AMP(  53)-AMP(  54)
     &             -AMP(  55)-AMP(  56)-AMP(  63)-AMP(  64)-AMP(  65)
     &             -AMP(  66)-AMP(  67)-AMP(  68)-AMP(  69)-AMP(  70)
      MATRIX_SSX_NINJGG = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_SSX_NINJGG =MATRIX_SSX_NINJGG+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       