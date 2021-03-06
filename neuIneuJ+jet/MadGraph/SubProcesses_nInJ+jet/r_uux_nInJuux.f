      SUBROUTINE SMATRIX_UUX_NINJUUX(P1,ANS)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C AND HELICITIES
C FOR THE POINT IN PHASE SPACE P(0:3,NEXTERNAL)
C  
C FOR PROCESS : u u~ -> n1 n2 u u~  
C  
C Crossing   1 is u u~ -> n1 n2 u u~  
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
      REAL*8 MATRIX_UUX_NINJUUX
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
      common/to_Ramps_uux_nInJuux/  amp2,       jamp2

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
      DATA NGRAPHS /  120/          
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
                 T=MATRIX_UUX_NINJUUX(P1,NHEL(1,IHEL),JC(1))            
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
       
       
      REAL*8 FUNCTION MATRIX_UUX_NINJUUX(P,NHEL,IC)
C  
C Generated by MadGraph II                                              
C RETURNS AMPLITUDE SQUARED SUMMED/AVG OVER COLORS
C FOR THE POINT WITH EXTERNAL LINES W(0:6,NEXTERNAL)
C  
C FOR PROCESS : u u~ -> n1 n2 u u~  
C  
      IMPLICIT NONE
C  
C CONSTANTS
C  
      INTEGER    NGRAPHS,    NEIGEN 
      PARAMETER(NGRAPHS= 120,NEIGEN=  2) 
      include "genps.inc"
      include "nexternal.inc"
      include "maxamps.inc"
      INTEGER    NWAVEFUNCS, NCOLOR
      PARAMETER(NWAVEFUNCS= 143, NCOLOR=   2) 
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
      common/to_Ramps_uux_nInJuux/  amp2,       jamp2
      include "coupl.inc"
C  
C COLOR DATA
C  
      DATA Denom(1)/            3/                                       
      DATA(CF(i,1),i=1,2) /     6,   -2/                            
C               T[ 5, 6]T[ 2, 1]                                           
      DATA Denom(2)/            3/                                       
      DATA(CF(i,2),i=1,2) /    -2,    6/                            
C               T[ 5, 1]T[ 2, 6]                                           
C ----------
C BEGIN CODE
C ----------
      CALL IXXXXX(P(0,1),ZERO,NHEL(1),+1*IC(1),W(1,1))        
      CALL OXXXXX(P(0,2),ZERO,NHEL(2),-1*IC(2),W(1,2))        
      CALL OXXXXX(P(0,3),MNI,NHEL(3),+1*IC(3),W(1,3))         
      CALL OXXXXX(P(0,4),MNJ,NHEL(4),+1*IC(4),W(1,4))         
      CALL IXXXXX(P(0,5),ZERO,NHEL(5),-1*IC(5),W(1,5))        
      CALL IXXXXX(P(0,6),ZERO,NHEL(6),-1*IC(6),W(1,6))        
      CALL HIOXXX(W(1,1),W(1,4),GULNJM,MUL,WUL,W(1,7))                                                          
      CALL FSOCXX(W(1,3),W(1,7),GULNIP,ZERO,ZERO,W(1,8))                                                          
      CALL JIOCXX(W(1,5),W(1,8),GG,ZERO,ZERO,W(1,9))    
      CALL IOVXXX(W(1,6),W(1,2),W(1,9),GG,AMP(1))             
      CALL HIOXXX(W(1,1),W(1,4),GURNJM,MUR,WUR,W(1,10))                                                          
      CALL FSOCXX(W(1,3),W(1,10),GURNIP,ZERO,ZERO,W(1,11))                                                          
      CALL JIOCXX(W(1,5),W(1,11),GG,ZERO,ZERO,W(1,12))    
      CALL IOVXXX(W(1,6),W(1,2),W(1,12),GG,AMP(2))             
      CALL HIOCXX(W(1,5),W(1,3),GULNIP,MUL,WUL,W(1,13))                                                          
      CALL JSSXXX(W(1,13),W(1,7),GC,ZERO,ZERO,W(1,14))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,14),GG,AMP(3))             
      CALL HIOCXX(W(1,5),W(1,3),GURNIP,MUR,WUR,W(1,15))                                                          
      CALL JSSXXX(W(1,15),W(1,10),GC,ZERO,ZERO,W(1,16))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,16),GG,AMP(4))             
      CALL FSOXXX(W(1,2),W(1,7),GQLGOP,MGO,WGO,W(1,17))                                                          
      CALL IOSXXX(W(1,6),W(1,17),W(1,13),GQLGOM,AMP(5))         
      CALL IOSXXX(W(1,6),W(1,17),W(1,15),GQRGOM,AMP(6))         
      CALL FSOXXX(W(1,2),W(1,10),GQRGOP,MGO,WGO,W(1,18))                                                          
      CALL IOSXXX(W(1,6),W(1,18),W(1,13),GQLGOM,AMP(7))         
      CALL IOSXXX(W(1,6),W(1,18),W(1,15),GQRGOM,AMP(8))         
      CALL FSOXXX(W(1,4),W(1,13),GULNJM,ZERO,ZERO,W(1,19))                                                          
      CALL JIOXXX(W(1,1),W(1,19),GG,ZERO,ZERO,W(1,20))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,20),GG,AMP(9))             
      CALL FSOXXX(W(1,4),W(1,15),GURNJM,ZERO,ZERO,W(1,21))                                                          
      CALL JIOXXX(W(1,1),W(1,21),GG,ZERO,ZERO,W(1,22))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,22),GG,AMP(10))             
      CALL HIOCXX(W(1,5),W(1,17),GQLGOP,MUL,WUL,W(1,23))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,23),GULNIM,AMP(11))         
      CALL HIOCXX(W(1,5),W(1,17),GQRGOP,MUR,WUR,W(1,24))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,24),GURNIM,AMP(12))         
      CALL HIOCXX(W(1,5),W(1,18),GQLGOP,MUL,WUL,W(1,25))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,25),GULNIM,AMP(13))         
      CALL HIOCXX(W(1,5),W(1,18),GQRGOP,MUR,WUR,W(1,26))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,26),GURNIM,AMP(14))         
      CALL FSICXX(W(1,5),W(1,7),GQLGOP,MGO,WGO,W(1,27))                                                          
      CALL HIOXXX(W(1,27),W(1,2),GQLGOP,MUL,WUL,W(1,28))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,28),GULNIM,AMP(15))         
      CALL HIOXXX(W(1,27),W(1,2),GQRGOP,MUR,WUR,W(1,29))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,29),GURNIM,AMP(16))         
      CALL FSICXX(W(1,5),W(1,10),GQRGOP,MGO,WGO,W(1,30))                                                          
      CALL HIOXXX(W(1,30),W(1,2),GQLGOP,MUL,WUL,W(1,31))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,31),GULNIM,AMP(17))         
      CALL HIOXXX(W(1,30),W(1,2),GQRGOP,MUR,WUR,W(1,32))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,32),GURNIM,AMP(18))         
      CALL HIOCXX(W(1,5),W(1,4),GULNJP,MUL,WUL,W(1,33))                                                          
      CALL FSIXXX(W(1,1),W(1,33),GQLGOM,MGO,WGO,W(1,34))                                                          
      CALL HIOXXX(W(1,34),W(1,2),GQLGOP,MUL,WUL,W(1,35))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,35),GULNIM,AMP(19))         
      CALL HIOXXX(W(1,34),W(1,2),GQRGOP,MUR,WUR,W(1,36))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,36),GURNIM,AMP(20))         
      CALL HIOCXX(W(1,5),W(1,4),GURNJP,MUR,WUR,W(1,37))                                                          
      CALL FSIXXX(W(1,1),W(1,37),GQRGOM,MGO,WGO,W(1,38))                                                          
      CALL HIOXXX(W(1,38),W(1,2),GQLGOP,MUL,WUL,W(1,39))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,39),GULNIM,AMP(21))         
      CALL HIOXXX(W(1,38),W(1,2),GQRGOP,MUR,WUR,W(1,40))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,40),GURNIM,AMP(22))         
      CALL JIOXXX(W(1,1),W(1,2),GG,ZERO,ZERO,W(1,41))     
      CALL HVSXXX(W(1,41),W(1,33),GC,MUL,WUL,W(1,42))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,42),GULNIM,AMP(23))         
      CALL HVSXXX(W(1,41),W(1,37),GC,MUR,WUR,W(1,43))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,43),GURNIM,AMP(24))         
      CALL FSOXXX(W(1,3),W(1,33),GULNIM,ZERO,ZERO,W(1,44))                                                          
      CALL IOVXXX(W(1,6),W(1,44),W(1,41),GG,AMP(25))             
      CALL FSOXXX(W(1,3),W(1,37),GURNIM,ZERO,ZERO,W(1,45))                                                          
      CALL IOVXXX(W(1,6),W(1,45),W(1,41),GG,AMP(26))             
      CALL FVICXX(W(1,5),W(1,41),GG,ZERO,ZERO,W(1,46))    
      CALL HIOCXX(W(1,46),W(1,4),GULNJP,MUL,WUL,W(1,47))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,47),GULNIM,AMP(27))         
      CALL HIOCXX(W(1,46),W(1,4),GURNJP,MUR,WUR,W(1,48))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,48),GURNIM,AMP(28))         
      CALL HIOXXX(W(1,1),W(1,3),GULNIM,MUL,WUL,W(1,49))                                                          
      CALL FSOCXX(W(1,4),W(1,49),GULNJP,ZERO,ZERO,W(1,50))                                                          
      CALL JIOCXX(W(1,5),W(1,50),GG,ZERO,ZERO,W(1,51))    
      CALL IOVXXX(W(1,6),W(1,2),W(1,51),GG,AMP(29))             
      CALL HIOXXX(W(1,1),W(1,3),GURNIM,MUR,WUR,W(1,52))                                                          
      CALL FSOCXX(W(1,4),W(1,52),GURNJP,ZERO,ZERO,W(1,53))                                                          
      CALL JIOCXX(W(1,5),W(1,53),GG,ZERO,ZERO,W(1,54))    
      CALL IOVXXX(W(1,6),W(1,2),W(1,54),GG,AMP(30))             
      CALL JSSXXX(W(1,33),W(1,49),GC,ZERO,ZERO,W(1,55))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,55),GG,AMP(31))             
      CALL JSSXXX(W(1,37),W(1,52),GC,ZERO,ZERO,W(1,56))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,56),GG,AMP(32))             
      CALL FSOXXX(W(1,2),W(1,49),GQLGOP,MGO,WGO,W(1,57))                                                          
      CALL IOSXXX(W(1,6),W(1,57),W(1,33),GQLGOM,AMP(33))         
      CALL IOSXXX(W(1,6),W(1,57),W(1,37),GQRGOM,AMP(34))         
      CALL FSOXXX(W(1,2),W(1,52),GQRGOP,MGO,WGO,W(1,58))                                                          
      CALL IOSXXX(W(1,6),W(1,58),W(1,33),GQLGOM,AMP(35))         
      CALL IOSXXX(W(1,6),W(1,58),W(1,37),GQRGOM,AMP(36))         
      CALL JIOXXX(W(1,1),W(1,44),GG,ZERO,ZERO,W(1,59))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,59),GG,AMP(37))             
      CALL JIOXXX(W(1,1),W(1,45),GG,ZERO,ZERO,W(1,60))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,60),GG,AMP(38))             
      CALL FSIXXX(W(1,1),W(1,13),GQLGOM,MGO,WGO,W(1,61))                                                          
      CALL HIOXXX(W(1,61),W(1,2),GQLGOP,MUL,WUL,W(1,62))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,62),GULNJM,AMP(39))         
      CALL HIOXXX(W(1,61),W(1,2),GQRGOP,MUR,WUR,W(1,63))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,63),GURNJM,AMP(40))         
      CALL FSIXXX(W(1,1),W(1,15),GQRGOM,MGO,WGO,W(1,64))                                                          
      CALL HIOXXX(W(1,64),W(1,2),GQLGOP,MUL,WUL,W(1,65))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,65),GULNJM,AMP(41))         
      CALL HIOXXX(W(1,64),W(1,2),GQRGOP,MUR,WUR,W(1,66))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,66),GURNJM,AMP(42))         
      CALL HVSXXX(W(1,41),W(1,13),GC,MUL,WUL,W(1,67))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,67),GULNJM,AMP(43))         
      CALL HVSXXX(W(1,41),W(1,15),GC,MUR,WUR,W(1,68))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,68),GURNJM,AMP(44))         
      CALL IOVXXX(W(1,6),W(1,19),W(1,41),GG,AMP(45))             
      CALL IOVXXX(W(1,6),W(1,21),W(1,41),GG,AMP(46))             
      CALL HIOCXX(W(1,46),W(1,3),GULNIP,MUL,WUL,W(1,69))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,69),GULNJM,AMP(47))         
      CALL HIOCXX(W(1,46),W(1,3),GURNIP,MUR,WUR,W(1,70))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,70),GURNJM,AMP(48))         
      CALL HIOCXX(W(1,5),W(1,57),GQLGOP,MUL,WUL,W(1,71))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,71),GULNJM,AMP(49))         
      CALL HIOCXX(W(1,5),W(1,57),GQRGOP,MUR,WUR,W(1,72))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,72),GURNJM,AMP(50))         
      CALL HIOCXX(W(1,5),W(1,58),GQLGOP,MUL,WUL,W(1,73))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,73),GULNJM,AMP(51))         
      CALL HIOCXX(W(1,5),W(1,58),GQRGOP,MUR,WUR,W(1,74))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,74),GURNJM,AMP(52))         
      CALL FSICXX(W(1,5),W(1,49),GQLGOP,MGO,WGO,W(1,75))                                                          
      CALL HIOXXX(W(1,75),W(1,2),GQLGOP,MUL,WUL,W(1,76))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,76),GULNJM,AMP(53))         
      CALL HIOXXX(W(1,75),W(1,2),GQRGOP,MUR,WUR,W(1,77))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,77),GURNJM,AMP(54))         
      CALL FSICXX(W(1,5),W(1,52),GQRGOP,MGO,WGO,W(1,78))                                                          
      CALL HIOXXX(W(1,78),W(1,2),GQLGOP,MUL,WUL,W(1,79))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,79),GULNJM,AMP(55))         
      CALL HIOXXX(W(1,78),W(1,2),GQRGOP,MUR,WUR,W(1,80))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,80),GURNJM,AMP(56))         
      CALL IXXXXX(P(0,4),MNJ,NHEL(4),-1*IC(4),W(1,81))         
      CALL OXXXXX(P(0,5),ZERO,NHEL(5),+1*IC(5),W(1,82))        
      CALL HIOXXX(W(1,81),W(1,2),GULNJP,MUL,WUL,W(1,83))                                                          
      CALL JIOXXX(W(1,1),W(1,82),GG,ZERO,ZERO,W(1,84))     
      CALL HVSXXX(W(1,84),W(1,83),GC,MUL,WUL,W(1,85))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,85),GULNIM,AMP(57))         
      CALL HIOXXX(W(1,81),W(1,2),GURNJP,MUR,WUR,W(1,86))                                                          
      CALL HVSXXX(W(1,84),W(1,86),GC,MUR,WUR,W(1,87))     
      CALL IOSXXX(W(1,6),W(1,3),W(1,87),GURNIM,AMP(58))         
      CALL FSOXXX(W(1,3),W(1,83),GULNIM,ZERO,ZERO,W(1,88))                                                          
      CALL IOVXXX(W(1,6),W(1,88),W(1,84),GG,AMP(59))             
      CALL FSOXXX(W(1,3),W(1,86),GURNIM,ZERO,ZERO,W(1,89))                                                          
      CALL IOVXXX(W(1,6),W(1,89),W(1,84),GG,AMP(60))             
      CALL FSIXXX(W(1,1),W(1,83),GQLGOM,MGO,WGO,W(1,90))                                                          
      CALL HIOXXX(W(1,90),W(1,82),GQLGOP,MUL,WUL,W(1,91))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,91),GULNIM,AMP(61))         
      CALL HIOXXX(W(1,90),W(1,82),GQRGOP,MUR,WUR,W(1,92))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,92),GURNIM,AMP(62))         
      CALL FSIXXX(W(1,1),W(1,86),GQRGOM,MGO,WGO,W(1,93))                                                          
      CALL HIOXXX(W(1,93),W(1,82),GQLGOP,MUL,WUL,W(1,94))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,94),GULNIM,AMP(63))         
      CALL HIOXXX(W(1,93),W(1,82),GQRGOP,MUR,WUR,W(1,95))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,95),GURNIM,AMP(64))         
      CALL FVOXXX(W(1,2),W(1,84),GG,ZERO,ZERO,W(1,96))     
      CALL HIOXXX(W(1,81),W(1,96),GULNJP,MUL,WUL,W(1,97))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,97),GULNIM,AMP(65))         
      CALL HIOXXX(W(1,81),W(1,96),GURNJP,MUR,WUR,W(1,98))                                                          
      CALL IOSXXX(W(1,6),W(1,3),W(1,98),GURNIM,AMP(66))         
      CALL JIOXXX(W(1,1),W(1,88),GG,ZERO,ZERO,W(1,99))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,99),GG,AMP(67))             
      CALL JIOXXX(W(1,1),W(1,89),GG,ZERO,ZERO,W(1,100))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,100),GG,AMP(68))             
      CALL FSIXXX(W(1,81),W(1,49),GULNJP,ZERO,ZERO,W(1,101))                                                          
      CALL JIOXXX(W(1,101),W(1,2),GG,ZERO,ZERO,W(1,102))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,102),GG,AMP(69))             
      CALL FSIXXX(W(1,81),W(1,52),GURNJP,ZERO,ZERO,W(1,103))                                                          
      CALL JIOXXX(W(1,103),W(1,2),GG,ZERO,ZERO,W(1,104))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,104),GG,AMP(70))             
      CALL JSSXXX(W(1,83),W(1,49),GC,ZERO,ZERO,W(1,105))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,105),GG,AMP(71))             
      CALL JSSXXX(W(1,86),W(1,52),GC,ZERO,ZERO,W(1,106))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,106),GG,AMP(72))             
      CALL FSOXXX(W(1,82),W(1,49),GQLGOP,MGO,WGO,W(1,107))                                                          
      CALL IOSXXX(W(1,6),W(1,107),W(1,83),GQLGOM,AMP(73))         
      CALL IOSXXX(W(1,6),W(1,107),W(1,86),GQRGOM,AMP(74))         
      CALL FSOXXX(W(1,82),W(1,52),GQRGOP,MGO,WGO,W(1,108))                                                          
      CALL IOSXXX(W(1,6),W(1,108),W(1,83),GQLGOM,AMP(75))         
      CALL IOSXXX(W(1,6),W(1,108),W(1,86),GQRGOM,AMP(76))         
      CALL JIOCXX(W(1,81),W(1,3),GZNIJ,ZMASS,ZWIDTH,W(1,109))                                                          
      CALL FVOXXX(W(1,2),W(1,109),GZU,ZERO,ZERO,W(1,110))    
      CALL JIOXXX(W(1,1),W(1,110),GG,ZERO,ZERO,W(1,111))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,111),GG,AMP(77))             
      CALL FVIXXX(W(1,1),W(1,109),GZU,ZERO,ZERO,W(1,112))    
      CALL JIOXXX(W(1,112),W(1,2),GG,ZERO,ZERO,W(1,113))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,113),GG,AMP(78))             
      CALL FVOXXX(W(1,82),W(1,109),GZU,ZERO,ZERO,W(1,114))    
      CALL IOVXXX(W(1,6),W(1,114),W(1,41),GG,AMP(79))             
      CALL FVOXXX(W(1,82),W(1,41),GG,ZERO,ZERO,W(1,115))     
      CALL IOVXXX(W(1,6),W(1,115),W(1,109),GZU,AMP(80))            
      CALL IOVXXX(W(1,6),W(1,96),W(1,109),GZU,AMP(81))            
      CALL IOVXXX(W(1,6),W(1,110),W(1,84),GG,AMP(82))             
      CALL JIOXXX(W(1,112),W(1,82),GG,ZERO,ZERO,W(1,116))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,116),GG,AMP(83))             
      CALL JIOXXX(W(1,1),W(1,114),GG,ZERO,ZERO,W(1,117))     
      CALL IOVXXX(W(1,6),W(1,2),W(1,117),GG,AMP(84))             
      CALL OXXXXX(P(0,6),ZERO,NHEL(6),+1*IC(6),W(1,118))        
      CALL IOSCXX(W(1,61),W(1,118),W(1,83),GQLGOM,AMP(85))        
      CALL IOSCXX(W(1,64),W(1,118),W(1,83),GQLGOM,AMP(86))        
      CALL IOSCXX(W(1,61),W(1,118),W(1,86),GQRGOM,AMP(87))        
      CALL IOSCXX(W(1,64),W(1,118),W(1,86),GQRGOM,AMP(88))        
      CALL IOSCXX(W(1,90),W(1,118),W(1,13),GQLGOM,AMP(89))        
      CALL IOSCXX(W(1,90),W(1,118),W(1,15),GQRGOM,AMP(90))        
      CALL IOSCXX(W(1,93),W(1,118),W(1,13),GQLGOM,AMP(91))        
      CALL IOSCXX(W(1,93),W(1,118),W(1,15),GQRGOM,AMP(92))        
      CALL IXXXXX(P(0,3),MNI,NHEL(3),-1*IC(3),W(1,119))         
      CALL HIOXXX(W(1,119),W(1,2),GULNIP,MUL,WUL,W(1,120))                                                          
      CALL HVSXXX(W(1,84),W(1,120),GC,MUL,WUL,W(1,121))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,121),GULNJM,AMP(93))         
      CALL HIOXXX(W(1,119),W(1,2),GURNIP,MUR,WUR,W(1,122))                                                          
      CALL HVSXXX(W(1,84),W(1,122),GC,MUR,WUR,W(1,123))     
      CALL IOSXXX(W(1,6),W(1,4),W(1,123),GURNJM,AMP(94))         
      CALL FSOXXX(W(1,4),W(1,120),GULNJM,ZERO,ZERO,W(1,124))                                                          
      CALL IOVXXX(W(1,6),W(1,124),W(1,84),GG,AMP(95))             
      CALL FSOXXX(W(1,4),W(1,122),GURNJM,ZERO,ZERO,W(1,125))                                                          
      CALL IOVXXX(W(1,6),W(1,125),W(1,84),GG,AMP(96))             
      CALL FSIXXX(W(1,1),W(1,120),GQLGOM,MGO,WGO,W(1,126))                                                          
      CALL HIOXXX(W(1,126),W(1,82),GQLGOP,MUL,WUL,W(1,127))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,127),GULNJM,AMP(97))         
      CALL HIOXXX(W(1,126),W(1,82),GQRGOP,MUR,WUR,W(1,128))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,128),GURNJM,AMP(98))         
      CALL FSIXXX(W(1,1),W(1,122),GQRGOM,MGO,WGO,W(1,129))                                                          
      CALL HIOXXX(W(1,129),W(1,82),GQLGOP,MUL,WUL,W(1,130))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,130),GULNJM,AMP(99))         
      CALL HIOXXX(W(1,129),W(1,82),GQRGOP,MUR,WUR,W(1,131))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,131),GURNJM,AMP(100))         
      CALL HIOXXX(W(1,119),W(1,96),GULNIP,MUL,WUL,W(1,132))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,132),GULNJM,AMP(101))         
      CALL HIOXXX(W(1,119),W(1,96),GURNIP,MUR,WUR,W(1,133))                                                          
      CALL IOSXXX(W(1,6),W(1,4),W(1,133),GURNJM,AMP(102))         
      CALL JIOXXX(W(1,1),W(1,124),GG,ZERO,ZERO,W(1,134))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,134),GG,AMP(103))             
      CALL JIOXXX(W(1,1),W(1,125),GG,ZERO,ZERO,W(1,135))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,135),GG,AMP(104))             
      CALL FSIXXX(W(1,119),W(1,7),GULNIP,ZERO,ZERO,W(1,136))                                                          
      CALL JIOXXX(W(1,136),W(1,2),GG,ZERO,ZERO,W(1,137))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,137),GG,AMP(105))             
      CALL FSIXXX(W(1,119),W(1,10),GURNIP,ZERO,ZERO,W(1,138))                                                          
      CALL JIOXXX(W(1,138),W(1,2),GG,ZERO,ZERO,W(1,139))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,139),GG,AMP(106))             
      CALL JSSXXX(W(1,120),W(1,7),GC,ZERO,ZERO,W(1,140))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,140),GG,AMP(107))             
      CALL JSSXXX(W(1,122),W(1,10),GC,ZERO,ZERO,W(1,141))     
      CALL IOVXXX(W(1,6),W(1,82),W(1,141),GG,AMP(108))             
      CALL FSOXXX(W(1,82),W(1,7),GQLGOP,MGO,WGO,W(1,142))                                                          
      CALL IOSXXX(W(1,6),W(1,142),W(1,120),GQLGOM,AMP(109))         
      CALL FSOXXX(W(1,82),W(1,10),GQRGOP,MGO,WGO,W(1,143))                                                          
      CALL IOSXXX(W(1,6),W(1,143),W(1,120),GQLGOM,AMP(110))         
      CALL IOSXXX(W(1,6),W(1,142),W(1,122),GQRGOM,AMP(111))         
      CALL IOSXXX(W(1,6),W(1,143),W(1,122),GQRGOM,AMP(112))         
      CALL IOSCXX(W(1,34),W(1,118),W(1,120),GQLGOM,AMP(113))        
      CALL IOSCXX(W(1,38),W(1,118),W(1,120),GQLGOM,AMP(114))        
      CALL IOSCXX(W(1,34),W(1,118),W(1,122),GQRGOM,AMP(115))        
      CALL IOSCXX(W(1,38),W(1,118),W(1,122),GQRGOM,AMP(116))        
      CALL IOSCXX(W(1,126),W(1,118),W(1,33),GQLGOM,AMP(117))        
      CALL IOSCXX(W(1,126),W(1,118),W(1,37),GQRGOM,AMP(118))        
      CALL IOSCXX(W(1,129),W(1,118),W(1,33),GQLGOM,AMP(119))        
      CALL IOSCXX(W(1,129),W(1,118),W(1,37),GQRGOM,AMP(120))        
      JAMP(   1) = +AMP(   1)+AMP(   2)+AMP(   3)+AMP(   4)+AMP(   9)
     &             +AMP(  10)-AMP(  15)-AMP(  16)-AMP(  17)-AMP(  18)
     &             +AMP(  19)+AMP(  20)+AMP(  21)+AMP(  22)-AMP(  29)
     &             -AMP(  30)-AMP(  31)-AMP(  32)-AMP(  37)-AMP(  38)
     &             -AMP(  39)-AMP(  40)-AMP(  41)-AMP(  42)+AMP(  53)
     &             +AMP(  54)+AMP(  55)+AMP(  56)+AMP(  57)+AMP(  58)
     &             +AMP(  59)+AMP(  60)+AMP(  65)+AMP(  66)-AMP(  73)
     &             -AMP(  74)-AMP(  75)-AMP(  76)-AMP(  81)-AMP(  82)
     &             -AMP(  83)-AMP(  84)-AMP(  85)-AMP(  86)-AMP(  87)
     &             -AMP(  88)-AMP(  93)-AMP(  94)-AMP(  95)-AMP(  96)
     &             -AMP( 101)-AMP( 102)+AMP( 109)+AMP( 110)+AMP( 111)
     &             +AMP( 112)+AMP( 113)+AMP( 114)+AMP( 115)+AMP( 116)
      JAMP(   2) = +AMP(   5)+AMP(   6)+AMP(   7)+AMP(   8)-AMP(  11)
     &             -AMP(  12)-AMP(  13)-AMP(  14)+AMP(  23)+AMP(  24)
     &             +AMP(  25)+AMP(  26)+AMP(  27)+AMP(  28)-AMP(  33)
     &             -AMP(  34)-AMP(  35)-AMP(  36)-AMP(  43)-AMP(  44)
     &             -AMP(  45)-AMP(  46)-AMP(  47)-AMP(  48)+AMP(  49)
     &             +AMP(  50)+AMP(  51)+AMP(  52)+AMP(  61)+AMP(  62)
     &             +AMP(  63)+AMP(  64)-AMP(  67)-AMP(  68)-AMP(  69)
     &             -AMP(  70)-AMP(  71)-AMP(  72)+AMP(  77)+AMP(  78)
     &             +AMP(  79)+AMP(  80)-AMP(  89)-AMP(  90)-AMP(  91)
     &             -AMP(  92)-AMP(  97)-AMP(  98)-AMP(  99)-AMP( 100)
     &             +AMP( 103)+AMP( 104)+AMP( 105)+AMP( 106)+AMP( 107)
     &             +AMP( 108)+AMP( 117)+AMP( 118)+AMP( 119)+AMP( 120)
      MATRIX_UUX_NINJUUX = 0.D0 
      DO I = 1, NCOLOR
          ZTEMP =(0.D0,0.D0)
          DO J = 1, NCOLOR
              ZTEMP = ZTEMP + CF(J,I)*JAMP(J)
          ENDDO
          MATRIX_UUX_NINJUUX =MATRIX_UUX_NINJUUX+ZTEMP*DCONJG(JAMP(I))/DENOM(I)   
      ENDDO
      Do I = 1, NGRAPHS
          amp2(i)=amp2(i)+amp(i)*dconjg(amp(i))
      Enddo
      Do I = 1, NCOLOR
          Jamp2(i)=Jamp2(i)+Jamp(i)*dconjg(Jamp(i))
      Enddo
C      CALL GAUGECHECK(JAMP,ZTEMP,EIGEN_VEC,EIGEN_VAL,NCOLOR,NEIGEN) 
      END
       
       
