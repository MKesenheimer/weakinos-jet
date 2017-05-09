      subroutine sborn_proc(p_born,legs,wgtmunu)
      implicit none
      include "nexternal.inc"
      include "coupl.inc"
      double precision p_born(0:3,nexternal-1)
      double precision wgtmunu(0:3,0:3,nexternal-1)
      double precision wgt2
      integer legs(nexternal-1),lstr,i
      character*20 str
      logical calculatedBorn
      integer skip
      common/cBorn/calculatedBorn,skip
 
      calculatedBorn=.false.
      
      call convert_to_string(nexternal-1,legs,str,lstr)

      ! neutralino chargino+
      if(str.eq."-12nIxJ0") then
         call sborn_cl_001(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-10nIxJ-2") then
         call sborn_cl_002(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."2-1nIxJ0") then
         call sborn_cl_003(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."20nIxJ1") then
         call sborn_cl_004(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."4-3nIxJ0") then
         call sborn_cl_003(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."40nIxJ3") then
         call sborn_cl_004(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-34nIxJ0") then
         call sborn_cl_001(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-30nIxJ-4") then
         call sborn_cl_002(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-1nIxJ-2") then
         call sborn_cl_009(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."02nIxJ1") then
         call sborn_cl_010(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."04nIxJ3") then
         call sborn_cl_010(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-3nIxJ-4") then
         call sborn_cl_009(p_born,wgtmunu,wgt2)
         goto 20

      ! neutralino chargino-
      elseif(str.eq."1-2nIxJ0") then
         call sborn_cl_011(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."10nIxJ2") then
         call sborn_cl_012(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-21nIxJ0") then
         call sborn_cl_013(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-20nIxJ-1") then
         call sborn_cl_014(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-43nIxJ0") then
         call sborn_cl_013(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-40nIxJ-3") then
         call sborn_cl_014(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."3-4nIxJ0") then
         call sborn_cl_011(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."30nIxJ4") then
         call sborn_cl_012(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."01nIxJ2") then
         call sborn_cl_019(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-2nIxJ-1") then
         call sborn_cl_020(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-4nIxJ-3") then
         call sborn_cl_020(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."03nIxJ4") then
         call sborn_cl_019(p_born,wgtmunu,wgt2)
         goto 20
      endif
 20   continue
      end

      subroutine born_color(legs,color)
      implicit none
      include "nexternal.inc"
      integer maxamps
      parameter (maxamps=6000)
      Double Precision amp2001(maxamps), jamp2001(0:maxamps)
      common/to_amps_001/amp2001,jamp2001
      Double Precision amp2002(maxamps), jamp2002(0:maxamps)
      common/to_amps_002/amp2002,jamp2002
      Double Precision amp2003(maxamps), jamp2003(0:maxamps)
      common/to_amps_003/amp2003,jamp2003
      Double Precision amp2004(maxamps), jamp2004(0:maxamps)
      common/to_amps_004/amp2004,jamp2004
      Double Precision amp2009(maxamps), jamp2009(0:maxamps)
      common/to_amps_009/amp2009,jamp2009
      Double Precision amp2010(maxamps), jamp2010(0:maxamps)
      common/to_amps_010/amp2010,jamp2010
      Double Precision amp2011(maxamps), jamp2011(0:maxamps)
      common/to_amps_011/amp2011,jamp2011
      Double Precision amp2012(maxamps), jamp2012(0:maxamps)
      common/to_amps_012/amp2012,jamp2012
      Double Precision amp2013(maxamps), jamp2013(0:maxamps)
      common/to_amps_013/amp2013,jamp2013
      Double Precision amp2014(maxamps), jamp2014(0:maxamps)
      common/to_amps_014/amp2014,jamp2014
      Double Precision amp2019(maxamps), jamp2019(0:maxamps)
      common/to_amps_019/amp2019,jamp2019
      Double Precision amp2020(maxamps), jamp2020(0:maxamps)
      common/to_amps_020/amp2020,jamp2020
      double precision jamp2cum(0:maxamps)
      integer ICOLUP(2,nexternal-1,maxamps)
      integer color(2,nexternal-1),color1(2,nexternal-1)
      double precision random,xtarget
      external random
      integer legs(nexternal-1),lstr,i,j
      character*20 str
      integer iflow,ifl
      
      call convert_to_string(nexternal-1,legs,str,lstr)

      ! neutralino chargino+
      if (str.eq."-12nIxJ0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-10nIxJ-2") then
         include "leshouches_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."20nIxJ1") then
         include "leshouches_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."40nIxJ3") then
         include "leshouches_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-30nIxJ-4") then
         include "leshouches_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."0-1nIxJ-2") then
         include "leshouches_009.inc"
         iflow=nint(jamp2009(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2009(i)
         enddo
         goto 20
      elseif(str.eq."02nIxJ1") then
         include "leshouches_010.inc"
         iflow=nint(jamp2010(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2010(i)
         enddo
         goto 20
      elseif(str.eq."04nIxJ3") then
         include "leshouches_010.inc"
         iflow=nint(jamp2010(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2010(i)
         enddo
         goto 20
      elseif(str.eq."0-3nIxJ-4") then
         include "leshouches_009.inc"
         iflow=nint(jamp2009(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2009(i)
         enddo
         goto 20

      ! neutralino chargino-
      elseif(str.eq."1-2nIxJ0") then
         include "leshouches_011.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."10nIxJ2") then
         include "leshouches_012.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ0") then
         include "leshouches_013.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."-20nIxJ-1") then
         include "leshouches_014.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ0") then
         include "leshouches_013.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."-40nIxJ-3") then
         include "leshouches_014.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ0") then
         include "leshouches_011.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."30nIxJ4") then
         include "leshouches_012.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."01nIxJ2") then
         include "leshouches_019.inc"
         iflow=nint(jamp2019(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2019(i)
         enddo
         goto 20
      elseif(str.eq."0-2nIxJ-1") then
         include "leshouches_020.inc"
         iflow=nint(jamp2020(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2020(i)
         enddo
         goto 20
      elseif(str.eq."0-4nIxJ-3") then
         include "leshouches_020.inc"
         iflow=nint(jamp2020(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2020(i)
         enddo
         goto 20
      elseif(str.eq."03nIxJ4") then
         include "leshouches_019.inc"
         iflow=nint(jamp2019(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2019(i)
         enddo
         goto 20
      endif
      
 20   continue
      xtarget=jamp2cum(iflow)*random()
      ifl=1
      do while(jamp2cum(ifl).lt.xtarget)
         ifl=ifl+1
      enddo
      do i=1,2
         do j=1,nexternal-1
            color(i,j)=ICOLUP(i,j,ifl)
         enddo
      enddo
      end


      subroutine convert_to_string(npart,id,string,lstring)
      implicit none
      integer npart,lstring,i
      character*20 string
      character*8 s
      integer id(npart)
#include "finalstate.h"
      if(final1.ne.id(3) .or. final2.ne.id(4)) then
        print*,"Error in convert_to_string: final states incorrect."
        print*,final1,final2
        print*,id(3),id(4)
        stop
      endif

      do i=1,20
         string(i:i)=' '
      enddo
      lstring=0
      do i=1,npart
#define SHORT
#ifdef SHORT
         ! MK: skip particle 3 and 4 (simplifies the if-queries in sborn_proc)
         if(i.eq.3) then
           string(lstring+1:lstring+2) = 'nI'
           lstring=lstring+2
           goto 31
         endif
         if(i.eq.4) then
           string(lstring+1:lstring+2) = 'xJ'
           lstring=lstring+2
           goto 31
         endif
#endif
         if(id(i).eq.21) id(i)=0
         if(abs(id(i)).le.9) then
            s=char(abs(id(i))+48)
         elseif(abs(id(i)).le.99)then
            s=char(abs(id(i))/10+48)
     &           //char(mod(abs(id(i)),10)+48)
               elseif(abs(id(i)).le.999) then
                  s=char(abs(id(i))/100+48)
     &           //char((abs(id(i))-(abs(id(i))/100)*100)/10+48)
     &           //char(mod(abs(id(i))-(abs(id(i))/100)*100,10)+48)
         elseif(abs(id(i)).le.9999) then
            s=char(abs(id(i))/1000+48)
     &        //char((abs(id(i))-(abs(id(i))/1000)*1000)/100+48)
     &        //char((abs(id(i))-(abs(id(i))/100)*100)/10+48)
     &        //char(mod(abs(id(i))-(abs(id(i))/100)*100,10)+48)
         elseif(abs(id(i)).le.99999) then
           s=char(abs(id(i))/10000+48)
     &        //char((abs(id(i))-(abs(id(i))/10000)*10000)/1000+48)
     &        //char((abs(id(i))-(abs(id(i))/1000)*1000)/100+48)
     &        //char((abs(id(i))-(abs(id(i))/100)*100)/10+48)
     &        //char(mod(abs(id(i))-(abs(id(i))/100)*100,10)+48)
         elseif(abs(id(i)).le.999999) then
            s=char(abs(id(i))/100000+48)
     &        //char((abs(id(i))-(abs(id(i))/100000)*100000)/10000+48)
     &        //char((abs(id(i))-(abs(id(i))/10000)*10000)/1000+48)
     &        //char((abs(id(i))-(abs(id(i))/1000)*1000)/100+48)
     &        //char((abs(id(i))-(abs(id(i))/100)*100)/10+48)
     &        //char(mod(abs(id(i))-(abs(id(i))/100)*100,10)+48)
         elseif(abs(id(i)).le.9999999) then
            s=char(abs(id(i))/1000000+48)
     &       //char((abs(id(i))-(abs(id(i))/1000000)*1000000)/100000+48)
     &       //char((abs(id(i))-(abs(id(i))/100000)*100000)/10000+48)
     &       //char((abs(id(i))-(abs(id(i))/10000)*10000)/1000+48)
     &       //char((abs(id(i))-(abs(id(i))/1000)*1000)/100+48)
     &       //char((abs(id(i))-(abs(id(i))/100)*100)/10+48)
     &       //char(mod(abs(id(i))-(abs(id(i))/100)*100,10)+48)
         else
            write(*,*) 'error, particle ID is too large',abs(id(i))
         endif
         if(id(i).ge.0) then
            if(id(i).le.9) then
               string(lstring+1:lstring+1)=s
               lstring=lstring+1
            elseif(id(i).le.99) then
               string(lstring+1:lstring+2)=s
               lstring=lstring+2
            elseif(id(i).le.999) then
               string(lstring+1:lstring+3)=s
               lstring=lstring+3
            elseif(id(i).le.9999) then
              string(lstring+1:lstring+4)=s
              lstring=lstring+4
            elseif(id(i).le.99999) then
              string(lstring+1:lstring+5)=s
              lstring=lstring+5
            elseif(id(i).le.999999) then
              string(lstring+1:lstring+6)=s
              lstring=lstring+6
            elseif(id(i).le.9999999) then
              string(lstring+1:lstring+7)=s
              lstring=lstring+7
            endif
         else
            if(abs(id(i)).le.9) then
               string(lstring+1:lstring+2)='-'//s
               lstring=lstring+2
            elseif(abs(id(i)).le.99) then
               string(lstring+1:lstring+3)='-'//s
               lstring=lstring+3
            elseif(abs(id(i)).le.999) then
               string(lstring+1:lstring+4)='-'//s
               lstring=lstring+4
            elseif(abs(id(i)).le.9999) then
               string(lstring+1:lstring+5)='-'//s
               lstring=lstring+5
            elseif(abs(id(i)).le.99999) then
               string(lstring+1:lstring+6)='-'//s
               lstring=lstring+6
            elseif(abs(id(i)).le.999999) then
               string(lstring+1:lstring+7)='-'//s
               lstring=lstring+7
            elseif(abs(id(i)).le.9999999) then
               string(lstring+1:lstring+8)='-'//s
               lstring=lstring+8
            endif
         endif
#ifdef SHORT
 31      continue
#endif
      enddo
      end
