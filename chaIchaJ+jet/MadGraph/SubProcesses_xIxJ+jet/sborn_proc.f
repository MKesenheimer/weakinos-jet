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

      ! different final states
      if(str.eq."-11xIxJ0") then
         call sborn_cl_001(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-10xIxJ-1") then
         call sborn_cl_002(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."1-1xIxJ0") then
         call sborn_cl_003(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."10xIxJ1") then
         call sborn_cl_004(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-22xIxJ0") then
         call sborn_cl_005(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-20xIxJ-2") then
         call sborn_cl_006(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."2-2xIxJ0") then
         call sborn_cl_007(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."20xIxJ2") then
         call sborn_cl_008(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-44xIxJ0") then
         call sborn_cl_005(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-40xIxJ-4") then
         call sborn_cl_006(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."4-4xIxJ0") then
         call sborn_cl_007(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."40xIxJ4") then
         call sborn_cl_008(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-33xIxJ0") then
         call sborn_cl_001(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-30xIxJ-3") then
         call sborn_cl_002(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."3-3xIxJ0") then
         call sborn_cl_003(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."30xIxJ3") then
         call sborn_cl_004(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-55xIxJ0") then
         call sborn_cl_001(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-50xIxJ-5") then
         call sborn_cl_002(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."5-5xIxJ0") then
         call sborn_cl_003(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."50xIxJ5") then
         call sborn_cl_004(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-1xIxJ-1") then
         call sborn_cl_021(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."01xIxJ1") then
         call sborn_cl_022(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-2xIxJ-2") then
         call sborn_cl_023(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."02xIxJ2") then
         call sborn_cl_024(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-4xIxJ-4") then
         call sborn_cl_023(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."04xIxJ4") then
         call sborn_cl_024(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-3xIxJ-3") then
         call sborn_cl_021(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."03xIxJ3") then
         call sborn_cl_022(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-5xIxJ-5") then
         call sborn_cl_021(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."05xIxJ5") then
         call sborn_cl_022(p_born,wgtmunu,wgt2)
         goto 20

      ! same final state
      elseif(str.eq."-11xIxI0") then
         call sborn_cl_011(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-10xIxI-1") then
         call sborn_cl_012(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."1-1xIxI0") then
         call sborn_cl_013(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."10xIxI1") then
         call sborn_cl_014(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-22xIxI0") then
         call sborn_cl_015(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-20xIxI-2") then
         call sborn_cl_016(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."2-2xIxI0") then
         call sborn_cl_017(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."20xIxI2") then
         call sborn_cl_018(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-44xIxI0") then
         call sborn_cl_015(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-40xIxI-4") then
         call sborn_cl_016(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."4-4xIxI0") then
         call sborn_cl_017(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."40xIxI4") then
         call sborn_cl_018(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-33xIxI0") then
         call sborn_cl_011(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-30xIxI-3") then
         call sborn_cl_012(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."3-3xIxI0") then
         call sborn_cl_013(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."30xIxI3") then
         call sborn_cl_014(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-55xIxI0") then
         call sborn_cl_011(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."-50xIxI-5") then
         call sborn_cl_012(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."5-5xIxI0") then
         call sborn_cl_013(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."50xIxI5") then
         call sborn_cl_014(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-1xIxI-1") then
         call sborn_cl_031(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."01xIxI1") then
         call sborn_cl_032(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-2xIxI-2") then
         call sborn_cl_033(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."02xIxI2") then
         call sborn_cl_034(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-4xIxI-4") then
         call sborn_cl_033(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."04xIxI4") then
         call sborn_cl_034(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-3xIxI-3") then
         call sborn_cl_031(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."03xIxI3") then
         call sborn_cl_032(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."0-5xIxI-5") then
         call sborn_cl_031(p_born,wgtmunu,wgt2)
         goto 20
      elseif(str.eq."05xIxI5") then
         call sborn_cl_032(p_born,wgtmunu,wgt2)
         goto 20
      endif

 20   continue
      end

      subroutine born_color(legs,color)
      implicit none
      include "nexternal.inc"
      integer maxamps
      parameter(maxamps=6000)
      Double Precision amp2001(maxamps), jamp2001(0:maxamps)
      common/to_amps_001/amp2001,jamp2001
      Double Precision amp2002(maxamps), jamp2002(0:maxamps)
      common/to_amps_002/amp2002,jamp2002
      Double Precision amp2003(maxamps), jamp2003(0:maxamps)
      common/to_amps_003/amp2003,jamp2003
      Double Precision amp2004(maxamps), jamp2004(0:maxamps)
      common/to_amps_004/amp2004,jamp2004
      Double Precision amp2005(maxamps), jamp2005(0:maxamps)
      common/to_amps_005/amp2005,jamp2005
      Double Precision amp2006(maxamps), jamp2006(0:maxamps)
      common/to_amps_006/amp2006,jamp2006
      Double Precision amp2007(maxamps), jamp2007(0:maxamps)
      common/to_amps_007/amp2007,jamp2007
      Double Precision amp2008(maxamps), jamp2008(0:maxamps)
      common/to_amps_008/amp2008,jamp2008
      Double Precision amp2021(maxamps), jamp2021(0:maxamps)
      common/to_amps_021/amp2021,jamp2021
      Double Precision amp2022(maxamps), jamp2022(0:maxamps)
      common/to_amps_022/amp2022,jamp2022
      Double Precision amp2023(maxamps), jamp2023(0:maxamps)
      common/to_amps_023/amp2023,jamp2023
      Double Precision amp2024(maxamps), jamp2024(0:maxamps)
      common/to_amps_024/amp2024,jamp2024
      Double Precision amp2011(maxamps), jamp2011(0:maxamps)
      common/to_amps_011/amp2011,jamp2011
      Double Precision amp2012(maxamps), jamp2012(0:maxamps)
      common/to_amps_012/amp2012,jamp2012
      Double Precision amp2013(maxamps), jamp2013(0:maxamps)
      common/to_amps_013/amp2013,jamp2013
      Double Precision amp2014(maxamps), jamp2014(0:maxamps)
      common/to_amps_014/amp2014,jamp2014
      Double Precision amp2015(maxamps), jamp2015(0:maxamps)
      common/to_amps_015/amp2015,jamp2015
      Double Precision amp2016(maxamps), jamp2016(0:maxamps)
      common/to_amps_016/amp2016,jamp2016
      Double Precision amp2017(maxamps), jamp2017(0:maxamps)
      common/to_amps_017/amp2017,jamp2017
      Double Precision amp2018(maxamps), jamp2018(0:maxamps)
      common/to_amps_018/amp2018,jamp2018
      Double Precision amp2031(maxamps), jamp2031(0:maxamps)
      common/to_amps_031/amp2031,jamp2031
      Double Precision amp2032(maxamps), jamp2032(0:maxamps)
      common/to_amps_032/amp2032,jamp2032
      Double Precision amp2033(maxamps), jamp2033(0:maxamps)
      common/to_amps_033/amp2033,jamp2033
      Double Precision amp2034(maxamps), jamp2034(0:maxamps)
      common/to_amps_034/amp2034,jamp2034
      double precision jamp2cum(0:maxamps)
      integer ICOLUP(2,nexternal-1,maxamps)
      integer color(2,nexternal-1),color1(2,nexternal-1)
      double precision random,xtarget
      external random
      integer legs(nexternal-1),lstr,i,j
      character*20 str
      integer iflow,ifl
      
      call convert_to_string(nexternal-1,legs,str,lstr)

      ! different final state
      if(str.eq."-11xIxJ0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-10xIxJ-1") then
         include "leshouches_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."10xIxJ1") then
         include "leshouches_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ0") then
         include "leshouches_005.inc"
         iflow=nint(jamp2005(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2005(i)
         enddo
         goto 20
      elseif(str.eq."-20xIxJ-2") then
         include "leshouches_006.inc"
         iflow=nint(jamp2006(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2006(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ0") then
         include "leshouches_007.inc"
         iflow=nint(jamp2007(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2007(i)
         enddo
         goto 20
      elseif(str.eq."20xIxJ2") then
         include "leshouches_008.inc"
         iflow=nint(jamp2008(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2008(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ0") then
         include "leshouches_005.inc"
         iflow=nint(jamp2005(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2005(i)
         enddo
         goto 20
      elseif(str.eq."-40xIxJ-4") then
         include "leshouches_006.inc"
         iflow=nint(jamp2006(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2006(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ0") then
         include "leshouches_007.inc"
         iflow=nint(jamp2007(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2007(i)
         enddo
         goto 20
      elseif(str.eq."40xIxJ4") then
         include "leshouches_008.inc"
         iflow=nint(jamp2008(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2008(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-30xIxJ-3") then
         include "leshouches_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."30xIxJ3") then
         include "leshouches_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-50xIxJ-5") then
         include "leshouches_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."50xIxJ5") then
         include "leshouches_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."0-1xIxJ-1") then
         include "leshouches_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif(str.eq."01xIxJ1") then
         include "leshouches_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20
      elseif(str.eq."0-2xIxJ-2") then
         include "leshouches_023.inc"
         iflow=nint(jamp2023(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2023(i)
         enddo
         goto 20
      elseif(str.eq."02xIxJ2") then
         include "leshouches_024.inc"
         iflow=nint(jamp2024(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2024(i)
         enddo
         goto 20
      elseif(str.eq."0-4xIxJ-4") then
         include "leshouches_023.inc"
         iflow=nint(jamp2023(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2023(i)
         enddo
         goto 20
      elseif(str.eq."04xIxJ4") then
         include "leshouches_024.inc"
         iflow=nint(jamp2024(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2024(i)
         enddo
         goto 20
      elseif(str.eq."0-3xIxJ-3") then
         include "leshouches_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif(str.eq."03xIxJ3") then
         include "leshouches_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20
      elseif(str.eq."0-5xIxJ-5") then
         include "leshouches_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif(str.eq."05xIxJ5") then
         include "leshouches_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20

      ! same final state
      elseif(str.eq."-11xIxI0") then
         include "leshouches_001.inc" ! this is ok!
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."-10xIxI-1") then
         include "leshouches_002.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."10xIxI1") then
         include "leshouches_004.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI0") then
         include "leshouches_005.inc"
         iflow=nint(jamp2015(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2015(i)
         enddo
         goto 20
      elseif(str.eq."-20xIxI-2") then
         include "leshouches_006.inc"
         iflow=nint(jamp2016(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2016(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI0") then
         include "leshouches_007.inc"
         iflow=nint(jamp2017(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2017(i)
         enddo
         goto 20
      elseif(str.eq."20xIxI2") then
         include "leshouches_008.inc"
         iflow=nint(jamp2018(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2018(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI0") then
         include "leshouches_005.inc"
         iflow=nint(jamp2015(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2015(i)
         enddo
         goto 20
      elseif(str.eq."-40xIxI-4") then
         include "leshouches_006.inc"
         iflow=nint(jamp2016(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2016(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI0") then
         include "leshouches_007.inc"
         iflow=nint(jamp2017(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2017(i)
         enddo
         goto 20
      elseif(str.eq."40xIxI4") then
         include "leshouches_008.inc"
         iflow=nint(jamp2018(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2018(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."-30xIxI-3") then
         include "leshouches_002.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."30xIxI3") then
         include "leshouches_004.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI0") then
         include "leshouches_001.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."-50xIxI-5") then
         include "leshouches_002.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI0") then
         include "leshouches_003.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."50xIxI5") then
         include "leshouches_004.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."0-1xIxI-1") then
         include "leshouches_021.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif(str.eq."01xIxI1") then
         include "leshouches_022.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
         enddo
         goto 20
      elseif(str.eq."0-2xIxI-2") then
         include "leshouches_023.inc"
         iflow=nint(jamp2033(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2033(i)
         enddo
         goto 20
      elseif(str.eq."02xIxI2") then
         include "leshouches_024.inc"
         iflow=nint(jamp2034(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2034(i)
         enddo
         goto 20
      elseif(str.eq."0-4xIxI-4") then
         include "leshouches_023.inc"
         iflow=nint(jamp2033(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2033(i)
         enddo
         goto 20
      elseif(str.eq."04xIxI4") then
         include "leshouches_024.inc"
         iflow=nint(jamp2034(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2034(i)
         enddo
         goto 20
      elseif(str.eq."0-3xIxI-3") then
         include "leshouches_021.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif(str.eq."03xIxI3") then
         include "leshouches_022.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
         enddo
         goto 20
      elseif(str.eq."0-5xIxI-5") then
         include "leshouches_021.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif(str.eq."05xIxI5") then
         include "leshouches_022.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
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
           string(lstring+1:lstring+2) = 'xI'
           lstring=lstring+2
           goto 31
         endif
         if(abs(final1).ne.abs(final2)) then
           if(i.eq.4) then
             string(lstring+1:lstring+2) = 'xJ'
             lstring=lstring+2
             goto 31
           endif
         else
           if(i.eq.4) then
             string(lstring+1:lstring+2) = 'xI'
             lstring=lstring+2
             goto 31
           endif
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
