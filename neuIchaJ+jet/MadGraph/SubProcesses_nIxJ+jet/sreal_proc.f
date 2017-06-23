      subroutine sreal_proc(p,legs,wgt)
      implicit none
      include "nexternal.inc"
      double precision p(0:3,nexternal),wgt
      integer legs(nexternal)
      
      if(legs(3).lt.0 .or. legs(4).lt.0) then
        call sreal_proc_nixjm(p,legs,wgt)
      else
        call sreal_proc_nixjp(p,legs,wgt)
      endif
      
      return
      end
      
      
      subroutine real_color(legs,color)
      implicit none
      include "nexternal.inc"
      integer color(2,nexternal)
      integer legs(nexternal)

      if(legs(3).lt.0 .or. legs(4).lt.0) then
        call real_color_nixjm(legs,color)
      else
        call real_color_nixjp(legs,color)
      endif
      
      return
      end
      
      
      
      
