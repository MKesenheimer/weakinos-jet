      subroutine sreal_proc(p,legs,wgt)
      implicit none
      include "nexternal.inc"
      double precision p(0:3,nexternal),wgt
      integer legs(nexternal)
      
      if(abs(legs(3)).eq.abs(legs(4))) then
        call sreal_proc_xixi(p,legs,wgt)
      else
        call sreal_proc_xixj(p,legs,wgt)
      endif
      
      return
      end
      
      
      subroutine real_color(legs,color)
      implicit none
      include "nexternal.inc"
      integer color(2,nexternal)
      integer legs(nexternal)

      if(abs(legs(3)).eq.abs(legs(4))) then
        call real_color_xixi(legs,color)
      else
        call real_color_xixj(legs,color)
      endif
      
      return
      end
      
      
      
      
