      subroutine quqdbar_xIxJquqdbar_realOS_squaredME(fc_result, ichan, helicities, flags)
        implicit none
#include "osres.h"
#include "indices.h"
        integer*8 helicities
        integer flags,i,ichan
        double precision fc_result(2)
        fc_result(:) = 0D0
        ! sfeij and sfekl get defined in subroutine set_channel
        if(ichan.ge.13 .and. ichan.le.14) then
	Sq1 = osres_sfeij
          call quqdbar_xIxJquqdbar_456_Sq1_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
        if(ichan.ge.15 .and. ichan.le.16) then
	Sq1 = osres_sfeij
          call quqdbar_xIxJquqdbar_465_Sq1_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
 10     continue
      end
