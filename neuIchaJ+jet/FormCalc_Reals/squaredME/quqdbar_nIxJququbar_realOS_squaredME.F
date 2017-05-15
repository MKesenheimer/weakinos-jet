      subroutine quqdbar_nIxJququbar_realOS_squaredME(fc_result, ichan, helicities, flags)
        implicit none
#include "osres.h"
#include "indices.h"
        integer*8 helicities
        integer flags,i,ichan
        double precision fc_result(2)
        fc_result(:) = 0D0
        ! sfeij and sfekl get defined in subroutine set_channel
        if(ichan.ge.1 .and. ichan.le.4) then
	Sq1 = osres_sfeij
	Sq2 = osres_sfekl
          call quqdbar_nIxJququbar_3546_Sq1Sq2_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
        if(ichan.ge.9 .and. ichan.le.10) then
	Sq1 = osres_sfeij
          call quqdbar_nIxJququbar_356_Sq1_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
        if(ichan.ge.11 .and. ichan.le.12) then
	Sq1 = osres_sfeij
          call quqdbar_nIxJququbar_365_Sq1_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
 10     continue
      end
