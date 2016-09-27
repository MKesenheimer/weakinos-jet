      subroutine qdqdbar_nInJqdbarqd_realOS_squaredME(fc_result, ichan, helicities, flags)
        implicit none
#include "osres.h"
#include "indices.h"
        integer*8 helicities
        integer flags,i,ichan
        double precision fc_result(2)
        fc_result(:) = 0D0
        ! sfeij and sfekl are defined in set_channel
        SfeSQ1 = osres_sfeij
        SFeSQ2 = osres_sfekl
        if(ichan.ge.1 .and. ichan.le.4) then
          call qdqdbar_nInJqdbarqd_3546_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
        if(ichan.ge.5 .and. ichan.le.8) then
          call qdqdbar_nInJqdbarqd_3645_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
 10     continue
      end
