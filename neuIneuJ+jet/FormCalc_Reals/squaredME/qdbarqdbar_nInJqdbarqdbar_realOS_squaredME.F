      subroutine qdbarqdbar_nInJqdbarqdbar_realOS_squaredME(fc_result, ichan, helicities, flags)
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
        if(ichan.eq.1) then
          call qdbarqdbar_nInJqdbarqdbar_3546_SquaredME(fc_result, helicities, flags)
          goto 10
        endif
 10     continue
      end
