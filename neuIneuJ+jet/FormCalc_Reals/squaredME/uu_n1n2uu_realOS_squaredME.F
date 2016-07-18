      subroutine uu_n1n2uu_realOS_squaredME(ampos, helicities, flags)
        implicit none
#include "osres.h"
        integer*8 helicities
        integer flags,i,j,k
        double precision ampos(2,cnosres),temp(2)
        ampos(:,:) = 0D0
        temp(:) = 0D0
        do i=1,nosres
          if(osresID(i).eq."L35L46") then
            call uu_n1n2uu_L35L46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."L35R46") then
            call uu_n1n2uu_L35R46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."R35L46") then
            call uu_n1n2uu_R35L46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."R35R46") then
            call uu_n1n2uu_R35R46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."L36L45") then
            call uu_n1n2uu_L36L45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."L36R45") then
            call uu_n1n2uu_L36R45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."R36L45") then
            call uu_n1n2uu_R36L45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."R36R45") then
            call uu_n1n2uu_R36R45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
        enddo
      end
