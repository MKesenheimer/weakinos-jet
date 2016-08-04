      subroutine quqd_nInJqdqu_realOS_squaredME(ampos, helicities, flags)
        implicit none
#include "osres.h"
#include "indices.h"
        integer*8 helicities
        integer flags,i,j,k
        double precision ampos(2,cnosres),temp(2)
        ampos(:,:) = 0D0
        temp(:) = 0D0
        do i=1,nosres
          if(osresID(i).eq."l35l46") then
            Sfe6=1
            Sfe7=1
            call quqd_nInJqdqu_l35l46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r35l46") then
            Sfe6=2
            Sfe7=1
            call quqd_nInJqdqu_r35l46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."l35r46") then
            Sfe6=1
            Sfe7=2
            call quqd_nInJqdqu_l35r46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r35r46") then
            Sfe6=2
            Sfe7=2
            call quqd_nInJqdqu_r35r46_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."l36l45") then
            Sfe6=1
            Sfe7=1
            call quqd_nInJqdqu_l36l45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r36l45") then
            Sfe6=2
            Sfe7=1
            call quqd_nInJqdqu_r36l45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."l36r45") then
            Sfe6=1
            Sfe7=2
            call quqd_nInJqdqu_l36r45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r36r45") then
            Sfe6=2
            Sfe7=2
            call quqd_nInJqdqu_r36r45_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
        enddo
      end
