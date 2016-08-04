      subroutine qdqubar_nInJqubarqd_realOS_squaredME(ampos, helicities, flags)
        implicit none
#include "osres.h"
#include "indices.h"
        integer*8 helicities
        integer flags,i,j,k
        double precision ampos(2,cnosres),temp(2)
        ampos(:,:) = 0D0
        temp(:) = 0D0
        do i=1,cnosres
          if(osresID(i).eq."l35l46") then
            SfeSQ1=1
            SfeSQ2=1
            call qdqubar_nInJqubarqd_3546_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r35l46") then
            SfeSQ1=2
            SfeSQ2=1
            call qdqubar_nInJqubarqd_3645_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."l35r46") then
            SfeSQ1=1
            SfeSQ2=2
            call qdqubar_nInJqubarqd_3546_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r35r46") then
            SfeSQ1=2
            SfeSQ2=2
            call qdqubar_nInJqubarqd_3546_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."l36l45") then
            SfeSQ1=1
            SfeSQ2=1
            call qdqubar_nInJqubarqd_3645_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r36l45") then
            SfeSQ1=2
            SfeSQ2=1
            call qdqubar_nInJqubarqd_3645_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."l36r45") then
            SfeSQ1=1
            SfeSQ2=2
            call qdqubar_nInJqubarqd_3546_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
          if(osresID(i).eq."r36r45") then
            SfeSQ1=2
            SfeSQ2=2
            call qdqubar_nInJqubarqd_3546_SquaredME(temp, helicities, flags)
            ampos(:,i) = temp(:)
          endif
        enddo
      end
