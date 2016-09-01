c############### generate histograms ###################################
      subroutine histogram(name,nbins,xm,xp,xval,weight)
        implicit none
#include "hist.h"
        character*(*) name
        integer k,nbins
        double precision xm,xp
        double precision xval,weight
        integer st,ind,iun,ncalls(nhistsmax)
        integer hits(nhistsmax,0:nbinsmax)
        double precision y(nhistsmax,0:nbinsmax),x(nhistsmax,0:nbinsmax)
        double precision yint(nhistsmax,0:nbinsmax)
        save y, x, yint, hits
        data ncalls/nhistsmax * 0/
        save ncalls
        logical init(nhistsmax)
        data init/nhistsmax * .true./
        
        call index(name,ind,iun)
        if(init(ind)) then
          do k=0,nbins
            y(ind,k) = 0D0
            yint(ind,k) = 0D0
            hits(ind,k) = 0
          enddo
          init(ind) = .false.
        endif
        
        do k=0,nbins
          x(ind,k) = xm + k*(xp-xm)/nbins
        enddo
        
        do k=0,(nbins-1)
          if(xval.ge.x(ind,k) .and. xval.lt.x(ind,k+1)) then
            hits(ind,k) = hits(ind,k) + 1
            y(ind,k) = y(ind,k) + weight
          endif
        enddo
        
        ncalls(ind) = ncalls(ind) + 1
        if(mod(ncalls(ind),100).eq.0) then
          do k=0,(nbins-1)
            if(hits(ind,k).gt.0) then
              yint(ind,k) = y(ind,k)/hits(ind,k)
            endif
          enddo
          
          open(iun, file=trim(adjustl(name))//'.top',status='replace')
          write(iun,*) " # ",trim(name),"  index ",ind
          do k=0,(nbins-1)
            write(iun,*) x(ind,k),x(ind,k+1),yint(ind,k) !,yinterr(k)
          enddo
          close(iun)
        endif  
      end
      
      subroutine index(name,ind,iun)
        implicit none
#include "hist.h"
        character*(*) name
        integer ind,iun
        integer i
        do i=1,nhistsmax
          if(trim(histlist(i)).eq.trim(name)) then
            ind = i
            iun = unitlist(i)
            return
          endif  
        enddo
        nhists = nhists + 1
        histlist(nhists) = name
        !call newunit(iun)
        iun = ind + 50
        unitlist(nhists) = iun
        ind = nhists
      end
c#######################################################################