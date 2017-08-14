c############### sigosres.f ############################################
c last modified by MK, 23.05.2016
c weakino pair + jet production
c new file on the basis of sigremnants.f, revision 3154
c changes not marked

c############### function sigosres #####################################
c Function to be integrated with mint, used to generate
c non-singular contributions using standard mint-gen method
c These contributions arise from real graphs without singular regions,
c or, when damping of R/B value is adopted, from the remnant of the
c damping
c retval is the function return value
c retvavl0 is an 'avatar' function the has similar value, but is much
c easier to compute (i.e. the Born term in this case)
c TODO:
c imode = 0 compute retval0 only.
c imode = 1 compute retval, retval0
c return value: output, 0: success; 1: retval0 was not computed
c                   (this function does not support an avatar function)
      function sigosres(xx,ww1,ifirst,imode,retval,retval0)
        implicit none
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_kn.h"
#include "pwhg_rad.h"
#include "pwhg_flg.h"
#include "pwhg_math.h"
c keep this order
#include "osres.h"
#include "pwhg_flst_add.h"
#include "pwhg_rad_add.h"
        integer sigosres,imode
        double precision retval,retval0,xx(ndiminteg),ww1
        integer ifirst,ichan,idi
        integer lset
        double precision xjac
        ! temporary results for calc. of resonant contributions
        double precision sigosres_contr
        ! temporary arrays
        double precision temp_arr(nosres)
        integer temp_sign(nosres)
        ! sum only over relevant on-shell channels
        integer nos
        
        ! set this to zero: radiation is always generated from production 
        ! process for osres contributions
        kn_resemitter = 0
     
        if(ifirst.eq.2) then
          call addupweightsosres(retval)
          if(flg_nlotest) call pwhgaccumup
          return
        endif

        call setscalesbtilde
        ! we need the last random-numbers for the radiation-generation
        do idi=1,ndiminteg
          rad_xradosres(idi) = xx(idi)
        enddo

        ! reset
        retval = 0D0
        rad_osres_arr(:,:) = 0D0
        
        ! TODO: use smartsig for sigosres_contr
    
        ! use the generic phase-space here,
        ! provide tan-mapping for the resonant particles
        ! sum over the resonances
#define VERSION1
#ifdef VERSION1
        do ichan=1,nosres
          ! reset result
          sigosres_contr = 0D0
          do lset=1,flst_nosres
            call real_osres_phsp(xx,flst_osres(:,lset),ichan)
            xjac = kn_jacreal*ww1*hc2
            call sigreal_osres(xjac,lset,ichan,rad_osres_arr(lset,ichan))
            ! sigosres_contr is summed over all processes for a given channel
            sigosres_contr = sigosres_contr+rad_osres_arr(lset,ichan)
          enddo
          call transfersign(rad_osres_arr(:,ichan),rad_osres_sign(:,ichan),flst_nosres)
          if(flg_nlotest) then
            call analysis_driver(sigosres_contr,1)
          endif
          ! retval is summed over all processes and over all channels
          retval = retval + dabs(sigosres_contr)
        enddo
#endif
! should be faster
! TODO: gives wrong results!
#ifdef VERSION2
        do lset=1,flst_nosres
          ! reset result
          sigosres_contr = 0D0
          ! check if gluino single resonances can occur (sum over nosres1 + nosres2)
          ! if not, sum over double squark resonances only (sum over nosres1)
#if defined(NINJ_JET)
          if(flst_osres(1,lset).eq.-flst_osres(2,lset) .and.
     &       flst_osres(5,lset).eq.-flst_osres(6,lset) .and.
     &       flst_osres(1,lset).ne.0.and.flst_osres(5,lset).ne.0) then
#elif defined(NIXJ_JET)
          if((flst_osres(1,lset).lt.0.and.flst_osres(2,lset).gt.0) .or.
     &       (flst_osres(1,lset).gt.0.and.flst_osres(2,lset).lt.0)) then
#elif defined(XIXJ_JET)
          if((((flst_osres(5,lset).gt.0.and.flst_osres(6,lset).lt.0) .or.
     &         (flst_osres(5,lset).lt.0.and.flst_osres(6,lset).gt.0)) .and.
     &          mod(flst_osres(5,lset)+flst_osres(6,lset),2).ne.0)) then
#endif
            nos = nosres1 + nosres2
          else
            nos = nosres1
          endif
          do ichan=1,nos
            call real_osres_phsp(xx,flst_osres(:,lset),ichan)
            xjac = kn_jacreal*ww1*hc2
            call sigreal_osres(xjac,lset,ichan,rad_osres_arr(lset,ichan))
            ! sigosres_contr is summed over all channels for a given process
            sigosres_contr = sigosres_contr+rad_osres_arr(lset,ichan)
            temp_arr(ichan) = rad_osres_arr(lset,ichan)
          enddo
          call transfersign(temp_arr(:),temp_sign(:),nosres)
          rad_osres_sign(lset,:) = temp_sign(:)
          if(flg_nlotest) then
            call analysis_driver(sigosres_contr,1)
          endif
          ! retval is summed over all processes and over all channels
          retval = retval + dabs(sigosres_contr)
        enddo
#endif

        ! -> return succes
        sigosres = 1
      end
c############### end function sigosres #################################

c############### subroutine sigreal_osres ##############################
c contributions from real graphs that do not have a singular region
      subroutine sigreal_osres(xjac,lset,ichan,r0)
        implicit none
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_kn.h"
#include "pwhg_rad.h"
#include "pwhg_flg.h"
#include "pwhg_pdf.h"
c keep this order
#include "osres.h"
#include "pwhg_flst_add.h"
#include "pwhg_rad_add.h"
        double precision xjac,r0
        integer lset
        double precision pdf1(-pdf_nparton:pdf_nparton)
        double precision pdf2(-pdf_nparton:pdf_nparton)
        integer ichan

        ! first check if the jacobian is 0
        ! if yes return immediately and save time
        if(xjac.eq.0D0) then 
          r0 = 0D0
          return
        endif
      
        ! compute graphs
        call pdfcall(1,kn_x1,pdf1)
        call pdfcall(2,kn_x2,pdf2)
        call setosresreal(kn_cmpreal,flst_osres(1,lset),ichan,r0)
        r0 = r0*xjac*pdf1(flst_osres(1,lset))*pdf2(flst_osres(2,lset))
      end
c############### end subroutine sigreal_osres ##########################

c############### subroutine compare_vecs_osres #########################
c slightliy modified copy of compare_vecs_reg
c make sure to avoid 0-amplitudes correctly
      subroutine compare_vecs_osres(nmomset,lset,res,lsetpr,cprop,iret)
        implicit none
#include "nlegborn.h"
#include "pwhg_flst.h"
        double precision ep
        parameter (ep=1d-12)
        integer nmomset,lset,lsetpr,iret,j,k
        double precision res(nmomset,*),cprop,rat

        ! added this section
        !===============================================================
        if(lset.eq.1) then
          do j=1,nmomset
            if(res(j,lset).ne.0) then
              iret=-1
              return
            endif
          enddo
          iret   = 1 ! this is important
          lsetpr = 1 ! by default, take it prop to first flst with prop-fact 0
          cprop  = 0D0
          return
        endif
        !===============================================================

        do j=1,lset-1
          ! added this section
          !=============================================================
          ! if the res of the amp-routine is 0:
          ! make sure not to divide by 0 (is independent of momentum-config):
          if(res(1,lset).eq.0) then
            iret   = 1 ! this is important
            lsetpr = 1 ! by default, take it prop to first flst with prop-fact 0
            cprop  = 0D0
            return
          endif
          if(res(1,j).eq.0D0) goto 10 !no need to compare to a 0-result
          !=============================================================

          rat = res(1,lset)/res(1,j)
          do k=1,nmomset
            if(abs(1D0-res(k,lset)/res(k,j)/rat).gt.ep) goto 10
          enddo
          if(abs(1D0-rat).lt.ep) then
            iret  = 0
            cprop = 1D0
          else
            iret  = 1
            cprop = rat
          endif
          lsetpr = j
          return
 10       continue
        enddo
        iret = -1
      end
c############### end subroutine compare_vecs_osres #####################

c############### subroutine addupweightsosres ##########################
c the following routines are similar to the btilde-case...
      subroutine addupweightsosres(sigosres)
        implicit none
#include "nlegborn.h"
#include "pwhg_flst.h"
# include "pwhg_rad.h"
c keep this order
#include "osres.h"
#include "pwhg_flst_add.h"
#include "pwhg_rad_add.h"
        double precision sigosres
        double precision dtotosres(nosres),dtotabsosres(nosres)
        double precision dtotpososres(nosres),dtotnegosres(nosres)
        double precision totosres(nosres),etotosres(nosres)
        double precision totabsosres(nosres),etotabsosres(nosres)
        double precision totpososres(nosres),etotpososres(nosres)
        double precision totnegosres(nosres),etotnegosres(nosres)
        integer ncalls,i,j

        common/cadduptotalsosres/totosres,etotosres,totabsosres
        common/cadduptotalsosres/etotabsosres,totpososres
        common/cadduptotalsosres/etotpososres,totnegosres,etotnegosres
        common/cadduptotalsosres/ncalls

        ! keep track of the number of calls
        ncalls = ncalls + 1
        !print*,ncalls

        ! reset all contributions
        do j=1,nosres
          dtotosres(j)    = 0D0
          dtotabsosres(j) = 0D0
          dtotpososres(j) = 0D0
          dtotnegosres(j) = 0D0
        enddo

        ! sum over resonances
        do j=1,nosres
          ! sum over processes
          do i=1,flst_nosres
            dtotosres(j)    = dtotosres(j) + rad_osres_arr(i,j)
     &                                     * rad_osres_sign(i,j)
            dtotabsosres(j) = dtotabsosres(j) + rad_osres_arr(i,j)
            if(rad_osres_sign(i,j).eq.1) then
              dtotpososres(j) = dtotpososres(j) + rad_osres_arr(i,j)
            else
              dtotnegosres(j) = dtotnegosres(j) + rad_osres_arr(i,j)
            endif
          enddo

          totosres(j)     = totosres(j)     + dtotosres(j)
          etotosres(j)    = etotosres(j)    + dtotosres(j)**2
          totabsosres(j)  = totabsosres(j)  + dtotabsosres(j)
          etotabsosres(j) = etotabsosres(j) + dtotabsosres(j)**2
          totpososres(j)  = totpososres(j)  + dtotpososres(j)
          etotpososres(j) = etotpososres(j) + dtotpososres(j)**2
          totnegosres(j)  = totnegosres(j)  + dtotnegosres(j)
          etotnegosres(j) = etotnegosres(j) + dtotnegosres(j)**2

          sigosres = sigosres + dtotabsosres(j)
        enddo
      end
c############### end subroutine addupweightsosres ######################


c############### subroutine addupweightsosres ##########################
c set all new totals concerning regulars/remnants/osres to zero
      subroutine resettotalsosres
        implicit none
#include "osres.h"
        double precision totosres(nosres),etotosres(nosres)
        double precision totabsosres(nosres),etotabsosres(nosres)
        double precision totpososres(nosres),etotpososres(nosres)
        double precision totnegosres(nosres),etotnegosres(nosres)
        integer ncalls,j

        common/cadduptotalsosres/totosres,etotosres,totabsosres
        common/cadduptotalsosres/etotabsosres,totpososres
        common/cadduptotalsosres/etotpososres,totnegosres,etotnegosres
        common/cadduptotalsosres/ncalls

        ncalls = 0

        do j=1,nosres
          totosres(j)     = 0D0
          etotosres(j)    = 0D0
          totabsosres(j)  = 0D0
          etotabsosres(j) = 0D0
          totpososres(j)  = 0D0
          etotpososres(j) = 0D0
          totnegosres(j)  = 0D0
          etotnegosres(j) = 0D0
        enddo
      end
c############### end subroutine addupweightsosres ######################

c############### subroutine finaltotalsosres ###########################
c similar to corresponding routine in btilde
      subroutine finaltotalsosres
        implicit none
#include "nlegborn.h"
#include "pwhg_flst.h"
#include "pwhg_rad.h"
c keep this order
#include "osres.h"
#include "pwhg_flst_add.h"
#include "pwhg_rad_add.h"
        double precision calc_error
        external calc_error

        double precision totosres(nosres),etotosres(nosres)
        double precision totabsosres(nosres),etotabsosres(nosres)
        double precision totpososres(nosres),etotpososres(nosres)
        double precision totnegosres(nosres),etotnegosres(nosres)
        integer ncalls,j

        common/cadduptotalsosres/totosres,etotosres,totabsosres
        common/cadduptotalsosres/etotabsosres,totpososres
        common/cadduptotalsosres/etotpososres,totnegosres,etotnegosres
        common/cadduptotalsosres/ncalls

        ! if we never call the remnant-routines: avoid NaNs
        if(ncalls.eq.0) ncalls = 1

        do j=1,nosres
          rad_totosres(j)     = totosres(j)/ncalls
          rad_etotosres(j)    = calc_error(totosres(j),
     &                                     etotosres(j),ncalls)
          rad_totabsosres(j)  = totabsosres(j)/ncalls
          rad_etotabsosres(j) = calc_error(totabsosres(j),
     &                                     etotabsosres(j),ncalls)
          rad_totpososres(j)  = totpososres(j)/ncalls
          rad_etotpososres(j) = calc_error(totpososres(j),
     &                                     etotpososres(j),ncalls)
          rad_totnegosres(j)  = totnegosres(j)/ncalls
          rad_etotnegosres(j) = calc_error(totnegosres(j),
     &                                     etotnegosres(j),ncalls)
        enddo
      end
c############### end subroutine finaltotalsosres #######################
