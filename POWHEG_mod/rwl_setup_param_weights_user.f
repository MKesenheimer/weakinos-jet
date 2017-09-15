      subroutine rwl_setup_params_weights_user(count)
      implicit none
      integer count
      integer, save :: old_wreg1, old_wreg2
      include 'pwhg_rwl.h'
      include 'pwhg_pdf.h'
      include 'pwhg_flg.h'
      include 'pwhg_st.h'
#include "osres.h"
#include "Flags.h"
      logical rwl_keypresent
      real * 8 val
      character * 5 scheme
      integer iorder,iret
      logical, save :: for_reweighting,novirtual
      real * 8 powheginput
      if(count==0) then
         for_reweighting = flg_for_reweighting
         novirtual = flg_novirtual
         old_wreg1 = wreg1
         old_wreg2 = wreg2
      elseif(count == -1) then
         flg_for_reweighting = for_reweighting
         flg_novirtual = novirtual
         wreg1 = old_wreg1
         wreg2 = old_wreg2
      else
         flg_for_reweighting = .false.
         flg_novirtual = .false.
         if(rwl_keypresent(count,'wreg1',val)) then
            wreg1 = val
         endif
         if(rwl_keypresent(count,'wreg2',val)) then
            wreg2 = val
         endif
      endif
      end
