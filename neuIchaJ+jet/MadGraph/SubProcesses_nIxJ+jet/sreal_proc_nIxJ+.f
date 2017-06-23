      subroutine sreal_proc_nixjp(p,legs,wgt)
      implicit none
      include "nexternal.inc"
      include "coupl.inc"
      double precision p(0:3,nexternal),wgt
      integer legs(nexternal),lstr
      character*20 str
      double precision P1(0:3,nexternal)
      integer i,ic(nexternal),legs1(nexternal)
      logical mtc,even
      
      do i=1,nexternal
         ic(i)=i
      enddo
      mtc=.false.
 10   call nexper(nexternal- 4,ic( 4+1),mtc,even)
      do i= 4+1,nexternal
         ic(i)=ic(i)+ 4
      enddo
      CALL SWITCHMOM(P,P1,IC,NEXTERNAL)
      CALL SWITCHLEGS(legs,legs1,IC,NEXTERNAL)
      
      call convert_to_string(nexternal,legs1,str,lstr)
      
      if(str.eq."-1-1nIxJ-1-2") then
         call smatrix_dxdx_nIxJdxux(p1,wgt)
         goto 20
      elseif(str.eq."-11nIxJ1-2") then
         call smatrix_dxd_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-11nIxJ-43") then
         call smatrix_dxd_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-11nIxJ5-6") then
         call smatrix_dxd_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-1-2nIxJ-2-2") then
         call smatrix_dxux_nIxJuxux(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ1-1") then
         call smatrix_dxu_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ2-2") then
         call smatrix_dxu_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ4-4") then
         call smatrix_dxu_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ3-3") then
         call smatrix_dxu_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ5-5") then
         call smatrix_dxu_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ6-6") then
         call smatrix_dxu_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-12nIxJ00") then
         call smatrix_dxu_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-1-4nIxJ-2-4") then
         call smatrix_dxcx_nIxJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-14nIxJ-13") then
         call smatrix_dxc_nIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."-14nIxJ-24") then
         call smatrix_dxc_nIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."-1-3nIxJ-1-4") then
         call smatrix_dxsx_nIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-1-3nIxJ-2-3") then
         call smatrix_dxsx_nIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-13nIxJ-23") then
         call smatrix_dxs_nIxJuxs(p1,wgt)
         goto 20
      elseif(str.eq."-1-5nIxJ-1-6") then
         call smatrix_dxbx_nIxJdxtx(p1,wgt)
         goto 20
      elseif(str.eq."-1-5nIxJ-2-5") then
         call smatrix_dxbx_nIxJuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-15nIxJ-25") then
         call smatrix_dxb_nIxJuxb(p1,wgt)
         goto 20
      elseif(str.eq."-10nIxJ-20") then
         call smatrix_dxg_nIxJuxg(p1,wgt)
         goto 20
      elseif(str.eq."1-1nIxJ1-2") then
         call smatrix_ddx_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."1-1nIxJ-43") then
         call smatrix_ddx_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."1-1nIxJ5-6") then
         call smatrix_ddx_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."12nIxJ11") then
         call smatrix_du_nIxJdd(p1,wgt)
         goto 20
      elseif(str.eq."14nIxJ13") then
         call smatrix_dc_nIxJds(p1,wgt)
         goto 20
      elseif(str.eq."1-3nIxJ1-4") then
         call smatrix_dsx_nIxJdcx(p1,wgt)
         goto 20
      elseif(str.eq."1-5nIxJ1-6") then
         call smatrix_dbx_nIxJdtx(p1,wgt)
         goto 20
      elseif(str.eq."-2-1nIxJ-2-2") then
         call smatrix_uxdx_nIxJuxux(p1,wgt)
         goto 20
      elseif(str.eq."-22nIxJ1-2") then
         call smatrix_uxu_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-22nIxJ-43") then
         call smatrix_uxu_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-22nIxJ5-6") then
         call smatrix_uxu_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-24nIxJ-23") then
         call smatrix_uxc_nIxJuxs(p1,wgt)
         goto 20
      elseif(str.eq."-2-3nIxJ-2-4") then
         call smatrix_uxsx_nIxJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-2-5nIxJ-2-6") then
         call smatrix_uxbx_nIxJuxtx(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ1-1") then
         call smatrix_udx_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ2-2") then
         call smatrix_udx_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ4-4") then
         call smatrix_udx_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ3-3") then
         call smatrix_udx_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ5-5") then
         call smatrix_udx_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ6-6") then
         call smatrix_udx_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."2-1nIxJ00") then
         call smatrix_udx_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."21nIxJ11") then
         call smatrix_ud_nIxJdd(p1,wgt)
         goto 20
      elseif(str.eq."2-2nIxJ1-2") then
         call smatrix_uux_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."2-2nIxJ-43") then
         call smatrix_uux_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."2-2nIxJ5-6") then
         call smatrix_uux_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."22nIxJ12") then
         call smatrix_uu_nIxJdu(p1,wgt)
         goto 20
      elseif(str.eq."2-4nIxJ1-4") then
         call smatrix_ucx_nIxJdcx(p1,wgt)
         goto 20
      elseif(str.eq."24nIxJ14") then
         call smatrix_uc_nIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."24nIxJ23") then
         call smatrix_uc_nIxJus(p1,wgt)
         goto 20
      elseif(str.eq."2-3nIxJ1-3") then
         call smatrix_usx_nIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."2-3nIxJ2-4") then
         call smatrix_usx_nIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."23nIxJ13") then
         call smatrix_us_nIxJds(p1,wgt)
         goto 20
      elseif(str.eq."2-5nIxJ1-5") then
         call smatrix_ubx_nIxJdbx(p1,wgt)
         goto 20
      elseif(str.eq."2-5nIxJ2-6") then
         call smatrix_ubx_nIxJutx(p1,wgt)
         goto 20
      elseif(str.eq."25nIxJ15") then
         call smatrix_ub_nIxJdb(p1,wgt)
         goto 20
      elseif(str.eq."20nIxJ10") then
         call smatrix_ug_nIxJdg(p1,wgt)
         goto 20
      elseif(str.eq."-4-1nIxJ-2-4") then
         call smatrix_cxdx_nIxJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-42nIxJ1-4") then
         call smatrix_cxu_nIxJdcx(p1,wgt)
         goto 20
      elseif(str.eq."-44nIxJ1-2") then
         call smatrix_cxc_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-44nIxJ-43") then
         call smatrix_cxc_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-44nIxJ5-6") then
         call smatrix_cxc_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-4-3nIxJ-4-4") then
         call smatrix_cxsx_nIxJcxcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-5nIxJ-4-6") then
         call smatrix_cxbx_nIxJcxtx(p1,wgt)
         goto 20
      elseif(str.eq."4-1nIxJ-13") then
         call smatrix_cdx_nIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."4-1nIxJ-24") then
         call smatrix_cdx_nIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."41nIxJ13") then
         call smatrix_cd_nIxJds(p1,wgt)
         goto 20
      elseif(str.eq."4-2nIxJ-23") then
         call smatrix_cux_nIxJuxs(p1,wgt)
         goto 20
      elseif(str.eq."42nIxJ14") then
         call smatrix_cu_nIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."42nIxJ23") then
         call smatrix_cu_nIxJus(p1,wgt)
         goto 20
      elseif(str.eq."4-4nIxJ1-2") then
         call smatrix_ccx_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."4-4nIxJ-43") then
         call smatrix_ccx_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."4-4nIxJ5-6") then
         call smatrix_ccx_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."44nIxJ43") then
         call smatrix_cc_nIxJcs(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ1-1") then
         call smatrix_csx_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ2-2") then
         call smatrix_csx_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ4-4") then
         call smatrix_csx_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ3-3") then
         call smatrix_csx_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ5-5") then
         call smatrix_csx_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ6-6") then
         call smatrix_csx_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."4-3nIxJ00") then
         call smatrix_csx_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."43nIxJ33") then
         call smatrix_cs_nIxJss(p1,wgt)
         goto 20
      elseif(str.eq."4-5nIxJ4-6") then
         call smatrix_cbx_nIxJctx(p1,wgt)
         goto 20
      elseif(str.eq."4-5nIxJ3-5") then
         call smatrix_cbx_nIxJsbx(p1,wgt)
         goto 20
      elseif(str.eq."45nIxJ35") then
         call smatrix_cb_nIxJsb(p1,wgt)
         goto 20
      elseif(str.eq."40nIxJ30") then
         call smatrix_cg_nIxJsg(p1,wgt)
         goto 20
      elseif(str.eq."-3-1nIxJ-1-4") then
         call smatrix_sxdx_nIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-3-1nIxJ-2-3") then
         call smatrix_sxdx_nIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-31nIxJ1-4") then
         call smatrix_sxd_nIxJdcx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2nIxJ-2-4") then
         call smatrix_sxux_nIxJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-32nIxJ1-3") then
         call smatrix_sxu_nIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."-32nIxJ2-4") then
         call smatrix_sxu_nIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."-3-4nIxJ-4-4") then
         call smatrix_sxcx_nIxJcxcx(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ1-1") then
         call smatrix_sxc_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ2-2") then
         call smatrix_sxc_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ4-4") then
         call smatrix_sxc_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ3-3") then
         call smatrix_sxc_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ5-5") then
         call smatrix_sxc_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ6-6") then
         call smatrix_sxc_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-34nIxJ00") then
         call smatrix_sxc_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-3-3nIxJ-4-3") then
         call smatrix_sxsx_nIxJcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-33nIxJ1-2") then
         call smatrix_sxs_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-33nIxJ-43") then
         call smatrix_sxs_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-33nIxJ5-6") then
         call smatrix_sxs_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-3-5nIxJ-4-5") then
         call smatrix_sxbx_nIxJcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-3-5nIxJ-3-6") then
         call smatrix_sxbx_nIxJsxtx(p1,wgt)
         goto 20
      elseif(str.eq."-35nIxJ-45") then
         call smatrix_sxb_nIxJcxb(p1,wgt)
         goto 20
      elseif(str.eq."-30nIxJ-40") then
         call smatrix_sxg_nIxJcxg(p1,wgt)
         goto 20
      elseif(str.eq."3-1nIxJ-23") then
         call smatrix_sdx_nIxJuxs(p1,wgt)
         goto 20
      elseif(str.eq."32nIxJ13") then
         call smatrix_su_nIxJds(p1,wgt)
         goto 20
      elseif(str.eq."34nIxJ33") then
         call smatrix_sc_nIxJss(p1,wgt)
         goto 20
      elseif(str.eq."3-3nIxJ1-2") then
         call smatrix_ssx_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."3-3nIxJ-43") then
         call smatrix_ssx_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."3-3nIxJ5-6") then
         call smatrix_ssx_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."3-5nIxJ3-6") then
         call smatrix_sbx_nIxJstx(p1,wgt)
         goto 20
      elseif(str.eq."-5-1nIxJ-1-6") then
         call smatrix_bxdx_nIxJdxtx(p1,wgt)
         goto 20
      elseif(str.eq."-5-1nIxJ-2-5") then
         call smatrix_bxdx_nIxJuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-51nIxJ1-6") then
         call smatrix_bxd_nIxJdtx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2nIxJ-2-6") then
         call smatrix_bxux_nIxJuxtx(p1,wgt)
         goto 20
      elseif(str.eq."-52nIxJ1-5") then
         call smatrix_bxu_nIxJdbx(p1,wgt)
         goto 20
      elseif(str.eq."-52nIxJ2-6") then
         call smatrix_bxu_nIxJutx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4nIxJ-4-6") then
         call smatrix_bxcx_nIxJcxtx(p1,wgt)
         goto 20
      elseif(str.eq."-54nIxJ4-6") then
         call smatrix_bxc_nIxJctx(p1,wgt)
         goto 20
      elseif(str.eq."-54nIxJ3-5") then
         call smatrix_bxc_nIxJsbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-3nIxJ-4-5") then
         call smatrix_bxsx_nIxJcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-3nIxJ-3-6") then
         call smatrix_bxsx_nIxJsxtx(p1,wgt)
         goto 20
      elseif(str.eq."-53nIxJ3-6") then
         call smatrix_bxs_nIxJstx(p1,wgt)
         goto 20
      elseif(str.eq."-5-5nIxJ-5-6") then
         call smatrix_bxbx_nIxJbxtx(p1,wgt)
         goto 20
      elseif(str.eq."-55nIxJ1-2") then
         call smatrix_bxb_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-55nIxJ-43") then
         call smatrix_bxb_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-55nIxJ5-6") then
         call smatrix_bxb_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-50nIxJ-60") then
         call smatrix_bxg_nIxJtxg(p1,wgt)
         goto 20
      elseif(str.eq."5-1nIxJ-25") then
         call smatrix_bdx_nIxJuxb(p1,wgt)
         goto 20
      elseif(str.eq."52nIxJ15") then
         call smatrix_bu_nIxJdb(p1,wgt)
         goto 20
      elseif(str.eq."54nIxJ35") then
         call smatrix_bc_nIxJsb(p1,wgt)
         goto 20
      elseif(str.eq."5-3nIxJ-45") then
         call smatrix_bsx_nIxJcxb(p1,wgt)
         goto 20
      elseif(str.eq."5-5nIxJ1-2") then
         call smatrix_bbx_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."5-5nIxJ-43") then
         call smatrix_bbx_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."5-5nIxJ5-6") then
         call smatrix_bbx_nIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."0-1nIxJ-20") then
         call smatrix_gdx_nIxJuxg(p1,wgt)
         goto 20
      elseif(str.eq."02nIxJ10") then
         call smatrix_gu_nIxJdg(p1,wgt)
         goto 20
      elseif(str.eq."04nIxJ30") then
         call smatrix_gc_nIxJsg(p1,wgt)
         goto 20
      elseif(str.eq."0-3nIxJ-40") then
         call smatrix_gsx_nIxJcxg(p1,wgt)
         goto 20
      elseif(str.eq."0-5nIxJ-60") then
         call smatrix_gbx_nIxJtxg(p1,wgt)
         goto 20
      elseif(str.eq."00nIxJ1-2") then
         call smatrix_gg_nIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."00nIxJ-43") then
         call smatrix_gg_nIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."00nIxJ5-6") then
         call smatrix_gg_nIxJbtx(p1,wgt)
         goto 20
      endif
      
      do while(mtc)
         do i= 4+1,nexternal
            ic(i)=ic(i)- 4
         enddo
         goto 10
      enddo
      if(.not.mtc) then
         write(*,*) "Error #1, in sreal_proc.f"
         stop
      endif
      
 20   continue
      return
      end
      
      
      subroutine real_color_nixjp(legs,color)
      implicit none
      include "nexternal.inc"
      include "maxamps.inc"
      Double Precision amp2001(maxamps), jamp2001(0:maxflow)
      common/to_Ramps_dxdx_nIxJdxux/amp2001,jamp2001
      Double Precision amp2002(maxamps), jamp2002(0:maxflow)
      common/to_Ramps_dxd_nIxJdux/amp2002,jamp2002
      Double Precision amp2003(maxamps), jamp2003(0:maxflow)
      common/to_Ramps_dxd_nIxJcxs/amp2003,jamp2003
      Double Precision amp2004(maxamps), jamp2004(0:maxflow)
      common/to_Ramps_dxd_nIxJbtx/amp2004,jamp2004
      Double Precision amp2005(maxamps), jamp2005(0:maxflow)
      common/to_Ramps_dxux_nIxJuxux/amp2005,jamp2005
      Double Precision amp2006(maxamps), jamp2006(0:maxflow)
      common/to_Ramps_dxu_nIxJddx/amp2006,jamp2006
      Double Precision amp2007(maxamps), jamp2007(0:maxflow)
      common/to_Ramps_dxu_nIxJuux/amp2007,jamp2007
      Double Precision amp2008(maxamps), jamp2008(0:maxflow)
      common/to_Ramps_dxu_nIxJccx/amp2008,jamp2008
      Double Precision amp2009(maxamps), jamp2009(0:maxflow)
      common/to_Ramps_dxu_nIxJssx/amp2009,jamp2009
      Double Precision amp2010(maxamps), jamp2010(0:maxflow)
      common/to_Ramps_dxu_nIxJbbx/amp2010,jamp2010
      Double Precision amp2011(maxamps), jamp2011(0:maxflow)
      common/to_Ramps_dxu_nIxJttx/amp2011,jamp2011
      Double Precision amp2012(maxamps), jamp2012(0:maxflow)
      common/to_Ramps_dxu_nIxJgg/amp2012,jamp2012
      Double Precision amp2013(maxamps), jamp2013(0:maxflow)
      common/to_Ramps_dxcx_nIxJuxcx/amp2013,jamp2013
      Double Precision amp2014(maxamps), jamp2014(0:maxflow)
      common/to_Ramps_dxc_nIxJdxs/amp2014,jamp2014
      Double Precision amp2015(maxamps), jamp2015(0:maxflow)
      common/to_Ramps_dxc_nIxJuxc/amp2015,jamp2015
      Double Precision amp2016(maxamps), jamp2016(0:maxflow)
      common/to_Ramps_dxsx_nIxJdxcx/amp2016,jamp2016
      Double Precision amp2017(maxamps), jamp2017(0:maxflow)
      common/to_Ramps_dxsx_nIxJuxsx/amp2017,jamp2017
      Double Precision amp2018(maxamps), jamp2018(0:maxflow)
      common/to_Ramps_dxs_nIxJuxs/amp2018,jamp2018
      Double Precision amp2019(maxamps), jamp2019(0:maxflow)
      common/to_Ramps_dxbx_nIxJdxtx/amp2019,jamp2019
      Double Precision amp2020(maxamps), jamp2020(0:maxflow)
      common/to_Ramps_dxbx_nIxJuxbx/amp2020,jamp2020
      Double Precision amp2021(maxamps), jamp2021(0:maxflow)
      common/to_Ramps_dxb_nIxJuxb/amp2021,jamp2021
      Double Precision amp2022(maxamps), jamp2022(0:maxflow)
      common/to_Ramps_dxg_nIxJuxg/amp2022,jamp2022
      Double Precision amp2023(maxamps), jamp2023(0:maxflow)
      common/to_Ramps_ddx_nIxJdux/amp2023,jamp2023
      Double Precision amp2024(maxamps), jamp2024(0:maxflow)
      common/to_Ramps_ddx_nIxJcxs/amp2024,jamp2024
      Double Precision amp2025(maxamps), jamp2025(0:maxflow)
      common/to_Ramps_ddx_nIxJbtx/amp2025,jamp2025
      Double Precision amp2026(maxamps), jamp2026(0:maxflow)
      common/to_Ramps_du_nIxJdd/amp2026,jamp2026
      Double Precision amp2027(maxamps), jamp2027(0:maxflow)
      common/to_Ramps_dc_nIxJds/amp2027,jamp2027
      Double Precision amp2028(maxamps), jamp2028(0:maxflow)
      common/to_Ramps_dsx_nIxJdcx/amp2028,jamp2028
      Double Precision amp2029(maxamps), jamp2029(0:maxflow)
      common/to_Ramps_dbx_nIxJdtx/amp2029,jamp2029
      Double Precision amp2030(maxamps), jamp2030(0:maxflow)
      common/to_Ramps_uxdx_nIxJuxux/amp2030,jamp2030
      Double Precision amp2031(maxamps), jamp2031(0:maxflow)
      common/to_Ramps_uxu_nIxJdux/amp2031,jamp2031
      Double Precision amp2032(maxamps), jamp2032(0:maxflow)
      common/to_Ramps_uxu_nIxJcxs/amp2032,jamp2032
      Double Precision amp2033(maxamps), jamp2033(0:maxflow)
      common/to_Ramps_uxu_nIxJbtx/amp2033,jamp2033
      Double Precision amp2034(maxamps), jamp2034(0:maxflow)
      common/to_Ramps_uxc_nIxJuxs/amp2034,jamp2034
      Double Precision amp2035(maxamps), jamp2035(0:maxflow)
      common/to_Ramps_uxsx_nIxJuxcx/amp2035,jamp2035
      Double Precision amp2036(maxamps), jamp2036(0:maxflow)
      common/to_Ramps_uxbx_nIxJuxtx/amp2036,jamp2036
      Double Precision amp2037(maxamps), jamp2037(0:maxflow)
      common/to_Ramps_udx_nIxJddx/amp2037,jamp2037
      Double Precision amp2038(maxamps), jamp2038(0:maxflow)
      common/to_Ramps_udx_nIxJuux/amp2038,jamp2038
      Double Precision amp2039(maxamps), jamp2039(0:maxflow)
      common/to_Ramps_udx_nIxJccx/amp2039,jamp2039
      Double Precision amp2040(maxamps), jamp2040(0:maxflow)
      common/to_Ramps_udx_nIxJssx/amp2040,jamp2040
      Double Precision amp2041(maxamps), jamp2041(0:maxflow)
      common/to_Ramps_udx_nIxJbbx/amp2041,jamp2041
      Double Precision amp2042(maxamps), jamp2042(0:maxflow)
      common/to_Ramps_udx_nIxJttx/amp2042,jamp2042
      Double Precision amp2043(maxamps), jamp2043(0:maxflow)
      common/to_Ramps_udx_nIxJgg/amp2043,jamp2043
      Double Precision amp2044(maxamps), jamp2044(0:maxflow)
      common/to_Ramps_ud_nIxJdd/amp2044,jamp2044
      Double Precision amp2045(maxamps), jamp2045(0:maxflow)
      common/to_Ramps_uux_nIxJdux/amp2045,jamp2045
      Double Precision amp2046(maxamps), jamp2046(0:maxflow)
      common/to_Ramps_uux_nIxJcxs/amp2046,jamp2046
      Double Precision amp2047(maxamps), jamp2047(0:maxflow)
      common/to_Ramps_uux_nIxJbtx/amp2047,jamp2047
      Double Precision amp2048(maxamps), jamp2048(0:maxflow)
      common/to_Ramps_uu_nIxJdu/amp2048,jamp2048
      Double Precision amp2049(maxamps), jamp2049(0:maxflow)
      common/to_Ramps_ucx_nIxJdcx/amp2049,jamp2049
      Double Precision amp2050(maxamps), jamp2050(0:maxflow)
      common/to_Ramps_uc_nIxJdc/amp2050,jamp2050
      Double Precision amp2051(maxamps), jamp2051(0:maxflow)
      common/to_Ramps_uc_nIxJus/amp2051,jamp2051
      Double Precision amp2052(maxamps), jamp2052(0:maxflow)
      common/to_Ramps_usx_nIxJdsx/amp2052,jamp2052
      Double Precision amp2053(maxamps), jamp2053(0:maxflow)
      common/to_Ramps_usx_nIxJucx/amp2053,jamp2053
      Double Precision amp2054(maxamps), jamp2054(0:maxflow)
      common/to_Ramps_us_nIxJds/amp2054,jamp2054
      Double Precision amp2055(maxamps), jamp2055(0:maxflow)
      common/to_Ramps_ubx_nIxJdbx/amp2055,jamp2055
      Double Precision amp2056(maxamps), jamp2056(0:maxflow)
      common/to_Ramps_ubx_nIxJutx/amp2056,jamp2056
      Double Precision amp2057(maxamps), jamp2057(0:maxflow)
      common/to_Ramps_ub_nIxJdb/amp2057,jamp2057
      Double Precision amp2058(maxamps), jamp2058(0:maxflow)
      common/to_Ramps_ug_nIxJdg/amp2058,jamp2058
      Double Precision amp2059(maxamps), jamp2059(0:maxflow)
      common/to_Ramps_cxdx_nIxJuxcx/amp2059,jamp2059
      Double Precision amp2060(maxamps), jamp2060(0:maxflow)
      common/to_Ramps_cxu_nIxJdcx/amp2060,jamp2060
      Double Precision amp2061(maxamps), jamp2061(0:maxflow)
      common/to_Ramps_cxc_nIxJdux/amp2061,jamp2061
      Double Precision amp2062(maxamps), jamp2062(0:maxflow)
      common/to_Ramps_cxc_nIxJcxs/amp2062,jamp2062
      Double Precision amp2063(maxamps), jamp2063(0:maxflow)
      common/to_Ramps_cxc_nIxJbtx/amp2063,jamp2063
      Double Precision amp2064(maxamps), jamp2064(0:maxflow)
      common/to_Ramps_cxsx_nIxJcxcx/amp2064,jamp2064
      Double Precision amp2065(maxamps), jamp2065(0:maxflow)
      common/to_Ramps_cxbx_nIxJcxtx/amp2065,jamp2065
      Double Precision amp2066(maxamps), jamp2066(0:maxflow)
      common/to_Ramps_cdx_nIxJdxs/amp2066,jamp2066
      Double Precision amp2067(maxamps), jamp2067(0:maxflow)
      common/to_Ramps_cdx_nIxJuxc/amp2067,jamp2067
      Double Precision amp2068(maxamps), jamp2068(0:maxflow)
      common/to_Ramps_cd_nIxJds/amp2068,jamp2068
      Double Precision amp2069(maxamps), jamp2069(0:maxflow)
      common/to_Ramps_cux_nIxJuxs/amp2069,jamp2069
      Double Precision amp2070(maxamps), jamp2070(0:maxflow)
      common/to_Ramps_cu_nIxJdc/amp2070,jamp2070
      Double Precision amp2071(maxamps), jamp2071(0:maxflow)
      common/to_Ramps_cu_nIxJus/amp2071,jamp2071
      Double Precision amp2072(maxamps), jamp2072(0:maxflow)
      common/to_Ramps_ccx_nIxJdux/amp2072,jamp2072
      Double Precision amp2073(maxamps), jamp2073(0:maxflow)
      common/to_Ramps_ccx_nIxJcxs/amp2073,jamp2073
      Double Precision amp2074(maxamps), jamp2074(0:maxflow)
      common/to_Ramps_ccx_nIxJbtx/amp2074,jamp2074
      Double Precision amp2075(maxamps), jamp2075(0:maxflow)
      common/to_Ramps_cc_nIxJcs/amp2075,jamp2075
      Double Precision amp2076(maxamps), jamp2076(0:maxflow)
      common/to_Ramps_csx_nIxJddx/amp2076,jamp2076
      Double Precision amp2077(maxamps), jamp2077(0:maxflow)
      common/to_Ramps_csx_nIxJuux/amp2077,jamp2077
      Double Precision amp2078(maxamps), jamp2078(0:maxflow)
      common/to_Ramps_csx_nIxJccx/amp2078,jamp2078
      Double Precision amp2079(maxamps), jamp2079(0:maxflow)
      common/to_Ramps_csx_nIxJssx/amp2079,jamp2079
      Double Precision amp2080(maxamps), jamp2080(0:maxflow)
      common/to_Ramps_csx_nIxJbbx/amp2080,jamp2080
      Double Precision amp2081(maxamps), jamp2081(0:maxflow)
      common/to_Ramps_csx_nIxJttx/amp2081,jamp2081
      Double Precision amp2082(maxamps), jamp2082(0:maxflow)
      common/to_Ramps_csx_nIxJgg/amp2082,jamp2082
      Double Precision amp2083(maxamps), jamp2083(0:maxflow)
      common/to_Ramps_cs_nIxJss/amp2083,jamp2083
      Double Precision amp2084(maxamps), jamp2084(0:maxflow)
      common/to_Ramps_cbx_nIxJctx/amp2084,jamp2084
      Double Precision amp2085(maxamps), jamp2085(0:maxflow)
      common/to_Ramps_cbx_nIxJsbx/amp2085,jamp2085
      Double Precision amp2086(maxamps), jamp2086(0:maxflow)
      common/to_Ramps_cb_nIxJsb/amp2086,jamp2086
      Double Precision amp2087(maxamps), jamp2087(0:maxflow)
      common/to_Ramps_cg_nIxJsg/amp2087,jamp2087
      Double Precision amp2088(maxamps), jamp2088(0:maxflow)
      common/to_Ramps_sxdx_nIxJdxcx/amp2088,jamp2088
      Double Precision amp2089(maxamps), jamp2089(0:maxflow)
      common/to_Ramps_sxdx_nIxJuxsx/amp2089,jamp2089
      Double Precision amp2090(maxamps), jamp2090(0:maxflow)
      common/to_Ramps_sxd_nIxJdcx/amp2090,jamp2090
      Double Precision amp2091(maxamps), jamp2091(0:maxflow)
      common/to_Ramps_sxux_nIxJuxcx/amp2091,jamp2091
      Double Precision amp2092(maxamps), jamp2092(0:maxflow)
      common/to_Ramps_sxu_nIxJdsx/amp2092,jamp2092
      Double Precision amp2093(maxamps), jamp2093(0:maxflow)
      common/to_Ramps_sxu_nIxJucx/amp2093,jamp2093
      Double Precision amp2094(maxamps), jamp2094(0:maxflow)
      common/to_Ramps_sxcx_nIxJcxcx/amp2094,jamp2094
      Double Precision amp2095(maxamps), jamp2095(0:maxflow)
      common/to_Ramps_sxc_nIxJddx/amp2095,jamp2095
      Double Precision amp2096(maxamps), jamp2096(0:maxflow)
      common/to_Ramps_sxc_nIxJuux/amp2096,jamp2096
      Double Precision amp2097(maxamps), jamp2097(0:maxflow)
      common/to_Ramps_sxc_nIxJccx/amp2097,jamp2097
      Double Precision amp2098(maxamps), jamp2098(0:maxflow)
      common/to_Ramps_sxc_nIxJssx/amp2098,jamp2098
      Double Precision amp2099(maxamps), jamp2099(0:maxflow)
      common/to_Ramps_sxc_nIxJbbx/amp2099,jamp2099
      Double Precision amp2100(maxamps), jamp2100(0:maxflow)
      common/to_Ramps_sxc_nIxJttx/amp2100,jamp2100
      Double Precision amp2101(maxamps), jamp2101(0:maxflow)
      common/to_Ramps_sxc_nIxJgg/amp2101,jamp2101
      Double Precision amp2102(maxamps), jamp2102(0:maxflow)
      common/to_Ramps_sxsx_nIxJcxsx/amp2102,jamp2102
      Double Precision amp2103(maxamps), jamp2103(0:maxflow)
      common/to_Ramps_sxs_nIxJdux/amp2103,jamp2103
      Double Precision amp2104(maxamps), jamp2104(0:maxflow)
      common/to_Ramps_sxs_nIxJcxs/amp2104,jamp2104
      Double Precision amp2105(maxamps), jamp2105(0:maxflow)
      common/to_Ramps_sxs_nIxJbtx/amp2105,jamp2105
      Double Precision amp2106(maxamps), jamp2106(0:maxflow)
      common/to_Ramps_sxbx_nIxJcxbx/amp2106,jamp2106
      Double Precision amp2107(maxamps), jamp2107(0:maxflow)
      common/to_Ramps_sxbx_nIxJsxtx/amp2107,jamp2107
      Double Precision amp2108(maxamps), jamp2108(0:maxflow)
      common/to_Ramps_sxb_nIxJcxb/amp2108,jamp2108
      Double Precision amp2109(maxamps), jamp2109(0:maxflow)
      common/to_Ramps_sxg_nIxJcxg/amp2109,jamp2109
      Double Precision amp2110(maxamps), jamp2110(0:maxflow)
      common/to_Ramps_sdx_nIxJuxs/amp2110,jamp2110
      Double Precision amp2111(maxamps), jamp2111(0:maxflow)
      common/to_Ramps_su_nIxJds/amp2111,jamp2111
      Double Precision amp2112(maxamps), jamp2112(0:maxflow)
      common/to_Ramps_sc_nIxJss/amp2112,jamp2112
      Double Precision amp2113(maxamps), jamp2113(0:maxflow)
      common/to_Ramps_ssx_nIxJdux/amp2113,jamp2113
      Double Precision amp2114(maxamps), jamp2114(0:maxflow)
      common/to_Ramps_ssx_nIxJcxs/amp2114,jamp2114
      Double Precision amp2115(maxamps), jamp2115(0:maxflow)
      common/to_Ramps_ssx_nIxJbtx/amp2115,jamp2115
      Double Precision amp2116(maxamps), jamp2116(0:maxflow)
      common/to_Ramps_sbx_nIxJstx/amp2116,jamp2116
      Double Precision amp2117(maxamps), jamp2117(0:maxflow)
      common/to_Ramps_bxdx_nIxJdxtx/amp2117,jamp2117
      Double Precision amp2118(maxamps), jamp2118(0:maxflow)
      common/to_Ramps_bxdx_nIxJuxbx/amp2118,jamp2118
      Double Precision amp2119(maxamps), jamp2119(0:maxflow)
      common/to_Ramps_bxd_nIxJdtx/amp2119,jamp2119
      Double Precision amp2120(maxamps), jamp2120(0:maxflow)
      common/to_Ramps_bxux_nIxJuxtx/amp2120,jamp2120
      Double Precision amp2121(maxamps), jamp2121(0:maxflow)
      common/to_Ramps_bxu_nIxJdbx/amp2121,jamp2121
      Double Precision amp2122(maxamps), jamp2122(0:maxflow)
      common/to_Ramps_bxu_nIxJutx/amp2122,jamp2122
      Double Precision amp2123(maxamps), jamp2123(0:maxflow)
      common/to_Ramps_bxcx_nIxJcxtx/amp2123,jamp2123
      Double Precision amp2124(maxamps), jamp2124(0:maxflow)
      common/to_Ramps_bxc_nIxJctx/amp2124,jamp2124
      Double Precision amp2125(maxamps), jamp2125(0:maxflow)
      common/to_Ramps_bxc_nIxJsbx/amp2125,jamp2125
      Double Precision amp2126(maxamps), jamp2126(0:maxflow)
      common/to_Ramps_bxsx_nIxJcxbx/amp2126,jamp2126
      Double Precision amp2127(maxamps), jamp2127(0:maxflow)
      common/to_Ramps_bxsx_nIxJsxtx/amp2127,jamp2127
      Double Precision amp2128(maxamps), jamp2128(0:maxflow)
      common/to_Ramps_bxs_nIxJstx/amp2128,jamp2128
      Double Precision amp2129(maxamps), jamp2129(0:maxflow)
      common/to_Ramps_bxbx_nIxJbxtx/amp2129,jamp2129
      Double Precision amp2130(maxamps), jamp2130(0:maxflow)
      common/to_Ramps_bxb_nIxJdux/amp2130,jamp2130
      Double Precision amp2131(maxamps), jamp2131(0:maxflow)
      common/to_Ramps_bxb_nIxJcxs/amp2131,jamp2131
      Double Precision amp2132(maxamps), jamp2132(0:maxflow)
      common/to_Ramps_bxb_nIxJbtx/amp2132,jamp2132
      Double Precision amp2133(maxamps), jamp2133(0:maxflow)
      common/to_Ramps_bxg_nIxJtxg/amp2133,jamp2133
      Double Precision amp2134(maxamps), jamp2134(0:maxflow)
      common/to_Ramps_bdx_nIxJuxb/amp2134,jamp2134
      Double Precision amp2135(maxamps), jamp2135(0:maxflow)
      common/to_Ramps_bu_nIxJdb/amp2135,jamp2135
      Double Precision amp2136(maxamps), jamp2136(0:maxflow)
      common/to_Ramps_bc_nIxJsb/amp2136,jamp2136
      Double Precision amp2137(maxamps), jamp2137(0:maxflow)
      common/to_Ramps_bsx_nIxJcxb/amp2137,jamp2137
      Double Precision amp2138(maxamps), jamp2138(0:maxflow)
      common/to_Ramps_bbx_nIxJdux/amp2138,jamp2138
      Double Precision amp2139(maxamps), jamp2139(0:maxflow)
      common/to_Ramps_bbx_nIxJcxs/amp2139,jamp2139
      Double Precision amp2140(maxamps), jamp2140(0:maxflow)
      common/to_Ramps_bbx_nIxJbtx/amp2140,jamp2140
      Double Precision amp2141(maxamps), jamp2141(0:maxflow)
      common/to_Ramps_gdx_nIxJuxg/amp2141,jamp2141
      Double Precision amp2142(maxamps), jamp2142(0:maxflow)
      common/to_Ramps_gu_nIxJdg/amp2142,jamp2142
      Double Precision amp2143(maxamps), jamp2143(0:maxflow)
      common/to_Ramps_gc_nIxJsg/amp2143,jamp2143
      Double Precision amp2144(maxamps), jamp2144(0:maxflow)
      common/to_Ramps_gsx_nIxJcxg/amp2144,jamp2144
      Double Precision amp2145(maxamps), jamp2145(0:maxflow)
      common/to_Ramps_gbx_nIxJtxg/amp2145,jamp2145
      Double Precision amp2146(maxamps), jamp2146(0:maxflow)
      common/to_Ramps_gg_nIxJdux/amp2146,jamp2146
      Double Precision amp2147(maxamps), jamp2147(0:maxflow)
      common/to_Ramps_gg_nIxJcxs/amp2147,jamp2147
      Double Precision amp2148(maxamps), jamp2148(0:maxflow)
      common/to_Ramps_gg_nIxJbtx/amp2148,jamp2148
      double precision jamp2cum(0:maxflow)
      integer ICOLUP(2,nexternal,maxamps)
      integer color(2,nexternal),color1(2,nexternal)
      double precision random,xtarget
      external random
      integer legs(nexternal),lstr,i,j
      character*20 str
      integer ic(nexternal),legs1(nexternal)
      integer iflow,ifl
      logical mtc,even
      
      do i=1,nexternal
         ic(i)=i
      enddo
      mtc=.false.
 10   call nexper(nexternal- 4,ic( 4+1),mtc,even)
      do i= 4+1,nexternal
         ic(i)=ic(i)+ 4
      enddo
      CALL SWITCHLEGS(legs,legs1,IC,NEXTERNAL)
      
      call convert_to_string(nexternal,legs1,str,lstr)
      
      if(str.eq."-1-1nIxJ-1-2") then
         include "leshouches_R_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-11nIxJ1-2") then
         include "leshouches_R_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."-11nIxJ-43") then
         include "leshouches_R_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."-11nIxJ5-6") then
         include "leshouches_R_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."-1-2nIxJ-2-2") then
         include "leshouches_R_005.inc"
         iflow=nint(jamp2005(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2005(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ1-1") then
         include "leshouches_R_006.inc"
         iflow=nint(jamp2006(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2006(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ2-2") then
         include "leshouches_R_007.inc"
         iflow=nint(jamp2007(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2007(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ4-4") then
         include "leshouches_R_008.inc"
         iflow=nint(jamp2008(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2008(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ3-3") then
         include "leshouches_R_009.inc"
         iflow=nint(jamp2009(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2009(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ5-5") then
         include "leshouches_R_010.inc"
         iflow=nint(jamp2010(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2010(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ6-6") then
         include "leshouches_R_011.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."-12nIxJ00") then
         include "leshouches_R_012.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."-1-4nIxJ-2-4") then
         include "leshouches_R_013.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."-14nIxJ-13") then
         include "leshouches_R_014.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."-14nIxJ-24") then
         include "leshouches_R_015.inc"
         iflow=nint(jamp2015(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2015(i)
         enddo
         goto 20
      elseif(str.eq."-1-3nIxJ-1-4") then
         include "leshouches_R_016.inc"
         iflow=nint(jamp2016(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2016(i)
         enddo
         goto 20
      elseif(str.eq."-1-3nIxJ-2-3") then
         include "leshouches_R_017.inc"
         iflow=nint(jamp2017(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2017(i)
         enddo
         goto 20
      elseif(str.eq."-13nIxJ-23") then
         include "leshouches_R_018.inc"
         iflow=nint(jamp2018(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2018(i)
         enddo
         goto 20
      elseif(str.eq."-1-5nIxJ-1-6") then
         include "leshouches_R_019.inc"
         iflow=nint(jamp2019(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2019(i)
         enddo
         goto 20
      elseif(str.eq."-1-5nIxJ-2-5") then
         include "leshouches_R_020.inc"
         iflow=nint(jamp2020(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2020(i)
         enddo
         goto 20
      elseif(str.eq."-15nIxJ-25") then
         include "leshouches_R_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif(str.eq."-10nIxJ-20") then
         include "leshouches_R_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20
      elseif(str.eq."1-1nIxJ1-2") then
         include "leshouches_R_023.inc"
         iflow=nint(jamp2023(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2023(i)
         enddo
         goto 20
      elseif(str.eq."1-1nIxJ-43") then
         include "leshouches_R_024.inc"
         iflow=nint(jamp2024(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2024(i)
         enddo
         goto 20
      elseif(str.eq."1-1nIxJ5-6") then
         include "leshouches_R_025.inc"
         iflow=nint(jamp2025(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2025(i)
         enddo
         goto 20
      elseif(str.eq."12nIxJ11") then
         include "leshouches_R_026.inc"
         iflow=nint(jamp2026(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2026(i)
         enddo
         goto 20
      elseif(str.eq."14nIxJ13") then
         include "leshouches_R_027.inc"
         iflow=nint(jamp2027(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2027(i)
         enddo
         goto 20
      elseif(str.eq."1-3nIxJ1-4") then
         include "leshouches_R_028.inc"
         iflow=nint(jamp2028(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2028(i)
         enddo
         goto 20
      elseif(str.eq."1-5nIxJ1-6") then
         include "leshouches_R_029.inc"
         iflow=nint(jamp2029(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2029(i)
         enddo
         goto 20
      elseif(str.eq."-2-1nIxJ-2-2") then
         include "leshouches_R_030.inc"
         iflow=nint(jamp2030(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2030(i)
         enddo
         goto 20
      elseif(str.eq."-22nIxJ1-2") then
         include "leshouches_R_031.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif(str.eq."-22nIxJ-43") then
         include "leshouches_R_032.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
         enddo
         goto 20
      elseif(str.eq."-22nIxJ5-6") then
         include "leshouches_R_033.inc"
         iflow=nint(jamp2033(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2033(i)
         enddo
         goto 20
      elseif(str.eq."-24nIxJ-23") then
         include "leshouches_R_034.inc"
         iflow=nint(jamp2034(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2034(i)
         enddo
         goto 20
      elseif(str.eq."-2-3nIxJ-2-4") then
         include "leshouches_R_035.inc"
         iflow=nint(jamp2035(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2035(i)
         enddo
         goto 20
      elseif(str.eq."-2-5nIxJ-2-6") then
         include "leshouches_R_036.inc"
         iflow=nint(jamp2036(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2036(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ1-1") then
         include "leshouches_R_037.inc"
         iflow=nint(jamp2037(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2037(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ2-2") then
         include "leshouches_R_038.inc"
         iflow=nint(jamp2038(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2038(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ4-4") then
         include "leshouches_R_039.inc"
         iflow=nint(jamp2039(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2039(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ3-3") then
         include "leshouches_R_040.inc"
         iflow=nint(jamp2040(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2040(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ5-5") then
         include "leshouches_R_041.inc"
         iflow=nint(jamp2041(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2041(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ6-6") then
         include "leshouches_R_042.inc"
         iflow=nint(jamp2042(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2042(i)
         enddo
         goto 20
      elseif(str.eq."2-1nIxJ00") then
         include "leshouches_R_043.inc"
         iflow=nint(jamp2043(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2043(i)
         enddo
         goto 20
      elseif(str.eq."21nIxJ11") then
         include "leshouches_R_044.inc"
         iflow=nint(jamp2044(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2044(i)
         enddo
         goto 20
      elseif(str.eq."2-2nIxJ1-2") then
         include "leshouches_R_045.inc"
         iflow=nint(jamp2045(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2045(i)
         enddo
         goto 20
      elseif(str.eq."2-2nIxJ-43") then
         include "leshouches_R_046.inc"
         iflow=nint(jamp2046(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2046(i)
         enddo
         goto 20
      elseif(str.eq."2-2nIxJ5-6") then
         include "leshouches_R_047.inc"
         iflow=nint(jamp2047(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2047(i)
         enddo
         goto 20
      elseif(str.eq."22nIxJ12") then
         include "leshouches_R_048.inc"
         iflow=nint(jamp2048(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2048(i)
         enddo
         goto 20
      elseif(str.eq."2-4nIxJ1-4") then
         include "leshouches_R_049.inc"
         iflow=nint(jamp2049(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2049(i)
         enddo
         goto 20
      elseif(str.eq."24nIxJ14") then
         include "leshouches_R_050.inc"
         iflow=nint(jamp2050(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2050(i)
         enddo
         goto 20
      elseif(str.eq."24nIxJ23") then
         include "leshouches_R_051.inc"
         iflow=nint(jamp2051(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2051(i)
         enddo
         goto 20
      elseif(str.eq."2-3nIxJ1-3") then
         include "leshouches_R_052.inc"
         iflow=nint(jamp2052(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2052(i)
         enddo
         goto 20
      elseif(str.eq."2-3nIxJ2-4") then
         include "leshouches_R_053.inc"
         iflow=nint(jamp2053(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2053(i)
         enddo
         goto 20
      elseif(str.eq."23nIxJ13") then
         include "leshouches_R_054.inc"
         iflow=nint(jamp2054(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2054(i)
         enddo
         goto 20
      elseif(str.eq."2-5nIxJ1-5") then
         include "leshouches_R_055.inc"
         iflow=nint(jamp2055(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2055(i)
         enddo
         goto 20
      elseif(str.eq."2-5nIxJ2-6") then
         include "leshouches_R_056.inc"
         iflow=nint(jamp2056(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2056(i)
         enddo
         goto 20
      elseif(str.eq."25nIxJ15") then
         include "leshouches_R_057.inc"
         iflow=nint(jamp2057(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2057(i)
         enddo
         goto 20
      elseif(str.eq."20nIxJ10") then
         include "leshouches_R_058.inc"
         iflow=nint(jamp2058(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2058(i)
         enddo
         goto 20
      elseif(str.eq."-4-1nIxJ-2-4") then
         include "leshouches_R_059.inc"
         iflow=nint(jamp2059(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2059(i)
         enddo
         goto 20
      elseif(str.eq."-42nIxJ1-4") then
         include "leshouches_R_060.inc"
         iflow=nint(jamp2060(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2060(i)
         enddo
         goto 20
      elseif(str.eq."-44nIxJ1-2") then
         include "leshouches_R_061.inc"
         iflow=nint(jamp2061(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2061(i)
         enddo
         goto 20
      elseif(str.eq."-44nIxJ-43") then
         include "leshouches_R_062.inc"
         iflow=nint(jamp2062(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2062(i)
         enddo
         goto 20
      elseif(str.eq."-44nIxJ5-6") then
         include "leshouches_R_063.inc"
         iflow=nint(jamp2063(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2063(i)
         enddo
         goto 20
      elseif(str.eq."-4-3nIxJ-4-4") then
         include "leshouches_R_064.inc"
         iflow=nint(jamp2064(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2064(i)
         enddo
         goto 20
      elseif(str.eq."-4-5nIxJ-4-6") then
         include "leshouches_R_065.inc"
         iflow=nint(jamp2065(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2065(i)
         enddo
         goto 20
      elseif(str.eq."4-1nIxJ-13") then
         include "leshouches_R_066.inc"
         iflow=nint(jamp2066(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2066(i)
         enddo
         goto 20
      elseif(str.eq."4-1nIxJ-24") then
         include "leshouches_R_067.inc"
         iflow=nint(jamp2067(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2067(i)
         enddo
         goto 20
      elseif(str.eq."41nIxJ13") then
         include "leshouches_R_068.inc"
         iflow=nint(jamp2068(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2068(i)
         enddo
         goto 20
      elseif(str.eq."4-2nIxJ-23") then
         include "leshouches_R_069.inc"
         iflow=nint(jamp2069(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2069(i)
         enddo
         goto 20
      elseif(str.eq."42nIxJ14") then
         include "leshouches_R_070.inc"
         iflow=nint(jamp2070(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2070(i)
         enddo
         goto 20
      elseif(str.eq."42nIxJ23") then
         include "leshouches_R_071.inc"
         iflow=nint(jamp2071(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2071(i)
         enddo
         goto 20
      elseif(str.eq."4-4nIxJ1-2") then
         include "leshouches_R_072.inc"
         iflow=nint(jamp2072(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2072(i)
         enddo
         goto 20
      elseif(str.eq."4-4nIxJ-43") then
         include "leshouches_R_073.inc"
         iflow=nint(jamp2073(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2073(i)
         enddo
         goto 20
      elseif(str.eq."4-4nIxJ5-6") then
         include "leshouches_R_074.inc"
         iflow=nint(jamp2074(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2074(i)
         enddo
         goto 20
      elseif(str.eq."44nIxJ43") then
         include "leshouches_R_075.inc"
         iflow=nint(jamp2075(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2075(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ1-1") then
         include "leshouches_R_076.inc"
         iflow=nint(jamp2076(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2076(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ2-2") then
         include "leshouches_R_077.inc"
         iflow=nint(jamp2077(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2077(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ4-4") then
         include "leshouches_R_078.inc"
         iflow=nint(jamp2078(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2078(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ3-3") then
         include "leshouches_R_079.inc"
         iflow=nint(jamp2079(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2079(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ5-5") then
         include "leshouches_R_080.inc"
         iflow=nint(jamp2080(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2080(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ6-6") then
         include "leshouches_R_081.inc"
         iflow=nint(jamp2081(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2081(i)
         enddo
         goto 20
      elseif(str.eq."4-3nIxJ00") then
         include "leshouches_R_082.inc"
         iflow=nint(jamp2082(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2082(i)
         enddo
         goto 20
      elseif(str.eq."43nIxJ33") then
         include "leshouches_R_083.inc"
         iflow=nint(jamp2083(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2083(i)
         enddo
         goto 20
      elseif(str.eq."4-5nIxJ4-6") then
         include "leshouches_R_084.inc"
         iflow=nint(jamp2084(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2084(i)
         enddo
         goto 20
      elseif(str.eq."4-5nIxJ3-5") then
         include "leshouches_R_085.inc"
         iflow=nint(jamp2085(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2085(i)
         enddo
         goto 20
      elseif(str.eq."45nIxJ35") then
         include "leshouches_R_086.inc"
         iflow=nint(jamp2086(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2086(i)
         enddo
         goto 20
      elseif(str.eq."40nIxJ30") then
         include "leshouches_R_087.inc"
         iflow=nint(jamp2087(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2087(i)
         enddo
         goto 20
      elseif(str.eq."-3-1nIxJ-1-4") then
         include "leshouches_R_088.inc"
         iflow=nint(jamp2088(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2088(i)
         enddo
         goto 20
      elseif(str.eq."-3-1nIxJ-2-3") then
         include "leshouches_R_089.inc"
         iflow=nint(jamp2089(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2089(i)
         enddo
         goto 20
      elseif(str.eq."-31nIxJ1-4") then
         include "leshouches_R_090.inc"
         iflow=nint(jamp2090(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2090(i)
         enddo
         goto 20
      elseif(str.eq."-3-2nIxJ-2-4") then
         include "leshouches_R_091.inc"
         iflow=nint(jamp2091(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2091(i)
         enddo
         goto 20
      elseif(str.eq."-32nIxJ1-3") then
         include "leshouches_R_092.inc"
         iflow=nint(jamp2092(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2092(i)
         enddo
         goto 20
      elseif(str.eq."-32nIxJ2-4") then
         include "leshouches_R_093.inc"
         iflow=nint(jamp2093(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2093(i)
         enddo
         goto 20
      elseif(str.eq."-3-4nIxJ-4-4") then
         include "leshouches_R_094.inc"
         iflow=nint(jamp2094(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2094(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ1-1") then
         include "leshouches_R_095.inc"
         iflow=nint(jamp2095(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2095(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ2-2") then
         include "leshouches_R_096.inc"
         iflow=nint(jamp2096(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2096(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ4-4") then
         include "leshouches_R_097.inc"
         iflow=nint(jamp2097(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2097(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ3-3") then
         include "leshouches_R_098.inc"
         iflow=nint(jamp2098(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2098(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ5-5") then
         include "leshouches_R_099.inc"
         iflow=nint(jamp2099(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2099(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ6-6") then
         include "leshouches_R_100.inc"
         iflow=nint(jamp2100(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2100(i)
         enddo
         goto 20
      elseif(str.eq."-34nIxJ00") then
         include "leshouches_R_101.inc"
         iflow=nint(jamp2101(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2101(i)
         enddo
         goto 20
      elseif(str.eq."-3-3nIxJ-4-3") then
         include "leshouches_R_102.inc"
         iflow=nint(jamp2102(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2102(i)
         enddo
         goto 20
      elseif(str.eq."-33nIxJ1-2") then
         include "leshouches_R_103.inc"
         iflow=nint(jamp2103(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2103(i)
         enddo
         goto 20
      elseif(str.eq."-33nIxJ-43") then
         include "leshouches_R_104.inc"
         iflow=nint(jamp2104(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2104(i)
         enddo
         goto 20
      elseif(str.eq."-33nIxJ5-6") then
         include "leshouches_R_105.inc"
         iflow=nint(jamp2105(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2105(i)
         enddo
         goto 20
      elseif(str.eq."-3-5nIxJ-4-5") then
         include "leshouches_R_106.inc"
         iflow=nint(jamp2106(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2106(i)
         enddo
         goto 20
      elseif(str.eq."-3-5nIxJ-3-6") then
         include "leshouches_R_107.inc"
         iflow=nint(jamp2107(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2107(i)
         enddo
         goto 20
      elseif(str.eq."-35nIxJ-45") then
         include "leshouches_R_108.inc"
         iflow=nint(jamp2108(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2108(i)
         enddo
         goto 20
      elseif(str.eq."-30nIxJ-40") then
         include "leshouches_R_109.inc"
         iflow=nint(jamp2109(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2109(i)
         enddo
         goto 20
      elseif(str.eq."3-1nIxJ-23") then
         include "leshouches_R_110.inc"
         iflow=nint(jamp2110(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2110(i)
         enddo
         goto 20
      elseif(str.eq."32nIxJ13") then
         include "leshouches_R_111.inc"
         iflow=nint(jamp2111(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2111(i)
         enddo
         goto 20
      elseif(str.eq."34nIxJ33") then
         include "leshouches_R_112.inc"
         iflow=nint(jamp2112(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2112(i)
         enddo
         goto 20
      elseif(str.eq."3-3nIxJ1-2") then
         include "leshouches_R_113.inc"
         iflow=nint(jamp2113(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2113(i)
         enddo
         goto 20
      elseif(str.eq."3-3nIxJ-43") then
         include "leshouches_R_114.inc"
         iflow=nint(jamp2114(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2114(i)
         enddo
         goto 20
      elseif(str.eq."3-3nIxJ5-6") then
         include "leshouches_R_115.inc"
         iflow=nint(jamp2115(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2115(i)
         enddo
         goto 20
      elseif(str.eq."3-5nIxJ3-6") then
         include "leshouches_R_116.inc"
         iflow=nint(jamp2116(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2116(i)
         enddo
         goto 20
      elseif(str.eq."-5-1nIxJ-1-6") then
         include "leshouches_R_117.inc"
         iflow=nint(jamp2117(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2117(i)
         enddo
         goto 20
      elseif(str.eq."-5-1nIxJ-2-5") then
         include "leshouches_R_118.inc"
         iflow=nint(jamp2118(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2118(i)
         enddo
         goto 20
      elseif(str.eq."-51nIxJ1-6") then
         include "leshouches_R_119.inc"
         iflow=nint(jamp2119(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2119(i)
         enddo
         goto 20
      elseif(str.eq."-5-2nIxJ-2-6") then
         include "leshouches_R_120.inc"
         iflow=nint(jamp2120(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2120(i)
         enddo
         goto 20
      elseif(str.eq."-52nIxJ1-5") then
         include "leshouches_R_121.inc"
         iflow=nint(jamp2121(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2121(i)
         enddo
         goto 20
      elseif(str.eq."-52nIxJ2-6") then
         include "leshouches_R_122.inc"
         iflow=nint(jamp2122(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2122(i)
         enddo
         goto 20
      elseif(str.eq."-5-4nIxJ-4-6") then
         include "leshouches_R_123.inc"
         iflow=nint(jamp2123(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2123(i)
         enddo
         goto 20
      elseif(str.eq."-54nIxJ4-6") then
         include "leshouches_R_124.inc"
         iflow=nint(jamp2124(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2124(i)
         enddo
         goto 20
      elseif(str.eq."-54nIxJ3-5") then
         include "leshouches_R_125.inc"
         iflow=nint(jamp2125(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2125(i)
         enddo
         goto 20
      elseif(str.eq."-5-3nIxJ-4-5") then
         include "leshouches_R_126.inc"
         iflow=nint(jamp2126(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2126(i)
         enddo
         goto 20
      elseif(str.eq."-5-3nIxJ-3-6") then
         include "leshouches_R_127.inc"
         iflow=nint(jamp2127(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2127(i)
         enddo
         goto 20
      elseif(str.eq."-53nIxJ3-6") then
         include "leshouches_R_128.inc"
         iflow=nint(jamp2128(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2128(i)
         enddo
         goto 20
      elseif(str.eq."-5-5nIxJ-5-6") then
         include "leshouches_R_129.inc"
         iflow=nint(jamp2129(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2129(i)
         enddo
         goto 20
      elseif(str.eq."-55nIxJ1-2") then
         include "leshouches_R_130.inc"
         iflow=nint(jamp2130(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2130(i)
         enddo
         goto 20
      elseif(str.eq."-55nIxJ-43") then
         include "leshouches_R_131.inc"
         iflow=nint(jamp2131(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2131(i)
         enddo
         goto 20
      elseif(str.eq."-55nIxJ5-6") then
         include "leshouches_R_132.inc"
         iflow=nint(jamp2132(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2132(i)
         enddo
         goto 20
      elseif(str.eq."-50nIxJ-60") then
         include "leshouches_R_133.inc"
         iflow=nint(jamp2133(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2133(i)
         enddo
         goto 20
      elseif(str.eq."5-1nIxJ-25") then
         include "leshouches_R_134.inc"
         iflow=nint(jamp2134(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2134(i)
         enddo
         goto 20
      elseif(str.eq."52nIxJ15") then
         include "leshouches_R_135.inc"
         iflow=nint(jamp2135(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2135(i)
         enddo
         goto 20
      elseif(str.eq."54nIxJ35") then
         include "leshouches_R_136.inc"
         iflow=nint(jamp2136(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2136(i)
         enddo
         goto 20
      elseif(str.eq."5-3nIxJ-45") then
         include "leshouches_R_137.inc"
         iflow=nint(jamp2137(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2137(i)
         enddo
         goto 20
      elseif(str.eq."5-5nIxJ1-2") then
         include "leshouches_R_138.inc"
         iflow=nint(jamp2138(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2138(i)
         enddo
         goto 20
      elseif(str.eq."5-5nIxJ-43") then
         include "leshouches_R_139.inc"
         iflow=nint(jamp2139(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2139(i)
         enddo
         goto 20
      elseif(str.eq."5-5nIxJ5-6") then
         include "leshouches_R_140.inc"
         iflow=nint(jamp2140(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2140(i)
         enddo
         goto 20
      elseif(str.eq."0-1nIxJ-20") then
         include "leshouches_R_141.inc"
         iflow=nint(jamp2141(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2141(i)
         enddo
         goto 20
      elseif(str.eq."02nIxJ10") then
         include "leshouches_R_142.inc"
         iflow=nint(jamp2142(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2142(i)
         enddo
         goto 20
      elseif(str.eq."04nIxJ30") then
         include "leshouches_R_143.inc"
         iflow=nint(jamp2143(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2143(i)
         enddo
         goto 20
      elseif(str.eq."0-3nIxJ-40") then
         include "leshouches_R_144.inc"
         iflow=nint(jamp2144(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2144(i)
         enddo
         goto 20
      elseif(str.eq."0-5nIxJ-60") then
         include "leshouches_R_145.inc"
         iflow=nint(jamp2145(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2145(i)
         enddo
         goto 20
      elseif(str.eq."00nIxJ1-2") then
         include "leshouches_R_146.inc"
         iflow=nint(jamp2146(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2146(i)
         enddo
         goto 20
      elseif(str.eq."00nIxJ-43") then
         include "leshouches_R_147.inc"
         iflow=nint(jamp2147(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2147(i)
         enddo
         goto 20
      elseif(str.eq."00nIxJ5-6") then
         include "leshouches_R_148.inc"
         iflow=nint(jamp2148(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2148(i)
         enddo
         goto 20
      endif
      
      do while(mtc)
         do i= 4+1,nexternal
            ic(i)=ic(i)- 4
         enddo
         goto 10
      enddo
      if(.not.mtc) then
         write(*,*) "Error #1, in sborn_proc.f"
         stop
      endif
      
 20   continue
      xtarget=jamp2cum(iflow)*random()
      ifl=1
      do while(jamp2cum(ifl).lt.xtarget)
         ifl=ifl+1
      enddo
      do i=1,2
         do j=1,nexternal
            color1(i,j)=ICOLUP(i,j,ifl)
         enddo
      enddo
      call switchcolor(color1,color,
     &     ic,nexternal)
      
      return
      end
      
      
      
      
