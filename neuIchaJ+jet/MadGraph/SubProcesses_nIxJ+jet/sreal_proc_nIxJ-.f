      subroutine sreal_proc_nixjm(p,legs,wgt)
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
      
      if(str.eq."-11nIxJ-12") then
         call smatrix_dxd_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-11nIxJ4-3") then
         call smatrix_dxd_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-11nIxJ-56") then
         call smatrix_dxd_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."-1-2nIxJ-1-1") then
         call smatrix_dxux_nIxJdxdx(p1,wgt)
         goto 20
      elseif(str.eq."-1-4nIxJ-1-3") then
         call smatrix_dxcx_nIxJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-13nIxJ-14") then
         call smatrix_dxs_nIxJdxc(p1,wgt)
         goto 20
      elseif(str.eq."-15nIxJ-16") then
         call smatrix_dxb_nIxJdxt(p1,wgt)
         goto 20
      elseif(str.eq."1-1nIxJ-12") then
         call smatrix_ddx_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."1-1nIxJ4-3") then
         call smatrix_ddx_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."1-1nIxJ-56") then
         call smatrix_ddx_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."11nIxJ12") then
         call smatrix_dd_nIxJdu(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ1-1") then
         call smatrix_dux_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ2-2") then
         call smatrix_dux_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ4-4") then
         call smatrix_dux_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ3-3") then
         call smatrix_dux_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ5-5") then
         call smatrix_dux_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ6-6") then
         call smatrix_dux_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."1-2nIxJ00") then
         call smatrix_dux_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."12nIxJ22") then
         call smatrix_du_nIxJuu(p1,wgt)
         goto 20
      elseif(str.eq."1-4nIxJ1-3") then
         call smatrix_dcx_nIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."1-4nIxJ2-4") then
         call smatrix_dcx_nIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."14nIxJ24") then
         call smatrix_dc_nIxJuc(p1,wgt)
         goto 20
      elseif(str.eq."1-3nIxJ2-3") then
         call smatrix_dsx_nIxJusx(p1,wgt)
         goto 20
      elseif(str.eq."13nIxJ14") then
         call smatrix_ds_nIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."13nIxJ23") then
         call smatrix_ds_nIxJus(p1,wgt)
         goto 20
      elseif(str.eq."1-5nIxJ2-5") then
         call smatrix_dbx_nIxJubx(p1,wgt)
         goto 20
      elseif(str.eq."15nIxJ16") then
         call smatrix_db_nIxJdt(p1,wgt)
         goto 20
      elseif(str.eq."15nIxJ25") then
         call smatrix_db_nIxJub(p1,wgt)
         goto 20
      elseif(str.eq."10nIxJ20") then
         call smatrix_dg_nIxJug(p1,wgt)
         goto 20
      elseif(str.eq."-2-1nIxJ-1-1") then
         call smatrix_uxdx_nIxJdxdx(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ1-1") then
         call smatrix_uxd_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ2-2") then
         call smatrix_uxd_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ4-4") then
         call smatrix_uxd_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ3-3") then
         call smatrix_uxd_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ5-5") then
         call smatrix_uxd_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ6-6") then
         call smatrix_uxd_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-21nIxJ00") then
         call smatrix_uxd_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-2-2nIxJ-1-2") then
         call smatrix_uxux_nIxJdxux(p1,wgt)
         goto 20
      elseif(str.eq."-22nIxJ-12") then
         call smatrix_uxu_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-22nIxJ4-3") then
         call smatrix_uxu_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-22nIxJ-56") then
         call smatrix_uxu_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."-2-4nIxJ-1-4") then
         call smatrix_uxcx_nIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-2-4nIxJ-2-3") then
         call smatrix_uxcx_nIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-24nIxJ-14") then
         call smatrix_uxc_nIxJdxc(p1,wgt)
         goto 20
      elseif(str.eq."-2-3nIxJ-1-3") then
         call smatrix_uxsx_nIxJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-23nIxJ-13") then
         call smatrix_uxs_nIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."-23nIxJ-24") then
         call smatrix_uxs_nIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."-2-5nIxJ-1-5") then
         call smatrix_uxbx_nIxJdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-25nIxJ-15") then
         call smatrix_uxb_nIxJdxb(p1,wgt)
         goto 20
      elseif(str.eq."-25nIxJ-26") then
         call smatrix_uxb_nIxJuxt(p1,wgt)
         goto 20
      elseif(str.eq."-20nIxJ-10") then
         call smatrix_uxg_nIxJdxg(p1,wgt)
         goto 20
      elseif(str.eq."21nIxJ22") then
         call smatrix_ud_nIxJuu(p1,wgt)
         goto 20
      elseif(str.eq."2-2nIxJ-12") then
         call smatrix_uux_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."2-2nIxJ4-3") then
         call smatrix_uux_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."2-2nIxJ-56") then
         call smatrix_uux_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."2-4nIxJ2-3") then
         call smatrix_ucx_nIxJusx(p1,wgt)
         goto 20
      elseif(str.eq."23nIxJ24") then
         call smatrix_us_nIxJuc(p1,wgt)
         goto 20
      elseif(str.eq."25nIxJ26") then
         call smatrix_ub_nIxJut(p1,wgt)
         goto 20
      elseif(str.eq."-4-1nIxJ-1-3") then
         call smatrix_cxdx_nIxJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-41nIxJ1-3") then
         call smatrix_cxd_nIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."-41nIxJ2-4") then
         call smatrix_cxd_nIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."-4-2nIxJ-1-4") then
         call smatrix_cxux_nIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-2nIxJ-2-3") then
         call smatrix_cxux_nIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-42nIxJ2-3") then
         call smatrix_cxu_nIxJusx(p1,wgt)
         goto 20
      elseif(str.eq."-4-4nIxJ-4-3") then
         call smatrix_cxcx_nIxJcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-44nIxJ-12") then
         call smatrix_cxc_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-44nIxJ4-3") then
         call smatrix_cxc_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-44nIxJ-56") then
         call smatrix_cxc_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."-4-3nIxJ-3-3") then
         call smatrix_cxsx_nIxJsxsx(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ1-1") then
         call smatrix_cxs_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ2-2") then
         call smatrix_cxs_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ4-4") then
         call smatrix_cxs_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ3-3") then
         call smatrix_cxs_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ5-5") then
         call smatrix_cxs_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ6-6") then
         call smatrix_cxs_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-43nIxJ00") then
         call smatrix_cxs_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-4-5nIxJ-3-5") then
         call smatrix_cxbx_nIxJsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-45nIxJ-46") then
         call smatrix_cxb_nIxJcxt(p1,wgt)
         goto 20
      elseif(str.eq."-45nIxJ-35") then
         call smatrix_cxb_nIxJsxb(p1,wgt)
         goto 20
      elseif(str.eq."-40nIxJ-30") then
         call smatrix_cxg_nIxJsxg(p1,wgt)
         goto 20
      elseif(str.eq."41nIxJ24") then
         call smatrix_cd_nIxJuc(p1,wgt)
         goto 20
      elseif(str.eq."4-2nIxJ-14") then
         call smatrix_cux_nIxJdxc(p1,wgt)
         goto 20
      elseif(str.eq."4-4nIxJ-12") then
         call smatrix_ccx_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."4-4nIxJ4-3") then
         call smatrix_ccx_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."4-4nIxJ-56") then
         call smatrix_ccx_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."43nIxJ44") then
         call smatrix_cs_nIxJcc(p1,wgt)
         goto 20
      elseif(str.eq."45nIxJ46") then
         call smatrix_cb_nIxJct(p1,wgt)
         goto 20
      elseif(str.eq."-31nIxJ2-3") then
         call smatrix_sxd_nIxJusx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2nIxJ-1-3") then
         call smatrix_sxux_nIxJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-3-4nIxJ-3-3") then
         call smatrix_sxcx_nIxJsxsx(p1,wgt)
         goto 20
      elseif(str.eq."-33nIxJ-12") then
         call smatrix_sxs_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-33nIxJ4-3") then
         call smatrix_sxs_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-33nIxJ-56") then
         call smatrix_sxs_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."-35nIxJ-36") then
         call smatrix_sxb_nIxJsxt(p1,wgt)
         goto 20
      elseif(str.eq."3-1nIxJ-14") then
         call smatrix_sdx_nIxJdxc(p1,wgt)
         goto 20
      elseif(str.eq."31nIxJ14") then
         call smatrix_sd_nIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."31nIxJ23") then
         call smatrix_sd_nIxJus(p1,wgt)
         goto 20
      elseif(str.eq."3-2nIxJ-13") then
         call smatrix_sux_nIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."3-2nIxJ-24") then
         call smatrix_sux_nIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."32nIxJ24") then
         call smatrix_su_nIxJuc(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ1-1") then
         call smatrix_scx_nIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ2-2") then
         call smatrix_scx_nIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ4-4") then
         call smatrix_scx_nIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ3-3") then
         call smatrix_scx_nIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ5-5") then
         call smatrix_scx_nIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ6-6") then
         call smatrix_scx_nIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."3-4nIxJ00") then
         call smatrix_scx_nIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."34nIxJ44") then
         call smatrix_sc_nIxJcc(p1,wgt)
         goto 20
      elseif(str.eq."3-3nIxJ-12") then
         call smatrix_ssx_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."3-3nIxJ4-3") then
         call smatrix_ssx_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."3-3nIxJ-56") then
         call smatrix_ssx_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."33nIxJ43") then
         call smatrix_ss_nIxJcs(p1,wgt)
         goto 20
      elseif(str.eq."3-5nIxJ4-5") then
         call smatrix_sbx_nIxJcbx(p1,wgt)
         goto 20
      elseif(str.eq."35nIxJ45") then
         call smatrix_sb_nIxJcb(p1,wgt)
         goto 20
      elseif(str.eq."35nIxJ36") then
         call smatrix_sb_nIxJst(p1,wgt)
         goto 20
      elseif(str.eq."30nIxJ40") then
         call smatrix_sg_nIxJcg(p1,wgt)
         goto 20
      elseif(str.eq."-51nIxJ2-5") then
         call smatrix_bxd_nIxJubx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2nIxJ-1-5") then
         call smatrix_bxux_nIxJdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4nIxJ-3-5") then
         call smatrix_bxcx_nIxJsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-53nIxJ4-5") then
         call smatrix_bxs_nIxJcbx(p1,wgt)
         goto 20
      elseif(str.eq."-55nIxJ-12") then
         call smatrix_bxb_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-55nIxJ4-3") then
         call smatrix_bxb_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-55nIxJ-56") then
         call smatrix_bxb_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."5-1nIxJ-16") then
         call smatrix_bdx_nIxJdxt(p1,wgt)
         goto 20
      elseif(str.eq."51nIxJ16") then
         call smatrix_bd_nIxJdt(p1,wgt)
         goto 20
      elseif(str.eq."51nIxJ25") then
         call smatrix_bd_nIxJub(p1,wgt)
         goto 20
      elseif(str.eq."5-2nIxJ-15") then
         call smatrix_bux_nIxJdxb(p1,wgt)
         goto 20
      elseif(str.eq."5-2nIxJ-26") then
         call smatrix_bux_nIxJuxt(p1,wgt)
         goto 20
      elseif(str.eq."52nIxJ26") then
         call smatrix_bu_nIxJut(p1,wgt)
         goto 20
      elseif(str.eq."5-4nIxJ-46") then
         call smatrix_bcx_nIxJcxt(p1,wgt)
         goto 20
      elseif(str.eq."5-4nIxJ-35") then
         call smatrix_bcx_nIxJsxb(p1,wgt)
         goto 20
      elseif(str.eq."54nIxJ46") then
         call smatrix_bc_nIxJct(p1,wgt)
         goto 20
      elseif(str.eq."5-3nIxJ-36") then
         call smatrix_bsx_nIxJsxt(p1,wgt)
         goto 20
      elseif(str.eq."53nIxJ45") then
         call smatrix_bs_nIxJcb(p1,wgt)
         goto 20
      elseif(str.eq."53nIxJ36") then
         call smatrix_bs_nIxJst(p1,wgt)
         goto 20
      elseif(str.eq."5-5nIxJ-12") then
         call smatrix_bbx_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."5-5nIxJ4-3") then
         call smatrix_bbx_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."5-5nIxJ-56") then
         call smatrix_bbx_nIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."55nIxJ56") then
         call smatrix_bb_nIxJbt(p1,wgt)
         goto 20
      elseif(str.eq."50nIxJ60") then
         call smatrix_bg_nIxJtg(p1,wgt)
         goto 20
      elseif(str.eq."01nIxJ20") then
         call smatrix_gd_nIxJug(p1,wgt)
         goto 20
      elseif(str.eq."0-2nIxJ-10") then
         call smatrix_gux_nIxJdxg(p1,wgt)
         goto 20
      elseif(str.eq."0-4nIxJ-30") then
         call smatrix_gcx_nIxJsxg(p1,wgt)
         goto 20
      elseif(str.eq."03nIxJ40") then
         call smatrix_gs_nIxJcg(p1,wgt)
         goto 20
      elseif(str.eq."05nIxJ60") then
         call smatrix_gb_nIxJtg(p1,wgt)
         goto 20
      elseif(str.eq."00nIxJ-12") then
         call smatrix_gg_nIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."00nIxJ4-3") then
         call smatrix_gg_nIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."00nIxJ-56") then
         call smatrix_gg_nIxJbxt(p1,wgt)
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
      
      
      subroutine real_color_nixjm(legs,color)
      implicit none
      include "nexternal.inc"
      include "maxamps.inc"
      Double Precision amp2149(maxamps), jamp2149(0:maxflow)
      common/to_Ramps_dxd_nIxJdxu/amp2149,jamp2149
      Double Precision amp2150(maxamps), jamp2150(0:maxflow)
      common/to_Ramps_dxd_nIxJcsx/amp2150,jamp2150
      Double Precision amp2151(maxamps), jamp2151(0:maxflow)
      common/to_Ramps_dxd_nIxJbxt/amp2151,jamp2151
      Double Precision amp2152(maxamps), jamp2152(0:maxflow)
      common/to_Ramps_dxux_nIxJdxdx/amp2152,jamp2152
      Double Precision amp2153(maxamps), jamp2153(0:maxflow)
      common/to_Ramps_dxcx_nIxJdxsx/amp2153,jamp2153
      Double Precision amp2154(maxamps), jamp2154(0:maxflow)
      common/to_Ramps_dxs_nIxJdxc/amp2154,jamp2154
      Double Precision amp2155(maxamps), jamp2155(0:maxflow)
      common/to_Ramps_dxb_nIxJdxt/amp2155,jamp2155
      Double Precision amp2156(maxamps), jamp2156(0:maxflow)
      common/to_Ramps_ddx_nIxJdxu/amp2156,jamp2156
      Double Precision amp2157(maxamps), jamp2157(0:maxflow)
      common/to_Ramps_ddx_nIxJcsx/amp2157,jamp2157
      Double Precision amp2158(maxamps), jamp2158(0:maxflow)
      common/to_Ramps_ddx_nIxJbxt/amp2158,jamp2158
      Double Precision amp2159(maxamps), jamp2159(0:maxflow)
      common/to_Ramps_dd_nIxJdu/amp2159,jamp2159
      Double Precision amp2160(maxamps), jamp2160(0:maxflow)
      common/to_Ramps_dux_nIxJddx/amp2160,jamp2160
      Double Precision amp2161(maxamps), jamp2161(0:maxflow)
      common/to_Ramps_dux_nIxJuux/amp2161,jamp2161
      Double Precision amp2162(maxamps), jamp2162(0:maxflow)
      common/to_Ramps_dux_nIxJccx/amp2162,jamp2162
      Double Precision amp2163(maxamps), jamp2163(0:maxflow)
      common/to_Ramps_dux_nIxJssx/amp2163,jamp2163
      Double Precision amp2164(maxamps), jamp2164(0:maxflow)
      common/to_Ramps_dux_nIxJbbx/amp2164,jamp2164
      Double Precision amp2165(maxamps), jamp2165(0:maxflow)
      common/to_Ramps_dux_nIxJttx/amp2165,jamp2165
      Double Precision amp2166(maxamps), jamp2166(0:maxflow)
      common/to_Ramps_dux_nIxJgg/amp2166,jamp2166
      Double Precision amp2167(maxamps), jamp2167(0:maxflow)
      common/to_Ramps_du_nIxJuu/amp2167,jamp2167
      Double Precision amp2168(maxamps), jamp2168(0:maxflow)
      common/to_Ramps_dcx_nIxJdsx/amp2168,jamp2168
      Double Precision amp2169(maxamps), jamp2169(0:maxflow)
      common/to_Ramps_dcx_nIxJucx/amp2169,jamp2169
      Double Precision amp2170(maxamps), jamp2170(0:maxflow)
      common/to_Ramps_dc_nIxJuc/amp2170,jamp2170
      Double Precision amp2171(maxamps), jamp2171(0:maxflow)
      common/to_Ramps_dsx_nIxJusx/amp2171,jamp2171
      Double Precision amp2172(maxamps), jamp2172(0:maxflow)
      common/to_Ramps_ds_nIxJdc/amp2172,jamp2172
      Double Precision amp2173(maxamps), jamp2173(0:maxflow)
      common/to_Ramps_ds_nIxJus/amp2173,jamp2173
      Double Precision amp2174(maxamps), jamp2174(0:maxflow)
      common/to_Ramps_dbx_nIxJubx/amp2174,jamp2174
      Double Precision amp2175(maxamps), jamp2175(0:maxflow)
      common/to_Ramps_db_nIxJdt/amp2175,jamp2175
      Double Precision amp2176(maxamps), jamp2176(0:maxflow)
      common/to_Ramps_db_nIxJub/amp2176,jamp2176
      Double Precision amp2177(maxamps), jamp2177(0:maxflow)
      common/to_Ramps_dg_nIxJug/amp2177,jamp2177
      Double Precision amp2178(maxamps), jamp2178(0:maxflow)
      common/to_Ramps_uxdx_nIxJdxdx/amp2178,jamp2178
      Double Precision amp2179(maxamps), jamp2179(0:maxflow)
      common/to_Ramps_uxd_nIxJddx/amp2179,jamp2179
      Double Precision amp2180(maxamps), jamp2180(0:maxflow)
      common/to_Ramps_uxd_nIxJuux/amp2180,jamp2180
      Double Precision amp2181(maxamps), jamp2181(0:maxflow)
      common/to_Ramps_uxd_nIxJccx/amp2181,jamp2181
      Double Precision amp2182(maxamps), jamp2182(0:maxflow)
      common/to_Ramps_uxd_nIxJssx/amp2182,jamp2182
      Double Precision amp2183(maxamps), jamp2183(0:maxflow)
      common/to_Ramps_uxd_nIxJbbx/amp2183,jamp2183
      Double Precision amp2184(maxamps), jamp2184(0:maxflow)
      common/to_Ramps_uxd_nIxJttx/amp2184,jamp2184
      Double Precision amp2185(maxamps), jamp2185(0:maxflow)
      common/to_Ramps_uxd_nIxJgg/amp2185,jamp2185
      Double Precision amp2186(maxamps), jamp2186(0:maxflow)
      common/to_Ramps_uxux_nIxJdxux/amp2186,jamp2186
      Double Precision amp2187(maxamps), jamp2187(0:maxflow)
      common/to_Ramps_uxu_nIxJdxu/amp2187,jamp2187
      Double Precision amp2188(maxamps), jamp2188(0:maxflow)
      common/to_Ramps_uxu_nIxJcsx/amp2188,jamp2188
      Double Precision amp2189(maxamps), jamp2189(0:maxflow)
      common/to_Ramps_uxu_nIxJbxt/amp2189,jamp2189
      Double Precision amp2190(maxamps), jamp2190(0:maxflow)
      common/to_Ramps_uxcx_nIxJdxcx/amp2190,jamp2190
      Double Precision amp2191(maxamps), jamp2191(0:maxflow)
      common/to_Ramps_uxcx_nIxJuxsx/amp2191,jamp2191
      Double Precision amp2192(maxamps), jamp2192(0:maxflow)
      common/to_Ramps_uxc_nIxJdxc/amp2192,jamp2192
      Double Precision amp2193(maxamps), jamp2193(0:maxflow)
      common/to_Ramps_uxsx_nIxJdxsx/amp2193,jamp2193
      Double Precision amp2194(maxamps), jamp2194(0:maxflow)
      common/to_Ramps_uxs_nIxJdxs/amp2194,jamp2194
      Double Precision amp2195(maxamps), jamp2195(0:maxflow)
      common/to_Ramps_uxs_nIxJuxc/amp2195,jamp2195
      Double Precision amp2196(maxamps), jamp2196(0:maxflow)
      common/to_Ramps_uxbx_nIxJdxbx/amp2196,jamp2196
      Double Precision amp2197(maxamps), jamp2197(0:maxflow)
      common/to_Ramps_uxb_nIxJdxb/amp2197,jamp2197
      Double Precision amp2198(maxamps), jamp2198(0:maxflow)
      common/to_Ramps_uxb_nIxJuxt/amp2198,jamp2198
      Double Precision amp2199(maxamps), jamp2199(0:maxflow)
      common/to_Ramps_uxg_nIxJdxg/amp2199,jamp2199
      Double Precision amp2200(maxamps), jamp2200(0:maxflow)
      common/to_Ramps_ud_nIxJuu/amp2200,jamp2200
      Double Precision amp2201(maxamps), jamp2201(0:maxflow)
      common/to_Ramps_uux_nIxJdxu/amp2201,jamp2201
      Double Precision amp2202(maxamps), jamp2202(0:maxflow)
      common/to_Ramps_uux_nIxJcsx/amp2202,jamp2202
      Double Precision amp2203(maxamps), jamp2203(0:maxflow)
      common/to_Ramps_uux_nIxJbxt/amp2203,jamp2203
      Double Precision amp2204(maxamps), jamp2204(0:maxflow)
      common/to_Ramps_ucx_nIxJusx/amp2204,jamp2204
      Double Precision amp2205(maxamps), jamp2205(0:maxflow)
      common/to_Ramps_us_nIxJuc/amp2205,jamp2205
      Double Precision amp2206(maxamps), jamp2206(0:maxflow)
      common/to_Ramps_ub_nIxJut/amp2206,jamp2206
      Double Precision amp2207(maxamps), jamp2207(0:maxflow)
      common/to_Ramps_cxdx_nIxJdxsx/amp2207,jamp2207
      Double Precision amp2208(maxamps), jamp2208(0:maxflow)
      common/to_Ramps_cxd_nIxJdsx/amp2208,jamp2208
      Double Precision amp2209(maxamps), jamp2209(0:maxflow)
      common/to_Ramps_cxd_nIxJucx/amp2209,jamp2209
      Double Precision amp2210(maxamps), jamp2210(0:maxflow)
      common/to_Ramps_cxux_nIxJdxcx/amp2210,jamp2210
      Double Precision amp2211(maxamps), jamp2211(0:maxflow)
      common/to_Ramps_cxux_nIxJuxsx/amp2211,jamp2211
      Double Precision amp2212(maxamps), jamp2212(0:maxflow)
      common/to_Ramps_cxu_nIxJusx/amp2212,jamp2212
      Double Precision amp2213(maxamps), jamp2213(0:maxflow)
      common/to_Ramps_cxcx_nIxJcxsx/amp2213,jamp2213
      Double Precision amp2214(maxamps), jamp2214(0:maxflow)
      common/to_Ramps_cxc_nIxJdxu/amp2214,jamp2214
      Double Precision amp2215(maxamps), jamp2215(0:maxflow)
      common/to_Ramps_cxc_nIxJcsx/amp2215,jamp2215
      Double Precision amp2216(maxamps), jamp2216(0:maxflow)
      common/to_Ramps_cxc_nIxJbxt/amp2216,jamp2216
      Double Precision amp2217(maxamps), jamp2217(0:maxflow)
      common/to_Ramps_cxsx_nIxJsxsx/amp2217,jamp2217
      Double Precision amp2218(maxamps), jamp2218(0:maxflow)
      common/to_Ramps_cxs_nIxJddx/amp2218,jamp2218
      Double Precision amp2219(maxamps), jamp2219(0:maxflow)
      common/to_Ramps_cxs_nIxJuux/amp2219,jamp2219
      Double Precision amp2220(maxamps), jamp2220(0:maxflow)
      common/to_Ramps_cxs_nIxJccx/amp2220,jamp2220
      Double Precision amp2221(maxamps), jamp2221(0:maxflow)
      common/to_Ramps_cxs_nIxJssx/amp2221,jamp2221
      Double Precision amp2222(maxamps), jamp2222(0:maxflow)
      common/to_Ramps_cxs_nIxJbbx/amp2222,jamp2222
      Double Precision amp2223(maxamps), jamp2223(0:maxflow)
      common/to_Ramps_cxs_nIxJttx/amp2223,jamp2223
      Double Precision amp2224(maxamps), jamp2224(0:maxflow)
      common/to_Ramps_cxs_nIxJgg/amp2224,jamp2224
      Double Precision amp2225(maxamps), jamp2225(0:maxflow)
      common/to_Ramps_cxbx_nIxJsxbx/amp2225,jamp2225
      Double Precision amp2226(maxamps), jamp2226(0:maxflow)
      common/to_Ramps_cxb_nIxJcxt/amp2226,jamp2226
      Double Precision amp2227(maxamps), jamp2227(0:maxflow)
      common/to_Ramps_cxb_nIxJsxb/amp2227,jamp2227
      Double Precision amp2228(maxamps), jamp2228(0:maxflow)
      common/to_Ramps_cxg_nIxJsxg/amp2228,jamp2228
      Double Precision amp2229(maxamps), jamp2229(0:maxflow)
      common/to_Ramps_cd_nIxJuc/amp2229,jamp2229
      Double Precision amp2230(maxamps), jamp2230(0:maxflow)
      common/to_Ramps_cux_nIxJdxc/amp2230,jamp2230
      Double Precision amp2231(maxamps), jamp2231(0:maxflow)
      common/to_Ramps_ccx_nIxJdxu/amp2231,jamp2231
      Double Precision amp2232(maxamps), jamp2232(0:maxflow)
      common/to_Ramps_ccx_nIxJcsx/amp2232,jamp2232
      Double Precision amp2233(maxamps), jamp2233(0:maxflow)
      common/to_Ramps_ccx_nIxJbxt/amp2233,jamp2233
      Double Precision amp2234(maxamps), jamp2234(0:maxflow)
      common/to_Ramps_cs_nIxJcc/amp2234,jamp2234
      Double Precision amp2235(maxamps), jamp2235(0:maxflow)
      common/to_Ramps_cb_nIxJct/amp2235,jamp2235
      Double Precision amp2236(maxamps), jamp2236(0:maxflow)
      common/to_Ramps_sxd_nIxJusx/amp2236,jamp2236
      Double Precision amp2237(maxamps), jamp2237(0:maxflow)
      common/to_Ramps_sxux_nIxJdxsx/amp2237,jamp2237
      Double Precision amp2238(maxamps), jamp2238(0:maxflow)
      common/to_Ramps_sxcx_nIxJsxsx/amp2238,jamp2238
      Double Precision amp2239(maxamps), jamp2239(0:maxflow)
      common/to_Ramps_sxs_nIxJdxu/amp2239,jamp2239
      Double Precision amp2240(maxamps), jamp2240(0:maxflow)
      common/to_Ramps_sxs_nIxJcsx/amp2240,jamp2240
      Double Precision amp2241(maxamps), jamp2241(0:maxflow)
      common/to_Ramps_sxs_nIxJbxt/amp2241,jamp2241
      Double Precision amp2242(maxamps), jamp2242(0:maxflow)
      common/to_Ramps_sxb_nIxJsxt/amp2242,jamp2242
      Double Precision amp2243(maxamps), jamp2243(0:maxflow)
      common/to_Ramps_sdx_nIxJdxc/amp2243,jamp2243
      Double Precision amp2244(maxamps), jamp2244(0:maxflow)
      common/to_Ramps_sd_nIxJdc/amp2244,jamp2244
      Double Precision amp2245(maxamps), jamp2245(0:maxflow)
      common/to_Ramps_sd_nIxJus/amp2245,jamp2245
      Double Precision amp2246(maxamps), jamp2246(0:maxflow)
      common/to_Ramps_sux_nIxJdxs/amp2246,jamp2246
      Double Precision amp2247(maxamps), jamp2247(0:maxflow)
      common/to_Ramps_sux_nIxJuxc/amp2247,jamp2247
      Double Precision amp2248(maxamps), jamp2248(0:maxflow)
      common/to_Ramps_su_nIxJuc/amp2248,jamp2248
      Double Precision amp2249(maxamps), jamp2249(0:maxflow)
      common/to_Ramps_scx_nIxJddx/amp2249,jamp2249
      Double Precision amp2250(maxamps), jamp2250(0:maxflow)
      common/to_Ramps_scx_nIxJuux/amp2250,jamp2250
      Double Precision amp2251(maxamps), jamp2251(0:maxflow)
      common/to_Ramps_scx_nIxJccx/amp2251,jamp2251
      Double Precision amp2252(maxamps), jamp2252(0:maxflow)
      common/to_Ramps_scx_nIxJssx/amp2252,jamp2252
      Double Precision amp2253(maxamps), jamp2253(0:maxflow)
      common/to_Ramps_scx_nIxJbbx/amp2253,jamp2253
      Double Precision amp2254(maxamps), jamp2254(0:maxflow)
      common/to_Ramps_scx_nIxJttx/amp2254,jamp2254
      Double Precision amp2255(maxamps), jamp2255(0:maxflow)
      common/to_Ramps_scx_nIxJgg/amp2255,jamp2255
      Double Precision amp2256(maxamps), jamp2256(0:maxflow)
      common/to_Ramps_sc_nIxJcc/amp2256,jamp2256
      Double Precision amp2257(maxamps), jamp2257(0:maxflow)
      common/to_Ramps_ssx_nIxJdxu/amp2257,jamp2257
      Double Precision amp2258(maxamps), jamp2258(0:maxflow)
      common/to_Ramps_ssx_nIxJcsx/amp2258,jamp2258
      Double Precision amp2259(maxamps), jamp2259(0:maxflow)
      common/to_Ramps_ssx_nIxJbxt/amp2259,jamp2259
      Double Precision amp2260(maxamps), jamp2260(0:maxflow)
      common/to_Ramps_ss_nIxJcs/amp2260,jamp2260
      Double Precision amp2261(maxamps), jamp2261(0:maxflow)
      common/to_Ramps_sbx_nIxJcbx/amp2261,jamp2261
      Double Precision amp2262(maxamps), jamp2262(0:maxflow)
      common/to_Ramps_sb_nIxJcb/amp2262,jamp2262
      Double Precision amp2263(maxamps), jamp2263(0:maxflow)
      common/to_Ramps_sb_nIxJst/amp2263,jamp2263
      Double Precision amp2264(maxamps), jamp2264(0:maxflow)
      common/to_Ramps_sg_nIxJcg/amp2264,jamp2264
      Double Precision amp2265(maxamps), jamp2265(0:maxflow)
      common/to_Ramps_bxd_nIxJubx/amp2265,jamp2265
      Double Precision amp2266(maxamps), jamp2266(0:maxflow)
      common/to_Ramps_bxux_nIxJdxbx/amp2266,jamp2266
      Double Precision amp2267(maxamps), jamp2267(0:maxflow)
      common/to_Ramps_bxcx_nIxJsxbx/amp2267,jamp2267
      Double Precision amp2268(maxamps), jamp2268(0:maxflow)
      common/to_Ramps_bxs_nIxJcbx/amp2268,jamp2268
      Double Precision amp2269(maxamps), jamp2269(0:maxflow)
      common/to_Ramps_bxb_nIxJdxu/amp2269,jamp2269
      Double Precision amp2270(maxamps), jamp2270(0:maxflow)
      common/to_Ramps_bxb_nIxJcsx/amp2270,jamp2270
      Double Precision amp2271(maxamps), jamp2271(0:maxflow)
      common/to_Ramps_bxb_nIxJbxt/amp2271,jamp2271
      Double Precision amp2272(maxamps), jamp2272(0:maxflow)
      common/to_Ramps_bdx_nIxJdxt/amp2272,jamp2272
      Double Precision amp2273(maxamps), jamp2273(0:maxflow)
      common/to_Ramps_bd_nIxJdt/amp2273,jamp2273
      Double Precision amp2274(maxamps), jamp2274(0:maxflow)
      common/to_Ramps_bd_nIxJub/amp2274,jamp2274
      Double Precision amp2275(maxamps), jamp2275(0:maxflow)
      common/to_Ramps_bux_nIxJdxb/amp2275,jamp2275
      Double Precision amp2276(maxamps), jamp2276(0:maxflow)
      common/to_Ramps_bux_nIxJuxt/amp2276,jamp2276
      Double Precision amp2277(maxamps), jamp2277(0:maxflow)
      common/to_Ramps_bu_nIxJut/amp2277,jamp2277
      Double Precision amp2278(maxamps), jamp2278(0:maxflow)
      common/to_Ramps_bcx_nIxJcxt/amp2278,jamp2278
      Double Precision amp2279(maxamps), jamp2279(0:maxflow)
      common/to_Ramps_bcx_nIxJsxb/amp2279,jamp2279
      Double Precision amp2280(maxamps), jamp2280(0:maxflow)
      common/to_Ramps_bc_nIxJct/amp2280,jamp2280
      Double Precision amp2281(maxamps), jamp2281(0:maxflow)
      common/to_Ramps_bsx_nIxJsxt/amp2281,jamp2281
      Double Precision amp2282(maxamps), jamp2282(0:maxflow)
      common/to_Ramps_bs_nIxJcb/amp2282,jamp2282
      Double Precision amp2283(maxamps), jamp2283(0:maxflow)
      common/to_Ramps_bs_nIxJst/amp2283,jamp2283
      Double Precision amp2284(maxamps), jamp2284(0:maxflow)
      common/to_Ramps_bbx_nIxJdxu/amp2284,jamp2284
      Double Precision amp2285(maxamps), jamp2285(0:maxflow)
      common/to_Ramps_bbx_nIxJcsx/amp2285,jamp2285
      Double Precision amp2286(maxamps), jamp2286(0:maxflow)
      common/to_Ramps_bbx_nIxJbxt/amp2286,jamp2286
      Double Precision amp2287(maxamps), jamp2287(0:maxflow)
      common/to_Ramps_bb_nIxJbt/amp2287,jamp2287
      Double Precision amp2288(maxamps), jamp2288(0:maxflow)
      common/to_Ramps_bg_nIxJtg/amp2288,jamp2288
      Double Precision amp2289(maxamps), jamp2289(0:maxflow)
      common/to_Ramps_gd_nIxJug/amp2289,jamp2289
      Double Precision amp2290(maxamps), jamp2290(0:maxflow)
      common/to_Ramps_gux_nIxJdxg/amp2290,jamp2290
      Double Precision amp2291(maxamps), jamp2291(0:maxflow)
      common/to_Ramps_gcx_nIxJsxg/amp2291,jamp2291
      Double Precision amp2292(maxamps), jamp2292(0:maxflow)
      common/to_Ramps_gs_nIxJcg/amp2292,jamp2292
      Double Precision amp2293(maxamps), jamp2293(0:maxflow)
      common/to_Ramps_gb_nIxJtg/amp2293,jamp2293
      Double Precision amp2294(maxamps), jamp2294(0:maxflow)
      common/to_Ramps_gg_nIxJdxu/amp2294,jamp2294
      Double Precision amp2295(maxamps), jamp2295(0:maxflow)
      common/to_Ramps_gg_nIxJcsx/amp2295,jamp2295
      Double Precision amp2296(maxamps), jamp2296(0:maxflow)
      common/to_Ramps_gg_nIxJbxt/amp2296,jamp2296
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
      
      if(str.eq."-11nIxJ-12") then
         include "leshouches_R_149.inc"
         iflow=nint(jamp2149(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2149(i)
         enddo
         goto 20
      elseif(str.eq."-11nIxJ4-3") then
         include "leshouches_R_150.inc"
         iflow=nint(jamp2150(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2150(i)
         enddo
         goto 20
      elseif(str.eq."-11nIxJ-56") then
         include "leshouches_R_151.inc"
         iflow=nint(jamp2151(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2151(i)
         enddo
         goto 20
      elseif(str.eq."-1-2nIxJ-1-1") then
         include "leshouches_R_152.inc"
         iflow=nint(jamp2152(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2152(i)
         enddo
         goto 20
      elseif(str.eq."-1-4nIxJ-1-3") then
         include "leshouches_R_153.inc"
         iflow=nint(jamp2153(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2153(i)
         enddo
         goto 20
      elseif(str.eq."-13nIxJ-14") then
         include "leshouches_R_154.inc"
         iflow=nint(jamp2154(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2154(i)
         enddo
         goto 20
      elseif(str.eq."-15nIxJ-16") then
         include "leshouches_R_155.inc"
         iflow=nint(jamp2155(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2155(i)
         enddo
         goto 20
      elseif(str.eq."1-1nIxJ-12") then
         include "leshouches_R_156.inc"
         iflow=nint(jamp2156(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2156(i)
         enddo
         goto 20
      elseif(str.eq."1-1nIxJ4-3") then
         include "leshouches_R_157.inc"
         iflow=nint(jamp2157(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2157(i)
         enddo
         goto 20
      elseif(str.eq."1-1nIxJ-56") then
         include "leshouches_R_158.inc"
         iflow=nint(jamp2158(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2158(i)
         enddo
         goto 20
      elseif(str.eq."11nIxJ12") then
         include "leshouches_R_159.inc"
         iflow=nint(jamp2159(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2159(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ1-1") then
         include "leshouches_R_160.inc"
         iflow=nint(jamp2160(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2160(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ2-2") then
         include "leshouches_R_161.inc"
         iflow=nint(jamp2161(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2161(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ4-4") then
         include "leshouches_R_162.inc"
         iflow=nint(jamp2162(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2162(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ3-3") then
         include "leshouches_R_163.inc"
         iflow=nint(jamp2163(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2163(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ5-5") then
         include "leshouches_R_164.inc"
         iflow=nint(jamp2164(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2164(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ6-6") then
         include "leshouches_R_165.inc"
         iflow=nint(jamp2165(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2165(i)
         enddo
         goto 20
      elseif(str.eq."1-2nIxJ00") then
         include "leshouches_R_166.inc"
         iflow=nint(jamp2166(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2166(i)
         enddo
         goto 20
      elseif(str.eq."12nIxJ22") then
         include "leshouches_R_167.inc"
         iflow=nint(jamp2167(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2167(i)
         enddo
         goto 20
      elseif(str.eq."1-4nIxJ1-3") then
         include "leshouches_R_168.inc"
         iflow=nint(jamp2168(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2168(i)
         enddo
         goto 20
      elseif(str.eq."1-4nIxJ2-4") then
         include "leshouches_R_169.inc"
         iflow=nint(jamp2169(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2169(i)
         enddo
         goto 20
      elseif(str.eq."14nIxJ24") then
         include "leshouches_R_170.inc"
         iflow=nint(jamp2170(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2170(i)
         enddo
         goto 20
      elseif(str.eq."1-3nIxJ2-3") then
         include "leshouches_R_171.inc"
         iflow=nint(jamp2171(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2171(i)
         enddo
         goto 20
      elseif(str.eq."13nIxJ14") then
         include "leshouches_R_172.inc"
         iflow=nint(jamp2172(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2172(i)
         enddo
         goto 20
      elseif(str.eq."13nIxJ23") then
         include "leshouches_R_173.inc"
         iflow=nint(jamp2173(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2173(i)
         enddo
         goto 20
      elseif(str.eq."1-5nIxJ2-5") then
         include "leshouches_R_174.inc"
         iflow=nint(jamp2174(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2174(i)
         enddo
         goto 20
      elseif(str.eq."15nIxJ16") then
         include "leshouches_R_175.inc"
         iflow=nint(jamp2175(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2175(i)
         enddo
         goto 20
      elseif(str.eq."15nIxJ25") then
         include "leshouches_R_176.inc"
         iflow=nint(jamp2176(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2176(i)
         enddo
         goto 20
      elseif(str.eq."10nIxJ20") then
         include "leshouches_R_177.inc"
         iflow=nint(jamp2177(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2177(i)
         enddo
         goto 20
      elseif(str.eq."-2-1nIxJ-1-1") then
         include "leshouches_R_178.inc"
         iflow=nint(jamp2178(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2178(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ1-1") then
         include "leshouches_R_179.inc"
         iflow=nint(jamp2179(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2179(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ2-2") then
         include "leshouches_R_180.inc"
         iflow=nint(jamp2180(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2180(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ4-4") then
         include "leshouches_R_181.inc"
         iflow=nint(jamp2181(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2181(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ3-3") then
         include "leshouches_R_182.inc"
         iflow=nint(jamp2182(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2182(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ5-5") then
         include "leshouches_R_183.inc"
         iflow=nint(jamp2183(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2183(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ6-6") then
         include "leshouches_R_184.inc"
         iflow=nint(jamp2184(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2184(i)
         enddo
         goto 20
      elseif(str.eq."-21nIxJ00") then
         include "leshouches_R_185.inc"
         iflow=nint(jamp2185(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2185(i)
         enddo
         goto 20
      elseif(str.eq."-2-2nIxJ-1-2") then
         include "leshouches_R_186.inc"
         iflow=nint(jamp2186(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2186(i)
         enddo
         goto 20
      elseif(str.eq."-22nIxJ-12") then
         include "leshouches_R_187.inc"
         iflow=nint(jamp2187(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2187(i)
         enddo
         goto 20
      elseif(str.eq."-22nIxJ4-3") then
         include "leshouches_R_188.inc"
         iflow=nint(jamp2188(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2188(i)
         enddo
         goto 20
      elseif(str.eq."-22nIxJ-56") then
         include "leshouches_R_189.inc"
         iflow=nint(jamp2189(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2189(i)
         enddo
         goto 20
      elseif(str.eq."-2-4nIxJ-1-4") then
         include "leshouches_R_190.inc"
         iflow=nint(jamp2190(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2190(i)
         enddo
         goto 20
      elseif(str.eq."-2-4nIxJ-2-3") then
         include "leshouches_R_191.inc"
         iflow=nint(jamp2191(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2191(i)
         enddo
         goto 20
      elseif(str.eq."-24nIxJ-14") then
         include "leshouches_R_192.inc"
         iflow=nint(jamp2192(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2192(i)
         enddo
         goto 20
      elseif(str.eq."-2-3nIxJ-1-3") then
         include "leshouches_R_193.inc"
         iflow=nint(jamp2193(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2193(i)
         enddo
         goto 20
      elseif(str.eq."-23nIxJ-13") then
         include "leshouches_R_194.inc"
         iflow=nint(jamp2194(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2194(i)
         enddo
         goto 20
      elseif(str.eq."-23nIxJ-24") then
         include "leshouches_R_195.inc"
         iflow=nint(jamp2195(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2195(i)
         enddo
         goto 20
      elseif(str.eq."-2-5nIxJ-1-5") then
         include "leshouches_R_196.inc"
         iflow=nint(jamp2196(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2196(i)
         enddo
         goto 20
      elseif(str.eq."-25nIxJ-15") then
         include "leshouches_R_197.inc"
         iflow=nint(jamp2197(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2197(i)
         enddo
         goto 20
      elseif(str.eq."-25nIxJ-26") then
         include "leshouches_R_198.inc"
         iflow=nint(jamp2198(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2198(i)
         enddo
         goto 20
      elseif(str.eq."-20nIxJ-10") then
         include "leshouches_R_199.inc"
         iflow=nint(jamp2199(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2199(i)
         enddo
         goto 20
      elseif(str.eq."21nIxJ22") then
         include "leshouches_R_200.inc"
         iflow=nint(jamp2200(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2200(i)
         enddo
         goto 20
      elseif(str.eq."2-2nIxJ-12") then
         include "leshouches_R_201.inc"
         iflow=nint(jamp2201(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2201(i)
         enddo
         goto 20
      elseif(str.eq."2-2nIxJ4-3") then
         include "leshouches_R_202.inc"
         iflow=nint(jamp2202(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2202(i)
         enddo
         goto 20
      elseif(str.eq."2-2nIxJ-56") then
         include "leshouches_R_203.inc"
         iflow=nint(jamp2203(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2203(i)
         enddo
         goto 20
      elseif(str.eq."2-4nIxJ2-3") then
         include "leshouches_R_204.inc"
         iflow=nint(jamp2204(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2204(i)
         enddo
         goto 20
      elseif(str.eq."23nIxJ24") then
         include "leshouches_R_205.inc"
         iflow=nint(jamp2205(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2205(i)
         enddo
         goto 20
      elseif(str.eq."25nIxJ26") then
         include "leshouches_R_206.inc"
         iflow=nint(jamp2206(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2206(i)
         enddo
         goto 20
      elseif(str.eq."-4-1nIxJ-1-3") then
         include "leshouches_R_207.inc"
         iflow=nint(jamp2207(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2207(i)
         enddo
         goto 20
      elseif(str.eq."-41nIxJ1-3") then
         include "leshouches_R_208.inc"
         iflow=nint(jamp2208(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2208(i)
         enddo
         goto 20
      elseif(str.eq."-41nIxJ2-4") then
         include "leshouches_R_209.inc"
         iflow=nint(jamp2209(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2209(i)
         enddo
         goto 20
      elseif(str.eq."-4-2nIxJ-1-4") then
         include "leshouches_R_210.inc"
         iflow=nint(jamp2210(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2210(i)
         enddo
         goto 20
      elseif(str.eq."-4-2nIxJ-2-3") then
         include "leshouches_R_211.inc"
         iflow=nint(jamp2211(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2211(i)
         enddo
         goto 20
      elseif(str.eq."-42nIxJ2-3") then
         include "leshouches_R_212.inc"
         iflow=nint(jamp2212(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2212(i)
         enddo
         goto 20
      elseif(str.eq."-4-4nIxJ-4-3") then
         include "leshouches_R_213.inc"
         iflow=nint(jamp2213(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2213(i)
         enddo
         goto 20
      elseif(str.eq."-44nIxJ-12") then
         include "leshouches_R_214.inc"
         iflow=nint(jamp2214(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2214(i)
         enddo
         goto 20
      elseif(str.eq."-44nIxJ4-3") then
         include "leshouches_R_215.inc"
         iflow=nint(jamp2215(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2215(i)
         enddo
         goto 20
      elseif(str.eq."-44nIxJ-56") then
         include "leshouches_R_216.inc"
         iflow=nint(jamp2216(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2216(i)
         enddo
         goto 20
      elseif(str.eq."-4-3nIxJ-3-3") then
         include "leshouches_R_217.inc"
         iflow=nint(jamp2217(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2217(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ1-1") then
         include "leshouches_R_218.inc"
         iflow=nint(jamp2218(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2218(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ2-2") then
         include "leshouches_R_219.inc"
         iflow=nint(jamp2219(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2219(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ4-4") then
         include "leshouches_R_220.inc"
         iflow=nint(jamp2220(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2220(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ3-3") then
         include "leshouches_R_221.inc"
         iflow=nint(jamp2221(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2221(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ5-5") then
         include "leshouches_R_222.inc"
         iflow=nint(jamp2222(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2222(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ6-6") then
         include "leshouches_R_223.inc"
         iflow=nint(jamp2223(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2223(i)
         enddo
         goto 20
      elseif(str.eq."-43nIxJ00") then
         include "leshouches_R_224.inc"
         iflow=nint(jamp2224(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2224(i)
         enddo
         goto 20
      elseif(str.eq."-4-5nIxJ-3-5") then
         include "leshouches_R_225.inc"
         iflow=nint(jamp2225(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2225(i)
         enddo
         goto 20
      elseif(str.eq."-45nIxJ-46") then
         include "leshouches_R_226.inc"
         iflow=nint(jamp2226(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2226(i)
         enddo
         goto 20
      elseif(str.eq."-45nIxJ-35") then
         include "leshouches_R_227.inc"
         iflow=nint(jamp2227(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2227(i)
         enddo
         goto 20
      elseif(str.eq."-40nIxJ-30") then
         include "leshouches_R_228.inc"
         iflow=nint(jamp2228(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2228(i)
         enddo
         goto 20
      elseif(str.eq."41nIxJ24") then
         include "leshouches_R_229.inc"
         iflow=nint(jamp2229(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2229(i)
         enddo
         goto 20
      elseif(str.eq."4-2nIxJ-14") then
         include "leshouches_R_230.inc"
         iflow=nint(jamp2230(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2230(i)
         enddo
         goto 20
      elseif(str.eq."4-4nIxJ-12") then
         include "leshouches_R_231.inc"
         iflow=nint(jamp2231(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2231(i)
         enddo
         goto 20
      elseif(str.eq."4-4nIxJ4-3") then
         include "leshouches_R_232.inc"
         iflow=nint(jamp2232(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2232(i)
         enddo
         goto 20
      elseif(str.eq."4-4nIxJ-56") then
         include "leshouches_R_233.inc"
         iflow=nint(jamp2233(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2233(i)
         enddo
         goto 20
      elseif(str.eq."43nIxJ44") then
         include "leshouches_R_234.inc"
         iflow=nint(jamp2234(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2234(i)
         enddo
         goto 20
      elseif(str.eq."45nIxJ46") then
         include "leshouches_R_235.inc"
         iflow=nint(jamp2235(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2235(i)
         enddo
         goto 20
      elseif(str.eq."-31nIxJ2-3") then
         include "leshouches_R_236.inc"
         iflow=nint(jamp2236(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2236(i)
         enddo
         goto 20
      elseif(str.eq."-3-2nIxJ-1-3") then
         include "leshouches_R_237.inc"
         iflow=nint(jamp2237(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2237(i)
         enddo
         goto 20
      elseif(str.eq."-3-4nIxJ-3-3") then
         include "leshouches_R_238.inc"
         iflow=nint(jamp2238(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2238(i)
         enddo
         goto 20
      elseif(str.eq."-33nIxJ-12") then
         include "leshouches_R_239.inc"
         iflow=nint(jamp2239(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2239(i)
         enddo
         goto 20
      elseif(str.eq."-33nIxJ4-3") then
         include "leshouches_R_240.inc"
         iflow=nint(jamp2240(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2240(i)
         enddo
         goto 20
      elseif(str.eq."-33nIxJ-56") then
         include "leshouches_R_241.inc"
         iflow=nint(jamp2241(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2241(i)
         enddo
         goto 20
      elseif(str.eq."-35nIxJ-36") then
         include "leshouches_R_242.inc"
         iflow=nint(jamp2242(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2242(i)
         enddo
         goto 20
      elseif(str.eq."3-1nIxJ-14") then
         include "leshouches_R_243.inc"
         iflow=nint(jamp2243(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2243(i)
         enddo
         goto 20
      elseif(str.eq."31nIxJ14") then
         include "leshouches_R_244.inc"
         iflow=nint(jamp2244(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2244(i)
         enddo
         goto 20
      elseif(str.eq."31nIxJ23") then
         include "leshouches_R_245.inc"
         iflow=nint(jamp2245(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2245(i)
         enddo
         goto 20
      elseif(str.eq."3-2nIxJ-13") then
         include "leshouches_R_246.inc"
         iflow=nint(jamp2246(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2246(i)
         enddo
         goto 20
      elseif(str.eq."3-2nIxJ-24") then
         include "leshouches_R_247.inc"
         iflow=nint(jamp2247(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2247(i)
         enddo
         goto 20
      elseif(str.eq."32nIxJ24") then
         include "leshouches_R_248.inc"
         iflow=nint(jamp2248(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2248(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ1-1") then
         include "leshouches_R_249.inc"
         iflow=nint(jamp2249(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2249(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ2-2") then
         include "leshouches_R_250.inc"
         iflow=nint(jamp2250(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2250(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ4-4") then
         include "leshouches_R_251.inc"
         iflow=nint(jamp2251(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2251(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ3-3") then
         include "leshouches_R_252.inc"
         iflow=nint(jamp2252(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2252(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ5-5") then
         include "leshouches_R_253.inc"
         iflow=nint(jamp2253(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2253(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ6-6") then
         include "leshouches_R_254.inc"
         iflow=nint(jamp2254(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2254(i)
         enddo
         goto 20
      elseif(str.eq."3-4nIxJ00") then
         include "leshouches_R_255.inc"
         iflow=nint(jamp2255(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2255(i)
         enddo
         goto 20
      elseif(str.eq."34nIxJ44") then
         include "leshouches_R_256.inc"
         iflow=nint(jamp2256(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2256(i)
         enddo
         goto 20
      elseif(str.eq."3-3nIxJ-12") then
         include "leshouches_R_257.inc"
         iflow=nint(jamp2257(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2257(i)
         enddo
         goto 20
      elseif(str.eq."3-3nIxJ4-3") then
         include "leshouches_R_258.inc"
         iflow=nint(jamp2258(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2258(i)
         enddo
         goto 20
      elseif(str.eq."3-3nIxJ-56") then
         include "leshouches_R_259.inc"
         iflow=nint(jamp2259(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2259(i)
         enddo
         goto 20
      elseif(str.eq."33nIxJ43") then
         include "leshouches_R_260.inc"
         iflow=nint(jamp2260(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2260(i)
         enddo
         goto 20
      elseif(str.eq."3-5nIxJ4-5") then
         include "leshouches_R_261.inc"
         iflow=nint(jamp2261(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2261(i)
         enddo
         goto 20
      elseif(str.eq."35nIxJ45") then
         include "leshouches_R_262.inc"
         iflow=nint(jamp2262(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2262(i)
         enddo
         goto 20
      elseif(str.eq."35nIxJ36") then
         include "leshouches_R_263.inc"
         iflow=nint(jamp2263(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2263(i)
         enddo
         goto 20
      elseif(str.eq."30nIxJ40") then
         include "leshouches_R_264.inc"
         iflow=nint(jamp2264(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2264(i)
         enddo
         goto 20
      elseif(str.eq."-51nIxJ2-5") then
         include "leshouches_R_265.inc"
         iflow=nint(jamp2265(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2265(i)
         enddo
         goto 20
      elseif(str.eq."-5-2nIxJ-1-5") then
         include "leshouches_R_266.inc"
         iflow=nint(jamp2266(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2266(i)
         enddo
         goto 20
      elseif(str.eq."-5-4nIxJ-3-5") then
         include "leshouches_R_267.inc"
         iflow=nint(jamp2267(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2267(i)
         enddo
         goto 20
      elseif(str.eq."-53nIxJ4-5") then
         include "leshouches_R_268.inc"
         iflow=nint(jamp2268(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2268(i)
         enddo
         goto 20
      elseif(str.eq."-55nIxJ-12") then
         include "leshouches_R_269.inc"
         iflow=nint(jamp2269(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2269(i)
         enddo
         goto 20
      elseif(str.eq."-55nIxJ4-3") then
         include "leshouches_R_270.inc"
         iflow=nint(jamp2270(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2270(i)
         enddo
         goto 20
      elseif(str.eq."-55nIxJ-56") then
         include "leshouches_R_271.inc"
         iflow=nint(jamp2271(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2271(i)
         enddo
         goto 20
      elseif(str.eq."5-1nIxJ-16") then
         include "leshouches_R_272.inc"
         iflow=nint(jamp2272(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2272(i)
         enddo
         goto 20
      elseif(str.eq."51nIxJ16") then
         include "leshouches_R_273.inc"
         iflow=nint(jamp2273(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2273(i)
         enddo
         goto 20
      elseif(str.eq."51nIxJ25") then
         include "leshouches_R_274.inc"
         iflow=nint(jamp2274(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2274(i)
         enddo
         goto 20
      elseif(str.eq."5-2nIxJ-15") then
         include "leshouches_R_275.inc"
         iflow=nint(jamp2275(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2275(i)
         enddo
         goto 20
      elseif(str.eq."5-2nIxJ-26") then
         include "leshouches_R_276.inc"
         iflow=nint(jamp2276(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2276(i)
         enddo
         goto 20
      elseif(str.eq."52nIxJ26") then
         include "leshouches_R_277.inc"
         iflow=nint(jamp2277(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2277(i)
         enddo
         goto 20
      elseif(str.eq."5-4nIxJ-46") then
         include "leshouches_R_278.inc"
         iflow=nint(jamp2278(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2278(i)
         enddo
         goto 20
      elseif(str.eq."5-4nIxJ-35") then
         include "leshouches_R_279.inc"
         iflow=nint(jamp2279(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2279(i)
         enddo
         goto 20
      elseif(str.eq."54nIxJ46") then
         include "leshouches_R_280.inc"
         iflow=nint(jamp2280(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2280(i)
         enddo
         goto 20
      elseif(str.eq."5-3nIxJ-36") then
         include "leshouches_R_281.inc"
         iflow=nint(jamp2281(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2281(i)
         enddo
         goto 20
      elseif(str.eq."53nIxJ45") then
         include "leshouches_R_282.inc"
         iflow=nint(jamp2282(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2282(i)
         enddo
         goto 20
      elseif(str.eq."53nIxJ36") then
         include "leshouches_R_283.inc"
         iflow=nint(jamp2283(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2283(i)
         enddo
         goto 20
      elseif(str.eq."5-5nIxJ-12") then
         include "leshouches_R_284.inc"
         iflow=nint(jamp2284(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2284(i)
         enddo
         goto 20
      elseif(str.eq."5-5nIxJ4-3") then
         include "leshouches_R_285.inc"
         iflow=nint(jamp2285(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2285(i)
         enddo
         goto 20
      elseif(str.eq."5-5nIxJ-56") then
         include "leshouches_R_286.inc"
         iflow=nint(jamp2286(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2286(i)
         enddo
         goto 20
      elseif(str.eq."55nIxJ56") then
         include "leshouches_R_287.inc"
         iflow=nint(jamp2287(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2287(i)
         enddo
         goto 20
      elseif(str.eq."50nIxJ60") then
         include "leshouches_R_288.inc"
         iflow=nint(jamp2288(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2288(i)
         enddo
         goto 20
      elseif(str.eq."01nIxJ20") then
         include "leshouches_R_289.inc"
         iflow=nint(jamp2289(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2289(i)
         enddo
         goto 20
      elseif(str.eq."0-2nIxJ-10") then
         include "leshouches_R_290.inc"
         iflow=nint(jamp2290(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2290(i)
         enddo
         goto 20
      elseif(str.eq."0-4nIxJ-30") then
         include "leshouches_R_291.inc"
         iflow=nint(jamp2291(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2291(i)
         enddo
         goto 20
      elseif(str.eq."03nIxJ40") then
         include "leshouches_R_292.inc"
         iflow=nint(jamp2292(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2292(i)
         enddo
         goto 20
      elseif(str.eq."05nIxJ60") then
         include "leshouches_R_293.inc"
         iflow=nint(jamp2293(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2293(i)
         enddo
         goto 20
      elseif(str.eq."00nIxJ-12") then
         include "leshouches_R_294.inc"
         iflow=nint(jamp2294(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2294(i)
         enddo
         goto 20
      elseif(str.eq."00nIxJ4-3") then
         include "leshouches_R_295.inc"
         iflow=nint(jamp2295(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2295(i)
         enddo
         goto 20
      elseif(str.eq."00nIxJ-56") then
         include "leshouches_R_296.inc"
         iflow=nint(jamp2296(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2296(i)
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
      
      
      
      
