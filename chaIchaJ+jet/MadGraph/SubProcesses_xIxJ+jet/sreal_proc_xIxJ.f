      subroutine sreal_proc_xixj(p,legs,wgt)
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
      
      if(str.eq."-1-1xIxJ-1-1") then
         call smatrix_dxdx_xIxJdxdx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ1-1") then
         call smatrix_dxd_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ2-2") then
         call smatrix_dxd_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ4-4") then
         call smatrix_dxd_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ3-3") then
         call smatrix_dxd_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ5-5") then
         call smatrix_dxd_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ6-6") then
         call smatrix_dxd_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxJ00") then
         call smatrix_dxd_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-1-2xIxJ-1-2") then
         call smatrix_dxux_xIxJdxux(p1,wgt)
         goto 20
      elseif(str.eq."-12xIxJ-12") then
         call smatrix_dxu_xIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-12xIxJ4-3") then
         call smatrix_dxu_xIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-12xIxJ-56") then
         call smatrix_dxu_xIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."-1-4xIxJ-1-4") then
         call smatrix_dxcx_xIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-1-4xIxJ-2-3") then
         call smatrix_dxcx_xIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-14xIxJ-14") then
         call smatrix_dxc_xIxJdxc(p1,wgt)
         goto 20
      elseif(str.eq."-1-3xIxJ-1-3") then
         call smatrix_dxsx_xIxJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-13xIxJ-13") then
         call smatrix_dxs_xIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."-13xIxJ-24") then
         call smatrix_dxs_xIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."-1-5xIxJ-1-5") then
         call smatrix_dxbx_xIxJdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-15xIxJ-15") then
         call smatrix_dxb_xIxJdxb(p1,wgt)
         goto 20
      elseif(str.eq."-15xIxJ-26") then
         call smatrix_dxb_xIxJuxt(p1,wgt)
         goto 20
      elseif(str.eq."-10xIxJ-10") then
         call smatrix_dxg_xIxJdxg(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ1-1") then
         call smatrix_ddx_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ2-2") then
         call smatrix_ddx_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ4-4") then
         call smatrix_ddx_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ3-3") then
         call smatrix_ddx_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ5-5") then
         call smatrix_ddx_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ6-6") then
         call smatrix_ddx_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxJ00") then
         call smatrix_ddx_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."11xIxJ11") then
         call smatrix_dd_xIxJdd(p1,wgt)
         goto 20
      elseif(str.eq."1-2xIxJ1-2") then
         call smatrix_dux_xIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."1-2xIxJ-43") then
         call smatrix_dux_xIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."1-2xIxJ5-6") then
         call smatrix_dux_xIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."12xIxJ12") then
         call smatrix_du_xIxJdu(p1,wgt)
         goto 20
      elseif(str.eq."1-4xIxJ1-4") then
         call smatrix_dcx_xIxJdcx(p1,wgt)
         goto 20
      elseif(str.eq."14xIxJ14") then
         call smatrix_dc_xIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."14xIxJ23") then
         call smatrix_dc_xIxJus(p1,wgt)
         goto 20
      elseif(str.eq."1-3xIxJ1-3") then
         call smatrix_dsx_xIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."1-3xIxJ2-4") then
         call smatrix_dsx_xIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."13xIxJ13") then
         call smatrix_ds_xIxJds(p1,wgt)
         goto 20
      elseif(str.eq."1-5xIxJ1-5") then
         call smatrix_dbx_xIxJdbx(p1,wgt)
         goto 20
      elseif(str.eq."1-5xIxJ2-6") then
         call smatrix_dbx_xIxJutx(p1,wgt)
         goto 20
      elseif(str.eq."15xIxJ15") then
         call smatrix_db_xIxJdb(p1,wgt)
         goto 20
      elseif(str.eq."10xIxJ10") then
         call smatrix_dg_xIxJdg(p1,wgt)
         goto 20
      elseif(str.eq."-2-1xIxJ-1-2") then
         call smatrix_uxdx_xIxJdxux(p1,wgt)
         goto 20
      elseif(str.eq."-21xIxJ1-2") then
         call smatrix_uxd_xIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-21xIxJ-43") then
         call smatrix_uxd_xIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-21xIxJ5-6") then
         call smatrix_uxd_xIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-2-2xIxJ-2-2") then
         call smatrix_uxux_xIxJuxux(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ1-1") then
         call smatrix_uxu_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ2-2") then
         call smatrix_uxu_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ4-4") then
         call smatrix_uxu_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ3-3") then
         call smatrix_uxu_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ5-5") then
         call smatrix_uxu_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ6-6") then
         call smatrix_uxu_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxJ00") then
         call smatrix_uxu_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-2-4xIxJ-2-4") then
         call smatrix_uxcx_xIxJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-24xIxJ-13") then
         call smatrix_uxc_xIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."-24xIxJ-24") then
         call smatrix_uxc_xIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."-2-3xIxJ-1-4") then
         call smatrix_uxsx_xIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-2-3xIxJ-2-3") then
         call smatrix_uxsx_xIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-23xIxJ-23") then
         call smatrix_uxs_xIxJuxs(p1,wgt)
         goto 20
      elseif(str.eq."-2-5xIxJ-1-6") then
         call smatrix_uxbx_xIxJdxtx(p1,wgt)
         goto 20
      elseif(str.eq."-2-5xIxJ-2-5") then
         call smatrix_uxbx_xIxJuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-25xIxJ-25") then
         call smatrix_uxb_xIxJuxb(p1,wgt)
         goto 20
      elseif(str.eq."-20xIxJ-20") then
         call smatrix_uxg_xIxJuxg(p1,wgt)
         goto 20
      elseif(str.eq."2-1xIxJ-12") then
         call smatrix_udx_xIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."2-1xIxJ4-3") then
         call smatrix_udx_xIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."2-1xIxJ-56") then
         call smatrix_udx_xIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."21xIxJ12") then
         call smatrix_ud_xIxJdu(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ1-1") then
         call smatrix_uux_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ2-2") then
         call smatrix_uux_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ4-4") then
         call smatrix_uux_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ3-3") then
         call smatrix_uux_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ5-5") then
         call smatrix_uux_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ6-6") then
         call smatrix_uux_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxJ00") then
         call smatrix_uux_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."22xIxJ22") then
         call smatrix_uu_xIxJuu(p1,wgt)
         goto 20
      elseif(str.eq."2-4xIxJ1-3") then
         call smatrix_ucx_xIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."2-4xIxJ2-4") then
         call smatrix_ucx_xIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."24xIxJ24") then
         call smatrix_uc_xIxJuc(p1,wgt)
         goto 20
      elseif(str.eq."2-3xIxJ2-3") then
         call smatrix_usx_xIxJusx(p1,wgt)
         goto 20
      elseif(str.eq."23xIxJ14") then
         call smatrix_us_xIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."23xIxJ23") then
         call smatrix_us_xIxJus(p1,wgt)
         goto 20
      elseif(str.eq."2-5xIxJ2-5") then
         call smatrix_ubx_xIxJubx(p1,wgt)
         goto 20
      elseif(str.eq."25xIxJ16") then
         call smatrix_ub_xIxJdt(p1,wgt)
         goto 20
      elseif(str.eq."25xIxJ25") then
         call smatrix_ub_xIxJub(p1,wgt)
         goto 20
      elseif(str.eq."20xIxJ20") then
         call smatrix_ug_xIxJug(p1,wgt)
         goto 20
      elseif(str.eq."-4-1xIxJ-1-4") then
         call smatrix_cxdx_xIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-1xIxJ-2-3") then
         call smatrix_cxdx_xIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-41xIxJ1-4") then
         call smatrix_cxd_xIxJdcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-2xIxJ-2-4") then
         call smatrix_cxux_xIxJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-42xIxJ1-3") then
         call smatrix_cxu_xIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."-42xIxJ2-4") then
         call smatrix_cxu_xIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."-4-4xIxJ-4-4") then
         call smatrix_cxcx_xIxJcxcx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ1-1") then
         call smatrix_cxc_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ2-2") then
         call smatrix_cxc_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ4-4") then
         call smatrix_cxc_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ3-3") then
         call smatrix_cxc_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ5-5") then
         call smatrix_cxc_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ6-6") then
         call smatrix_cxc_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxJ00") then
         call smatrix_cxc_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-4-3xIxJ-4-3") then
         call smatrix_cxsx_xIxJcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-43xIxJ1-2") then
         call smatrix_cxs_xIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."-43xIxJ-43") then
         call smatrix_cxs_xIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-43xIxJ5-6") then
         call smatrix_cxs_xIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."-4-5xIxJ-4-5") then
         call smatrix_cxbx_xIxJcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-4-5xIxJ-3-6") then
         call smatrix_cxbx_xIxJsxtx(p1,wgt)
         goto 20
      elseif(str.eq."-45xIxJ-45") then
         call smatrix_cxb_xIxJcxb(p1,wgt)
         goto 20
      elseif(str.eq."-40xIxJ-40") then
         call smatrix_cxg_xIxJcxg(p1,wgt)
         goto 20
      elseif(str.eq."4-1xIxJ-14") then
         call smatrix_cdx_xIxJdxc(p1,wgt)
         goto 20
      elseif(str.eq."41xIxJ14") then
         call smatrix_cd_xIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."41xIxJ23") then
         call smatrix_cd_xIxJus(p1,wgt)
         goto 20
      elseif(str.eq."4-2xIxJ-13") then
         call smatrix_cux_xIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."4-2xIxJ-24") then
         call smatrix_cux_xIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."42xIxJ24") then
         call smatrix_cu_xIxJuc(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ1-1") then
         call smatrix_ccx_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ2-2") then
         call smatrix_ccx_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ4-4") then
         call smatrix_ccx_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ3-3") then
         call smatrix_ccx_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ5-5") then
         call smatrix_ccx_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ6-6") then
         call smatrix_ccx_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxJ00") then
         call smatrix_ccx_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."44xIxJ44") then
         call smatrix_cc_xIxJcc(p1,wgt)
         goto 20
      elseif(str.eq."4-3xIxJ-12") then
         call smatrix_csx_xIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."4-3xIxJ4-3") then
         call smatrix_csx_xIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."4-3xIxJ-56") then
         call smatrix_csx_xIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."43xIxJ43") then
         call smatrix_cs_xIxJcs(p1,wgt)
         goto 20
      elseif(str.eq."4-5xIxJ4-5") then
         call smatrix_cbx_xIxJcbx(p1,wgt)
         goto 20
      elseif(str.eq."45xIxJ45") then
         call smatrix_cb_xIxJcb(p1,wgt)
         goto 20
      elseif(str.eq."45xIxJ36") then
         call smatrix_cb_xIxJst(p1,wgt)
         goto 20
      elseif(str.eq."40xIxJ40") then
         call smatrix_cg_xIxJcg(p1,wgt)
         goto 20
      elseif(str.eq."-3-1xIxJ-1-3") then
         call smatrix_sxdx_xIxJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-31xIxJ1-3") then
         call smatrix_sxd_xIxJdsx(p1,wgt)
         goto 20
      elseif(str.eq."-31xIxJ2-4") then
         call smatrix_sxd_xIxJucx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2xIxJ-1-4") then
         call smatrix_sxux_xIxJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2xIxJ-2-3") then
         call smatrix_sxux_xIxJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-32xIxJ2-3") then
         call smatrix_sxu_xIxJusx(p1,wgt)
         goto 20
      elseif(str.eq."-3-4xIxJ-4-3") then
         call smatrix_sxcx_xIxJcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-34xIxJ-12") then
         call smatrix_sxc_xIxJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-34xIxJ4-3") then
         call smatrix_sxc_xIxJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-34xIxJ-56") then
         call smatrix_sxc_xIxJbxt(p1,wgt)
         goto 20
      elseif(str.eq."-3-3xIxJ-3-3") then
         call smatrix_sxsx_xIxJsxsx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ1-1") then
         call smatrix_sxs_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ2-2") then
         call smatrix_sxs_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ4-4") then
         call smatrix_sxs_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ3-3") then
         call smatrix_sxs_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ5-5") then
         call smatrix_sxs_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ6-6") then
         call smatrix_sxs_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxJ00") then
         call smatrix_sxs_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-3-5xIxJ-3-5") then
         call smatrix_sxbx_xIxJsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-35xIxJ-46") then
         call smatrix_sxb_xIxJcxt(p1,wgt)
         goto 20
      elseif(str.eq."-35xIxJ-35") then
         call smatrix_sxb_xIxJsxb(p1,wgt)
         goto 20
      elseif(str.eq."-30xIxJ-30") then
         call smatrix_sxg_xIxJsxg(p1,wgt)
         goto 20
      elseif(str.eq."3-1xIxJ-13") then
         call smatrix_sdx_xIxJdxs(p1,wgt)
         goto 20
      elseif(str.eq."3-1xIxJ-24") then
         call smatrix_sdx_xIxJuxc(p1,wgt)
         goto 20
      elseif(str.eq."31xIxJ13") then
         call smatrix_sd_xIxJds(p1,wgt)
         goto 20
      elseif(str.eq."3-2xIxJ-23") then
         call smatrix_sux_xIxJuxs(p1,wgt)
         goto 20
      elseif(str.eq."32xIxJ14") then
         call smatrix_su_xIxJdc(p1,wgt)
         goto 20
      elseif(str.eq."32xIxJ23") then
         call smatrix_su_xIxJus(p1,wgt)
         goto 20
      elseif(str.eq."3-4xIxJ1-2") then
         call smatrix_scx_xIxJdux(p1,wgt)
         goto 20
      elseif(str.eq."3-4xIxJ-43") then
         call smatrix_scx_xIxJcxs(p1,wgt)
         goto 20
      elseif(str.eq."3-4xIxJ5-6") then
         call smatrix_scx_xIxJbtx(p1,wgt)
         goto 20
      elseif(str.eq."34xIxJ43") then
         call smatrix_sc_xIxJcs(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ1-1") then
         call smatrix_ssx_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ2-2") then
         call smatrix_ssx_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ4-4") then
         call smatrix_ssx_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ3-3") then
         call smatrix_ssx_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ5-5") then
         call smatrix_ssx_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ6-6") then
         call smatrix_ssx_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxJ00") then
         call smatrix_ssx_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."33xIxJ33") then
         call smatrix_ss_xIxJss(p1,wgt)
         goto 20
      elseif(str.eq."3-5xIxJ4-6") then
         call smatrix_sbx_xIxJctx(p1,wgt)
         goto 20
      elseif(str.eq."3-5xIxJ3-5") then
         call smatrix_sbx_xIxJsbx(p1,wgt)
         goto 20
      elseif(str.eq."35xIxJ35") then
         call smatrix_sb_xIxJsb(p1,wgt)
         goto 20
      elseif(str.eq."30xIxJ30") then
         call smatrix_sg_xIxJsg(p1,wgt)
         goto 20
      elseif(str.eq."-5-1xIxJ-1-5") then
         call smatrix_bxdx_xIxJdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-51xIxJ1-5") then
         call smatrix_bxd_xIxJdbx(p1,wgt)
         goto 20
      elseif(str.eq."-51xIxJ2-6") then
         call smatrix_bxd_xIxJutx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2xIxJ-1-6") then
         call smatrix_bxux_xIxJdxtx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2xIxJ-2-5") then
         call smatrix_bxux_xIxJuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-52xIxJ2-5") then
         call smatrix_bxu_xIxJubx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4xIxJ-4-5") then
         call smatrix_bxcx_xIxJcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4xIxJ-3-6") then
         call smatrix_bxcx_xIxJsxtx(p1,wgt)
         goto 20
      elseif(str.eq."-54xIxJ4-5") then
         call smatrix_bxc_xIxJcbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-3xIxJ-3-5") then
         call smatrix_bxsx_xIxJsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-53xIxJ4-6") then
         call smatrix_bxs_xIxJctx(p1,wgt)
         goto 20
      elseif(str.eq."-53xIxJ3-5") then
         call smatrix_bxs_xIxJsbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-5xIxJ-5-5") then
         call smatrix_bxbx_xIxJbxbx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ1-1") then
         call smatrix_bxb_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ2-2") then
         call smatrix_bxb_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ4-4") then
         call smatrix_bxb_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ3-3") then
         call smatrix_bxb_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ5-5") then
         call smatrix_bxb_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ6-6") then
         call smatrix_bxb_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxJ00") then
         call smatrix_bxb_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."-50xIxJ-50") then
         call smatrix_bxg_xIxJbxg(p1,wgt)
         goto 20
      elseif(str.eq."5-1xIxJ-15") then
         call smatrix_bdx_xIxJdxb(p1,wgt)
         goto 20
      elseif(str.eq."5-1xIxJ-26") then
         call smatrix_bdx_xIxJuxt(p1,wgt)
         goto 20
      elseif(str.eq."51xIxJ15") then
         call smatrix_bd_xIxJdb(p1,wgt)
         goto 20
      elseif(str.eq."5-2xIxJ-25") then
         call smatrix_bux_xIxJuxb(p1,wgt)
         goto 20
      elseif(str.eq."52xIxJ16") then
         call smatrix_bu_xIxJdt(p1,wgt)
         goto 20
      elseif(str.eq."52xIxJ25") then
         call smatrix_bu_xIxJub(p1,wgt)
         goto 20
      elseif(str.eq."5-4xIxJ-45") then
         call smatrix_bcx_xIxJcxb(p1,wgt)
         goto 20
      elseif(str.eq."54xIxJ45") then
         call smatrix_bc_xIxJcb(p1,wgt)
         goto 20
      elseif(str.eq."54xIxJ36") then
         call smatrix_bc_xIxJst(p1,wgt)
         goto 20
      elseif(str.eq."5-3xIxJ-46") then
         call smatrix_bsx_xIxJcxt(p1,wgt)
         goto 20
      elseif(str.eq."5-3xIxJ-35") then
         call smatrix_bsx_xIxJsxb(p1,wgt)
         goto 20
      elseif(str.eq."53xIxJ35") then
         call smatrix_bs_xIxJsb(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ1-1") then
         call smatrix_bbx_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ2-2") then
         call smatrix_bbx_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ4-4") then
         call smatrix_bbx_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ3-3") then
         call smatrix_bbx_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ5-5") then
         call smatrix_bbx_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ6-6") then
         call smatrix_bbx_xIxJttx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxJ00") then
         call smatrix_bbx_xIxJgg(p1,wgt)
         goto 20
      elseif(str.eq."55xIxJ55") then
         call smatrix_bb_xIxJbb(p1,wgt)
         goto 20
      elseif(str.eq."50xIxJ50") then
         call smatrix_bg_xIxJbg(p1,wgt)
         goto 20
      elseif(str.eq."0-1xIxJ-10") then
         call smatrix_gdx_xIxJdxg(p1,wgt)
         goto 20
      elseif(str.eq."01xIxJ10") then
         call smatrix_gd_xIxJdg(p1,wgt)
         goto 20
      elseif(str.eq."0-2xIxJ-20") then
         call smatrix_gux_xIxJuxg(p1,wgt)
         goto 20
      elseif(str.eq."02xIxJ20") then
         call smatrix_gu_xIxJug(p1,wgt)
         goto 20
      elseif(str.eq."0-4xIxJ-40") then
         call smatrix_gcx_xIxJcxg(p1,wgt)
         goto 20
      elseif(str.eq."04xIxJ40") then
         call smatrix_gc_xIxJcg(p1,wgt)
         goto 20
      elseif(str.eq."0-3xIxJ-30") then
         call smatrix_gsx_xIxJsxg(p1,wgt)
         goto 20
      elseif(str.eq."03xIxJ30") then
         call smatrix_gs_xIxJsg(p1,wgt)
         goto 20
      elseif(str.eq."0-5xIxJ-50") then
         call smatrix_gbx_xIxJbxg(p1,wgt)
         goto 20
      elseif(str.eq."05xIxJ50") then
         call smatrix_gb_xIxJbg(p1,wgt)
         goto 20
      elseif(str.eq."00xIxJ1-1") then
         call smatrix_gg_xIxJddx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxJ2-2") then
         call smatrix_gg_xIxJuux(p1,wgt)
         goto 20
      elseif(str.eq."00xIxJ4-4") then
         call smatrix_gg_xIxJccx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxJ3-3") then
         call smatrix_gg_xIxJssx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxJ5-5") then
         call smatrix_gg_xIxJbbx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxJ6-6") then
         call smatrix_gg_xIxJttx(p1,wgt)
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
      
      
      subroutine real_color_xixj(legs,color)
      implicit none
      include "nexternal.inc"
      include "maxamps.inc"
      Double Precision amp2001(maxamps), jamp2001(0:maxflow)
      common/to_Ramps_dxdx_xIxJdxdx/amp2001,jamp2001
      Double Precision amp2002(maxamps), jamp2002(0:maxflow)
      common/to_Ramps_dxd_xIxJddx/amp2002,jamp2002
      Double Precision amp2003(maxamps), jamp2003(0:maxflow)
      common/to_Ramps_dxd_xIxJuux/amp2003,jamp2003
      Double Precision amp2004(maxamps), jamp2004(0:maxflow)
      common/to_Ramps_dxd_xIxJccx/amp2004,jamp2004
      Double Precision amp2005(maxamps), jamp2005(0:maxflow)
      common/to_Ramps_dxd_xIxJssx/amp2005,jamp2005
      Double Precision amp2006(maxamps), jamp2006(0:maxflow)
      common/to_Ramps_dxd_xIxJbbx/amp2006,jamp2006
      Double Precision amp2007(maxamps), jamp2007(0:maxflow)
      common/to_Ramps_dxd_xIxJttx/amp2007,jamp2007
      Double Precision amp2008(maxamps), jamp2008(0:maxflow)
      common/to_Ramps_dxd_xIxJgg/amp2008,jamp2008
      Double Precision amp2009(maxamps), jamp2009(0:maxflow)
      common/to_Ramps_dxux_xIxJdxux/amp2009,jamp2009
      Double Precision amp2010(maxamps), jamp2010(0:maxflow)
      common/to_Ramps_dxu_xIxJdxu/amp2010,jamp2010
      Double Precision amp2011(maxamps), jamp2011(0:maxflow)
      common/to_Ramps_dxu_xIxJcsx/amp2011,jamp2011
      Double Precision amp2012(maxamps), jamp2012(0:maxflow)
      common/to_Ramps_dxu_xIxJbxt/amp2012,jamp2012
      Double Precision amp2013(maxamps), jamp2013(0:maxflow)
      common/to_Ramps_dxcx_xIxJdxcx/amp2013,jamp2013
      Double Precision amp2014(maxamps), jamp2014(0:maxflow)
      common/to_Ramps_dxcx_xIxJuxsx/amp2014,jamp2014
      Double Precision amp2015(maxamps), jamp2015(0:maxflow)
      common/to_Ramps_dxc_xIxJdxc/amp2015,jamp2015
      Double Precision amp2016(maxamps), jamp2016(0:maxflow)
      common/to_Ramps_dxsx_xIxJdxsx/amp2016,jamp2016
      Double Precision amp2017(maxamps), jamp2017(0:maxflow)
      common/to_Ramps_dxs_xIxJdxs/amp2017,jamp2017
      Double Precision amp2018(maxamps), jamp2018(0:maxflow)
      common/to_Ramps_dxs_xIxJuxc/amp2018,jamp2018
      Double Precision amp2019(maxamps), jamp2019(0:maxflow)
      common/to_Ramps_dxbx_xIxJdxbx/amp2019,jamp2019
      Double Precision amp2020(maxamps), jamp2020(0:maxflow)
      common/to_Ramps_dxb_xIxJdxb/amp2020,jamp2020
      Double Precision amp2021(maxamps), jamp2021(0:maxflow)
      common/to_Ramps_dxb_xIxJuxt/amp2021,jamp2021
      Double Precision amp2022(maxamps), jamp2022(0:maxflow)
      common/to_Ramps_dxg_xIxJdxg/amp2022,jamp2022
      Double Precision amp2023(maxamps), jamp2023(0:maxflow)
      common/to_Ramps_ddx_xIxJddx/amp2023,jamp2023
      Double Precision amp2024(maxamps), jamp2024(0:maxflow)
      common/to_Ramps_ddx_xIxJuux/amp2024,jamp2024
      Double Precision amp2025(maxamps), jamp2025(0:maxflow)
      common/to_Ramps_ddx_xIxJccx/amp2025,jamp2025
      Double Precision amp2026(maxamps), jamp2026(0:maxflow)
      common/to_Ramps_ddx_xIxJssx/amp2026,jamp2026
      Double Precision amp2027(maxamps), jamp2027(0:maxflow)
      common/to_Ramps_ddx_xIxJbbx/amp2027,jamp2027
      Double Precision amp2028(maxamps), jamp2028(0:maxflow)
      common/to_Ramps_ddx_xIxJttx/amp2028,jamp2028
      Double Precision amp2029(maxamps), jamp2029(0:maxflow)
      common/to_Ramps_ddx_xIxJgg/amp2029,jamp2029
      Double Precision amp2030(maxamps), jamp2030(0:maxflow)
      common/to_Ramps_dd_xIxJdd/amp2030,jamp2030
      Double Precision amp2031(maxamps), jamp2031(0:maxflow)
      common/to_Ramps_dux_xIxJdux/amp2031,jamp2031
      Double Precision amp2032(maxamps), jamp2032(0:maxflow)
      common/to_Ramps_dux_xIxJcxs/amp2032,jamp2032
      Double Precision amp2033(maxamps), jamp2033(0:maxflow)
      common/to_Ramps_dux_xIxJbtx/amp2033,jamp2033
      Double Precision amp2034(maxamps), jamp2034(0:maxflow)
      common/to_Ramps_du_xIxJdu/amp2034,jamp2034
      Double Precision amp2035(maxamps), jamp2035(0:maxflow)
      common/to_Ramps_dcx_xIxJdcx/amp2035,jamp2035
      Double Precision amp2036(maxamps), jamp2036(0:maxflow)
      common/to_Ramps_dc_xIxJdc/amp2036,jamp2036
      Double Precision amp2037(maxamps), jamp2037(0:maxflow)
      common/to_Ramps_dc_xIxJus/amp2037,jamp2037
      Double Precision amp2038(maxamps), jamp2038(0:maxflow)
      common/to_Ramps_dsx_xIxJdsx/amp2038,jamp2038
      Double Precision amp2039(maxamps), jamp2039(0:maxflow)
      common/to_Ramps_dsx_xIxJucx/amp2039,jamp2039
      Double Precision amp2040(maxamps), jamp2040(0:maxflow)
      common/to_Ramps_ds_xIxJds/amp2040,jamp2040
      Double Precision amp2041(maxamps), jamp2041(0:maxflow)
      common/to_Ramps_dbx_xIxJdbx/amp2041,jamp2041
      Double Precision amp2042(maxamps), jamp2042(0:maxflow)
      common/to_Ramps_dbx_xIxJutx/amp2042,jamp2042
      Double Precision amp2043(maxamps), jamp2043(0:maxflow)
      common/to_Ramps_db_xIxJdb/amp2043,jamp2043
      Double Precision amp2044(maxamps), jamp2044(0:maxflow)
      common/to_Ramps_dg_xIxJdg/amp2044,jamp2044
      Double Precision amp2045(maxamps), jamp2045(0:maxflow)
      common/to_Ramps_uxdx_xIxJdxux/amp2045,jamp2045
      Double Precision amp2046(maxamps), jamp2046(0:maxflow)
      common/to_Ramps_uxd_xIxJdux/amp2046,jamp2046
      Double Precision amp2047(maxamps), jamp2047(0:maxflow)
      common/to_Ramps_uxd_xIxJcxs/amp2047,jamp2047
      Double Precision amp2048(maxamps), jamp2048(0:maxflow)
      common/to_Ramps_uxd_xIxJbtx/amp2048,jamp2048
      Double Precision amp2049(maxamps), jamp2049(0:maxflow)
      common/to_Ramps_uxux_xIxJuxux/amp2049,jamp2049
      Double Precision amp2050(maxamps), jamp2050(0:maxflow)
      common/to_Ramps_uxu_xIxJddx/amp2050,jamp2050
      Double Precision amp2051(maxamps), jamp2051(0:maxflow)
      common/to_Ramps_uxu_xIxJuux/amp2051,jamp2051
      Double Precision amp2052(maxamps), jamp2052(0:maxflow)
      common/to_Ramps_uxu_xIxJccx/amp2052,jamp2052
      Double Precision amp2053(maxamps), jamp2053(0:maxflow)
      common/to_Ramps_uxu_xIxJssx/amp2053,jamp2053
      Double Precision amp2054(maxamps), jamp2054(0:maxflow)
      common/to_Ramps_uxu_xIxJbbx/amp2054,jamp2054
      Double Precision amp2055(maxamps), jamp2055(0:maxflow)
      common/to_Ramps_uxu_xIxJttx/amp2055,jamp2055
      Double Precision amp2056(maxamps), jamp2056(0:maxflow)
      common/to_Ramps_uxu_xIxJgg/amp2056,jamp2056
      Double Precision amp2057(maxamps), jamp2057(0:maxflow)
      common/to_Ramps_uxcx_xIxJuxcx/amp2057,jamp2057
      Double Precision amp2058(maxamps), jamp2058(0:maxflow)
      common/to_Ramps_uxc_xIxJdxs/amp2058,jamp2058
      Double Precision amp2059(maxamps), jamp2059(0:maxflow)
      common/to_Ramps_uxc_xIxJuxc/amp2059,jamp2059
      Double Precision amp2060(maxamps), jamp2060(0:maxflow)
      common/to_Ramps_uxsx_xIxJdxcx/amp2060,jamp2060
      Double Precision amp2061(maxamps), jamp2061(0:maxflow)
      common/to_Ramps_uxsx_xIxJuxsx/amp2061,jamp2061
      Double Precision amp2062(maxamps), jamp2062(0:maxflow)
      common/to_Ramps_uxs_xIxJuxs/amp2062,jamp2062
      Double Precision amp2063(maxamps), jamp2063(0:maxflow)
      common/to_Ramps_uxbx_xIxJdxtx/amp2063,jamp2063
      Double Precision amp2064(maxamps), jamp2064(0:maxflow)
      common/to_Ramps_uxbx_xIxJuxbx/amp2064,jamp2064
      Double Precision amp2065(maxamps), jamp2065(0:maxflow)
      common/to_Ramps_uxb_xIxJuxb/amp2065,jamp2065
      Double Precision amp2066(maxamps), jamp2066(0:maxflow)
      common/to_Ramps_uxg_xIxJuxg/amp2066,jamp2066
      Double Precision amp2067(maxamps), jamp2067(0:maxflow)
      common/to_Ramps_udx_xIxJdxu/amp2067,jamp2067
      Double Precision amp2068(maxamps), jamp2068(0:maxflow)
      common/to_Ramps_udx_xIxJcsx/amp2068,jamp2068
      Double Precision amp2069(maxamps), jamp2069(0:maxflow)
      common/to_Ramps_udx_xIxJbxt/amp2069,jamp2069
      Double Precision amp2070(maxamps), jamp2070(0:maxflow)
      common/to_Ramps_ud_xIxJdu/amp2070,jamp2070
      Double Precision amp2071(maxamps), jamp2071(0:maxflow)
      common/to_Ramps_uux_xIxJddx/amp2071,jamp2071
      Double Precision amp2072(maxamps), jamp2072(0:maxflow)
      common/to_Ramps_uux_xIxJuux/amp2072,jamp2072
      Double Precision amp2073(maxamps), jamp2073(0:maxflow)
      common/to_Ramps_uux_xIxJccx/amp2073,jamp2073
      Double Precision amp2074(maxamps), jamp2074(0:maxflow)
      common/to_Ramps_uux_xIxJssx/amp2074,jamp2074
      Double Precision amp2075(maxamps), jamp2075(0:maxflow)
      common/to_Ramps_uux_xIxJbbx/amp2075,jamp2075
      Double Precision amp2076(maxamps), jamp2076(0:maxflow)
      common/to_Ramps_uux_xIxJttx/amp2076,jamp2076
      Double Precision amp2077(maxamps), jamp2077(0:maxflow)
      common/to_Ramps_uux_xIxJgg/amp2077,jamp2077
      Double Precision amp2078(maxamps), jamp2078(0:maxflow)
      common/to_Ramps_uu_xIxJuu/amp2078,jamp2078
      Double Precision amp2079(maxamps), jamp2079(0:maxflow)
      common/to_Ramps_ucx_xIxJdsx/amp2079,jamp2079
      Double Precision amp2080(maxamps), jamp2080(0:maxflow)
      common/to_Ramps_ucx_xIxJucx/amp2080,jamp2080
      Double Precision amp2081(maxamps), jamp2081(0:maxflow)
      common/to_Ramps_uc_xIxJuc/amp2081,jamp2081
      Double Precision amp2082(maxamps), jamp2082(0:maxflow)
      common/to_Ramps_usx_xIxJusx/amp2082,jamp2082
      Double Precision amp2083(maxamps), jamp2083(0:maxflow)
      common/to_Ramps_us_xIxJdc/amp2083,jamp2083
      Double Precision amp2084(maxamps), jamp2084(0:maxflow)
      common/to_Ramps_us_xIxJus/amp2084,jamp2084
      Double Precision amp2085(maxamps), jamp2085(0:maxflow)
      common/to_Ramps_ubx_xIxJubx/amp2085,jamp2085
      Double Precision amp2086(maxamps), jamp2086(0:maxflow)
      common/to_Ramps_ub_xIxJdt/amp2086,jamp2086
      Double Precision amp2087(maxamps), jamp2087(0:maxflow)
      common/to_Ramps_ub_xIxJub/amp2087,jamp2087
      Double Precision amp2088(maxamps), jamp2088(0:maxflow)
      common/to_Ramps_ug_xIxJug/amp2088,jamp2088
      Double Precision amp2089(maxamps), jamp2089(0:maxflow)
      common/to_Ramps_cxdx_xIxJdxcx/amp2089,jamp2089
      Double Precision amp2090(maxamps), jamp2090(0:maxflow)
      common/to_Ramps_cxdx_xIxJuxsx/amp2090,jamp2090
      Double Precision amp2091(maxamps), jamp2091(0:maxflow)
      common/to_Ramps_cxd_xIxJdcx/amp2091,jamp2091
      Double Precision amp2092(maxamps), jamp2092(0:maxflow)
      common/to_Ramps_cxux_xIxJuxcx/amp2092,jamp2092
      Double Precision amp2093(maxamps), jamp2093(0:maxflow)
      common/to_Ramps_cxu_xIxJdsx/amp2093,jamp2093
      Double Precision amp2094(maxamps), jamp2094(0:maxflow)
      common/to_Ramps_cxu_xIxJucx/amp2094,jamp2094
      Double Precision amp2095(maxamps), jamp2095(0:maxflow)
      common/to_Ramps_cxcx_xIxJcxcx/amp2095,jamp2095
      Double Precision amp2096(maxamps), jamp2096(0:maxflow)
      common/to_Ramps_cxc_xIxJddx/amp2096,jamp2096
      Double Precision amp2097(maxamps), jamp2097(0:maxflow)
      common/to_Ramps_cxc_xIxJuux/amp2097,jamp2097
      Double Precision amp2098(maxamps), jamp2098(0:maxflow)
      common/to_Ramps_cxc_xIxJccx/amp2098,jamp2098
      Double Precision amp2099(maxamps), jamp2099(0:maxflow)
      common/to_Ramps_cxc_xIxJssx/amp2099,jamp2099
      Double Precision amp2100(maxamps), jamp2100(0:maxflow)
      common/to_Ramps_cxc_xIxJbbx/amp2100,jamp2100
      Double Precision amp2101(maxamps), jamp2101(0:maxflow)
      common/to_Ramps_cxc_xIxJttx/amp2101,jamp2101
      Double Precision amp2102(maxamps), jamp2102(0:maxflow)
      common/to_Ramps_cxc_xIxJgg/amp2102,jamp2102
      Double Precision amp2103(maxamps), jamp2103(0:maxflow)
      common/to_Ramps_cxsx_xIxJcxsx/amp2103,jamp2103
      Double Precision amp2104(maxamps), jamp2104(0:maxflow)
      common/to_Ramps_cxs_xIxJdux/amp2104,jamp2104
      Double Precision amp2105(maxamps), jamp2105(0:maxflow)
      common/to_Ramps_cxs_xIxJcxs/amp2105,jamp2105
      Double Precision amp2106(maxamps), jamp2106(0:maxflow)
      common/to_Ramps_cxs_xIxJbtx/amp2106,jamp2106
      Double Precision amp2107(maxamps), jamp2107(0:maxflow)
      common/to_Ramps_cxbx_xIxJcxbx/amp2107,jamp2107
      Double Precision amp2108(maxamps), jamp2108(0:maxflow)
      common/to_Ramps_cxbx_xIxJsxtx/amp2108,jamp2108
      Double Precision amp2109(maxamps), jamp2109(0:maxflow)
      common/to_Ramps_cxb_xIxJcxb/amp2109,jamp2109
      Double Precision amp2110(maxamps), jamp2110(0:maxflow)
      common/to_Ramps_cxg_xIxJcxg/amp2110,jamp2110
      Double Precision amp2111(maxamps), jamp2111(0:maxflow)
      common/to_Ramps_cdx_xIxJdxc/amp2111,jamp2111
      Double Precision amp2112(maxamps), jamp2112(0:maxflow)
      common/to_Ramps_cd_xIxJdc/amp2112,jamp2112
      Double Precision amp2113(maxamps), jamp2113(0:maxflow)
      common/to_Ramps_cd_xIxJus/amp2113,jamp2113
      Double Precision amp2114(maxamps), jamp2114(0:maxflow)
      common/to_Ramps_cux_xIxJdxs/amp2114,jamp2114
      Double Precision amp2115(maxamps), jamp2115(0:maxflow)
      common/to_Ramps_cux_xIxJuxc/amp2115,jamp2115
      Double Precision amp2116(maxamps), jamp2116(0:maxflow)
      common/to_Ramps_cu_xIxJuc/amp2116,jamp2116
      Double Precision amp2117(maxamps), jamp2117(0:maxflow)
      common/to_Ramps_ccx_xIxJddx/amp2117,jamp2117
      Double Precision amp2118(maxamps), jamp2118(0:maxflow)
      common/to_Ramps_ccx_xIxJuux/amp2118,jamp2118
      Double Precision amp2119(maxamps), jamp2119(0:maxflow)
      common/to_Ramps_ccx_xIxJccx/amp2119,jamp2119
      Double Precision amp2120(maxamps), jamp2120(0:maxflow)
      common/to_Ramps_ccx_xIxJssx/amp2120,jamp2120
      Double Precision amp2121(maxamps), jamp2121(0:maxflow)
      common/to_Ramps_ccx_xIxJbbx/amp2121,jamp2121
      Double Precision amp2122(maxamps), jamp2122(0:maxflow)
      common/to_Ramps_ccx_xIxJttx/amp2122,jamp2122
      Double Precision amp2123(maxamps), jamp2123(0:maxflow)
      common/to_Ramps_ccx_xIxJgg/amp2123,jamp2123
      Double Precision amp2124(maxamps), jamp2124(0:maxflow)
      common/to_Ramps_cc_xIxJcc/amp2124,jamp2124
      Double Precision amp2125(maxamps), jamp2125(0:maxflow)
      common/to_Ramps_csx_xIxJdxu/amp2125,jamp2125
      Double Precision amp2126(maxamps), jamp2126(0:maxflow)
      common/to_Ramps_csx_xIxJcsx/amp2126,jamp2126
      Double Precision amp2127(maxamps), jamp2127(0:maxflow)
      common/to_Ramps_csx_xIxJbxt/amp2127,jamp2127
      Double Precision amp2128(maxamps), jamp2128(0:maxflow)
      common/to_Ramps_cs_xIxJcs/amp2128,jamp2128
      Double Precision amp2129(maxamps), jamp2129(0:maxflow)
      common/to_Ramps_cbx_xIxJcbx/amp2129,jamp2129
      Double Precision amp2130(maxamps), jamp2130(0:maxflow)
      common/to_Ramps_cb_xIxJcb/amp2130,jamp2130
      Double Precision amp2131(maxamps), jamp2131(0:maxflow)
      common/to_Ramps_cb_xIxJst/amp2131,jamp2131
      Double Precision amp2132(maxamps), jamp2132(0:maxflow)
      common/to_Ramps_cg_xIxJcg/amp2132,jamp2132
      Double Precision amp2133(maxamps), jamp2133(0:maxflow)
      common/to_Ramps_sxdx_xIxJdxsx/amp2133,jamp2133
      Double Precision amp2134(maxamps), jamp2134(0:maxflow)
      common/to_Ramps_sxd_xIxJdsx/amp2134,jamp2134
      Double Precision amp2135(maxamps), jamp2135(0:maxflow)
      common/to_Ramps_sxd_xIxJucx/amp2135,jamp2135
      Double Precision amp2136(maxamps), jamp2136(0:maxflow)
      common/to_Ramps_sxux_xIxJdxcx/amp2136,jamp2136
      Double Precision amp2137(maxamps), jamp2137(0:maxflow)
      common/to_Ramps_sxux_xIxJuxsx/amp2137,jamp2137
      Double Precision amp2138(maxamps), jamp2138(0:maxflow)
      common/to_Ramps_sxu_xIxJusx/amp2138,jamp2138
      Double Precision amp2139(maxamps), jamp2139(0:maxflow)
      common/to_Ramps_sxcx_xIxJcxsx/amp2139,jamp2139
      Double Precision amp2140(maxamps), jamp2140(0:maxflow)
      common/to_Ramps_sxc_xIxJdxu/amp2140,jamp2140
      Double Precision amp2141(maxamps), jamp2141(0:maxflow)
      common/to_Ramps_sxc_xIxJcsx/amp2141,jamp2141
      Double Precision amp2142(maxamps), jamp2142(0:maxflow)
      common/to_Ramps_sxc_xIxJbxt/amp2142,jamp2142
      Double Precision amp2143(maxamps), jamp2143(0:maxflow)
      common/to_Ramps_sxsx_xIxJsxsx/amp2143,jamp2143
      Double Precision amp2144(maxamps), jamp2144(0:maxflow)
      common/to_Ramps_sxs_xIxJddx/amp2144,jamp2144
      Double Precision amp2145(maxamps), jamp2145(0:maxflow)
      common/to_Ramps_sxs_xIxJuux/amp2145,jamp2145
      Double Precision amp2146(maxamps), jamp2146(0:maxflow)
      common/to_Ramps_sxs_xIxJccx/amp2146,jamp2146
      Double Precision amp2147(maxamps), jamp2147(0:maxflow)
      common/to_Ramps_sxs_xIxJssx/amp2147,jamp2147
      Double Precision amp2148(maxamps), jamp2148(0:maxflow)
      common/to_Ramps_sxs_xIxJbbx/amp2148,jamp2148
      Double Precision amp2149(maxamps), jamp2149(0:maxflow)
      common/to_Ramps_sxs_xIxJttx/amp2149,jamp2149
      Double Precision amp2150(maxamps), jamp2150(0:maxflow)
      common/to_Ramps_sxs_xIxJgg/amp2150,jamp2150
      Double Precision amp2151(maxamps), jamp2151(0:maxflow)
      common/to_Ramps_sxbx_xIxJsxbx/amp2151,jamp2151
      Double Precision amp2152(maxamps), jamp2152(0:maxflow)
      common/to_Ramps_sxb_xIxJcxt/amp2152,jamp2152
      Double Precision amp2153(maxamps), jamp2153(0:maxflow)
      common/to_Ramps_sxb_xIxJsxb/amp2153,jamp2153
      Double Precision amp2154(maxamps), jamp2154(0:maxflow)
      common/to_Ramps_sxg_xIxJsxg/amp2154,jamp2154
      Double Precision amp2155(maxamps), jamp2155(0:maxflow)
      common/to_Ramps_sdx_xIxJdxs/amp2155,jamp2155
      Double Precision amp2156(maxamps), jamp2156(0:maxflow)
      common/to_Ramps_sdx_xIxJuxc/amp2156,jamp2156
      Double Precision amp2157(maxamps), jamp2157(0:maxflow)
      common/to_Ramps_sd_xIxJds/amp2157,jamp2157
      Double Precision amp2158(maxamps), jamp2158(0:maxflow)
      common/to_Ramps_sux_xIxJuxs/amp2158,jamp2158
      Double Precision amp2159(maxamps), jamp2159(0:maxflow)
      common/to_Ramps_su_xIxJdc/amp2159,jamp2159
      Double Precision amp2160(maxamps), jamp2160(0:maxflow)
      common/to_Ramps_su_xIxJus/amp2160,jamp2160
      Double Precision amp2161(maxamps), jamp2161(0:maxflow)
      common/to_Ramps_scx_xIxJdux/amp2161,jamp2161
      Double Precision amp2162(maxamps), jamp2162(0:maxflow)
      common/to_Ramps_scx_xIxJcxs/amp2162,jamp2162
      Double Precision amp2163(maxamps), jamp2163(0:maxflow)
      common/to_Ramps_scx_xIxJbtx/amp2163,jamp2163
      Double Precision amp2164(maxamps), jamp2164(0:maxflow)
      common/to_Ramps_sc_xIxJcs/amp2164,jamp2164
      Double Precision amp2165(maxamps), jamp2165(0:maxflow)
      common/to_Ramps_ssx_xIxJddx/amp2165,jamp2165
      Double Precision amp2166(maxamps), jamp2166(0:maxflow)
      common/to_Ramps_ssx_xIxJuux/amp2166,jamp2166
      Double Precision amp2167(maxamps), jamp2167(0:maxflow)
      common/to_Ramps_ssx_xIxJccx/amp2167,jamp2167
      Double Precision amp2168(maxamps), jamp2168(0:maxflow)
      common/to_Ramps_ssx_xIxJssx/amp2168,jamp2168
      Double Precision amp2169(maxamps), jamp2169(0:maxflow)
      common/to_Ramps_ssx_xIxJbbx/amp2169,jamp2169
      Double Precision amp2170(maxamps), jamp2170(0:maxflow)
      common/to_Ramps_ssx_xIxJttx/amp2170,jamp2170
      Double Precision amp2171(maxamps), jamp2171(0:maxflow)
      common/to_Ramps_ssx_xIxJgg/amp2171,jamp2171
      Double Precision amp2172(maxamps), jamp2172(0:maxflow)
      common/to_Ramps_ss_xIxJss/amp2172,jamp2172
      Double Precision amp2173(maxamps), jamp2173(0:maxflow)
      common/to_Ramps_sbx_xIxJctx/amp2173,jamp2173
      Double Precision amp2174(maxamps), jamp2174(0:maxflow)
      common/to_Ramps_sbx_xIxJsbx/amp2174,jamp2174
      Double Precision amp2175(maxamps), jamp2175(0:maxflow)
      common/to_Ramps_sb_xIxJsb/amp2175,jamp2175
      Double Precision amp2176(maxamps), jamp2176(0:maxflow)
      common/to_Ramps_sg_xIxJsg/amp2176,jamp2176
      Double Precision amp2177(maxamps), jamp2177(0:maxflow)
      common/to_Ramps_bxdx_xIxJdxbx/amp2177,jamp2177
      Double Precision amp2178(maxamps), jamp2178(0:maxflow)
      common/to_Ramps_bxd_xIxJdbx/amp2178,jamp2178
      Double Precision amp2179(maxamps), jamp2179(0:maxflow)
      common/to_Ramps_bxd_xIxJutx/amp2179,jamp2179
      Double Precision amp2180(maxamps), jamp2180(0:maxflow)
      common/to_Ramps_bxux_xIxJdxtx/amp2180,jamp2180
      Double Precision amp2181(maxamps), jamp2181(0:maxflow)
      common/to_Ramps_bxux_xIxJuxbx/amp2181,jamp2181
      Double Precision amp2182(maxamps), jamp2182(0:maxflow)
      common/to_Ramps_bxu_xIxJubx/amp2182,jamp2182
      Double Precision amp2183(maxamps), jamp2183(0:maxflow)
      common/to_Ramps_bxcx_xIxJcxbx/amp2183,jamp2183
      Double Precision amp2184(maxamps), jamp2184(0:maxflow)
      common/to_Ramps_bxcx_xIxJsxtx/amp2184,jamp2184
      Double Precision amp2185(maxamps), jamp2185(0:maxflow)
      common/to_Ramps_bxc_xIxJcbx/amp2185,jamp2185
      Double Precision amp2186(maxamps), jamp2186(0:maxflow)
      common/to_Ramps_bxsx_xIxJsxbx/amp2186,jamp2186
      Double Precision amp2187(maxamps), jamp2187(0:maxflow)
      common/to_Ramps_bxs_xIxJctx/amp2187,jamp2187
      Double Precision amp2188(maxamps), jamp2188(0:maxflow)
      common/to_Ramps_bxs_xIxJsbx/amp2188,jamp2188
      Double Precision amp2189(maxamps), jamp2189(0:maxflow)
      common/to_Ramps_bxbx_xIxJbxbx/amp2189,jamp2189
      Double Precision amp2190(maxamps), jamp2190(0:maxflow)
      common/to_Ramps_bxb_xIxJddx/amp2190,jamp2190
      Double Precision amp2191(maxamps), jamp2191(0:maxflow)
      common/to_Ramps_bxb_xIxJuux/amp2191,jamp2191
      Double Precision amp2192(maxamps), jamp2192(0:maxflow)
      common/to_Ramps_bxb_xIxJccx/amp2192,jamp2192
      Double Precision amp2193(maxamps), jamp2193(0:maxflow)
      common/to_Ramps_bxb_xIxJssx/amp2193,jamp2193
      Double Precision amp2194(maxamps), jamp2194(0:maxflow)
      common/to_Ramps_bxb_xIxJbbx/amp2194,jamp2194
      Double Precision amp2195(maxamps), jamp2195(0:maxflow)
      common/to_Ramps_bxb_xIxJttx/amp2195,jamp2195
      Double Precision amp2196(maxamps), jamp2196(0:maxflow)
      common/to_Ramps_bxb_xIxJgg/amp2196,jamp2196
      Double Precision amp2197(maxamps), jamp2197(0:maxflow)
      common/to_Ramps_bxg_xIxJbxg/amp2197,jamp2197
      Double Precision amp2198(maxamps), jamp2198(0:maxflow)
      common/to_Ramps_bdx_xIxJdxb/amp2198,jamp2198
      Double Precision amp2199(maxamps), jamp2199(0:maxflow)
      common/to_Ramps_bdx_xIxJuxt/amp2199,jamp2199
      Double Precision amp2200(maxamps), jamp2200(0:maxflow)
      common/to_Ramps_bd_xIxJdb/amp2200,jamp2200
      Double Precision amp2201(maxamps), jamp2201(0:maxflow)
      common/to_Ramps_bux_xIxJuxb/amp2201,jamp2201
      Double Precision amp2202(maxamps), jamp2202(0:maxflow)
      common/to_Ramps_bu_xIxJdt/amp2202,jamp2202
      Double Precision amp2203(maxamps), jamp2203(0:maxflow)
      common/to_Ramps_bu_xIxJub/amp2203,jamp2203
      Double Precision amp2204(maxamps), jamp2204(0:maxflow)
      common/to_Ramps_bcx_xIxJcxb/amp2204,jamp2204
      Double Precision amp2205(maxamps), jamp2205(0:maxflow)
      common/to_Ramps_bc_xIxJcb/amp2205,jamp2205
      Double Precision amp2206(maxamps), jamp2206(0:maxflow)
      common/to_Ramps_bc_xIxJst/amp2206,jamp2206
      Double Precision amp2207(maxamps), jamp2207(0:maxflow)
      common/to_Ramps_bsx_xIxJcxt/amp2207,jamp2207
      Double Precision amp2208(maxamps), jamp2208(0:maxflow)
      common/to_Ramps_bsx_xIxJsxb/amp2208,jamp2208
      Double Precision amp2209(maxamps), jamp2209(0:maxflow)
      common/to_Ramps_bs_xIxJsb/amp2209,jamp2209
      Double Precision amp2210(maxamps), jamp2210(0:maxflow)
      common/to_Ramps_bbx_xIxJddx/amp2210,jamp2210
      Double Precision amp2211(maxamps), jamp2211(0:maxflow)
      common/to_Ramps_bbx_xIxJuux/amp2211,jamp2211
      Double Precision amp2212(maxamps), jamp2212(0:maxflow)
      common/to_Ramps_bbx_xIxJccx/amp2212,jamp2212
      Double Precision amp2213(maxamps), jamp2213(0:maxflow)
      common/to_Ramps_bbx_xIxJssx/amp2213,jamp2213
      Double Precision amp2214(maxamps), jamp2214(0:maxflow)
      common/to_Ramps_bbx_xIxJbbx/amp2214,jamp2214
      Double Precision amp2215(maxamps), jamp2215(0:maxflow)
      common/to_Ramps_bbx_xIxJttx/amp2215,jamp2215
      Double Precision amp2216(maxamps), jamp2216(0:maxflow)
      common/to_Ramps_bbx_xIxJgg/amp2216,jamp2216
      Double Precision amp2217(maxamps), jamp2217(0:maxflow)
      common/to_Ramps_bb_xIxJbb/amp2217,jamp2217
      Double Precision amp2218(maxamps), jamp2218(0:maxflow)
      common/to_Ramps_bg_xIxJbg/amp2218,jamp2218
      Double Precision amp2219(maxamps), jamp2219(0:maxflow)
      common/to_Ramps_gdx_xIxJdxg/amp2219,jamp2219
      Double Precision amp2220(maxamps), jamp2220(0:maxflow)
      common/to_Ramps_gd_xIxJdg/amp2220,jamp2220
      Double Precision amp2221(maxamps), jamp2221(0:maxflow)
      common/to_Ramps_gux_xIxJuxg/amp2221,jamp2221
      Double Precision amp2222(maxamps), jamp2222(0:maxflow)
      common/to_Ramps_gu_xIxJug/amp2222,jamp2222
      Double Precision amp2223(maxamps), jamp2223(0:maxflow)
      common/to_Ramps_gcx_xIxJcxg/amp2223,jamp2223
      Double Precision amp2224(maxamps), jamp2224(0:maxflow)
      common/to_Ramps_gc_xIxJcg/amp2224,jamp2224
      Double Precision amp2225(maxamps), jamp2225(0:maxflow)
      common/to_Ramps_gsx_xIxJsxg/amp2225,jamp2225
      Double Precision amp2226(maxamps), jamp2226(0:maxflow)
      common/to_Ramps_gs_xIxJsg/amp2226,jamp2226
      Double Precision amp2227(maxamps), jamp2227(0:maxflow)
      common/to_Ramps_gbx_xIxJbxg/amp2227,jamp2227
      Double Precision amp2228(maxamps), jamp2228(0:maxflow)
      common/to_Ramps_gb_xIxJbg/amp2228,jamp2228
      Double Precision amp2229(maxamps), jamp2229(0:maxflow)
      common/to_Ramps_gg_xIxJddx/amp2229,jamp2229
      Double Precision amp2230(maxamps), jamp2230(0:maxflow)
      common/to_Ramps_gg_xIxJuux/amp2230,jamp2230
      Double Precision amp2231(maxamps), jamp2231(0:maxflow)
      common/to_Ramps_gg_xIxJccx/amp2231,jamp2231
      Double Precision amp2232(maxamps), jamp2232(0:maxflow)
      common/to_Ramps_gg_xIxJssx/amp2232,jamp2232
      Double Precision amp2233(maxamps), jamp2233(0:maxflow)
      common/to_Ramps_gg_xIxJbbx/amp2233,jamp2233
      Double Precision amp2234(maxamps), jamp2234(0:maxflow)
      common/to_Ramps_gg_xIxJttx/amp2234,jamp2234
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
      
      if(str.eq."-1-1xIxJ-1-1") then
         include "leshouches_R_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ1-1") then
         include "leshouches_R_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ2-2") then
         include "leshouches_R_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ4-4") then
         include "leshouches_R_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ3-3") then
         include "leshouches_R_005.inc"
         iflow=nint(jamp2005(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2005(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ5-5") then
         include "leshouches_R_006.inc"
         iflow=nint(jamp2006(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2006(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ6-6") then
         include "leshouches_R_007.inc"
         iflow=nint(jamp2007(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2007(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxJ00") then
         include "leshouches_R_008.inc"
         iflow=nint(jamp2008(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2008(i)
         enddo
         goto 20
      elseif(str.eq."-1-2xIxJ-1-2") then
         include "leshouches_R_009.inc"
         iflow=nint(jamp2009(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2009(i)
         enddo
         goto 20
      elseif(str.eq."-12xIxJ-12") then
         include "leshouches_R_010.inc"
         iflow=nint(jamp2010(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2010(i)
         enddo
         goto 20
      elseif(str.eq."-12xIxJ4-3") then
         include "leshouches_R_011.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."-12xIxJ-56") then
         include "leshouches_R_012.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."-1-4xIxJ-1-4") then
         include "leshouches_R_013.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."-1-4xIxJ-2-3") then
         include "leshouches_R_014.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."-14xIxJ-14") then
         include "leshouches_R_015.inc"
         iflow=nint(jamp2015(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2015(i)
         enddo
         goto 20
      elseif(str.eq."-1-3xIxJ-1-3") then
         include "leshouches_R_016.inc"
         iflow=nint(jamp2016(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2016(i)
         enddo
         goto 20
      elseif(str.eq."-13xIxJ-13") then
         include "leshouches_R_017.inc"
         iflow=nint(jamp2017(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2017(i)
         enddo
         goto 20
      elseif(str.eq."-13xIxJ-24") then
         include "leshouches_R_018.inc"
         iflow=nint(jamp2018(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2018(i)
         enddo
         goto 20
      elseif(str.eq."-1-5xIxJ-1-5") then
         include "leshouches_R_019.inc"
         iflow=nint(jamp2019(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2019(i)
         enddo
         goto 20
      elseif(str.eq."-15xIxJ-15") then
         include "leshouches_R_020.inc"
         iflow=nint(jamp2020(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2020(i)
         enddo
         goto 20
      elseif(str.eq."-15xIxJ-26") then
         include "leshouches_R_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif(str.eq."-10xIxJ-10") then
         include "leshouches_R_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ1-1") then
         include "leshouches_R_023.inc"
         iflow=nint(jamp2023(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2023(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ2-2") then
         include "leshouches_R_024.inc"
         iflow=nint(jamp2024(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2024(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ4-4") then
         include "leshouches_R_025.inc"
         iflow=nint(jamp2025(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2025(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ3-3") then
         include "leshouches_R_026.inc"
         iflow=nint(jamp2026(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2026(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ5-5") then
         include "leshouches_R_027.inc"
         iflow=nint(jamp2027(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2027(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ6-6") then
         include "leshouches_R_028.inc"
         iflow=nint(jamp2028(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2028(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxJ00") then
         include "leshouches_R_029.inc"
         iflow=nint(jamp2029(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2029(i)
         enddo
         goto 20
      elseif(str.eq."11xIxJ11") then
         include "leshouches_R_030.inc"
         iflow=nint(jamp2030(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2030(i)
         enddo
         goto 20
      elseif(str.eq."1-2xIxJ1-2") then
         include "leshouches_R_031.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif(str.eq."1-2xIxJ-43") then
         include "leshouches_R_032.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
         enddo
         goto 20
      elseif(str.eq."1-2xIxJ5-6") then
         include "leshouches_R_033.inc"
         iflow=nint(jamp2033(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2033(i)
         enddo
         goto 20
      elseif(str.eq."12xIxJ12") then
         include "leshouches_R_034.inc"
         iflow=nint(jamp2034(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2034(i)
         enddo
         goto 20
      elseif(str.eq."1-4xIxJ1-4") then
         include "leshouches_R_035.inc"
         iflow=nint(jamp2035(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2035(i)
         enddo
         goto 20
      elseif(str.eq."14xIxJ14") then
         include "leshouches_R_036.inc"
         iflow=nint(jamp2036(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2036(i)
         enddo
         goto 20
      elseif(str.eq."14xIxJ23") then
         include "leshouches_R_037.inc"
         iflow=nint(jamp2037(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2037(i)
         enddo
         goto 20
      elseif(str.eq."1-3xIxJ1-3") then
         include "leshouches_R_038.inc"
         iflow=nint(jamp2038(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2038(i)
         enddo
         goto 20
      elseif(str.eq."1-3xIxJ2-4") then
         include "leshouches_R_039.inc"
         iflow=nint(jamp2039(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2039(i)
         enddo
         goto 20
      elseif(str.eq."13xIxJ13") then
         include "leshouches_R_040.inc"
         iflow=nint(jamp2040(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2040(i)
         enddo
         goto 20
      elseif(str.eq."1-5xIxJ1-5") then
         include "leshouches_R_041.inc"
         iflow=nint(jamp2041(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2041(i)
         enddo
         goto 20
      elseif(str.eq."1-5xIxJ2-6") then
         include "leshouches_R_042.inc"
         iflow=nint(jamp2042(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2042(i)
         enddo
         goto 20
      elseif(str.eq."15xIxJ15") then
         include "leshouches_R_043.inc"
         iflow=nint(jamp2043(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2043(i)
         enddo
         goto 20
      elseif(str.eq."10xIxJ10") then
         include "leshouches_R_044.inc"
         iflow=nint(jamp2044(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2044(i)
         enddo
         goto 20
      elseif(str.eq."-2-1xIxJ-1-2") then
         include "leshouches_R_045.inc"
         iflow=nint(jamp2045(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2045(i)
         enddo
         goto 20
      elseif(str.eq."-21xIxJ1-2") then
         include "leshouches_R_046.inc"
         iflow=nint(jamp2046(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2046(i)
         enddo
         goto 20
      elseif(str.eq."-21xIxJ-43") then
         include "leshouches_R_047.inc"
         iflow=nint(jamp2047(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2047(i)
         enddo
         goto 20
      elseif(str.eq."-21xIxJ5-6") then
         include "leshouches_R_048.inc"
         iflow=nint(jamp2048(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2048(i)
         enddo
         goto 20
      elseif(str.eq."-2-2xIxJ-2-2") then
         include "leshouches_R_049.inc"
         iflow=nint(jamp2049(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2049(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ1-1") then
         include "leshouches_R_050.inc"
         iflow=nint(jamp2050(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2050(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ2-2") then
         include "leshouches_R_051.inc"
         iflow=nint(jamp2051(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2051(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ4-4") then
         include "leshouches_R_052.inc"
         iflow=nint(jamp2052(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2052(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ3-3") then
         include "leshouches_R_053.inc"
         iflow=nint(jamp2053(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2053(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ5-5") then
         include "leshouches_R_054.inc"
         iflow=nint(jamp2054(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2054(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ6-6") then
         include "leshouches_R_055.inc"
         iflow=nint(jamp2055(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2055(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxJ00") then
         include "leshouches_R_056.inc"
         iflow=nint(jamp2056(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2056(i)
         enddo
         goto 20
      elseif(str.eq."-2-4xIxJ-2-4") then
         include "leshouches_R_057.inc"
         iflow=nint(jamp2057(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2057(i)
         enddo
         goto 20
      elseif(str.eq."-24xIxJ-13") then
         include "leshouches_R_058.inc"
         iflow=nint(jamp2058(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2058(i)
         enddo
         goto 20
      elseif(str.eq."-24xIxJ-24") then
         include "leshouches_R_059.inc"
         iflow=nint(jamp2059(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2059(i)
         enddo
         goto 20
      elseif(str.eq."-2-3xIxJ-1-4") then
         include "leshouches_R_060.inc"
         iflow=nint(jamp2060(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2060(i)
         enddo
         goto 20
      elseif(str.eq."-2-3xIxJ-2-3") then
         include "leshouches_R_061.inc"
         iflow=nint(jamp2061(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2061(i)
         enddo
         goto 20
      elseif(str.eq."-23xIxJ-23") then
         include "leshouches_R_062.inc"
         iflow=nint(jamp2062(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2062(i)
         enddo
         goto 20
      elseif(str.eq."-2-5xIxJ-1-6") then
         include "leshouches_R_063.inc"
         iflow=nint(jamp2063(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2063(i)
         enddo
         goto 20
      elseif(str.eq."-2-5xIxJ-2-5") then
         include "leshouches_R_064.inc"
         iflow=nint(jamp2064(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2064(i)
         enddo
         goto 20
      elseif(str.eq."-25xIxJ-25") then
         include "leshouches_R_065.inc"
         iflow=nint(jamp2065(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2065(i)
         enddo
         goto 20
      elseif(str.eq."-20xIxJ-20") then
         include "leshouches_R_066.inc"
         iflow=nint(jamp2066(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2066(i)
         enddo
         goto 20
      elseif(str.eq."2-1xIxJ-12") then
         include "leshouches_R_067.inc"
         iflow=nint(jamp2067(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2067(i)
         enddo
         goto 20
      elseif(str.eq."2-1xIxJ4-3") then
         include "leshouches_R_068.inc"
         iflow=nint(jamp2068(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2068(i)
         enddo
         goto 20
      elseif(str.eq."2-1xIxJ-56") then
         include "leshouches_R_069.inc"
         iflow=nint(jamp2069(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2069(i)
         enddo
         goto 20
      elseif(str.eq."21xIxJ12") then
         include "leshouches_R_070.inc"
         iflow=nint(jamp2070(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2070(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ1-1") then
         include "leshouches_R_071.inc"
         iflow=nint(jamp2071(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2071(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ2-2") then
         include "leshouches_R_072.inc"
         iflow=nint(jamp2072(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2072(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ4-4") then
         include "leshouches_R_073.inc"
         iflow=nint(jamp2073(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2073(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ3-3") then
         include "leshouches_R_074.inc"
         iflow=nint(jamp2074(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2074(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ5-5") then
         include "leshouches_R_075.inc"
         iflow=nint(jamp2075(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2075(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ6-6") then
         include "leshouches_R_076.inc"
         iflow=nint(jamp2076(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2076(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxJ00") then
         include "leshouches_R_077.inc"
         iflow=nint(jamp2077(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2077(i)
         enddo
         goto 20
      elseif(str.eq."22xIxJ22") then
         include "leshouches_R_078.inc"
         iflow=nint(jamp2078(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2078(i)
         enddo
         goto 20
      elseif(str.eq."2-4xIxJ1-3") then
         include "leshouches_R_079.inc"
         iflow=nint(jamp2079(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2079(i)
         enddo
         goto 20
      elseif(str.eq."2-4xIxJ2-4") then
         include "leshouches_R_080.inc"
         iflow=nint(jamp2080(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2080(i)
         enddo
         goto 20
      elseif(str.eq."24xIxJ24") then
         include "leshouches_R_081.inc"
         iflow=nint(jamp2081(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2081(i)
         enddo
         goto 20
      elseif(str.eq."2-3xIxJ2-3") then
         include "leshouches_R_082.inc"
         iflow=nint(jamp2082(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2082(i)
         enddo
         goto 20
      elseif(str.eq."23xIxJ14") then
         include "leshouches_R_083.inc"
         iflow=nint(jamp2083(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2083(i)
         enddo
         goto 20
      elseif(str.eq."23xIxJ23") then
         include "leshouches_R_084.inc"
         iflow=nint(jamp2084(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2084(i)
         enddo
         goto 20
      elseif(str.eq."2-5xIxJ2-5") then
         include "leshouches_R_085.inc"
         iflow=nint(jamp2085(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2085(i)
         enddo
         goto 20
      elseif(str.eq."25xIxJ16") then
         include "leshouches_R_086.inc"
         iflow=nint(jamp2086(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2086(i)
         enddo
         goto 20
      elseif(str.eq."25xIxJ25") then
         include "leshouches_R_087.inc"
         iflow=nint(jamp2087(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2087(i)
         enddo
         goto 20
      elseif(str.eq."20xIxJ20") then
         include "leshouches_R_088.inc"
         iflow=nint(jamp2088(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2088(i)
         enddo
         goto 20
      elseif(str.eq."-4-1xIxJ-1-4") then
         include "leshouches_R_089.inc"
         iflow=nint(jamp2089(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2089(i)
         enddo
         goto 20
      elseif(str.eq."-4-1xIxJ-2-3") then
         include "leshouches_R_090.inc"
         iflow=nint(jamp2090(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2090(i)
         enddo
         goto 20
      elseif(str.eq."-41xIxJ1-4") then
         include "leshouches_R_091.inc"
         iflow=nint(jamp2091(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2091(i)
         enddo
         goto 20
      elseif(str.eq."-4-2xIxJ-2-4") then
         include "leshouches_R_092.inc"
         iflow=nint(jamp2092(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2092(i)
         enddo
         goto 20
      elseif(str.eq."-42xIxJ1-3") then
         include "leshouches_R_093.inc"
         iflow=nint(jamp2093(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2093(i)
         enddo
         goto 20
      elseif(str.eq."-42xIxJ2-4") then
         include "leshouches_R_094.inc"
         iflow=nint(jamp2094(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2094(i)
         enddo
         goto 20
      elseif(str.eq."-4-4xIxJ-4-4") then
         include "leshouches_R_095.inc"
         iflow=nint(jamp2095(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2095(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ1-1") then
         include "leshouches_R_096.inc"
         iflow=nint(jamp2096(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2096(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ2-2") then
         include "leshouches_R_097.inc"
         iflow=nint(jamp2097(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2097(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ4-4") then
         include "leshouches_R_098.inc"
         iflow=nint(jamp2098(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2098(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ3-3") then
         include "leshouches_R_099.inc"
         iflow=nint(jamp2099(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2099(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ5-5") then
         include "leshouches_R_100.inc"
         iflow=nint(jamp2100(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2100(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ6-6") then
         include "leshouches_R_101.inc"
         iflow=nint(jamp2101(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2101(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxJ00") then
         include "leshouches_R_102.inc"
         iflow=nint(jamp2102(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2102(i)
         enddo
         goto 20
      elseif(str.eq."-4-3xIxJ-4-3") then
         include "leshouches_R_103.inc"
         iflow=nint(jamp2103(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2103(i)
         enddo
         goto 20
      elseif(str.eq."-43xIxJ1-2") then
         include "leshouches_R_104.inc"
         iflow=nint(jamp2104(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2104(i)
         enddo
         goto 20
      elseif(str.eq."-43xIxJ-43") then
         include "leshouches_R_105.inc"
         iflow=nint(jamp2105(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2105(i)
         enddo
         goto 20
      elseif(str.eq."-43xIxJ5-6") then
         include "leshouches_R_106.inc"
         iflow=nint(jamp2106(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2106(i)
         enddo
         goto 20
      elseif(str.eq."-4-5xIxJ-4-5") then
         include "leshouches_R_107.inc"
         iflow=nint(jamp2107(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2107(i)
         enddo
         goto 20
      elseif(str.eq."-4-5xIxJ-3-6") then
         include "leshouches_R_108.inc"
         iflow=nint(jamp2108(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2108(i)
         enddo
         goto 20
      elseif(str.eq."-45xIxJ-45") then
         include "leshouches_R_109.inc"
         iflow=nint(jamp2109(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2109(i)
         enddo
         goto 20
      elseif(str.eq."-40xIxJ-40") then
         include "leshouches_R_110.inc"
         iflow=nint(jamp2110(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2110(i)
         enddo
         goto 20
      elseif(str.eq."4-1xIxJ-14") then
         include "leshouches_R_111.inc"
         iflow=nint(jamp2111(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2111(i)
         enddo
         goto 20
      elseif(str.eq."41xIxJ14") then
         include "leshouches_R_112.inc"
         iflow=nint(jamp2112(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2112(i)
         enddo
         goto 20
      elseif(str.eq."41xIxJ23") then
         include "leshouches_R_113.inc"
         iflow=nint(jamp2113(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2113(i)
         enddo
         goto 20
      elseif(str.eq."4-2xIxJ-13") then
         include "leshouches_R_114.inc"
         iflow=nint(jamp2114(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2114(i)
         enddo
         goto 20
      elseif(str.eq."4-2xIxJ-24") then
         include "leshouches_R_115.inc"
         iflow=nint(jamp2115(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2115(i)
         enddo
         goto 20
      elseif(str.eq."42xIxJ24") then
         include "leshouches_R_116.inc"
         iflow=nint(jamp2116(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2116(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ1-1") then
         include "leshouches_R_117.inc"
         iflow=nint(jamp2117(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2117(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ2-2") then
         include "leshouches_R_118.inc"
         iflow=nint(jamp2118(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2118(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ4-4") then
         include "leshouches_R_119.inc"
         iflow=nint(jamp2119(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2119(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ3-3") then
         include "leshouches_R_120.inc"
         iflow=nint(jamp2120(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2120(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ5-5") then
         include "leshouches_R_121.inc"
         iflow=nint(jamp2121(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2121(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ6-6") then
         include "leshouches_R_122.inc"
         iflow=nint(jamp2122(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2122(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxJ00") then
         include "leshouches_R_123.inc"
         iflow=nint(jamp2123(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2123(i)
         enddo
         goto 20
      elseif(str.eq."44xIxJ44") then
         include "leshouches_R_124.inc"
         iflow=nint(jamp2124(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2124(i)
         enddo
         goto 20
      elseif(str.eq."4-3xIxJ-12") then
         include "leshouches_R_125.inc"
         iflow=nint(jamp2125(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2125(i)
         enddo
         goto 20
      elseif(str.eq."4-3xIxJ4-3") then
         include "leshouches_R_126.inc"
         iflow=nint(jamp2126(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2126(i)
         enddo
         goto 20
      elseif(str.eq."4-3xIxJ-56") then
         include "leshouches_R_127.inc"
         iflow=nint(jamp2127(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2127(i)
         enddo
         goto 20
      elseif(str.eq."43xIxJ43") then
         include "leshouches_R_128.inc"
         iflow=nint(jamp2128(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2128(i)
         enddo
         goto 20
      elseif(str.eq."4-5xIxJ4-5") then
         include "leshouches_R_129.inc"
         iflow=nint(jamp2129(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2129(i)
         enddo
         goto 20
      elseif(str.eq."45xIxJ45") then
         include "leshouches_R_130.inc"
         iflow=nint(jamp2130(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2130(i)
         enddo
         goto 20
      elseif(str.eq."45xIxJ36") then
         include "leshouches_R_131.inc"
         iflow=nint(jamp2131(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2131(i)
         enddo
         goto 20
      elseif(str.eq."40xIxJ40") then
         include "leshouches_R_132.inc"
         iflow=nint(jamp2132(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2132(i)
         enddo
         goto 20
      elseif(str.eq."-3-1xIxJ-1-3") then
         include "leshouches_R_133.inc"
         iflow=nint(jamp2133(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2133(i)
         enddo
         goto 20
      elseif(str.eq."-31xIxJ1-3") then
         include "leshouches_R_134.inc"
         iflow=nint(jamp2134(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2134(i)
         enddo
         goto 20
      elseif(str.eq."-31xIxJ2-4") then
         include "leshouches_R_135.inc"
         iflow=nint(jamp2135(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2135(i)
         enddo
         goto 20
      elseif(str.eq."-3-2xIxJ-1-4") then
         include "leshouches_R_136.inc"
         iflow=nint(jamp2136(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2136(i)
         enddo
         goto 20
      elseif(str.eq."-3-2xIxJ-2-3") then
         include "leshouches_R_137.inc"
         iflow=nint(jamp2137(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2137(i)
         enddo
         goto 20
      elseif(str.eq."-32xIxJ2-3") then
         include "leshouches_R_138.inc"
         iflow=nint(jamp2138(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2138(i)
         enddo
         goto 20
      elseif(str.eq."-3-4xIxJ-4-3") then
         include "leshouches_R_139.inc"
         iflow=nint(jamp2139(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2139(i)
         enddo
         goto 20
      elseif(str.eq."-34xIxJ-12") then
         include "leshouches_R_140.inc"
         iflow=nint(jamp2140(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2140(i)
         enddo
         goto 20
      elseif(str.eq."-34xIxJ4-3") then
         include "leshouches_R_141.inc"
         iflow=nint(jamp2141(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2141(i)
         enddo
         goto 20
      elseif(str.eq."-34xIxJ-56") then
         include "leshouches_R_142.inc"
         iflow=nint(jamp2142(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2142(i)
         enddo
         goto 20
      elseif(str.eq."-3-3xIxJ-3-3") then
         include "leshouches_R_143.inc"
         iflow=nint(jamp2143(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2143(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ1-1") then
         include "leshouches_R_144.inc"
         iflow=nint(jamp2144(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2144(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ2-2") then
         include "leshouches_R_145.inc"
         iflow=nint(jamp2145(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2145(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ4-4") then
         include "leshouches_R_146.inc"
         iflow=nint(jamp2146(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2146(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ3-3") then
         include "leshouches_R_147.inc"
         iflow=nint(jamp2147(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2147(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ5-5") then
         include "leshouches_R_148.inc"
         iflow=nint(jamp2148(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2148(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ6-6") then
         include "leshouches_R_149.inc"
         iflow=nint(jamp2149(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2149(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxJ00") then
         include "leshouches_R_150.inc"
         iflow=nint(jamp2150(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2150(i)
         enddo
         goto 20
      elseif(str.eq."-3-5xIxJ-3-5") then
         include "leshouches_R_151.inc"
         iflow=nint(jamp2151(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2151(i)
         enddo
         goto 20
      elseif(str.eq."-35xIxJ-46") then
         include "leshouches_R_152.inc"
         iflow=nint(jamp2152(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2152(i)
         enddo
         goto 20
      elseif(str.eq."-35xIxJ-35") then
         include "leshouches_R_153.inc"
         iflow=nint(jamp2153(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2153(i)
         enddo
         goto 20
      elseif(str.eq."-30xIxJ-30") then
         include "leshouches_R_154.inc"
         iflow=nint(jamp2154(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2154(i)
         enddo
         goto 20
      elseif(str.eq."3-1xIxJ-13") then
         include "leshouches_R_155.inc"
         iflow=nint(jamp2155(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2155(i)
         enddo
         goto 20
      elseif(str.eq."3-1xIxJ-24") then
         include "leshouches_R_156.inc"
         iflow=nint(jamp2156(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2156(i)
         enddo
         goto 20
      elseif(str.eq."31xIxJ13") then
         include "leshouches_R_157.inc"
         iflow=nint(jamp2157(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2157(i)
         enddo
         goto 20
      elseif(str.eq."3-2xIxJ-23") then
         include "leshouches_R_158.inc"
         iflow=nint(jamp2158(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2158(i)
         enddo
         goto 20
      elseif(str.eq."32xIxJ14") then
         include "leshouches_R_159.inc"
         iflow=nint(jamp2159(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2159(i)
         enddo
         goto 20
      elseif(str.eq."32xIxJ23") then
         include "leshouches_R_160.inc"
         iflow=nint(jamp2160(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2160(i)
         enddo
         goto 20
      elseif(str.eq."3-4xIxJ1-2") then
         include "leshouches_R_161.inc"
         iflow=nint(jamp2161(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2161(i)
         enddo
         goto 20
      elseif(str.eq."3-4xIxJ-43") then
         include "leshouches_R_162.inc"
         iflow=nint(jamp2162(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2162(i)
         enddo
         goto 20
      elseif(str.eq."3-4xIxJ5-6") then
         include "leshouches_R_163.inc"
         iflow=nint(jamp2163(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2163(i)
         enddo
         goto 20
      elseif(str.eq."34xIxJ43") then
         include "leshouches_R_164.inc"
         iflow=nint(jamp2164(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2164(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ1-1") then
         include "leshouches_R_165.inc"
         iflow=nint(jamp2165(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2165(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ2-2") then
         include "leshouches_R_166.inc"
         iflow=nint(jamp2166(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2166(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ4-4") then
         include "leshouches_R_167.inc"
         iflow=nint(jamp2167(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2167(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ3-3") then
         include "leshouches_R_168.inc"
         iflow=nint(jamp2168(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2168(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ5-5") then
         include "leshouches_R_169.inc"
         iflow=nint(jamp2169(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2169(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ6-6") then
         include "leshouches_R_170.inc"
         iflow=nint(jamp2170(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2170(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxJ00") then
         include "leshouches_R_171.inc"
         iflow=nint(jamp2171(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2171(i)
         enddo
         goto 20
      elseif(str.eq."33xIxJ33") then
         include "leshouches_R_172.inc"
         iflow=nint(jamp2172(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2172(i)
         enddo
         goto 20
      elseif(str.eq."3-5xIxJ4-6") then
         include "leshouches_R_173.inc"
         iflow=nint(jamp2173(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2173(i)
         enddo
         goto 20
      elseif(str.eq."3-5xIxJ3-5") then
         include "leshouches_R_174.inc"
         iflow=nint(jamp2174(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2174(i)
         enddo
         goto 20
      elseif(str.eq."35xIxJ35") then
         include "leshouches_R_175.inc"
         iflow=nint(jamp2175(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2175(i)
         enddo
         goto 20
      elseif(str.eq."30xIxJ30") then
         include "leshouches_R_176.inc"
         iflow=nint(jamp2176(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2176(i)
         enddo
         goto 20
      elseif(str.eq."-5-1xIxJ-1-5") then
         include "leshouches_R_177.inc"
         iflow=nint(jamp2177(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2177(i)
         enddo
         goto 20
      elseif(str.eq."-51xIxJ1-5") then
         include "leshouches_R_178.inc"
         iflow=nint(jamp2178(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2178(i)
         enddo
         goto 20
      elseif(str.eq."-51xIxJ2-6") then
         include "leshouches_R_179.inc"
         iflow=nint(jamp2179(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2179(i)
         enddo
         goto 20
      elseif(str.eq."-5-2xIxJ-1-6") then
         include "leshouches_R_180.inc"
         iflow=nint(jamp2180(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2180(i)
         enddo
         goto 20
      elseif(str.eq."-5-2xIxJ-2-5") then
         include "leshouches_R_181.inc"
         iflow=nint(jamp2181(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2181(i)
         enddo
         goto 20
      elseif(str.eq."-52xIxJ2-5") then
         include "leshouches_R_182.inc"
         iflow=nint(jamp2182(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2182(i)
         enddo
         goto 20
      elseif(str.eq."-5-4xIxJ-4-5") then
         include "leshouches_R_183.inc"
         iflow=nint(jamp2183(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2183(i)
         enddo
         goto 20
      elseif(str.eq."-5-4xIxJ-3-6") then
         include "leshouches_R_184.inc"
         iflow=nint(jamp2184(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2184(i)
         enddo
         goto 20
      elseif(str.eq."-54xIxJ4-5") then
         include "leshouches_R_185.inc"
         iflow=nint(jamp2185(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2185(i)
         enddo
         goto 20
      elseif(str.eq."-5-3xIxJ-3-5") then
         include "leshouches_R_186.inc"
         iflow=nint(jamp2186(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2186(i)
         enddo
         goto 20
      elseif(str.eq."-53xIxJ4-6") then
         include "leshouches_R_187.inc"
         iflow=nint(jamp2187(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2187(i)
         enddo
         goto 20
      elseif(str.eq."-53xIxJ3-5") then
         include "leshouches_R_188.inc"
         iflow=nint(jamp2188(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2188(i)
         enddo
         goto 20
      elseif(str.eq."-5-5xIxJ-5-5") then
         include "leshouches_R_189.inc"
         iflow=nint(jamp2189(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2189(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ1-1") then
         include "leshouches_R_190.inc"
         iflow=nint(jamp2190(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2190(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ2-2") then
         include "leshouches_R_191.inc"
         iflow=nint(jamp2191(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2191(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ4-4") then
         include "leshouches_R_192.inc"
         iflow=nint(jamp2192(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2192(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ3-3") then
         include "leshouches_R_193.inc"
         iflow=nint(jamp2193(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2193(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ5-5") then
         include "leshouches_R_194.inc"
         iflow=nint(jamp2194(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2194(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ6-6") then
         include "leshouches_R_195.inc"
         iflow=nint(jamp2195(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2195(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxJ00") then
         include "leshouches_R_196.inc"
         iflow=nint(jamp2196(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2196(i)
         enddo
         goto 20
      elseif(str.eq."-50xIxJ-50") then
         include "leshouches_R_197.inc"
         iflow=nint(jamp2197(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2197(i)
         enddo
         goto 20
      elseif(str.eq."5-1xIxJ-15") then
         include "leshouches_R_198.inc"
         iflow=nint(jamp2198(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2198(i)
         enddo
         goto 20
      elseif(str.eq."5-1xIxJ-26") then
         include "leshouches_R_199.inc"
         iflow=nint(jamp2199(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2199(i)
         enddo
         goto 20
      elseif(str.eq."51xIxJ15") then
         include "leshouches_R_200.inc"
         iflow=nint(jamp2200(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2200(i)
         enddo
         goto 20
      elseif(str.eq."5-2xIxJ-25") then
         include "leshouches_R_201.inc"
         iflow=nint(jamp2201(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2201(i)
         enddo
         goto 20
      elseif(str.eq."52xIxJ16") then
         include "leshouches_R_202.inc"
         iflow=nint(jamp2202(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2202(i)
         enddo
         goto 20
      elseif(str.eq."52xIxJ25") then
         include "leshouches_R_203.inc"
         iflow=nint(jamp2203(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2203(i)
         enddo
         goto 20
      elseif(str.eq."5-4xIxJ-45") then
         include "leshouches_R_204.inc"
         iflow=nint(jamp2204(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2204(i)
         enddo
         goto 20
      elseif(str.eq."54xIxJ45") then
         include "leshouches_R_205.inc"
         iflow=nint(jamp2205(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2205(i)
         enddo
         goto 20
      elseif(str.eq."54xIxJ36") then
         include "leshouches_R_206.inc"
         iflow=nint(jamp2206(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2206(i)
         enddo
         goto 20
      elseif(str.eq."5-3xIxJ-46") then
         include "leshouches_R_207.inc"
         iflow=nint(jamp2207(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2207(i)
         enddo
         goto 20
      elseif(str.eq."5-3xIxJ-35") then
         include "leshouches_R_208.inc"
         iflow=nint(jamp2208(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2208(i)
         enddo
         goto 20
      elseif(str.eq."53xIxJ35") then
         include "leshouches_R_209.inc"
         iflow=nint(jamp2209(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2209(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ1-1") then
         include "leshouches_R_210.inc"
         iflow=nint(jamp2210(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2210(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ2-2") then
         include "leshouches_R_211.inc"
         iflow=nint(jamp2211(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2211(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ4-4") then
         include "leshouches_R_212.inc"
         iflow=nint(jamp2212(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2212(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ3-3") then
         include "leshouches_R_213.inc"
         iflow=nint(jamp2213(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2213(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ5-5") then
         include "leshouches_R_214.inc"
         iflow=nint(jamp2214(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2214(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ6-6") then
         include "leshouches_R_215.inc"
         iflow=nint(jamp2215(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2215(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxJ00") then
         include "leshouches_R_216.inc"
         iflow=nint(jamp2216(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2216(i)
         enddo
         goto 20
      elseif(str.eq."55xIxJ55") then
         include "leshouches_R_217.inc"
         iflow=nint(jamp2217(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2217(i)
         enddo
         goto 20
      elseif(str.eq."50xIxJ50") then
         include "leshouches_R_218.inc"
         iflow=nint(jamp2218(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2218(i)
         enddo
         goto 20
      elseif(str.eq."0-1xIxJ-10") then
         include "leshouches_R_219.inc"
         iflow=nint(jamp2219(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2219(i)
         enddo
         goto 20
      elseif(str.eq."01xIxJ10") then
         include "leshouches_R_220.inc"
         iflow=nint(jamp2220(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2220(i)
         enddo
         goto 20
      elseif(str.eq."0-2xIxJ-20") then
         include "leshouches_R_221.inc"
         iflow=nint(jamp2221(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2221(i)
         enddo
         goto 20
      elseif(str.eq."02xIxJ20") then
         include "leshouches_R_222.inc"
         iflow=nint(jamp2222(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2222(i)
         enddo
         goto 20
      elseif(str.eq."0-4xIxJ-40") then
         include "leshouches_R_223.inc"
         iflow=nint(jamp2223(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2223(i)
         enddo
         goto 20
      elseif(str.eq."04xIxJ40") then
         include "leshouches_R_224.inc"
         iflow=nint(jamp2224(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2224(i)
         enddo
         goto 20
      elseif(str.eq."0-3xIxJ-30") then
         include "leshouches_R_225.inc"
         iflow=nint(jamp2225(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2225(i)
         enddo
         goto 20
      elseif(str.eq."03xIxJ30") then
         include "leshouches_R_226.inc"
         iflow=nint(jamp2226(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2226(i)
         enddo
         goto 20
      elseif(str.eq."0-5xIxJ-50") then
         include "leshouches_R_227.inc"
         iflow=nint(jamp2227(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2227(i)
         enddo
         goto 20
      elseif(str.eq."05xIxJ50") then
         include "leshouches_R_228.inc"
         iflow=nint(jamp2228(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2228(i)
         enddo
         goto 20
      elseif(str.eq."00xIxJ1-1") then
         include "leshouches_R_229.inc"
         iflow=nint(jamp2229(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2229(i)
         enddo
         goto 20
      elseif(str.eq."00xIxJ2-2") then
         include "leshouches_R_230.inc"
         iflow=nint(jamp2230(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2230(i)
         enddo
         goto 20
      elseif(str.eq."00xIxJ4-4") then
         include "leshouches_R_231.inc"
         iflow=nint(jamp2231(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2231(i)
         enddo
         goto 20
      elseif(str.eq."00xIxJ3-3") then
         include "leshouches_R_232.inc"
         iflow=nint(jamp2232(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2232(i)
         enddo
         goto 20
      elseif(str.eq."00xIxJ5-5") then
         include "leshouches_R_233.inc"
         iflow=nint(jamp2233(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2233(i)
         enddo
         goto 20
      elseif(str.eq."00xIxJ6-6") then
         include "leshouches_R_234.inc"
         iflow=nint(jamp2234(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2234(i)
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
      
      
      
      
