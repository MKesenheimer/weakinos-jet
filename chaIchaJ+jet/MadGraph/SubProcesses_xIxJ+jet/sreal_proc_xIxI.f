      subroutine sreal_proc_xixi(p,legs,wgt)
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
      
      if(str.eq."-1-1xIxI-1-1") then
         call smatrix_dxdx_xIxIdxdx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI1-1") then
         call smatrix_dxd_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI2-2") then
         call smatrix_dxd_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI4-4") then
         call smatrix_dxd_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI3-3") then
         call smatrix_dxd_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI5-5") then
         call smatrix_dxd_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI6-6") then
         call smatrix_dxd_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."-11xIxI00") then
         call smatrix_dxd_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."-1-2xIxI-1-2") then
         call smatrix_dxux_xIxIdxux(p1,wgt)
         goto 20
      elseif(str.eq."-12xIxI-12") then
         call smatrix_dxu_xIxIdxu(p1,wgt)
         goto 20
      elseif(str.eq."-12xIxI4-3") then
         call smatrix_dxu_xIxIcsx(p1,wgt)
         goto 20
      elseif(str.eq."-12xIxI-56") then
         call smatrix_dxu_xIxIbxt(p1,wgt)
         goto 20
      elseif(str.eq."-1-4xIxI-1-4") then
         call smatrix_dxcx_xIxIdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-1-4xIxI-2-3") then
         call smatrix_dxcx_xIxIuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-14xIxI-14") then
         call smatrix_dxc_xIxIdxc(p1,wgt)
         goto 20
      elseif(str.eq."-1-3xIxI-1-3") then
         call smatrix_dxsx_xIxIdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-13xIxI-13") then
         call smatrix_dxs_xIxIdxs(p1,wgt)
         goto 20
      elseif(str.eq."-13xIxI-24") then
         call smatrix_dxs_xIxIuxc(p1,wgt)
         goto 20
      elseif(str.eq."-1-5xIxI-1-5") then
         call smatrix_dxbx_xIxIdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-15xIxI-15") then
         call smatrix_dxb_xIxIdxb(p1,wgt)
         goto 20
      elseif(str.eq."-15xIxI-26") then
         call smatrix_dxb_xIxIuxt(p1,wgt)
         goto 20
      elseif(str.eq."-10xIxI-10") then
         call smatrix_dxg_xIxIdxg(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI1-1") then
         call smatrix_ddx_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI2-2") then
         call smatrix_ddx_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI4-4") then
         call smatrix_ddx_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI3-3") then
         call smatrix_ddx_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI5-5") then
         call smatrix_ddx_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI6-6") then
         call smatrix_ddx_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."1-1xIxI00") then
         call smatrix_ddx_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."11xIxI11") then
         call smatrix_dd_xIxIdd(p1,wgt)
         goto 20
      elseif(str.eq."1-2xIxI1-2") then
         call smatrix_dux_xIxIdux(p1,wgt)
         goto 20
      elseif(str.eq."1-2xIxI-43") then
         call smatrix_dux_xIxIcxs(p1,wgt)
         goto 20
      elseif(str.eq."1-2xIxI5-6") then
         call smatrix_dux_xIxIbtx(p1,wgt)
         goto 20
      elseif(str.eq."12xIxI12") then
         call smatrix_du_xIxIdu(p1,wgt)
         goto 20
      elseif(str.eq."1-4xIxI1-4") then
         call smatrix_dcx_xIxIdcx(p1,wgt)
         goto 20
      elseif(str.eq."14xIxI14") then
         call smatrix_dc_xIxIdc(p1,wgt)
         goto 20
      elseif(str.eq."14xIxI23") then
         call smatrix_dc_xIxIus(p1,wgt)
         goto 20
      elseif(str.eq."1-3xIxI1-3") then
         call smatrix_dsx_xIxIdsx(p1,wgt)
         goto 20
      elseif(str.eq."1-3xIxI2-4") then
         call smatrix_dsx_xIxIucx(p1,wgt)
         goto 20
      elseif(str.eq."13xIxI13") then
         call smatrix_ds_xIxIds(p1,wgt)
         goto 20
      elseif(str.eq."1-5xIxI1-5") then
         call smatrix_dbx_xIxIdbx(p1,wgt)
         goto 20
      elseif(str.eq."1-5xIxI2-6") then
         call smatrix_dbx_xIxIutx(p1,wgt)
         goto 20
      elseif(str.eq."15xIxI15") then
         call smatrix_db_xIxIdb(p1,wgt)
         goto 20
      elseif(str.eq."10xIxI10") then
         call smatrix_dg_xIxIdg(p1,wgt)
         goto 20
      elseif(str.eq."-2-1xIxI-1-2") then
         call smatrix_uxdx_xIxIdxux(p1,wgt)
         goto 20
      elseif(str.eq."-21xIxI1-2") then
         call smatrix_uxd_xIxIdux(p1,wgt)
         goto 20
      elseif(str.eq."-21xIxI-43") then
         call smatrix_uxd_xIxIcxs(p1,wgt)
         goto 20
      elseif(str.eq."-21xIxI5-6") then
         call smatrix_uxd_xIxIbtx(p1,wgt)
         goto 20
      elseif(str.eq."-2-2xIxI-2-2") then
         call smatrix_uxux_xIxIuxux(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI1-1") then
         call smatrix_uxu_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI2-2") then
         call smatrix_uxu_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI4-4") then
         call smatrix_uxu_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI3-3") then
         call smatrix_uxu_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI5-5") then
         call smatrix_uxu_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI6-6") then
         call smatrix_uxu_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."-22xIxI00") then
         call smatrix_uxu_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."-2-4xIxI-2-4") then
         call smatrix_uxcx_xIxIuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-24xIxI-13") then
         call smatrix_uxc_xIxIdxs(p1,wgt)
         goto 20
      elseif(str.eq."-24xIxI-24") then
         call smatrix_uxc_xIxIuxc(p1,wgt)
         goto 20
      elseif(str.eq."-2-3xIxI-1-4") then
         call smatrix_uxsx_xIxIdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-2-3xIxI-2-3") then
         call smatrix_uxsx_xIxIuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-23xIxI-23") then
         call smatrix_uxs_xIxIuxs(p1,wgt)
         goto 20
      elseif(str.eq."-2-5xIxI-1-6") then
         call smatrix_uxbx_xIxIdxtx(p1,wgt)
         goto 20
      elseif(str.eq."-2-5xIxI-2-5") then
         call smatrix_uxbx_xIxIuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-25xIxI-25") then
         call smatrix_uxb_xIxIuxb(p1,wgt)
         goto 20
      elseif(str.eq."-20xIxI-20") then
         call smatrix_uxg_xIxIuxg(p1,wgt)
         goto 20
      elseif(str.eq."2-1xIxI-12") then
         call smatrix_udx_xIxIdxu(p1,wgt)
         goto 20
      elseif(str.eq."2-1xIxI4-3") then
         call smatrix_udx_xIxIcsx(p1,wgt)
         goto 20
      elseif(str.eq."2-1xIxI-56") then
         call smatrix_udx_xIxIbxt(p1,wgt)
         goto 20
      elseif(str.eq."21xIxI12") then
         call smatrix_ud_xIxIdu(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI1-1") then
         call smatrix_uux_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI2-2") then
         call smatrix_uux_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI4-4") then
         call smatrix_uux_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI3-3") then
         call smatrix_uux_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI5-5") then
         call smatrix_uux_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI6-6") then
         call smatrix_uux_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."2-2xIxI00") then
         call smatrix_uux_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."22xIxI22") then
         call smatrix_uu_xIxIuu(p1,wgt)
         goto 20
      elseif(str.eq."2-4xIxI1-3") then
         call smatrix_ucx_xIxIdsx(p1,wgt)
         goto 20
      elseif(str.eq."2-4xIxI2-4") then
         call smatrix_ucx_xIxIucx(p1,wgt)
         goto 20
      elseif(str.eq."24xIxI24") then
         call smatrix_uc_xIxIuc(p1,wgt)
         goto 20
      elseif(str.eq."2-3xIxI2-3") then
         call smatrix_usx_xIxIusx(p1,wgt)
         goto 20
      elseif(str.eq."23xIxI14") then
         call smatrix_us_xIxIdc(p1,wgt)
         goto 20
      elseif(str.eq."23xIxI23") then
         call smatrix_us_xIxIus(p1,wgt)
         goto 20
      elseif(str.eq."2-5xIxI2-5") then
         call smatrix_ubx_xIxIubx(p1,wgt)
         goto 20
      elseif(str.eq."25xIxI16") then
         call smatrix_ub_xIxIdt(p1,wgt)
         goto 20
      elseif(str.eq."25xIxI25") then
         call smatrix_ub_xIxIub(p1,wgt)
         goto 20
      elseif(str.eq."20xIxI20") then
         call smatrix_ug_xIxIug(p1,wgt)
         goto 20
      elseif(str.eq."-4-1xIxI-1-4") then
         call smatrix_cxdx_xIxIdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-1xIxI-2-3") then
         call smatrix_cxdx_xIxIuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-41xIxI1-4") then
         call smatrix_cxd_xIxIdcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-2xIxI-2-4") then
         call smatrix_cxux_xIxIuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-42xIxI1-3") then
         call smatrix_cxu_xIxIdsx(p1,wgt)
         goto 20
      elseif(str.eq."-42xIxI2-4") then
         call smatrix_cxu_xIxIucx(p1,wgt)
         goto 20
      elseif(str.eq."-4-4xIxI-4-4") then
         call smatrix_cxcx_xIxIcxcx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI1-1") then
         call smatrix_cxc_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI2-2") then
         call smatrix_cxc_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI4-4") then
         call smatrix_cxc_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI3-3") then
         call smatrix_cxc_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI5-5") then
         call smatrix_cxc_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI6-6") then
         call smatrix_cxc_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."-44xIxI00") then
         call smatrix_cxc_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."-4-3xIxI-4-3") then
         call smatrix_cxsx_xIxIcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-43xIxI1-2") then
         call smatrix_cxs_xIxIdux(p1,wgt)
         goto 20
      elseif(str.eq."-43xIxI-43") then
         call smatrix_cxs_xIxIcxs(p1,wgt)
         goto 20
      elseif(str.eq."-43xIxI5-6") then
         call smatrix_cxs_xIxIbtx(p1,wgt)
         goto 20
      elseif(str.eq."-4-5xIxI-4-5") then
         call smatrix_cxbx_xIxIcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-4-5xIxI-3-6") then
         call smatrix_cxbx_xIxIsxtx(p1,wgt)
         goto 20
      elseif(str.eq."-45xIxI-45") then
         call smatrix_cxb_xIxIcxb(p1,wgt)
         goto 20
      elseif(str.eq."-40xIxI-40") then
         call smatrix_cxg_xIxIcxg(p1,wgt)
         goto 20
      elseif(str.eq."4-1xIxI-14") then
         call smatrix_cdx_xIxIdxc(p1,wgt)
         goto 20
      elseif(str.eq."41xIxI14") then
         call smatrix_cd_xIxIdc(p1,wgt)
         goto 20
      elseif(str.eq."41xIxI23") then
         call smatrix_cd_xIxIus(p1,wgt)
         goto 20
      elseif(str.eq."4-2xIxI-13") then
         call smatrix_cux_xIxIdxs(p1,wgt)
         goto 20
      elseif(str.eq."4-2xIxI-24") then
         call smatrix_cux_xIxIuxc(p1,wgt)
         goto 20
      elseif(str.eq."42xIxI24") then
         call smatrix_cu_xIxIuc(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI1-1") then
         call smatrix_ccx_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI2-2") then
         call smatrix_ccx_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI4-4") then
         call smatrix_ccx_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI3-3") then
         call smatrix_ccx_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI5-5") then
         call smatrix_ccx_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI6-6") then
         call smatrix_ccx_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."4-4xIxI00") then
         call smatrix_ccx_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."44xIxI44") then
         call smatrix_cc_xIxIcc(p1,wgt)
         goto 20
      elseif(str.eq."4-3xIxI-12") then
         call smatrix_csx_xIxIdxu(p1,wgt)
         goto 20
      elseif(str.eq."4-3xIxI4-3") then
         call smatrix_csx_xIxIcsx(p1,wgt)
         goto 20
      elseif(str.eq."4-3xIxI-56") then
         call smatrix_csx_xIxIbxt(p1,wgt)
         goto 20
      elseif(str.eq."43xIxI43") then
         call smatrix_cs_xIxIcs(p1,wgt)
         goto 20
      elseif(str.eq."4-5xIxI4-5") then
         call smatrix_cbx_xIxIcbx(p1,wgt)
         goto 20
      elseif(str.eq."45xIxI45") then
         call smatrix_cb_xIxIcb(p1,wgt)
         goto 20
      elseif(str.eq."45xIxI36") then
         call smatrix_cb_xIxIst(p1,wgt)
         goto 20
      elseif(str.eq."40xIxI40") then
         call smatrix_cg_xIxIcg(p1,wgt)
         goto 20
      elseif(str.eq."-3-1xIxI-1-3") then
         call smatrix_sxdx_xIxIdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-31xIxI1-3") then
         call smatrix_sxd_xIxIdsx(p1,wgt)
         goto 20
      elseif(str.eq."-31xIxI2-4") then
         call smatrix_sxd_xIxIucx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2xIxI-1-4") then
         call smatrix_sxux_xIxIdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2xIxI-2-3") then
         call smatrix_sxux_xIxIuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-32xIxI2-3") then
         call smatrix_sxu_xIxIusx(p1,wgt)
         goto 20
      elseif(str.eq."-3-4xIxI-4-3") then
         call smatrix_sxcx_xIxIcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-34xIxI-12") then
         call smatrix_sxc_xIxIdxu(p1,wgt)
         goto 20
      elseif(str.eq."-34xIxI4-3") then
         call smatrix_sxc_xIxIcsx(p1,wgt)
         goto 20
      elseif(str.eq."-34xIxI-56") then
         call smatrix_sxc_xIxIbxt(p1,wgt)
         goto 20
      elseif(str.eq."-3-3xIxI-3-3") then
         call smatrix_sxsx_xIxIsxsx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI1-1") then
         call smatrix_sxs_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI2-2") then
         call smatrix_sxs_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI4-4") then
         call smatrix_sxs_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI3-3") then
         call smatrix_sxs_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI5-5") then
         call smatrix_sxs_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI6-6") then
         call smatrix_sxs_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."-33xIxI00") then
         call smatrix_sxs_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."-3-5xIxI-3-5") then
         call smatrix_sxbx_xIxIsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-35xIxI-46") then
         call smatrix_sxb_xIxIcxt(p1,wgt)
         goto 20
      elseif(str.eq."-35xIxI-35") then
         call smatrix_sxb_xIxIsxb(p1,wgt)
         goto 20
      elseif(str.eq."-30xIxI-30") then
         call smatrix_sxg_xIxIsxg(p1,wgt)
         goto 20
      elseif(str.eq."3-1xIxI-13") then
         call smatrix_sdx_xIxIdxs(p1,wgt)
         goto 20
      elseif(str.eq."3-1xIxI-24") then
         call smatrix_sdx_xIxIuxc(p1,wgt)
         goto 20
      elseif(str.eq."31xIxI13") then
         call smatrix_sd_xIxIds(p1,wgt)
         goto 20
      elseif(str.eq."3-2xIxI-23") then
         call smatrix_sux_xIxIuxs(p1,wgt)
         goto 20
      elseif(str.eq."32xIxI14") then
         call smatrix_su_xIxIdc(p1,wgt)
         goto 20
      elseif(str.eq."32xIxI23") then
         call smatrix_su_xIxIus(p1,wgt)
         goto 20
      elseif(str.eq."3-4xIxI1-2") then
         call smatrix_scx_xIxIdux(p1,wgt)
         goto 20
      elseif(str.eq."3-4xIxI-43") then
         call smatrix_scx_xIxIcxs(p1,wgt)
         goto 20
      elseif(str.eq."3-4xIxI5-6") then
         call smatrix_scx_xIxIbtx(p1,wgt)
         goto 20
      elseif(str.eq."34xIxI43") then
         call smatrix_sc_xIxIcs(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI1-1") then
         call smatrix_ssx_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI2-2") then
         call smatrix_ssx_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI4-4") then
         call smatrix_ssx_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI3-3") then
         call smatrix_ssx_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI5-5") then
         call smatrix_ssx_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI6-6") then
         call smatrix_ssx_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."3-3xIxI00") then
         call smatrix_ssx_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."33xIxI33") then
         call smatrix_ss_xIxIss(p1,wgt)
         goto 20
      elseif(str.eq."3-5xIxI4-6") then
         call smatrix_sbx_xIxIctx(p1,wgt)
         goto 20
      elseif(str.eq."3-5xIxI3-5") then
         call smatrix_sbx_xIxIsbx(p1,wgt)
         goto 20
      elseif(str.eq."35xIxI35") then
         call smatrix_sb_xIxIsb(p1,wgt)
         goto 20
      elseif(str.eq."30xIxI30") then
         call smatrix_sg_xIxIsg(p1,wgt)
         goto 20
      elseif(str.eq."-5-1xIxI-1-5") then
         call smatrix_bxdx_xIxIdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-51xIxI1-5") then
         call smatrix_bxd_xIxIdbx(p1,wgt)
         goto 20
      elseif(str.eq."-51xIxI2-6") then
         call smatrix_bxd_xIxIutx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2xIxI-1-6") then
         call smatrix_bxux_xIxIdxtx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2xIxI-2-5") then
         call smatrix_bxux_xIxIuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-52xIxI2-5") then
         call smatrix_bxu_xIxIubx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4xIxI-4-5") then
         call smatrix_bxcx_xIxIcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4xIxI-3-6") then
         call smatrix_bxcx_xIxIsxtx(p1,wgt)
         goto 20
      elseif(str.eq."-54xIxI4-5") then
         call smatrix_bxc_xIxIcbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-3xIxI-3-5") then
         call smatrix_bxsx_xIxIsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-53xIxI4-6") then
         call smatrix_bxs_xIxIctx(p1,wgt)
         goto 20
      elseif(str.eq."-53xIxI3-5") then
         call smatrix_bxs_xIxIsbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-5xIxI-5-5") then
         call smatrix_bxbx_xIxIbxbx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI1-1") then
         call smatrix_bxb_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI2-2") then
         call smatrix_bxb_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI4-4") then
         call smatrix_bxb_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI3-3") then
         call smatrix_bxb_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI5-5") then
         call smatrix_bxb_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI6-6") then
         call smatrix_bxb_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."-55xIxI00") then
         call smatrix_bxb_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."-50xIxI-50") then
         call smatrix_bxg_xIxIbxg(p1,wgt)
         goto 20
      elseif(str.eq."5-1xIxI-15") then
         call smatrix_bdx_xIxIdxb(p1,wgt)
         goto 20
      elseif(str.eq."5-1xIxI-26") then
         call smatrix_bdx_xIxIuxt(p1,wgt)
         goto 20
      elseif(str.eq."51xIxI15") then
         call smatrix_bd_xIxIdb(p1,wgt)
         goto 20
      elseif(str.eq."5-2xIxI-25") then
         call smatrix_bux_xIxIuxb(p1,wgt)
         goto 20
      elseif(str.eq."52xIxI16") then
         call smatrix_bu_xIxIdt(p1,wgt)
         goto 20
      elseif(str.eq."52xIxI25") then
         call smatrix_bu_xIxIub(p1,wgt)
         goto 20
      elseif(str.eq."5-4xIxI-45") then
         call smatrix_bcx_xIxIcxb(p1,wgt)
         goto 20
      elseif(str.eq."54xIxI45") then
         call smatrix_bc_xIxIcb(p1,wgt)
         goto 20
      elseif(str.eq."54xIxI36") then
         call smatrix_bc_xIxIst(p1,wgt)
         goto 20
      elseif(str.eq."5-3xIxI-46") then
         call smatrix_bsx_xIxIcxt(p1,wgt)
         goto 20
      elseif(str.eq."5-3xIxI-35") then
         call smatrix_bsx_xIxIsxb(p1,wgt)
         goto 20
      elseif(str.eq."53xIxI35") then
         call smatrix_bs_xIxIsb(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI1-1") then
         call smatrix_bbx_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI2-2") then
         call smatrix_bbx_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI4-4") then
         call smatrix_bbx_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI3-3") then
         call smatrix_bbx_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI5-5") then
         call smatrix_bbx_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI6-6") then
         call smatrix_bbx_xIxIttx(p1,wgt)
         goto 20
      elseif(str.eq."5-5xIxI00") then
         call smatrix_bbx_xIxIgg(p1,wgt)
         goto 20
      elseif(str.eq."55xIxI55") then
         call smatrix_bb_xIxIbb(p1,wgt)
         goto 20
      elseif(str.eq."50xIxI50") then
         call smatrix_bg_xIxIbg(p1,wgt)
         goto 20
      elseif(str.eq."0-1xIxI-10") then
         call smatrix_gdx_xIxIdxg(p1,wgt)
         goto 20
      elseif(str.eq."01xIxI10") then
         call smatrix_gd_xIxIdg(p1,wgt)
         goto 20
      elseif(str.eq."0-2xIxI-20") then
         call smatrix_gux_xIxIuxg(p1,wgt)
         goto 20
      elseif(str.eq."02xIxI20") then
         call smatrix_gu_xIxIug(p1,wgt)
         goto 20
      elseif(str.eq."0-4xIxI-40") then
         call smatrix_gcx_xIxIcxg(p1,wgt)
         goto 20
      elseif(str.eq."04xIxI40") then
         call smatrix_gc_xIxIcg(p1,wgt)
         goto 20
      elseif(str.eq."0-3xIxI-30") then
         call smatrix_gsx_xIxIsxg(p1,wgt)
         goto 20
      elseif(str.eq."03xIxI30") then
         call smatrix_gs_xIxIsg(p1,wgt)
         goto 20
      elseif(str.eq."0-5xIxI-50") then
         call smatrix_gbx_xIxIbxg(p1,wgt)
         goto 20
      elseif(str.eq."05xIxI50") then
         call smatrix_gb_xIxIbg(p1,wgt)
         goto 20
      elseif(str.eq."00xIxI1-1") then
         call smatrix_gg_xIxIddx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxI2-2") then
         call smatrix_gg_xIxIuux(p1,wgt)
         goto 20
      elseif(str.eq."00xIxI4-4") then
         call smatrix_gg_xIxIccx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxI3-3") then
         call smatrix_gg_xIxIssx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxI5-5") then
         call smatrix_gg_xIxIbbx(p1,wgt)
         goto 20
      elseif(str.eq."00xIxI6-6") then
         call smatrix_gg_xIxIttx(p1,wgt)
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
      
      
      subroutine real_color_xixi(legs,color)
      implicit none
      include "nexternal.inc"
      include "maxamps.inc"
      Double Precision amp2235(maxamps), jamp2235(0:maxflow)
      common/to_Ramps_dxdx_xIxIdxdx/amp2235,jamp2235
      Double Precision amp2236(maxamps), jamp2236(0:maxflow)
      common/to_Ramps_dxd_xIxIddx/amp2236,jamp2236
      Double Precision amp2237(maxamps), jamp2237(0:maxflow)
      common/to_Ramps_dxd_xIxIuux/amp2237,jamp2237
      Double Precision amp2238(maxamps), jamp2238(0:maxflow)
      common/to_Ramps_dxd_xIxIccx/amp2238,jamp2238
      Double Precision amp2239(maxamps), jamp2239(0:maxflow)
      common/to_Ramps_dxd_xIxIssx/amp2239,jamp2239
      Double Precision amp2240(maxamps), jamp2240(0:maxflow)
      common/to_Ramps_dxd_xIxIbbx/amp2240,jamp2240
      Double Precision amp2241(maxamps), jamp2241(0:maxflow)
      common/to_Ramps_dxd_xIxIttx/amp2241,jamp2241
      Double Precision amp2242(maxamps), jamp2242(0:maxflow)
      common/to_Ramps_dxd_xIxIgg/amp2242,jamp2242
      Double Precision amp2243(maxamps), jamp2243(0:maxflow)
      common/to_Ramps_dxux_xIxIdxux/amp2243,jamp2243
      Double Precision amp2244(maxamps), jamp2244(0:maxflow)
      common/to_Ramps_dxu_xIxIdxu/amp2244,jamp2244
      Double Precision amp2245(maxamps), jamp2245(0:maxflow)
      common/to_Ramps_dxu_xIxIcsx/amp2245,jamp2245
      Double Precision amp2246(maxamps), jamp2246(0:maxflow)
      common/to_Ramps_dxu_xIxIbxt/amp2246,jamp2246
      Double Precision amp2247(maxamps), jamp2247(0:maxflow)
      common/to_Ramps_dxcx_xIxIdxcx/amp2247,jamp2247
      Double Precision amp2248(maxamps), jamp2248(0:maxflow)
      common/to_Ramps_dxcx_xIxIuxsx/amp2248,jamp2248
      Double Precision amp2249(maxamps), jamp2249(0:maxflow)
      common/to_Ramps_dxc_xIxIdxc/amp2249,jamp2249
      Double Precision amp2250(maxamps), jamp2250(0:maxflow)
      common/to_Ramps_dxsx_xIxIdxsx/amp2250,jamp2250
      Double Precision amp2251(maxamps), jamp2251(0:maxflow)
      common/to_Ramps_dxs_xIxIdxs/amp2251,jamp2251
      Double Precision amp2252(maxamps), jamp2252(0:maxflow)
      common/to_Ramps_dxs_xIxIuxc/amp2252,jamp2252
      Double Precision amp2253(maxamps), jamp2253(0:maxflow)
      common/to_Ramps_dxbx_xIxIdxbx/amp2253,jamp2253
      Double Precision amp2254(maxamps), jamp2254(0:maxflow)
      common/to_Ramps_dxb_xIxIdxb/amp2254,jamp2254
      Double Precision amp2255(maxamps), jamp2255(0:maxflow)
      common/to_Ramps_dxb_xIxIuxt/amp2255,jamp2255
      Double Precision amp2256(maxamps), jamp2256(0:maxflow)
      common/to_Ramps_dxg_xIxIdxg/amp2256,jamp2256
      Double Precision amp2257(maxamps), jamp2257(0:maxflow)
      common/to_Ramps_ddx_xIxIddx/amp2257,jamp2257
      Double Precision amp2258(maxamps), jamp2258(0:maxflow)
      common/to_Ramps_ddx_xIxIuux/amp2258,jamp2258
      Double Precision amp2259(maxamps), jamp2259(0:maxflow)
      common/to_Ramps_ddx_xIxIccx/amp2259,jamp2259
      Double Precision amp2260(maxamps), jamp2260(0:maxflow)
      common/to_Ramps_ddx_xIxIssx/amp2260,jamp2260
      Double Precision amp2261(maxamps), jamp2261(0:maxflow)
      common/to_Ramps_ddx_xIxIbbx/amp2261,jamp2261
      Double Precision amp2262(maxamps), jamp2262(0:maxflow)
      common/to_Ramps_ddx_xIxIttx/amp2262,jamp2262
      Double Precision amp2263(maxamps), jamp2263(0:maxflow)
      common/to_Ramps_ddx_xIxIgg/amp2263,jamp2263
      Double Precision amp2264(maxamps), jamp2264(0:maxflow)
      common/to_Ramps_dd_xIxIdd/amp2264,jamp2264
      Double Precision amp2265(maxamps), jamp2265(0:maxflow)
      common/to_Ramps_dux_xIxIdux/amp2265,jamp2265
      Double Precision amp2266(maxamps), jamp2266(0:maxflow)
      common/to_Ramps_dux_xIxIcxs/amp2266,jamp2266
      Double Precision amp2267(maxamps), jamp2267(0:maxflow)
      common/to_Ramps_dux_xIxIbtx/amp2267,jamp2267
      Double Precision amp2268(maxamps), jamp2268(0:maxflow)
      common/to_Ramps_du_xIxIdu/amp2268,jamp2268
      Double Precision amp2269(maxamps), jamp2269(0:maxflow)
      common/to_Ramps_dcx_xIxIdcx/amp2269,jamp2269
      Double Precision amp2270(maxamps), jamp2270(0:maxflow)
      common/to_Ramps_dc_xIxIdc/amp2270,jamp2270
      Double Precision amp2271(maxamps), jamp2271(0:maxflow)
      common/to_Ramps_dc_xIxIus/amp2271,jamp2271
      Double Precision amp2272(maxamps), jamp2272(0:maxflow)
      common/to_Ramps_dsx_xIxIdsx/amp2272,jamp2272
      Double Precision amp2273(maxamps), jamp2273(0:maxflow)
      common/to_Ramps_dsx_xIxIucx/amp2273,jamp2273
      Double Precision amp2274(maxamps), jamp2274(0:maxflow)
      common/to_Ramps_ds_xIxIds/amp2274,jamp2274
      Double Precision amp2275(maxamps), jamp2275(0:maxflow)
      common/to_Ramps_dbx_xIxIdbx/amp2275,jamp2275
      Double Precision amp2276(maxamps), jamp2276(0:maxflow)
      common/to_Ramps_dbx_xIxIutx/amp2276,jamp2276
      Double Precision amp2277(maxamps), jamp2277(0:maxflow)
      common/to_Ramps_db_xIxIdb/amp2277,jamp2277
      Double Precision amp2278(maxamps), jamp2278(0:maxflow)
      common/to_Ramps_dg_xIxIdg/amp2278,jamp2278
      Double Precision amp2279(maxamps), jamp2279(0:maxflow)
      common/to_Ramps_uxdx_xIxIdxux/amp2279,jamp2279
      Double Precision amp2280(maxamps), jamp2280(0:maxflow)
      common/to_Ramps_uxd_xIxIdux/amp2280,jamp2280
      Double Precision amp2281(maxamps), jamp2281(0:maxflow)
      common/to_Ramps_uxd_xIxIcxs/amp2281,jamp2281
      Double Precision amp2282(maxamps), jamp2282(0:maxflow)
      common/to_Ramps_uxd_xIxIbtx/amp2282,jamp2282
      Double Precision amp2283(maxamps), jamp2283(0:maxflow)
      common/to_Ramps_uxux_xIxIuxux/amp2283,jamp2283
      Double Precision amp2284(maxamps), jamp2284(0:maxflow)
      common/to_Ramps_uxu_xIxIddx/amp2284,jamp2284
      Double Precision amp2285(maxamps), jamp2285(0:maxflow)
      common/to_Ramps_uxu_xIxIuux/amp2285,jamp2285
      Double Precision amp2286(maxamps), jamp2286(0:maxflow)
      common/to_Ramps_uxu_xIxIccx/amp2286,jamp2286
      Double Precision amp2287(maxamps), jamp2287(0:maxflow)
      common/to_Ramps_uxu_xIxIssx/amp2287,jamp2287
      Double Precision amp2288(maxamps), jamp2288(0:maxflow)
      common/to_Ramps_uxu_xIxIbbx/amp2288,jamp2288
      Double Precision amp2289(maxamps), jamp2289(0:maxflow)
      common/to_Ramps_uxu_xIxIttx/amp2289,jamp2289
      Double Precision amp2290(maxamps), jamp2290(0:maxflow)
      common/to_Ramps_uxu_xIxIgg/amp2290,jamp2290
      Double Precision amp2291(maxamps), jamp2291(0:maxflow)
      common/to_Ramps_uxcx_xIxIuxcx/amp2291,jamp2291
      Double Precision amp2292(maxamps), jamp2292(0:maxflow)
      common/to_Ramps_uxc_xIxIdxs/amp2292,jamp2292
      Double Precision amp2293(maxamps), jamp2293(0:maxflow)
      common/to_Ramps_uxc_xIxIuxc/amp2293,jamp2293
      Double Precision amp2294(maxamps), jamp2294(0:maxflow)
      common/to_Ramps_uxsx_xIxIdxcx/amp2294,jamp2294
      Double Precision amp2295(maxamps), jamp2295(0:maxflow)
      common/to_Ramps_uxsx_xIxIuxsx/amp2295,jamp2295
      Double Precision amp2296(maxamps), jamp2296(0:maxflow)
      common/to_Ramps_uxs_xIxIuxs/amp2296,jamp2296
      Double Precision amp2297(maxamps), jamp2297(0:maxflow)
      common/to_Ramps_uxbx_xIxIdxtx/amp2297,jamp2297
      Double Precision amp2298(maxamps), jamp2298(0:maxflow)
      common/to_Ramps_uxbx_xIxIuxbx/amp2298,jamp2298
      Double Precision amp2299(maxamps), jamp2299(0:maxflow)
      common/to_Ramps_uxb_xIxIuxb/amp2299,jamp2299
      Double Precision amp2300(maxamps), jamp2300(0:maxflow)
      common/to_Ramps_uxg_xIxIuxg/amp2300,jamp2300
      Double Precision amp2301(maxamps), jamp2301(0:maxflow)
      common/to_Ramps_udx_xIxIdxu/amp2301,jamp2301
      Double Precision amp2302(maxamps), jamp2302(0:maxflow)
      common/to_Ramps_udx_xIxIcsx/amp2302,jamp2302
      Double Precision amp2303(maxamps), jamp2303(0:maxflow)
      common/to_Ramps_udx_xIxIbxt/amp2303,jamp2303
      Double Precision amp2304(maxamps), jamp2304(0:maxflow)
      common/to_Ramps_ud_xIxIdu/amp2304,jamp2304
      Double Precision amp2305(maxamps), jamp2305(0:maxflow)
      common/to_Ramps_uux_xIxIddx/amp2305,jamp2305
      Double Precision amp2306(maxamps), jamp2306(0:maxflow)
      common/to_Ramps_uux_xIxIuux/amp2306,jamp2306
      Double Precision amp2307(maxamps), jamp2307(0:maxflow)
      common/to_Ramps_uux_xIxIccx/amp2307,jamp2307
      Double Precision amp2308(maxamps), jamp2308(0:maxflow)
      common/to_Ramps_uux_xIxIssx/amp2308,jamp2308
      Double Precision amp2309(maxamps), jamp2309(0:maxflow)
      common/to_Ramps_uux_xIxIbbx/amp2309,jamp2309
      Double Precision amp2310(maxamps), jamp2310(0:maxflow)
      common/to_Ramps_uux_xIxIttx/amp2310,jamp2310
      Double Precision amp2311(maxamps), jamp2311(0:maxflow)
      common/to_Ramps_uux_xIxIgg/amp2311,jamp2311
      Double Precision amp2312(maxamps), jamp2312(0:maxflow)
      common/to_Ramps_uu_xIxIuu/amp2312,jamp2312
      Double Precision amp2313(maxamps), jamp2313(0:maxflow)
      common/to_Ramps_ucx_xIxIdsx/amp2313,jamp2313
      Double Precision amp2314(maxamps), jamp2314(0:maxflow)
      common/to_Ramps_ucx_xIxIucx/amp2314,jamp2314
      Double Precision amp2315(maxamps), jamp2315(0:maxflow)
      common/to_Ramps_uc_xIxIuc/amp2315,jamp2315
      Double Precision amp2316(maxamps), jamp2316(0:maxflow)
      common/to_Ramps_usx_xIxIusx/amp2316,jamp2316
      Double Precision amp2317(maxamps), jamp2317(0:maxflow)
      common/to_Ramps_us_xIxIdc/amp2317,jamp2317
      Double Precision amp2318(maxamps), jamp2318(0:maxflow)
      common/to_Ramps_us_xIxIus/amp2318,jamp2318
      Double Precision amp2319(maxamps), jamp2319(0:maxflow)
      common/to_Ramps_ubx_xIxIubx/amp2319,jamp2319
      Double Precision amp2320(maxamps), jamp2320(0:maxflow)
      common/to_Ramps_ub_xIxIdt/amp2320,jamp2320
      Double Precision amp2321(maxamps), jamp2321(0:maxflow)
      common/to_Ramps_ub_xIxIub/amp2321,jamp2321
      Double Precision amp2322(maxamps), jamp2322(0:maxflow)
      common/to_Ramps_ug_xIxIug/amp2322,jamp2322
      Double Precision amp2323(maxamps), jamp2323(0:maxflow)
      common/to_Ramps_cxdx_xIxIdxcx/amp2323,jamp2323
      Double Precision amp2324(maxamps), jamp2324(0:maxflow)
      common/to_Ramps_cxdx_xIxIuxsx/amp2324,jamp2324
      Double Precision amp2325(maxamps), jamp2325(0:maxflow)
      common/to_Ramps_cxd_xIxIdcx/amp2325,jamp2325
      Double Precision amp2326(maxamps), jamp2326(0:maxflow)
      common/to_Ramps_cxux_xIxIuxcx/amp2326,jamp2326
      Double Precision amp2327(maxamps), jamp2327(0:maxflow)
      common/to_Ramps_cxu_xIxIdsx/amp2327,jamp2327
      Double Precision amp2328(maxamps), jamp2328(0:maxflow)
      common/to_Ramps_cxu_xIxIucx/amp2328,jamp2328
      Double Precision amp2329(maxamps), jamp2329(0:maxflow)
      common/to_Ramps_cxcx_xIxIcxcx/amp2329,jamp2329
      Double Precision amp2330(maxamps), jamp2330(0:maxflow)
      common/to_Ramps_cxc_xIxIddx/amp2330,jamp2330
      Double Precision amp2331(maxamps), jamp2331(0:maxflow)
      common/to_Ramps_cxc_xIxIuux/amp2331,jamp2331
      Double Precision amp2332(maxamps), jamp2332(0:maxflow)
      common/to_Ramps_cxc_xIxIccx/amp2332,jamp2332
      Double Precision amp2333(maxamps), jamp2333(0:maxflow)
      common/to_Ramps_cxc_xIxIssx/amp2333,jamp2333
      Double Precision amp2334(maxamps), jamp2334(0:maxflow)
      common/to_Ramps_cxc_xIxIbbx/amp2334,jamp2334
      Double Precision amp2335(maxamps), jamp2335(0:maxflow)
      common/to_Ramps_cxc_xIxIttx/amp2335,jamp2335
      Double Precision amp2336(maxamps), jamp2336(0:maxflow)
      common/to_Ramps_cxc_xIxIgg/amp2336,jamp2336
      Double Precision amp2337(maxamps), jamp2337(0:maxflow)
      common/to_Ramps_cxsx_xIxIcxsx/amp2337,jamp2337
      Double Precision amp2338(maxamps), jamp2338(0:maxflow)
      common/to_Ramps_cxs_xIxIdux/amp2338,jamp2338
      Double Precision amp2339(maxamps), jamp2339(0:maxflow)
      common/to_Ramps_cxs_xIxIcxs/amp2339,jamp2339
      Double Precision amp2340(maxamps), jamp2340(0:maxflow)
      common/to_Ramps_cxs_xIxIbtx/amp2340,jamp2340
      Double Precision amp2341(maxamps), jamp2341(0:maxflow)
      common/to_Ramps_cxbx_xIxIcxbx/amp2341,jamp2341
      Double Precision amp2342(maxamps), jamp2342(0:maxflow)
      common/to_Ramps_cxbx_xIxIsxtx/amp2342,jamp2342
      Double Precision amp2343(maxamps), jamp2343(0:maxflow)
      common/to_Ramps_cxb_xIxIcxb/amp2343,jamp2343
      Double Precision amp2344(maxamps), jamp2344(0:maxflow)
      common/to_Ramps_cxg_xIxIcxg/amp2344,jamp2344
      Double Precision amp2345(maxamps), jamp2345(0:maxflow)
      common/to_Ramps_cdx_xIxIdxc/amp2345,jamp2345
      Double Precision amp2346(maxamps), jamp2346(0:maxflow)
      common/to_Ramps_cd_xIxIdc/amp2346,jamp2346
      Double Precision amp2347(maxamps), jamp2347(0:maxflow)
      common/to_Ramps_cd_xIxIus/amp2347,jamp2347
      Double Precision amp2348(maxamps), jamp2348(0:maxflow)
      common/to_Ramps_cux_xIxIdxs/amp2348,jamp2348
      Double Precision amp2349(maxamps), jamp2349(0:maxflow)
      common/to_Ramps_cux_xIxIuxc/amp2349,jamp2349
      Double Precision amp2350(maxamps), jamp2350(0:maxflow)
      common/to_Ramps_cu_xIxIuc/amp2350,jamp2350
      Double Precision amp2351(maxamps), jamp2351(0:maxflow)
      common/to_Ramps_ccx_xIxIddx/amp2351,jamp2351
      Double Precision amp2352(maxamps), jamp2352(0:maxflow)
      common/to_Ramps_ccx_xIxIuux/amp2352,jamp2352
      Double Precision amp2353(maxamps), jamp2353(0:maxflow)
      common/to_Ramps_ccx_xIxIccx/amp2353,jamp2353
      Double Precision amp2354(maxamps), jamp2354(0:maxflow)
      common/to_Ramps_ccx_xIxIssx/amp2354,jamp2354
      Double Precision amp2355(maxamps), jamp2355(0:maxflow)
      common/to_Ramps_ccx_xIxIbbx/amp2355,jamp2355
      Double Precision amp2356(maxamps), jamp2356(0:maxflow)
      common/to_Ramps_ccx_xIxIttx/amp2356,jamp2356
      Double Precision amp2357(maxamps), jamp2357(0:maxflow)
      common/to_Ramps_ccx_xIxIgg/amp2357,jamp2357
      Double Precision amp2358(maxamps), jamp2358(0:maxflow)
      common/to_Ramps_cc_xIxIcc/amp2358,jamp2358
      Double Precision amp2359(maxamps), jamp2359(0:maxflow)
      common/to_Ramps_csx_xIxIdxu/amp2359,jamp2359
      Double Precision amp2360(maxamps), jamp2360(0:maxflow)
      common/to_Ramps_csx_xIxIcsx/amp2360,jamp2360
      Double Precision amp2361(maxamps), jamp2361(0:maxflow)
      common/to_Ramps_csx_xIxIbxt/amp2361,jamp2361
      Double Precision amp2362(maxamps), jamp2362(0:maxflow)
      common/to_Ramps_cs_xIxIcs/amp2362,jamp2362
      Double Precision amp2363(maxamps), jamp2363(0:maxflow)
      common/to_Ramps_cbx_xIxIcbx/amp2363,jamp2363
      Double Precision amp2364(maxamps), jamp2364(0:maxflow)
      common/to_Ramps_cb_xIxIcb/amp2364,jamp2364
      Double Precision amp2365(maxamps), jamp2365(0:maxflow)
      common/to_Ramps_cb_xIxIst/amp2365,jamp2365
      Double Precision amp2366(maxamps), jamp2366(0:maxflow)
      common/to_Ramps_cg_xIxIcg/amp2366,jamp2366
      Double Precision amp2367(maxamps), jamp2367(0:maxflow)
      common/to_Ramps_sxdx_xIxIdxsx/amp2367,jamp2367
      Double Precision amp2368(maxamps), jamp2368(0:maxflow)
      common/to_Ramps_sxd_xIxIdsx/amp2368,jamp2368
      Double Precision amp2369(maxamps), jamp2369(0:maxflow)
      common/to_Ramps_sxd_xIxIucx/amp2369,jamp2369
      Double Precision amp2370(maxamps), jamp2370(0:maxflow)
      common/to_Ramps_sxux_xIxIdxcx/amp2370,jamp2370
      Double Precision amp2371(maxamps), jamp2371(0:maxflow)
      common/to_Ramps_sxux_xIxIuxsx/amp2371,jamp2371
      Double Precision amp2372(maxamps), jamp2372(0:maxflow)
      common/to_Ramps_sxu_xIxIusx/amp2372,jamp2372
      Double Precision amp2373(maxamps), jamp2373(0:maxflow)
      common/to_Ramps_sxcx_xIxIcxsx/amp2373,jamp2373
      Double Precision amp2374(maxamps), jamp2374(0:maxflow)
      common/to_Ramps_sxc_xIxIdxu/amp2374,jamp2374
      Double Precision amp2375(maxamps), jamp2375(0:maxflow)
      common/to_Ramps_sxc_xIxIcsx/amp2375,jamp2375
      Double Precision amp2376(maxamps), jamp2376(0:maxflow)
      common/to_Ramps_sxc_xIxIbxt/amp2376,jamp2376
      Double Precision amp2377(maxamps), jamp2377(0:maxflow)
      common/to_Ramps_sxsx_xIxIsxsx/amp2377,jamp2377
      Double Precision amp2378(maxamps), jamp2378(0:maxflow)
      common/to_Ramps_sxs_xIxIddx/amp2378,jamp2378
      Double Precision amp2379(maxamps), jamp2379(0:maxflow)
      common/to_Ramps_sxs_xIxIuux/amp2379,jamp2379
      Double Precision amp2380(maxamps), jamp2380(0:maxflow)
      common/to_Ramps_sxs_xIxIccx/amp2380,jamp2380
      Double Precision amp2381(maxamps), jamp2381(0:maxflow)
      common/to_Ramps_sxs_xIxIssx/amp2381,jamp2381
      Double Precision amp2382(maxamps), jamp2382(0:maxflow)
      common/to_Ramps_sxs_xIxIbbx/amp2382,jamp2382
      Double Precision amp2383(maxamps), jamp2383(0:maxflow)
      common/to_Ramps_sxs_xIxIttx/amp2383,jamp2383
      Double Precision amp2384(maxamps), jamp2384(0:maxflow)
      common/to_Ramps_sxs_xIxIgg/amp2384,jamp2384
      Double Precision amp2385(maxamps), jamp2385(0:maxflow)
      common/to_Ramps_sxbx_xIxIsxbx/amp2385,jamp2385
      Double Precision amp2386(maxamps), jamp2386(0:maxflow)
      common/to_Ramps_sxb_xIxIcxt/amp2386,jamp2386
      Double Precision amp2387(maxamps), jamp2387(0:maxflow)
      common/to_Ramps_sxb_xIxIsxb/amp2387,jamp2387
      Double Precision amp2388(maxamps), jamp2388(0:maxflow)
      common/to_Ramps_sxg_xIxIsxg/amp2388,jamp2388
      Double Precision amp2389(maxamps), jamp2389(0:maxflow)
      common/to_Ramps_sdx_xIxIdxs/amp2389,jamp2389
      Double Precision amp2390(maxamps), jamp2390(0:maxflow)
      common/to_Ramps_sdx_xIxIuxc/amp2390,jamp2390
      Double Precision amp2391(maxamps), jamp2391(0:maxflow)
      common/to_Ramps_sd_xIxIds/amp2391,jamp2391
      Double Precision amp2392(maxamps), jamp2392(0:maxflow)
      common/to_Ramps_sux_xIxIuxs/amp2392,jamp2392
      Double Precision amp2393(maxamps), jamp2393(0:maxflow)
      common/to_Ramps_su_xIxIdc/amp2393,jamp2393
      Double Precision amp2394(maxamps), jamp2394(0:maxflow)
      common/to_Ramps_su_xIxIus/amp2394,jamp2394
      Double Precision amp2395(maxamps), jamp2395(0:maxflow)
      common/to_Ramps_scx_xIxIdux/amp2395,jamp2395
      Double Precision amp2396(maxamps), jamp2396(0:maxflow)
      common/to_Ramps_scx_xIxIcxs/amp2396,jamp2396
      Double Precision amp2397(maxamps), jamp2397(0:maxflow)
      common/to_Ramps_scx_xIxIbtx/amp2397,jamp2397
      Double Precision amp2398(maxamps), jamp2398(0:maxflow)
      common/to_Ramps_sc_xIxIcs/amp2398,jamp2398
      Double Precision amp2399(maxamps), jamp2399(0:maxflow)
      common/to_Ramps_ssx_xIxIddx/amp2399,jamp2399
      Double Precision amp2400(maxamps), jamp2400(0:maxflow)
      common/to_Ramps_ssx_xIxIuux/amp2400,jamp2400
      Double Precision amp2401(maxamps), jamp2401(0:maxflow)
      common/to_Ramps_ssx_xIxIccx/amp2401,jamp2401
      Double Precision amp2402(maxamps), jamp2402(0:maxflow)
      common/to_Ramps_ssx_xIxIssx/amp2402,jamp2402
      Double Precision amp2403(maxamps), jamp2403(0:maxflow)
      common/to_Ramps_ssx_xIxIbbx/amp2403,jamp2403
      Double Precision amp2404(maxamps), jamp2404(0:maxflow)
      common/to_Ramps_ssx_xIxIttx/amp2404,jamp2404
      Double Precision amp2405(maxamps), jamp2405(0:maxflow)
      common/to_Ramps_ssx_xIxIgg/amp2405,jamp2405
      Double Precision amp2406(maxamps), jamp2406(0:maxflow)
      common/to_Ramps_ss_xIxIss/amp2406,jamp2406
      Double Precision amp2407(maxamps), jamp2407(0:maxflow)
      common/to_Ramps_sbx_xIxIctx/amp2407,jamp2407
      Double Precision amp2408(maxamps), jamp2408(0:maxflow)
      common/to_Ramps_sbx_xIxIsbx/amp2408,jamp2408
      Double Precision amp2409(maxamps), jamp2409(0:maxflow)
      common/to_Ramps_sb_xIxIsb/amp2409,jamp2409
      Double Precision amp2410(maxamps), jamp2410(0:maxflow)
      common/to_Ramps_sg_xIxIsg/amp2410,jamp2410
      Double Precision amp2411(maxamps), jamp2411(0:maxflow)
      common/to_Ramps_bxdx_xIxIdxbx/amp2411,jamp2411
      Double Precision amp2412(maxamps), jamp2412(0:maxflow)
      common/to_Ramps_bxd_xIxIdbx/amp2412,jamp2412
      Double Precision amp2413(maxamps), jamp2413(0:maxflow)
      common/to_Ramps_bxd_xIxIutx/amp2413,jamp2413
      Double Precision amp2414(maxamps), jamp2414(0:maxflow)
      common/to_Ramps_bxux_xIxIdxtx/amp2414,jamp2414
      Double Precision amp2415(maxamps), jamp2415(0:maxflow)
      common/to_Ramps_bxux_xIxIuxbx/amp2415,jamp2415
      Double Precision amp2416(maxamps), jamp2416(0:maxflow)
      common/to_Ramps_bxu_xIxIubx/amp2416,jamp2416
      Double Precision amp2417(maxamps), jamp2417(0:maxflow)
      common/to_Ramps_bxcx_xIxIcxbx/amp2417,jamp2417
      Double Precision amp2418(maxamps), jamp2418(0:maxflow)
      common/to_Ramps_bxcx_xIxIsxtx/amp2418,jamp2418
      Double Precision amp2419(maxamps), jamp2419(0:maxflow)
      common/to_Ramps_bxc_xIxIcbx/amp2419,jamp2419
      Double Precision amp2420(maxamps), jamp2420(0:maxflow)
      common/to_Ramps_bxsx_xIxIsxbx/amp2420,jamp2420
      Double Precision amp2421(maxamps), jamp2421(0:maxflow)
      common/to_Ramps_bxs_xIxIctx/amp2421,jamp2421
      Double Precision amp2422(maxamps), jamp2422(0:maxflow)
      common/to_Ramps_bxs_xIxIsbx/amp2422,jamp2422
      Double Precision amp2423(maxamps), jamp2423(0:maxflow)
      common/to_Ramps_bxbx_xIxIbxbx/amp2423,jamp2423
      Double Precision amp2424(maxamps), jamp2424(0:maxflow)
      common/to_Ramps_bxb_xIxIddx/amp2424,jamp2424
      Double Precision amp2425(maxamps), jamp2425(0:maxflow)
      common/to_Ramps_bxb_xIxIuux/amp2425,jamp2425
      Double Precision amp2426(maxamps), jamp2426(0:maxflow)
      common/to_Ramps_bxb_xIxIccx/amp2426,jamp2426
      Double Precision amp2427(maxamps), jamp2427(0:maxflow)
      common/to_Ramps_bxb_xIxIssx/amp2427,jamp2427
      Double Precision amp2428(maxamps), jamp2428(0:maxflow)
      common/to_Ramps_bxb_xIxIbbx/amp2428,jamp2428
      Double Precision amp2429(maxamps), jamp2429(0:maxflow)
      common/to_Ramps_bxb_xIxIttx/amp2429,jamp2429
      Double Precision amp2430(maxamps), jamp2430(0:maxflow)
      common/to_Ramps_bxb_xIxIgg/amp2430,jamp2430
      Double Precision amp2431(maxamps), jamp2431(0:maxflow)
      common/to_Ramps_bxg_xIxIbxg/amp2431,jamp2431
      Double Precision amp2432(maxamps), jamp2432(0:maxflow)
      common/to_Ramps_bdx_xIxIdxb/amp2432,jamp2432
      Double Precision amp2433(maxamps), jamp2433(0:maxflow)
      common/to_Ramps_bdx_xIxIuxt/amp2433,jamp2433
      Double Precision amp2434(maxamps), jamp2434(0:maxflow)
      common/to_Ramps_bd_xIxIdb/amp2434,jamp2434
      Double Precision amp2435(maxamps), jamp2435(0:maxflow)
      common/to_Ramps_bux_xIxIuxb/amp2435,jamp2435
      Double Precision amp2436(maxamps), jamp2436(0:maxflow)
      common/to_Ramps_bu_xIxIdt/amp2436,jamp2436
      Double Precision amp2437(maxamps), jamp2437(0:maxflow)
      common/to_Ramps_bu_xIxIub/amp2437,jamp2437
      Double Precision amp2438(maxamps), jamp2438(0:maxflow)
      common/to_Ramps_bcx_xIxIcxb/amp2438,jamp2438
      Double Precision amp2439(maxamps), jamp2439(0:maxflow)
      common/to_Ramps_bc_xIxIcb/amp2439,jamp2439
      Double Precision amp2440(maxamps), jamp2440(0:maxflow)
      common/to_Ramps_bc_xIxIst/amp2440,jamp2440
      Double Precision amp2441(maxamps), jamp2441(0:maxflow)
      common/to_Ramps_bsx_xIxIcxt/amp2441,jamp2441
      Double Precision amp2442(maxamps), jamp2442(0:maxflow)
      common/to_Ramps_bsx_xIxIsxb/amp2442,jamp2442
      Double Precision amp2443(maxamps), jamp2443(0:maxflow)
      common/to_Ramps_bs_xIxIsb/amp2443,jamp2443
      Double Precision amp2444(maxamps), jamp2444(0:maxflow)
      common/to_Ramps_bbx_xIxIddx/amp2444,jamp2444
      Double Precision amp2445(maxamps), jamp2445(0:maxflow)
      common/to_Ramps_bbx_xIxIuux/amp2445,jamp2445
      Double Precision amp2446(maxamps), jamp2446(0:maxflow)
      common/to_Ramps_bbx_xIxIccx/amp2446,jamp2446
      Double Precision amp2447(maxamps), jamp2447(0:maxflow)
      common/to_Ramps_bbx_xIxIssx/amp2447,jamp2447
      Double Precision amp2448(maxamps), jamp2448(0:maxflow)
      common/to_Ramps_bbx_xIxIbbx/amp2448,jamp2448
      Double Precision amp2449(maxamps), jamp2449(0:maxflow)
      common/to_Ramps_bbx_xIxIttx/amp2449,jamp2449
      Double Precision amp2450(maxamps), jamp2450(0:maxflow)
      common/to_Ramps_bbx_xIxIgg/amp2450,jamp2450
      Double Precision amp2451(maxamps), jamp2451(0:maxflow)
      common/to_Ramps_bb_xIxIbb/amp2451,jamp2451
      Double Precision amp2452(maxamps), jamp2452(0:maxflow)
      common/to_Ramps_bg_xIxIbg/amp2452,jamp2452
      Double Precision amp2453(maxamps), jamp2453(0:maxflow)
      common/to_Ramps_gdx_xIxIdxg/amp2453,jamp2453
      Double Precision amp2454(maxamps), jamp2454(0:maxflow)
      common/to_Ramps_gd_xIxIdg/amp2454,jamp2454
      Double Precision amp2455(maxamps), jamp2455(0:maxflow)
      common/to_Ramps_gux_xIxIuxg/amp2455,jamp2455
      Double Precision amp2456(maxamps), jamp2456(0:maxflow)
      common/to_Ramps_gu_xIxIug/amp2456,jamp2456
      Double Precision amp2457(maxamps), jamp2457(0:maxflow)
      common/to_Ramps_gcx_xIxIcxg/amp2457,jamp2457
      Double Precision amp2458(maxamps), jamp2458(0:maxflow)
      common/to_Ramps_gc_xIxIcg/amp2458,jamp2458
      Double Precision amp2459(maxamps), jamp2459(0:maxflow)
      common/to_Ramps_gsx_xIxIsxg/amp2459,jamp2459
      Double Precision amp2460(maxamps), jamp2460(0:maxflow)
      common/to_Ramps_gs_xIxIsg/amp2460,jamp2460
      Double Precision amp2461(maxamps), jamp2461(0:maxflow)
      common/to_Ramps_gbx_xIxIbxg/amp2461,jamp2461
      Double Precision amp2462(maxamps), jamp2462(0:maxflow)
      common/to_Ramps_gb_xIxIbg/amp2462,jamp2462
      Double Precision amp2463(maxamps), jamp2463(0:maxflow)
      common/to_Ramps_gg_xIxIddx/amp2463,jamp2463
      Double Precision amp2464(maxamps), jamp2464(0:maxflow)
      common/to_Ramps_gg_xIxIuux/amp2464,jamp2464
      Double Precision amp2465(maxamps), jamp2465(0:maxflow)
      common/to_Ramps_gg_xIxIccx/amp2465,jamp2465
      Double Precision amp2466(maxamps), jamp2466(0:maxflow)
      common/to_Ramps_gg_xIxIssx/amp2466,jamp2466
      Double Precision amp2467(maxamps), jamp2467(0:maxflow)
      common/to_Ramps_gg_xIxIbbx/amp2467,jamp2467
      Double Precision amp2468(maxamps), jamp2468(0:maxflow)
      common/to_Ramps_gg_xIxIttx/amp2468,jamp2468
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
      
      if(str.eq."-1-1xIxI-1-1") then
         include "leshouches_R_235.inc"
         iflow=nint(jamp2235(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2235(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI1-1") then
         include "leshouches_R_236.inc"
         iflow=nint(jamp2236(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2236(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI2-2") then
         include "leshouches_R_237.inc"
         iflow=nint(jamp2237(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2237(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI4-4") then
         include "leshouches_R_238.inc"
         iflow=nint(jamp2238(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2238(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI3-3") then
         include "leshouches_R_239.inc"
         iflow=nint(jamp2239(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2239(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI5-5") then
         include "leshouches_R_240.inc"
         iflow=nint(jamp2240(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2240(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI6-6") then
         include "leshouches_R_241.inc"
         iflow=nint(jamp2241(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2241(i)
         enddo
         goto 20
      elseif(str.eq."-11xIxI00") then
         include "leshouches_R_242.inc"
         iflow=nint(jamp2242(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2242(i)
         enddo
         goto 20
      elseif(str.eq."-1-2xIxI-1-2") then
         include "leshouches_R_243.inc"
         iflow=nint(jamp2243(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2243(i)
         enddo
         goto 20
      elseif(str.eq."-12xIxI-12") then
         include "leshouches_R_244.inc"
         iflow=nint(jamp2244(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2244(i)
         enddo
         goto 20
      elseif(str.eq."-12xIxI4-3") then
         include "leshouches_R_245.inc"
         iflow=nint(jamp2245(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2245(i)
         enddo
         goto 20
      elseif(str.eq."-12xIxI-56") then
         include "leshouches_R_246.inc"
         iflow=nint(jamp2246(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2246(i)
         enddo
         goto 20
      elseif(str.eq."-1-4xIxI-1-4") then
         include "leshouches_R_247.inc"
         iflow=nint(jamp2247(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2247(i)
         enddo
         goto 20
      elseif(str.eq."-1-4xIxI-2-3") then
         include "leshouches_R_248.inc"
         iflow=nint(jamp2248(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2248(i)
         enddo
         goto 20
      elseif(str.eq."-14xIxI-14") then
         include "leshouches_R_249.inc"
         iflow=nint(jamp2249(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2249(i)
         enddo
         goto 20
      elseif(str.eq."-1-3xIxI-1-3") then
         include "leshouches_R_250.inc"
         iflow=nint(jamp2250(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2250(i)
         enddo
         goto 20
      elseif(str.eq."-13xIxI-13") then
         include "leshouches_R_251.inc"
         iflow=nint(jamp2251(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2251(i)
         enddo
         goto 20
      elseif(str.eq."-13xIxI-24") then
         include "leshouches_R_252.inc"
         iflow=nint(jamp2252(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2252(i)
         enddo
         goto 20
      elseif(str.eq."-1-5xIxI-1-5") then
         include "leshouches_R_253.inc"
         iflow=nint(jamp2253(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2253(i)
         enddo
         goto 20
      elseif(str.eq."-15xIxI-15") then
         include "leshouches_R_254.inc"
         iflow=nint(jamp2254(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2254(i)
         enddo
         goto 20
      elseif(str.eq."-15xIxI-26") then
         include "leshouches_R_255.inc"
         iflow=nint(jamp2255(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2255(i)
         enddo
         goto 20
      elseif(str.eq."-10xIxI-10") then
         include "leshouches_R_256.inc"
         iflow=nint(jamp2256(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2256(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI1-1") then
         include "leshouches_R_257.inc"
         iflow=nint(jamp2257(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2257(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI2-2") then
         include "leshouches_R_258.inc"
         iflow=nint(jamp2258(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2258(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI4-4") then
         include "leshouches_R_259.inc"
         iflow=nint(jamp2259(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2259(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI3-3") then
         include "leshouches_R_260.inc"
         iflow=nint(jamp2260(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2260(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI5-5") then
         include "leshouches_R_261.inc"
         iflow=nint(jamp2261(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2261(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI6-6") then
         include "leshouches_R_262.inc"
         iflow=nint(jamp2262(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2262(i)
         enddo
         goto 20
      elseif(str.eq."1-1xIxI00") then
         include "leshouches_R_263.inc"
         iflow=nint(jamp2263(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2263(i)
         enddo
         goto 20
      elseif(str.eq."11xIxI11") then
         include "leshouches_R_264.inc"
         iflow=nint(jamp2264(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2264(i)
         enddo
         goto 20
      elseif(str.eq."1-2xIxI1-2") then
         include "leshouches_R_265.inc"
         iflow=nint(jamp2265(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2265(i)
         enddo
         goto 20
      elseif(str.eq."1-2xIxI-43") then
         include "leshouches_R_266.inc"
         iflow=nint(jamp2266(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2266(i)
         enddo
         goto 20
      elseif(str.eq."1-2xIxI5-6") then
         include "leshouches_R_267.inc"
         iflow=nint(jamp2267(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2267(i)
         enddo
         goto 20
      elseif(str.eq."12xIxI12") then
         include "leshouches_R_268.inc"
         iflow=nint(jamp2268(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2268(i)
         enddo
         goto 20
      elseif(str.eq."1-4xIxI1-4") then
         include "leshouches_R_269.inc"
         iflow=nint(jamp2269(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2269(i)
         enddo
         goto 20
      elseif(str.eq."14xIxI14") then
         include "leshouches_R_270.inc"
         iflow=nint(jamp2270(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2270(i)
         enddo
         goto 20
      elseif(str.eq."14xIxI23") then
         include "leshouches_R_271.inc"
         iflow=nint(jamp2271(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2271(i)
         enddo
         goto 20
      elseif(str.eq."1-3xIxI1-3") then
         include "leshouches_R_272.inc"
         iflow=nint(jamp2272(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2272(i)
         enddo
         goto 20
      elseif(str.eq."1-3xIxI2-4") then
         include "leshouches_R_273.inc"
         iflow=nint(jamp2273(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2273(i)
         enddo
         goto 20
      elseif(str.eq."13xIxI13") then
         include "leshouches_R_274.inc"
         iflow=nint(jamp2274(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2274(i)
         enddo
         goto 20
      elseif(str.eq."1-5xIxI1-5") then
         include "leshouches_R_275.inc"
         iflow=nint(jamp2275(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2275(i)
         enddo
         goto 20
      elseif(str.eq."1-5xIxI2-6") then
         include "leshouches_R_276.inc"
         iflow=nint(jamp2276(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2276(i)
         enddo
         goto 20
      elseif(str.eq."15xIxI15") then
         include "leshouches_R_277.inc"
         iflow=nint(jamp2277(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2277(i)
         enddo
         goto 20
      elseif(str.eq."10xIxI10") then
         include "leshouches_R_278.inc"
         iflow=nint(jamp2278(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2278(i)
         enddo
         goto 20
      elseif(str.eq."-2-1xIxI-1-2") then
         include "leshouches_R_279.inc"
         iflow=nint(jamp2279(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2279(i)
         enddo
         goto 20
      elseif(str.eq."-21xIxI1-2") then
         include "leshouches_R_280.inc"
         iflow=nint(jamp2280(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2280(i)
         enddo
         goto 20
      elseif(str.eq."-21xIxI-43") then
         include "leshouches_R_281.inc"
         iflow=nint(jamp2281(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2281(i)
         enddo
         goto 20
      elseif(str.eq."-21xIxI5-6") then
         include "leshouches_R_282.inc"
         iflow=nint(jamp2282(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2282(i)
         enddo
         goto 20
      elseif(str.eq."-2-2xIxI-2-2") then
         include "leshouches_R_283.inc"
         iflow=nint(jamp2283(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2283(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI1-1") then
         include "leshouches_R_284.inc"
         iflow=nint(jamp2284(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2284(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI2-2") then
         include "leshouches_R_285.inc"
         iflow=nint(jamp2285(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2285(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI4-4") then
         include "leshouches_R_286.inc"
         iflow=nint(jamp2286(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2286(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI3-3") then
         include "leshouches_R_287.inc"
         iflow=nint(jamp2287(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2287(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI5-5") then
         include "leshouches_R_288.inc"
         iflow=nint(jamp2288(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2288(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI6-6") then
         include "leshouches_R_289.inc"
         iflow=nint(jamp2289(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2289(i)
         enddo
         goto 20
      elseif(str.eq."-22xIxI00") then
         include "leshouches_R_290.inc"
         iflow=nint(jamp2290(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2290(i)
         enddo
         goto 20
      elseif(str.eq."-2-4xIxI-2-4") then
         include "leshouches_R_291.inc"
         iflow=nint(jamp2291(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2291(i)
         enddo
         goto 20
      elseif(str.eq."-24xIxI-13") then
         include "leshouches_R_292.inc"
         iflow=nint(jamp2292(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2292(i)
         enddo
         goto 20
      elseif(str.eq."-24xIxI-24") then
         include "leshouches_R_293.inc"
         iflow=nint(jamp2293(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2293(i)
         enddo
         goto 20
      elseif(str.eq."-2-3xIxI-1-4") then
         include "leshouches_R_294.inc"
         iflow=nint(jamp2294(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2294(i)
         enddo
         goto 20
      elseif(str.eq."-2-3xIxI-2-3") then
         include "leshouches_R_295.inc"
         iflow=nint(jamp2295(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2295(i)
         enddo
         goto 20
      elseif(str.eq."-23xIxI-23") then
         include "leshouches_R_296.inc"
         iflow=nint(jamp2296(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2296(i)
         enddo
         goto 20
      elseif(str.eq."-2-5xIxI-1-6") then
         include "leshouches_R_297.inc"
         iflow=nint(jamp2297(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2297(i)
         enddo
         goto 20
      elseif(str.eq."-2-5xIxI-2-5") then
         include "leshouches_R_298.inc"
         iflow=nint(jamp2298(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2298(i)
         enddo
         goto 20
      elseif(str.eq."-25xIxI-25") then
         include "leshouches_R_299.inc"
         iflow=nint(jamp2299(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2299(i)
         enddo
         goto 20
      elseif(str.eq."-20xIxI-20") then
         include "leshouches_R_300.inc"
         iflow=nint(jamp2300(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2300(i)
         enddo
         goto 20
      elseif(str.eq."2-1xIxI-12") then
         include "leshouches_R_301.inc"
         iflow=nint(jamp2301(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2301(i)
         enddo
         goto 20
      elseif(str.eq."2-1xIxI4-3") then
         include "leshouches_R_302.inc"
         iflow=nint(jamp2302(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2302(i)
         enddo
         goto 20
      elseif(str.eq."2-1xIxI-56") then
         include "leshouches_R_303.inc"
         iflow=nint(jamp2303(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2303(i)
         enddo
         goto 20
      elseif(str.eq."21xIxI12") then
         include "leshouches_R_304.inc"
         iflow=nint(jamp2304(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2304(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI1-1") then
         include "leshouches_R_305.inc"
         iflow=nint(jamp2305(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2305(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI2-2") then
         include "leshouches_R_306.inc"
         iflow=nint(jamp2306(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2306(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI4-4") then
         include "leshouches_R_307.inc"
         iflow=nint(jamp2307(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2307(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI3-3") then
         include "leshouches_R_308.inc"
         iflow=nint(jamp2308(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2308(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI5-5") then
         include "leshouches_R_309.inc"
         iflow=nint(jamp2309(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2309(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI6-6") then
         include "leshouches_R_310.inc"
         iflow=nint(jamp2310(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2310(i)
         enddo
         goto 20
      elseif(str.eq."2-2xIxI00") then
         include "leshouches_R_311.inc"
         iflow=nint(jamp2311(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2311(i)
         enddo
         goto 20
      elseif(str.eq."22xIxI22") then
         include "leshouches_R_312.inc"
         iflow=nint(jamp2312(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2312(i)
         enddo
         goto 20
      elseif(str.eq."2-4xIxI1-3") then
         include "leshouches_R_313.inc"
         iflow=nint(jamp2313(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2313(i)
         enddo
         goto 20
      elseif(str.eq."2-4xIxI2-4") then
         include "leshouches_R_314.inc"
         iflow=nint(jamp2314(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2314(i)
         enddo
         goto 20
      elseif(str.eq."24xIxI24") then
         include "leshouches_R_315.inc"
         iflow=nint(jamp2315(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2315(i)
         enddo
         goto 20
      elseif(str.eq."2-3xIxI2-3") then
         include "leshouches_R_316.inc"
         iflow=nint(jamp2316(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2316(i)
         enddo
         goto 20
      elseif(str.eq."23xIxI14") then
         include "leshouches_R_317.inc"
         iflow=nint(jamp2317(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2317(i)
         enddo
         goto 20
      elseif(str.eq."23xIxI23") then
         include "leshouches_R_318.inc"
         iflow=nint(jamp2318(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2318(i)
         enddo
         goto 20
      elseif(str.eq."2-5xIxI2-5") then
         include "leshouches_R_319.inc"
         iflow=nint(jamp2319(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2319(i)
         enddo
         goto 20
      elseif(str.eq."25xIxI16") then
         include "leshouches_R_320.inc"
         iflow=nint(jamp2320(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2320(i)
         enddo
         goto 20
      elseif(str.eq."25xIxI25") then
         include "leshouches_R_321.inc"
         iflow=nint(jamp2321(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2321(i)
         enddo
         goto 20
      elseif(str.eq."20xIxI20") then
         include "leshouches_R_322.inc"
         iflow=nint(jamp2322(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2322(i)
         enddo
         goto 20
      elseif(str.eq."-4-1xIxI-1-4") then
         include "leshouches_R_323.inc"
         iflow=nint(jamp2323(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2323(i)
         enddo
         goto 20
      elseif(str.eq."-4-1xIxI-2-3") then
         include "leshouches_R_324.inc"
         iflow=nint(jamp2324(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2324(i)
         enddo
         goto 20
      elseif(str.eq."-41xIxI1-4") then
         include "leshouches_R_325.inc"
         iflow=nint(jamp2325(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2325(i)
         enddo
         goto 20
      elseif(str.eq."-4-2xIxI-2-4") then
         include "leshouches_R_326.inc"
         iflow=nint(jamp2326(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2326(i)
         enddo
         goto 20
      elseif(str.eq."-42xIxI1-3") then
         include "leshouches_R_327.inc"
         iflow=nint(jamp2327(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2327(i)
         enddo
         goto 20
      elseif(str.eq."-42xIxI2-4") then
         include "leshouches_R_328.inc"
         iflow=nint(jamp2328(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2328(i)
         enddo
         goto 20
      elseif(str.eq."-4-4xIxI-4-4") then
         include "leshouches_R_329.inc"
         iflow=nint(jamp2329(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2329(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI1-1") then
         include "leshouches_R_330.inc"
         iflow=nint(jamp2330(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2330(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI2-2") then
         include "leshouches_R_331.inc"
         iflow=nint(jamp2331(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2331(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI4-4") then
         include "leshouches_R_332.inc"
         iflow=nint(jamp2332(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2332(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI3-3") then
         include "leshouches_R_333.inc"
         iflow=nint(jamp2333(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2333(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI5-5") then
         include "leshouches_R_334.inc"
         iflow=nint(jamp2334(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2334(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI6-6") then
         include "leshouches_R_335.inc"
         iflow=nint(jamp2335(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2335(i)
         enddo
         goto 20
      elseif(str.eq."-44xIxI00") then
         include "leshouches_R_336.inc"
         iflow=nint(jamp2336(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2336(i)
         enddo
         goto 20
      elseif(str.eq."-4-3xIxI-4-3") then
         include "leshouches_R_337.inc"
         iflow=nint(jamp2337(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2337(i)
         enddo
         goto 20
      elseif(str.eq."-43xIxI1-2") then
         include "leshouches_R_338.inc"
         iflow=nint(jamp2338(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2338(i)
         enddo
         goto 20
      elseif(str.eq."-43xIxI-43") then
         include "leshouches_R_339.inc"
         iflow=nint(jamp2339(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2339(i)
         enddo
         goto 20
      elseif(str.eq."-43xIxI5-6") then
         include "leshouches_R_340.inc"
         iflow=nint(jamp2340(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2340(i)
         enddo
         goto 20
      elseif(str.eq."-4-5xIxI-4-5") then
         include "leshouches_R_341.inc"
         iflow=nint(jamp2341(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2341(i)
         enddo
         goto 20
      elseif(str.eq."-4-5xIxI-3-6") then
         include "leshouches_R_342.inc"
         iflow=nint(jamp2342(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2342(i)
         enddo
         goto 20
      elseif(str.eq."-45xIxI-45") then
         include "leshouches_R_343.inc"
         iflow=nint(jamp2343(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2343(i)
         enddo
         goto 20
      elseif(str.eq."-40xIxI-40") then
         include "leshouches_R_344.inc"
         iflow=nint(jamp2344(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2344(i)
         enddo
         goto 20
      elseif(str.eq."4-1xIxI-14") then
         include "leshouches_R_345.inc"
         iflow=nint(jamp2345(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2345(i)
         enddo
         goto 20
      elseif(str.eq."41xIxI14") then
         include "leshouches_R_346.inc"
         iflow=nint(jamp2346(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2346(i)
         enddo
         goto 20
      elseif(str.eq."41xIxI23") then
         include "leshouches_R_347.inc"
         iflow=nint(jamp2347(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2347(i)
         enddo
         goto 20
      elseif(str.eq."4-2xIxI-13") then
         include "leshouches_R_348.inc"
         iflow=nint(jamp2348(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2348(i)
         enddo
         goto 20
      elseif(str.eq."4-2xIxI-24") then
         include "leshouches_R_349.inc"
         iflow=nint(jamp2349(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2349(i)
         enddo
         goto 20
      elseif(str.eq."42xIxI24") then
         include "leshouches_R_350.inc"
         iflow=nint(jamp2350(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2350(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI1-1") then
         include "leshouches_R_351.inc"
         iflow=nint(jamp2351(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2351(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI2-2") then
         include "leshouches_R_352.inc"
         iflow=nint(jamp2352(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2352(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI4-4") then
         include "leshouches_R_353.inc"
         iflow=nint(jamp2353(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2353(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI3-3") then
         include "leshouches_R_354.inc"
         iflow=nint(jamp2354(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2354(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI5-5") then
         include "leshouches_R_355.inc"
         iflow=nint(jamp2355(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2355(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI6-6") then
         include "leshouches_R_356.inc"
         iflow=nint(jamp2356(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2356(i)
         enddo
         goto 20
      elseif(str.eq."4-4xIxI00") then
         include "leshouches_R_357.inc"
         iflow=nint(jamp2357(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2357(i)
         enddo
         goto 20
      elseif(str.eq."44xIxI44") then
         include "leshouches_R_358.inc"
         iflow=nint(jamp2358(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2358(i)
         enddo
         goto 20
      elseif(str.eq."4-3xIxI-12") then
         include "leshouches_R_359.inc"
         iflow=nint(jamp2359(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2359(i)
         enddo
         goto 20
      elseif(str.eq."4-3xIxI4-3") then
         include "leshouches_R_360.inc"
         iflow=nint(jamp2360(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2360(i)
         enddo
         goto 20
      elseif(str.eq."4-3xIxI-56") then
         include "leshouches_R_361.inc"
         iflow=nint(jamp2361(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2361(i)
         enddo
         goto 20
      elseif(str.eq."43xIxI43") then
         include "leshouches_R_362.inc"
         iflow=nint(jamp2362(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2362(i)
         enddo
         goto 20
      elseif(str.eq."4-5xIxI4-5") then
         include "leshouches_R_363.inc"
         iflow=nint(jamp2363(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2363(i)
         enddo
         goto 20
      elseif(str.eq."45xIxI45") then
         include "leshouches_R_364.inc"
         iflow=nint(jamp2364(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2364(i)
         enddo
         goto 20
      elseif(str.eq."45xIxI36") then
         include "leshouches_R_365.inc"
         iflow=nint(jamp2365(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2365(i)
         enddo
         goto 20
      elseif(str.eq."40xIxI40") then
         include "leshouches_R_366.inc"
         iflow=nint(jamp2366(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2366(i)
         enddo
         goto 20
      elseif(str.eq."-3-1xIxI-1-3") then
         include "leshouches_R_367.inc"
         iflow=nint(jamp2367(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2367(i)
         enddo
         goto 20
      elseif(str.eq."-31xIxI1-3") then
         include "leshouches_R_368.inc"
         iflow=nint(jamp2368(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2368(i)
         enddo
         goto 20
      elseif(str.eq."-31xIxI2-4") then
         include "leshouches_R_369.inc"
         iflow=nint(jamp2369(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2369(i)
         enddo
         goto 20
      elseif(str.eq."-3-2xIxI-1-4") then
         include "leshouches_R_370.inc"
         iflow=nint(jamp2370(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2370(i)
         enddo
         goto 20
      elseif(str.eq."-3-2xIxI-2-3") then
         include "leshouches_R_371.inc"
         iflow=nint(jamp2371(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2371(i)
         enddo
         goto 20
      elseif(str.eq."-32xIxI2-3") then
         include "leshouches_R_372.inc"
         iflow=nint(jamp2372(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2372(i)
         enddo
         goto 20
      elseif(str.eq."-3-4xIxI-4-3") then
         include "leshouches_R_373.inc"
         iflow=nint(jamp2373(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2373(i)
         enddo
         goto 20
      elseif(str.eq."-34xIxI-12") then
         include "leshouches_R_374.inc"
         iflow=nint(jamp2374(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2374(i)
         enddo
         goto 20
      elseif(str.eq."-34xIxI4-3") then
         include "leshouches_R_375.inc"
         iflow=nint(jamp2375(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2375(i)
         enddo
         goto 20
      elseif(str.eq."-34xIxI-56") then
         include "leshouches_R_376.inc"
         iflow=nint(jamp2376(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2376(i)
         enddo
         goto 20
      elseif(str.eq."-3-3xIxI-3-3") then
         include "leshouches_R_377.inc"
         iflow=nint(jamp2377(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2377(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI1-1") then
         include "leshouches_R_378.inc"
         iflow=nint(jamp2378(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2378(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI2-2") then
         include "leshouches_R_379.inc"
         iflow=nint(jamp2379(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2379(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI4-4") then
         include "leshouches_R_380.inc"
         iflow=nint(jamp2380(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2380(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI3-3") then
         include "leshouches_R_381.inc"
         iflow=nint(jamp2381(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2381(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI5-5") then
         include "leshouches_R_382.inc"
         iflow=nint(jamp2382(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2382(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI6-6") then
         include "leshouches_R_383.inc"
         iflow=nint(jamp2383(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2383(i)
         enddo
         goto 20
      elseif(str.eq."-33xIxI00") then
         include "leshouches_R_384.inc"
         iflow=nint(jamp2384(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2384(i)
         enddo
         goto 20
      elseif(str.eq."-3-5xIxI-3-5") then
         include "leshouches_R_385.inc"
         iflow=nint(jamp2385(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2385(i)
         enddo
         goto 20
      elseif(str.eq."-35xIxI-46") then
         include "leshouches_R_386.inc"
         iflow=nint(jamp2386(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2386(i)
         enddo
         goto 20
      elseif(str.eq."-35xIxI-35") then
         include "leshouches_R_387.inc"
         iflow=nint(jamp2387(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2387(i)
         enddo
         goto 20
      elseif(str.eq."-30xIxI-30") then
         include "leshouches_R_388.inc"
         iflow=nint(jamp2388(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2388(i)
         enddo
         goto 20
      elseif(str.eq."3-1xIxI-13") then
         include "leshouches_R_389.inc"
         iflow=nint(jamp2389(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2389(i)
         enddo
         goto 20
      elseif(str.eq."3-1xIxI-24") then
         include "leshouches_R_390.inc"
         iflow=nint(jamp2390(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2390(i)
         enddo
         goto 20
      elseif(str.eq."31xIxI13") then
         include "leshouches_R_391.inc"
         iflow=nint(jamp2391(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2391(i)
         enddo
         goto 20
      elseif(str.eq."3-2xIxI-23") then
         include "leshouches_R_392.inc"
         iflow=nint(jamp2392(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2392(i)
         enddo
         goto 20
      elseif(str.eq."32xIxI14") then
         include "leshouches_R_393.inc"
         iflow=nint(jamp2393(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2393(i)
         enddo
         goto 20
      elseif(str.eq."32xIxI23") then
         include "leshouches_R_394.inc"
         iflow=nint(jamp2394(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2394(i)
         enddo
         goto 20
      elseif(str.eq."3-4xIxI1-2") then
         include "leshouches_R_395.inc"
         iflow=nint(jamp2395(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2395(i)
         enddo
         goto 20
      elseif(str.eq."3-4xIxI-43") then
         include "leshouches_R_396.inc"
         iflow=nint(jamp2396(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2396(i)
         enddo
         goto 20
      elseif(str.eq."3-4xIxI5-6") then
         include "leshouches_R_397.inc"
         iflow=nint(jamp2397(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2397(i)
         enddo
         goto 20
      elseif(str.eq."34xIxI43") then
         include "leshouches_R_398.inc"
         iflow=nint(jamp2398(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2398(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI1-1") then
         include "leshouches_R_399.inc"
         iflow=nint(jamp2399(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2399(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI2-2") then
         include "leshouches_R_400.inc"
         iflow=nint(jamp2400(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2400(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI4-4") then
         include "leshouches_R_401.inc"
         iflow=nint(jamp2401(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2401(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI3-3") then
         include "leshouches_R_402.inc"
         iflow=nint(jamp2402(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2402(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI5-5") then
         include "leshouches_R_403.inc"
         iflow=nint(jamp2403(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2403(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI6-6") then
         include "leshouches_R_404.inc"
         iflow=nint(jamp2404(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2404(i)
         enddo
         goto 20
      elseif(str.eq."3-3xIxI00") then
         include "leshouches_R_405.inc"
         iflow=nint(jamp2405(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2405(i)
         enddo
         goto 20
      elseif(str.eq."33xIxI33") then
         include "leshouches_R_406.inc"
         iflow=nint(jamp2406(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2406(i)
         enddo
         goto 20
      elseif(str.eq."3-5xIxI4-6") then
         include "leshouches_R_407.inc"
         iflow=nint(jamp2407(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2407(i)
         enddo
         goto 20
      elseif(str.eq."3-5xIxI3-5") then
         include "leshouches_R_408.inc"
         iflow=nint(jamp2408(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2408(i)
         enddo
         goto 20
      elseif(str.eq."35xIxI35") then
         include "leshouches_R_409.inc"
         iflow=nint(jamp2409(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2409(i)
         enddo
         goto 20
      elseif(str.eq."30xIxI30") then
         include "leshouches_R_410.inc"
         iflow=nint(jamp2410(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2410(i)
         enddo
         goto 20
      elseif(str.eq."-5-1xIxI-1-5") then
         include "leshouches_R_411.inc"
         iflow=nint(jamp2411(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2411(i)
         enddo
         goto 20
      elseif(str.eq."-51xIxI1-5") then
         include "leshouches_R_412.inc"
         iflow=nint(jamp2412(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2412(i)
         enddo
         goto 20
      elseif(str.eq."-51xIxI2-6") then
         include "leshouches_R_413.inc"
         iflow=nint(jamp2413(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2413(i)
         enddo
         goto 20
      elseif(str.eq."-5-2xIxI-1-6") then
         include "leshouches_R_414.inc"
         iflow=nint(jamp2414(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2414(i)
         enddo
         goto 20
      elseif(str.eq."-5-2xIxI-2-5") then
         include "leshouches_R_415.inc"
         iflow=nint(jamp2415(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2415(i)
         enddo
         goto 20
      elseif(str.eq."-52xIxI2-5") then
         include "leshouches_R_416.inc"
         iflow=nint(jamp2416(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2416(i)
         enddo
         goto 20
      elseif(str.eq."-5-4xIxI-4-5") then
         include "leshouches_R_417.inc"
         iflow=nint(jamp2417(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2417(i)
         enddo
         goto 20
      elseif(str.eq."-5-4xIxI-3-6") then
         include "leshouches_R_418.inc"
         iflow=nint(jamp2418(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2418(i)
         enddo
         goto 20
      elseif(str.eq."-54xIxI4-5") then
         include "leshouches_R_419.inc"
         iflow=nint(jamp2419(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2419(i)
         enddo
         goto 20
      elseif(str.eq."-5-3xIxI-3-5") then
         include "leshouches_R_420.inc"
         iflow=nint(jamp2420(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2420(i)
         enddo
         goto 20
      elseif(str.eq."-53xIxI4-6") then
         include "leshouches_R_421.inc"
         iflow=nint(jamp2421(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2421(i)
         enddo
         goto 20
      elseif(str.eq."-53xIxI3-5") then
         include "leshouches_R_422.inc"
         iflow=nint(jamp2422(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2422(i)
         enddo
         goto 20
      elseif(str.eq."-5-5xIxI-5-5") then
         include "leshouches_R_423.inc"
         iflow=nint(jamp2423(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2423(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI1-1") then
         include "leshouches_R_424.inc"
         iflow=nint(jamp2424(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2424(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI2-2") then
         include "leshouches_R_425.inc"
         iflow=nint(jamp2425(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2425(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI4-4") then
         include "leshouches_R_426.inc"
         iflow=nint(jamp2426(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2426(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI3-3") then
         include "leshouches_R_427.inc"
         iflow=nint(jamp2427(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2427(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI5-5") then
         include "leshouches_R_428.inc"
         iflow=nint(jamp2428(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2428(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI6-6") then
         include "leshouches_R_429.inc"
         iflow=nint(jamp2429(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2429(i)
         enddo
         goto 20
      elseif(str.eq."-55xIxI00") then
         include "leshouches_R_430.inc"
         iflow=nint(jamp2430(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2430(i)
         enddo
         goto 20
      elseif(str.eq."-50xIxI-50") then
         include "leshouches_R_431.inc"
         iflow=nint(jamp2431(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2431(i)
         enddo
         goto 20
      elseif(str.eq."5-1xIxI-15") then
         include "leshouches_R_432.inc"
         iflow=nint(jamp2432(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2432(i)
         enddo
         goto 20
      elseif(str.eq."5-1xIxI-26") then
         include "leshouches_R_433.inc"
         iflow=nint(jamp2433(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2433(i)
         enddo
         goto 20
      elseif(str.eq."51xIxI15") then
         include "leshouches_R_434.inc"
         iflow=nint(jamp2434(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2434(i)
         enddo
         goto 20
      elseif(str.eq."5-2xIxI-25") then
         include "leshouches_R_435.inc"
         iflow=nint(jamp2435(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2435(i)
         enddo
         goto 20
      elseif(str.eq."52xIxI16") then
         include "leshouches_R_436.inc"
         iflow=nint(jamp2436(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2436(i)
         enddo
         goto 20
      elseif(str.eq."52xIxI25") then
         include "leshouches_R_437.inc"
         iflow=nint(jamp2437(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2437(i)
         enddo
         goto 20
      elseif(str.eq."5-4xIxI-45") then
         include "leshouches_R_438.inc"
         iflow=nint(jamp2438(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2438(i)
         enddo
         goto 20
      elseif(str.eq."54xIxI45") then
         include "leshouches_R_439.inc"
         iflow=nint(jamp2439(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2439(i)
         enddo
         goto 20
      elseif(str.eq."54xIxI36") then
         include "leshouches_R_440.inc"
         iflow=nint(jamp2440(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2440(i)
         enddo
         goto 20
      elseif(str.eq."5-3xIxI-46") then
         include "leshouches_R_441.inc"
         iflow=nint(jamp2441(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2441(i)
         enddo
         goto 20
      elseif(str.eq."5-3xIxI-35") then
         include "leshouches_R_442.inc"
         iflow=nint(jamp2442(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2442(i)
         enddo
         goto 20
      elseif(str.eq."53xIxI35") then
         include "leshouches_R_443.inc"
         iflow=nint(jamp2443(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2443(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI1-1") then
         include "leshouches_R_444.inc"
         iflow=nint(jamp2444(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2444(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI2-2") then
         include "leshouches_R_445.inc"
         iflow=nint(jamp2445(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2445(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI4-4") then
         include "leshouches_R_446.inc"
         iflow=nint(jamp2446(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2446(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI3-3") then
         include "leshouches_R_447.inc"
         iflow=nint(jamp2447(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2447(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI5-5") then
         include "leshouches_R_448.inc"
         iflow=nint(jamp2448(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2448(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI6-6") then
         include "leshouches_R_449.inc"
         iflow=nint(jamp2449(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2449(i)
         enddo
         goto 20
      elseif(str.eq."5-5xIxI00") then
         include "leshouches_R_450.inc"
         iflow=nint(jamp2450(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2450(i)
         enddo
         goto 20
      elseif(str.eq."55xIxI55") then
         include "leshouches_R_451.inc"
         iflow=nint(jamp2451(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2451(i)
         enddo
         goto 20
      elseif(str.eq."50xIxI50") then
         include "leshouches_R_452.inc"
         iflow=nint(jamp2452(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2452(i)
         enddo
         goto 20
      elseif(str.eq."0-1xIxI-10") then
         include "leshouches_R_453.inc"
         iflow=nint(jamp2453(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2453(i)
         enddo
         goto 20
      elseif(str.eq."01xIxI10") then
         include "leshouches_R_454.inc"
         iflow=nint(jamp2454(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2454(i)
         enddo
         goto 20
      elseif(str.eq."0-2xIxI-20") then
         include "leshouches_R_455.inc"
         iflow=nint(jamp2455(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2455(i)
         enddo
         goto 20
      elseif(str.eq."02xIxI20") then
         include "leshouches_R_456.inc"
         iflow=nint(jamp2456(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2456(i)
         enddo
         goto 20
      elseif(str.eq."0-4xIxI-40") then
         include "leshouches_R_457.inc"
         iflow=nint(jamp2457(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2457(i)
         enddo
         goto 20
      elseif(str.eq."04xIxI40") then
         include "leshouches_R_458.inc"
         iflow=nint(jamp2458(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2458(i)
         enddo
         goto 20
      elseif(str.eq."0-3xIxI-30") then
         include "leshouches_R_459.inc"
         iflow=nint(jamp2459(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2459(i)
         enddo
         goto 20
      elseif(str.eq."03xIxI30") then
         include "leshouches_R_460.inc"
         iflow=nint(jamp2460(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2460(i)
         enddo
         goto 20
      elseif(str.eq."0-5xIxI-50") then
         include "leshouches_R_461.inc"
         iflow=nint(jamp2461(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2461(i)
         enddo
         goto 20
      elseif(str.eq."05xIxI50") then
         include "leshouches_R_462.inc"
         iflow=nint(jamp2462(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2462(i)
         enddo
         goto 20
      elseif(str.eq."00xIxI1-1") then
         include "leshouches_R_463.inc"
         iflow=nint(jamp2463(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2463(i)
         enddo
         goto 20
      elseif(str.eq."00xIxI2-2") then
         include "leshouches_R_464.inc"
         iflow=nint(jamp2464(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2464(i)
         enddo
         goto 20
      elseif(str.eq."00xIxI4-4") then
         include "leshouches_R_465.inc"
         iflow=nint(jamp2465(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2465(i)
         enddo
         goto 20
      elseif(str.eq."00xIxI3-3") then
         include "leshouches_R_466.inc"
         iflow=nint(jamp2466(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2466(i)
         enddo
         goto 20
      elseif(str.eq."00xIxI5-5") then
         include "leshouches_R_467.inc"
         iflow=nint(jamp2467(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2467(i)
         enddo
         goto 20
      elseif(str.eq."00xIxI6-6") then
         include "leshouches_R_468.inc"
         iflow=nint(jamp2468(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2468(i)
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
      
      
      
      
