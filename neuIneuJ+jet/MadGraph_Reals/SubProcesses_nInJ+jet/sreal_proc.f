      subroutine sreal_proc(p,legs,wgt)
      implicit none
      include "nexternal.inc"
      include "coupl.inc"
      double precision p(0:3,nexternal),wgt
      character*20 str
      double precision P1(0:3,nexternal)
!       double precision P2(0:3,nexternal)
!       double precision P3(0:3,nexternal)
!       double precision P4(0:3,nexternal)
      integer legs(nexternal),lstr
      integer i,k,ic(nexternal),legs1(nexternal)
      logical mtc,even
      
      wgt = 0
      
!       do i=1,nexternal
!          ic(i)=i
!       enddo
!       mtc=.false.
!  10   call nexper(nexternal- 4,ic( 4+1),mtc,even)
!       do i= 4+1,nexternal
!          ic(i)=ic(i)+ 4
!       enddo
!       call switchmom(p,p1,ic,nexternal)
!       CALL SWITCHLEGS(legs,legs1,IC,NEXTERNAL)
      
      
      do k=1,nexternal
        do i=0,3
          p1(i,k) = p(i,k)
        enddo
        legs1(k) = legs(k)
      enddo
      
      ! crossing
      ! is ugly and not fast, but does not matter. MadGraph amplitudes 
      ! are going to be removed sooner or later
      do k=1,4
      if(k.eq.1) then
        ! change nothing
      elseif(k.eq.2) then
        do i=0,3
        p1(i,5) = p(i,6)
        p1(i,6) = p(i,5)
        enddo
        legs1(5) = legs(6)
        legs1(6) = legs(5)
      
      elseif(k.eq.3) then
        do i=0,3
        p1(i,1) = p(i,2)
        p1(i,2) = p(i,1)
        p1(i,5) = p(i,6)
        p1(i,6) = p(i,5)
        enddo
        legs1(1) = legs(2)
        legs1(2) = legs(1)
        legs1(5) = legs(6)
        legs1(6) = legs(5)
      
      elseif(k.eq.4) then
        do i=0,3
        p1(i,1) = p(i,2)
        p1(i,2) = p(i,1)
        enddo
        legs1(1) = legs(2)
        legs1(2) = legs(1)
      endif
      
      call convert_to_string(nexternal,legs1(:),str,lstr)
#ifdef DEBUGQ
      print*, "[DEBUG] legs = ",legs
      print*, "[DEBUG] legs1 = ",legs1
      print*, "[DEBUG] str = ", str
#endif
      
      
      if(str.eq."-1-1nInJ-1-1") then
         call smatrix_dxdx_nInJdxdx(p1,wgt)
         goto 20
      elseif(str.eq."-11nInJ1-1") then
         call smatrix_dxd_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."-11nInJ2-2") then
         call smatrix_dxd_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."-11nInJ4-4") then
         call smatrix_dxd_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."-11nInJ3-3") then
         call smatrix_dxd_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."-11nInJ5-5") then
         call smatrix_dxd_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-11nInJ00") then
         call smatrix_dxd_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."-1-2nInJ-1-2") then
         call smatrix_dxux_nInJdxux(p1,wgt)
         goto 20
      elseif(str.eq."-12nInJ-12") then
         call smatrix_dxu_nInJdxu(p1,wgt)
         goto 20
      elseif(str.eq."-1-4nInJ-1-4") then
         call smatrix_dxcx_nInJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-14nInJ-14") then
         call smatrix_dxc_nInJdxc(p1,wgt)
         goto 20
      elseif(str.eq."-1-3nInJ-1-3") then
         call smatrix_dxsx_nInJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-13nInJ-13") then
         call smatrix_dxs_nInJdxs(p1,wgt)
         goto 20
      elseif(str.eq."-1-5nInJ-1-5") then
         call smatrix_dxbx_nInJdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-15nInJ-15") then
         call smatrix_dxb_nInJdxb(p1,wgt)
         goto 20
      elseif(str.eq."-10nInJ-10") then
         call smatrix_dxg_nInJdxg(p1,wgt)
         goto 20
      elseif(str.eq."1-1nInJ1-1") then
         call smatrix_ddx_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."1-1nInJ2-2") then
         call smatrix_ddx_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."1-1nInJ4-4") then
         call smatrix_ddx_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."1-1nInJ3-3") then
         call smatrix_ddx_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."1-1nInJ5-5") then
         call smatrix_ddx_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."1-1nInJ00") then
         call smatrix_ddx_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."11nInJ11") then
         call smatrix_dd_nInJdd(p1,wgt)
         goto 20
      elseif(str.eq."1-2nInJ1-2") then
         call smatrix_dux_nInJdux(p1,wgt)
         goto 20
      elseif(str.eq."12nInJ12") then
         call smatrix_du_nInJdu(p1,wgt)
         goto 20
      elseif(str.eq."1-4nInJ1-4") then
         call smatrix_dcx_nInJdcx(p1,wgt)
         goto 20
      elseif(str.eq."14nInJ14") then
         call smatrix_dc_nInJdc(p1,wgt)
         goto 20
      elseif(str.eq."1-3nInJ1-3") then
         call smatrix_dsx_nInJdsx(p1,wgt)
         goto 20
      elseif(str.eq."13nInJ13") then
         call smatrix_ds_nInJds(p1,wgt)
         goto 20
      elseif(str.eq."1-5nInJ1-5") then
         call smatrix_dbx_nInJdbx(p1,wgt)
         goto 20
      elseif(str.eq."15nInJ15") then
         call smatrix_db_nInJdb(p1,wgt)
         goto 20
      elseif(str.eq."10nInJ10") then
         call smatrix_dg_nInJdg(p1,wgt)
         goto 20
      elseif(str.eq."-2-1nInJ-1-2") then
         call smatrix_uxdx_nInJdxux(p1,wgt)
         goto 20
      elseif(str.eq."-21nInJ1-2") then
         call smatrix_uxd_nInJdux(p1,wgt)
         goto 20
      elseif(str.eq."-2-2nInJ-2-2") then
         call smatrix_uxux_nInJuxux(p1,wgt)
         goto 20
      elseif(str.eq."-22nInJ1-1") then
         call smatrix_uxu_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."-22nInJ2-2") then
         call smatrix_uxu_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."-22nInJ4-4") then
         call smatrix_uxu_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."-22nInJ3-3") then
         call smatrix_uxu_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."-22nInJ5-5") then
         call smatrix_uxu_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-22nInJ00") then
         call smatrix_uxu_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."-2-4nInJ-2-4") then
         call smatrix_uxcx_nInJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-24nInJ-24") then
         call smatrix_uxc_nInJuxc(p1,wgt)
         goto 20
      elseif(str.eq."-2-3nInJ-2-3") then
         call smatrix_uxsx_nInJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-23nInJ-23") then
         call smatrix_uxs_nInJuxs(p1,wgt)
         goto 20
      elseif(str.eq."-2-5nInJ-2-5") then
         call smatrix_uxbx_nInJuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-25nInJ-25") then
         call smatrix_uxb_nInJuxb(p1,wgt)
         goto 20
      elseif(str.eq."-20nInJ-20") then
         call smatrix_uxg_nInJuxg(p1,wgt)
         goto 20
      elseif(str.eq."2-1nInJ-12") then
         call smatrix_udx_nInJdxu(p1,wgt)
         goto 20
      elseif(str.eq."21nInJ12") then
         call smatrix_ud_nInJdu(p1,wgt)
         goto 20
      elseif(str.eq."2-2nInJ1-1") then
         call smatrix_uux_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."2-2nInJ2-2") then
         call smatrix_uux_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."2-2nInJ4-4") then
         call smatrix_uux_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."2-2nInJ3-3") then
         call smatrix_uux_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."2-2nInJ5-5") then
         call smatrix_uux_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."2-2nInJ00") then
         call smatrix_uux_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."22nInJ22") then
         call smatrix_uu_nInJuu(p1,wgt)
         goto 20
      elseif(str.eq."2-4nInJ2-4") then
         call smatrix_ucx_nInJucx(p1,wgt)
         goto 20
      elseif(str.eq."24nInJ24") then
         call smatrix_uc_nInJuc(p1,wgt)
         goto 20
      elseif(str.eq."2-3nInJ2-3") then
         call smatrix_usx_nInJusx(p1,wgt)
         goto 20
      elseif(str.eq."23nInJ23") then
         call smatrix_us_nInJus(p1,wgt)
         goto 20
      elseif(str.eq."2-5nInJ2-5") then
         call smatrix_ubx_nInJubx(p1,wgt)
         goto 20
      elseif(str.eq."25nInJ25") then
         call smatrix_ub_nInJub(p1,wgt)
         goto 20
      elseif(str.eq."20nInJ20") then
         call smatrix_ug_nInJug(p1,wgt)
         goto 20
      elseif(str.eq."-4-1nInJ-1-4") then
         call smatrix_cxdx_nInJdxcx(p1,wgt)
         goto 20
      elseif(str.eq."-41nInJ1-4") then
         call smatrix_cxd_nInJdcx(p1,wgt)
         goto 20
      elseif(str.eq."-4-2nInJ-2-4") then
         call smatrix_cxux_nInJuxcx(p1,wgt)
         goto 20
      elseif(str.eq."-42nInJ2-4") then
         call smatrix_cxu_nInJucx(p1,wgt)
         goto 20
      elseif(str.eq."-4-4nInJ-4-4") then
         call smatrix_cxcx_nInJcxcx(p1,wgt)
         goto 20
      elseif(str.eq."-44nInJ1-1") then
         call smatrix_cxc_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."-44nInJ2-2") then
         call smatrix_cxc_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."-44nInJ4-4") then
         call smatrix_cxc_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."-44nInJ3-3") then
         call smatrix_cxc_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."-44nInJ5-5") then
         call smatrix_cxc_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-44nInJ00") then
         call smatrix_cxc_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."-4-3nInJ-4-3") then
         call smatrix_cxsx_nInJcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-43nInJ-43") then
         call smatrix_cxs_nInJcxs(p1,wgt)
         goto 20
      elseif(str.eq."3-4nInJ-43") then
         call smatrix_scx_nInJcxs(p1,wgt)
         goto 20
      elseif(str.eq."-4-5nInJ-4-5") then
         call smatrix_cxbx_nInJcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-45nInJ-45") then
         call smatrix_cxb_nInJcxb(p1,wgt)
         goto 20
      elseif(str.eq."-40nInJ-40") then
         call smatrix_cxg_nInJcxg(p1,wgt)
         goto 20
      elseif(str.eq."4-1nInJ-14") then
         call smatrix_cdx_nInJdxc(p1,wgt)
         goto 20
      elseif(str.eq."41nInJ14") then
         call smatrix_cd_nInJdc(p1,wgt)
         goto 20
      elseif(str.eq."4-2nInJ-24") then
         call smatrix_cux_nInJuxc(p1,wgt)
         goto 20
      elseif(str.eq."42nInJ24") then
         call smatrix_cu_nInJuc(p1,wgt)
         goto 20
      elseif(str.eq."4-4nInJ1-1") then
         call smatrix_ccx_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."4-4nInJ2-2") then
         call smatrix_ccx_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."4-4nInJ4-4") then
         call smatrix_ccx_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."4-4nInJ3-3") then
         call smatrix_ccx_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."4-4nInJ5-5") then
         call smatrix_ccx_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."4-4nInJ00") then
         call smatrix_ccx_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."44nInJ44") then
         call smatrix_cc_nInJcc(p1,wgt)
         goto 20
      elseif(str.eq."4-3nInJ4-3") then
         call smatrix_csx_nInJcsx(p1,wgt)
         goto 20
      elseif(str.eq."-34nInJ4-3") then
         call smatrix_sxc_nInJcsx(p1,wgt)
         goto 20
      elseif(str.eq."4-5nInJ4-5") then
         call smatrix_cbx_nInJcbx(p1,wgt)
         goto 20
      elseif(str.eq."45nInJ45") then
         call smatrix_cb_nInJcb(p1,wgt)
         goto 20
      elseif(str.eq."40nInJ40") then
         call smatrix_cg_nInJcg(p1,wgt)
         goto 20
      elseif(str.eq."-3-1nInJ-1-3") then
         call smatrix_sxdx_nInJdxsx(p1,wgt)
         goto 20
      elseif(str.eq."-31nInJ1-3") then
         call smatrix_sxd_nInJdsx(p1,wgt)
         goto 20
      elseif(str.eq."-3-2nInJ-2-3") then
         call smatrix_sxux_nInJuxsx(p1,wgt)
         goto 20
      elseif(str.eq."-32nInJ2-3") then
         call smatrix_sxu_nInJusx(p1,wgt)
         goto 20 
      elseif(str.eq."-3-4nInJ-4-3") then
         call smatrix_sxcx_nInJcxsx(p1,wgt)
         goto 20
      elseif(str.eq."-3-3nInJ-3-3") then
         call smatrix_sxsx_nInJsxsx(p1,wgt)
         goto 20
      elseif(str.eq."-33nInJ1-1") then
         call smatrix_sxs_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."-33nInJ2-2") then
         call smatrix_sxs_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."-33nInJ4-4") then
         call smatrix_sxs_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."-33nInJ3-3") then
         call smatrix_sxs_nInJssx(p1,wgt)
         goto 20 
      elseif(str.eq."-33nInJ5-5") then
         call smatrix_sxs_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-33nInJ00") then
         call smatrix_sxs_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."-3-5nInJ-3-5") then
         call smatrix_sxbx_nInJsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-35nInJ-35") then
         call smatrix_sxb_nInJsxb(p1,wgt)
         goto 20
      elseif(str.eq."-30nInJ-30") then
         call smatrix_sxg_nInJsxg(p1,wgt)
         goto 20  
      elseif(str.eq."3-1nInJ-13") then
         call smatrix_sdx_nInJdxs(p1,wgt)
         goto 20
      elseif(str.eq."31nInJ13") then
         call smatrix_sd_nInJds(p1,wgt)
         goto 20
      elseif(str.eq."3-2nInJ-23") then
         call smatrix_sux_nInJuxs(p1,wgt)
         goto 20
      elseif(str.eq."32nInJ23") then
         call smatrix_su_nInJus(p1,wgt)
         goto 20
      elseif(str.eq."43nInJ43") then
         call smatrix_cs_nInJcs(p1,wgt)
         goto 20
      elseif(str.eq."34nInJ43") then
         call smatrix_sc_nInJcs(p1,wgt)
         goto 20
      elseif(str.eq."3-3nInJ1-1") then
         call smatrix_ssx_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."3-3nInJ2-2") then
         call smatrix_ssx_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."3-3nInJ4-4") then
         call smatrix_ssx_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."3-3nInJ3-3") then
         call smatrix_ssx_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."3-3nInJ5-5") then
         call smatrix_ssx_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."3-3nInJ00") then
         call smatrix_ssx_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."33nInJ33") then
         call smatrix_ss_nInJss(p1,wgt)
         goto 20
      elseif(str.eq."3-5nInJ3-5") then
         call smatrix_sbx_nInJsbx(p1,wgt)
         goto 20
      elseif(str.eq."35nInJ35") then
         call smatrix_sb_nInJsb(p1,wgt)
         goto 20
      elseif(str.eq."30nInJ30") then
         call smatrix_sg_nInJsg(p1,wgt)
         goto 20
      elseif(str.eq."-5-1nInJ-1-5") then
         call smatrix_bxdx_nInJdxbx(p1,wgt)
         goto 20
      elseif(str.eq."-51nInJ1-5") then
         call smatrix_bxd_nInJdbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-2nInJ-2-5") then
         call smatrix_bxux_nInJuxbx(p1,wgt)
         goto 20
      elseif(str.eq."-52nInJ2-5") then
         call smatrix_bxu_nInJubx(p1,wgt)
         goto 20
      elseif(str.eq."-5-4nInJ-4-5") then
         call smatrix_bxcx_nInJcxbx(p1,wgt)
         goto 20
      elseif(str.eq."-54nInJ4-5") then
         call smatrix_bxc_nInJcbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-3nInJ-3-5") then
         call smatrix_bxsx_nInJsxbx(p1,wgt)
         goto 20
      elseif(str.eq."-53nInJ3-5") then
         call smatrix_bxs_nInJsbx(p1,wgt)
         goto 20
      elseif(str.eq."-5-5nInJ-5-5") then
         call smatrix_bxbx_nInJbxbx(p1,wgt)
         goto 20
      elseif(str.eq."-55nInJ1-1") then
         call smatrix_bxb_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."-55nInJ2-2") then
         call smatrix_bxb_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."-55nInJ4-4") then
         call smatrix_bxb_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."-55nInJ3-3") then
         call smatrix_bxb_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."-55nInJ5-5") then
         call smatrix_bxb_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."-55nInJ00") then
         call smatrix_bxb_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."-50nInJ-50") then
         call smatrix_bxg_nInJbxg(p1,wgt)
         goto 20
      elseif(str.eq."5-1nInJ-15") then
         call smatrix_bdx_nInJdxb(p1,wgt)
         goto 20
      elseif(str.eq."51nInJ15") then
         call smatrix_bd_nInJdb(p1,wgt)
         goto 20
      elseif(str.eq."5-2nInJ-25") then
         call smatrix_bux_nInJuxb(p1,wgt)
         goto 20
      elseif(str.eq."52nInJ25") then
         call smatrix_bu_nInJub(p1,wgt)
         goto 20
      elseif(str.eq."5-4nInJ-45") then
         call smatrix_bcx_nInJcxb(p1,wgt)
         goto 20
      elseif(str.eq."54nInJ45") then
         call smatrix_bc_nInJcb(p1,wgt)
         goto 20
      elseif(str.eq."5-3nInJ-35") then
         call smatrix_bsx_nInJsxb(p1,wgt)
         goto 20
      elseif(str.eq."53nInJ35") then
         call smatrix_bs_nInJsb(p1,wgt)
         goto 20
      elseif(str.eq."5-5nInJ1-1") then
         call smatrix_bbx_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."5-5nInJ2-2") then
         call smatrix_bbx_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."5-5nInJ4-4") then
         call smatrix_bbx_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."5-5nInJ3-3") then
         call smatrix_bbx_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."5-5nInJ5-5") then
         call smatrix_bbx_nInJbbx(p1,wgt)
         goto 20
      elseif(str.eq."5-5nInJ00") then
         call smatrix_bbx_nInJgg(p1,wgt)
         goto 20
      elseif(str.eq."55nInJ55") then
         call smatrix_bb_nInJbb(p1,wgt)
         goto 20
      elseif(str.eq."50nInJ50") then
         call smatrix_bg_nInJbg(p1,wgt)
         goto 20
      elseif(str.eq."0-1nInJ-10") then
         call smatrix_gdx_nInJdxg(p1,wgt)
         goto 20
      elseif(str.eq."01nInJ10") then
         call smatrix_gd_nInJdg(p1,wgt)
         goto 20
      elseif(str.eq."0-2nInJ-20") then
         call smatrix_gux_nInJuxg(p1,wgt)
         goto 20
      elseif(str.eq."02nInJ20") then
         call smatrix_gu_nInJug(p1,wgt)
         goto 20
      elseif(str.eq."0-4nInJ-40") then
         call smatrix_gcx_nInJcxg(p1,wgt)
         goto 20
      elseif(str.eq."04nInJ40") then
         call smatrix_gc_nInJcg(p1,wgt)
         goto 20
      elseif(str.eq."0-3nInJ-30") then
         call smatrix_gsx_nInJsxg(p1,wgt)
         goto 20
      elseif(str.eq."03nInJ30") then
         call smatrix_gs_nInJsg(p1,wgt)
         goto 20
      elseif(str.eq."0-5nInJ-50") then
         call smatrix_gbx_nInJbxg(p1,wgt)
         goto 20
      elseif(str.eq."05nInJ50") then
         call smatrix_gb_nInJbg(p1,wgt)
         goto 20
      elseif(str.eq."00nInJ1-1") then
         call smatrix_gg_nInJddx(p1,wgt)
         goto 20
      elseif(str.eq."00nInJ2-2") then
         call smatrix_gg_nInJuux(p1,wgt)
         goto 20
      elseif(str.eq."00nInJ4-4") then
         call smatrix_gg_nInJccx(p1,wgt)
         goto 20
      elseif(str.eq."00nInJ3-3") then
         call smatrix_gg_nInJssx(p1,wgt)
         goto 20
      elseif(str.eq."00nInJ5-5") then
         call smatrix_gg_nInJbbx(p1,wgt)
         goto 20
      endif
 20   continue
 
#ifdef DEBUGQ
      print*,"k   = ",k
      print*,"str = ", str
      print*,"wgt = ",wgt
#endif
 
      if(wgt .gt. 1D-99) goto 21
      enddo
 21   continue  
      !if(wgt .eq. 0D0) then
      !    print*, "str = ", str
      !    print*,"WARNING: maybe error in real amplitudes: ", wgt
      !    stop
      !endif
      return
      end
      
      
      subroutine real_color(legs,color)
      implicit none
      include "nexternal.inc"
      include "maxamps.inc"
      Double Precision amp2001(maxamps), jamp2001(0:maxamps)
      common/to_Ramps_001/amp2001,jamp2001
      Double Precision amp2002(maxamps), jamp2002(0:maxamps)
      common/to_Ramps_002/amp2002,jamp2002
      Double Precision amp2003(maxamps), jamp2003(0:maxamps)
      common/to_Ramps_003/amp2003,jamp2003
      Double Precision amp2004(maxamps), jamp2004(0:maxamps)
      common/to_Ramps_004/amp2004,jamp2004
      Double Precision amp2005(maxamps), jamp2005(0:maxamps)
      common/to_Ramps_005/amp2005,jamp2005
      Double Precision amp2006(maxamps), jamp2006(0:maxamps)
      common/to_Ramps_006/amp2006,jamp2006
      Double Precision amp2007(maxamps), jamp2007(0:maxamps)
      common/to_Ramps_007/amp2007,jamp2007
      Double Precision amp2008(maxamps), jamp2008(0:maxamps)
      common/to_Ramps_008/amp2008,jamp2008
      Double Precision amp2009(maxamps), jamp2009(0:maxamps)
      common/to_Ramps_009/amp2009,jamp2009
      Double Precision amp2010(maxamps), jamp2010(0:maxamps)
      common/to_Ramps_010/amp2010,jamp2010
      Double Precision amp2011(maxamps), jamp2011(0:maxamps)
      common/to_Ramps_011/amp2011,jamp2011
      Double Precision amp2012(maxamps), jamp2012(0:maxamps)
      common/to_Ramps_012/amp2012,jamp2012
      Double Precision amp2013(maxamps), jamp2013(0:maxamps)
      common/to_Ramps_013/amp2013,jamp2013
      Double Precision amp2014(maxamps), jamp2014(0:maxamps)
      common/to_Ramps_014/amp2014,jamp2014
      Double Precision amp2015(maxamps), jamp2015(0:maxamps)
      common/to_Ramps_015/amp2015,jamp2015
      Double Precision amp2016(maxamps), jamp2016(0:maxamps)
      common/to_Ramps_016/amp2016,jamp2016
      Double Precision amp2017(maxamps), jamp2017(0:maxamps)
      common/to_Ramps_017/amp2017,jamp2017
      Double Precision amp2018(maxamps), jamp2018(0:maxamps)
      common/to_Ramps_018/amp2018,jamp2018
      Double Precision amp2019(maxamps), jamp2019(0:maxamps)
      common/to_Ramps_019/amp2019,jamp2019
      Double Precision amp2020(maxamps), jamp2020(0:maxamps)
      common/to_Ramps_020/amp2020,jamp2020
      Double Precision amp2021(maxamps), jamp2021(0:maxamps)
      common/to_Ramps_021/amp2021,jamp2021
      Double Precision amp2022(maxamps), jamp2022(0:maxamps)
      common/to_Ramps_022/amp2022,jamp2022
      Double Precision amp2023(maxamps), jamp2023(0:maxamps)
      common/to_Ramps_023/amp2023,jamp2023
      Double Precision amp2024(maxamps), jamp2024(0:maxamps)
      common/to_Ramps_024/amp2024,jamp2024
      Double Precision amp2025(maxamps), jamp2025(0:maxamps)
      common/to_Ramps_025/amp2025,jamp2025
      Double Precision amp2026(maxamps), jamp2026(0:maxamps)
      common/to_Ramps_026/amp2026,jamp2026
      Double Precision amp2027(maxamps), jamp2027(0:maxamps)
      common/to_Ramps_027/amp2027,jamp2027
      Double Precision amp2028(maxamps), jamp2028(0:maxamps)
      common/to_Ramps_028/amp2028,jamp2028
      Double Precision amp2029(maxamps), jamp2029(0:maxamps)
      common/to_Ramps_029/amp2029,jamp2029
      Double Precision amp2030(maxamps), jamp2030(0:maxamps)
      common/to_Ramps_030/amp2030,jamp2030
      Double Precision amp2031(maxamps), jamp2031(0:maxamps)
      common/to_Ramps_031/amp2031,jamp2031
      Double Precision amp2032(maxamps), jamp2032(0:maxamps)
      common/to_Ramps_032/amp2032,jamp2032
      Double Precision amp2033(maxamps), jamp2033(0:maxamps)
      common/to_Ramps_033/amp2033,jamp2033
      Double Precision amp2034(maxamps), jamp2034(0:maxamps)
      common/to_Ramps_034/amp2034,jamp2034
      Double Precision amp2035(maxamps), jamp2035(0:maxamps)
      common/to_Ramps_035/amp2035,jamp2035
      Double Precision amp2036(maxamps), jamp2036(0:maxamps)
      common/to_Ramps_036/amp2036,jamp2036
      Double Precision amp2037(maxamps), jamp2037(0:maxamps)
      common/to_Ramps_037/amp2037,jamp2037
      Double Precision amp2038(maxamps), jamp2038(0:maxamps)
      common/to_Ramps_038/amp2038,jamp2038
      Double Precision amp2039(maxamps), jamp2039(0:maxamps)
      common/to_Ramps_039/amp2039,jamp2039
      Double Precision amp2040(maxamps), jamp2040(0:maxamps)
      common/to_Ramps_040/amp2040,jamp2040
      Double Precision amp2041(maxamps), jamp2041(0:maxamps)
      common/to_Ramps_041/amp2041,jamp2041
      Double Precision amp2042(maxamps), jamp2042(0:maxamps)
      common/to_Ramps_042/amp2042,jamp2042
      Double Precision amp2043(maxamps), jamp2043(0:maxamps)
      common/to_Ramps_043/amp2043,jamp2043
      Double Precision amp2044(maxamps), jamp2044(0:maxamps)
      common/to_Ramps_044/amp2044,jamp2044
      Double Precision amp2045(maxamps), jamp2045(0:maxamps)
      common/to_Ramps_045/amp2045,jamp2045
      Double Precision amp2046(maxamps), jamp2046(0:maxamps)
      common/to_Ramps_046/amp2046,jamp2046
      Double Precision amp2047(maxamps), jamp2047(0:maxamps)
      common/to_Ramps_047/amp2047,jamp2047
      Double Precision amp2048(maxamps), jamp2048(0:maxamps)
      common/to_Ramps_048/amp2048,jamp2048
      Double Precision amp2049(maxamps), jamp2049(0:maxamps)
      common/to_Ramps_049/amp2049,jamp2049
      Double Precision amp2050(maxamps), jamp2050(0:maxamps)
      common/to_Ramps_050/amp2050,jamp2050
      Double Precision amp2051(maxamps), jamp2051(0:maxamps)
      common/to_Ramps_051/amp2051,jamp2051
      Double Precision amp2052(maxamps), jamp2052(0:maxamps)
      common/to_Ramps_052/amp2052,jamp2052
      Double Precision amp2053(maxamps), jamp2053(0:maxamps)
      common/to_Ramps_053/amp2053,jamp2053
      Double Precision amp2054(maxamps), jamp2054(0:maxamps)
      common/to_Ramps_054/amp2054,jamp2054
      Double Precision amp2055(maxamps), jamp2055(0:maxamps)
      common/to_Ramps_055/amp2055,jamp2055
      Double Precision amp2056(maxamps), jamp2056(0:maxamps)
      common/to_Ramps_056/amp2056,jamp2056
      Double Precision amp2057(maxamps), jamp2057(0:maxamps)
      common/to_Ramps_057/amp2057,jamp2057
      Double Precision amp2058(maxamps), jamp2058(0:maxamps)
      common/to_Ramps_058/amp2058,jamp2058
      Double Precision amp2059(maxamps), jamp2059(0:maxamps)
      common/to_Ramps_059/amp2059,jamp2059
      Double Precision amp2060(maxamps), jamp2060(0:maxamps)
      common/to_Ramps_060/amp2060,jamp2060
      Double Precision amp2061(maxamps), jamp2061(0:maxamps)
      common/to_Ramps_061/amp2061,jamp2061
      Double Precision amp2062(maxamps), jamp2062(0:maxamps)
      common/to_Ramps_062/amp2062,jamp2062
      Double Precision amp2063(maxamps), jamp2063(0:maxamps)
      common/to_Ramps_063/amp2063,jamp2063
      Double Precision amp2064(maxamps), jamp2064(0:maxamps)
      common/to_Ramps_064/amp2064,jamp2064
      Double Precision amp2065(maxamps), jamp2065(0:maxamps)
      common/to_Ramps_065/amp2065,jamp2065
      Double Precision amp2066(maxamps), jamp2066(0:maxamps)
      common/to_Ramps_066/amp2066,jamp2066
      Double Precision amp2067(maxamps), jamp2067(0:maxamps)
      common/to_Ramps_067/amp2067,jamp2067
      Double Precision amp2068(maxamps), jamp2068(0:maxamps)
      common/to_Ramps_068/amp2068,jamp2068
      Double Precision amp2069(maxamps), jamp2069(0:maxamps)
      common/to_Ramps_069/amp2069,jamp2069
      Double Precision amp2070(maxamps), jamp2070(0:maxamps)
      common/to_Ramps_070/amp2070,jamp2070
      Double Precision amp2071(maxamps), jamp2071(0:maxamps)
      common/to_Ramps_071/amp2071,jamp2071
      Double Precision amp2072(maxamps), jamp2072(0:maxamps)
      common/to_Ramps_072/amp2072,jamp2072
      Double Precision amp2073(maxamps), jamp2073(0:maxamps)
      common/to_Ramps_073/amp2073,jamp2073
      Double Precision amp2074(maxamps), jamp2074(0:maxamps)
      common/to_Ramps_074/amp2074,jamp2074
      Double Precision amp2075(maxamps), jamp2075(0:maxamps)
      common/to_Ramps_075/amp2075,jamp2075
      Double Precision amp2076(maxamps), jamp2076(0:maxamps)
      common/to_Ramps_076/amp2076,jamp2076
      Double Precision amp2077(maxamps), jamp2077(0:maxamps)
      common/to_Ramps_077/amp2077,jamp2077
      Double Precision amp2078(maxamps), jamp2078(0:maxamps)
      common/to_Ramps_078/amp2078,jamp2078
      Double Precision amp2079(maxamps), jamp2079(0:maxamps)
      common/to_Ramps_079/amp2079,jamp2079
      Double Precision amp2080(maxamps), jamp2080(0:maxamps)
      common/to_Ramps_080/amp2080,jamp2080
      Double Precision amp2081(maxamps), jamp2081(0:maxamps)
      common/to_Ramps_081/amp2081,jamp2081
      Double Precision amp2082(maxamps), jamp2082(0:maxamps)
      common/to_Ramps_082/amp2082,jamp2082
      Double Precision amp2083(maxamps), jamp2083(0:maxamps)
      common/to_Ramps_083/amp2083,jamp2083
      Double Precision amp2084(maxamps), jamp2084(0:maxamps)
      common/to_Ramps_084/amp2084,jamp2084
      Double Precision amp2085(maxamps), jamp2085(0:maxamps)
      common/to_Ramps_085/amp2085,jamp2085
      Double Precision amp2086(maxamps), jamp2086(0:maxamps)
      common/to_Ramps_086/amp2086,jamp2086
      Double Precision amp2087(maxamps), jamp2087(0:maxamps)
      common/to_Ramps_087/amp2087,jamp2087
      Double Precision amp2088(maxamps), jamp2088(0:maxamps)
      common/to_Ramps_088/amp2088,jamp2088
      Double Precision amp2089(maxamps), jamp2089(0:maxamps)
      common/to_Ramps_089/amp2089,jamp2089
      Double Precision amp2090(maxamps), jamp2090(0:maxamps)
      common/to_Ramps_090/amp2090,jamp2090
      Double Precision amp2091(maxamps), jamp2091(0:maxamps)
      common/to_Ramps_091/amp2091,jamp2091
      Double Precision amp2092(maxamps), jamp2092(0:maxamps)
      common/to_Ramps_092/amp2092,jamp2092
      Double Precision amp2093(maxamps), jamp2093(0:maxamps)
      common/to_Ramps_093/amp2093,jamp2093
      Double Precision amp2094(maxamps), jamp2094(0:maxamps)
      common/to_Ramps_094/amp2094,jamp2094
      Double Precision amp2095(maxamps), jamp2095(0:maxamps)
      common/to_Ramps_095/amp2095,jamp2095
      Double Precision amp2096(maxamps), jamp2096(0:maxamps)
      common/to_Ramps_096/amp2096,jamp2096
      Double Precision amp2097(maxamps), jamp2097(0:maxamps)
      common/to_Ramps_097/amp2097,jamp2097
      Double Precision amp2098(maxamps), jamp2098(0:maxamps)
      common/to_Ramps_098/amp2098,jamp2098
      Double Precision amp2099(maxamps), jamp2099(0:maxamps)
      common/to_Ramps_099/amp2099,jamp2099
      Double Precision amp2100(maxamps), jamp2100(0:maxamps)
      common/to_Ramps_100/amp2100,jamp2100
      Double Precision amp2101(maxamps), jamp2101(0:maxamps)
      common/to_Ramps_101/amp2101,jamp2101
      Double Precision amp2102(maxamps), jamp2102(0:maxamps)
      common/to_Ramps_102/amp2102,jamp2102
      Double Precision amp2103(maxamps), jamp2103(0:maxamps)
      common/to_Ramps_103/amp2103,jamp2103
      Double Precision amp2104(maxamps), jamp2104(0:maxamps)
      common/to_Ramps_104/amp2104,jamp2104
      Double Precision amp2105(maxamps), jamp2105(0:maxamps)
      common/to_Ramps_105/amp2105,jamp2105
      Double Precision amp2106(maxamps), jamp2106(0:maxamps)
      common/to_Ramps_106/amp2106,jamp2106
      Double Precision amp2107(maxamps), jamp2107(0:maxamps)
      common/to_Ramps_107/amp2107,jamp2107
      Double Precision amp2108(maxamps), jamp2108(0:maxamps)
      common/to_Ramps_108/amp2108,jamp2108
      Double Precision amp2109(maxamps), jamp2109(0:maxamps)
      common/to_Ramps_109/amp2109,jamp2109
      Double Precision amp2110(maxamps), jamp2110(0:maxamps)
      common/to_Ramps_110/amp2110,jamp2110
      Double Precision amp2111(maxamps), jamp2111(0:maxamps)
      common/to_Ramps_111/amp2111,jamp2111
      Double Precision amp2112(maxamps), jamp2112(0:maxamps)
      common/to_Ramps_112/amp2112,jamp2112
      Double Precision amp2113(maxamps), jamp2113(0:maxamps)
      common/to_Ramps_113/amp2113,jamp2113
      Double Precision amp2114(maxamps), jamp2114(0:maxamps)
      common/to_Ramps_114/amp2114,jamp2114
      Double Precision amp2115(maxamps), jamp2115(0:maxamps)
      common/to_Ramps_115/amp2115,jamp2115
      Double Precision amp2116(maxamps), jamp2116(0:maxamps)
      common/to_Ramps_116/amp2116,jamp2116
      Double Precision amp2117(maxamps), jamp2117(0:maxamps)
      common/to_Ramps_117/amp2117,jamp2117
      Double Precision amp2118(maxamps), jamp2118(0:maxamps)
      common/to_Ramps_118/amp2118,jamp2118
      Double Precision amp2119(maxamps), jamp2119(0:maxamps)
      common/to_Ramps_119/amp2119,jamp2119
      Double Precision amp2120(maxamps), jamp2120(0:maxamps)
      common/to_Ramps_120/amp2120,jamp2120
      Double Precision amp2121(maxamps), jamp2121(0:maxamps)
      common/to_Ramps_121/amp2121,jamp2121
      Double Precision amp2122(maxamps), jamp2122(0:maxamps)
      common/to_Ramps_122/amp2122,jamp2122
      Double Precision amp2123(maxamps), jamp2123(0:maxamps)
      common/to_Ramps_123/amp2123,jamp2123
      Double Precision amp2124(maxamps), jamp2124(0:maxamps)
      common/to_Ramps_124/amp2124,jamp2124
      Double Precision amp2125(maxamps), jamp2125(0:maxamps)
      common/to_Ramps_125/amp2125,jamp2125
      Double Precision amp2126(maxamps), jamp2126(0:maxamps)
      common/to_Ramps_126/amp2126,jamp2126
      Double Precision amp2127(maxamps), jamp2127(0:maxamps)
      common/to_Ramps_127/amp2127,jamp2127
      Double Precision amp2128(maxamps), jamp2128(0:maxamps)
      common/to_Ramps_128/amp2128,jamp2128
      Double Precision amp2129(maxamps), jamp2129(0:maxamps)
      common/to_Ramps_129/amp2129,jamp2129
      Double Precision amp2130(maxamps), jamp2130(0:maxamps)
      common/to_Ramps_130/amp2130,jamp2130
      Double Precision amp2131(maxamps), jamp2131(0:maxamps)
      common/to_Ramps_131/amp2131,jamp2131
      Double Precision amp2132(maxamps), jamp2132(0:maxamps)
      common/to_Ramps_132/amp2132,jamp2132
      Double Precision amp2133(maxamps), jamp2133(0:maxamps)
      common/to_Ramps_133/amp2133,jamp2133
      Double Precision amp2134(maxamps), jamp2134(0:maxamps)
      common/to_Ramps_134/amp2134,jamp2134
      Double Precision amp2135(maxamps), jamp2135(0:maxamps)
      common/to_Ramps_135/amp2135,jamp2135
      Double Precision amp2136(maxamps), jamp2136(0:maxamps)
      common/to_Ramps_136/amp2136,jamp2136
      Double Precision amp2137(maxamps), jamp2137(0:maxamps)
      common/to_Ramps_137/amp2137,jamp2137
      Double Precision amp2138(maxamps), jamp2138(0:maxamps)
      common/to_Ramps_138/amp2138,jamp2138
      Double Precision amp2139(maxamps), jamp2139(0:maxamps)
      common/to_Ramps_139/amp2139,jamp2139
      Double Precision amp2140(maxamps), jamp2140(0:maxamps)
      common/to_Ramps_140/amp2140,jamp2140
      Double Precision amp2141(maxamps), jamp2141(0:maxamps)
      common/to_Ramps_141/amp2141,jamp2141
      Double Precision amp2142(maxamps), jamp2142(0:maxamps)
      common/to_Ramps_142/amp2142,jamp2142
      Double Precision amp2143(maxamps), jamp2143(0:maxamps)
      common/to_Ramps_143/amp2143,jamp2143
      Double Precision amp2144(maxamps), jamp2144(0:maxamps)
      common/to_Ramps_144/amp2144,jamp2144
      Double Precision amp2145(maxamps), jamp2145(0:maxamps)
      common/to_Ramps_145/amp2145,jamp2145
      Double Precision amp2146(maxamps), jamp2146(0:maxamps)
      common/to_Ramps_146/amp2146,jamp2146
      Double Precision amp2147(maxamps), jamp2147(0:maxamps)
      common/to_Ramps_147/amp2147,jamp2147
      Double Precision amp2148(maxamps), jamp2148(0:maxamps)
      common/to_Ramps_148/amp2148,jamp2148
      Double Precision amp2149(maxamps), jamp2149(0:maxamps)
      common/to_Ramps_149/amp2149,jamp2149
      Double Precision amp2150(maxamps), jamp2150(0:maxamps)
      common/to_Ramps_150/amp2150,jamp2150
      Double Precision amp2151(maxamps), jamp2151(0:maxamps)
      common/to_Ramps_151/amp2151,jamp2151
      Double Precision amp2152(maxamps), jamp2152(0:maxamps)
      common/to_Ramps_152/amp2152,jamp2152
      Double Precision amp2153(maxamps), jamp2153(0:maxamps)
      common/to_Ramps_153/amp2153,jamp2153
      Double Precision amp2154(maxamps), jamp2154(0:maxamps)
      common/to_Ramps_154/amp2154,jamp2154
      Double Precision amp2155(maxamps), jamp2155(0:maxamps)
      common/to_Ramps_155/amp2155,jamp2155
      Double Precision amp2156(maxamps), jamp2156(0:maxamps)
      common/to_Ramps_156/amp2156,jamp2156
      Double Precision amp2157(maxamps), jamp2157(0:maxamps)
      common/to_Ramps_157/amp2157,jamp2157
      Double Precision amp2158(maxamps), jamp2158(0:maxamps)
      common/to_Ramps_158/amp2158,jamp2158
      Double Precision amp2159(maxamps), jamp2159(0:maxamps)
      common/to_Ramps_159/amp2159,jamp2159
      Double Precision amp2160(maxamps), jamp2160(0:maxamps)
      common/to_Ramps_160/amp2160,jamp2160
      Double Precision amp2161(maxamps), jamp2161(0:maxamps)
      common/to_Ramps_161/amp2161,jamp2161
      Double Precision amp2162(maxamps), jamp2162(0:maxamps)
      common/to_Ramps_162/amp2162,jamp2162
      Double Precision amp2163(maxamps), jamp2163(0:maxamps)
      common/to_Ramps_163/amp2163,jamp2163
      Double Precision amp2164(maxamps), jamp2164(0:maxamps)
      common/to_Ramps_164/amp2164,jamp2164
      Double Precision amp2165(maxamps), jamp2165(0:maxamps)
      common/to_Ramps_165/amp2165,jamp2165
      Double Precision amp2166(maxamps), jamp2166(0:maxamps)
      common/to_Ramps_166/amp2166,jamp2166
      Double Precision amp2167(maxamps), jamp2167(0:maxamps)
      common/to_Ramps_167/amp2167,jamp2167
      Double Precision amp2168(maxamps), jamp2168(0:maxamps)
      common/to_Ramps_168/amp2168,jamp2168
      Double Precision amp2169(maxamps), jamp2169(0:maxamps)
      common/to_Ramps_169/amp2169,jamp2169
      Double Precision amp2170(maxamps), jamp2170(0:maxamps)
      common/to_Ramps_170/amp2170,jamp2170
      Double Precision amp2171(maxamps), jamp2171(0:maxamps)
      common/to_Ramps_171/amp2171,jamp2171
      Double Precision amp2172(maxamps), jamp2172(0:maxamps)
      common/to_Ramps_172/amp2172,jamp2172
      Double Precision amp2173(maxamps), jamp2173(0:maxamps)
      common/to_Ramps_173/amp2173,jamp2173
      Double Precision amp2174(maxamps), jamp2174(0:maxamps)
      common/to_Ramps_174/amp2174,jamp2174
      Double Precision amp2175(maxamps), jamp2175(0:maxamps)
      common/to_Ramps_175/amp2175,jamp2175
      double precision jamp2cum(0:maxamps)
      integer ICOLUP(2,nexternal,maxamps)
      integer color(2,nexternal),color1(2,nexternal)
      double precision random,xtarget
      external random
      integer legs(nexternal),lstr,i,j
      character*20 str
      integer ic(nexternal),legs1(nexternal)
      integer iflow,ifl
      logical mtc,even

      ! TODO: vereinfachen und verallgemeinern!
#ifdef DEBUG
      print*,"in subroutine real_color. Uncomment to continue"
      stop
#endif

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
      
      if(str.eq."-1-1nInJ-1-1") then
         include "leshouches_R_001.inc"
         iflow=nint(jamp2001(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2001(i)
         enddo
         goto 20
      elseif(str.eq."-11nInJ1-1") then
         include "leshouches_R_002.inc"
         iflow=nint(jamp2002(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2002(i)
         enddo
         goto 20
      elseif(str.eq."-11nInJ2-2") then
         include "leshouches_R_003.inc"
         iflow=nint(jamp2003(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2003(i)
         enddo
         goto 20
      elseif(str.eq."-11nInJ4-4") then
         include "leshouches_R_004.inc"
         iflow=nint(jamp2004(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2004(i)
         enddo
         goto 20
      elseif(str.eq."-11nInJ3-3") then
         include "leshouches_R_005.inc"
         iflow=nint(jamp2005(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2005(i)
         enddo
         goto 20
      elseif(str.eq."-11nInJ5-5") then
         include "leshouches_R_006.inc"
         iflow=nint(jamp2006(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2006(i)
         enddo
         goto 20
      elseif(str.eq."-11nInJ00") then
         include "leshouches_R_007.inc"
         iflow=nint(jamp2007(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2007(i)
         enddo
         goto 20
      elseif(str.eq."-1-2nInJ-1-2") then
         include "leshouches_R_008.inc"
         iflow=nint(jamp2008(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2008(i)
         enddo
         goto 20
      elseif(str.eq."-12nInJ-12") then
         include "leshouches_R_009.inc"
         iflow=nint(jamp2009(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2009(i)
         enddo
         goto 20
      elseif(str.eq."-1-4nInJ-1-4") then
         include "leshouches_R_010.inc"
         iflow=nint(jamp2010(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2010(i)
         enddo
         goto 20
      elseif(str.eq."-14nInJ-14") then
         include "leshouches_R_011.inc"
         iflow=nint(jamp2011(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2011(i)
         enddo
         goto 20
      elseif(str.eq."-1-3nInJ-1-3") then
         include "leshouches_R_012.inc"
         iflow=nint(jamp2012(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2012(i)
         enddo
         goto 20
      elseif(str.eq."-13nInJ-13") then
         include "leshouches_R_013.inc"
         iflow=nint(jamp2013(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2013(i)
         enddo
         goto 20
      elseif(str.eq."-1-5nInJ-1-5") then
         include "leshouches_R_014.inc"
         iflow=nint(jamp2014(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2014(i)
         enddo
         goto 20
      elseif(str.eq."-15nInJ-15") then
         include "leshouches_R_015.inc"
         iflow=nint(jamp2015(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2015(i)
         enddo
         goto 20
      elseif(str.eq."-10nInJ-10") then
         include "leshouches_R_016.inc"
         iflow=nint(jamp2016(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2016(i)
         enddo
         goto 20
      elseif(str.eq."1-1nInJ1-1") then
         include "leshouches_R_017.inc"
         iflow=nint(jamp2017(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2017(i)
         enddo
         goto 20
      elseif(str.eq."1-1nInJ2-2") then
         include "leshouches_R_018.inc"
         iflow=nint(jamp2018(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2018(i)
         enddo
         goto 20
      elseif(str.eq."1-1nInJ4-4") then
         include "leshouches_R_019.inc"
         iflow=nint(jamp2019(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2019(i)
         enddo
         goto 20
      elseif(str.eq."1-1nInJ3-3") then
         include "leshouches_R_020.inc"
         iflow=nint(jamp2020(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2020(i)
         enddo
         goto 20
      elseif(str.eq."1-1nInJ5-5") then
         include "leshouches_R_021.inc"
         iflow=nint(jamp2021(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2021(i)
         enddo
         goto 20
      elseif(str.eq."1-1nInJ00") then
         include "leshouches_R_022.inc"
         iflow=nint(jamp2022(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2022(i)
         enddo
         goto 20
      elseif(str.eq."11nInJ11") then
         include "leshouches_R_023.inc"
         iflow=nint(jamp2023(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2023(i)
         enddo
         goto 20
      elseif(str.eq."1-2nInJ1-2") then
         include "leshouches_R_024.inc"
         iflow=nint(jamp2024(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2024(i)
         enddo
         goto 20
      elseif(str.eq."12nInJ12") then
         include "leshouches_R_025.inc"
         iflow=nint(jamp2025(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2025(i)
         enddo
         goto 20
      elseif(str.eq."1-4nInJ1-4") then
         include "leshouches_R_026.inc"
         iflow=nint(jamp2026(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2026(i)
         enddo
         goto 20
      elseif(str.eq."14nInJ14") then
         include "leshouches_R_027.inc"
         iflow=nint(jamp2027(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2027(i)
         enddo
         goto 20
      elseif(str.eq."1-3nInJ1-3") then
         include "leshouches_R_028.inc"
         iflow=nint(jamp2028(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2028(i)
         enddo
         goto 20
      elseif(str.eq."13nInJ13") then
         include "leshouches_R_029.inc"
         iflow=nint(jamp2029(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2029(i)
         enddo
         goto 20
      elseif(str.eq."1-5nInJ1-5") then
         include "leshouches_R_030.inc"
         iflow=nint(jamp2030(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2030(i)
         enddo
         goto 20
      elseif(str.eq."15nInJ15") then
         include "leshouches_R_031.inc"
         iflow=nint(jamp2031(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2031(i)
         enddo
         goto 20
      elseif(str.eq."10nInJ10") then
         include "leshouches_R_032.inc"
         iflow=nint(jamp2032(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2032(i)
         enddo
         goto 20
      elseif(str.eq."-2-1nInJ-1-2") then
         include "leshouches_R_033.inc"
         iflow=nint(jamp2033(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2033(i)
         enddo
         goto 20
      elseif(str.eq."-21nInJ1-2") then
         include "leshouches_R_034.inc"
         iflow=nint(jamp2034(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2034(i)
         enddo
         goto 20
      elseif(str.eq."-2-2nInJ-2-2") then
         include "leshouches_R_035.inc"
         iflow=nint(jamp2035(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2035(i)
         enddo
         goto 20
      elseif(str.eq."-22nInJ1-1") then
         include "leshouches_R_036.inc"
         iflow=nint(jamp2036(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2036(i)
         enddo
         goto 20
      elseif(str.eq."-22nInJ2-2") then
         include "leshouches_R_037.inc"
         iflow=nint(jamp2037(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2037(i)
         enddo
         goto 20
      elseif(str.eq."-22nInJ4-4") then
         include "leshouches_R_038.inc"
         iflow=nint(jamp2038(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2038(i)
         enddo
         goto 20
      elseif(str.eq."-22nInJ3-3") then
         include "leshouches_R_039.inc"
         iflow=nint(jamp2039(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2039(i)
         enddo
         goto 20
      elseif(str.eq."-22nInJ5-5") then
         include "leshouches_R_040.inc"
         iflow=nint(jamp2040(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2040(i)
         enddo
         goto 20
      elseif(str.eq."-22nInJ00") then
         include "leshouches_R_041.inc"
         iflow=nint(jamp2041(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2041(i)
         enddo
         goto 20
      elseif(str.eq."-2-4nInJ-2-4") then
         include "leshouches_R_042.inc"
         iflow=nint(jamp2042(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2042(i)
         enddo
         goto 20
      elseif(str.eq."-24nInJ-24") then
         include "leshouches_R_043.inc"
         iflow=nint(jamp2043(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2043(i)
         enddo
         goto 20
      elseif(str.eq."-2-3nInJ-2-3") then
         include "leshouches_R_044.inc"
         iflow=nint(jamp2044(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2044(i)
         enddo
         goto 20
      elseif(str.eq."-23nInJ-23") then
         include "leshouches_R_045.inc"
         iflow=nint(jamp2045(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2045(i)
         enddo
         goto 20
      elseif(str.eq."-2-5nInJ-2-5") then
         include "leshouches_R_046.inc"
         iflow=nint(jamp2046(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2046(i)
         enddo
         goto 20
      elseif(str.eq."-25nInJ-25") then
         include "leshouches_R_047.inc"
         iflow=nint(jamp2047(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2047(i)
         enddo
         goto 20
      elseif(str.eq."-20nInJ-20") then
         include "leshouches_R_048.inc"
         iflow=nint(jamp2048(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2048(i)
         enddo
         goto 20
      elseif(str.eq."2-1nInJ-12") then
         include "leshouches_R_049.inc"
         iflow=nint(jamp2049(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2049(i)
         enddo
         goto 20
      elseif(str.eq."21nInJ12") then
         include "leshouches_R_050.inc"
         iflow=nint(jamp2050(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2050(i)
         enddo
         goto 20
      elseif(str.eq."2-2nInJ1-1") then
         include "leshouches_R_051.inc"
         iflow=nint(jamp2051(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2051(i)
         enddo
         goto 20
      elseif(str.eq."2-2nInJ2-2") then
         include "leshouches_R_052.inc"
         iflow=nint(jamp2052(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2052(i)
         enddo
         goto 20
      elseif(str.eq."2-2nInJ4-4") then
         include "leshouches_R_053.inc"
         iflow=nint(jamp2053(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2053(i)
         enddo
         goto 20
      elseif(str.eq."2-2nInJ3-3") then
         include "leshouches_R_054.inc"
         iflow=nint(jamp2054(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2054(i)
         enddo
         goto 20
      elseif(str.eq."2-2nInJ5-5") then
         include "leshouches_R_055.inc"
         iflow=nint(jamp2055(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2055(i)
         enddo
         goto 20
      elseif(str.eq."2-2nInJ00") then
         include "leshouches_R_056.inc"
         iflow=nint(jamp2056(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2056(i)
         enddo
         goto 20
      elseif(str.eq."22nInJ22") then
         include "leshouches_R_057.inc"
         iflow=nint(jamp2057(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2057(i)
         enddo
         goto 20
      elseif(str.eq."2-4nInJ2-4") then
         include "leshouches_R_058.inc"
         iflow=nint(jamp2058(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2058(i)
         enddo
         goto 20
      elseif(str.eq."24nInJ24") then
         include "leshouches_R_059.inc"
         iflow=nint(jamp2059(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2059(i)
         enddo
         goto 20
      elseif(str.eq."2-3nInJ2-3") then
         include "leshouches_R_060.inc"
         iflow=nint(jamp2060(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2060(i)
         enddo
         goto 20
      elseif(str.eq."23nInJ23") then
         include "leshouches_R_061.inc"
         iflow=nint(jamp2061(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2061(i)
         enddo
         goto 20
      elseif(str.eq."2-5nInJ2-5") then
         include "leshouches_R_062.inc"
         iflow=nint(jamp2062(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2062(i)
         enddo
         goto 20
      elseif(str.eq."25nInJ25") then
         include "leshouches_R_063.inc"
         iflow=nint(jamp2063(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2063(i)
         enddo
         goto 20
      elseif(str.eq."20nInJ20") then
         include "leshouches_R_064.inc"
         iflow=nint(jamp2064(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2064(i)
         enddo
         goto 20
      elseif(str.eq."-4-1nInJ-1-4") then
         include "leshouches_R_065.inc"
         iflow=nint(jamp2065(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2065(i)
         enddo
         goto 20
      elseif(str.eq."-41nInJ1-4") then
         include "leshouches_R_066.inc"
         iflow=nint(jamp2066(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2066(i)
         enddo
         goto 20
      elseif(str.eq."-4-2nInJ-2-4") then
         include "leshouches_R_067.inc"
         iflow=nint(jamp2067(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2067(i)
         enddo
         goto 20
      elseif(str.eq."-42nInJ2-4") then
         include "leshouches_R_068.inc"
         iflow=nint(jamp2068(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2068(i)
         enddo
         goto 20
      elseif(str.eq."-4-4nInJ-4-4") then
         include "leshouches_R_069.inc"
         iflow=nint(jamp2069(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2069(i)
         enddo
         goto 20
      elseif(str.eq."-44nInJ1-1") then
         include "leshouches_R_070.inc"
         iflow=nint(jamp2070(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2070(i)
         enddo
         goto 20
      elseif(str.eq."-44nInJ2-2") then
         include "leshouches_R_071.inc"
         iflow=nint(jamp2071(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2071(i)
         enddo
         goto 20
      elseif(str.eq."-44nInJ4-4") then
         include "leshouches_R_072.inc"
         iflow=nint(jamp2072(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2072(i)
         enddo
         goto 20
      elseif(str.eq."-44nInJ3-3") then
         include "leshouches_R_073.inc"
         iflow=nint(jamp2073(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2073(i)
         enddo
         goto 20
      elseif(str.eq."-44nInJ5-5") then
         include "leshouches_R_074.inc"
         iflow=nint(jamp2074(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2074(i)
         enddo
         goto 20
      elseif(str.eq."-44nInJ00") then
         include "leshouches_R_075.inc"
         iflow=nint(jamp2075(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2075(i)
         enddo
         goto 20
      elseif(str.eq."-4-3nInJ-4-3") then
         include "leshouches_R_076.inc"
         iflow=nint(jamp2076(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2076(i)
         enddo
         goto 20
      elseif(str.eq."-43nInJ-43") then
         include "leshouches_R_077.inc"
         iflow=nint(jamp2077(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2077(i)
         enddo
         goto 20
      elseif(str.eq."-4-5nInJ-4-5") then
         include "leshouches_R_078.inc"
         iflow=nint(jamp2078(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2078(i)
         enddo
         goto 20
      elseif(str.eq."-45nInJ-45") then
         include "leshouches_R_079.inc"
         iflow=nint(jamp2079(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2079(i)
         enddo
         goto 20
      elseif(str.eq."-40nInJ-40") then
         include "leshouches_R_080.inc"
         iflow=nint(jamp2080(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2080(i)
         enddo
         goto 20
      elseif(str.eq."4-1nInJ-14") then
         include "leshouches_R_081.inc"
         iflow=nint(jamp2081(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2081(i)
         enddo
         goto 20
      elseif(str.eq."41nInJ14") then
         include "leshouches_R_082.inc"
         iflow=nint(jamp2082(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2082(i)
         enddo
         goto 20
      elseif(str.eq."4-2nInJ-24") then
         include "leshouches_R_083.inc"
         iflow=nint(jamp2083(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2083(i)
         enddo
         goto 20
      elseif(str.eq."42nInJ24") then
         include "leshouches_R_084.inc"
         iflow=nint(jamp2084(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2084(i)
         enddo
         goto 20
      elseif(str.eq."4-4nInJ1-1") then
         include "leshouches_R_085.inc"
         iflow=nint(jamp2085(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2085(i)
         enddo
         goto 20
      elseif(str.eq."4-4nInJ2-2") then
         include "leshouches_R_086.inc"
         iflow=nint(jamp2086(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2086(i)
         enddo
         goto 20
      elseif(str.eq."4-4nInJ4-4") then
         include "leshouches_R_087.inc"
         iflow=nint(jamp2087(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2087(i)
         enddo
         goto 20
      elseif(str.eq."4-4nInJ3-3") then
         include "leshouches_R_088.inc"
         iflow=nint(jamp2088(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2088(i)
         enddo
         goto 20
      elseif(str.eq."4-4nInJ5-5") then
         include "leshouches_R_089.inc"
         iflow=nint(jamp2089(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2089(i)
         enddo
         goto 20
      elseif(str.eq."4-4nInJ00") then
         include "leshouches_R_090.inc"
         iflow=nint(jamp2090(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2090(i)
         enddo
         goto 20
      elseif(str.eq."44nInJ44") then
         include "leshouches_R_091.inc"
         iflow=nint(jamp2091(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2091(i)
         enddo
         goto 20
      elseif(str.eq."4-3nInJ4-3") then
         include "leshouches_R_092.inc"
         iflow=nint(jamp2092(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2092(i)
         enddo
         goto 20
      elseif(str.eq."43nInJ43") then
         include "leshouches_R_093.inc"
         iflow=nint(jamp2093(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2093(i)
         enddo
         goto 20
      elseif(str.eq."4-5nInJ4-5") then
         include "leshouches_R_094.inc"
         iflow=nint(jamp2094(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2094(i)
         enddo
         goto 20
      elseif(str.eq."45nInJ45") then
         include "leshouches_R_095.inc"
         iflow=nint(jamp2095(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2095(i)
         enddo
         goto 20
      elseif(str.eq."40nInJ40") then
         include "leshouches_R_096.inc"
         iflow=nint(jamp2096(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2096(i)
         enddo
         goto 20
      elseif(str.eq."-3-1nInJ-1-3") then
         include "leshouches_R_097.inc"
         iflow=nint(jamp2097(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2097(i)
         enddo
         goto 20
      elseif(str.eq."-31nInJ1-3") then
         include "leshouches_R_098.inc"
         iflow=nint(jamp2098(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2098(i)
         enddo
         goto 20
      elseif(str.eq."-3-2nInJ-2-3") then
         include "leshouches_R_099.inc"
         iflow=nint(jamp2099(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2099(i)
         enddo
         goto 20
      elseif(str.eq."-32nInJ2-3") then
         include "leshouches_R_100.inc"
         iflow=nint(jamp2100(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2100(i)
         enddo
         goto 20
      elseif(str.eq."-3-4nInJ-4-3") then
         include "leshouches_R_101.inc"
         iflow=nint(jamp2101(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2101(i)
         enddo
         goto 20
      elseif(str.eq."-34nInJ4-3") then
         include "leshouches_R_102.inc"
         iflow=nint(jamp2102(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2102(i)
         enddo
         goto 20
      elseif(str.eq."-3-3nInJ-3-3") then
         include "leshouches_R_103.inc"
         iflow=nint(jamp2103(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2103(i)
         enddo
         goto 20
      elseif(str.eq."-33nInJ1-1") then
         include "leshouches_R_104.inc"
         iflow=nint(jamp2104(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2104(i)
         enddo
         goto 20
      elseif(str.eq."-33nInJ2-2") then
         include "leshouches_R_105.inc"
         iflow=nint(jamp2105(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2105(i)
         enddo
         goto 20
      elseif(str.eq."-33nInJ4-4") then
         include "leshouches_R_106.inc"
         iflow=nint(jamp2106(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2106(i)
         enddo
         goto 20
      elseif(str.eq."-33nInJ3-3") then
         include "leshouches_R_107.inc"
         iflow=nint(jamp2107(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2107(i)
         enddo
         goto 20
      elseif(str.eq."-33nInJ5-5") then
         include "leshouches_R_108.inc"
         iflow=nint(jamp2108(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2108(i)
         enddo
         goto 20
      elseif(str.eq."-33nInJ00") then
         include "leshouches_R_109.inc"
         iflow=nint(jamp2109(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2109(i)
         enddo
         goto 20
      elseif(str.eq."-3-5nInJ-3-5") then
         include "leshouches_R_110.inc"
         iflow=nint(jamp2110(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2110(i)
         enddo
         goto 20
      elseif(str.eq."-35nInJ-35") then
         include "leshouches_R_111.inc"
         iflow=nint(jamp2111(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2111(i)
         enddo
         goto 20
      elseif(str.eq."-30nInJ-30") then
         include "leshouches_R_112.inc"
         iflow=nint(jamp2112(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2112(i)
         enddo
         goto 20
      elseif(str.eq."3-1nInJ-13") then
         include "leshouches_R_113.inc"
         iflow=nint(jamp2113(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2113(i)
         enddo
         goto 20
      elseif(str.eq."31nInJ13") then
         include "leshouches_R_114.inc"
         iflow=nint(jamp2114(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2114(i)
         enddo
         goto 20
      elseif(str.eq."3-2nInJ-23") then
         include "leshouches_R_115.inc"
         iflow=nint(jamp2115(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2115(i)
         enddo
         goto 20
      elseif(str.eq."32nInJ23") then
         include "leshouches_R_116.inc"
         iflow=nint(jamp2116(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2116(i)
         enddo
         goto 20
      elseif(str.eq."3-4nInJ-43") then
         include "leshouches_R_117.inc"
         iflow=nint(jamp2117(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2117(i)
         enddo
         goto 20
      elseif(str.eq."34nInJ43") then
         include "leshouches_R_118.inc"
         iflow=nint(jamp2118(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2118(i)
         enddo
         goto 20
      elseif(str.eq."3-3nInJ1-1") then
         include "leshouches_R_119.inc"
         iflow=nint(jamp2119(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2119(i)
         enddo
         goto 20
      elseif(str.eq."3-3nInJ2-2") then
         include "leshouches_R_120.inc"
         iflow=nint(jamp2120(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2120(i)
         enddo
         goto 20
      elseif(str.eq."3-3nInJ4-4") then
         include "leshouches_R_121.inc"
         iflow=nint(jamp2121(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2121(i)
         enddo
         goto 20
      elseif(str.eq."3-3nInJ3-3") then
         include "leshouches_R_122.inc"
         iflow=nint(jamp2122(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2122(i)
         enddo
         goto 20
      elseif(str.eq."3-3nInJ5-5") then
         include "leshouches_R_123.inc"
         iflow=nint(jamp2123(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2123(i)
         enddo
         goto 20
      elseif(str.eq."3-3nInJ00") then
         include "leshouches_R_124.inc"
         iflow=nint(jamp2124(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2124(i)
         enddo
         goto 20
      elseif(str.eq."33nInJ33") then
         include "leshouches_R_125.inc"
         iflow=nint(jamp2125(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2125(i)
         enddo
         goto 20
      elseif(str.eq."3-5nInJ3-5") then
         include "leshouches_R_126.inc"
         iflow=nint(jamp2126(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2126(i)
         enddo
         goto 20
      elseif(str.eq."35nInJ35") then
         include "leshouches_R_127.inc"
         iflow=nint(jamp2127(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2127(i)
         enddo
         goto 20
      elseif(str.eq."30nInJ30") then
         include "leshouches_R_128.inc"
         iflow=nint(jamp2128(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2128(i)
         enddo
         goto 20
      elseif(str.eq."-5-1nInJ-1-5") then
         include "leshouches_R_129.inc"
         iflow=nint(jamp2129(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2129(i)
         enddo
         goto 20
      elseif(str.eq."-51nInJ1-5") then
         include "leshouches_R_130.inc"
         iflow=nint(jamp2130(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2130(i)
         enddo
         goto 20
      elseif(str.eq."-5-2nInJ-2-5") then
         include "leshouches_R_131.inc"
         iflow=nint(jamp2131(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2131(i)
         enddo
         goto 20
      elseif(str.eq."-52nInJ2-5") then
         include "leshouches_R_132.inc"
         iflow=nint(jamp2132(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2132(i)
         enddo
         goto 20
      elseif(str.eq."-5-4nInJ-4-5") then
         include "leshouches_R_133.inc"
         iflow=nint(jamp2133(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2133(i)
         enddo
         goto 20
      elseif(str.eq."-54nInJ4-5") then
         include "leshouches_R_134.inc"
         iflow=nint(jamp2134(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2134(i)
         enddo
         goto 20
      elseif(str.eq."-5-3nInJ-3-5") then
         include "leshouches_R_135.inc"
         iflow=nint(jamp2135(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2135(i)
         enddo
         goto 20
      elseif(str.eq."-53nInJ3-5") then
         include "leshouches_R_136.inc"
         iflow=nint(jamp2136(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2136(i)
         enddo
         goto 20
      elseif(str.eq."-5-5nInJ-5-5") then
         include "leshouches_R_137.inc"
         iflow=nint(jamp2137(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2137(i)
         enddo
         goto 20
      elseif(str.eq."-55nInJ1-1") then
         include "leshouches_R_138.inc"
         iflow=nint(jamp2138(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2138(i)
         enddo
         goto 20
      elseif(str.eq."-55nInJ2-2") then
         include "leshouches_R_139.inc"
         iflow=nint(jamp2139(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2139(i)
         enddo
         goto 20
      elseif(str.eq."-55nInJ4-4") then
         include "leshouches_R_140.inc"
         iflow=nint(jamp2140(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2140(i)
         enddo
         goto 20
      elseif(str.eq."-55nInJ3-3") then
         include "leshouches_R_141.inc"
         iflow=nint(jamp2141(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2141(i)
         enddo
         goto 20
      elseif(str.eq."-55nInJ5-5") then
         include "leshouches_R_142.inc"
         iflow=nint(jamp2142(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2142(i)
         enddo
         goto 20
      elseif(str.eq."-55nInJ00") then
         include "leshouches_R_143.inc"
         iflow=nint(jamp2143(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2143(i)
         enddo
         goto 20
      elseif(str.eq."-50nInJ-50") then
         include "leshouches_R_144.inc"
         iflow=nint(jamp2144(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2144(i)
         enddo
         goto 20
      elseif(str.eq."5-1nInJ-15") then
         include "leshouches_R_145.inc"
         iflow=nint(jamp2145(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2145(i)
         enddo
         goto 20
      elseif(str.eq."51nInJ15") then
         include "leshouches_R_146.inc"
         iflow=nint(jamp2146(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2146(i)
         enddo
         goto 20
      elseif(str.eq."5-2nInJ-25") then
         include "leshouches_R_147.inc"
         iflow=nint(jamp2147(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2147(i)
         enddo
         goto 20
      elseif(str.eq."52nInJ25") then
         include "leshouches_R_148.inc"
         iflow=nint(jamp2148(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2148(i)
         enddo
         goto 20
      elseif(str.eq."5-4nInJ-45") then
         include "leshouches_R_149.inc"
         iflow=nint(jamp2149(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2149(i)
         enddo
         goto 20
      elseif(str.eq."54nInJ45") then
         include "leshouches_R_150.inc"
         iflow=nint(jamp2150(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2150(i)
         enddo
         goto 20
      elseif(str.eq."5-3nInJ-35") then
         include "leshouches_R_151.inc"
         iflow=nint(jamp2151(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2151(i)
         enddo
         goto 20
      elseif(str.eq."53nInJ35") then
         include "leshouches_R_152.inc"
         iflow=nint(jamp2152(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2152(i)
         enddo
         goto 20
      elseif(str.eq."5-5nInJ1-1") then
         include "leshouches_R_153.inc"
         iflow=nint(jamp2153(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2153(i)
         enddo
         goto 20
      elseif(str.eq."5-5nInJ2-2") then
         include "leshouches_R_154.inc"
         iflow=nint(jamp2154(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2154(i)
         enddo
         goto 20
      elseif(str.eq."5-5nInJ4-4") then
         include "leshouches_R_155.inc"
         iflow=nint(jamp2155(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2155(i)
         enddo
         goto 20
      elseif(str.eq."5-5nInJ3-3") then
         include "leshouches_R_156.inc"
         iflow=nint(jamp2156(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2156(i)
         enddo
         goto 20
      elseif(str.eq."5-5nInJ5-5") then
         include "leshouches_R_157.inc"
         iflow=nint(jamp2157(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2157(i)
         enddo
         goto 20
      elseif(str.eq."5-5nInJ00") then
         include "leshouches_R_158.inc"
         iflow=nint(jamp2158(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2158(i)
         enddo
         goto 20
      elseif(str.eq."55nInJ55") then
         include "leshouches_R_159.inc"
         iflow=nint(jamp2159(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2159(i)
         enddo
         goto 20
      elseif(str.eq."50nInJ50") then
         include "leshouches_R_160.inc"
         iflow=nint(jamp2160(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2160(i)
         enddo
         goto 20
      elseif(str.eq."0-1nInJ-10") then
         include "leshouches_R_161.inc"
         iflow=nint(jamp2161(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2161(i)
         enddo
         goto 20
      elseif(str.eq."01nInJ10") then
         include "leshouches_R_162.inc"
         iflow=nint(jamp2162(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2162(i)
         enddo
         goto 20
      elseif(str.eq."0-2nInJ-20") then
         include "leshouches_R_163.inc"
         iflow=nint(jamp2163(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2163(i)
         enddo
         goto 20
      elseif(str.eq."02nInJ20") then
         include "leshouches_R_164.inc"
         iflow=nint(jamp2164(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2164(i)
         enddo
         goto 20
      elseif(str.eq."0-4nInJ-40") then
         include "leshouches_R_165.inc"
         iflow=nint(jamp2165(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2165(i)
         enddo
         goto 20
      elseif(str.eq."04nInJ40") then
         include "leshouches_R_166.inc"
         iflow=nint(jamp2166(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2166(i)
         enddo
         goto 20
      elseif(str.eq."0-3nInJ-30") then
         include "leshouches_R_167.inc"
         iflow=nint(jamp2167(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2167(i)
         enddo
         goto 20
      elseif(str.eq."03nInJ30") then
         include "leshouches_R_168.inc"
         iflow=nint(jamp2168(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2168(i)
         enddo
         goto 20
      elseif(str.eq."0-5nInJ-50") then
         include "leshouches_R_169.inc"
         iflow=nint(jamp2169(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2169(i)
         enddo
         goto 20
      elseif(str.eq."05nInJ50") then
         include "leshouches_R_170.inc"
         iflow=nint(jamp2170(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2170(i)
         enddo
         goto 20
      elseif(str.eq."00nInJ1-1") then
         include "leshouches_R_171.inc"
         iflow=nint(jamp2171(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2171(i)
         enddo
         goto 20
      elseif(str.eq."00nInJ2-2") then
         include "leshouches_R_172.inc"
         iflow=nint(jamp2172(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2172(i)
         enddo
         goto 20
      elseif(str.eq."00nInJ4-4") then
         include "leshouches_R_173.inc"
         iflow=nint(jamp2173(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2173(i)
         enddo
         goto 20
      elseif(str.eq."00nInJ3-3") then
         include "leshouches_R_174.inc"
         iflow=nint(jamp2174(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2174(i)
         enddo
         goto 20
      elseif(str.eq."00nInJ5-5") then
         include "leshouches_R_175.inc"
         iflow=nint(jamp2175(0))
         jamp2cum(0)=0d0
         do i=1,iflow
            jamp2cum(i)=jamp2cum(i-1)+jamp2175(i)
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