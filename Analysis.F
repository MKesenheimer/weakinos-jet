c############### Analysis.F ############################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### subroutine init_hist ##################################
c book all histograms, will be filled later
      subroutine init_hist
        implicit none
#include "LesHouches.h"
#include "PhysPars.h"
        ! tell this analysis file which program is running it
        character*6 WHCPRG
        common/cWHCPRG/WHCPRG
        ! parameters
        integer n,m
        double precision mm, mp, ptm, ptp
        double precision em, ep
        double precision phim, phip

        call inihists

        ! check program that runs analysis
        print*
        print*, "===================================================="
        if(whcprg.eq."NLO") then
          print*, "fixed-order NLO analysis"
        elseif(whcprg.eq."LHE") then
          print*, "LHE analysis"
          call init_couplings
        elseif(whcprg.eq."HERWIG") then
          print*, "HERWIG analysis"
          print*, "not implemented analysis"
          print*, "no plots will be present at the end of the run"
          call init_couplings
        elseif(whcprg.eq."PYTHIA") then
          print*, "PYTHIA analysis"
          call init_couplings
        endif
        print*, "===================================================="

        ! global parameters
        ! number of bins
        n = 40
        m = 100
        ! mass(A+B)
        mm = dabs(par_Fin1mass) + dabs(par_Fin2mass)
        mp = 4D0*mm
        ! Pt(A), Pt(B), Pt(miss)
        ptm = 0D0
        ptp = ptm + 400D0 
        ! eta
        em = -5D0
        ep = em + 10D0
        ! angle seperation
        phim = 2.85D0
        phip = 3.15D0

        ! No cuts, Test
        call bookupeqbins("xsec",1D0,-0.5D0,0.5D0)

        ! Jcuts, scenario 1:
        ! ptj > 20GeV, eta < 4.5
        call bookupeqbins("xsec(Jcuts1)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("mass(A+B,Jcuts1)",(mp-mm)/n,mm,mp)
        call bookupeqbins("Pt(A,Jcuts1)",(ptp-ptm)/n,ptm,ptp)
        call bookupeqbins("Pt(B,Jcuts1)",(ptp-ptm)/n,ptm,ptp)
        call bookupeqbins("eta(A,Jcuts1)",(ep-em)/n,em,ep)
        call bookupeqbins("eta(B,Jcuts1)",(ep-em)/n,em,ep)
        call bookupeqbins("N(J,Jcuts1)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Jcuts1)",400D0/n,20D0,420D0)
        call bookupeqbins("Pt(J1,Jcuts1,fine)",200D0/n,20D0,220D0)
        call bookupeqbins("eta(J1,Jcuts1)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(J2,Jcuts1)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Jcuts1,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Jcuts1)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Jcuts1,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Jcuts1,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Jcuts1)",400D0/n,0D0,400D0)
        call bookupeqbins("dphi(pMiss,J1,Jcuts1)",(phip-phim)/n,phim,phip)

        ! Jcuts, scenario 2:
        ! ptj > 30GeV, eta < 2.8
        call bookupeqbins("xsec(Jcuts2)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("mass(A+B,Jcuts2)",(mp-mm)/n,mm,mp)
        call bookupeqbins("Pt(A,Jcuts2)",(ptp-ptm)/n,ptm,ptp)
        call bookupeqbins("Pt(B,Jcuts2)",(ptp-ptm)/n,ptm,ptp)
        call bookupeqbins("eta(A,Jcuts2)",(ep-em)/n,em,ep)
        call bookupeqbins("eta(B,Jcuts2)",(ep-em)/n,em,ep)
        call bookupeqbins("N(J,Jcuts2)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Jcuts2)",400D0/n,30D0,430D0)
        call bookupeqbins("Pt(J1,Jcuts2,fine)",200D0/n,30D0,230D0)
        call bookupeqbins("eta(J1,Jcuts2)",6D0/n,-3D0,3D0)
        call bookupeqbins("Pt(J2,Jcuts2)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Jcuts2,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Jcuts2)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Jcuts2,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Jcuts2,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Jcuts2)",400D0/n,0D0,400D0)
        call bookupeqbins("dphi(pMiss,J1,Jcuts2)",(phip-phim)/n,phim,phip)

        ! Mcuts, scenario 1: 
        ! all jets ptj > 30GeV, eta < 4.5
        ! leading jet pt > 120GeV, eta < 2.0
        ! pt_miss > 120GeV
        call bookupeqbins("xsec(Mcuts1)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("N(J,Mcuts1)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Mcuts1)",400D0/n,120D0,520D0)
        call bookupeqbins("Pt(J1,Mcuts1,fine)",200D0/n,20D0,220D0)
        call bookupeqbins("eta(J1,Mcuts1)",5D0/n,-2.5D0,2.5D0)
        call bookupeqbins("Pt(J2,Mcuts1)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Mcuts1,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Mcuts1)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts1,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts1,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Mcuts1)",400D0/n,120D0,520D0)
        call bookupeqbins("dphi(pMiss,J1,Mcuts1)",(phip-phim)/n,phim,phip)

        ! Mcuts, scenario 2: 
        ! all jets ptj > 30GeV, eta < 2.8
        ! leading jet pt > 80GeV
        ! pt_miss > 100GeV
        call bookupeqbins("xsec(Mcuts2)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("N(J,Mcuts2)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Mcuts2)",400D0/n,80D0,480D0)
        call bookupeqbins("Pt(J1,Mcuts2,fine)",200D0/n,80D0,280D0)
        call bookupeqbins("eta(J1,Mcuts2)",6D0/n,-3D0,3D0)
        call bookupeqbins("Pt(J2,Mcuts2)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Mcuts2,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Mcuts2)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts2,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts2,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Mcuts2)",400D0/n,100D0,500D0)
        call bookupeqbins("dphi(pMiss,J1,Mcuts2)",(phip-phim)/n,phim,phip)

        ! Mcuts, scenario 3:
        ! all jets ptj > 30GeV, eta < 2.8
        ! leading jet pt > 150GeV
        ! pt_miss > 100GeV
        call bookupeqbins("xsec(Mcuts3)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("N(J,Mcuts3)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Mcuts3)",400D0/n,150D0,550D0)
        call bookupeqbins("Pt(J1,Mcuts3,fine)",200D0/n,150D0,350D0)
        call bookupeqbins("eta(J1,Mcuts3)",6D0/n,-3D0,3D0)
        call bookupeqbins("Pt(J2,Mcuts3)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Mcuts3,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Mcuts3)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts3,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts3,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Mcuts3)",400D0/n,100D0,500D0)
        call bookupeqbins("dphi(pMiss,J1,Mcuts3)",(phip-phim)/n,phim,phip)

        ! Mcuts, scenario 4: 
        ! leading jet pt > 100GeV, eta < 4.5
        ! veto on second jet with pt < 30GeV
        ! pt_miss > 85GeV
        call bookupeqbins("xsec(Mcuts4)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("N(J,Mcuts4)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Mcuts4)",400D0/n,150D0,550D0)
        call bookupeqbins("Pt(J1,Mcuts4,fine)",200D0/n,30D0,230D0)
        call bookupeqbins("eta(J1,Mcuts4)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(J2,Mcuts4)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Mcuts4,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Mcuts4)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts4,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts4,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Mcuts4)",400D0/n,85D0,485D0)
        call bookupeqbins("dphi(pMiss,J1,Mcuts4)",(phip-phim)/n,phim,phip)

        ! Mcuts, scenario 5:
        ! all jets ptj > 20GeV, eta < 4.5 
        ! leading jet pt > 100GeV, eta < 4.5
        ! pt_miss > 85GeV
        call bookupeqbins("xsec(Mcuts5)",1D0,-0.5D0,0.5D0)
        call bookupeqbins("N(J,Mcuts5)",1D0,-0.5D0,3.5D0)
        call bookupeqbins("Pt(J1,Mcuts5)",400D0/n,100D0,500D0)
        call bookupeqbins("Pt(J1,Mcuts5,fine)",200D0/n,100D0,300D0)
        call bookupeqbins("eta(J1,Mcuts5)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(J2,Mcuts5)",400D0/n,0D0,400D0)
        call bookupeqbins("Pt(J2,Mcuts5,fine)",200D0/m,0D0,200D0)
        call bookupeqbins("eta(J2,Mcuts5)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts5,10)",10D0/n,-5D0,5D0)
        call bookupeqbins("eta(J2,Mcuts5,20)",10D0/n,-5D0,5D0)
        call bookupeqbins("Pt(Miss,Mcuts5)",400D0/n,85D0,485D0)
        call bookupeqbins("dphi(pMiss,J1,Mcuts5)",(phip-phim)/n,phim,phip)
      end
c############### end subroutine init_hist ##############################

c############### subroutine analysis ###################################
c extract all data required for the histograms
c calculate quantities that shall be plotted and fill histograms
      subroutine analysis(dsig0)
        implicit none
#include "LesHouches.h"
#include "hepevt.h"
#include "pwhg_weights.h"
#include "Flags.h"
#include "pwhg_flg.h"
#include "PhysPars.h"
        integer  dsigdim
        parameter (dsigdim = 20)
        double precision dsig(dsigdim), dsig0 ! total cross section
        ! tells if subroutine is called for the first time
        logical ini
        data ini/.true./
        save ini
        ! tell this analysis file which program is running it
        character *6 WHCPRG
        common/cWHCPRG/WHCPRG
        data WHCPRG/'NLO'/ ! default, should be set when analysis is called
        ! external functions
        double precision pt 
        logical isdetectable
        external pt, isdetectable
        ! kinematical quantities
        double precision p_finA(0:3), p_finB(0:3), p_finAfinB(0:3)
        double precision pt_finA, pt_finB
        double precision eta_finA, eta_finB
        double precision y_finA, y_finB
        double precision m_finAfinB
        double precision pt_finAfinB, y_finAfinB
        ! jet parameters
        integer mjets, num_vis_jets, maxjet
        parameter (maxjet=2048)
        double precision kt(maxjet), eta(maxjet), rap(maxjet)
        double precision phi(maxjet), pj(4,maxjet), ptrel(maxjet)
        double precision p_jet(0:3,maxjet)
        ! PDG and HEP IDs
        integer id_finA, id_finB
        integer ihep_finA, ihep_finB        
        ! variables for missing energy
        double precision p_miss(0:3), pt_miss
        ! cuts
        ! ptjmin is a technical cut
        double precision ptjmin, R
        common /jetcuts/ptjmin, R
        ! cut logic
        logical JcutsPassed, McutsPassed
        ! angle seperation of p_miss and p_jet(1)
        double precision dphi, phi_miss, phi_jet
        ! indices
        integer i, j
        ! constants
        double precision pi
        parameter (pi = 4.D0*datan(1.D0))

        ! at first call
        if(ini) then
          ! extract final state types from process id
          call decode_pair(lprup(1),id_finA,id_finB)
          ! check final state
          if(.not.((id_finA.eq.1000022 .or. id_finA.eq. 1000023  .or.
     &               id_finA.eq.1000025 .or. id_finA.eq. 1000035  .or.
     &               id_finA.eq.1000024 .or. id_finA.eq.-1000024  .or.
     &               id_finA.eq.1000037 .or. id_finA.eq.-1000037) .and.
     &              (id_finB.eq.1000022 .or. id_finB.eq. 1000023  .or.
     &               id_finB.eq.1000025 .or. id_finB.eq. 1000035  .or.
     &               id_finB.eq.1000024 .or. id_finB.eq.-1000024  .or.
     &               id_finB.eq.1000037 .or. id_finB.eq.-1000037))) then
            print*, "analysis: final state not supported"
            stop
          endif
          ! if everything is ok print status
          print*, "extracted from process ID: ", lprup(1)
          print*, "the final state pair: ", id_finA, ", ", id_finB
          ini = .false.
        endif

        ! from pico to femto
        !dsig0 = dsig0*1000D0

        ! setup mutliple weights analysis
        call multi_plot_setup(dsig0,dsig,dsigdim)
        
        ! return if no entries in dsig
        if(sum(abs(dsig)).eq.0) return

        ! find final state 4-momenta
        call get4momentum_fin_pair(id_finA,p_finA,id_finB,p_finB)

        ! calculate final state kinematical quantities:
        ! final state A, B transverse momentum
        pt_finA = pt(p_finA)
        pt_finB = pt(p_finB)
        ! final states pseudorapidity
        call getpseudorap(p_finA,eta_finA)
        call getpseudorap(p_finB,eta_finB)
        ! final states rapidity
        call getrapidity(p_finA,y_finA)
        call getrapidity(p_finB,y_finB)
        ! invariant mass of the final state system
        p_finAfinB(:) = p_finB(:)+p_finA(:)
        call getinvmass(p_finAfinB,m_finAfinB)
        ! transverse momentum of the final state system
        pt_finAfinB = pt(p_finAfinB)
        ! rapidity of the final state system
        call getrapidity(p_finAfinB,y_finAfinB) 
       
        ! build jets
        ! technical jet parameters for fastjet
        ptjmin = 1D0
        mjets = 40
        R = 0.4D0
        do j = 1,maxjet
          kt(j)    = 0D0
          eta(j)   = 0D0
          rap(j)   = 0D0
          phi(j)   = 0D0
          pj(:,j)  = 0D0
          ptrel(j) = 0D0
        enddo
        call buildjets(mjets,kt,eta,rap,phi,pj,ptrel)
        do i=1,mjets
          p_jet(0,i) = pj(4,i)
          do j=1,3
            p_jet(j,i) = pj(j,i)
          enddo
        enddo

        ! --------------------------------------------------------------
        ! No cuts, test
        ! --------------------------------------------------------------
        call filld("xsec",0D0,dsig)

        ! --------------------------------------------------------------
        ! Jcuts, scenario 1: 
        ! ptj > 20GeV, eta < 4.5
        ! --------------------------------------------------------------
        call get_num_vis_jets(kt(:),eta(:),mjets,20D0,4.5D0,num_vis_jets)
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,20D0,4.5D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        JcutsPassed = .true.
        if(mjets.lt.1) JcutsPassed = .false.
        if(kt(1).lt.20D0) JcutsPassed = .false.
        if(dabs(eta(1)).gt.4.5D0) JcutsPassed = .false.
        if(JcutsPassed) then
          call filld("xsec(Jcuts1)",0D0,dsig)
          call filld("mass(A+B,Jcuts1)",m_finAfinB,dsig)
          call filld("Pt(A,Jcuts1)",pt_finA,dsig)
          call filld("Pt(B,Jcuts1)",pt_finB,dsig)
          call filld("eta(A,Jcuts1)",eta_finA,dsig)
          call filld("eta(B,Jcuts1)",eta_finB,dsig)
          call filld("N(J,Jcuts1)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Jcuts1)",kt(1),dsig)
          call filld("Pt(J1,Jcuts1,fine)",kt(1),dsig)
          call filld("eta(J1,Jcuts1)",eta(1),dsig)
          call filld("Pt(J2,Jcuts1)",kt(2),dsig)
          call filld("Pt(J2,Jcuts1,fine)",kt(2),dsig)
          call filld("eta(J2,Jcuts1)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Jcuts1,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Jcuts1,20)",eta(2),dsig)
          call filld("Pt(Miss,Jcuts1)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Jcuts1)",dphi,dsig)
        endif

        ! --------------------------------------------------------------
        ! Jcuts, scenario 2: 
        ! ptj > 30GeV, eta < 2.8
        ! --------------------------------------------------------------
        call get_num_vis_jets(kt(:),eta(:),mjets,30D0,2.8D0,num_vis_jets)
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,30D0,2.8D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        JcutsPassed = .true.
        if(mjets.lt.1) JcutsPassed = .false.
        if(kt(1).lt.30D0) JcutsPassed = .false.
        if(dabs(eta(1)).gt.2.8D0) JcutsPassed = .false.
        if(JcutsPassed) then
          call filld("xsec(Jcuts2)",0D0,dsig)
          call filld("mass(A+B,Jcuts2)",m_finAfinB,dsig)
          call filld("Pt(A,Jcuts2)",pt_finA,dsig)
          call filld("Pt(B,Jcuts2)",pt_finB,dsig)
          call filld("eta(A,Jcuts2)",eta_finA,dsig)
          call filld("eta(B,Jcuts2)",eta_finB,dsig)
          call filld("N(J,Jcuts2)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Jcuts2)",kt(1),dsig)
          call filld("Pt(J1,Jcuts2,fine)",kt(1),dsig)
          call filld("eta(J1,Jcuts2)",eta(1),dsig)
          call filld("Pt(J2,Jcuts2)",kt(2),dsig)
          call filld("Pt(J2,Jcuts2,fine)",kt(2),dsig)
          call filld("eta(J2,Jcuts2)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Jcuts2,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Jcuts2,20)",eta(2),dsig)
          call filld("Pt(Miss,Jcuts2)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Jcuts2)",dphi,dsig)
        endif

        ! --------------------------------------------------------------
        ! Mcuts, scenario 1: 
        ! all jets ptj > 30GeV, eta < 4.5
        ! leading jet pt > 120GeV, eta < 2.0
        ! pt_miss > 120GeV
        ! --------------------------------------------------------------
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,30D0,4.5D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        McutsPassed = .true.
        if(mjets.lt.1) McutsPassed = .false.
        if(kt(1).lt.120d0) McutsPassed = .false.
        if(dabs(eta(1)).gt.2.0d0) McutsPassed = .false.
        if(pt_miss.lt.120d0) McutsPassed = .false.
        if(McutsPassed) then
          call filld("xsec(Mcuts1)",0D0,dsig)
          call filld("N(J,Mcuts1)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Mcuts1)",kt(1),dsig)
          call filld("Pt(J1,Mcuts1,fine)",kt(1),dsig)
          call filld("eta(J1,Mcuts1)",eta(1),dsig)
          call filld("Pt(J2,Mcuts1)",kt(2),dsig)
          call filld("Pt(J2,Mcuts1,fine)",kt(2),dsig)
          call filld("eta(J2,Mcuts1)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Mcuts1,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Mcuts1,20)",eta(2),dsig)
          call filld("Pt(Miss,Mcuts1)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Mcuts1)",dphi,dsig)
        endif

        ! --------------------------------------------------------------
        ! Mcuts, scenario 2: 
        ! all jets ptj > 30GeV, eta < 2.8
        ! leading jet pt > 80GeV
        ! pt_miss > 100GeV
        ! --------------------------------------------------------------
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,30D0,2.8D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        McutsPassed = .true.
        if(mjets.lt.1) McutsPassed = .false.
        if(kt(1).lt.80d0) McutsPassed = .false.
        if(dabs(eta(1)).gt.2.8d0) McutsPassed = .false.
        if(pt_miss.lt.100d0) McutsPassed = .false.
        if(McutsPassed) then
          call filld("xsec(Mcuts2)",0D0,dsig)
          call filld("N(J,Mcuts2)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Mcuts2)",kt(1),dsig)
          call filld("Pt(J1,Mcuts2,fine)",kt(1),dsig)
          call filld("eta(J1,Mcuts2)",eta(1),dsig)
          call filld("Pt(J2,Mcuts2)",kt(2),dsig)
          call filld("Pt(J2,Mcuts2,fine)",kt(2),dsig)
          call filld("eta(J2,Mcuts2)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Mcuts2,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Mcuts2,20)",eta(2),dsig)
          call filld("Pt(Miss,Mcuts2)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Mcuts2)",dphi,dsig)
        endif

        ! --------------------------------------------------------------
        ! Mcuts, scenario 3:
        ! all jets ptj > 30GeV, eta < 2.8
        ! leading jet pt > 150GeV
        ! pt_miss > 100GeV
        ! --------------------------------------------------------------
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,30D0,2.8D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        McutsPassed = .true.
        if(mjets.lt.1) McutsPassed=.false.
        if(kt(1).lt.150d0) McutsPassed=.false.
        if(dabs(eta(1)).gt.2.8d0) McutsPassed=.false.
        if(pt_miss.lt.100d0) McutsPassed=.false.
        if(McutsPassed) then
          call filld("xsec(Mcuts3)",0D0,dsig)
          call filld("N(J,Mcuts3)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Mcuts3)",kt(1),dsig)
          call filld("Pt(J1,Mcuts3,fine)",kt(1),dsig)
          call filld("eta(J1,Mcuts3)",eta(1),dsig)
          call filld("Pt(J2,Mcuts3)",kt(2),dsig)
          call filld("Pt(J2,Mcuts3,fine)",kt(2),dsig)
          call filld("eta(J2,Mcuts3)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Mcuts3,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Mcuts3,20)",eta(2),dsig)
          call filld("Pt(Miss,Mcuts3)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Mcuts3)",dphi,dsig)
        endif

        ! --------------------------------------------------------------
        ! Mcuts, scenario 4: 
        ! leading jet pt > 100GeV, eta < 4.5
        ! veto on second jet with pt < 30GeV
        ! pt_miss > 85GeV
        ! --------------------------------------------------------------
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,100D0,4.5D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        McutsPassed = .true.
        if(mjets.lt.1) McutsPassed = .false.
        if(kt(1).lt.100d0) McutsPassed = .false.
        if(kt(2).gt.30d0) McutsPassed = .false.
        if(dabs(eta(1)).gt.4.5d0) McutsPassed = .false.
        if(pt_miss.lt.85d0) McutsPassed = .false.
        if(McutsPassed) then
          call filld("xsec(Mcuts4)",0D0,dsig)
          call filld("N(J,Mcuts4)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Mcuts4)",kt(1),dsig)
          call filld("Pt(J1,Mcuts4,fine)",kt(1),dsig)
          call filld("eta(J1,Mcuts4)",eta(1),dsig)
          call filld("Pt(J2,Mcuts4)",kt(2),dsig)
          call filld("Pt(J2,Mcuts4,fine)",kt(2),dsig)
          call filld("eta(J2,Mcuts4)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Mcuts4,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Mcuts4,20)",eta(2),dsig)
          call filld("Pt(Miss,Mcuts4)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Mcuts4)",dphi,dsig)
        endif

        ! --------------------------------------------------------------
        ! Mcuts, scenario 5:
        ! all jets ptj > 20GeV, eta < 4.5 
        ! leading jet pt > 100GeV, eta < 4.5
        ! pt_miss > 85GeV
        ! --------------------------------------------------------------
        if((.not.isdetectable(id_finA)).and.(.not.isdetectable(id_finB))) then
          call obs_miss(p_jet(:,:),mjets,20D0,4.5D0,p_miss(:),pt_miss)
          call angle_sep(p_miss(:),p_jet(:,1),dphi)
        endif
        ! fill histograms
        McutsPassed = .true.
        if(mjets.lt.1) McutsPassed = .false.
        if(kt(1).lt.100d0) McutsPassed = .false.
        if(dabs(eta(1)).gt.4.5d0) McutsPassed = .false.
        if(pt_miss.lt.85d0) McutsPassed = .false.
        if(McutsPassed) then
          call filld("xsec(Mcuts5)",0D0,dsig)
          call filld("N(J,Mcuts5)",dble(num_vis_jets),dsig)
          call filld("Pt(J1,Mcuts5)",kt(1),dsig)
          call filld("Pt(J1,Mcuts5,fine)",kt(1),dsig)
          call filld("eta(J1,Mcuts5)",eta(1),dsig)
          call filld("Pt(J2,Mcuts5)",kt(2),dsig)
          call filld("Pt(J2,Mcuts5,fine)",kt(2),dsig)
          call filld("eta(J2,Mcuts5)",eta(2),dsig)
          if(kt(2).gt.10D0) call filld("eta(J2,Mcuts5,10)",eta(2),dsig)
          if(kt(2).gt.20D0) call filld("eta(J2,Mcuts5,20)",eta(2),dsig)
          call filld("Pt(Miss,Mcuts5)",pt_miss,dsig)
          call filld("dphi(pMiss,J1,Mcuts5)",dphi,dsig)
        endif
      end
c############### end subroutine analysis ###############################

c############### subroutine get_num_vis_jets ###########################
      subroutine get_num_vis_jets(kt,eta,mjets,ktmin,etamax,n)
        implicit none
        integer i, mjets, n, maxjet
        parameter (maxjet=2048)
        double precision kt(maxjet), eta(maxjet)
        double precision ktmin, etamax
        n = 0
        do i = 1,mjets
          if((kt(i).gt.ktmin).and.(dabs(eta(i)).lt.etamax)) then
            n = n+1
          endif
        enddo
      end
c############### end subroutine get_num_vis_jets #######################

c############### subroutine obs_miss ###################################
      subroutine obs_miss(p_detect,m,ptmin,etamax,p_miss,pt_miss)
        implicit none
        integer m,i
        double precision p_detect(0:3,m)
        double precision pt,eta,ptmin,etamax
        double precision p_miss(0:3),pt_miss
        p_miss(:) = 0D0
        do i=1,m
          call getpseudorap(p_detect(:,i),eta)
          call get_pt(p_detect(:,i),pt)
          if((pt.gt.ptmin).and.(dabs(eta).lt.etamax)) then
            p_miss(:) = p_miss(:) - p_detect(:,i)
          endif
        enddo
        call get_pt(p_miss(:),pt_miss)
      end
c############### end subroutine obs_miss ###############################

c############### subroutine angle_sep ##################################
      subroutine angle_sep(p1,p2,dphi)
        implicit none
        double precision p1(0:3),p2(0:3),dphi
        double precision phi1,phi2
        ! constants
        double precision pi
        parameter (pi = 4.D0*datan(1.D0))
        call getazimuth(p1(:),phi1)
        call getazimuth(p2(:),phi2)
        dphi = phi2 - phi1
        if(dphi.lt.0) dphi = 2*pi + dphi ! redefine [-2pi,2pi] -> [0,2pi]
        if(dphi.gt.pi) dphi = 2*pi - dphi ! redefine [0,2pi] -> [0,pi]
      end
c############### end subroutine angle_sep ##############################

c############### subroutine getihep ####################################
c takes PDG ID and returns the index of this particle in the HEPEVT-list
c call this routine multiple times to get for one particle type
c all occurent indices in the HEPEVT-list
c st is the hepevent status:
c 1: particle undecayed in final state
c 2: particle decayed and not in final state
c use the flag reset to reset the search and to begin at the beginning
      subroutine getihep(PDG_ID,st,reset,HEP_i)
        implicit none
#include "hepevt.h"
        integer PDG_ID, HEP_i, ihep, st
        ! store here the index of the already found particle id's
        ! (to keep track which particle was already found,
        !  -> it is possible to call this subroutine several times with 
        !  the same PDG_ID and get the next HEPEVT-id)
        integer foundlist(nmxhep)
        logical reset
        save foundlist
        if(reset) then
          do ihep=1,nhep
            foundlist = 0
          enddo
        endif
        HEP_i = 0
        do ihep=1,nhep
          if(isthep(ihep).eq.st) then
            if( (foundlist(ihep).ne.idhep(ihep)) .and.
     &          (idhep(ihep).eq.PDG_ID) ) then
              HEP_i = ihep
              foundlist(ihep) = idhep(ihep)
              return
            endif  
          endif
        enddo
      end
c############### end subroutine getihep ################################

c############### subroutine getdaughters ###############################
c takes PDG ID of mother and returns the PDG ID and the momenta of two
c daughters.
c calling this routine for the first time reset must be set to .true.
c calling this routine a second time with reset equals to .false. will
c return the daughters of the next mother.
c example:
c Mother 1 with PDG = 1000023 decays into 1000022 and -14
c Mother 2 with PDG = 1000023 decays into 1000022 and -16
c Mother 3 with PDG = 1000024 decays into 1000022 and -15
c
c call getdaughters(1000023,.true.,D1PDG_ID,D2PDG_ID)2c   -> D1PDG_ID = 1000022 and D2PDG_ID = -14
c call getdaughters(1000023,.false.,D1PDG_ID,D2PDG_ID)
c   -> D1PDG_ID = 1000022 and D2PDG_ID = -16
c call getdaughters(1000024,.false.,D1PDG_ID,D2PDG_ID)
c   -> D1PDG_ID = 1000022 and D2PDG_ID = -15
      subroutine getdaughters(MPDG_ID,reset,D1PDG_ID,D2PDG_ID,pD1,pD2)
        implicit none
#include "hepevt.h"
        integer MPDG_ID, D1PDG_ID, D2PDG_ID, ihep
        double precision pD1(0:3), pD2(0:3)
        logical reset
        call getihep(MPDG_ID,2,reset,ihep)
        D1PDG_ID = idhep(jdahep(1,ihep))
        D2PDG_ID = idhep(jdahep(2,ihep))
        call get4momentum_ihep(jdahep(1,ihep),pD1)
        call get4momentum_ihep(jdahep(2,ihep),pD2)
      end
c############### end subroutine getdaughters ###########################

c############### subroutine isdecayed ##################################
c takes PDG ID and returns if the particle decayed
      subroutine isdecayed(PDG_ID,fdec)
        implicit none
#include "hepevt.h"
        integer PDG_ID, ihep
        logical fdec
        fdec = .false.
        do ihep=1,nhep
          if(isthep(ihep).eq.2) then ! hepevent status 2 -> particle decayed
            if(idhep(ihep).eq.PDG_ID) then
              fdec = .true.
            endif
          endif
        enddo
      end
c############### end subroutine isdecayed ##############################

c############### subroutine get4momentum_sortedpt ######################
c takes PDG ID
c returns four-momentum of undecayed particle with this ID sorted by
c pt. Select with iorder the pt occurence:
c 1: highest pt
c 2: second highest pt
c 3: third highest pt ...
      subroutine get4momentum_sortedpt(PDG_ID,iorder,p)
        implicit none
        integer PDG_ID, n, nmax, mu
        parameter (nmax=200)
        integer list(nmax),iorder
        double precision p(0:3)
        do mu=0,3
          p(mu) = 0d0
        enddo
        call getptsortedlist(PDG_ID,n,list)
        if(list(iorder).ne.0) then  ! only if particle with PDG_ID is found
          ! take four-momentum of first particle in list (largest pt)
          call get4momentum_ihep(list(iorder),p)
        endif
      end
c############### end subroutine get4momentum_sortedpt ##################

c############### subroutine get4momentum_largestpt #####################
c takes PDG ID
c returns four-momentum of undecayed particle with this ID and largest pt
      subroutine get4momentum_largestpt(PDG_ID,p)
        implicit none
        integer PDG_ID
        double precision p(0:3)
        call get4momentum_sortedpt(PDG_ID,1,p)
      end
c############### end subroutine get4momentum_largestpt #################

c############### subroutine getptsortedlist ############################
c takes PDG ID
c returns list with ihep numbers of these particles sorted by pt in list
      subroutine getptsortedlist(PDG_ID,n,list)
        implicit none
#include "hepevt.h"
        integer PDG_ID, n, nmax, ihep
        parameter (nmax=200)
        integer list(nmax)
        do n=1,nmax
          list(n) = 0
        enddo
        ! determine number of undecayed particles with PDG_ID in event
        n = 0
        do ihep=1,nhep
          if(isthep(ihep).eq.1) then
            if(idhep(ihep).eq.PDG_ID) then
              n = n+1
            endif
          endif
        enddo
        ! check that maximal number of allowed particles is not exceeded
        if(n.gt.nmax) then
          print*,"more than", nmax, "particles with ID", PDG_ID
          print*,"could not fill pt sorted list"
        else
          ! fill list with found particles
          n = 0
          do ihep=1,nhep
            if(isthep(ihep).eq.1) then
              if(idhep(ihep).eq.PDG_ID) then
                n = n+1
                list(n) = ihep
              endif
            endif
          enddo
          ! sort by pt
          call sortbypt(n,list)
        endif
      end
c############### end subroutine getptsortedlist ########################

c############### subroutine sortbypt ###################################
c sorts array iarr of ihep numbers by pt (decreasing)
      subroutine sortbypt(n,iarr)
        implicit none
#include "hepevt.h"
        integer n,iarr(n)
        integer j,k
        double precision tmp,pt_m(nmxhep)
        logical touched
        double precision pt
        external pt
        do j=1,n
          pt_m(j) = dsqrt(phep(1,iarr(j))**2+phep(2,iarr(j))**2)
        enddo
        ! bubble sort
        touched = .true.
        do while(touched)
          touched = .false.
          do j=1,n-1
            if(pt_m(j).lt.pt_m(j+1)) then
              k = iarr(j)
              iarr(j) = iarr(j+1)
              iarr(j+1) = k
              tmp = pt_m(j)
              pt_m(j) = pt_m(j+1)
              pt_m(j+1) = tmp
              touched = .true.
            endif
          enddo
        enddo
      end
c############### end subroutine sortbypt ###############################

c############### subroutine getPt ######################################
c calculate Pt, it is assumed p(0) = E, p(1:3) = vec{p}
      double precision function pt(p)
        implicit none
        double precision p(0:3)
        pt = dsqrt(p(1)**2+p(2)**2)
      end
      subroutine get_pt(p,pt)
        implicit none
        double precision p(0:3),pt
        pt = dsqrt(p(1)**2+p(2)**2)
      end
c############### end subroutine getPt ##################################

c############### subroutine get4momentum_ihep ##########################
c takes ihep number of a particle in the HEPEVT common block
c puts its 4-momentum into p
c the phep momenta is in the laboratory frame
      subroutine get4momentum_ihep(ihep,p)
        implicit none
#include "hepevt.h"
#include "nlegborn.h"
#include "pwhg_kn.h"
        integer ihep, mu
        double precision p(0:3)
        do mu=0,3
          p(mu)=0D0
        enddo
        if(ihep.lt.0 .or. ihep.gt.nhep) then
          print*, "Warning: no particle with ihep number", ihep,
     &              "in current event"
          print*, "could not determine four-momentum"
          return
          !stop
        else
          ! p is defined in the laboratory frame (~kn_pborn(i))
          call convert_hep_momenta(phep(:,ihep),p(:))
        endif
      end
c############### end subroutine get4momentum_ihep ######################

c############### subroutine convert_hep_momenta ########################
c converts the hep momentum standard to the conventional momentum
c standard: hep (p_i, E, m) -> conventional (E, p_i)
      subroutine convert_hep_momenta(p_hep,p_con)
        implicit none
        integer mu
        double precision p_hep(1:5)
        double precision p_con(0:3)
        p_con(0) = p_hep(4)
        do mu=1,3
          p_con(mu) = p_hep(mu)
        enddo
      end
c############### end subroutine convert_hep_momenta ####################

c############### subroutine get4momentum_fin_pair ######################
c takes particle data group identity code of the final state pair
c puts 4-momentum of first and second particle
c in HEPEVT common block with this ID into p1 and p2
c this routine can be used even if the IDs of particle 1 and 2 are the same
c (this was not possible in dislepton_jet)
      subroutine get4momentum_fin_pair(PDG_ID1,p1,PDG_ID2,p2)
        implicit none
#include "hepevt.h"
#include "PhysPars.h"
        integer PDG_ID1,PDG_ID2
        ! output
        double precision p1(0:3), p2(0:3)
        double precision momsq
        external momsq
        ! indices
        integer ihep, imu
        ! store id of found particle?
        integer found1, found2
        ! check on-shell condition
        double precision relerror
        do imu=0,3
          p1(imu) = 0D0
          p2(imu) = 0D0
        enddo
        found1 = -1
        found2 = -1
        do ihep=1,nhep
          if(.not.(found2 .eq. found1)) then
            if(PDG_ID2.eq.idhep(ihep)) then
              call get4momentum_ihep(ihep,p2)
              found2=ihep
            endif
          endif
          if(found1 .eq. -1) then
            if(PDG_ID1.eq.idhep(ihep)) then
              call get4momentum_ihep(ihep,p1)
              found1=ihep
            endif
          endif
        enddo
        ! check if the extracted particles are indeed the final state particles
        relerror = (dsqrt(momsq(p1))-dabs(par_Fin1mass))
     &             /(dsqrt(momsq(p1))+dabs(par_Fin1mass))
        if( dabs(relerror) .gt. 1D-6 ) then
          print*,"error: wrong particle extracted in "
          print*,"subroutine get4momentum_fin_pair"
          print*,"p1**2 = ",momsq(p1)
          print*,"m1**2 = ",par_Fin1mass**2
          stop
        endif
        relerror = (dsqrt(momsq(p2))-dabs(par_Fin2mass))
     &             /(dsqrt(momsq(p2))+dabs(par_Fin2mass))
        if( dabs(relerror) .gt. 1D-6 ) then
          print*,"error: wrong particle extracted in "
          print*,"subroutine get4momentum_fin_pair"
          print*,"p2**2 = ",momsq(p2)
          print*,"m2**2 = ",par_Fin2mass**2
          stop
        endif
        if(found1 .eq. found2) then
          print*, "error in get4momentum_fin_pair."
          print*, "idhep is the same for particle 1 and particle 2,"
          print*, "but should be different."
          print*, "idhep1", found1
          print*, "idhep2", found2
          stop
        endif
        if(found1 .eq. -1) then
          print*, "particle with PDG ID", PDG_ID1, "not found"
          print*, "could not determine four-momentum"
          stop
        endif
        if(found2 .eq. -1) then
          print*, "particle with PDG ID", PDG_ID2, "not found"
          print*, "could not determine four-momentum"
          stop
        endif
      end
c############### end subroutine get4momentum_fin_pair ##################

c############### function rsepn ########################################
c calculate the separation in the lego plot between the two momenta
c p1 and p2
      function rsepn(p1,p2)
        implicit none
        include 'pwhg_math.h'  
        double precision rsepn,p1(0:3),p2(0:3)
        double precision y1,phi1,y2,phi2
        double precision delphi
        y1 = 0D0
        y2 = 0D0
        delphi = 0D0
        if(p1(0).eq.0D0 .or. p2(0).eq.0D0) then
          goto 111
        endif

        call getazimuth(p1(:),phi1)
        call getazimuth(p2(:),phi2)
        call getrapidity(p1(:),y1)
        call getrapidity(p2(:),y2)
      
        delphi = dabs(phi1-phi2)
        if(delphi.gt.pi) then
          delphi = 2*pi-delphi
        endif
        if(delphi.lt.0 .or. delphi.gt.pi) then
          print*,' problem in rsepn. delphi = ',delphi
        endif
 111    continue
        rsepn = dsqrt((y1-y2)**2 + delphi**2)
      end
c############### function rsepn ########################################

c############### subroutine getazimuth #################################
c calculates azimuth phi in [0,2pi] in respect to x-axis
      subroutine getazimuth(p,phi)
        implicit none
#include "pwhg_math.h"
        double precision p(0:3),phi
        phi=0D0
        if(p(1).gt.0D0) then
          if(p(2).ge.0D0) phi = datan(p(2)/p(1))
          if(p(2).lt.0D0) phi = datan(p(2)/p(1))+2D0*pi
        elseif(p(1).lt.0D0) then
          phi = datan(p(2)/p(1))+pi
        elseif(p(1).eq.0D0) then
          if(p(2).gt.0D0) phi = pi/2D0
          if(p(2).lt.0D0) phi = -pi/2D0
        endif
      end
c############### end subroutine getazimuth #############################

c############### subroutine getrapidity ################################
      subroutine getrapidity(p,y)
        implicit none
        double precision p(0:3),y
        y = 0.5D0*dlog((p(0)+p(3))/(p(0)-p(3)))
      end
c############### end subroutine getrapidity ############################

c############### subroutine getinvmass #################################
      subroutine getinvmass(p,m)
        implicit none
        double precision p(0:3),m
        m = dsqrt(dabs((p(0)**2-p(3)**2)-p(1)**2-p(2)**2))
      end
c############### end subroutine getinvmass #############################

c############### subroutine getpseudorap ###############################
      subroutine getpseudorap(p,eta)
        implicit none
        double precision p(0:3),eta,pt,th
        double precision eps
        parameter (eps=1.d-5)
        pt = dsqrt(p(1)**2+p(2)**2)
        if(pt.lt.eps.and.dabs(p(3)).lt.eps)then
          eta = sign(1.D0,p(3))*1.d8
        elseif(pt.lt.eps) then   ! MK: added this elseif
          eta = sign(1.D0,p(3))*1.d8
        else
          th  = datan2(pt,p(3))
          eta = -dlog(dtan(th/2.D0))
        endif
      end
c############### end subroutine getpseudorap ###########################      

c############### subroutine buildjets ##################################
c jets are orderd by decreasing pt
      subroutine buildjets(mjets,kt,eta,rap,phi,pjet,ptrel)
        implicit none
#include "hepevt.h"
        integer mjets
        double precision kt(mjets),eta(mjets),rap(mjets),phi(mjets)
        double precision pjet(4,mjets),ptrel(mjets)
        integer   maxtrack,maxjet
        parameter (maxtrack=2048,maxjet=2048)
        double precision  ptrack(4,maxtrack),pj(4,maxjet)
        integer   jetvec(maxtrack),itrackhep(maxtrack)
        integer   ntracks,njets
        integer   j,k,mu
        double precision palg,pp,tmp
        double precision ptjmin,R
        common /jetcuts/ptjmin,R
        logical isjetparticle

        ! Initialize arrays and counters for output jets
        do j=1,maxtrack
          do mu=1,4
            ptrack(mu,j) = 0D0
          enddo
          jetvec(j) = 0
        enddo
        ntracks = 0
        do j=1,mjets
          do mu=1,4
            pjet(mu,j) = 0D0
            pj(mu,j) = 0D0
          enddo
        enddo
        ! Extract final state particles to feed to jet finder
        do j=1,nhep
          if(isthep(j).eq.1 .and. isjetparticle(idhep(j))) then
            if(ntracks.eq.maxtrack) then
              print*, "analyze: need to increase maxtrack!"
              print*, "ntracks: ",ntracks
              stop
            endif
            ntracks = ntracks+1
            itrackhep(ntracks) = j
            do mu=1,4
              ptrack(mu,ntracks) = phep(mu,j)
            enddo
          endif
        enddo
        if(ntracks.eq.0) then
          mjets=0
          return
        endif

        ! note: ptjmin and R are in common block "jetcuts"
        ! palg=-1 -> anti-kT (c.f. ZZ code):
        palg=-1
        call fastjetppgenkt(ptrack,ntracks,R,palg,ptjmin,pjet,njets,
     &                      jetvec)
        mjets = min(mjets,njets)
        if(njets.eq.0) return
        ! check consistency
        do k=1,ntracks
          if(jetvec(k).gt.0) then
            do mu=1,4
              pj(mu,jetvec(k)) = pj(mu,jetvec(k))+ptrack(mu,k)
            enddo
          endif
        enddo
        tmp = 0
        do j=1,mjets
          do mu=1,4
            tmp = tmp+dabs(pj(mu,j)-pjet(mu,j))
          enddo
        enddo
        if(tmp.gt.1d-4) then
          print*, "error in buildjets:894"
        endif
        ! computing arrays of useful kinematics quantities for hardest jets
        do j=1,mjets
          kt(j) = dsqrt(pjet(1,j)**2+pjet(2,j)**2)
          pp = dsqrt(kt(j)**2+pjet(3,j)**2)
          eta(j) = 0.5D0*dlog((pjet(4,j)+pjet(3,j))/
     &                        (pjet(4,j)-pjet(3,j)))
          rap(j) = 0.5D0*dlog((pjet(4,j)+pjet(3,j))/
     &                        (pjet(4,j)-pjet(3,j)))
          phi(j) = datan2(pjet(2,j),pjet(1,j))
        enddo
        call computeptrel(ptrack,ntracks,rap,kt,phi,mjets,jetvec,ptrel)
      end
c############### end subroutine buildjets ##############################

c############### subroutine isjetparticle ##############################
c determines if particle contributes to a jet
      logical function isjetparticle(PDG_ID)
        implicit none
        integer PDG_ID
        logical injetwhitelist, injetblacklist
        isjetparticle = .false.
        ! find out if particle is in
        ! jetwhitelist: particles that contribute to a jet
        ! jetblacklist: particles that do not contribute to a jet
        if(injetwhitelist(PDG_ID)) then
          isjetparticle = .true.
        else if(injetblacklist(PDG_ID)) then
          isjetparticle = .false.
        else
          print*, "could not determine if particle with ID", PDG_ID
          print*, "contributes to a jet"
          stop
        endif
      end
c############### end subroutine isjetparticle ##########################

c############### function injetwhitelist ###############################
c determines if particle is in whitelist of particles that contribute
c to a jet
      logical function injetwhitelist(PDG_ID)
        implicit none
        integer PDG_ID, id
        injetwhitelist = .false.
        id = iabs(PDG_ID)
        ! the following particles contribute to a jet
        ! quarks
        if(1.le.id .and. id.le.6) injetwhitelist = .true.
        ! photon is not in whitelist
        if(id.eq.22) injetwhitelist = .false.
        ! gluon
        if(id.eq.21) injetwhitelist = .true.
        if(id.eq.0) injetwhitelist = .true.  ! POWHEG numbering
        ! diquarks
        if((1000.le.id .and. id.le.6000)
     &      .and. (mod(id,100).eq.1 .or. mod(id,100).eq.3)) then
          injetwhitelist = .true.
        endif
        ! mesons
        if((111.le.id .and. id.le.600)
     &      .or. (10000.le.id .and. id.le.999999)
     &      .or. (9000000.le.id .and. id.le.9100000)) then
          injetwhitelist = .true.
        endif
        ! baryons
        if((1000.le.id .and. id.le.6000)
     &      .and. (mod(id,10).eq.2 .or. mod(id,10).eq.4)) then
          injetwhitelist = .true.
        endif
      end
c############### end function injetwhitelist ###########################

c############### function injetblacklist ###############################
c determines if particle is in blacklist of particles that do not
c contribute to a jet
      logical function injetblacklist(PDG_ID)
        implicit none
        integer PDG_ID, id
        injetblacklist = .false.
        id = iabs(PDG_ID)
        ! the following particles may occur
        ! but do not contribute to a jet
        ! leptons
        if(id.eq.11) injetblacklist=.true.  ! e
        if(id.eq.12) injetblacklist=.true.  ! nue
        if(id.eq.13) injetblacklist=.true.  ! mu
        if(id.eq.14) injetblacklist=.true.  ! numu
        if(id.eq.15) injetblacklist=.true.  ! tau
        if(id.eq.16) injetblacklist=.true.  ! nu_tau
        ! sleptons
        if(id.eq.1000011) injetblacklist=.true.  ! ~e_L
        if(id.eq.1000012) injetblacklist=.true.  ! ~snue_L
        if(id.eq.1000013) injetblacklist=.true.  ! ~mu_L
        if(id.eq.1000014) injetblacklist=.true.  ! ~snumu_L
        if(id.eq.1000015) injetblacklist=.true.  ! ~stau_1
        if(id.eq.1000016) injetblacklist=.true.  ! ~snu_tau_L
        if(id.eq.2000011) injetblacklist=.true.  ! ~e_R
        if(id.eq.2000013) injetblacklist=.true.  ! ~mu_R
        if(id.eq.2000015) injetblacklist=.true.  ! ~stau_2
        ! neutralinos
        if(id.eq.1000022) injetblacklist=.true.  ! ~n(1)
        if(id.eq.1000023) injetblacklist=.true.  ! ~n(2)
        if(id.eq.1000025) injetblacklist=.true.  ! ~n(3)
        if(id.eq.1000035) injetblacklist=.true.  ! ~n(4)
        ! charginos
        if(id.eq.1000024) injetblacklist=.true.  ! ~x(1)
        if(id.eq.1000037) injetblacklist=.true.  ! ~x(2)
        ! photon
        if(id.eq.22) injetblacklist=.true.
        ! W,Z,Higgs
        if(id.eq.23) injetblacklist=.true.
        if(id.eq.24) injetblacklist=.true.
        if(id.eq.25) injetblacklist=.true.
      end
c############### end function injetblacklist ###########################

c############### subroutine injetblacklist #############################
c determines if particle is detectable or not (for PT_miss)
      logical function isdetectable(PDG_ID)
        implicit none
        integer PDG_ID, id
        isdetectable = .true.
        id = iabs(PDG_ID)
        ! leptons
        if(id.eq.12) isdetectable=.false.  ! nue
        if(id.eq.14) isdetectable=.false.  ! numu
        if(id.eq.16) isdetectable=.false.  ! nu_tau
        ! sleptons
        if(id.eq.1000012) isdetectable=.false.  ! ~snue_L
        if(id.eq.1000014) isdetectable=.false.  ! ~snumu_L
        if(id.eq.1000016) isdetectable=.false.  ! ~snu_tau_L
        ! neutralinos
        if(id.eq.1000022) isdetectable=.false.  ! ~n(1)
        if(id.eq.1000023) isdetectable=.false.  ! ~n(2)
        if(id.eq.1000025) isdetectable=.false.  ! ~n(3)
        if(id.eq.1000035) isdetectable=.false.  ! ~n(4)
      end
c############### end subroutine injetblacklist #########################

c############### subroutine computeptrel ###############################
c pt rel is sum of the ptrack momentum projection ortogonal to the jet
c momentum in the frame where the jet has zero rapidity
      subroutine computeptrel(ptracks,ntracks,rapjets,ktjets,phijets,
     &     njets,jetvec,ptrel)
        implicit none
        integer ntracks,njets,jetvec(ntracks)
        double precision ptracks(4,ntracks),rapjets(njets)
        double precision ktjets(njets),phijets(njets),ptrel(njets)
        integer j,i
        double precision yj,kj1,kj2,y,pt(3)
        do j=1,njets
          ptrel(j)=0
        enddo
        do i=1,ntracks
          j = jetvec(i)
          if(j.gt.0.and.j.le.njets) then
            ! Track i belongs to jet j
            yj = rapjets(j)
            kj1 = ktjets(j)*dcos(phijets(j))
            kj2 = ktjets(j)*dsin(phijets(j))
            ! rapidity of track i
            y = 0.5D0*dlog((ptracks(4,i)+ptracks(3,i))
     &                   /(ptracks(4,i)-ptracks(3,i)))
            ! rapidity of track i in frame where the jet has zero rapidity
            y = y-yj
            ! find momentum of track i in frame where the jet has zero rapidity
            pt(1) = ptracks(1,i)
            pt(2) = ptracks(2,i)
            pt(3) = dsqrt(pt(1)**2+pt(2)**2)*sinh(y)
            ! pt rel is sum of the ptrack momentum projection ortogonal to the jet
            ! momentum in the frame where the jet has zero rapidity
            ptrel(j) = dsqrt(((pt(1)*kj2-pt(2)*kj1)**2+
     &                       (         -pt(3)*kj2)**2+
     &                       (pt(3)*kj1          )**2)/
     &                       (kj1**2+kj2**2)) + ptrel(j)
          endif
        enddo
      end
c############### end subroutine computeptrel ###########################


c############### subroutine store_decchain #############################
      subroutine store_decchain(str_user_decchain,user_decchain)
        ! the considered decay chains
        character*100 str_user_decchain
        integer user_decchain(100,0:2)
        ! operations on strings of decay chains
        character*10 substr
        integer npos,len1
        
        user_decchain(:,:) = 0
        
        len1 = len(trim(str_user_decchain))
        ! exit subroutine without doing anything if no string is given
        if(len1.eq.0) then
          return
        endif

        do i=1,100
          ! get the mother (string in front of "->")
          npos = index(str_user_decchain,"->")
          if(npos.gt.1) then
            substr = str_user_decchain(1:npos-1)
            ! convert string to int
            if(len(trim(substr)).gt.0) read(substr(:),*) user_decchain(i,0)
            str_user_decchain = str_user_decchain(npos+2:len1)
            ! get the first daughter
            npos = index(str_user_decchain,",")
            substr = str_user_decchain(1:npos-1)
            if(len(trim(substr)).gt.0) read(substr(:),*) user_decchain(i,1)
            str_user_decchain = str_user_decchain(npos+1:len1)
            ! get the second daughter
            npos = index(str_user_decchain,";")
            substr = str_user_decchain(1:npos-1)
            if(len(trim(substr)).gt.0) read(substr(:),*) user_decchain(i,2)
            str_user_decchain = str_user_decchain(npos+1:len1)
            print*,user_decchain(i,0),"->",user_decchain(i,1),user_decchain(i,2)
          else
            ! reached stable particle (no decay chain)
            substr = str_user_decchain(1:len1)
            if(len(trim(substr)).gt.0) then
              read(substr(:),*) user_decchain(1,0)
              print*,"stable particle: ", user_decchain(1,0)
            endif
            return
          endif
        enddo
      end
c############### end subroutine store_decchain #########################
