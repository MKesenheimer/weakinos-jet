c############### init_couplings.f ######################################
c last modified by MK, 23.05.2016
c weakino pair + jet production

c############### init_couplings subroutine #############################
c setup SM and MSSM parameters in PhysPars.h
c requires encoded process ID in lprup(1)
      subroutine init_couplings
        implicit none
#include "PhysPars.h"
#include "LesHouches.h"
#include "osres.h"
#ifndef collier
#include "looptools.h"
#endif
        double precision powheginput
        external powheginput
        ! final state IDs
        integer id_fin1, id_fin2

        ! extract particle types from process id
        call decode_pair(lprup(1),id_fin1,id_fin2)

        ! set PhysPars parameters
        call set_phys_params
        ! set madgraph parameters.
        ! note: calling this subroutine for the first time reads from 
        ! the provided slha file.
        call set_madgraph_params
        
        ! set remaining FormCalc parameters
        call set_formcalc_params

        ! hack to overwrite the madgraph alpha 
        ! (par_alpha is the one we want to use)
        if(alfa .ne. alpha) then
          alpha = par_alpha
          call set_madgraph_params
        endif

        ! set general MadGraph couplings
        call set_general_couplings(id_fin1,id_fin2)
        ! set the parameters for treating the on-shell resonances
        call set_os_regulator

        ! Print out all parameters if you want
#if DEBUG > 0
        print*
        print*, "===================================================="
        print*, "               set POWHEG parameters                "
        print*, "===================================================="
        print*
        print*, "Z mass          = ",par_Zmass
        print*, "W mass          = ",par_Wmass
        print*, "Z width         = ",par_Zwidth
        print*, "W width         = ",par_Wwidth
        print*, "1/alpha         = ",1d0/par_alpha
        print*
        print*, "final state masses = ", par_Fin1mass
        print*, "                     ", par_Fin2mass
        print*, "                     ", par_Fin3mass
        print*
        print*, "OS regulator WREG1, WREG2 = ", wreg1, wreg2
        print*        
#endif
 
        ! check if the MadGraph parameters are the same as the ones
        ! used by FormCalc
        if( (MZ                 .ne. zmass        ) .or.
     &      (MW                 .ne. wmass        ) .or.
     &      (WZ                 .ne. zwidth       ) .or.
     &      (WW                 .ne. wwidth       ) .or.
     &      (alfa               .ne. alpha        ) .or.
     &      (EL                 .ne. dreal(gal(1))) .or.
     &      (dabs(MNeu(1))      .ne. dabs(mn1)    ) .or.
     &      (dabs(MNeu(2))      .ne. dabs(mn2)    ) .or.
     &      (dabs(MNeu(3))      .ne. dabs(mn3)    ) .or.
     &      (dabs(MNeu(4))      .ne. dabs(mn4)    ) .or.
     &      (MCha(1)            .ne. mx1          ) .or.
     &      (MCha(2)            .ne. mx2          ) .or.
     &      (MSf(1,3,1)         .ne. mul          ) .or.
     &      (MSf(2,3,1)         .ne. mur          ) .or.
     &      (MSf(1,4,1)         .ne. mdl          ) .or.
     &      (MSf(2,4,1)         .ne. mdr          ) .or.
     &      (MSf(1,3,2)         .ne. mcl          ) .or.
     &      (MSf(2,3,2)         .ne. mcr          ) .or.
     &      (MSf(1,4,2)         .ne. msl          ) .or.
     &      (MSf(2,4,2)         .ne. msr          ) .or.
     &      (MSf(1,3,3)         .ne. mtl          ) .or.
     &      (MSf(2,3,3)         .ne. mtr          ) .or.
     &      (MSf(1,4,3)         .ne. mbl          ) .or.
     &      (MSf(2,4,3)         .ne. mbr          ) .or.
     &      (MGl                .ne. mgo          ) .or.
     &      (WGl                .ne. wgo          ) .or.
     &      (WSf(1,4,1)         .ne. wdl          ) .or.
     &      (WSf(2,4,1)         .ne. wdr          ) .or.
     &      (WSf(1,3,1)         .ne. wul          ) .or.
     &      (WSf(2,3,1)         .ne. wur          ) .or.
     &      (WSf(1,4,2)         .ne. wsl          ) .or.
     &      (WSf(2,4,2)         .ne. wsr          ) .or.
     &      (WSf(1,3,2)         .ne. wcl          ) .or.
     &      (WSf(2,3,2)         .ne. wcr          ) .or.
     &      (WSf(1,4,3)         .ne. wbl          ) .or.
     &      (WSf(2,4,3)         .ne. wbr          ) .or.
     &      (WSf(1,3,3)         .ne. wtl          ) .or.
     &      (WSf(2,3,3)         .ne. wtr          ) .or.
     &      (cdabs(ZNeu(1,1))   .ne. dabs(bwmix(1,1)) ) .or. ! check only if the absolute
     &      (cdabs(ZNeu(1,2))   .ne. dabs(bwmix(1,2)) ) .or. ! value is equal, since the
     &      (cdabs(ZNeu(1,3))   .ne. dabs(bwmix(1,3)) ) .or. ! FormCalc-mixing matrices
     &      (cdabs(ZNeu(1,4))   .ne. dabs(bwmix(1,4)) ) .or. ! can be complex
     &      (cdabs(ZNeu(2,1))   .ne. dabs(bwmix(2,1)) ) .or.
     &      (cdabs(ZNeu(2,2))   .ne. dabs(bwmix(2,2)) ) .or.
     &      (cdabs(ZNeu(2,3))   .ne. dabs(bwmix(2,3)) ) .or.
     &      (cdabs(ZNeu(2,4))   .ne. dabs(bwmix(2,4)) ) .or.
     &      (cdabs(ZNeu(3,1))   .ne. dabs(bwmix(3,1)) ) .or.
     &      (cdabs(ZNeu(3,2))   .ne. dabs(bwmix(3,2)) ) .or.
     &      (cdabs(ZNeu(3,3))   .ne. dabs(bwmix(3,3)) ) .or.
     &      (cdabs(ZNeu(3,4))   .ne. dabs(bwmix(3,4)) ) .or.
     &      (cdabs(ZNeu(4,1))   .ne. dabs(bwmix(4,1)) ) .or.
     &      (cdabs(ZNeu(4,2))   .ne. dabs(bwmix(4,2)) ) .or.
     &      (cdabs(ZNeu(4,3))   .ne. dabs(bwmix(4,3)) ) .or.
     &      (cdabs(ZNeu(4,4))   .ne. dabs(bwmix(4,4)) ) .or.
     &      (UCha(1,1)          .ne. uumix(1,1)   ) .or.
     &      (UCha(1,2)          .ne. uumix(1,2)   ) .or.
     &      (UCha(2,1)          .ne. uumix(2,1)   ) .or.
     &      (UCha(2,2)          .ne. uumix(2,2)   ) .or.
     &      (VCha(1,1)          .ne. vvmix(1,1)   ) .or.
     &      (VCha(1,2)          .ne. vvmix(1,2)   ) .or.
     &      (VCha(2,1)          .ne. vvmix(2,1)   ) .or.
     &      (VCha(2,2)          .ne. vvmix(2,2)   ) ) then
          print*, "error: Madgraph uses parameters different to "
     &          //"PhysPars.h or FormCalc paramaters"
          !stop
        endif

        ! check if all paramaters are properly set
        if ( (par_alpha    .eq. invalid) .or.
     &       (par_Zmass    .eq. invalid) .or.
     &       (par_Wmass    .eq. invalid) .or.
     &       (par_Zwidth   .eq. invalid) .or.
     &       (par_Wwidth   .eq. invalid) .or.
     &       (par_Zmass2   .eq. invalid) .or.
     &       (par_Wmass2   .eq. invalid) .or.
     &       (par_Fin1mass .eq. invalid) .or.
     &       (par_Fin2mass .eq. invalid) .or.
     &       (par_Fin3mass .eq. invalid) ) then
          print*,"warning: One or more parameters are not set:"
          print*,"par_alpha: ",par_alpha
          print*,"par_Zmass: ",par_Zmass
          print*,"par_Wmass: ",par_Wmass
          print*,"par_Zwidth: ",par_Zwidth
          print*,"par_Wwidth: ",par_Wwidth
          print*,"par_Zmass2: ",par_Zmass2
          print*,"par_Wmass2: ",par_Wmass2
          print*,"par_Fin1mass: ",par_Fin1mass
          print*,"par_Fin2mass: ",par_Fin2mass
          print*,"par_Fin3mass: ",par_Fin3mass
          print*,"If possible set the invalid parameters in "//
     &           "powheg.input"
          stop
        endif


#ifndef ANALYSIS
#ifdef collier
        ! init COLLIER
        print*, "===================================================="
        print*, "                   init Collier                     "
        print*, "===================================================="
        call init_collier(6,6)
#else
        ! init LoopTools
        print*, "===================================================="
        print*, "             init LoopTools                         "
        call ltini
        ! 0*key: compute version a
        ! 1*key: compute version b
        ! 2*key: compute both, compare, return a
        ! 3*key: compute both, compare, return b
        ! KeyA0, KeyBget, KeyC0, KeyD0, KeyEget, KeyEgetC, KeyAll 
        call setversionkey(0*KeyAll) ! default 0
        call setmaxdev(1D-3) ! default 1D-12
        call setminmass(0D0) ! default 0D0
        call setzeroeps(10D-22) ! default 1D-22
        call setdiffeps(10D-12) ! default 1D-12
        print*,"versionkey: ", getversionkey()
        print*,"maxdev: ", getmaxdev()
        print*,"minmass: ", getminmass()
        print*, "===================================================="
        print*
#endif
#endif
      end
c############### end subroutine init_couplings #########################

c############### subroutine init_madgraph_params #######################
c sets the Madgraph parameters
      subroutine set_madgraph_params
        implicit none
        character*100 slhafilename
        logical called, readslha
        data called/.false./
        save called
        ! reads from slha file only when called this subroutine
        ! for the first time. When this subroutine is called
        ! more than once, the SM Input parameters can be overwritten
        ! with parameters from powheg.input.
        if(called) then
          !print*, "note: Overwrite madgraph paramaters."
          readslha = .false.
        else
          call powheginputstring("SLHA",slhafilename)
          called   = .true.
          readslha = .true.
        endif
        call setpara(slhafilename,readslha)
      end
c############### end subroutine init_madgraph_params ###################

c############### subroutine set_ebe_couplings ##########################
c set the madgraph and formcalc couplings event by event
      subroutine set_ebe_couplings
        implicit none
#include "PhysPars.h"
#include "pwhg_st.h"
        ! MadGraph
        ! QCD coupling constant
        alphaS = st_alpha
        G      = dsqrt(alphaS*4d0*m_pi)
        GG(1)  = -G
        GG(2)  = -G
        ! e.m. coupling
        alpha  = par_alpha
        Gal(1) = dcmplx(dsqrt(alpha*4d0*m_pi))
        Gal(2) = dcmplx(dsqrt(alpha*4d0*m_pi))
        ! FormCalc
        ! QCD coupling
        AlfaS  = alphaS
        AlfaS2 = AlfaS**2
        GS = dsqrt(AlfaS*4d0*m_pi)
        ! e.m. coupling
        Alfa = par_alpha
        Alfa2   = Alfa**2
        EL = dsqrt(Alfa*4d0*m_pi)

        ! set the MadGraph SUSY-QCD couplings
        call INIT_SUSY_QCD(GS)
      end
c############### end subroutine set_ebe_couplings ######################

c############### set_phys_params subroutine ############################
c extracts mass and mixing matrices from slhadata
c assumes slhadata to be filled already
c sets the PhysPars.h parameters
      subroutine set_phys_params
        implicit none
#include "PhysPars.h"
#include "LesHouches.h"
#include "pwhg_st.h"
        double precision powheginput
        external powheginput
        integer i,j,k   ! loops
        ! decay widths
        integer particle, channel
        ! for reading parameters from SHLA file
        integer error
        character*100 slhafilename
        ! final state IDs
        integer id_fin1, id_fin2
#define SLHA_ALPHA
#ifndef SLHA_ALPHA
        ! weinberg angle
        double precision sinW
#endif
        
        par_alpha    = invalid
        par_Zmass    = invalid
        par_Wmass    = invalid
        par_Zmass2   = invalid
        par_Wmass2   = invalid
        par_Zwidth   = invalid
        par_Wwidth   = invalid
        par_Fin1mass = invalid
        par_Fin2mass = invalid
        par_Fin3mass = invalid

        ! generation cut for soft jets
        gcut = 10D0
        print*,"note: Using generation cut of",gcut,"GeV for jets in "//
     &         "Phasespace"

        ! get SLHA filename
        call powheginputstring("SLHA",slhafilename)
        ! read in parameters from SLHA file
        call SLHARead(error, slhadata, slhafilename, 1)
        if (error.ne.0)  stop "error reading parameters from SLHA file"

        ! read all particle widths from SLHA-file
        nchannels = SLHADecayTable(slhadata, 0, width, partid, 7, 1024)

        ! SM parameters from SLHA-file
        print*, "note: ignoring alpha_s value given in SLHA file"
        par_Zmass   = dreal(SMInputs_MZ)
        par_Wmass   = dreal(Mass_MW)
        par_gf      = dreal(SMInputs_GF)
        par_alphaMZ = 1d0/dreal(SMInputs_invAlfaMZ)
#ifdef SLHA_ALPHA
        par_alpha   = par_alphaMZ
#else
        sinW        = dsqrt(1d0-par_Wmass**2/par_Zmass**2)
        par_alpha   = dsqrt(2d0)*par_Wmass**2*sinW**2*par_gf/m_pi
#endif
        !print*,1/par_alpha
        !stop
        par_Zmass2  = par_Zmass**2
        par_Wmass2  = par_Wmass**2

        ! particle widths from SLHA-file
        do channel = 1, nchannels
          nparticles = partid(0,channel)
          if(nparticles .eq. 1) then
            if(partid(1,channel).eq.23) then
              par_Zwidth = width(channel)
            else if(partid(1,channel).eq.24) then
              par_Wwidth = width(channel)
            endif
          endif
        enddo

        if(par_Wwidth .eq. invalid) then
          print*, "warning: W-width not found in SLHA file."
          print*, "         Use zero width."
          par_Wwidth = 0D0
        endif

        if(par_Zwidth .eq. invalid) then
          print*, "warning: Z-width not found in SLHA file."
          print*, "         Use zero width."
          par_Zwidth = 0D0
        endif
        
        ! Sfermion masses
        do i=1,2     ! left, right
          do j=1,4   ! type (nu,e,u,d)
            do k=1,3 ! generation
              par_MSf(i,j,k) = dreal(Mass_MSf(i,j,k))
            enddo
          enddo
        enddo
        
        ! Neutralino masses
        do i=1,4
          par_MNeu(i)  = dabs(dreal(Mass_MNeu(i)))
          !par_MNeu(i)  = dreal(Mass_MNeu(i))
        enddo
        
        ! Chargino masses
        do i=1,2
          par_MCha(i)  = dreal(Mass_MCha(i))
        enddo
        
        ! gluino mass
        par_MGl = dreal(Mass_MGl)

        ! quark masses
        par_MU = 0D0
        par_MC = 0D0
        par_MD = 0D0
        par_MS = 0D0

        ! 4 or 5 flavor scheme
        if(st_nlight.le.4) then
          if(powheginput("#msbar").gt.0) then
            if(powheginput("msbar").eq.1) then
              par_MB = dreal(SMInputs_Mb)
              print*, "using msbar mass for b-quark: ", par_MB
            endif
          else
            par_MB = dreal(Mass_Mf(4,3))
            print*, "using pole mass for b-quark: ", par_MB
          endif
        else
          par_MB = 0D0
          print*, "neglecting b-quark mass: ", par_MB
        endif

        par_MT = dreal(SMInputs_Mt)
c particle widths from SLHA-file
c NOTE: FormCalc does not use squark widths in the current software 
c version.
        do channel = 1, nchannels
          nparticles = partid(0,channel)
          if(nparticles .eq. 1) then
            select case(partid(1,channel))
              case (1000021) ! gluino
                par_WGl = width(channel)
#ifndef NEGLECT_SWIDTH
              case (1000001) ! sdown_L
                par_WSf(1,4,1) = width(channel)
              case (2000001) ! sdown_R
                par_WSf(2,4,1) = width(channel)
              case (1000002) ! sup_L
                par_WSf(1,3,1) = width(channel)
              case (2000002) ! sup_R
                par_WSf(2,3,1) = width(channel)
              case (1000003) ! sstrange_L
                par_WSf(1,4,2) = width(channel)
              case (2000003) ! sstrange_R
                par_WSf(2,4,2) = width(channel)
              case (1000004) ! scharm_L
                par_WSf(1,3,2) = width(channel)
              case (2000004) ! scharm_R
                par_WSf(2,3,2) = width(channel)
              case (1000005) ! sbottom_1
                par_WSf(1,4,3) = width(channel)
              case (2000005) ! sbottom_2
                par_WSf(2,4,3) = width(channel)
              case (1000006) ! stop_1
                par_WSf(1,3,3) = width(channel)
              case (2000006) ! stop_2
                par_WSf(2,3,3) = width(channel)
#endif
            endselect
          endif
        enddo
#ifdef NEGLECT_SWIDTH
        print*,"Warning: FormCalc does not use squark widths."
        do i=1,2 ! sfermion index s
          do j=3,4 ! type (u or d)
            do k=1,3 ! generation
              WSf(i,j,k) = 0d0
            enddo
          enddo
        enddo
#endif

        ! extract particle types from process id
        call decode_pair(lprup(1),id_fin1,id_fin2)
        
        select case(abs(id_fin1))
          case (1000022) ! neutralino1
            par_Fin1mass = par_MNeu(1)
          case (1000023) ! neutralino2
            par_Fin1mass = par_MNeu(2)
          case (1000025) ! neutralino3
            par_Fin1mass = par_MNeu(3)
          case (1000035) ! neutralino4
            par_Fin1mass = par_MNeu(4)
          case (1000024) ! chargino1
            par_Fin1mass = par_MCha(1)
          case (1000037) ! chargino2
            par_Fin1mass = par_MCha(2)
          case (2000011) ! slepton+
            par_Fin1mass = par_MSf(2,2,1)
          case default
            print*, "encountered unknown particle ID for fin1 ",id_fin1
            stop
        endselect
        select case(abs(id_fin2))
          case (1000022) ! neutralino1
            par_Fin2mass = par_MNeu(1)
          case (1000023) ! neutralino2
            par_Fin2mass = par_MNeu(2)
          case (1000025) ! neutralino3
            par_Fin2mass = par_MNeu(3)
          case (1000035) ! neutralino4
            par_Fin2mass = par_MNeu(4)
          case (1000024) ! chargino1
            par_Fin2mass = par_MCha(1)
          case (1000037) ! chargino2
            par_Fin2mass = par_MCha(2)
          case (2000011) ! slepton
            par_Fin2mass = par_MSf(2,2,1)
          case default
            print*, "encountered unknown particle ID for fin2", id_fin2
            stop
        endselect
        
        if( (dabs(par_Fin1mass) + dabs(par_Fin2mass)) .lt. par_Zmass )
     &  then
          print*, "Final state masses low. On-shell W/Z production "//
     &            "followed by weakino decays might be more suitable."
          stop
        endif

        ! mass of the jet (is used only for phsp)
        par_Fin3mass = 0D0
      end
c############### end subroutine set_phys_params ########################

c############### subroutine set_formcalc_params ########################
c extracts mass and mixing matrices from slhadata
c assumes slhadata to be filled already
c sets the FormCalc parameters
      subroutine set_formcalc_params
        implicit none
#include "PhysPars.h"
        double precision powheginput, kronecker
        external powheginput, kronecker
        integer i,j,k,l ! loops
        ! Avoid multiple calls to this subroutine.
        logical called
        data called/.false./
        save called
        if(called) then
          return
        else
          called=.true.
        endif

        ! Slepton/Squark masses and widths
        do i=1,2 ! sfermion index s
          do j=1,4 ! type (nu,e,u,d)
            do k=1,3 ! generation
              WSf(i,j,k)  = par_WSf(i,j,k)
              MSf(i,j,k)  = par_MSf(i,j,k)
              MSf2(i,j,k) = MSf(i,j,k)**2
            enddo
          enddo
        enddo
       
        ! Neutralino masses
        do i=1,4 !neutralino index s
          MNeu(i)  = par_MNeu(i)
          MNeu2(i) = MNeu(i)**2
        enddo
        
        ! Chargino masses
        do i=1,2 !chargino index c
          MCha(i)  = par_MCha(i)
          MCha2(i) = MCha(i)**2
        enddo
        
        ! Z/W mass
        MZ  = par_Zmass
        MZ2 = MZ**2
        MW  = par_Wmass
        MW2 = MW**2

        ! Quark masses
        MU   = par_MU
        MU2  = MU**2
        MC   = par_MC
        MC2  = MC**2
        MD   = par_MD
        MD2  = MD**2
        MS   = par_MS
        MS2  = MS**2
        MB   = par_MB
        MB2  = MB**2
        MT   = par_MT
        MT2  = MT**2
        
        Mf(1,1) = 0D0
        Mf(2,1) = 0D0
        Mf(3,1) = MU
        Mf(4,1) = MD
        Mf(1,2) = 0D0
        Mf(2,2) = 0D0
        Mf(3,2) = MC
        Mf(4,2) = MS
        Mf(1,3) = 0D0
        Mf(2,3) = 0D0
        Mf(3,3) = MT
        Mf(4,3) = MB

        Mf2(1,1) = 0D0
        Mf2(2,1) = 0D0
        Mf2(3,1) = MU2
        Mf2(4,1) = MD2
        Mf2(1,2) = 0D0
        Mf2(2,2) = 0D0
        Mf2(3,2) = MC2
        Mf2(4,2) = MS2
        Mf2(1,3) = 0D0
        Mf2(2,3) = 0D0
        Mf2(3,3) = MT2
        Mf2(4,3) = MB2
        
        ! Gluino mass and width
        MGl  = par_Mgl
        MGl2 = MGl**2
        WGl  = par_WGl

        ! Z/W Widths (added by hand to formcalc)
        WZ  = par_Zwidth
        WW  = par_Wwidth

        ! Neutralino mixing matrix
        ! SLHA name: NMix_ZNeu(n1,n2)
        do i=1,4 ! neutralino index n1
          do j=1,4 ! neutralino index n2
            if(dreal(Mass_MNeu(i)).lt.0D0) then ! check if we need a complex mixing matrix
              ZNeu(i,j) = ii*dcmplx(NMix_ZNeu(i,j))
            else
              ZNeu(i,j) = dcmplx(NMix_ZNeu(i,j))
            endif  
          enddo
        enddo
        
        ! Chargino mixing matrix
        do i=1,2 ! chargino index c1
          do j=1,2 ! chargino index c2
            VCha(i,j) =  dcmplx(VMix_VCha(i,j))
            UCha(i,j) =  dcmplx(UMix_UCha(i,j))
          enddo
        enddo

        ! Squark mixing matrices (u, d, s, c - sfermions)
        do i=1,2 ! sfermion index s1 (left, right)
          do j=1,2 ! sfermion index s2
            do k=2,4 ! type index t (u- or d-type)
              do l=1,2 ! generation index g (note: generation 3 is in line 448)
                if(i.eq.j)  USf(i,j,k,l) = (1D0,0D0)
                if(i.ne.j)  USf(i,j,k,l) = (0D0,0D0)
              enddo
            enddo
          enddo
        enddo
        do i=1,2 ! sfermion index s1
          do j=1,2 ! sfermion index s2
            ! Mixing between t-sfermions (mixing)
            USf(i,j,3,3) = dcmplx(StopMix_USf(i,j))
            ! Mixing between b-sfermions (mixing)
            USf(i,j,4,3) = dcmplx(SbotMix_USf(i,j))
          enddo
        enddo
        
        ! mixing angles
        CW  = MW/MZ
        CW2 = CW**2
        SW2 = 1 - CW2
        SW  = dsqrt(SW2)
        
        ! Higgs parameters 
        TB = dble(MinPar_TB)
        CB = dabs(1.D0/dsqrt(1+TB**2))
        SB = dabs(TB*CB)
        SB2 = SB**2
        CB2 = CB**2
        TB2 = TB**2
        S2B = 2*TB*CB2
        C2B = (CB - SB)*(CB + SB)
        
        TA = dtan(dble(Alpha_Alpha))
        CA = dabs(1.D0/dsqrt(1+TA**2))
        SA = dabs(TA*CA)
        SA2 = SA**2
        CA2 = CA**2
        TA2 = TA**2
        S2A = 2*TA*CA2
        C2A = (CA - SA)*(CA + SA)
        
        SAB = SA*CB + CA*SB
        CAB = CA*CB - SA*SB
        SBA = SB*CA - CB*SA
        SBA2 = SBA**2
        CBA = CB*CA + SB*SA
        CBA2 = CBA**2
        
        Mh0 = dble(Mass_Mh0)
        MHH = dble(Mass_MHH)
        MA0 = dble(Mass_MA0)
        MHp = dble(Mass_MHp)
        Mh02 = Mh0**2
        MHH2 = MHH**2 
        MA02 = MA0**2
        MHp2 = MHp**2
        
        ! Gaugino mass parameter
        Mino3 = dcmplx(Mgl)
        
        ! trilinear couplings
        do i=2,4 ! type 
          do j=1,3 ! generation
            do k=1,3 ! generation
              Af(i,j,k) = dcmplx(Af_Af(j,k,i))
            enddo
          enddo
        enddo
        
        ! mass term mixing the 2 Higgs doublets
        MUE  = dcmplx(HMix_MUE)

        ! couplings
        ! set Formcalc couplings equal to the ones provided by POWHEG
        Alfa    = par_alpha
        Alfa2   = Alfa**2
        AlfaS   = dble(SMInputs_AlfasMZ) ! dummy value, gets overwritten by set_ebe_couplings
        AlfaS2  = AlfaS**2
        AlfaMZ  = par_alphaMZ
        AlfaSMZ = dble(SMInputs_AlfasMZ)
        EL = dsqrt(4*m_pi*Alfa)
        GS = dsqrt(4*m_pi*AlfaS)

        ! conjugate complex parameters
        do i=1,2
          do j=1,2
            do k=1,4
              do l=1,3
                USfC(i,j,k,l) = dconjg(USf(i,j,k,l))
              enddo
            enddo
          enddo
        enddo

        do i=1,2
          do j=1,2
            VChaC(i,j) = dconjg(VCha(i,j))
            UChaC(i,j) = dconjg(UCha(i,j))
          enddo
        enddo

        do i=1,4
          do j=1,4
            ZNeuC(i,j) = dconjg(ZNeu(i,j))
          enddo
        enddo

        do i=2,4
          do j=1,3
            do k=1,3
              AfC(i,j,k) = dconjg(Af(i,j,k))
            enddo
          enddo
        enddo

        Mino3C = dconjg(Mino3)
        MUEC = dconjg(MUE)
        
        ! complexified parameters for arguments of the loop integrals
        ! are used only for internal propagators
        MZC = dcmplx(MZ)
        MZ2C = dcmplx(MZ2)
        MZ2W = MZ2C - ii*MZ*WZ
        MWC = dcmplx(MW)
        MW2C = dcmplx(MW2)
        MW2W = MW2C - ii*MW*WW
        MUC = dcmplx(MU)
        MU2C = dcmplx(MU2)
        MCC = dcmplx(MC)
        MC2C = dcmplx(MC2)
        MTC = dcmplx(MT)
        MT2C = dcmplx(MT2)
        MDC = dcmplx(MD)
        MD2C = dcmplx(MD2)
        MSC = dcmplx(MS)
        MS2C = dcmplx(MS2)
        MBC = dcmplx(MB)
        MB2C = dcmplx(MB2)

        do i=1,4
          do j=1,3
            MfC(i,j) = dcmplx(Mf(i,j))
            Mf2C(i,j) = dcmplx(Mf2(i,j))
          enddo
        enddo
 
        do i=1,4
          MNeuC(i) = dcmplx(MNeu(i))
          MNeu2C(i) = dcmplx(MNeu2(i))
        enddo
 
        do i=1,2
          MChaC(i) = dcmplx(MCha(i))
          MCha2C(i) = dcmplx(MCha2(i))
        enddo
 
        do i=1,2
          do j=1,4
            do k=1,3
              MSfC(i,j,k) = dcmplx(MSf(i,j,k))
              MSf2C(i,j,k) = dcmplx(MSf2(i,j,k))
              MSf2W(i,j,k) = MSf2C(i,j,k) - ii*MSf(i,j,k)*WSf(i,j,k)
            enddo
          enddo
        enddo
 
        MGlC = dcmplx(MGl)
        MGl2C = dcmplx(MGl2)
        MGl2W = MGl2C - ii*MGl*WGl
 
        Mh0C = dcmplx(Mh0)
        MHHC = dcmplx(MHH)
        MA0C = dcmplx(MA0)
        MHpC = dcmplx(MHp)
        Mh02C = dcmplx(Mh02)
        MHH2C = dcmplx(MHH2)
        MA02C = dcmplx(MA02)
        MHp2C = dcmplx(MHp2)
        
        ! Gauge parameters
        GaugeXi(1) = 1
        GaugeXi(2) = 1
        GaugeXi(3) = 1
        GaugeXi(4) = 1
      end
c############### end subroutine set_formcalc_params ####################

c############### subroutine set_os_regulator ###########################
c set the on shell regulator wreg1 (single) and wreg2 (double resonances)
      subroutine set_os_regulator
        implicit none
#include "PhysPars.h"
#include "osres.h"
        integer i,j,k,nmass
        double precision ewi1,ewi2,m_avrg
        double precision powheginput
        external powheginput

        ewi1  = 0D0 ! (default)
        if(powheginput("#ewi1").ne.0) then
          ewi1 = powheginput("ewi1")
        endif
        wreg1 = ewi1*MGl ! Prospino like definition
        
        nmass = 0
        m_avrg = 0D0
        do i=1,2 ! sfermion index s
          do j=3,4 ! type (nu,e,u,d)
            do k=1,3 ! generation
              m_avrg = m_avrg + MSf(i,j,k)
              nmass = nmass + 1
            enddo
          enddo
        enddo
        m_avrg = m_avrg/nmass

        ewi2  = 0D0 ! (default)
        if(powheginput("#ewi2").ne.0) then
          ewi2 = powheginput("ewi2")
        endif
        wreg2 = ewi2*m_avrg ! Prospino like definition        
      
        print*,"Masses for on-shell regulator scaling:"
        print*,"MGl = ",MGl
        print*,"msq = ",m_avrg
        print*,"On-shell regulators used:"
        print*,"ewi1 = ",ewi1,", wreg1 = ",wreg1
        print*,"ewi1 = ",ewi2,", wreg2 = ",wreg2
      end
c############### end subroutine set_os_regulator #######################
