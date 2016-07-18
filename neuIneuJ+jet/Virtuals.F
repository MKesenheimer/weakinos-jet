c############### Virtuals.f ############################################
c last modified by MK, 10.06.2016
c weakino pair + jet production
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### subroutine setvirtual #################################
c returns (M_B * M_V)/(as/(2pi)) as real number,
c where M_B is the Born amplitude and
c M_V is the finite part of the virtual amplitude
c The as/(2pi) factor is attached at a later point
c A factor (4 Pi)^ep/Gamma(1-ep) with ep=(4-d)/2 is implied.
      subroutine setvirtual(p,vflav,ampvirt)
        implicit none
#include "PhysPars.h"
#include "pwhg_st.h"
#include "pwhg_math.h"
#include "nlegborn.h"
#include "looptools.h"
#include "Flags.h"
#include "decl.h"
        double precision p(0:3,nlegborn)
        integer vflav(nlegborn)
        ! copy of vflav and p
        double precision ps(0:3,nlegborn)
        integer  flavs(nlegborn)
        ! results. Note: bornjk and bmunu is only used to be able to
        ! call the MadGraph Born amplitude, when using fake virtuals.
        double precision born, bornjk(nlegborn,nlegborn)
        double precision bmunu(0:3,0:3,nlegborn)
        double precision virtual, ampvirt
        ! formcalc results
        double precision fc_result(2)
        ! symmetry factor
        double precision symfac
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! phase space
        double precision ex, ey, ez, psNorm
        ! function to calculate the squared momentum sum
        double precision momsum2sq
        ! masses
        double precision MassI1, MassI2, MassO1, MassO2, MassO3
        ! check 4-momentum conservation
        logical lresult
        ! status
        logical verbose
        logical init_virtual
        data init_virtual/.true./
#ifdef DEBUGQ
#define DEBUG1
        ! debugging
        integer counter
        data counter/0/
        save counter
#endif

        if (init_virtual) then
          print*
          print*, "=== Init virtual... ==="
          if(flg_fakevirtuals) then
            print*, "Note: Using fake virtuals"
#ifdef FORM_BORN
            print*, "and FormCalc Born amplitudes"
#endif
#ifdef MAD_BORN
            print*, "and MadGraph Born amplitudes"
#endif
          endif
          verbose = .true.
          init_virtual = .false.
        else
          verbose = .false.
        endif
        
        ! backup flavor list and momenta, so we can operate on it without
        ! affecting the rest of the program
        flavs = vflav
        ps = p
        
        ! check four momentum conservation
        call check_4conservation(p, nlegborn, 1, lresult)

        ! calculate the couplings for this PS point
        call set_ebe_couplings

        ! crossing (FormCalc provides only amplitudes for qqbar 
        ! and not for qbarq)
        if((flavs(1).gt.0 .and. flavs(2).lt.0) .or.
     &     (flavs(1).lt.0 .and. flavs(2).eq.0) .or.
     &     (flavs(1).gt.0 .and. flavs(2).eq.0)) then
          ps(:,1) = p(:,1)
          ps(:,2) = p(:,2)
        else
          ps(:,1) = p(:,2)
          ps(:,2) = p(:,1)
        endif

        ! unpolarized particles: B01110 01010 01010 01010 01010 = D15018314, FC8Guid.pdf p.49
        if(flavs(1).eq.0) then
          symfac = 96d0
          helicities = 15018314
        elseif(flavs(2).eq.0) then
          symfac = 96d0
          helicities = 10955082
        elseif(flavs(3).eq.0) then
          symfac = 36d0
          helicities = 1082106
        elseif(flavs(4).eq.0) then
          symfac = 36d0
          helicities = 10824138
        elseif(flavs(5).eq.0) then
          symfac = 36d0
          helicities = 10824014
        else
          print*, "ERROR: Wrong flavor structure in setvirtual."
          print*, "flav = ", flavs
          stop
        endif

        ! symmetry factor for equal final states
        if(flavs(3).eq.flavs(4)) then
          symfac = 2d0*symfac
        endif

        ! calculate the virtuals only when needed
        if(.not.flg_fakevirtuals) then
          ! flags: Bit0 (reset) = 1, Bit1 (loop) = 1 -> B11 = D3
          flags = 3
          call calcRenConst
        else
          ! flags: Bit0 (reset) = 1, Bit1 (loop) = 0 -> B01 = D1
          flags = 1
        endif
        !call ltini
        call setlambda(0d0) ! Only finite part
        call setuvdiv(0d0)  ! no 1/eps_UV terms
        call setdelta(0d0)  ! set divergences to zero
        call setmudim(st_muren2) ! renormalization scale
c#define CHECKUV
#ifdef CHECKUV
        ! test for finiteness
        ! print*, "checking for UV finiteness"
        call setuvdiv(1d0)
        call setDelta(1D7)    ! Test UV finiteness
#endif

        ! reset the FormCalc results
        fc_result(1) = 0.D0    ! born
        fc_result(2) = 0.D0    ! virtual amplitude
        
#ifndef NEGLECTBMASS
          print*,"ERROR: FormCalc massive b-quark amplitudes not yet "
     &           //"implemented."
          stop
#endif

        ! is defined in Born.F, but loads also the virtual amplitudes
        call getBVAmps(ps,flavs,helicities,flags,fc_result)

        ! no fake virtuals
        if(.not.flg_fakevirtuals) then
          born    = fc_result(1)/symfac
          virtual = fc_result(2)/symfac ! = 2 x Re[M_tree* x M_1-loop], FC8Guid.pdf p.48
          ! need to extract a factor N=(4Pi)^eps/Ga(1-eps)
          ! -> LoopTools does already extract this factor
          ampvirt = virtual/(st_alpha/(2d0*pi))
          !ampvirt = virtual*2d0*pi ! if FormCalc Alfas = 1
        ! use fake virtuals
        else
#ifdef FORM_BORN
          born    = fc_result(1)/symfac
#endif
#ifdef MAD_BORN
          call sborn_proc(p,flavs,born,bornjk,bmunu)
#endif
          ampvirt = 1d-2*born
        endif
        
        ! Print informations
        if (verbose) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "born           = ", born
          print*, "2*Born*Virtual = ", ampvirt
          print*
        endif

        if(isnan(virtual)) then
          !print*,"got strange value for virtual amplitude...", virtual
          virtual = 0D0
        endif
        
#ifdef DEBUG1
        if(counter < 5) then
          print*, "process: ", flavs
          print*, "s  = ", momsum2sq(p(0:3,1), p(0:3,2))
          print*, "t  = ", momsum2sq(p(0:3,1),-p(0:3,3))
          print*, "u  = ", momsum2sq(p(0:3,2),-p(0:3,3))
          print*, "virt = ", ampvirt
          counter = counter + 1
        endif
#endif
      end
c############### end subroutine setvirtual #############################
