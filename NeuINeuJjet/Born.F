c############### Born.f ################################################
c last modified by MK, 22.01.2015
c -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6
c t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t

c############### setborn subroutine ####################################
c sets Born cross section
c Born contribution:
c parton parton -> weakino weakino

      subroutine setborn(p,bflav,born,bornjk,bmunu)
        implicit none

#include "pwhg_math.h"
#include "nlegborn.h"
#include "PhysPars.h"


        integer bflav(nlegborn)
        double precision p(0:3,nlegborn), bornjk(nlegborn,nlegborn)
        double precision bmunu(0:3,0:3,nlegborn), born, colcf
        ! copy of bflav and p
        double precision pc(0:3,nlegborn)
        integer  flavc(nlegborn)
        ! formcalc results
        double precision fc_result(2)
        ! symmetry factor
        double precision symfac
        ! helicity and flags parameter for formcalc       
        integer*8 helicities
        integer flags
        ! indices
        integer j,k,nmu,nnu
        ! kinematics: mandelstam variables, quark masses, weakino masses
        double precision s, t, u, MassQ1, MassQ2, MassF1, MassF2, MassF3
        ! check 4-momentum conservation
        logical lresult
        ! phase space
        double precision ex, ey, ez, psNorm
        ! function to calculate the squared momentum sum
        double precision momsq, momsum3sq, momsum2sq
        external momsq, momsum2sq, momsum3sq
        ! calculate the relative error between formcalc and madgraph borns
        double precision fborn, mborn
        ! initialization variables
        logical verbose
        logical init_born
        data init_born/.true./

        ! debugging
        integer counter
     


        if (init_born) then
           print*
           print*, "=== Init born... ==="
           verbose = .true.
           init_born = .false.
        else
           verbose = .false.
        endif

        !this check is not needed in the current implemenation
        !if (.not.( (abs(bflav(2)).eq.abs(bflav(1)))
      !&      .or.   (abs(bflav(2)+bflav(1)).eq.1) )) then
      !    print*,"~~~~~~~~~~~~~~~~~~~ATTENTION~~~~~~~~~~~~~~~~~~~~~~"
      !    print*,"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      !    print*, "Error in fct. setborn: invalid initial state"
      !    print*, "bflav(1) = ", bflav(1)
      !    print*, "bflav(2) = ", bflav(2)
      !    print*,"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      !    print*,"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      !    stop
      !  endif
      
        ! check four momentum conservation
        call check_4conservation(p, nlegborn, 1, lresult)
      
              !if(verbose) then
          ! calculate kinematics only for debug purpose
          s = momsum2sq(p(0:3,1), p(0:3,2))
          t = momsum2sq(p(0:3,1),-p(0:3,3))
          u = momsum2sq(p(0:3,2),-p(0:3,3))
        !endif
	
	!print*,"~~~~~~~~~~~~~~~~"
	!print*,"s = ",s
	!print*,"~~~~~~~~~~~~~~~"
    
#if !defined(FORM_BORN) && !defined(MAD_BORN)
        print*,"Error: Preprocessor flag FORM_BORN or"
     &       //" MAD_BORN not set in Makefile."
        stop
#endif
        
#if defined(CHECK_FORM_MAD) || defined(FORM_BORN)
c ---------------------------- Formcalc Born ---------------------------

        ! store flavor list, so we can operate on it without
        ! affecting the other program
        flavc = bflav
        pc = p

        
        ! calculate the couplings for this PS point
        call set_ebe_couplings

        ! load process dependent masses
        call set_process(bflav(1:4), MassQ1, MassQ2, MassF1, MassF2)
        
        ! crossing (FormCalc provides only amplitudes for qqbar 
        ! and not for qbarq) crossing only if theres no gluon in the
        ! initial state
        if((bflav(1).gt.0).and.(bflav(2).lt.0)) then
          p(:,1) = pc(:,1)
          p(:,2) = pc(:,2)
        else
          p(:,1) = pc(:,2)
          p(:,2) = pc(:,1)
        endif
        
        ! init phase space
        ! calculate unit vectors
        psNorm=dsqrt(p(1,1)**2+p(2,1)**2+p(3,1)**2)
        ex=p(1,1)/psNorm
        ey=p(2,1)/psNorm
        ez=p(3,1)/psNorm
        call VecSet(1, MassQ1, psNorm, ex,ey,ez)
        psNorm=dsqrt(p(1,2)**2+p(2,2)**2+p(3,2)**2)
        ex=p(1,2)/psNorm
        ey=p(2,2)/psNorm
        ez=p(3,2)/psNorm
        call VecSet(2, MassQ2, psNorm, ex,ey,ez)
        psNorm=dsqrt(p(1,3)**2+p(2,3)**2+p(3,3)**2)
        ex=p(1,3)/psNorm
        ey=p(2,3)/psNorm
        ez=p(3,3)/psNorm
        call VecSet(3, MassF1, psNorm, ex,ey,ez)
        psNorm=dsqrt(p(1,4)**2+p(2,4)**2+p(3,4)**2)
        ex=p(1,4)/psNorm
        ey=p(2,4)/psNorm
        ez=p(3,4)/psNorm
        call VecSet(4, MassF2, psNorm, ex,ey,ez)
        psNorm=dsqrt(p(1,5)**2+p(2,5)**2+p(3,5)**2)
        ex=p(1,5)/psNorm
        ey=p(2,5)/psNorm
        ez=p(3,5)/psNorm
        MassF3 = 0 !gluon hardcoded as 5. particle
        call VecSet(5, MassF3, psNorm, ex,ey,ez)

        ! unpolarized particles: B01010 01010 01010 01010 = D338250, FC8Guid.pdf p.49
        ! unpolarized 2 -> 3 process: 01010 01010 01010 01010 01010 ---> 10824014
        !helicities = 338250  2 -> 2 process
        ! flags: Bit0 (reset) = 1, Bit1 (loop) = 0 -> B01 = D1
        flags = 1
        
        !call clearcache
        fc_result(1) = 0.D0    ! born
        fc_result(2) = 0.D0    ! virtual (not needed here)
        
        ! symmetry factor
        !if(bflav(3).eq.bflav(4)) then
          ! 72 = 2*2*3*3*2 (spin factor 2, colour factor 3, equal particles in the final state 2)
        !  symfac = 72d0 ! take spin and colour factors into account
        !else
          ! 36 = 2*2*3*3 (spin factor 2, colour factor 3)
        !  symfac = 36d0 ! take spin and colour factors into account
        !endif
        
        
        ! sets the correct symmetry factor/helicity flag depending on
        ! the gluon being in the initial or final state
        
        ! initial state gluon
        if(((bflav(1).eq.0).or.(bflav(2).eq.0)).and.
     &       (bflav(5).ne.0)) then
          symfac = 96
          helicities = 1955082
        ! final state gluon  
        elseif(((bflav(1).ne.0).and.(bflav(2).ne.0)).and.
     &       (bflav(5).eq.0)) then
	  symfac = 72    
	  helicities = 10824014 
	endif
        
        

	       !dd -> neu neu jet 
        if(     ((bflav(1).eq.1 .and. bflav(2).eq.-1) .or.
     &           (bflav(1).eq.-1 .and. bflav(2).eq.1)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
	  call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
       !uu -> neu neu jet
        elseif(((bflav(1).eq.-2 .and. bflav(2).eq.2) .or.
     &           (bflav(1).eq.2 .and. bflav(2).eq.-2)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call uubar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !ss -> neu neu jet
        elseif(((bflav(1).eq.3 .and. bflav(2).eq.-3) .or.
     &           (bflav(1).eq.-3 .and. bflav(2).eq.3)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !cc  -> neu neu jet
        elseif(((bflav(1).eq.-4 .and. bflav(2).eq.4) .or.
     &           (bflav(1).eq.4 .and. bflav(2).eq.-4)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call uubar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !bb  -> neu neu jet
        elseif(((bflav(1).eq.-5 .and. bflav(2).eq.5) .or.
     &           (bflav(1).eq.5 .and. bflav(2).eq.-5)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ddbar_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !dg -> neu neu jet
        elseif(((bflav(1).eq.1 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.1)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !ug -> neu neu jet
        elseif(((bflav(1).eq.2 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.2)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ug_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !sg -> neu neu jet
        elseif(((bflav(1).eq.3 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.3)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !cg -> neu neu jet
        elseif(((bflav(1).eq.4 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.4)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ug_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !bg -> neu neu jet
        elseif(((bflav(1).eq.5 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.5)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !dbar g -> neu neu jet
       elseif(((bflav(1).eq.-1 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-1)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !ubar g -> neu neu jet
        elseif(((bflav(1).eq.-2 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-2)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ubarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !sbar g -> neu neu jet
        elseif(((bflav(1).eq.-3 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-3)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !cbar g -> neu neu jet
        elseif(((bflav(1).eq.-4 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-4)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call ubarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20
        !bbar g -> neu neu jet
        elseif(((bflav(1).eq.-5 .and. bflav(2).eq.0) .or.
     &           (bflav(1).eq.0 .and. bflav(2).eq.-5)) .and. 
     &           (bflav(3).eq.1000022 .and. bflav(4).eq.1000022) ) then
          call dbarg_n1n1j_squaredME(fc_result, helicities, flags)
          goto 20

      
      
        else
          print*, "ERROR: Wrong flavor structure in subroutine setborn."
          print*, "bflav = ", bflav
          stop
          fc_result(1) = 1.d-15
          fc_result(2) = 1.d-15
        endif
        
        !fc_result ist set to a small number, preferably machineepsilon
        !because setting it to 0 leads to runtime problems during the
        !integration part
        
        if(fc_result(1).eq.0) then
          fc_result(1) = 1.d-15
	endif
	
	if(fc_result(2).eq.0) then
          fc_result(2) = 1.d-15
	endif
	
	

 20   continue
        
        born = fc_result(1)/symfac ! take spin and colour factors into account
        fborn = born


        
        if (verbose) then
          print*, "process: ", bflav
          print*, "s  = ", s
          print*, "t  = ", t
          print*, "u  = ", u
#ifdef DEBUGQ
          print*, "MN = ", par_Fin1mass
          print*, "MN = ", par_Fin2mass
          print*, "MZ = ", par_Zmass
          print*, "MSD= ", MSf(1,3,1)
#endif
          print*, "born (formcalc) = ", fborn
#ifdef DEBUGQ
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)
          enddo
#endif
          print*
        endif
        
        ! restore flavor list
        bflav = flavc
        p = pc
        
c ---------------------------- End FormCalc Born -----------------------
c endif preprocessor condition "if defined(CHECK_FORM_MAD) || defined(FORM_BORN)"
#endif


#if defined(CHECK_FORM_MAD) || defined(MAD_BORN)
! ---------------------------- MadGraph Born ---------------------------

        ! store flavor list, so we can operate on it without
        ! affecting the other program
        flavc = bflav
        pc = p
        
        ! calculate the couplings for this PS point
        call set_ebe_couplings
        call sborn_proc(p,bflav,born,bornjk,bmunu)
        mborn = born
        
        if (verbose) then
          print*, "process: ", bflav
          print*, "s  = ", s
          print*, "t  = ", t
          print*, "u  = ", u
#ifdef DEBUGQ
          print*, "MN = ", par_Fin1mass
          print*, "MN = ", par_Fin2mass
          print*, "MZ = ", par_Zmass
          print*, "MSD= ", par_MSf(1,3,1)
#endif
          print*, "born (madgraph) = ", mborn
#ifdef DEBUGQ
          do j=1,nlegborn
            print*, "bornjk = ", bornjk(j,:)
          enddo
#endif
          print*
        endif

        ! restore flavor list
        bflav = flavc 
        
        p = pc
        
c ---------------------------- End Madgraph Born -----------------------
c endif preprocessor condition "if defined(CHECK_FORM_MAD) || defined(MAD_BORN)"
#endif


#ifdef CHECK_FORM_MAD
        ! compare the formcalc and madgraph amplitudes  !error : 1d-09
        if( (abs((fborn - mborn)/(fborn + mborn)) > 2) ) then ! the relative error is probably larger for smaller amplitudes
          print*, "Error: formcalc and madgraph borns are too different"
          print*, "in process: ", bflav
          print*, "born (formcalc) = ", fborn
          print*, "born (madgraph) = ", mborn
          print*, "rel. err. = ", abs((fborn - mborn)/(fborn + mborn))
          print*
          print*, "Check input parameters or spin and colour factors in"
     &          //" formcalc amplitudes."
          stop
        endif
#endif
 

! If the program calculates both madgraph and formcalc borns (preprocessor flag CHECK_FORM_MAD)
! the user can choose which born amplitude powheg should use (formcalc or madgraph)
#if defined(CHECK_FORM_MAD) && defined(MAD_BORN)
        born = mborn ! (default)
#endif
#if defined(CHECK_FORM_MAD) && defined(FORM_BORN)
        born = fborn
#endif



#ifdef DEBUG1
        if(counter < 10) then
          print*, "process: ", bflav
          print*, "s  = ", s
          !print*, "t  = ", t
          !print*, "u  = ", u
          print*, "born (madgraph) = ", mborn
          print*, "born (formcalc) = ", fborn
          counter = counter + 1
        endif
        if(counter.eq.10) then
        print*,"counter in born.f l. 417 reached 10 --> stopping"
        !stop
        endif
#endif

      end

c############### end setborn subroutine ################################
