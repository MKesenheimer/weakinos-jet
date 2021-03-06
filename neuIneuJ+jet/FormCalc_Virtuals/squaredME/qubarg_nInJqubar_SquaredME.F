*#define CHECK

#if 0
* qubarg_nInJqubar_SquaredME.F
* assembly of squared matrix element
* generated by FormCalc 9.4 (7 Jun 2016) on 6-Mar-2017 19:04
#endif

#include "qubarg_nInJqubar_vars.h"
#include "virt_inline.h"
#include "virt_contains.h"


************************************************************************

        subroutine qubarg_nInJqubar_SquaredMEHel(HelInd(vmax,res), flags)
        implicit none
        SIMD_ONLY(integer vmax)
        ResType res(HelDim(*))
        integer flags

#include "qubarg_nInJqubar_vars.h"

        HelType amp
        ResType ampsq
#if DEBUG >= 3
        SIMD_ONLY(integer v)
#endif

* BEGIN ABBR_HEL
        call qubarg_nInJqubar_abbr0h
        TEST(flags, BIT_LOOP)
        call qubarg_nInJqubar_abbr1h1
        call qubarg_nInJqubar_abbr1h2
        call qubarg_nInJqubar_abbr1h3
        call qubarg_nInJqubar_abbr1h4
        call qubarg_nInJqubar_abbr1h5
        call qubarg_nInJqubar_abbr1h6
        call qubarg_nInJqubar_abbr1h7
        call qubarg_nInJqubar_abbr1h8
        call qubarg_nInJqubar_abbr1h9
        call qubarg_nInJqubar_abbr1h10
        call qubarg_nInJqubar_abbr1h11
        call qubarg_nInJqubar_abbr1h12
        call qubarg_nInJqubar_abbr1h13
        call qubarg_nInJqubar_abbr1h14
        call qubarg_nInJqubar_abbr1h15
        call qubarg_nInJqubar_abbr1h16
        call qubarg_nInJqubar_abbr1h17
        call qubarg_nInJqubar_abbr1h18
        call qubarg_nInJqubar_abbr1h19
        call qubarg_nInJqubar_abbr1h20
        call qubarg_nInJqubar_abbr1h21
        call qubarg_nInJqubar_abbr1h22
        call qubarg_nInJqubar_abbr1h23
        call qubarg_nInJqubar_abbr1h24
        call qubarg_nInJqubar_abbr1h25
        call qubarg_nInJqubar_abbr1h26
        call qubarg_nInJqubar_abbr1h27
        ENDTEST(flags, BIT_LOOP)
* END ABBR_HEL

        ampsq = 0
* BEGIN FF_TREE
        Ctree(HelAll(1)) = 0

        call qubarg_nInJqubar_born

        LOOP(Sfe6, 1,2,1)
        call qubarg_nInJqubar_born_Sfe6
        ENDLOOP(Sfe6)
* END FF_TREE

* BEGIN M2_TREE
        amp = 0
        amp = amp + Ctree(HelAll(1))*MatSUN(1,1)
        ampsq = ampsq + Re(amp*Conjugate(Ctree(HelAll(1))))
* END M2_TREE
        res(HelAll(1)) = ampsq

        ampsq = 0
        TEST(flags, BIT_LOOP)
* BEGIN FF_LOOP
        Cloop(HelAll(1)) = 0

        call qubarg_nInJqubar_self
        call qubarg_nInJqubar_vert
        call qubarg_nInJqubar_box

        LOOP(Gen6, 1,3,1)
        call qubarg_nInJqubar_vert_Gen6
        ENDLOOP(Gen6)

        LOOP(Sfe6, 1,2,1)
        call qubarg_nInJqubar_self_Sfe6
        call qubarg_nInJqubar_vert_Sfe6
        call qubarg_nInJqubar_box_Sfe6
        call qubarg_nInJqubar_pent_Sfe6
        ENDLOOP(Sfe6)

        LOOP(Sfe7, 1,2,1)
        call qubarg_nInJqubar_vert_Sfe7
        ENDLOOP(Sfe7)

        LOOP(Sfe6, 1,2,1)
        LOOP(Gen6, 1,3,1)
        call qubarg_nInJqubar_vert_Gen6_Sfe6
        call qubarg_nInJqubar_box_Gen6_Sfe6
        ENDLOOP(Gen6)
        ENDLOOP(Sfe6)

        LOOP(Sfe7, 1,2,1)
        LOOP(Sfe6, 1,2,1)
        call qubarg_nInJqubar_self_Sfe6_Sfe7
        call qubarg_nInJqubar_vert_Sfe6_Sfe7
        call qubarg_nInJqubar_box_Sfe6_Sfe7
        call qubarg_nInJqubar_pent_Sfe6_Sfe7
        ENDLOOP(Sfe6)
        ENDLOOP(Sfe7)

        LOOP(Sfe7, 1,2,1)
        LOOP(Sfe6, 1,2,1)
        LOOP(Gen6, 1,3,1)
        call qubarg_nInJqubar_vert_Gen6_Sfe6_Sfe7
        ENDLOOP(Gen6)
        ENDLOOP(Sfe6)
        ENDLOOP(Sfe7)

        LOOP(Sfe8, 1,2,1)
        LOOP(Sfe7, 1,2,1)
        LOOP(Sfe6, 1,2,1)
        call qubarg_nInJqubar_self_Sfe6_Sfe7_Sfe8
        ENDLOOP(Sfe6)
        ENDLOOP(Sfe7)
        ENDLOOP(Sfe8)
* END FF_LOOP

* BEGIN M2_LOOP
        amp = 0
        amp = amp + Cloop(HelAll(1))*MatSUN(1,1)
        ampsq = ampsq + Re(amp*Conjugate(Ctree(HelAll(1))))
        ampsq = ampsq + ampsq
* END M2_LOOP
        ENDTEST(flags, BIT_LOOP)
        res(HelAll(2)) = ampsq

#if DEBUG >= 3
        print 1, HelLoop(Hel(HelInd(v,1:LEGS)), res(HelInd(v,1:2)), v,vmax)
1        format(' sqme(', LEGS I3, ') =', 2F25.12)
#endif
        end

************************************************************************

        subroutine qubarg_nInJqubar_SquaredME(result, helicities, flags)
        implicit none
        RealType result(*)
        integer*8 helicities
        integer flags

#include "qubarg_nInJqubar_vars.h"
#include "qubarg_nInJqubar_specs.h"

* BEGIN VAR_DECL
        integer*8 hbits, hlast
        integer seq(2)
        save hlast, seq
        integer i, h, hmax
        parameter (hmax = 32)
        SIMD_ONLY(integer v)
        integer hsimd
        parameter (hsimd = SIMD_CEIL(hmax))
        ResType res(HelDim(2),hsimd)
        RealType rtree, rloop
        external qubarg_nInJqubar_SquaredMEHel
* END VAR_DECL

* BEGIN HSEL_DECL
        RealType norm
        RealType hseltest(0:hmax-1)
        ResType hseltest_v(HelDim(hsimd))
        equivalence (hseltest_v, hseltest)
        RealType hselmin
        integer hseli
        save hseltest, hselmin, hseli
* END HSEL_DECL

        data MatSUN /NaN(1)/
* -------- MK: added -----------------------------------------------------------
#include "skiphel_vars.h"
* -------- MK: added -----------------------------------------------------------

#include "virt_inline.h"

* BEGIN SETMASS
        TEST(flags, BIT_SETMASS)
        result(1) = Mf(3,Gen(1))
        result(2) = 0
        result(3) = MNeu(Neu(3))
        result(4) = MNeu(Neu(4))
        result(5) = Mf(3,Gen(5))
        return
        ENDTEST(flags, BIT_SETMASS)
* END SETMASS

* BEGIN INVARIANTS
        S = SInvariant(1,2)
        T = TInvariant(1,3)
        T14 = TInvariant(1,4)
        U = TInvariant(2,3)
        T24 = TInvariant(2,4)
        S34 = SInvariant(3,4)
        SC = dcmplx(S)
        TC = dcmplx(T)
        T14C = dcmplx(T14)
        UC = dcmplx(U)
        T24C = dcmplx(T24)
        S34C = dcmplx(S34)
* END INVARIANTS

        CHK_INI(seq)

        TEST(flags, BIT_RESET)
        hlast = 0
        seq(1) = seq(1) + 1
        INI_S(4)
* BEGIN ABBR_S
        call qubarg_nInJqubar_abbr0s
        TEST(flags, BIT_LOOP)
        call qubarg_nInJqubar_abbr1s1
        call qubarg_nInJqubar_abbr1s2
        ENDTEST(flags, BIT_LOOP)
* END ABBR_S
        ENDTEST(flags, BIT_RESET)

        seq(2) = seq(2) + 1
        INI_A()
* BEGIN ABBR_ANGLE
        call qubarg_nInJqubar_abbr0a
        TEST(flags, BIT_LOOP)
        call qubarg_nInJqubar_abbr1a
        ENDTEST(flags, BIT_LOOP)
* END ABBR_ANGLE

* BEGIN HEL_LOOP
        SIMD_ONLY(v = 1)
        h = 0

        do i = 0, hmax - 1
* BEGIN HSEL_IF
          if( hseltest(i) .lt. hselmin ) cycle
* END HSEL_IF

#define hval(s,m,a,b) a*mod(i/s,m)-b

          Hel0(1) = hval(16,2,2,1)
          Hel0(2) = hval(8,2,2,1)
          Hel0(3) = hval(4,2,2,1)
          Hel0(4) = hval(2,2,2,1)
          Hel0(5) = hval(1,2,2,1)
          hbits = Generic(ARG_HEL,JOIN_HEL)
          if( iand(helicities, hbits) .ne. hbits ) cycle
#if DEBUG >= 1
          if( helicities .ne. hlast ) print 1, ' helicities: ', Hel0
#endif
          SIMD_ONLY(call VecCopy(v, LEGS))
          SIMD_ONLY(v = mod(v, SIMD) + 1)
          SIMD_ONLY(if( v .eq. 1 ) then)
          h = h + 1
          
* -------- MK: added -----------------------------------------------------------
#if NSKIP > 0
          if(.not.skiphel(h)) then
            call qubarg_nInJqubar_SquaredMEHel(HelInd(SIMD,res(HelAll(1),h)), flags)
          else
            res(HelAll(1),h) = 0D0
#if DEBUG >= 1
            print*,"skipping helicity ",h
#endif
          endif
          ! determine which hel-amplitudes to skip during the initialisation phase
          if(initskip) then
            if(res(HelAll(1),h).eq.0D0) mskip(h) = mskip(h) + 1
            if(mskip(h).ge.NSKIP) skiphel(h) = .true.
            if(minit.ge.ninit) initskip = .false.
            minit = minit + 1
          endif
#else
          call qubarg_nInJqubar_SquaredMEHel(HelInd(SIMD,res(HelAll(1),h)), flags)
#endif
#if DEBUG >= 2
          print*,h,res(HelAll(1),h)
#endif
* -------- MK: added -----------------------------------------------------------

          SIMD_ONLY(endif)
        enddo

        SIMD_ONLY(if( v .ne. 1 ) then)
        SIMD_ONLY(h = h + 1)
        SIMD_ONLY(call qubarg_nInJqubar_SquaredMEHel(HelInd(v,res(HelAll(1),h)), flags))
        SIMD_ONLY(endif)
        DEINI()
        SIMD_ONLY(if( v .ne. 1 ) res(v:SIMD,:,h) = 0)
* END HEL_LOOP

* BEGIN RESULT
        rtree = 0
        rloop = 0
        do i = 1, h
          rtree = rtree + HelSum(res(HelAll(1),i))
          rloop = rloop + HelSum(res(HelAll(2),i))
        enddo
        result(1) = rtree
        TEST(flags, BIT_LOOP)
#ifdef WF_RENORMALIZATION
        rloop = rloop + (WF_RENORMALIZATION)*rtree
#endif
        result(2) = rloop
        ENDTEST(flags, BIT_LOOP)

#if DEBUG >= 2
        print *, PROCNAME, ' =', rtree, rloop
#endif
* END RESULT

* BEGIN HSEL_SET
        if( helicities .ne. hlast ) then
          hseltest = 0
          hselmin = 0
          hseli = 0
        endif
        if( hseli .lt. hseln ) then
          norm = 1/(rtree + rloop)
          do i = 1, hsimd
            hseltest_v(HelAll(i)) = hseltest_v(HelAll(i)) +
     &        abs(norm*(res(HelAll(1),i) + res(HelAll(2),i)))
          enddo
          hseli = hseli + 1
          if( hseli .eq. hseln ) then
            hselmin = 0
            do i = 0, hmax - 1
              hselmin = max(hselmin, hseltest(i))
            enddo
            hselmin = hselmin*hseleps
            do i = 0, hmax - 1
              if( hseltest(i) .ge. hselmin ) cycle
              Hel0(1) = hval(16,2,2,1)
              Hel0(2) = hval(8,2,2,1)
              Hel0(3) = hval(4,2,2,1)
              Hel0(4) = hval(2,2,2,1)
              Hel0(5) = hval(1,2,2,1)
              print 1, ' neglecting ', Hel0
1              format(A, LEGS I3)
            enddo
          endif
        endif
* END HSEL_SET

#ifdef CHECK
        print *, 'S =', S
        print *, 'T =', T
        print *, 'T14 =', T14
        print *, 'U =', U
        print *, 'T24 =', T24
        print *, 'S34 =', S34
        print *, 'tree =', rtree
        print *, 'loop =', rloop
        stop
#endif

        hlast = helicities

#include "virt_contains.h"

        end

