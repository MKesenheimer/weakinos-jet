! Collier-LT interface to cope with the Nput subroutines used by FormCalc.
!
!
! 1. In Collier there are two different scales, mu_ir and mu_uv,
! while in LT there is only a single mu scale => do not forget
! to set in the main program mu_uv = mu_ir
!
! 2. LT uses lambda variable to control the IR divergences,
! Collier uses delta_ir1 and delta_ir2 in the DR scheme.
! The interface reproduces the lambda-scheme, so the user
! does not need to care about delta_ir1 and delta_ir2 anymore.
!
! 3. LT functions not needed: setdelta, setmudim, setuvdiv
!    Collier functions not needed: SetDeltaIR_cll
!
! 4. Collier functions needed: SetDeltaIR_cll, SetMuUV2_cll,SetMuIR2_cll
!    LT functions neeeded: setlambda

#include "lt_types.h"

subroutine Aput(Atab,m)
  use COLLIER
  implicit none
#include "lt_collier.h"
  ! arguments
  ComplexType, intent(out) :: Atab(Naa)
  ComplexType, intent(in)  :: m
  ! local variables
  integer :: i,j,j0
  RealType :: DeltaIR1,DeltaIR2
  RealType :: DeltaIR1s,DeltaIR2s
  ComplexType, allocatable :: Acoeff(:),Acoeffuv(:)
  RealType, allocatable :: Aerr(:)
  ! parameters
  integer, parameter :: rank = 2
  RealType, parameter :: pi = 4.d0*atan(1.d0)

  ! character*10 coeffname(Naa)
  !
  ! data (coeffname(i), i = 1, Naa) /
  ! &     'aa0', 'aa0:1', 'aa0:2',
  ! &     'aa00', 'aa00:1', 'aa00:2' /

  allocate(Acoeff(0:rank/2))
  allocate(Acoeffuv(0:rank/2))
  allocate(Aerr(0:rank))

  call GetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

  ! finite part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = pi**2/6d0
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)

  call A_cll(Acoeff,Acoeffuv,m,rank,Aerr)

  do i = 0,(Naa/3-1)
     j = 1+3*i
     Atab(j) = Acoeff(i)
  enddo

  ! 1/eps part of the loop functions:
  DeltaIR1 = 1d0
  DeltaIR2 = pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call A_cll(Acoeff,Acoeffuv,m,rank,Aerr)

  do i = 0,(Naa/3-1)
     j0 = 1+3*i
     j  = j0+1
     Atab(j) = Acoeff(i) - Atab(j0)
  enddo

  ! 1/eps^2 part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = 1d0+pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call A_cll(Acoeff,Acoeffuv,m,rank,Aerr)

  do i = 0,(Naa/3-1)
     j0 = 1+3*i
     j  = j0+2
     Atab(j) = Acoeff(i) - Atab(j0)
  enddo

  deallocate(Acoeff,Acoeffuv,Aerr)

  call SetDeltaIR_cll(DeltaIR1s,DeltaIR2s) 

#if DEBUG > 4
  do i=1,Naa
    if(isNaN(dble(Atab(i))) .or. isNaN(dimag(Atab(i)))) then
      print*,"Atab",Atab(:)
      print*,"m",m
      stop
    endif
  enddo
#endif

end subroutine Aput

! ----------------------------------------------------------------------

subroutine Bput(Btab,p10,m02,m12)
  use COLLIER
  implicit none
#include "lt_collier.h"
  ! arguments
  ComplexType, intent(out) :: Btab(Nbb)
  ComplexType, intent(in)  :: p10,m02,m12
  ! local variables
  integer :: i,j,j0
  RealType :: DeltaIR1,DeltaIR2
  RealType :: DeltaIR1s,DeltaIR2s
  ComplexType, allocatable :: Bcoeff(:),Bcoeffuv(:)
  ComplexType, allocatable :: DBcoeff(:,:),DBcoeffuv(:,:)
  RealType, allocatable :: Berr(:),DBerr(:)
  ! parameters
  integer, parameter :: rank = 3
  RealType, parameter :: pi = 4.d0*atan(1.d0)

  ! character*10 coeffname(Nbb)
  !
  ! data (coeffname(i), i = 1, Nbb) /
  ! &     'bb0', 'bb0:1', 'bb0:2',
  ! &     'bb1', 'bb1:1', 'bb1:2',
  ! &     'bb00', 'bb00:1', 'bb00:2',
  ! &     'bb11', 'bb11:1', 'bb11:2',
  ! &     'bb001', 'bb001:1', 'bb001:2',
  ! &     'bb111', 'bb111:1', 'bb111:2',
  ! &     'dbb0', 'dbb0:1', 'dbb0:2',
  ! &     'dbb1', 'dbb1:1', 'dbb1:2',
  ! &     'dbb00', 'dbb00:1', 'dbb00:2',
  ! &     'dbb11', 'dbb11:1', 'dbb11:2',
  ! &     'dbb001', 'dbb001:1', 'dbb001:2' /

  call GetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

  allocate(Bcoeff(GetNc_cll(2,rank)))
  allocate(Bcoeffuv(GetNc_cll(2,rank)))
  allocate(Berr(0:rank))
  allocate(DBcoeff(0:rank/2,0:rank))
  allocate(DBcoeffuv(0:rank/2,0:rank))
  allocate(DBerr(0:rank))

  ! finite part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = pi**2/6d0
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)

  call B_cll(Bcoeff,Bcoeffuv,p10,m02,m12,rank,Berr)
  call DB_cll(DBcoeff,DBcoeffuv,p10,m02,m12,rank,DBerr)

  do i = 1,(Nbb/3)
     j = 1+3*(i-1)
     if(i.le.6) then
        Btab(j) = Bcoeff(i)
     else
        select case (i)
        case(7)
           Btab(j) = DBcoeff(0,0)
        case(8)
           Btab(j) = DBcoeff(0,1)
        case(9)
           Btab(j) = DBcoeff(1,0)
        case(10)
           Btab(j) = DBcoeff(0,2)
        case(11)
           Btab(j) = DBcoeff(1,1)
        end select
     endif
  enddo

  ! 1/eps part of the loop functions:
  DeltaIR1 = 1d0
  DeltaIR2 = pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call B_cll(Bcoeff,Bcoeffuv,p10,m02,m12,rank,Berr)
  call DB_cll(DBcoeff,DBcoeffuv,p10,m02,m12,rank,DBerr)

  do i = 1,(Nbb/3)
     j0  = 1+3*(i-1)
     j   = j0+1
     if(i.le.6) then
        Btab(j) = Bcoeff(i) - Btab(j0)
     else
        select case (i)
        case(7)
           Btab(j) = DBcoeff(0,0) - Btab(j0)
        case(8)
           Btab(j) = DBcoeff(0,1) - Btab(j0)
        case(9)
           Btab(j) = DBcoeff(1,0) - Btab(j0)
        case(10)
           Btab(j) = DBcoeff(0,2) - Btab(j0)
        case(11)
           Btab(j) = DBcoeff(1,1) - Btab(j0)
        end select
     endif
  enddo

  ! 1/eps^2 part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = 1d0+pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call B_cll(Bcoeff,Bcoeffuv,p10,m02,m12,rank,Berr)
  call DB_cll(DBcoeff,DBcoeffuv,p10,m02,m12,rank,DBerr)

  do i = 1,(Nbb/3)
     j0  = 1+3*(i-1)
     j   = j0+2
     if(i.le.6) then
        Btab(j) = Bcoeff(i) - Btab(j0)
     else
        select case (i)
        case(7)
           Btab(j) = DBcoeff(0,0) - Btab(j0)
        case(8)
           Btab(j) = DBcoeff(0,1) - Btab(j0)
        case(9)
           Btab(j) = DBcoeff(1,0) - Btab(j0)
        case(10)
           Btab(j) = DBcoeff(0,2) - Btab(j0)
        case(11)
           Btab(j) = DBcoeff(1,1) - Btab(j0)
        end select
     endif
  enddo

  deallocate(Bcoeff,Bcoeffuv,Berr,DBcoeff,DBcoeffuv,DBerr)

  call SetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

#if DEBUG > 4
  do i=1,Nbb
    if(isNaN(dble(Btab(i))) .or. isNaN(dimag(Btab(i)))) then
      print*,"Btab",Btab(:)
      print*,"p10",p10
      print*,"m02",m02
      print*,"m12",m12
      stop
    endif
  enddo
#endif

end subroutine Bput

! ----------------------------------------------------------------------

subroutine Cput(Ctab,p10,p21,p20,m02,m12,m22)
  use COLLIER
  implicit none
#include "lt_collier.h"
  ! arguments
  ComplexType, intent(out) :: Ctab(Ncc)
  ComplexType, intent(in)  :: p10,p21,p20,m02,m12,m22
  ! local variables
  integer :: i,j,j0
  RealType :: DeltaIR1,DeltaIR2
  RealType :: DeltaIR1s,DeltaIR2s
  ComplexType, allocatable :: Ccoeff(:),Ccoeffuv(:)
  RealType, allocatable :: Cerr(:)
  ! parameters
  integer, parameter :: rank = 4
  RealType, parameter :: pi = 4.d0*atan(1.d0)
  
  ! character*10 coeffname(Ncc)
  !
  ! data (coeffname(i), i = 1, Ncc) /
  ! &    'cc0', 'cc0:1', 'cc0:2',
  ! &    'cc1', 'cc1:1', 'cc1:2',
  ! &    'cc2', 'cc2:1', 'cc2:2',
  ! &    'cc00', 'cc00:1', 'cc00:2',
  ! &    'cc11', 'cc11:1', 'cc11:2',
  ! &    'cc12', 'cc12:1', 'cc12:2',
  ! &    'cc22', 'cc22:1', 'cc22:2',
  ! &    'cc001', 'cc001:1', 'cc001:2',
  ! &    'cc002', 'cc002:1', 'cc002:2',
  ! &    'cc111', 'cc111:1', 'cc111:2',
  ! &    'cc112', 'cc112:1', 'cc112:2',
  ! &    'cc122', 'cc122:1', 'cc122:2',
  ! &    'cc222', 'cc222:1', 'cc222:2',
  ! &    'cc0000', 'cc0000:1', 'cc0000:2',
  ! &    'cc0011', 'cc0011:1', 'cc0011:2',
  ! &    'cc0012', 'cc0012:1', 'cc0012:2',
  ! &    'cc0022', 'cc0022:1', 'cc0022:2',
  ! &    'cc1111', 'cc1111:1', 'cc1111:2',
  ! &    'cc1112', 'cc1112:1', 'cc1112:2',
  ! &    'cc1122', 'cc1122:1', 'cc1122:2',
  ! &    'cc1222', 'cc1222:1', 'cc1222:2',
  ! &    'cc2222', 'cc2222:1', 'cc2222:2' /
  
  call GetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

  allocate(Ccoeff(GetNc_cll(3,rank)))
  allocate(Ccoeffuv(GetNc_cll(3,rank)))
  allocate(Cerr(0:rank))

  ! finite part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = pi**2/6d0
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)

  call C_cll(Ccoeff,Ccoeffuv,p10,p21,p20,m02,m12,m22,rank,Cerr)
  !call C_cll(Ccoeff,Ccoeffuv,p20,p21,p10,m02,m22,m12,rank,Cerr) ! symmetry
  !call C_cll(Ccoeff,Ccoeffuv,p10,p20,p21,m12,m02,m22,rank,Cerr) ! symmetry
  !call C_cll(Ccoeff,Ccoeffuv,p21,p20,p10,m12,m22,m02,rank,Cerr) ! symmetry
  !call C_cll(Ccoeff,Ccoeffuv,p20,p10,p21,m22,m02,m12,rank,Cerr) ! symmetry
  !call C_cll(Ccoeff,Ccoeffuv,p21,p10,p20,m22,m12,m02,rank,Cerr) ! symmetry
  
  do i = 1,(Ncc/3)
     j = 1+3*(i-1)
     Ctab(j) = Ccoeff(i)     
  enddo

  ! 1/eps part of the loop functions:
  DeltaIR1 = 1d0
  DeltaIR2 = pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call C_cll(Ccoeff,Ccoeffuv,p10,p21,p20,m02,m12,m22,rank,Cerr)

  do i = 1,(Ncc/3)
     j0  = 1+3*(i-1)
     j   = j0+1
     Ctab(j) = Ccoeff(i) - Ctab(j0)
  enddo

  ! 1/eps^2 part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = 1d0+pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call C_cll(Ccoeff,Ccoeffuv,p10,p21,p20,m02,m12,m22,rank,Cerr)

  do i = 1,(Ncc/3)
     j0  = 1+3*(i-1)
     j   = j0+2
     Ctab(j) = Ccoeff(i) - Ctab(j0)
  enddo

  deallocate(Ccoeff,Ccoeffuv,Cerr)

  call SetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

#if DEBUG > 4
  do i=1,Ncc
    if(isNaN(dble(Ctab(i))) .or. isNaN(dimag(Ctab(i)))) then
      print*,"Ctab",Ctab(:)
      print*,"p10",p10
      print*,"p21",p21
      print*,"p20",p20
      print*,"m02",m02
      print*,"m12",m12
      print*,"m22",m22
      stop
    endif
  enddo  
#endif

end subroutine Cput

! ----------------------------------------------------------------------    

subroutine Dput(Dtab,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32)
  use COLLIER
  implicit none
#include "lt_collier.h"
  ! arguments
  ComplexType, intent(out) :: Dtab(Ndd)
  ComplexType, intent(in)  :: p10,p21,p32,p30,p20,p31,m02,m12,m22,m32
  ! local variables
  integer :: i,j,j0
  RealType :: DeltaIR1,DeltaIR2
  RealType :: DeltaIR1s,DeltaIR2s
  ComplexType, allocatable :: Dcoeff(:),Dcoeffuv(:)
  RealType, allocatable :: Derr(:)
  ! parameters
  integer, parameter :: rank = 5
  RealType, parameter :: pi = 4.d0*atan(1.d0)

  ! character*10 coeffname(Ndd)
  !
  ! data (coeffname(i), i = 1, Ndd) /
  ! &    'dd0', 'dd0:1', 'dd0:2',
  ! &    'dd1', 'dd1:1', 'dd1:2',
  ! &    'dd2', 'dd2:1', 'dd2:2',
  ! &    'dd3', 'dd3:1', 'dd3:2',
  ! &    'dd00', 'dd00:1', 'dd00:2',
  ! &    'dd11', 'dd11:1', 'dd11:2',
  ! &    'dd12', 'dd12:1', 'dd12:2',
  ! &    'dd13', 'dd13:1', 'dd13:2',
  ! &    'dd22', 'dd22:1', 'dd22:2',
  ! &    'dd23', 'dd23:1', 'dd23:2',
  ! &    'dd33', 'dd33:1', 'dd33:2',
  ! &    'dd001', 'dd001:1', 'dd001:2',
  ! &    'dd002', 'dd002:1', 'dd002:2',
  ! &    'dd003', 'dd003:1', 'dd003:2',
  ! &    'dd111', 'dd111:1', 'dd111:2', 
  ! &    'dd112', 'dd112:1', 'dd112:2',
  ! &    'dd113', 'dd113:1', 'dd113:2',
  ! &    'dd122', 'dd122:1', 'dd122:2',
  ! &    'dd123', 'dd123:1', 'dd123:2',
  ! &    'dd133', 'dd133:1', 'dd133:2',
  ! &    'dd222', 'dd222:1', 'dd222:2',
  ! &    'dd223', 'dd223:1', 'dd223:2',
  ! &    'dd233', 'dd233:1', 'dd233:2',
  ! &    'dd333', 'dd333:1', 'dd333:2',
  ! &    'dd0000', 'dd0000:1', 'dd0000:2',
  ! &    'dd0011', 'dd0011:1', 'dd0011:2',
  ! &    'dd0012', 'dd0012:1', 'dd0012:2',
  ! &    'dd0013', 'dd0013:1', 'dd0013:2',
  ! &    'dd0022', 'dd0022:1', 'dd0022:2',
  ! &    'dd0023', 'dd0023:1', 'dd0023:2',
  ! &    'dd0033', 'dd0033:1', 'dd0033:2',
  ! &    'dd1111', 'dd1111:1', 'dd1111:2',
  ! &    'dd1112', 'dd1112:1', 'dd1112:2',
  ! &    'dd1113', 'dd1113:1', 'dd1113:2',
  ! &    'dd1122', 'dd1122:1', 'dd1122:2',
  ! &    'dd1123', 'dd1123:1', 'dd1123:2',
  ! &    'dd1133', 'dd1133:1', 'dd1133:2',
  ! &    'dd1222', 'dd1222:1', 'dd1222:2',
  ! &    'dd1223', 'dd1223:1', 'dd1223:2',
  ! &    'dd1233', 'dd1233:1', 'dd1233:2',
  ! &    'dd1333', 'dd1333:1', 'dd1333:2',
  ! &    'dd2222', 'dd2222:1', 'dd2222:2',
  ! &    'dd2223', 'dd2223:1', 'dd2223:2',
  ! &    'dd2233', 'dd2233:1', 'dd2233:2',
  ! &    'dd2333', 'dd2333:1', 'dd2333:2',
  ! &    'dd3333', 'dd3333:1', 'dd3333:2',
  ! &    'dd00001', 'dd00001:1', 'dd00001:2',
  ! &    'dd00002', 'dd00002:1', 'dd00002:2',
  ! &    'dd00003', 'dd00003:1', 'dd00003:2',
  ! &    'dd00111', 'dd00111:1', 'dd00111:2',
  ! &    'dd00112', 'dd00112:1', 'dd00112:2',
  ! &    'dd00113', 'dd00113:1', 'dd00113:2',
  ! &    'dd00122', 'dd00122:1', 'dd00122:2',
  ! &    'dd00123', 'dd00123:1', 'dd00123:2',
  ! &    'dd00133', 'dd00133:1', 'dd00133:2',
  ! &    'dd00222', 'dd00222:1', 'dd00222:2',
  ! &    'dd00223', 'dd00223:1', 'dd00223:2',
  ! &    'dd00233', 'dd00233:1', 'dd00233:2',
  ! &    'dd00333', 'dd00333:1', 'dd00333:2',
  ! &    'dd11111', 'dd11111:1', 'dd11111:2', 
  ! &    'dd11112', 'dd11112:1', 'dd11112:2',
  ! &    'dd11113', 'dd11113:1', 'dd11113:2',
  ! &    'dd11122', 'dd11122:1', 'dd11122:2',
  ! &    'dd11123', 'dd11123:1', 'dd11123:2',
  ! &    'dd11133', 'dd11133:1', 'dd11133:2',
  ! &    'dd11222', 'dd11222:1', 'dd11222:2',
  ! &    'dd11223', 'dd11223:1', 'dd11223:2',
  ! &    'dd11233', 'dd11233:1', 'dd11233:2',
  ! &    'dd11333', 'dd11333:1', 'dd11333:2',
  ! &    'dd12222', 'dd12222:1', 'dd12222:2',
  ! &    'dd12223', 'dd12223:1', 'dd12223:2',
  ! &    'dd12233', 'dd12233:1', 'dd12233:2',
  ! &    'dd12333', 'dd12333:1', 'dd12333:2',
  ! &    'dd13333', 'dd13333:1', 'dd13333:2',
  ! &    'dd22222', 'dd22222:1', 'dd22222:2',
  ! &    'dd22223', 'dd22223:1', 'dd22223:2',
  ! &    'dd22233', 'dd22233:1', 'dd22233:2',
  ! &    'dd22333', 'dd22333:1', 'dd22333:2',
  ! &    'dd23333', 'dd23333:1', 'dd23333:2',
  ! &    'dd33333', 'dd33333:1', 'dd33333:2' /
  
  call GetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

  allocate(Dcoeff(GetNc_cll(4,rank)))
  allocate(Dcoeffuv(GetNc_cll(4,rank)))
  allocate(Derr(0:rank))

  ! finite part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = pi**2/6d0
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)

  call D_cll(Dcoeff,Dcoeffuv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32, &
            rank,Derr)

  do i = 1,(Ndd/3)
     j = 1+3*(i-1)
     Dtab(j) = Dcoeff(i)     
  enddo

  ! 1/eps part of the loop functions:
  DeltaIR1 = 1d0
  DeltaIR2 = pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call D_cll(Dcoeff,Dcoeffuv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32, & 
             rank,Derr)

  do i = 1,(Ndd/3)
     j0  = 1+3*(i-1)
     j   = j0+1
     Dtab(j) = Dcoeff(i) - Dtab(j0)
  enddo

  ! 1/eps^2 part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = 1d0+pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call D_cll(Dcoeff,Dcoeffuv,p10,p21,p32,p30,p20,p31,m02,m12,m22,m32, & 
             rank,Derr)

  do i = 1,(Ndd/3)
     j0  = 1+3*(i-1)
     j   = j0+2
     Dtab(j) = Dcoeff(i) - Dtab(j0)
  enddo

  deallocate(Dcoeff,Dcoeffuv,Derr)

  call SetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

#if DEBUG > 4
  do i=1,Ndd          
    if(isNaN(dble(Dtab(i))) .or. isNaN(dimag(Dtab(i)))) then
      print*,"Dtab",Dtab(:)
      print*,"p10",p10
      print*,"p21",p21
      print*,"p32",p32
      print*,"p30",p30
      print*,"p20",p20
      print*,"p31",p31
      print*,"m02",m02
      print*,"m12",m12
      print*,"m22",m22
      print*,"m32",m32
      stop        
    endif              
  enddo                  
#endif  

end subroutine Dput

! ----------------------------------------------------------------------
      
subroutine Eput(Etab,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41,m02,m12, &
                m22,m32,m42)
  use COLLIER
  implicit none
#include "lt_collier.h"
  ! arguments
  ComplexType, intent(out) :: Etab(Nee)
  ComplexType, intent(in)  :: p10,p21,p32,p43,p40,p20,p31,p42,p30,p41
  ComplexType, intent(in)  :: m02,m12,m22,m32,m42
  ! local variables
  integer :: i,j,j0
  RealType :: DeltaIR1,DeltaIR2
  RealType :: DeltaIR1s,DeltaIR2s
  ComplexType, allocatable :: Ecoeff(:),Ecoeffuv(:)
  RealType, allocatable :: Eerr(:)
  ! parameters
  integer, parameter :: rank = 6
  RealType, parameter :: pi = 4.d0*atan(1.d0)

  ! character*10 coeffname(Nee)
  !
  ! data (coeffname(i), i = 1, Nee) /
  ! &    'ee0', 'ee0:1', 'ee0:2',
  ! &    'ee1', 'ee1:1', 'ee1:2',
  ! &    'ee2', 'ee2:1', 'ee2:2',
  ! &    'ee3', 'ee3:1', 'ee3:2',
  ! &    'ee4', 'ee4:1', 'ee4:2',
  ! &    'ee00', 'ee00:1', 'ee00:2',
  ! &    'ee11', 'ee11:1', 'ee11:2',
  ! &    'ee12', 'ee12:1', 'ee12:2',
  ! &    'ee13', 'ee13:1', 'ee13:2',
  ! &    'ee14', 'ee14:1', 'ee14:2',
  ! &    'ee22', 'ee22:1', 'ee22:2',
  ! &    'ee23', 'ee23:1', 'ee23:2',
  ! &    'ee24', 'ee24:1', 'ee24:2',
  ! &    'ee33', 'ee33:1', 'ee33:2',
  ! &    'ee34', 'ee34:1', 'ee34:2',
  ! &    'ee44', 'ee44:1', 'ee44:2',
  ! &    'ee001', 'ee001:1', 'ee001:2',
  ! &    'ee002', 'ee002:1', 'ee002:2',
  ! &    'ee003', 'ee003:1', 'ee003:2',
  ! &    'ee004', 'ee004:1', 'ee004:2',
  ! &    'ee111', 'ee111:1', 'ee111:2',
  ! &    'ee112', 'ee112:1', 'ee112:2',
  ! &    'ee113', 'ee113:1', 'ee113:2',
  ! &    'ee114', 'ee114:1', 'ee114:2',
  ! &    'ee122', 'ee122:1', 'ee122:2',
  ! &    'ee123', 'ee123:1', 'ee123:2',
  ! &    'ee124', 'ee124:1', 'ee124:2',
  ! &    'ee133', 'ee133:1', 'ee133:2',
  ! &    'ee134', 'ee134:1', 'ee134:2',
  ! &    'ee144', 'ee144:1', 'ee144:2',
  ! &    'ee222', 'ee222:1', 'ee222:2',
  ! &    'ee223', 'ee223:1', 'ee223:2',
  ! &    'ee224', 'ee224:1', 'ee224:2',
  ! &    'ee233', 'ee233:1', 'ee233:2',
  ! &    'ee234', 'ee234:1', 'ee234:2',
  ! &    'ee244', 'ee244:1', 'ee244:2',
  ! &    'ee333', 'ee333:1', 'ee333:2',
  ! &    'ee334', 'ee334:1', 'ee334:2',
  ! &    'ee344', 'ee344:1', 'ee344:2',
  ! &    'ee444', 'ee444:1', 'ee444:2',
  ! &    'ee0000', 'ee0000:1', 'ee0000:2',
  ! &    'ee0011', 'ee0011:1', 'ee0011:2',
  ! &    'ee0012', 'ee0012:1', 'ee0012:2',
  ! &    'ee0013', 'ee0013:1', 'ee0013:2',
  ! &    'ee0014', 'ee0014:1', 'ee0014:2',
  ! &    'ee0022', 'ee0022:1', 'ee0022:2',
  ! &    'ee0023', 'ee0023:1', 'ee0023:2',
  ! &    'ee0024', 'ee0024:1', 'ee0024:2',
  ! &    'ee0033', 'ee0033:1', 'ee0033:2',
  ! &    'ee0034', 'ee0034:1', 'ee0034:2',
  ! &    'ee0044', 'ee0044:1', 'ee0044:2',
  ! &    'ee1111', 'ee1111:1', 'ee1111:2',
  ! &    'ee1112', 'ee1112:1', 'ee1112:2',
  ! &    'ee1113', 'ee1113:1', 'ee1113:2',
  ! &    'ee1114', 'ee1114:1', 'ee1114:2',
  ! &    'ee1122', 'ee1122:1', 'ee1122:2',
  ! &    'ee1123', 'ee1123:1', 'ee1123:2',
  ! &    'ee1124', 'ee1124:1', 'ee1124:2',
  ! &    'ee1133', 'ee1133:1', 'ee1133:2',
  ! &    'ee1134', 'ee1134:1', 'ee1134:2',
  ! &    'ee1144', 'ee1144:1', 'ee1144:2',
  ! &    'ee1222', 'ee1222:1', 'ee1222:2',
  ! &    'ee1223', 'ee1223:1', 'ee1223:2',
  ! &    'ee1224', 'ee1224:1', 'ee1224:2',
  ! &    'ee1233', 'ee1233:1', 'ee1233:2',
  ! &    'ee1234', 'ee1234:1', 'ee1234:2',
  ! &    'ee1244', 'ee1244:1', 'ee1244:2',
  ! &    'ee1333', 'ee1333:1', 'ee1333:2',
  ! &    'ee1334', 'ee1334:1', 'ee1334:2',
  ! &    'ee1344', 'ee1344:1', 'ee1344:2',
  ! &    'ee1444', 'ee1444:1', 'ee1444:2',
  ! &    'ee2222', 'ee2222:1', 'ee2222:2',
  ! &    'ee2223', 'ee2223:1', 'ee2223:2',
  ! &    'ee2224', 'ee2224:1', 'ee2224:2',
  ! &    'ee2233', 'ee2233:1', 'ee2233:2',
  ! &    'ee2234', 'ee2234:1', 'ee2234:2',
  ! &    'ee2244', 'ee2244:1', 'ee2244:2',
  ! &    'ee2333', 'ee2333:1', 'ee2333:2',
  ! &    'ee2334', 'ee2334:1', 'ee2334:2', 
  ! &    'ee2344', 'ee2344:1', 'ee2344:2',
  ! &    'ee2444', 'ee2444:1', 'ee2444:2',
  ! &    'ee3333', 'ee3333:1', 'ee3333:2',
  ! &    'ee3334', 'ee3334:1', 'ee3334:2',
  ! &    'ee3344', 'ee3344:1', 'ee3344:2',
  ! &    'ee3444', 'ee3444:1', 'ee3444:2',
  ! &    'ee4444', 'ee4444:1', 'ee4444:2' /

  call GetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

  allocate(Ecoeff(GetNc_cll(5,rank)))
  allocate(Ecoeffuv(GetNc_cll(5,rank)))
  allocate(Eerr(0:rank))

  ! finite part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = pi**2/6d0
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)

  call E_cll(Ecoeff,Ecoeffuv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
             m02,m12,m22,m32,m42,rank,Eerr)

  do i = 1,(Nee/3)
     j = 1+3*(i-1)
     Etab(j) = Ecoeff(i)     
  enddo

  ! 1/eps part of the loop functions:
  DeltaIR1 = 1d0
  DeltaIR2 = pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call E_cll(Ecoeff,Ecoeffuv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
             m02,m12,m22,m32,m42,rank,Eerr)

  do i = 1,(Nee/3)
     j0  = 1+3*(i-1)
     j   = j0+1
     Etab(j) = Ecoeff(i) - Etab(j0)
  enddo

  ! 1/eps^2 part of the loop functions:
  DeltaIR1 = 0d0
  DeltaIR2 = 1d0+pi**2/6d0  
  call SetDeltaIR_cll(DeltaIR1,DeltaIR2)
  
  call E_cll(Ecoeff,Ecoeffuv,p10,p21,p32,p43,p40,p20,p31,p42,p30,p41, &
             m02,m12,m22,m32,m42,rank,Eerr)

  do i = 1,(Nee/3)
     j0  = 1+3*(i-1)
     j   = j0+2
     Etab(j) = Ecoeff(i) - Etab(j0)
  enddo

  deallocate(Ecoeff,Ecoeffuv,Eerr)

  call SetDeltaIR_cll(DeltaIR1s,DeltaIR2s)

#if DEBUG > 4 
  do i=1,Nee
    if(isNaN(dble(Etab(i))) .or. isNaN(dimag(Etab(i)))) then
      print*,"Etab",Etab(:)
      print*,"p10",p10
      print*,"p21",p21
      print*,"p32",p32
      print*,"p43",p43
      print*,"p40",p40
      print*,"p20",p20
      print*,"p31",p31
      print*,"p42",p42
      print*,"p30",p30
      print*,"p41",p41
      print*,"m02",m02
      print*,"m12",m12
      print*,"m22",m22
      print*,"m32",m32
      print*,"m42",m42
      stop        
    endif              
  enddo                  
#endif 

end subroutine Eput

! ----------------------------------------------------------------------

ComplexType function A0i(i, m)
  implicit none
#include "lt_collier.h"
#include "lt_ff.h"
  integer, intent(in) :: i
  ComplexType, intent(in) :: m
  ComplexType :: aa(Naa)
  call Aput(aa,m)
  A0i = aa(i+epsi)
end function A0i

! ----------------------------------------------------------------------

ComplexType function A0(m)
  implicit none
  ComplexType m,A0i
  external A0i
  A0 = A0i(1,m)
end function A0

! ----------------------------------------------------------------------

ComplexType function B0i(i,p,m1,m2)
  implicit none
#include "lt_collier.h"
#include "lt_ff.h"
  integer, intent(in) :: i
  ComplexType, intent(in) :: p,m1,m2
  ComplexType :: bb(Nbb)
  call Bput(bb,p,m1,m2)
  B0i = bb(i+epsi)
end function B0i

! ----------------------------------------------------------------------

ComplexType function C0i(i,p1,p2,p1p2,m1,m2,m3)
  implicit none
#include "lt_collier.h"
#include "lt_ff.h"
  integer, intent(in) :: i
  ComplexType, intent(in) :: p1,p2,p1p2,m1,m2,m3
  ComplexType :: cc(Ncc)
  call Cput(cc,p1,p2,p1p2,m1,m2,m3)
  C0i = cc(i+epsi)
end function C0i

! ----------------------------------------------------------------------

ComplexType function D0i(i,p1,p2,p3,p4,p1p2,p2p3,m1,m2,m3,m4)
  implicit none
#include "lt_collier.h"
#include "lt_ff.h"
  integer, intent(in) :: i
  ComplexType, intent(in) :: p1,p2,p3,p4,p1p2,p2p3,m1,m2,m3,m4  
  ComplexType :: dd(Ndd)
  call Dput(dd,p1,p2,p3,p4,p1p2,p2p3,m1,m2,m3,m4)
  D0i = dd(i+epsi)
end function D0i

! ----------------------------------------------------------------------

ComplexType function E0i(i,p1,p2,p3,p4,p5,p1p2,p2p3,p3p4,p4p5,p5p1,m1, &
                         m2,m3,m4,m5)
  implicit none
#include "lt_collier.h"
#include "lt_ff.h"
  integer, intent(in) :: i
  ComplexType, intent(in) :: p1,p2,p3,p4,p5,p1p2,p2p3,p3p4
  ComplexType, intent(in) :: p4p5,p5p1,m1,m2,m3,m4,m5
  ComplexType :: ee(Nee)
  call Eput(ee,p1,p2,p3,p4,p5,p1p2,p2p3,p3p4,p4p5,p5p1,m1,m2,m3,m4,m5)
  E0i = ee(i+epsi)
end function E0i

! ----------------------------------------------------------------------
! routines from LoopTools used for the interface with Collier
! ----------------------------------------------------------------------

subroutine setlambda(lam_)
  implicit none
#include "lt_ff.h"
  RealType, intent(in) :: lam_
  RealType :: lambda_
  if( lam_ .eq. 0d0 .or. lam_ .eq. -1d0 .or. lam_ .eq. -2d0 ) then
     lambda_ = dim(lam_,0D0)
     epsi = int(dim(0D0,lam_))
  elseif(lam_.gt.0d0) then
     print *, "mass regularization not yet implemented in the interface"
     stop
  else
     print *, "illegal value for lambda"
     lambda_ = 0
     epsi = 0
     stop
  endif
  ! if( abs(lambda - lambda_) .gt. diffeps ) call clearcache ! not useful
  lambda = lambda_
end subroutine setlambda

! ----------------------------------------------------------------------

RealType function getlambda()
  implicit none
#include "lt_ff.h"
  getlambda = lambda
end function getlambda

! ----------------------------------------------------------------------

integer function getepsi()
  implicit none
#include "lt_ff.h"
  getepsi = epsi
end function getepsi

! ----------------------------------------------------------------------

subroutine clearcache
  use COLLIER
  implicit none
#include "lt_ff.h"
  !call InitEvent_cll(NCACHE) ! leads to memory issues!
  ! reinitialize the cache system instead
  if(NcacheSave.ne.0 .and. NmaxSave.ne.0) then
    call InitCacheSystem_cll(NCacheSave,NmaxSave)
  endif
end subroutine clearcache

! ----------------------------------------------------------------------

subroutine markcache
  use COLLIER
  implicit none
  call SwitchOffCacheSystem_cll ! check (this generates huge output)
end subroutine markcache

! ----------------------------------------------------------------------

subroutine restorecache
  use COLLIER
  implicit none
  call SwitchOnCacheSystem_cll ! check (this generates huge output)
end subroutine restorecache

! ----------------------------------------------------------------------

! initialisation of Collier library
subroutine init_collier(Nmax,Rmax)
  use COLLIER
  implicit none
#include "lt_ff.h"
  ! ncache = number of cache systems to be used (here ncache = 1)
  ! Nmax   = maximal N up to which N-point integrals are cached
  ! Rmax   = maximal rank of loop integrals (usually = Nmax)
  integer, intent(in) :: Nmax,Rmax
  integer, parameter :: ncache = 1
  ! save Nmax, Rmax and Ncache for later 
  ! (for if we want to clear the cache)
  NCacheSave = ncache
  NmaxSave = Nmax
  RmaxSave = Rmax
  call Init_cll(Nmax,Rmax,"collier")
  call InitCacheSystem_cll(ncache,Nmax)
  ! 1: use COLI branch, 2: DD branch
  call SetMode_cll(1)
  !
  call InitEvent_cll(ncache)
end subroutine init_collier

! ----------------------------------------------------------------------

subroutine setuvdiv(delta_uv)
  use COLLIER
  implicit none
  double precision, intent(in) :: delta_uv
  call SetDeltaUV_cll(delta_uv)
end subroutine setuvdiv

! ----------------------------------------------------------------------

subroutine setdelta(delta_uv)
  use COLLIER
  implicit none
  double precision, intent(in) :: delta_uv
  call SetDeltaUV_cll(delta_uv)
end subroutine setdelta

! ----------------------------------------------------------------------

double precision function getdelta()
  use COLLIER
  implicit none
  double precision DeltaUV
  call GetdeltaUV_cll(DeltaUV)
  getdelta = DeltaUV
end function getdelta

! ----------------------------------------------------------------------

subroutine setmudim(mu2)
  use COLLIER
  implicit none
  double precision, intent(in) :: mu2
  call SetMuUV2_cll(mu2)
  call SetMuIR2_cll(mu2)
end subroutine setmudim

! ----------------------------------------------------------------------

! initialisation of global parameters for loop functions
subroutine setparam_collier(delta_uv,mu2,lambda)
  use COLLIER
  implicit none
  double precision, intent(in) :: delta_uv,mu2,lambda
  call SetDeltaUV_cll(delta_uv)  ! set UV divergence control parameter
  call SetMuUV2_cll(mu2) ! renormalization scale squared
  call SetMuIR2_cll(mu2) ! renormalization scale squared
  call setlambda(lambda) ! set IR divergence control parameter in DimReg
  ! lambda=0: finite part; lambda=-1.d0: 1/eps part; lambda=-2.d0: 1/eps^2 part
end subroutine setparam_collier
