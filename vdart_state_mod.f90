! vdart_state_mod.f90
! Project: VDaRT (Darrieus 3D)
! Purpose: Runtime state container replacing COMMON blocks (allocatable arrays).
! Author: U.S.Paulsen
! Date: 2026-02-25
! SPDX-License-Identifier: MIT

module vdart_state_mod
  use vdart_kinds_mod
  implicit none

  public :: allocate_state, deallocate_state
  public :: allocate_mesh, deallocate_mesh
  public :: NB, NOL, KMNET, NPSI, IR, IRUN, KMAKS
  public :: C, DTETA, DT, OMEGA, EPS1, RC, HSTAR, FI0DOT, UINF, BOOLPRINT
  public :: GAMME, SWB, UREL, ALFA, ALFAF, CL, CD
  public :: H1, POINT, HAS, ITALX, ITALY, ITALZ

  integer :: NB = 0, NOL = 0, KMNET = 0, NPSI = 0, IR = 0, IRUN = 0, KMAKS = 0
  integer :: ITALX = 0, ITALY = 0, ITALZ = 0
  real(dp) :: C = 0.0_dp, DTETA = 0.0_dp, DT = 0.0_dp, OMEGA = 0.0_dp
  real(dp) :: EPS1 = 1.0e-6_dp, RC = 0.0_dp, HSTAR = 0.0_dp, FI0DOT = 0.0_dp
  real(dp) :: UINF = 0.0_dp
  logical :: BOOLPRINT = .false.

  ! Allocatable arrays sized at runtime
  real(dp), allocatable :: GAMME(:,:,:)    ! (NB, NOL, KMNET)    - bound/shedded circulation
  real(dp), allocatable :: SWB(:,:,: )     ! (NB, NOL, 3)        - downwash
  real(dp), allocatable :: UREL(:,:)       ! (NB, NOL)
  real(dp), allocatable :: ALFA(:,:)       ! (NB, NOL)
  real(dp), allocatable :: ALFAF(:,:)      ! (NB, NOL)
  real(dp), allocatable :: CL(:,:)         ! (NB, NOL)
  real(dp), allocatable :: CD(:,:)         ! (NB, NOL)

  ! Mesh / position arrays (allocate via allocate_mesh)
  real(dp), allocatable :: H1(:,:,:,:)     ! (NB, NOL, KMNET, 3)  - point positions per blade/snippet/k/coord
  real(dp), allocatable :: POINT(:,:,:,:)  ! (ITALX, ITALY, ITALZ, 3) - sampling grid points
  real(dp), allocatable :: HAS(:,:,:,:)    ! (ITALX, ITALY, ITALZ, 3) - sampled induced velocities

contains

  subroutine allocate_state(nb_in, nol_in, kmnet_in, ierr)
    integer, intent(in) :: nb_in, nol_in, kmnet_in
    integer, intent(out) :: ierr
    integer :: ios

    ierr = 0
    if (nb_in <= 0 .or. nol_in <= 0 .or. kmnet_in <= 0) then
      ierr = 1
      return
    end if

    NB = nb_in
    NOL = nol_in
    KMNET = kmnet_in

    allocate( GAMME(NB, NOL, KMNET), stat=ios )
    if (ios /= 0) then
      ierr = 2
      return
    end if
    GAMME = 0.0_dp

    allocate( SWB(NB, NOL, 3), stat=ios )
    if (ios /= 0) then
      ierr = 3
      return
    end if
    SWB = 0.0_dp

    allocate( UREL(NB, NOL), stat=ios )
    if (ios /= 0) then
      ierr = 4
      return
    end if
    UREL = 0.0_dp

    allocate( ALFA(NB, NOL), stat=ios )
    if (ios /= 0) then
      ierr = 5
      return
    end if
    ALFA = 0.0_dp

    allocate( ALFAF(NB, NOL), stat=ios )
    if (ios /= 0) then
      ierr = 6
      return
    end if
    ALFAF = 0.0_dp

    allocate( CL(NB, NOL), stat=ios )
    if (ios /= 0) then
      ierr = 7
      return
    end if
    CL = 0.0_dp

    allocate( CD(NB, NOL), stat=ios )
    if (ios /= 0) then
      ierr = 8
      return
    end if
    CD = 0.0_dp

  end subroutine allocate_state

  subroutine deallocate_state()
    implicit none
    if (allocated(GAMME)) deallocate(GAMME)
    if (allocated(SWB))   deallocate(SWB)
    if (allocated(UREL))  deallocate(UREL)
    if (allocated(ALFA))  deallocate(ALFA)
    if (allocated(ALFAF)) deallocate(ALFAF)
    if (allocated(CL))    deallocate(CL)
    if (allocated(CD))    deallocate(CD)
    if (allocated(H1))    deallocate(H1)
    if (allocated(POINT)) deallocate(POINT)
    if (allocated(HAS))   deallocate(HAS)
    NB = 0
    NOL = 0
    KMNET = 0
    ITALX = 0
    ITALY = 0
    ITALZ = 0
  end subroutine deallocate_state

  ! Allocate mesh grids used by BSA / BSPD3 / WIND
  subroutine allocate_mesh(ix, iy, iz, ierr)
    integer, intent(in) :: ix, iy, iz
    integer, intent(out) :: ierr
    integer :: ios

    ierr = 0
    if (ix <= 0 .or. iy <= 0 .or. iz <= 0) then
      ierr = 1
      return
    end if

    ITALX = ix
    ITALY = iy
    ITALZ = iz

    allocate( POINT(ITALX, ITALY, ITALZ, 3), stat=ios )
    if (ios /= 0) then
      ierr = 2
      return
    end if
    POINT = 0.0_dp

    allocate( HAS(ITALX, ITALY, ITALZ, 3), stat=ios )
    if (ios /= 0) then
      ierr = 3
      return
    end if
    HAS = 0.0_dp
  end subroutine allocate_mesh

  subroutine deallocate_mesh()
    implicit none
    if (allocated(POINT)) then
      deallocate(POINT)
    end if
    if (allocated(HAS)) then
      deallocate(HAS)
    end if
    ITALX = 0
    ITALY = 0
    ITALZ = 0
  end subroutine deallocate_mesh

end module vdart_state_mod