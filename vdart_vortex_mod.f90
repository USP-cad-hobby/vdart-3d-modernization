! vdart_vortex_mod.f90
! Project: VDaRT (Darrieus 3D)
! Purpose: Iterative vortex solver (conservative, modular conversion).
! Author: U.S.Paulsen
! Date: 2026-02-24
! SPDX-License-Identifier: MIT

module vdart_vortex_mod
  use vdart_kinds_mod
  use vdart_state_mod
  use vdart_aero_mod
  implicit none

  public :: vortex_iterate

contains

  subroutine vortex_iterate(ARES, max_iter, tol, ierr)
    real(dp), intent(in) :: ARES
    integer, intent(in), optional :: max_iter
    real(dp), intent(in), optional :: tol
    integer, intent(out) :: ierr

    integer :: it, imax
    real(dp) :: tolv
    real(dp), allocatable :: GAMMET(:,:)
    integer :: i, j
    real(dp) :: GMAX, GMIX, GG

    ierr = 0
    if (.not. allocated(GAMME)) then
      write(*,*) 'ERROR: state not allocated (GAMME)'
      ierr = 1
      return
    end if

    imax = 100
    if (present(max_iter)) imax = max_iter
    tolv = 1.0e-6_dp
    if (present(tol)) tolv = tol

    allocate( GAMMET(NB, NOL) )
    GAMMET = 0.0_dp

    it = 0
1   continue
    it = it + 1

    do i = 1, NB
      do j = 1, NOL
        GAMMET(i,j) = GAMME(i,j,1)
      end do
    end do

    do i = 1, NB
      do j = 1, NOL
        if (CL(i,j) == 0.0_dp) then
          call clcdideal(ALFA(i,j)*180.0_dp/pi, CL(i,j), CD(i,j))
        end if
        GAMME(i,j,1) = 0.5_dp * C * CL(i,j) * max(UREL(i,j), 0.0_dp)
      end do
    end do

    GMAX = 0.0_dp
    GMIX = 0.0_dp
    do i = 1, NB
      do j = 1, NOL
        GMAX = max(GMAX, abs(GAMME(i,j,1)))
        GMIX = max(GMIX, abs(GAMME(i,j,1) - GAMMET(i,j)))
      end do
    end do

    if (GMAX <= 0.0_dp) then
      GG = 0.0_dp
    else
      GG = GMIX / GMAX
    end if

    if (GG < tolv) then
      ierr = 0
      deallocate(GAMMET)
      return
    end if

    do i = 1, NB
      do j = 1, NOL
        GAMME(i,j,1) = ARES * GAMME(i,j,1) + (1.0_dp - ARES) * GAMMET(i,j)
      end do
    end do

    if (it >= imax) then
      write(*,*) 'WARNING: vortex_iterate reached max iterations ', it
      ierr = 2
      deallocate(GAMMET)
      return
    end if
    if (it < imax) then
      goto 1
    end if

  end subroutine vortex_iterate

end module vdart_vortex_mod