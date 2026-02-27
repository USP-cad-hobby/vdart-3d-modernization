! vdart_vortex_mod.f90
! Project: VDaRT (Darrieus 3D)
! Purpose: VORTEX - iterative bound vortex circulation solver calling NETHAS.
! Author: U.S.Paulsen
! Date: 2026-02-26
! SPDX-License-Identifier: MIT

module vdart_vortex_mod
  use vdart_kinds_mod
  use vdart_state_mod
  use vdart_aero_mod
  use vdart_bsa_mod
  use vdart_wind_mod
  implicit none

  public :: vortex_iterate

contains

  subroutine vortex_iterate(ares, max_iter, tol, ierr)
    real(dp), intent(in) :: ares
    integer, intent(in), optional :: max_iter
    real(dp), intent(in), optional :: tol
    integer, intent(out) :: ierr

    integer :: it, imax, inb, ins, i, j, l, n, n1, nol1
    real(dp) :: tolv, gmax, gmix, gg, teta, rey
    real(dp), allocatable :: gammet(:,:), swslip(:,:,:)
    real(dp) :: wslip(3)

    ierr = 0
    if (.not. allocated(GAMME)) then
      write(*,*) 'VORTEX ERROR: GAMME not allocated. Call allocate_state first.'
      ierr = 1
      return
    end if

    imax = 9999
    if (present(max_iter)) imax = max_iter
    tolv = EPS1
    if (present(tol)) tolv = tol

    nol1 = NOL + 1
    n = NOL / 2
    n1 = n + 1
    teta = IRUN * DTETA

    allocate( gammet(NB, NOL), swslip(NB, NOL+1, 3) )
    gammet = 0.0_dp
    swslip = 0.0_dp
    it = 0

    ! Compute induced velocities from wake
    do inb = 1, NB
      do ins = 1, n1
        call bsa(3, inb, ins, 1, wslip)
        do l = 1, 3
          swslip(inb, ins, l) = wslip(l)
        end do
      end do
    end do

    ! Mirror
    do inb = 1, NB
      do ins = n1, NOL
        do l = 1, 2
          swslip(inb, ins+1, l) = swslip(inb, nol1 - ins, l)
        end do
        swslip(inb, ins+1, 3) = -swslip(inb, nol1 - ins, 3)
      end do
    end do

    ! Average to element centers
    do inb = 1, NB
      do ins = 1, NOL
        do l = 1, 3
          swslip(inb, ins, l) = 0.5_dp * (swslip(inb, ins+1, l) + swslip(inb, ins, l))
          SWB(inb, ins, l) = swslip(inb, ins, l)
        end do
        GAMME(inb, ins, 1) = GAMME(inb, ins, NPSI)
      end do
    end do

    call wind(teta)

100 continue
    it = it + 1

    do inb = 1, NB
      do ins = 1, n
        gammet(inb, ins) = GAMME(inb, ins, 1)
        rey = UREL(inb, ins) * C / ANY
        call clcdideal(ALFA(inb, ins) * 180.0_dp / pi, CL(inb, ins), CD(inb, ins))
        GAMME(inb, ins, 1) = 0.5_dp * C * CL(inb, ins) * max(UREL(inb, ins), 0.0_dp)
      end do
    end do

    ! Mirror
    do inb = 1, NB
      do ins = n1, NOL
        gammet(inb, ins) = gammet(inb, nol1 - ins)
        GAMME(inb, ins, 1) = GAMME(inb, nol1 - ins, 1)
        CL(inb, ins) = CL(inb, nol1 - ins)
        CD(inb, ins) = CD(inb, nol1 - ins)
      end do
    end do

    gmax = 0.0_dp
    gmix = 0.0_dp
    do inb = 1, NB
      do ins = 1, n
        gmax = max(abs(GAMME(inb, ins, 1)), gmax)
        gmix = max(abs(GAMME(inb, ins, 1) - gammet(inb, ins)), gmix)
      end do
    end do

    if (gmax <= 0.0_dp) then
      ierr = 0
      deallocate(gammet, swslip)
      return
    end if

    gg = abs(gmix / gmax)
    if (gg < tolv) then
      ierr = 0
      deallocate(gammet, swslip)
      return
    end if

    if (it >= imax) then
      write(*,*) 'VORTEX WARNING: reached max iterations ', it
      ierr = 2
      deallocate(gammet, swslip)
      return
    end if

    do i = 1, NB
      do j = 1, NOL
        GAMME(i, j, 1) = ares * GAMME(i, j, 1) + (1.0_dp - ares) * gammet(i, j)
      end do
    end do

    goto 100

  end subroutine vortex_iterate

end module vdart_vortex_mod