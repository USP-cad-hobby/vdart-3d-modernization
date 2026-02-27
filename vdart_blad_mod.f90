! vdart_blad_mod.f90
! Project: VDaRT (Darrieus 3D)
! Purpose: BLAD - generate blade geometry (section coordinates, span lengths, angles).
! Author: U.S.Paulsen
! Date: 2026-02-27
! SPDX-License-Identifier: MIT

module vdart_blad_mod
  use vdart_kinds_mod
  use vdart_state_mod
  implicit none

  public :: blad

contains

  subroutine blad(pitchoff)
    ! Generates blade section coordinates (BLSNIT), span lengths (DSPAN),
    ! and blade angles (BETA) for various rotor shapes.
    !
    ! Rotor shapes (determined by B parameter):
    !   SHAPE=1: Straight blade (B > -1000): Y = A*Z + B*H0
    !   SHAPE=2: Parabolic (Darrieus) (-2000 < B < -1000): Y = 4*A*Z*(1 - Z/H0)
    !   SHAPE=3: Modified troposkien (B < -3000): Y,Z read from RS array
    !
    ! Input:
    !   pitchoff - pitch offset angle (radians) applied to all sections

    real(dp), intent(in) :: pitchoff

    integer :: i, j, l, nol1, nol2, shape
    real(dp) :: del, vl2, tanb, offsetpitch
    real(dp) :: v(3)

    if (.not. allocated(BLSNIT) .or. .not. allocated(DSPAN) .or. .not. allocated(BETA)) then
      write(*,*) 'BLAD ERROR: Arrays not allocated. Call allocate_state first.'
      return
    end if

    offsetpitch = pitchoff
    nol1 = NOL + 1
    nol2 = NOL - 1

    ! Determine rotor shape from B parameter
    if (B > -1000.0_dp) then
      shape = 1  ! Straight blade
    else if (B > -2000.0_dp .and. B < -1000.0_dp) then
      shape = 2  ! Parabolic
    else if (B < -3000.0_dp) then
      shape = 3  ! Modified troposkien (from RS table)
    else
      shape = 1  ! Default to straight
    end if

    ! Helper functions for blade shape
    ! GERADE (straight): Y = A*Z + B*H0
    ! PARAB (parabolic): Y = 4*A*Z*(1 - Z/H0)

    ! First blade (master blade at teta=0)
    ! Bottom section (J=1, Z=0)
    BLSNIT(1, 1, 1) = 0.0_dp  ! X = 0
    BLSNIT(1, 1, 3) = 0.0_dp  ! Z = 0
    if (shape == 1) then
      BLSNIT(1, 1, 2) = A * 0.0_dp + B * H0  ! Y from straight line
    else if (shape == 2) then
      BLSNIT(1, 1, 2) = 4.0_dp * A * 0.0_dp * (1.0_dp - 0.0_dp / H0)  ! Y from parabola
    else if (shape == 3) then
      BLSNIT(1, 1, 3) = RS(1, 1)
      BLSNIT(1, 1, 2) = RS(1, 2)
    end if

    ! Second section (J=2, Z=BSAF*H0/3)
    BLSNIT(1, 2, 3) = BSAF * H0 / 3.0_dp
    if (shape == 1) then
      BLSNIT(1, 2, 2) = A * BLSNIT(1, 2, 3) + B * H0
    else if (shape == 2) then
      BLSNIT(1, 2, 2) = 4.0_dp * A * BLSNIT(1, 2, 3) * (1.0_dp - BLSNIT(1, 2, 3) / H0)
    else if (shape == 3) then
      BLSNIT(1, 2, 3) = RS(2, 1)
      BLSNIT(1, 2, 2) = RS(2, 2)
    end if
    BLSNIT(1, 2, 1) = 0.0_dp

    ! Middle sections (J=3 to NOL-1)
    del = H0 * (1.0_dp - 2.0_dp * BSAF) / real(NOL - 4, dp)
    do j = 3, nol2
      BLSNIT(1, j, 3) = BSAF * H0 + del * real(j - 3, dp)
      if (shape == 1) then
        BLSNIT(1, j, 2) = A * BLSNIT(1, j, 3) + B * H0
      else if (shape == 2) then
        BLSNIT(1, j, 2) = 4.0_dp * A * BLSNIT(1, j, 3) * (1.0_dp - BLSNIT(1, j, 3) / H0)
      else if (shape == 3) then
        BLSNIT(1, j, 3) = RS(j, 1)
        BLSNIT(1, j, 2) = RS(j, 2)
      end if
      BLSNIT(1, j, 1) = 0.0_dp
    end do

    ! Second-to-last section (J=NOL)
    BLSNIT(1, NOL, 3) = (1.0_dp - BSAF / 3.0_dp) * H0
    if (shape == 1) then
      BLSNIT(1, NOL, 2) = A * BLSNIT(1, NOL, 3) + B * H0
    else if (shape == 2) then
      BLSNIT(1, NOL, 2) = 4.0_dp * A * BLSNIT(1, NOL, 3) * (1.0_dp - BLSNIT(1, NOL, 3) / H0)
    else if (shape == 3) then
      BLSNIT(1, NOL, 3) = RS(NOL, 1)
      BLSNIT(1, NOL, 2) = RS(NOL, 2)
    end if
    BLSNIT(1, NOL, 1) = 0.0_dp

    ! Top section (J=NOL+1, Z=H0)
    BLSNIT(1, nol1, 3) = H0
    if (shape == 1) then
      BLSNIT(1, nol1, 2) = A * H0 + B * H0
    else if (shape == 2) then
      BLSNIT(1, nol1, 2) = 4.0_dp * A * H0 * (1.0_dp - H0 / H0)  ! = 0 at top
    else if (shape == 3) then
      BLSNIT(1, nol1, 3) = RS(nol1, 1)
      BLSNIT(1, nol1, 2) = RS(nol1, 2)
    end if
    BLSNIT(1, nol1, 1) = 0.0_dp

    ! Compute span lengths (DSPAN) and blade angles (BETA) for each element
    do j = 1, NOL
      vl2 = 0.0_dp
      do l = 1, 3
        v(l) = BLSNIT(1, j+1, l) - BLSNIT(1, j, l)
        vl2 = vl2 + v(l)**2
      end do
      DSPAN(j) = sqrt(vl2)

      ! Blade angle (atan of dY/dZ)
      tanb = (BLSNIT(1, j+1, 2) - BLSNIT(1, j, 2)) / (BLSNIT(1, j+1, 3) - BLSNIT(1, j, 3))
      BETA(j) = atan(tanb)

      ! Set pitch angle (radians)
      FI0(j) = 0.0_dp + offsetpitch
    end do

    ! Copy master blade to other blades (rotated by crank angle)
    if (NB == 1) return

    do i = 2, NB
      do j = 1, nol1
        BLSNIT(i, j, 3) = BLSNIT(1, j, 3)  ! Z unchanged
        BLSNIT(i, j, 2) = BLSNIT(1, j, 2) * cos(CRANK(i))  ! Y rotated
        BLSNIT(i, j, 1) = -BLSNIT(1, j, 1) * sin(CRANK(i))  ! X rotated
      end do
    end do

  end subroutine blad

end module vdart_blad_mod