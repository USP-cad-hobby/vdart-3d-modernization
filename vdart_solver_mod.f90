! vdart_solver_mod.f90
! Project: VDaRT (Darrieus 3D)
! Purpose: Main solver orchestration - time-stepping loop.
! Author: U.S.Paulsen
! Date: 2026-02-27
! SPDX-License-Identifier: MIT

module vdart_solver_mod
  use vdart_kinds_mod
  use vdart_state_mod
  use vdart_start_mod
  use vdart_nethas_mod
  use vdart_flyt_mod
  use vdart_vortex_mod
  use vdart_forces_mod
  implicit none

  public :: solver_run

contains

  subroutine solver_run(nb_in, nol_in, kmnet_in, krun_in, kmaks_in, eps_conv, ares, ierr)
    ! Main time-stepping solver for VDaRT
    
    integer, intent(in) :: nb_in, nol_in, kmnet_in, krun_in, kmaks_in
    real(dp), intent(in) :: eps_conv, ares
    integer, intent(out) :: ierr

    logical :: converged
    real(dp) :: gpert, gpern, gg
    integer :: i, j

    ierr = 0

    ! Initialize state arrays
    write(*,*) 'Allocating state arrays...'
    call allocate_state(nb_in, nol_in, kmnet_in, ierr)
    if (ierr /= 0) then
      write(*,*) 'ERROR: Failed to allocate state. ierr=', ierr
      return
    end if

    ! Set convergence parameters (module variables)
    EPS1 = eps_conv
    KMAKS = kmaks_in
    IRUN = 0

    ! Initialize vortex mesh backward in time
    write(*,*) 'Initializing vortex mesh (START)...'
    call start(.false., ierr)
    if (ierr /= 0) then
      write(*,*) 'ERROR: START failed. ierr=', ierr
      call deallocate_state()
      return
    end if

    write(*,*) 'Starting time-stepping loop...'
    converged = .false.

    ! Main time-stepping loop
    do while (.not. converged .and. IRUN < KMAKS)

      ! Compute induced velocities at all mesh points
      call nethas()

      ! Increment time step
      IRUN = IRUN + 1
      write(*,'(A,I5,A,F8.2,A)') '  Step ', IRUN, '  Azimuth: ', IRUN*DTETA*180.0_dp/pi, ' deg'

      ! Move vortex positions forward in time
      call flyt()

      ! Solve for bound circulation (iterative)
      call vortex_iterate(ares, ierr=ierr)
      if (ierr /= 0) then
        write(*,*) 'WARNING: VORTEX iteration did not fully converge at IRUN=', IRUN
      end if

      ! Compute blade forces
      call forces()

      ! Check global convergence (circulation periodicity)
      if (IRUN > krun_in) then
        gpert = 0.0_dp
        gpern = 0.0_dp
        do i = 1, NB
          do j = 1, NOL
            gpert = max(abs(GAMME(i, j, 1) - GAMME(i, j, NPSI)), gpert)
            gpern = max(abs(GAMME(i, j, 1)), gpern)
          end do
        end do

        if (gpern > 0.0_dp) then
          gg = gpert / gpern
          if (gg < EPS1) then
            write(*,'(A,I5,A,E12.4)') '  Converged at IRUN=', IRUN, '  GG=', gg
            converged = .true.
          end if
        end if
      end if

    end do

    if (.not. converged) then
      write(*,*) 'WARNING: Maximum iterations reached without convergence.'
    end if

    write(*,*) 'Time-stepping complete. IRUN=', IRUN

    ! Output summary
    write(*,*) 'Computing final statistics...'
    call output_summary()

  end subroutine solver_run

  subroutine output_summary()
    ! Output summary statistics

    integer :: i, j, l
    real(dp) :: tsum, tmean, cp, aref

    write(*,*) ''
    write(*,*) '========================================='
    write(*,*) '  VDaRT Simulation Summary'
    write(*,*) '========================================='
    write(*,*) '  Total time steps:      ', IRUN
    write(*,*) '  Number of blades:      ', NB
    write(*,*) '  Number of sections:    ', NOL
    write(*,*) '  Mesh size (KMNET):     ', KMNET
    write(*,*) '========================================='

    ! Compute mean tangential force (simple average over last revolution)
    if (IRUN > 0 .and. IR > 0) then
      tsum = 0.0_dp
      do l = max(1, IRUN - IR + 1), IRUN
        do i = 1, NB
          do j = 1, NOL
            tsum = tsum + FT(i, j, l) * DSPAN(j)
          end do
        end do
      end do
      tmean = tsum / real(min(IRUN, IR), dp)

      ! Power coefficient (simplified — needs rotor reference area)
      aref = H0 * 2.0_dp * maxval(BLSNIT(1, :, 2))  ! rough estimate
      if (UINF > 0.0_dp .and. aref > 0.0_dp) then
        cp = 2.0_dp * tmean * OMEGA / (RO * (UINF**3) * aref)
        write(*,'(A,F10.5)') '  Power coefficient (CP): ', cp
      end if
      write(*,'(A,F12.3,A)') '  Mean torque:            ', tmean, ' Nm'
    end if

    write(*,*) '========================================='

  end subroutine output_summary

end module vdart_solver_mod