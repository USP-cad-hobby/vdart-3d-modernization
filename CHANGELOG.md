# Changelog

All notable changes to the VDaRT modernization project.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

### Planned
- Main program to run full simulations
- Structured output (CSV, VTK, HDF5)
- OpenMP threading
- GPU acceleration (OpenACC)

---

## [0.2.0] - 2026-02-27

### Added
- `vdart_wind_mod`: Compute relative velocity and angle of attack at blade sections
- `vdart_flyt_mod`: Move vortex positions forward in time (includes RENUMK)
- `vdart_forces_mod`: Compute blade loads (FR, FT, FB) from aerodynamics
- `vdart_blad_mod`: Generate blade geometry for straight/parabolic/troposkien rotors
- `vdart_start_mod`: Initialize vortex mesh backward in time from t=0
- `vdart_nethas_mod`: Compute induced velocities at all mesh points
- `vdart_solver_mod`: Main time-stepping orchestration loop
- Force arrays (FR, FT, FB) to `vdart_state_mod`
- Geometry arrays (DSPAN, BETA, FI0, CRANK, RS, H2) to `vdart_state_mod`

### Changed
- Updated `vdart_vortex_mod`: Integrated wind() call, removed stub
- Updated `build.bat`: Added new modules to compile order
- Updated `.gitignore`: Exclude Intel Fortran .obj files

### Documentation
- Updated README.md with module descriptions and current status
- Updated MODERNIZATION_ROADMAP.md to reflect completed work
- Added CHANGELOG.md

---

## [0.1.0] - 2026-02-26

### Added
- `vdart_kinds_mod`: Precision definitions (dp, sp) and constants (PI)
- `vdart_io_mod`: File I/O utilities
- `vdart_biot_mod`: Biot-Savart law implementation (Scully vortex core model)
- `vdart_aero_mod`: Aerodynamic polars (CL/CD ideal flat-plate theory)
- `vdart_state_mod`: Centralized runtime state (replaces COMMON blocks)
- `vdart_bsa_mod`: Induced velocity from vortex mesh/wake
- `vdart_vortex_mod`: Iterative bound circulation solver (initial version)
- `test_vortex.f90`: Test harness
- `build.bat`: Windows build script with layout enforcement
- `scripts/enforce_file_per_module.py`: Layout checker
- `.gitignore`: Build artifacts exclusion
- LICENSE (MIT)
- README.md (initial)
- MODERNIZATION_ROADMAP.md (initial)

### Changed
- Converted from Fortran 77 fixed-form to Fortran 90 free-form
- Replaced COMMON blocks with module variables
- Replaced fixed-size arrays with allocatable arrays
- Added explicit interfaces with intent declarations

---

## [0.0.1] - 2024-XX-XX

### Added
- Initial repository setup
- Legacy code import: `vdart_3d_R5.FOR`

---

[Unreleased]: https://github.com/USP-cad-hobby/vdart-3d-modernization/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/USP-cad-hobby/vdart-3d-modernization/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/USP-cad-hobby/vdart-3d-modernization/releases/tag/v0.1.0
[0.0.1]: https://github.com/USP-cad-hobby/vdart-3d-modernization/releases/tag/v0.0.1