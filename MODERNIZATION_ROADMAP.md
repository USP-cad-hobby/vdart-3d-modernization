# VDaRT Modernization Roadmap

## Overview
Systematic conversion of legacy Fortran 77 code (vdart_3d_R5.FOR) to modern Fortran 90+ with GPU acceleration (OpenACC/OpenMP offload).

---

## Completed Phases ✅

### Phase 0: Foundation (COMPLETE)
- [x] `vdart_kinds_mod.f90` - Precision & constants
- [x] `vdart_state_mod.f90` - Dynamic state container (replaces COMMON blocks)
- [x] `vdart_aero_mod.f90` - Aerodynamics helpers (interpolation, ideal polar)
- [x] `vdart_biot_mod.f90` - Biot-Savart kernel (GPU-ready)
- [x] `vdart_bsa_mod.f90` - Induced velocity summation
- [x] `vdart_io_mod.f90` - Safe I/O helpers
- [x] `test_vortex.f90` - Basic test suite

**Status:** ✅ COMPLETE & TESTED

---

## In-Progress Phases 🚧

### Phase 1: Wake Mesh & Velocity (PLANNED)
**Objective:** Port velocity computation routines with GPU offload support

- **NETHAS Modernization** → `vdart_nethas_mod.f90`
  - [ ] Dynamic arrays for wake mesh
  - [ ] Mirror symmetry (Phase 1/2 decomposition)
  - [ ] OpenACC `!$acc parallel loop` annotations
  - [ ] Integrate with main driver
  - [ ] Validate vs. legacy output
  - [ ] Add performance profiling

**Target Date:** March 15, 2026  
**Related Issue:** #1 (Modernize NETHAS)

---

## Planned Phases 📋

### Phase 2: Aerodynamic Forces (PLANNED)
**Target Legacy Code:** WIND, FORCES subroutines

#### Routine: WIND
- Convert to `vdart_wind_mod.f90`
- Compute relative velocity at blade sections
- OpenACC offload for triple loop (I, J, K)
- Support blade pitch control (`FI0DOT`)

#### Routine: FORCES
- Convert to `vdart_forces_mod.f90`
- Compute normal/tangential loads from CL/CD
- Distribute loads in radial/tangential/axial directions

**Target Date:** April 1, 2026  
**Status:** Waiting for Phase 1 completion  
**Related Issues:** #2 (WIND), #3 (FORCES)

---

### Phase 3: Vortex Iteration Solver (PLANNED)
**Target Legacy Code:** VORTEX subroutine

- Enhanced `vdart_vortex_mod.f90`
  - Iterative bound circulation solver
  - GPU-accelerated summation over wake
  - Convergence tolerancing & diagnostics
  - Support non-uniform blade meshes

**Target Date:** April 15, 2026  
**Status:** Depends on Phase 1/2  
**Related Issue:** #4

---

### Phase 4: Main Driver & Integration (PLANNED)
**Objective:** Orchestrate simulation pipeline

- `vdart_main.f90` - Top-level control
- `vdart_simulation.f90` - Time-stepping loop
- Full pipeline: NETHAS → WIND → FORCES → VORTEX
- File I/O and output formatting
- Validation/regression tests vs. legacy code

**Target Date:** May 1, 2026  
**Status:** Deferred until Phases 1-3 stable  
**Related Issue:** #5

---

### Phase 5: GPU Performance Tuning (PLANNED)
**Objective:** Optimize kernel performance

- Profile BIOT-Savart kernel
- Tune OpenACC loop schedules (collapse, chunk sizes)
- Memory bandwidth analysis
- Compare nvfortran vs. gfortran+omp offload
- Benchmark speedup vs. legacy (2-3D scaling)

**Target Date:** May 15, 2026  
**Status:** Post-integration  
**Related Issue:** #6

---

### Phase 6: Documentation & Release (PLANNED)
**Objective:** Final deliverable

- Comprehensive README + migration guide
- Doxygen/doc comments on all routines
- GPU compilation guide (gfortran, nvfortran, ifort)
- Example cases + CI/CD tests
- CHANGELOG and release notes
- Tag v1.0 release

**Target Date:** June 1, 2026  
**Status:** Final phase  
**Related Issue:** #7

---

## Key Metrics & Milestones

| Phase | Target Date | Files | Tests | GPU Ready? | Status |
|-------|-------------|-------|-------|-----------|--------|
| Phase 0 | ✅ 2026-02-24 | 7 | 1 | Yes | ✅ DONE |
| Phase 1 | 2026-03-15 | +1 | +2 | Yes | 📋 PLANNED |
| Phase 2 | 2026-04-01 | +2 | +3 | Yes | 📋 PLANNED |
| Phase 3 | 2026-04-15 | +1 | +2 | Yes | 📋 PLANNED |
| Phase 4 | 2026-05-01 | +2 | +5 | Yes | 📋 PLANNED |
| Phase 5 | 2026-05-15 | 0 | +3 | Yes | 📋 PLANNED |
| Phase 6 | 2026-06-01 | +3 | +1 | Yes | 📋 PLANNED |

---

## GitHub Tracking

### Branches
- `main` - Stable, integrated, tested code
- `feature/modernize-nethas` - Phase 1 (NETHAS)
- `feature/modernize-wind-forces` - Phase 2 (WIND, FORCES)
- `feature/modernize-vortex` - Phase 3 (VORTEX iteration)
- `feature/main-driver` - Phase 4 (Main + integration)

### Issues
Each phase has a dedicated issue with checklist items linking to PRs.

### Pull Requests
Every modernized module gets a dedicated PR with:
- Description linking to legacy source
- Tests validating correctness
- Performance metrics (if GPU-related)
- Links to related issues

---

## Development Checklist

For each new module `vdart_XXX_mod.f90`:

- [ ] Module created with clear docs & headers
- [ ] All dependencies imported (vdart_kinds_mod, vdart_state_mod, etc.)
- [ ] Public/private interface clearly declared
- [ ] GPU directives (`!$acc` or `!$omp target`) added for hot loops
- [ ] Error handling (ierr codes) for all edge cases
- [ ] Unit test written in `test_vdart_XXX.f90`
- [ ] Validated vs. legacy code output (< 1e-10 rel. error)
- [ ] Committed and pushed to feature branch
- [ ] PR created with description and results
- [ ] Code review / self-review completed
- [ ] Merged to main

---

## Notes & References

- **Legacy source:** `vdart_3d_R5.FOR` (original Fortran 77 code, ~2500 lines)
- **Modern style guide:** Fortran 2008+ free-form, allocatable arrays, modules, no COMMON blocks
- **GPU target:** NVIDIA GPUs (via nvfortran, gfortran+OpenACC, or ifx+OpenMP offload)
- **Testing standard:** Validate numerical equivalence with legacy within 1e-10 relative error
- **Compiler support:** ifx (Windows/VS 2026), gfortran (Linux/macOS), nvfortran (NVIDIA specialist)

---

## Contact & Questions

- **Project:** VDaRT Modernization (Darrieus 3D Rotor)
- **Owner:** USP-cad-hobby
- **Repository:** [USP-cad-hobby/vdart-3d-modernization](https://github.com/USP-cad-hobby/vdart-3d-modernization)
- **Status:** Active development

---

*Last Updated: 2026-02-26*