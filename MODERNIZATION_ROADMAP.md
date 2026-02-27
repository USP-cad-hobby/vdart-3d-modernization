# VDaRT Modernization Roadmap

**Project**: Modernization of VDaRT 3D Darrieus rotor CFD code  
**Status**: Active Development  
**Last Updated**: February 27, 2026

---

## Phase 1: Core Refactoring ✅ COMPLETE

**Goal**: Convert legacy Fortran 77 to modern Fortran 90+ with modules and allocatable arrays.

### ✅ Completed Tasks

| Task | Module | Status | Date |
|------|--------|--------|------|
| Precision & constants | `vdart_kinds_mod` | ✅ Complete | Feb 2026 |
| File I/O utilities | `vdart_io_mod` | ✅ Complete | Feb 2026 |
| Biot-Savart law | `vdart_biot_mod` | ✅ Complete | Feb 2026 |
| Aerodynamic polars | `vdart_aero_mod` | ✅ Complete | Feb 2026 |
| State management | `vdart_state_mod` | ✅ Complete | Feb 2026 |
| Induced velocities (mesh) | `vdart_bsa_mod` | ✅ Complete | Feb 2026 |
| Relative velocity/AOA | `vdart_wind_mod` | ✅ Complete | Feb 27, 2026 |
| Move vortex positions | `vdart_flyt_mod` | ✅ Complete | Feb 27, 2026 |
| Compute blade forces | `vdart_forces_mod` | ✅ Complete | Feb 27, 2026 |
| Blade geometry generator | `vdart_blad_mod` | ✅ Complete | Feb 27, 2026 |
| Mesh initialization | `vdart_start_mod` | ✅ Complete | Feb 27, 2026 |
| Wake velocities | `vdart_nethas_mod` | ✅ Complete | Feb 27, 2026 |
| Circulation solver | `vdart_vortex_mod` | ✅ Complete | Feb 27, 2026 |
| Main solver loop | `vdart_solver_mod` | ✅ Complete | Feb 27, 2026 |
| Build infrastructure | `build.bat`, layout checker | ✅ Complete | Feb 2026 |

**Total Modules Converted**: 14 / 14 core modules

---

## Phase 2: Integration & Validation 🚧 IN PROGRESS

**Goal**: Wire modules into working end-to-end simulation and validate against legacy results.

### 🚧 Current Tasks

- [ ] Create main program (`main.f90`) that calls solver with realistic test case
- [ ] Set up test case: 3-blade Darrieus rotor (H=5m, D=3m)
- [ ] Run simulation and compare with legacy VDaRT output
- [ ] Validate forces, torque, power coefficient

### 📋 Planned Tasks

- [ ] Add structured output (CSV, VTK, HDF5)
- [ ] Implement post-processing utilities
- [ ] Create regression test suite
- [ ] Document physics and numerical methods

---

## Phase 3: Performance Optimization 📋 PLANNED

**Goal**: Profile code, optimize hot paths, prepare for GPU acceleration.

### Planned Tasks

- [ ] Profile with Intel VTune or gprof
- [ ] Optimize Biot-Savart loop (most expensive)
- [ ] Add OpenMP threading for multi-core CPU
- [ ] Benchmark: target 5-10× speedup vs. legacy

---

## Phase 4: GPU Acceleration 📋 PLANNED

**Goal**: Port computational kernels to GPU using OpenACC.

### Planned Tasks

- [ ] Add OpenACC directives to BSA, BIOT loops
- [ ] Test on NVIDIA GPU (A100, RTX 4090, etc.)
- [ ] Optimize data movement (minimize host-device transfers)
- [ ] Benchmark GPU vs. CPU performance

---

## Phase 5: Advanced Features 📋 FUTURE

**Goal**: Add modern capabilities beyond legacy code.

### Planned Tasks

- [ ] Dynamic mesh refinement
- [ ] Parallel I/O (MPI)
- [ ] Python bindings (f2py or pybind11)
- [ ] Web-based visualization (ParaView, VTK.js)
- [ ] Coupled FSI (fluid-structure interaction)

---

## Milestones

| Milestone | Target | Status |
|-----------|--------|--------|
| Core modules complete | Feb 2026 | ✅ DONE |
| Working demo program | Mar 2026 | 🚧 In Progress |
| Validation complete | Mar 2026 | 📋 Planned |
| OpenMP threading | Apr 2026 | 📋 Planned |
| GPU acceleration (OpenACC) | May 2026 | 📋 Planned |
| Production release v1.0 | Jun 2026 | 📋 Planned |

---

## Known Issues / Tech Debt

- [ ] `vdart_aero_mod`: Only ideal polar implemented; full CLCD table reader needed
- [ ] `vdart_bsa_mod`: INDI parameter logic could be simplified
- [ ] `vdart_solver_mod`: Output summary is placeholder; needs full torque calculation
- [ ] Error handling: Most modules print warnings but don't propagate ierr consistently
- [ ] No unit tests yet

---

## Dependencies

- **Compiler**: Intel Fortran (ifx) or gfortran 10+
- **Build**: Python 3.8+ (for layout checker)
- **GPU** (future): NVIDIA CUDA Toolkit, OpenACC-capable compiler

---

## References

- Original VDaRT code: `vdart_3d_R5.FOR`
- Fortran 90 standard: ISO/IEC 1539-1:1997
- OpenACC 3.0 specification