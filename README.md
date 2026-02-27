# VDaRT: Darrieus 3D Rotor Simulation

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Fortran](https://img.shields.io/badge/Fortran-90%2B-734f96)
![Status](https://img.shields.io/badge/Status-Active%20Development-brightgreen)

## Overview

VDaRT (Vertical Axis Darrieus Rotor Three-Dimensional) is a modernized Fortran 90+ codebase for simulating 3D aerodynamic flows around Darrieus-type vertical-axis wind turbines (VAWTs) using free-wake vortex methods.

### Original Source

- **Legacy Code**: `vdart_3d_R5.FOR` (Fortran 77, fixed-form)
- **Original Author**: U.S. Paulsen
- **Modernization**: 2024-2026

### Modernization Goals

1. ✅ **Fortran 90+** - Free-form, allocatable arrays, modules (eliminate COMMON blocks)
2. 🚧 **GPU Acceleration** - OpenACC and OpenMP offload support (planned)
3. ✅ **Modularity** - Clean separation of concerns (14 modules)
4. 🚧 **Performance** - Benchmark against legacy code (in progress)

---

## Current Status (February 2026)

### ✅ Completed Modules (14)

| Module | Purpose | Status |
|--------|---------|--------|
| `vdart_kinds_mod` | Precision definitions (dp, sp), constants (PI) | ✅ Complete |
| `vdart_io_mod` | File I/O utilities | ✅ Complete |
| `vdart_state_mod` | Centralized runtime state (replaces COMMON blocks) | ✅ Complete |
| `vdart_biot_mod` | Biot-Savart law (induced velocity from vortex filament) | ✅ Complete |
| `vdart_aero_mod` | Aerodynamic polars (CL/CD lookup, ideal flat-plate theory) | ✅ Complete |
| `vdart_bsa_mod` | Induced velocity from full vortex mesh/wake | ✅ Complete |
| `vdart_wind_mod` | Relative velocity & angle of attack at blade sections | ✅ Complete |
| `vdart_flyt_mod` | Move vortex positions forward in time (includes RENUMK) | ✅ Complete |
| `vdart_forces_mod` | Compute blade loads (FR, FT, FB) from aerodynamics | ✅ Complete |
| `vdart_blad_mod` | Generate blade geometry (BLSNIT, DSPAN, BETA) | ✅ Complete |
| `vdart_start_mod` | Initialize vortex mesh backward in time from t=0 | ✅ Complete |
| `vdart_nethas_mod` | Compute induced velocities at all mesh points | ✅ Complete |
| `vdart_vortex_mod` | Iterative bound circulation solver | ✅ Complete |
| `vdart_solver_mod` | Main time-stepping orchestration loop | ✅ Complete |

### 🚧 In Progress

- Main program to call solver with realistic test cases
- Output formatting (VTK, CSV, HDF5)
- Validation against legacy results

### 📋 To Do

- Convert remaining utilities (BSPD3, CLCD table reader, PLOT)
- Add OpenACC directives for GPU acceleration
- Performance benchmarking
- Full documentation of physics and numerics

---

## Quick Start

### Prerequisites

#### Windows + Intel Fortran (Recommended)

- ✅ Intel Fortran Compiler (ifx) - Part of [Intel oneAPI HPC Toolkit](https://www.intel.com/content/www/en/en/developer/tools/oneapi/hpc-toolkit.html)
- ✅ Visual Studio 2022+ (optional but recommended)
- ✅ Python 3.8+ (for build tools)
- ✅ Git

#### Alternative: Linux + gfortran

- gfortran 10+ (with OpenMP support)
- Python 3.8+
- Git

### Build

#### Option 1: Windows with Intel Fortran (ifx)
