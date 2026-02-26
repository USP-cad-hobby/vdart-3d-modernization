# VDaRT: Darrieus 3D Rotor Simulation

## Overview

**VDaRT** is a modernized Fortran 90+ codebase for simulating 3D aerodynamic flows around **Darrieus-type vertical-axis wind turbines (VAWTs)** using vortex methods with GPU acceleration.

### Original Source
- **Legacy Code:** `vdart_3d_R5.FOR` (Fortran 77, fixed-form)
- **Author/Contributor:** U.S. Paulsen

### Modernization Goals
1. **Fortran 90+** - Free-form, allocatable arrays, modules (eliminate COMMON blocks)
2. **GPU Acceleration** - OpenACC and OpenMP offload support
3. **Modularity** - Clean separation of concerns (aerodynamics, vortex mesh, I/O)
4. **Performance** - Benchmark against legacy code; target 5-10× speedup on GPU

---

## Quick Start

### Prerequisites

#### **For Visual Studio 2026 + ifx (Windows with NVIDIA GPU)**
- ✅ **Intel Fortran Compiler (ifx)** - Part of Intel oneAPI HPC Toolkit
  - Download: [Intel HPC Toolkit](https://www.intel.com/content/www/en/en/developer/tools/oneapi/hpc-toolkit.html)
- ✅ **Visual Studio 2026** (optional but recommended)
- ✅ **NVIDIA CUDA Toolkit** (for NVIDIA GPU support with ifx)
  - Download: [NVIDIA CUDA Toolkit](https://developer.nvidia.com/cuda-toolkit)
- ✅ **NVIDIA GPU** (A100, H100, RTX 4090, etc.)
- ✅ **Windows 10/11** or **Windows Server 2022+**

#### **Alternative: Linux with gfortran**
- **gfortran** with OpenACC support (GCC 10+)
- **NVIDIA CUDA Toolkit** (for GPU support)

#### **Alternative: NVIDIA HPC SDK (nvfortran)**
- **nvfortran** (NVIDIA HPC SDK 22+)
- Native NVIDIA GPU support

### Build & Run

#### **Option 1: Visual Studio 2026 + ifx + NVIDIA GPU** ⭐ RECOMMENDED FOR YOUR SETUP

**Step 1: Set up Intel Environment**
```cmd
REM Open Intel oneAPI Command Prompt for Fortran
REM (Search "Intel oneAPI Command Prompt for Fortran" in Windows Start Menu)
REM All subsequent commands run in this terminal
