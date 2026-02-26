@echo off
REM build.bat
REM Project: VDaRT (Darrieus 3D) - build with layout enforcement using shared tools
REM Author: U.S.Paulsen
REM Date: 2026-02-25

if "%TOOLS_DIR%"=="" (
  REM default fallback for shared tools
  set "TOOLS_DIR=C:\Users\Gaming-Z87\tools"
)

echo Running layout enforcement...
python "%TOOLS_DIR%\enforce_file_per_module.py" --root "%CD%"
if %ERRORLEVEL% NEQ 0 (
  echo Layout enforcement failed. Fix reported issues and try again.
  exit /B 1
)

echo Cleaning previous build artifacts...
del /Q *.mod 2>nul
del /Q *.obj 2>nul
del /Q *.o 2>nul
del /Q *.exe 2>nul

echo Compiling modules (order matters)...
ifx -c vdart_kinds_mod.f90 || goto :err
ifx -c vdart_io_mod.f90    || goto :err
ifx -c vdart_biot_mod.f90  || goto :err
ifx -c vdart_aero_mod.f90  || goto :err
ifx -c vdart_state_mod.f90 || goto :err
ifx -c vdart_bsa_mod.f90   || goto :err
ifx -c vdart_vortex_mod.f90|| goto :err
ifx -c test_vortex.f90     || goto :err

echo Linking...
ifx vdart_kinds_mod.obj vdart_io_mod.obj vdart_biot_mod.obj vdart_aero_mod.obj vdart_state_mod.obj vdart_bsa_mod.obj vdart_vortex_mod.obj test_vortex.obj -o test_vortex.exe || goto :err

echo Build succeeded: test_vortex.exe created.
goto :done

:err
echo.
echo Build failed. See compiler output above.
exit /B 1

:done
exit /B 0