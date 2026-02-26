del /Q *.mod del /Q *.obj del /Q *.o del /Q *.exe

ifx -c vdart_kinds_mod.f90
ifx -c vdart_io_mod.f90
ifx -c vdart_biot_mod.f90
ifx -c vdart_aero_mod.f90
ifx -c vdart_state_mod.f90
ifx -c vdart_vortex_mod.f90
ifx -c test_vortex.f90

rem verify object files exist
dir *.obj

rem link using .obj files (order not strict when all .obj present)
ifx vdart_kinds_mod.obj vdart_io_mod.obj vdart_biot_mod.obj vdart_aero_mod.obj vdart_state_mod.obj vdart_vortex_mod.obj test_vortex.obj -o test_vortex.exe