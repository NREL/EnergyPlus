@echo off

rem Windows batch file version of the format checker script. Basically, the
rem same caveats apply here as apply to the shell script version. This version
rem does assume that the clang-format executable is in the path, which is
rem different than the shell script.

set exts=.cc .hh .cpp .hpp .c .h
set dirs=src\EnergyPlus\ tst\EnergyPlus\unit\

for %%d in (%dirs%) do (
  echo ***Processing files in directory: %%d
  for %%x in (%exts%) do call:check %%d %%x
)

goto:eof

:check
for /r %~1 %%f in (*%~2) do clang-format --dry-run --Werror --style=file %%f
goto:eof

