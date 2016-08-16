set rvpath=
if EXIST eplusout.inp goto :inp
rem produces all variables in .eso file to .csv
%rvpath%readvarseso.exe
goto :done
:inp
rem reads variable specifications from input file
%rvpath%ReadVarsESO.exe eplusout.inp
:done
set rvpath=

