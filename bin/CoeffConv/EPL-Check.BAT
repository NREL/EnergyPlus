:  CoeffCheck Batch File for EP-Launch Program 
:  Utilities distributed with EnergyPlus 
:  Created on: 13 Feb 2007 by J. Glazer
:  Based on:   EPL-Conv.bat
:  Revised:    
:
:  This batch file executes CoeffCheck with an optional paramter.  If a parameters is used
:  the environmental variable below is ignored. If a parameter is used it is %inout% 
:
:     %inout% contains the file with full path and no extensions for input and output files
:
:  1. Use input parameters
IF "%3" NEQ "PARAM" SET io=%inout%
IF "%3" EQU "PARAM" SET io=%~1

:  2. Clean up working directory
IF EXIST CoeffCheckInput.txt    DEL CoeffCheckInput.txt
IF EXIST CoeffCheckOutput.txt   DEL CoeffCheckOutput.txt

:  3. Copy input data file to working directory
IF EXIST "%io%.cci" COPY "%io%.cci" CoeffCheckInput.txt

:  4. Execute CoeffConv
CoeffCheck.exe

:  5. Remove old version of the output file
IF EXIST "%io%.cco" DEL "%io%.cco"

:  6. Copy output files to input/output path
IF EXIST CoeffCheckOutput.txt MOVE CoeffCheckOutput.txt "%io%.cco"

:  7.  Clean up directory.
IF EXIST CoeffCheckInput.txt    DEL CoeffCheckInput.txt
IF EXIST CoeffCheckOutput.txt   DEL CoeffCheckOutput.txt

:  8. Finished
