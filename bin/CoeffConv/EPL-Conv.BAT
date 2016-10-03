:  CoeffConv Batch File for EP-Launch Program 
:  Utilities distributed with EnergyPlus 
:  Created on: 12 Feb 2007 by J. Glazer
:  Based on:   EPL-RUN.bat
:  Revised:    
:
:  This batch file executes CoeffConv with an optional paramter.  If a parameters is used
:  the environmental variable below is ignored. If a parameter is used it is %inout% 
:
:     %inout% contains the file with full path and no extensions for input and output files
:
:  1. Use input parameters
IF "%3" NEQ "PARAM" SET io=%inout%
IF "%3" EQU "PARAM" SET io=%~1

:  2. Clean up working directory
IF EXIST CoeffConvInput.txt    DEL CoeffConvInput.txt
IF EXIST CoeffConvOutput.txt   DEL CoeffConvOutput.txt

:  3. Copy input data file to working directory
IF EXIST "%io%.coi" COPY "%io%.coi" CoeffConvInput.txt

:  4. Execute CoeffConv
CoeffConv.exe

:  5. Remove old version of the output file
IF EXIST "%io%.coo" DEL "%io%.coo"

:  6. Copy output files to input/output path
IF EXIST CoeffConvOutput.txt MOVE CoeffConvOutput.txt "%io%.coo"

:  7.  Clean up directory.
IF EXIST CoeffConvInput.txt    DEL CoeffConvInput.txt
IF EXIST CoeffConvOutput.txt   DEL CoeffConvOutput.txt

:  8. Finished
