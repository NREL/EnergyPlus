@echo off
: -----------------------------------------------------------------------------------------------
: RunCalSoilSurfTemp
: 
: Modified on Feb 14 2007 to allow EP-Launch to use batch file
: The first part of the batch file is the original batch file
: but the second portion starting with EPL is run when the 
: batch file is started with INOUT being defined.

: -----------------------------------------------------------------------------------------------
IF "%3" EQU "PARAM" GOTO :EPLarg
IF "%inout%" NEQ "" GOTO :EPL
IF "%1" NEQ "" GOTO :NoErr

 :Err
 echo usage: %0 WeatherFileName (req - no extension)
 echo Current Parameters:
 echo Program     : CalcSoilSurfTemp.exe
 echo Input Path  : %input_path%
 echo Output Path : %output_path%
 echo Weather Path: %weather_path%
goto :done

:NoErr
 echo ===== %0 (Run Soil Surface Temperature Calculation) ===== Start =====
:Instructions:
:  Complete the following path and program names.
:  path names must have a following \ or errors will happen
 set program_path=
 set program_name=CalcSoilSurfTemp.exe
 set input_path=
 set output_path=
 set weather_path=..\..\WeatherData\
 
 : Display basic parameters of the run
 echo Running %program_path%%program_name%
 echo Weather File: %weather_path%%1.epw
 
 echo Checking Files...

 if not EXIST %weather_path%%1.epw echo No Weather File:%weather_path%%1.epw
 if not EXIST %weather_path%%1.epw goto :done
 
 IF EXIST %output_path%CalcSoilSurfTemp.out ERASE %output_path%CalcSoilSurfTemp.out
 
:exe
 
 ECHO Begin Soil Surface Temperature Calculation processing . . . 

 %program_path%%program_name% %weather_path%%1.epw

 IF EXIST "CalcSoilSurfTemp.out" MOVE "CalcSoilSurfTemp.out" %output_path%CalcSoilSurfTemp.out
 
goto :done

: -----------------------------------------------------------------------------------------------
: EP-Launch section
:EPLarg
SET inout=%~1

:EPL

:
:  1. Clean up working directory
IF EXIST CalcSoilSurfTemp.out    DEL CalcSoilSurfTemp.out

:  2. Execute CalcSoilSurfTemp
CalcSoilSurfTemp.exe "%inout%".epw

:  3. Remove old version of the output file
IF EXIST "%inout%.out" DEL "%inout%.out"

:  4. Copy output files to input/output path
IF EXIST CalcSoilSurfTemp.out MOVE CalcSoilSurfTemp.out "%inout%.out"

:  5.  Clean up directory.
IF EXIST CalcSoilSurfTemp.out    DEL CalcSoilSurfTemp.out
 
 :done
 echo ===== %0 %1 ===== Complete =====
 