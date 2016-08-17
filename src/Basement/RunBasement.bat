@echo off
: -----------------------------------------------------------------------------------------------
: RunBasement
: 
: Modified on Feb/July 2007 by Glazer to allow EP-Launch to use batch file.
:
: This file may be used four ways. 
:
:   Using the Single or Group tab on EP-Launch will run Basement prior to a normal simulation.
:   This is indicated by the STANDARDIN. 
:
:   Using the Utility tab on EP-Launch will set two environmental variables copy this 
:   file to a temporary batch file and run the temporary batch file. This mode has no arguments.
:
:   As an option EP-Launch can use batch file parameters. In this mode no temporary batch file 
:   is created and the input file and weather file are passed as arguments with the third parameter
:   being PARAM as a flag. The first and second parameters are the input and weather file.
:
:   For batch file use without EP-Launch, the program, input and output paths are specified in 
:   set commands (near line 55). The first and second parameters are the input and weather file
:   names. No third parameter is needed.
:
: -----------------------------------------------------------------------------------------------
 IF "%3" == "STANDARDIN" goto :StandardIn
 IF "%3" == "PARAM" goto :EPLwArg
 if "%1" == "" goto :TestEPL
 if "%2" == "" goto :TestEPL
 goto :UseArg
 
:TestEPL
 if "%inout%" == "" goto :Err
 if "%wthr%" == "" goto :Err

 set infile=%inout%
 set outfile=%inout%
 set wthrfile=%wthr%
 goto :DoRun

:EPLwArg
SET infile=%~1
SET outfile=%~1
SET wthrfile=%~2
goto :DoRun

:StandardIn
SET outfile=%~1
set program_path=
goto :SkipInEPW

 :Err
 echo usage: %0 InputFileName (req - no extension) WeatherFileName (req - no extension)
 echo Current Parameters:
 echo Program     : Basement.exe
 echo Input Path  : %input_path%
 echo Output Path : %output_path%
 echo Weather Path: %weather_path%
 goto :done

:UseArg
 echo ===== %0 (Run Basement Temperature Generation) ===== Start =====
:Instructions:
:  Complete the following path and program names.
:  path names must have a following \ or errors will happen
 set program_path=
 set input_path=
 set output_path=
 set weather_path=..\..\WeatherData\

 set infile=%input_path%%1
 set outfile=%output_path%%1
 set wthrfile=%weather_path%%2

 :DoRun
 : Display basic parameters of the run
 echo Running Basement.exe
 echo Input File  : %infile%.idf
 echo Output Files: %outfile%_bsmt.csv %outfile%_bsmt.audit %outfile%_bsmt.out
 echo Weather File: %wthrfile%.epw

 IF EXIST in.epw DEL in.epw
 if not EXIST "%wthrfile%.epw" echo No Weather File:%wthrfile%.epw
 if not EXIST "%wthrfile%.epw" goto :done
 copy "%wthrfile%.epw" in.epw

 IF EXIST BasementGHTIn.idf DEL BasementGHTIn.idf
 IF NOT EXIST "%infile%.idf" echo No Input File: %infile%.idf
 IF NOT EXIST "%infile%.idf" goto :done
 copy "%infile%.idf" BasementGHTIn.idf

:SkipInEPW
 IF EXIST eplusout.end DEL eplusout.end
 IF EXIST GrTemp.TXT DEL GrTemp.TXT
 IF EXIST TempInit.TXT DEL TempInit.TXT
 IF EXIST audit.out DEL audit.out
 IF EXIST rundebugout.txt DEL rundebugout.txt
 IF EXIST basementout.audit DEL basementout.audit

 IF EXIST "%outfile%.csv" ERASE "%outfile%.csv"
 IF EXIST "%outfile%.audit" ERASE "%outfile%.audit"
 IF EXIST "%outfile%.out" ERASE "%outfile%.out"
 IF EXIST "%outfile%_out.idf" ERASE "%outfile%_out.idf"
 IF EXIST "%outfile%_bsmt.csv" ERASE "%outfile%_bsmt.csv"
 IF EXIST "%outfile%_bsmt.audit" ERASE "%outfile%_bsmt.audit"
 IF EXIST "%outfile%_bsmt.out" ERASE "%outfile%_bsmt.out"
 IF EXIST "%outfile%_out_bsmt.idf" ERASE "%outfile%_out_bsmt.idf"

ECHO Begin Basement Temperature Calculation processing . . . 
 %program_path%Basement.exe

 IF EXIST "MonthlyResults.csv" MOVE "MonthlyResults.csv" "%outfile%_bsmt.csv"
 IF EXIST "RunINPUT.TXT" MOVE "RunINPUT.TXT" "%outfile%_bsmt.out"
 IF EXIST "RunDEBUGOUT.txt" MOVE "RunDEBUGOUT.txt" basementout.audit
 IF NOT EXIST basementout.audit echo Basement Audit File > basementout.audit
 IF EXIST "eplusout.err" copy basementout.audit + eplusout.err basementout.audit
 IF EXIST audit.out copy basementout.audit + audit.out basementout.audit
 IF EXIST basementout.audit MOVE basementout.audit "%outfile%_bsmt.audit"

 ECHO Removing extra files . . .

 IF EXIST eplusout.end DEL eplusout.end
 IF EXIST GrTemp.TXT DEL GrTemp.TXT
 IF EXIST TempInit.TXT DEL TempInit.TXT
 IF EXIST audit.out DEL audit.out
 IF EXIST rundebugout.txt DEL rundebugout.txt
 IF EXIST basementout.audit DEL basementout.audit

 IF "%3" == "STANDARDIN" goto :done
 IF EXIST "EPObjects.TXT" COPY "EPObjects.TXT" "%outfile%_out_bsmt.idf"
 IF EXIST EPObjects.TXT DEL EPObjects.TXT  
 IF EXIST BasementGHTIn.idf DEL BasementGHTIn.idf
 IF EXIST in.epw DEL in.epw
 
 :done
 echo ===== %0 %1 ===== Complete =====
