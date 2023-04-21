@echo off
: -----------------------------------------------------------------------------------------------
: RunSlab
: 
: Modified on Feb/July 2007 by Glazer to allow EP-Launch to use batch file.
:
: This file may be used four ways. 
:
:   Using the Single or Group tab on EP-Launch will run Slab prior to a normal simulation.
:   This is indicated by the STANDARDIN. 
:
:   Using the Utility tab on EP-Launch will set two environmental variables copy this 
:   file to a temporary batch file and run the temporary batch file. This mode has no arguments.

:   Normally EP-Launch will set two environmental variables copy this file to a temporary
:   batch file and run the temporary batch file. This mode has no arguments.
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
 echo Program     : Slab.exe
 echo Input Path  : %input_path%
 echo Output Path : %output_path%
 echo Weather Path: %weather_path%
 goto :done

:UseArg
 echo ===== %0 (Run Slab Temperature Generation) ===== Start =====
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
 echo Running %program_path%Slab.exe
 echo Input File  : %infile%.idf
 echo Output File : %outfile%_slab.gtp %outfile%_slab.out %outfile%_slab.ger (if errors)
 echo Weather File: %wthrfile%.epw

 IF EXIST in.epw DEL in.epw
 if not EXIST "%wthrfile%.epw" echo No Weather File:%wthrfile%.epw
 if not EXIST "%wthrfile%.epw" goto :done
 copy "%wthrfile%.epw" in.epw

 IF EXIST GHTIn.idf DEL GHTIn.idf
 IF NOT EXIST "%infile%.idf" echo No Input File: %infile%.idf
 IF NOT EXIST "%infile%.idf" goto :done
 copy "%infile%.idf" GHTIn.idf
 
:SkipInEPW
 IF EXIST SLABDBOUT.TXT DEL SLABDBOUT.TXT
 IF EXIST eplusout.end DEL eplusout.end
 IF EXIST SLABINP.TXT DEL SLABINP.TXT
 IF EXIST "SLABSplit Surface Temps.TXT" DEL "SLABSplit Surface Temps.TXT"
 IF EXIST audit.out DEL audit.out
 IF EXIST "%outfile%.gtp" ERASE "%outfile%.gtp"
 IF EXIST "%outfile%.ger" ERASE "%outfile%.ger"
 IF EXIST "%outfile%_slab.gtp" ERASE "%outfile%_slab.gtp"
 IF EXIST "%outfile%_slab.ger" ERASE "%outfile%_slab.ger"
 
 ECHO Begin Slab Temperature Calculation processing . . . 

 %program_path%Slab.exe

 IF EXIST eplusout.err copy eplusout.err+audit.out "%outfile%_slab.ger"
 IF EXIST SLABINP.TXT MOVE SLABINP.TXT "%outfile%_slab.out"

 ECHO Removing extra files . . .
 IF EXIST SLABDBOUT.TXT DEL SLABDBOUT.TXT
 IF EXIST eplusout.end DEL eplusout.end
 IF EXIST "SLABSplit Surface Temps.TXT" DEL "SLABSplit Surface Temps.TXT"
 IF EXIST audit.out DEL audit.out
 IF EXIST eplusout.err DEL eplusout.err

 IF "%3" == "STANDARDIN" goto :done
 IF EXIST "SLABSurfaceTemps.txt" COPY "SLABSurfaceTemps.txt" "%outfile%_slab.gtp"
 IF EXIST "SLABSurfaceTemps.txt" DEL "SLABSurfaceTemps.txt"
 IF EXIST GHTIn.idf DEL GHTIn.idf
 IF EXIST in.epw DEL in.epw
 
 :done
 echo ===== %0 %1 ===== Complete =====
