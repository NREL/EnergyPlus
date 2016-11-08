echo off
:  EnergyPlus batch file for running files that contain 
:  one or more of the following objects: 
:    Parametric:SetValueForRun
:    Parametric:Logic
:    Parametric:RunControl
:    Parametric:FileNameSuffix
:
:  Created on: 28 Aug 2009 by J. Glazer
:
:  Revisions: none
:
:  This batch file expects two parameters
:        %1 contains the IDF file without extension 
:        %2 contains the EPW weather file without extension 

set input_path=ExampleFiles\

: Test if files are specified
IF [%1] EQU [] GOTO :FileMissing
IF [%2] EQU [] GOTO :FileMissing
GOTO :Proceed

:FileMissing
ECHO -------------------------------------------------------------------------------
ECHO WARNING
ECHO One of more files are missing from the command line
ECHO To use %0 you must enter both the IDF file and the EPW file without extensions
ECHO or file paths. 
ECHO    IDF file name entered: %1
ECHO    EPW file name entered: %2
ECHO    Path for the IDF file: %input_path%
ECHO -------------------------------------------------------------------------------
GOTO :SkipToEnd

:Proceed
SET inidf=%1

: Delete old files created by previous runs of the preprocessor
DEL "%input_path%%inidf%-*.idf"

: Run the preprocessor 
.\PreProcess\ParametricPreprocessor\ParametricPreprocessor.exe "%input_path%%inidf%".idf

: Check if any files were created using the preprocessor 

SET found=F
FOR %%G IN ("%input_path%%inidf%-*.idf") DO (
   ECHO %%G
   SET found=T
   )

IF "%found%" EQU "F" GOTO :NotParamWarning

FOR %%G IN ("%input_path%%inidf%-*.idf") DO (
   ECHO --------------------------------------------------------------------------------------
   ECHO   The file is: %%G
   ECHO       weather: %epwne%
   CALL RunEPlus %%~nG %epwne%
   )
GOTO :SkipToEnd
   
:NotParamWarning
ECHO -------------------------------------------------------------------------------
ECHO WARNING
ECHO The IDF file selected does not generate parametric options.
ECHO -------------------------------------------------------------------------------
GOTO :SkipToEnd   
   
:SkipToEnd
