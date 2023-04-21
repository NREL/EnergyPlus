@echo off
: This batch file is used to run EnergyPlus simulations using the RunEPlus.bat for all 
: the files in the current directory across multiple separate processor cores.  It has  
: two parameters, the weather file name to use for simulations and the number of 
: processors.
:
:   RunDirMulti <weather file> (opt) <number processor cores> (opt)
: 
: The RunDirMulti batch file loops through the files located in the current directory 
: and puts RunEPlus calls to each file into as many temporary batch files as processor  
: cores and then starts each of the batch files. No load balancing between the cores 
: is achieved using this method. The RunDirMulti.bat file should be located in a 
: directory that contains the IDF files.
:
SET maindir=
:
: Main routine
: The default weather file name if not provided as an argument.
SET weather=USA_IL_Chicago-OHare.Intl.AP.725300_TMY3
: The default number of separate processor cores that the simulations should use if 
: not provided as an argument.
SET numProc=4
IF "%1" NEQ "" SET weather=%1
IF "%2" NEQ "" SET numProc=%2
IF NOT DEFINED maindir (
  ECHO:
  ECHO ===========================================================================
  ECHO:
  ECHO RunDirMult.bat does not have the maindir variable set. Please edit the line
  ECHO in the RunDirMulti.bat file that says:
  ECHO:
  ECHO   SET maindir=
  ECHO:
  ECHO Add the full path for the directory that EnergyPlus is installed after the  
  ECHO equals sign and include double quotes around the path. Make sure the end  
  ECHO of the path ends with a trailing backslash. For example:
  ECHO:
  ECHO   SET maindir="c:\EnergyPlusVx-x-x\"
  ECHO:
  ECHO where the x's are replaced with the actual version number if EnergyPlus was 
  ECHO installed in the default installation directory.
  ECHO:
  ECHO ===========================================================================
  ECHO:
  GOTO:eof
 )
SET count=0
: Loop through the temporary directories and delete the temporary batch files.
for /L %%G in (1,1,%numProc%) do call :clean1 %%G
: Loop through each filename and divides them into each temporary batch file.
for %%F in (.\*.idf) do call :divide1  "%%F" 
: Loop through each temporary directory and adds EXIT to each temporary batch file.
for /L %%G in (1,1,%numProc%) do echo EXIT >> .\tempsim%%G\simpart%%G.bat
: Loop through each temporary directory and starts the batch file in a new window 
for /L %%G in (1,1,%numProc%) do call :startEach1 %%G
: The following line goes to the end of the batch file.
GOTO:eof


: Subroutine that deletes the temporary batch files from each
: working directory.
:clean1
IF EXIST .\tempsim%1\simpart%1.bat (
  DEL .\tempsim%1\simpart%1.bat
) ELSE (
  MD .\tempsim%1
)
: The following line returns to the main routine.
GOTO:eof


: Subroutine that takes the file name and uses a counter
: and the MOD operator (double percent) to group them
: into as many batch files as necessary.
:divide1
SET /a count="count + 1"
SET /a group="count %% numProc + 1"
echo CALL %maindir%RunEPlus.bat "%~dpn1"     %weather% >>.\tempsim%group%\simpart%group%.bat 
: The following line returns to the main routine.
GOTO:eof


: Subroutine that starts each batch file
:startEach1
cd .\tempsim%1
START "Batch Simulation%1" simpart%1.bat
cd ..
: The following line returns to the main routine.
GOTO:eof

