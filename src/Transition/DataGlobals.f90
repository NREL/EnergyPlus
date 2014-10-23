MODULE DataGlobals      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   January 1997
          !       MODIFIED       May 1997 (RKS) Added Weather Variables
          !       MODIFIED       December 1997 (RKS,DF,LKL) Split into DataGlobals and DataEnvironment
          !       MODIFIED       February 1999 (FW) Added NextHour, WGTNEXT, WGTNOW
          !       MODIFIED       September 1999 (LKL) Rename WGTNEXT,WGTNOW for clarity
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for all variables which are considered
          ! to be "global" in nature in EnergyPlus.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! None!--This module is USEd by all other modules; it should not USE anything.

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
DOUBLE PRECISION, PARAMETER    :: Pi = 3.141592653589793D0   ! Pi 3.1415926535897932384626435
DOUBLE PRECISION, PARAMETER    :: PiOvr2 = Pi/2.D0          ! Pi/2
DOUBLE PRECISION, PARAMETER    :: DegToRadians = Pi/180.D0  ! Conversion for Degrees to Radians
DOUBLE PRECISION, PARAMETER    :: SecInHour = 3600.0D0      ! Conversion for hours to seconds
INTEGER, PARAMETER :: MaxNameLength = 100     ! Maximum Name Length in Characters -- should be the same
                                              ! as MaxAlphaArgLength in InputProcessor module

REAL, PARAMETER    :: KelvinConv = 273.15     ! Conversion factor for C to K and K to C
REAL, PARAMETER    :: InitConvTemp = 5.05     ! [deg C], standard init vol to mass flow conversion temp
REAL, PARAMETER    :: AutoCalculate = -99999. ! automatically calculate some fields.

DOUBLE PRECISION, PARAMETER    :: StefanBoltzmann = 5.6697D-8   ! Stefan-Boltzmann constant in W/(m2*K4)

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE
    SUBROUTINE ShowMessage(Message,Unit1,Unit2)
    !  Use when you want to create your own message for the error file.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowContinueError(Message,Unit1,Unit2)
    !  Use when you are "continuing" an error message over several lines.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowContinueErrorTimeStamp(Message,Unit1,Unit2)
    !  Use when you are "continuing" an error message and want to show the environment, day and time.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowFatalError(Message,Unit1,Unit2)
    !  Use when you want the program to terminate after writing messages
    !  to appropriate files
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowSevereError(Message,Unit1,Unit2)
    !  Use for "severe" error messages.  Might have several severe tests and then terminate.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE
  INTERFACE
    SUBROUTINE ShowWarningError(Message,Unit1,Unit2)
    !  Use for "warning" error messages.
    CHARACTER(len=*) Message    ! Message automatically written to "error file"
    INTEGER, OPTIONAL :: Unit1  ! Unit number of open formatted file for message
    INTEGER, OPTIONAL :: Unit2  ! Unit number of open formatted file for message
    END SUBROUTINE
  END INTERFACE


          ! MODULE VARIABLE DECLARATIONS:

!LOGICAL :: BeginDayFlag         ! Set to true at the start of each day, set to false after first time step in day
!LOGICAL :: BeginEnvrnFlag       ! Set to true at the start of each environment, set to false after first time step in environ
!LOGICAL :: BeginHourFlag        ! Set to true at the start of each hour, set to false after first time step in hour
!LOGICAL :: BeginSimFlag         ! Set to true until any actual simulation (full or sizing) has begun, set to
!                                !   false after first time step
!LOGICAL :: BeginFullSimFlag     ! Set to true until full simulation has begun, set to
!                                !   false after first time step
!LOGICAL :: BeginTimeStepFlag    ! Set to true at the start of each time step, set to false after first subtime step of time step
REAL    :: BigNumber            ! Max Number (real) used for initializations
DOUBLE PRECISION :: DBigNumber  ! Max Number (double precision) used for initializations
!INTEGER :: DayOfSim             ! Counter for days (during the simulation)
!CHARACTER(len=25) :: DayOfSimChr ! Counter for days (during the simulation) (character -- for reporting, large so can use write *)
!LOGICAL :: EndEnvrnFlag         ! Set to true at the end of each environment (last time step of last hour of last day of environ)
!LOGICAL :: EndDayFlag           ! Set to true at the end of each day (last time step of last hour of day)
!LOGICAL :: EndHourFlag          ! Set to true at the end of each hour (last time step of hour)
!INTEGER :: PreviousHour         ! Previous Hour Index
!INTEGER :: HourOfDay            ! Counter for hours in a simulation day
!DOUBLE PRECISION :: WeightPreviousHour   ! Weighting of value for previous hour
!DOUBLE PRECISION :: WeightNow            ! Weighting of value for current hour
!INTEGER :: NumOfDayInEnvrn      ! Number of days in the simulation for a particular environment
!INTEGER :: NumOfTimeStepInHour  ! Number of time steps in each hour of the simulation
!INTEGER :: NumOfZones           ! Total number of Zones for simulation
!INTEGER :: TimeStep             ! Counter for time steps (fractional hours)
!DOUBLE PRECISION :: TimeStepZone         ! Zone time step in fractional hours
!LOGICAL :: WarmupFlag           ! Set to true during the warmup portion of a simulation
!INTEGER :: OutputFileStandard   ! Unit number for the standard output file (hourly data only)
!INTEGER :: StdOutputRecordCount ! Count of Standard output records
!INTEGER :: OutputFileInits      ! Unit number for the standard Initialization output file
!INTEGER :: OutputFileDebug      ! Unit number for debug outputs
!CHARACTER(len=1) :: SizingFileColSep=' '  ! Character to separate columns in sizing outputs
!INTEGER :: OutputFileZoneSizing ! Unit number of zone sizing calc output file
!INTEGER :: OutputFileSysSizing  ! Unit number of system sizing calc output file
!INTEGER :: OutputFileMeters     ! Unit number for meters output
!INTEGER :: StdMeterRecordCount  ! Count of Meter output records
!INTEGER :: OutputFileBNDetails  ! Unit number for Branch-Node Details
!! INTEGER :: OutputFileConstrainParams ! Unit number for special constrained free parameters output file
!                                !  (for penalty functions in optimizing)
!LOGICAL :: DebugOutput
!LOGICAL :: EvenDuringWarmup
!LOGICAL :: ZoneSizingCalc = .FALSE.       ! TRUE if zone sizing calculation
!LOGICAL :: SysSizingCalc = .FALSE.        ! TRUE if system sizing calculation
!LOGICAL :: DoZoneSizing         ! User input in RUN CONTROL object
!LOGICAL :: DoSystemSizing       ! User input in RUN CONTROL object
!LOGICAL :: DoPlantSizing        ! User input in RUN CONTROL object
!LOGICAL :: DoDesDaySim          ! User input in RUN CONTROL object
!LOGICAL :: DoWeathSim           ! User input in RUN CONTROL object
!LOGICAL :: WeathSimReq = .false. ! Input has a RunPeriod request
!LOGICAL :: WeatherFile          ! TRUE if current environment is a weather file
!LOGICAL :: DoOutputReporting    ! TRUE if variables to be written out
!LOGICAL :: DoingSizing=.false.  ! TRUE when "sizing" is being performed (some error messages won't be displayed)
!LOGICAL :: DisplayFluidPropsErrors=.true.  ! false when Fluid Properties Warnings should not be displayed (e.g. during warmup)
!LOGICAL :: DisplayExtraWarnings=.false. ! True when section "DisplayExtraWarnings" is entered
!LOGICAL :: DisplayAdvancedReportVariables=.false. ! True when section "DisplayAdvancedReportVariables" is entered
!LOGICAL :: IgnoreInteriorWindowTransmission=.false. ! True when section "IgnoreInteriorWindowTransmission" is entered
!LOGICAL :: MakeMirroredDetachedShading=.true. ! True (default) when Detached Shading Surfaces should be "mirrored"
!LOGICAL :: MakeMirroredAttachedShading=.true. ! True (default) when Attached Shading Surfaces should be "mirrored"
!DOUBLE PRECISION :: CurrentTime ! CurrentTime, in fractional hours, from start of day. Uses Loads time step.
!INTEGER :: SimTimeSteps         ! Number of (Loads) timesteps since beginning of run period (environment).
!INTEGER :: MinutesPerTimeStep   ! Minutes per time step calculated from NumTimeStepInHour (number of minutes per load time step)
!LOGICAL :: DisplayPerfSimulationFlag ! Set to true when "Performing Simulation" should be displayed
!LOGICAL :: MetersHaveBeenInitialized=.false.

!     NOTICE
!
!     Copyright © 1996-2007 The Board of Trustees of the University of Illinois
!     and The Regents of the University of California through Ernest Orlando Lawrence
!     Berkeley National Laboratory.  All rights reserved.
!
!     Portions of the EnergyPlus software package have been developed and copyrighted
!     by other individuals, companies and institutions.  These portions have been
!     incorporated into the EnergyPlus software package under license.   For a complete
!     list of contributors, see "Notice" located in EnergyPlus.f90.
!
!     NOTICE: The U.S. Government is granted for itself and others acting on its
!     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
!     reproduce, prepare derivative works, and perform publicly and display publicly.
!     Beginning five (5) years after permission to assert copyright is granted,
!     subject to two possible five year renewals, the U.S. Government is granted for
!     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
!     worldwide license in this data to reproduce, prepare derivative works,
!     distribute copies to the public, perform publicly and display publicly, and to
!     permit others to do so.
!
!     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
!

END MODULE DataGlobals
