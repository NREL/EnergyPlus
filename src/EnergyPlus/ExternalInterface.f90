    MODULE ExternalInterface

    ! Module containing the routines dealing with the BCVTB and FMU interface

    ! MODULE INFORMATION:
    !       AUTHOR         Michael Wetter
    !       DATE WRITTEN   2Dec2007
    !       MODIFIED       Rui Zhang July 2009
    !       MODIFIED       Thierry S. Nouidui 2011
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS MODULE:
    ! To encapsulate the data and routines required to interface
    ! the Building Controls Virtual Test Bed (BCVTB) and FunctionalMockupUnits (FMU)

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! http://simulationresearch.lbl.gov/bcvtb
    ! http://www.modelisar.com

    ! OTHER NOTES:
    ! na

    ! USE STATEMENTS:
!    USE DataGlobals, ONLY: MaxNameLength
!    USE DataPrecisionGlobals
!    USE DataStringGlobals, ONLY: PathChar,AltPathChar,CurrentWorkingFolder
!    USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, &
!    ShowFatalError, SetupOutputVariable, ShowContinueError
!    USE General, ONLY: TrimSigDigits
!    USE ISO_C_BINDING, ONLY: C_PTR, C_INT, C_DOUBLE

    ! <use statements for access to subroutines in other modules>

!    IMPLICIT NONE ! Enforce explicit typing of all variables

!    REAL(r64), PUBLIC :: tComm = 0.0d0      ! Communication time step
!    REAL(r64), PUBLIC :: tStop = 3600.0d0   ! Stop time used during the warmup period
!    REAL(r64), PUBLIC :: tStart = 0.0d0     ! Start time used during the warmup period
!    REAL(r64), PUBLIC :: hStep = 15.0d0     ! Communication step size
!    INTEGER, PUBLIC :: fmiTrue = 1
!    INTEGER, PUBLIC :: fmiFalse = 0
!    LOGICAL, PUBLIC :: FlagReIni = .FALSE.                         ! Flag for reinitialization of states in GetSetAndDoStep
!    CHARACTER(len=10*MaxNameLength) :: FMURootWorkingFolder = ' '  ! FMU root working folder
!    INTEGER ::LEN_FMU_ROOT_DIR

!    PRIVATE ! Everything private unless explicitly made public

!    ! MODULE PARAMETER DEFINITIONS:
!    INTEGER, PARAMETER :: maxVar = 1024              ! Maximum number of variables to be exchanged
!    INTEGER, PARAMETER :: maxErrMsgLength = 10000    ! Maximum error message length from xml schema validation
!    INTEGER, PARAMETER :: indexSchedule = 1   ! Index for schedule in inpVarTypes
!    INTEGER, PARAMETER :: indexVariable = 2   ! Index for variable in inpVarTypes
!    INTEGER, PARAMETER :: indexActuator = 3   ! Index for actuator in inpVarTypes
!    INTEGER, PARAMETER :: nInKeys       = 3   ! Number of input variables available in ExternalInterface (=highest index* number)
!    INTEGER, PARAMETER :: fmiOK = 0           ! fmiOK
!    INTEGER, PARAMETER :: fmiWarning = 1      ! fmiWarning
!    INTEGER, PARAMETER :: fmiDiscard = 2      ! fmiDiscard
!    INTEGER, PARAMETER :: fmiError = 3        ! fmiError
!    INTEGER, PARAMETER :: fmiFatal = 4        ! fmiPending
!    INTEGER, PARAMETER :: fmiPending = 5      ! fmiPending
!    CHARACTER(len=*), PARAMETER :: socCfgFilNam="socket.cfg" ! socket configuration file
!    CHARACTER(len=*), PARAMETER :: BlankString=' '


    ! DERIVED TYPE DEFINITIONS:

!    TYPE  fmuInputVariableType
!        CHARACTER(len= MaxNameLength)      :: Name = BlankString         ! Name of FMU input variable
!        INTEGER                            :: ValueReference = 0       ! = fmiValueReference specific to FMU variable
!    END TYPE fmuInputVariableType

!    TYPE  checkFMUInstanceNameType
!        CHARACTER(len= MaxNameLength)      :: Name = BlankString         ! Name of fmu instance
!    END TYPE checkFMUInstanceNameType

!    TYPE  eplusOutputVariableType
!        CHARACTER(len=MaxNameLength), DIMENSION(1)       :: Name   = BlankString ! Variable name in EnergyPlus
!        CHARACTER(len=MaxNameLength), DIMENSION(1)       :: VarKey = BlankString ! Key value in EnergyPlus
!        REAL(r64)                                        :: RTSValue  = 0.0d0    ! Real value of variable at the Zone Time Step
!        INTEGER                            :: ITSValue       = 0               ! Integer value of variable at the Zone Time Step
!        INTEGER                            :: VarIndex       = 0               ! Index Value of variable
!        INTEGER                            :: VarType        = 0               ! Type of variable at the Zone Time Step
!        CHARACTER(len=MaxNameLength)       :: VarUnits       = BlankString       ! Units string, may be blank
!    END TYPE eplusOutputVariableType

!    TYPE  fmuOutputVariableScheduleType
!        CHARACTER(len=MaxNameLength)  :: Name = BlankString           ! Name of fmu output variable --> schedule in energyplus
!        REAL(r64)                     :: RealVarValue         = 0.0d0 ! = Real value at the Zone Time Step
!        INTEGER                       :: ValueReference       = 0   ! = fmiValueReference specific to FMU variable
!    END TYPE fmuOutputVariableScheduleType

!    TYPE  fmuOutputVariableVariableType
!        CHARACTER(len=MaxNameLength)  :: Name = BlankString           ! Name of fmu output variable --> variable in energyplus
!        REAL(r64)                     :: RealVarValue         = 0.0d0 ! = Real value at the Zone Time Step
!        INTEGER                       :: ValueReference       = 0   ! = fmiValueReference specific to FMU variable
!    END TYPE fmuOutputVariableVariableType

!    TYPE  fmuOutputVariableActuatorType
!        CHARACTER(len=MaxNameLength)  :: Name = BlankString           ! Name of fmu output variable --> actuator in energyplus
!        REAL(r64)                     :: RealVarValue         = 0.0d0 ! = Real value at the Zone Time Step
!        INTEGER                       :: ValueReference       = 0   ! = fmiValueReference specific to FMU variable
!    END TYPE fmuOutputVariableActuatorType

!    TYPE  eplusInputVariableScheduleType
!        CHARACTER(len=MaxNameLength)  :: Name = BlankString        ! Name of energyplus input variable from Type schedule
!        INTEGER                       :: VarIndex      = 0       ! Index Value of this variable
!        INTEGER                       :: InitialValue              ! Initial value used during the warmup
!    END TYPE eplusInputVariableScheduleType

!    TYPE  eplusInputVariableVariableType
!        CHARACTER(len=MaxNameLength)  :: Name   = BlankString      ! Name of energyplus input variable from Type variable
!        INTEGER                       :: VarIndex      = 0       ! Index Value of this variable
!    END TYPE eplusInputVariableVariableType

!    TYPE  eplusInputVariableActuatorType
!        CHARACTER(len=MaxNameLength)  :: Name   = BlankString      ! Name of energyplus input variable from Type actuator
!        INTEGER                       :: VarIndex      = 0       ! Index Value of this variable
!    END TYPE eplusInputVariableActuatorType

!    TYPE InstanceType
!        CHARACTER(len=MaxNameLength)       :: Name = BlankString               ! FMU Filename
!        CHARACTER(len=10 * MaxNameLength)  :: modelID = BlankString            ! FMU modelID
!        CHARACTER(len=10 * MaxNameLength)  :: modelGUID = BlankString          ! FMU modelGUID
!        CHARACTER(len=10 * MaxNameLength)  :: WorkingFolder = BlankString      ! Path to the FMU wokring folder
!        CHARACTER(len=10 * MaxNameLength)  :: WorkingFolder_wLib = BlankString ! Path to the binaries
!        CHARACTER(len=10 * MaxNameLength)  :: fmiVersionNumber = BlankString   ! Version number of FMI used
!        INTEGER                            :: NumInputVariablesInFMU=0         ! Number of input variables in fmu
!        INTEGER                            :: NumInputVariablesInIDF=0         ! Number of fmus input variables in idf
!        INTEGER                            :: NumOutputVariablesInFMU=0        ! Number of output variables in fmu
!        INTEGER                            :: NumOutputVariablesInIDF=0        ! Number of output variables in idf
!        INTEGER                            :: NumOutputVariablesSchedule=0     ! Number of output variables from type schedule
!        INTEGER                            :: NumOutputVariablesVariable=0     ! Number of output variables from type variable
!        INTEGER                            :: NumOutputVariablesActuator=0     ! Number of output variables from type actuator
!        INTEGER                            :: LenModelID=0                     ! Length of modelID trimmed
!        INTEGER                            :: LenModelGUID=0                   ! Length of modelGUID trimmed
!        INTEGER                            :: LenWorkingFolder=0               ! Length of working folder trimmed
!        INTEGER                            :: LenWorkingFolder_wLib=0          ! Length of working folder with libraries trimmed
!        TYPE (C_PTR)                       :: fmiComponent                     ! FMU instance
!        INTEGER                            :: fmiStatus  ! Status of fmi
!        INTEGER                            :: Index      ! Index of FMU
!        ! Variable Types structure for fmu input variables
!        TYPE (fmuInputVariableType), &
!        DIMENSION(:), ALLOCATABLE  :: fmuInputVariable
!        ! Variable Types structure for checking duplicates fmu input variables
!        TYPE (fmuInputVariableType), &
!        DIMENSION(:), ALLOCATABLE  :: checkfmuInputVariable
!        ! Variable Types structure for energyplus output variables
!        TYPE (eplusOutputVariableType), &
!        DIMENSION(:), ALLOCATABLE  :: eplusOutputVariable
!        ! Variable Types structure for fmu output variables from type schedule
!        TYPE (fmuOutputVariableScheduleType), &
!        DIMENSION(:), ALLOCATABLE  :: fmuOutputVariableSchedule
!        ! Variable Types structure for energyplus input variables from type schedule
!        TYPE (eplusInputVariableScheduleType), &
!        DIMENSION(:), ALLOCATABLE  :: eplusInputVariableSchedule
!        ! Variable Types structure for fmu output variables from type variable
!        TYPE (fmuOutputVariableVariableType), &
!        DIMENSION(:), ALLOCATABLE  :: fmuOutputVariableVariable
!        ! Variable Types structure for energyplus input variables from type variable
!        TYPE (eplusInputVariableVariableType), &
!        DIMENSION(:), ALLOCATABLE  :: eplusInputVariableVariable
!        ! Variable Types structure for fmu output variables from type actuator
!        TYPE (fmuOutputVariableActuatorType), &
!        DIMENSION(:), ALLOCATABLE  :: fmuOutputVariableActuator
!        ! Variable Types structure for energyplus input variables from type actuator
!        TYPE (eplusInputVariableActuatorType), &
!        DIMENSION(:), ALLOCATABLE  :: eplusInputVariableActuator
!    END TYPE InstanceType

!    TYPE FMUType
!        CHARACTER(len=MaxNameLength)  :: Name      = BlankString          ! FMU Filename
!        REAL(r64)                     :: TimeOut   = 0.0d0                ! Default TimeOut value
!        INTEGER                       :: Visible   = 0                    ! Default Visible value
!        INTEGER                       :: Interactive = 0                  ! Default Interactive value
!        INTEGER                       :: LoggingOn = 0                    ! Default LoggingOn value
!        INTEGER                       :: NumInstances=0                   ! Number of Instances
!        INTEGER                       :: TotNumInputVariablesInIDF=0      ! Number of input variables
!        INTEGER                       :: TotNumOutputVariablesSchedule=0  ! Number of output variables from type schedule
!        INTEGER                       :: TotNumOutputVariablesVariable=0  ! Number of output variables from type variable
!        INTEGER                       :: TotNumOutputVariablesActuator=0  ! Number of output variables from type actuator
!        TYPE  (InstanceType), &
!        DIMENSION(:), ALLOCATABLE  :: Instance  ! Variable Types structure for energyplus input variables from type actuator
!    END TYPE FMUType

    ! MODULE VARIABLE DECLARATIONS:

!    TYPE (FMUType), &
!    DIMENSION(:), ALLOCATABLE      :: FMU  ! Variable Types structure
!    TYPE (FMUType), &
!    DIMENSION(:), ALLOCATABLE      :: FMUTemp  ! Variable Types structure
!    TYPE (checkFMUInstanceNameType), &
!    DIMENSION(:), ALLOCATABLE      :: checkInstanceName                   ! Variable Types structure for checking instance names
!    INTEGER, PUBLIC                         :: NumExternalInterfaces = 0           ! Number of ExternalInterface objects
!    INTEGER, PUBLIC                         :: NumExternalInterfacesBCVTB = 0      ! Number of BCVTB ExternalInterface objects
!    INTEGER, PUBLIC                         :: NumExternalInterfacesFMUImport = 0  ! Number of FMU ExternalInterface objects
!    INTEGER, PUBLIC                         :: NumExternalInterfacesFMUExport = 0  ! Number of FMU ExternalInterface objects
!    INTEGER, PUBLIC                         :: NumFMUObjects = 0                   ! Number of FMU objects
!    INTEGER, PUBLIC                         :: FMUExportActivate = 0               ! FMU Export flag
!    LOGICAL, PUBLIC                         :: haveExternalInterfaceBCVTB  = .FALSE.          ! Flag for BCVTB interface
!    LOGICAL, PUBLIC                         :: haveExternalInterfaceFMUImport = .FALSE.       ! Flag for FMU-Import interface
!    LOGICAL, PUBLIC                         :: haveExternalInterfaceFMUExport = .FALSE.       ! Flag for FMU-Export interface
!    INTEGER                                 :: simulationStatus = 1 ! Status flag. Used to report during
!    ! which phase an error occured.
!    ! (1=initialization, 2=time stepping)

!    INTEGER,      ALLOCATABLE, DIMENSION(:) :: keyVarIndexes        ! Array index for specific key name
!    INTEGER,      ALLOCATABLE, DIMENSION(:) :: varTypes             ! Types of variables in keyVarIndexes
!    INTEGER,      ALLOCATABLE, DIMENSION(:) :: varInd               ! Index of ErlVariables for ExternalInterface
!    INTEGER                                 :: socketFD = -1        ! socket file descriptor
!    LOGICAL                                 :: ErrorsFound=.false.  ! Set to true if errors are found
!    LOGICAL                                 :: noMoreValues=.false. ! Flag, true if no more values
!    ! will be sent by the server

!    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: varKeys  ! Keys of report variables used for data exchange
!    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: varNames ! Names of report variables used for data exchange
!    INTEGER, ALLOCATABLE, DIMENSION(:)                      :: inpVarTypes! Names of report variables used for data exchange
!    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: inpVarNames ! Names of report variables used for data exchange

!    LOGICAL       :: configuredControlPoints = .FALSE. ! True if control points have been configured
!    LOGICAL       :: useEMS = .false.  ! Will be set to true if ExternalInterface writes to EMS variables or actuators

!    ! SUBROUTINE SPECIFICATIONS FOR MODULE ExternalInterface:


!    PUBLIC  ExternalInterfaceExchangeVariables
!    PUBLIC  CloseSocket
!    PRIVATE InitExternalInterface
!    PRIVATE GetExternalInterfaceInput
!    PRIVATE CalcExternalInterface
!    PRIVATE ParseString
!    PRIVATE GetReportVariableKey
!    PRIVATE StopExternalInterfaceIfError
!    PRIVATE ValidateRunControl
!    PRIVATE WarnIfExternalInterfaceObjectsAreUsed
!    PUBLIC CalcExternalInterfaceFMUImport
!    PUBLIC InitExternalInterfaceFMUImport
!    PUBLIC InstantiateInitializeFMUImport
!    PUBLIC TerminateResetFreeFMUImport
!    PUBLIC GetSetVariablesAndDoStepFMUImport

!    CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE ExternalInterfaceExchangeVariables

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   2Dec2007
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! Exchanges variables between EnergyPlus and the BCVTB socket.

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE DataGlobals, ONLY: WarmupFlag, KindOfSim, ksRunPeriodWeather, ZoneTSReporting !, ZoneSizingCalc, SysSizingCalc
!    USE, INTRINSIC :: ieee_exceptions
!    USE, INTRINSIC :: ieee_arithmetic
!    USE, INTRINSIC :: ieee_features

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:


!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na
!    INTERFACE
!    INTEGER (C_INT) FUNCTION checkOperatingSystem(errorMessage) BIND (C, NAME="checkOperatingSystem")
!    ! Function called to check operating system
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: errorMessage ! error message
!    END FUNCTION checkOperatingSystem
!    END INTERFACE

!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    LOGICAL,SAVE :: GetInputFlag = .true.  ! First time, input is "gotten"
!    CHARACTER(len=10*MaxNameLength) :: errorMessage = BlankString ! Error message
!    INTEGER :: retValErrMsg
!    IF (GetInputFlag) THEN
!        CALL GetExternalInterfaceInput
!        GetInputFlag=.false.
!    ENDIF

!    ! Parameters for KindOfSim
!    !INTEGER, PARAMETER :: ksDesignDay = 1
!    !INTEGER, PARAMETER :: ksRunPeriodDesign = 2
!    !INTEGER, PARAMETER :: ksRunPeriodWeather = 3

!    IF (haveExternalInterfaceBCVTB .OR. haveExternalInterfaceFMUExport) THEN
!        CALL InitExternalInterface()
!        ! Exchange data only after sizing and after warm-up.
!        ! Note that checking for ZoneSizingCalc SysSizingCalc does not work here, hence we
!        ! use the KindOfSim flag
!        IF (.NOT.(WarmupFlag)  .AND. (KindOfSim .EQ. ksRunPeriodWeather)) THEN
!            CALL CalcExternalInterface()
!        ENDIF
!    END IF

!    IF (haveExternalInterfaceFMUImport) THEN
!        retValErrMsg = checkOperatingSystem(errorMessage)
!        IF (retValErrMsg .NE. 0 ) THEN
!            CALL ShowSevereError('ExternalInterface/ExternalInterfaceExchangeVariables:"'//TRIM(errorMessage)//'"')
!            ErrorsFound = .true.
!            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!            CALL StopExternalInterfaceIfError
!        END IF
!        ! initialize the FunctionalMockupUnitImport interface
!        CALL IEEE_SET_HALTING_MODE(IEEE_OVERFLOW,.FALSE.)
!        CALL IEEE_SET_HALTING_MODE(IEEE_INVALID,.FALSE.)
!        CALL IEEE_SET_HALTING_MODE(IEEE_DIVIDE_BY_ZERO,.FALSE.)
!        CALL InitExternalInterfaceFMUImport()
!        ! No Data exchange during design days
!        ! Data Exchange data during warmup and after warmup
!        CALL CalcExternalInterfaceFMUImport()
!        CALL IEEE_SET_FLAG(ieee_all,.false.)
!        CALL IEEE_SET_HALTING_MODE(IEEE_OVERFLOW,.TRUE.)
!        CALL IEEE_SET_HALTING_MODE(IEEE_INVALID,.TRUE.)
!        CALL IEEE_SET_HALTING_MODE(IEEE_DIVIDE_BY_ZERO,.TRUE.)
!    END IF

!    RETURN

!    END SUBROUTINE ExternalInterfaceExchangeVariables

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE GetExternalInterfaceInput

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   2Dec2007
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! Obtains input data for ExternalInterface

!    ! METHODOLOGY EMPLOYED:
!    ! Uses InputProcessor "Get" routines to obtain data.

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
!    USE DataIPShortCuts

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! na

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    CHARACTER(len=MaxNameLength), DIMENSION(3) :: Alphas  ! Alpha items for object
!    REAL, DIMENSION(1)             :: Numbers ! Numeric items for object
!    INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
!    INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
!    INTEGER                        :: IOStatus   ! Used in GetObjectItem
!    INTEGER                        :: Loop       ! Loop counter
!    LOGICAL                        :: IsNotOK              ! Flag to verify name
!    LOGICAL                        :: IsBlank              ! Flag for blank name


!    cCurrentModuleObject='ExternalInterface'
!    NumExternalInterfaces = GetNumObjectsFound(cCurrentModuleObject)

!    DO Loop=1,NumExternalInterfaces ! This loop determines whether the external interface is for FMU or BCVTB
!        CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!        AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
!        IF (SameString(cAlphaArgs(1), 'PtolemyServer')) THEN ! The BCVTB interface is activated.
!            NumExternalInterfacesBCVTB = NumExternalInterfacesBCVTB + 1
!        ELSEIF (SameString(cAlphaArgs(1), 'FunctionalMockupUnitImport')) THEN
!            ! The functional mock up unit import interface is activated.
!            NumExternalInterfacesFMUImport = NumExternalInterfacesFMUImport + 1
!        ELSEIF (SameString(cAlphaArgs(1), 'FunctionalMockupUnitExport')) THEN
!            ! The functional mock up unit export interface is activated.
!            NumExternalInterfacesFMUExport = NumExternalInterfacesFMUExport + 1
!        END IF
!    END DO
!    ! Check if objects are used although BCVTB interface object is not defined
!    IF (NumExternalInterfacesBCVTB == 0) THEN
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:Schedule')
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:Variable')
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:Actuator')
!    ENDIF

!    ! Check if objects are used although FMUExport interface is not defined
!    IF (NumExternalInterfacesFMUExport == 0) THEN
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:FunctionalMockupUnitExport:To:Schedule')
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:FunctionalMockupUnitExport:To:Variable')
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:FunctionalMockupUnitExport:To:Actuator')
!    ENDIF

!    ! Check if objects are used although FMU Import interface is not defined
!    IF (NumExternalInterfacesFMUImport == 0) THEN
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:FunctionalMockupUnitImport:To:Schedule')
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:FunctionalMockupUnitImport:To:Variable')
!        CALL WarnIfExternalInterfaceObjectsAreUsed('ExternalInterface:FunctionalMockupUnitImport:To:Actuator')
!    ENDIF

!    IF ((NumExternalInterfacesBCVTB == 1) .AND. (NumExternalInterfacesFMUExport == 0)) THEN
!        haveExternalInterfaceBCVTB = .TRUE.
!        CALL DisplayString('Instantiating Building Controls Virtual Test Bed')
!        ALLOCATE(varKeys(maxVar))  ! Keys of report variables used for data exchange
!        varKeys=' '
!        ALLOCATE(varNames(maxVar)) ! Names of report variables used for data exchange
!        varNames=' '
!        ALLOCATE(inpVarTypes(maxVar)) ! Names of report variables used for data exchange
!        inpVarTypes=0
!        ALLOCATE(inpVarNames(maxVar)) ! Names of report variables used for data exchange
!        inpVarNames=' '
!        CALL VerifyExternalInterfaceObject
!    ELSEIF ((NumExternalInterfacesBCVTB == 0) .AND. (NumExternalInterfacesFMUExport == 1)) THEN
!        haveExternalInterfaceFMUExport = .TRUE.
!        FMUExportActivate = 1
!        CALL DisplayString('Instantiating FunctionalMockupUnitExport interface')
!        ALLOCATE(varKeys(maxVar))  ! Keys of report variables used for data exchange
!        varKeys=' '
!        ALLOCATE(varNames(maxVar)) ! Names of report variables used for data exchange
!        varNames=' '
!        ALLOCATE(inpVarTypes(maxVar)) ! Names of report variables used for data exchange
!        inpVarTypes=0
!        ALLOCATE(inpVarNames(maxVar)) ! Names of report variables used for data exchange
!        inpVarNames=' '
!        CALL VerifyExternalInterfaceObject
!    ELSEIF ((NumExternalInterfacesBCVTB == 1) .AND. (NumExternalInterfacesFMUExport /= 0)) THEN
!        CALL ShowSevereError('GetExternalInterfaceInput: Cannot have Ptolemy and FMU-Export interface simultaneously.')
!        ErrorsFound = .true.
!    ENDIF

!    IF ((NumExternalInterfacesFMUImport == 1) .AND. (NumExternalInterfacesFMUExport == 0)) THEN
!        haveExternalInterfaceFMUImport = .TRUE.
!        CALL DisplayString('Instantiating FunctionalMockupUnitImport interface')
!        cCurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport'
!        NumFMUObjects = GetNumObjectsFound(cCurrentModuleObject)
!        CALL VerifyExternalInterfaceObject
!    ELSEIF ((NumExternalInterfacesFMUImport == 1) .AND. (NumExternalInterfacesFMUExport /= 0)) THEN
!        CALL ShowSevereError('GetExternalInterfaceInput: Cannot have FMU-Import and FMU-Export interface simultaneously.')
!        ErrorsFound = .true.
!    ENDIF

!    IF (NumExternalInterfacesBCVTB > 1) THEN
!        CALL ShowSevereError('GetExternalInterfaceInput: Cannot have more than one Ptolemy interface.')
!        CALL ShowContinueError('GetExternalInterfaceInput: Errors found in input.')
!        ErrorsFound = .true.
!    ENDIF

!    IF (NumExternalInterfacesFMUExport > 1) THEN
!        CALL ShowSevereError('GetExternalInterfaceInput: Cannot have more than one FMU-Export interface.')
!        CALL ShowContinueError('Errors found in input.')
!        ErrorsFound = .true.
!    ENDIF

!    IF (NumExternalInterfacesFMUImport > 1) THEN
!        CALL ShowSevereError('GetExternalInterfaceInput: Cannot have more than one FMU-Import interface.')
!        CALL ShowContinueError('Errors found in input.')
!        ErrorsFound = .true.
!    ENDIF
!    IF (ErrorsFound) THEN
!        CALL ShowFatalError('GetExternalInterfaceInput: preceding conditions cause termination.')
!    ENDIF
!    CALL StopExternalInterfaceIfError

!    RETURN

!    END SUBROUTINE GetExternalInterfaceInput

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE StopExternalInterfaceIfError
!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   9Jan2008
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine gracefully stops the ExternalInterface if an error has been found.
!    ! It sends an appropriate message to the ExternalInterface
!    ! and then calls a fatal error to stop EnergyPlus.

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    INTERFACE
!    INTEGER(C_INT) FUNCTION sendclientmessage(socketFD, flaWri) BIND (C, NAME="sendclientmessage")
!    USE ISO_C_BINDING, ONLY: C_INT
!    INTEGER(C_INT) socketFD ! socket file descriptor
!    INTEGER(C_INT) flaWri   ! flag to write to the socket
!    END FUNCTION sendclientmessage
!    END INTERFACE

!    ! USE STATEMENTS:
!    ! na

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER retVal ! Return value, needed to catch return value of function call
!    IF ((NumExternalInterfacesBCVTB /= 0) .OR. (NumExternalInterfacesFMUExport /= 0))  THEN
!        IF ( ErrorsFound ) THEN
!            ! Check if the socket is open
!            IF ( socketFD .GE. 0 ) THEN
!                ! Socket is open
!                IF (simulationStatus == 1) THEN
!                    retVal = sendclientmessage(socketFD, -10)
!                ELSE
!                    retVal = sendclientmessage(socketFD, -20)
!                ENDIF
!            ENDIF
!            CALL ShowFatalError('Error in ExternalInterface: Check EnergyPlus *.err file.')
!        ENDIF
!    END IF
!    IF (NumExternalInterfacesFMUImport /= 0) THEN
!        IF ( ErrorsFound ) THEN
!            CALL ShowFatalError('ExternalInterface/StopExternalInterfaceIfError: Error in ExternalInterface:'// &
!            'Check EnergyPlus *.err file.')
!        ENDIF
!    END IF
!    END SUBROUTINE StopExternalInterfaceIfError

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE CloseSocket(FlagToWriteToSocket)
!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   December 2008
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine tries to write the optional error code to the
!    ! socket and then closes the socket

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    INTERFACE
!    INTEGER(C_INT) FUNCTION establishclientsocket(fileName) BIND (C, NAME="establishclientsocket")
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! file from which socket port number will be read
!    END FUNCTION establishclientsocket
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) function sendclientmessage(socketFD, flaWri) BIND (C, NAME="sendclientmessage")
!    USE ISO_C_BINDING, ONLY: C_INT
!    INTEGER(C_INT) socketFD ! socket file descriptor
!    INTEGER(C_INT) flaWri   ! flag to write to the socket
!    END FUNCTION sendclientmessage
!    END INTERFACE

!    ! USE STATEMENTS:
!    ! na

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    INTEGER, INTENT(IN) :: FlagToWriteToSocket  ! flag to write to the socket
!    ! +1: E+ reached final time
!    ! -1: E+ had some error

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER retVal ! Return value, needed to catch return value of function call
!    LOGICAL fileExist ! Set to true if file exists

!    ! Try to establish socket connection. This is needed if Ptolemy started E+,
!    ! but E+ had an error before the call to InitExternalInterface.

!    INQUIRE (FILE=socCfgFilNam, EXIST=fileExist)

!    IF ( socketFD ==  -1 .AND. fileExist) THEN
!        socketFD = establishclientsocket(socCfgFilNam)
!    END IF

!    IF ( socketFD .GE. 0 ) THEN
!        retVal = sendclientmessage(socketFD, FlagToWriteToSocket)
!        ! Don't close socket as this may give sometimes an IOException in Windows
!        ! This problem seems to affect only Windows but not Mac
!        !     close(socketFD)
!    ENDIF
!    RETURN

!    END SUBROUTINE CloseSocket

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE ParseString(str, ele, nEle)
!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   8Jan2008
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine parses the semicolon separated string xmlStr
!    ! and assigns each element to ele

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE InputProcessor, ONLY: MakeUPPERCase

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    CHARACTER(len=*), INTENT(IN) :: str                          ! The string, with all elements separated by ';'
!    CHARACTER(len=MaxNameLength), DIMENSION(:), INTENT(OUT) :: ele  ! The elements
!    INTEGER, INTENT(IN)                  :: nEle ! The number of elements

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! na
!    INTEGER :: i ! Counter
!    INTEGER :: iSta ! Start of substring
!    INTEGER :: iEnd ! End of substring
!    INTEGER :: lenStr ! Length of string
!    lenStr = len(str)
!    iEnd = 1
!    DO i = 1, nEle
!        iSta = iEnd  ! add one to skip ';'
!        iEnd = iSta+INDEX(str(iSta:lenStr), ';')
!        ele(i) = TRIM(MakeUPPERCase(str(iSta:(iEnd-2))))
!    ENDDO
!    END SUBROUTINE ParseString

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE InitExternalInterface()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   2Dec2007
!    !       MODIFIED       Rui Zhang Aug 2009
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine is for initializations of the ExternalInterface

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE ScheduleManager, ONLY: GetDayScheduleIndex
!    USE RuntimeLanguageProcessor, ONLY: isExternalInterfaceErlVariable, FindEMSVariable
!    USE DataGlobals, ONLY: WeathSimReq

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:

!    LOGICAL,SAVE :: firstCall    = .TRUE.  ! First time, input has been read

!    CHARACTER(len=*), PARAMETER :: simCfgFilNam="variables.cfg"

!    INTEGER :: i, j         ! Loop counter
!    ! INTERFACE BLOCK SPECIFICATIONS
!    ! na

!    ! DERIVED TYPE DEFINITIONS
!    ! na

!    INTERFACE
!    INTEGER(C_INT) FUNCTION establishclientsocket(fileName) BIND (C, NAME="establishclientsocket")
!    ! Returns the logical unit number of the socket
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! file from which socket port number will be read
!    END FUNCTION establishclientsocket
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getmainversionnumber() BIND (C, NAME="getmainversionnumber")
!    ! Returns the main version number of the interface, or a negative
!    ! number if a dummy dll is used
!    USE ISO_C_BINDING, ONLY: C_INT
!    END FUNCTION getmainversionnumber
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getnumberofxmlvalues(fileName, xpathExpr)
!    ! Returns the number of XML values in the file, or a negative
!    ! value if an error occurred
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! Name of XML file
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: xpathExpr ! XPath expression
!    END FUNCTION getnumberofxmlvalues
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getxmlvalue(fileName, xpathExpr, res, nAtr) BIND (C, NAME="getxmlvalue")
!    ! Gets the xml values, and returns -1 if an error occurred, or 0 otherwise
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName  ! Name of XML file
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: xpathExpr ! XPath expression
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: res       ! The values of the xml attributes will be stored here
!    INTEGER(C_INT)                :: nAtr             ! The number of attributes to be obtained
!    END FUNCTION getxmlvalue
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getepvariables(fileName, outVarsNam, outVarsTyp, nOutVars, inVarsKey, &
!    nInVarsKeys, inVars, nInVars, inVarsTyp, strLen) BIND (C, NAME="getepvariables")
!    ! Gets the EnergyPlus variables with which data is being exchanged.
!    ! This function return -1 if an error occurred, or 0 otherwise.
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName    ! Name of the XMLfile
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: outVarsNam  ! Comma seperated string for returned
!    ! output variable names
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: outVarsTyp  ! Comma seperated string for returned
!    ! output variable types
!    INTEGER(C_INT)                :: nOutVars           ! Number of output variables found
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: inVarsKey   ! The available variable types from
!    ! ExternalInterface for EP input(comma seperated)
!    INTEGER(C_INT)                :: nInVarsKeys        ! Number of available variables types
!    ! from ExternalInterface for EP input
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: inVars      ! Comma seperated string for returned
!    ! input variables names
!    INTEGER(C_INT)                :: nInVars            ! Number of input variables found
!    INTEGER(C_INT), DIMENSION(*)  :: inVarsTyp          ! Comma seperated string for returned
!    ! input variable type
!    INTEGER(C_INT)                :: strLen             ! Length of the xml string
!    ! to be returned
!    END FUNCTION getepvariables
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getepvariablesFMU(fileName, outVarsNam, outVarsTyp, nOutVars, inVarsKey, &
!    nInVarsKeys, inVars, nInVars, inVarsTyp, strLen) BIND (C, NAME="getepvariablesFMU")
!    ! Gets the EnergyPlus variables with which data is being exchanged.
!    ! This function return -1 if an error occurred, or 0 otherwise.
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fileName    ! Name of the XMLfile
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: outVarsNam  ! Comma seperated string for returned
!    ! output variable names
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: outVarsTyp  ! Comma seperated string for returned
!    ! output variable types
!    INTEGER(C_INT)                :: nOutVars           ! Number of output variables found
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: inVarsKey   ! The available variable types from
!    ! ExternalInterface for EP input(comma seperated)
!    INTEGER(C_INT)                :: nInVarsKeys        ! Number of available variables types
!    ! from ExternalInterface for EP input
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: inVars      ! Comma seperated string for returned
!    ! input variables names
!    INTEGER(C_INT)                :: nInVars            ! Number of input variables found
!    INTEGER(C_INT), DIMENSION(*)  :: inVarsTyp          ! Comma seperated string for returned
!    ! input variable type
!    INTEGER(C_INT)                :: strLen             ! Length of the xml string
!    ! to be returned
!    END FUNCTION getepvariablesFMU
!    END INTERFACE

!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER, PARAMETER :: lenXmlStr = maxVar*MaxNameLength ! Length of xml string
!    CHARACTER(len=lenXmlStr)     :: xmlStrOut       ! xml values in string, separated by ';'
!    CHARACTER(len=lenXmlStr)     :: xmlStrOutTyp    ! xml values in string, separated by ';'
!    CHARACTER(len=lenXmlStr)     :: xmlStrInKey     ! xml values in string, separated by ';'
!    CHARACTER(len=lenXmlStr)     :: xmlStrIn        ! xml values in string, separated by ';'
!    CHARACTER(len=lenXmlStr)     :: xmlStrInTyp     ! xml values in string, separated by ';'
!    INTEGER, SAVE                :: nOutVal         ! Number of output values (E+ -> ExternalInterface)
!    INTEGER, SAVE                :: nInpVar         ! Number of input values (ExternalInterface -> E+)
!    INTEGER                      :: retVal          ! Return value of function call, used for error handling
!    INTEGER                      :: counter = 0     ! Counter for ErlVariables
!    INTEGER                      :: mainVersion     ! The version number

!    CHARACTER(len=MaxNameLength), DIMENSION(maxVar) :: curVals ! Names of schedules (i.e., schedule names)
!    INTEGER curNumInpVal                                       ! current number of input values for the InputValType
!    CHARACTER(len=maxErrMsgLength)   :: validateErrMsg         ! error returned when xml Schema validate failed
!    INTEGER                          :: errMsgLen              ! the length of the error message
!    LOGICAL socFileExist                                       ! Set to true if socket configuration
!    ! file exists
!    LOGICAL simFileExist                                       ! Set to true if simulation configuration
!    ! file exists
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    IF (FirstCall) THEN
!        CALL DisplayString('ExternalInterface initializes.')
!        ! do one time initializations
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        IF (haveExternalInterfaceBCVTB) THEN
!            ! Check version number
!            mainVersion = getmainversionnumber()
!            IF (mainVersion .LT. 0.0d0) THEN
!                CALL ShowSevereError('ExternalInterface: BCVTB is not installed in this version.')
!                ErrorsFound = .true.
!                CALL StopExternalInterfaceIfError
!            END IF
!        END IF

!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Get port number
!        INQUIRE (FILE=socCfgFilNam, EXIST=socFileExist)
!        IF (socFileExist) THEN
!            socketFD = establishclientsocket(socCfgFilNam)
!            IF (socketFD .LT. 0) THEN
!                CALL ShowSevereError('ExternalInterface: Could not open socket. File descriptor = ' &
!                //TRIM(TrimSigDigits(socketFD))//'.')
!                ErrorsFound = .true.
!            END IF
!        ELSE
!            CALL ShowSevereError('ExternalInterface: Did not find file "'//socCfgFilNam//'".')
!            CALL ShowContinueError('This file needs to be in same directory as in.idf.')
!            CALL ShowContinueError('Check the documentation for the ExternalInterface.')
!            ErrorsFound = .true.
!        END IF
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Make sure that idf file specified a run period other than
!        ! design day and system sizing.
!        CALL ValidateRunControl

!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        CALL StopExternalInterfaceIfError

!        xmlStrOut(1:lenXmlStr) = " "
!        xmlStrOutTyp(1:lenXmlStr) = " "
!        xmlStrInKey(1:lenXmlStr) = " "
!        xmlStrIn(1:lenXmlStr) = " "
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Get input and output variables for EnergyPlus in sequence
!        xmlStrInKey= "schedule," &
!        //"variable," &
!        //"actuator"//char(0)
!        ! Check if simCfgFilNam exists.
!        INQUIRE (FILE=simCfgFilNam, EXIST=simFileExist)
!        IF (simFileExist) THEN
!            IF (haveExternalInterfaceBCVTB) THEN
!                retVal = getepvariables(simCfgFilNam, &
!                xmlStrOutTyp ,xmlStrOut, nOutVal, &
!                xmlStrInKey, nInKeys, &
!                xmlStrIn, nInpVar, inpVarTypes, lenXmlStr)
!            ELSEIF (haveExternalInterfaceFMUExport) THEN
!                retVal = getepvariablesFMU(simCfgFilNam, &
!                xmlStrOutTyp ,xmlStrOut, nOutVal, &
!                xmlStrInKey, nInKeys, &
!                xmlStrIn, nInpVar, inpVarTypes, lenXmlStr)
!            END IF
!            ! handle errors when reading variables.cfg file
!            IF ( retVal .LT. 0 ) THEN
!                CALL ShowSevereError('ExternalInterface: Error when getting input and output variables for EnergyPlus,')
!                CALL ShowContinueError('check simulation.log for error message.')
!                ErrorsFound = .true.
!            END IF
!        ELSE
!            CALL ShowSevereError('ExternalInterface: Did not find file "'//simCfgFilNam//'".')
!            CALL ShowContinueError('This file needs to be in same directory as in.idf.')
!            CALL ShowContinueError('Check the documentation for the ExternalInterface.')
!            ErrorsFound = .true.
!        END IF
!        CALL StopExternalInterfaceIfError

!        IF ( nOutVal + nInpVar .GT. maxVar ) THEN
!            CALL ShowSevereError('ExternalInterface: Too many variables to be exchanged.')
!            CALL ShowContinueError('Attempted to exchange '//TRIM(TrimSigDigits(nOutVal))//' outputs')
!            CALL ShowContinueError('plus '//TRIM(TrimSigDigits(nOutVal))//' inputs.')
!            CALL ShowContinueError('Maximum allowed is sum is '//TRIM(TrimSigDigits(maxVar))//'.')
!            CALL ShowContinueError('To fix, increase maxVar in ExternalInterface.f90')
!            ErrorsFound = .true.
!        END IF
!        CALL StopExternalInterfaceIfError
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        !!!!!!!!!!!!!!!!!!!!!
!        IF ( nOutVal .LT. 0 ) THEN
!            CALL ShowSevereError('ExternalInterface: Error when getting number of xml values for outputs.')
!            ErrorsFound = .true.
!        ELSE
!            CALL ParseString(xmlStrOut, varNames, nOutVal)
!            CALL ParseString(xmlStrOutTyp, varkeys, nOutVal)
!        END IF
!        CALL StopExternalInterfaceIfError

!        IF ( nInpVar .LT. 0 ) THEN
!            CALL ShowSevereError('ExternalInterface: Error when getting number of xml values for inputs.')
!            ErrorsFound = .true.
!        ELSE
!            CALL ParseString(xmlStrIn, inpVarNames, nInpVar)
!        END IF
!        CALL StopExternalInterfaceIfError

!        CALL DisplayString('Number of outputs in ExternalInterface = '//TrimSigDigits(nOutVal))
!        CALL DisplayString('Number of inputs  in ExternalInterface = '//TrimSigDigits(nInpVar))

!        FirstCall = .FALSE.

!    ELSEIF (.NOT.ConfiguredControlPoints) THEN
!        ALLOCATE(keyVarIndexes( nOutVal ))
!        ALLOCATE(varTypes( nOutVal ))
!        CALL GetReportVariableKey(varKeys, nOutVal, varNames, keyVarIndexes, varTypes)
!        ALLOCATE(varInd( nInpVar ))
!        DO i=1, nInpVar
!            IF (inpVarTypes(i) .EQ. indexSchedule) THEN
!                varInd(i) = GetDayScheduleIndex(inpVarNames(i))
!            ELSEIF (inpVarTypes(i) .EQ. indexVariable) THEN
!                varInd(i) = FindEMSVariable(inpVarNames(i), 0)
!            ELSEIF (inpVarTypes(i) .EQ. indexActuator) THEN
!                varInd(i) = FindEMSVariable(inpVarNames(i), 0)
!            END IF
!            IF (varInd(i) .LE. 0) THEN
!                CALL ShowSevereError('ExternalInterface: Error, xml file "' // simCfgFilNam // '" declares variable "' &
!                //TRIM(inpVarNames(i))//'",')
!                CALL ShowContinueError('but variable was not found in idf file.')
!                ErrorsFound = .true.
!            END IF
!        ENDDO
!        CALL StopExternalInterfaceIfError
!        ! Configure Erl variables
!        DO i=1, nInpVar
!            IF (inpVarTypes(i) .EQ. indexVariable) THEN ! ems-globalvariable
!                useEMS = .true.
!                IF ( .NOT. isExternalInterfaceErlVariable(varInd(i)) ) THEN
!                    CALL ShowSevereError('ExternalInterface: Error, xml file "' // simCfgFilNam // '" declares variable "' &
!                    //TRIM(inpVarNames(i))//'",')
!                    CALL ShowContinueError('But this variable is an ordinary Erl variable, not an ExternalInterface variable.')
!                    CALL ShowContinueError('You must specify a variable of type "ExternalInterface:Variable".')
!                    ErrorsFound = .true.
!                ENDIF
!            ELSEIF (inpVarTypes(i) .EQ. indexActuator) THEN ! ems-actuator
!                useEMS = .true.
!                IF ( .NOT. isExternalInterfaceErlVariable(varInd(i)) ) THEN
!                    CALL ShowSevereError('ExternalInterface: Error, xml file "' // simCfgFilNam // '" declares variable "' &
!                    //TRIM(inpVarNames(i))//'",')
!                    CALL ShowContinueError('But this variable is an ordinary Erl actuator, not an ExternalInterface actuator.')
!                    CALL ShowContinueError('You must specify a variable of type "ExternalInterface:Actuator".')
!                    ErrorsFound = .true.
!                ENDIF
!            END IF
!        ENDDO
!        ConfiguredControlPoints = .TRUE.
!    END IF
!    CALL StopExternalInterfaceIfError
!    RETURN

!    END SUBROUTINE InitExternalInterface

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !SUBROUTINE GetSetVariablesAndDoStepFMUImport()
!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   08Aug2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This routine gets, sets and does the time integration in FMUs.

!    ! METHODOLOGY EMPLOYED:

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE RuntimeLanguageProcessor, ONLY: isExternalInterfaceErlVariable, FindEMSVariable
!    USE DataInterfaces, ONLY:GetInternalVariableValueExternalInterface, GetInternalVariableValue
!    USE ScheduleManager, ONLY: ExternalInterfaceSetSchedule
!    USE RuntimeLanguageProcessor, ONLY: ExternalInterfaceSetErlVariable
!    USE EMSManager, ONLY: ManageEMS
!    USE DataGlobals, ONLY: WarmupFlag, KindOfSim, ksRunPeriodWeather, emsCallFromExternalInterface


!    USE ISO_C_BINDING, ONLY : C_PTR
!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    INTEGER, PARAMETER :: IntegerVar = 1      ! Integer variable
!    INTEGER, PARAMETER :: RealVar = 2         ! Real variable
!    LOGICAL, SAVE      :: FirstCallGetSetDoStep = .TRUE.  ! Flag to check when External Interface is called first time
!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

!    INTEGER :: i, j, k       ! Loop counter


!    INTERFACE
!    INTEGER FUNCTION fmiGetReal(fmiComponent, valRef, &
!    fmuVariableValue, numOutputs, index) BIND (C, NAME="fmiEPlusGetReal")
!    ! Function called to get real values from FMU outputs
!    USE ISO_C_BINDING, ONLY: C_INT, C_PTR, C_DOUBLE
!    TYPE (C_PTR)                         :: fmiComponent                 ! Pointer to FMU instance
!    INTEGER(kind=C_INT), DIMENSION(*)    :: valRef                       ! Parameter fmiValueReference
!    REAL (kind=C_DOUBLE), DIMENSION(*)   :: fmuVariableValue             ! FMU output variables
!    INTEGER(kind=C_INT)                  :: numOutputs                   ! Number of input variables
!    INTEGER(kind=C_INT)                  :: index                        ! Index of the FMU
!    END FUNCTION fmiGetReal
!    END INTERFACE


!    INTERFACE
!    INTEGER FUNCTION fmiSetReal(fmiComponent, valRef, &
!    fmuVariableValue, numInputs, index) BIND (C, NAME="fmiEPlusSetReal")
!    ! Function called to set real values to FMU inputs
!    USE ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
!    TYPE (C_PTR)                              :: fmiComponent                ! Pointer to FMU instance
!    INTEGER(kind=C_INT) , DIMENSION(*)        :: valRef                      ! Parameter fmiValueReference
!    REAL (kind=C_DOUBLE), DIMENSION(*)        :: fmuVariableValue            ! FMU input variables
!    INTEGER(kind=C_INT)                       :: numInputs                   ! Number of input variables
!    INTEGER(kind=C_INT)                       :: index                        ! Index of the FMU
!    END FUNCTION fmiSetReal
!    END INTERFACE

!    INTERFACE
!    INTEGER FUNCTION fmiDoStep(fmiComponent, curCommPoint, &
!    commStepSize, newStep, index) BIND (C, NAME="fmiEPlusDoStep")
!    !  Function called to do one step of the co-simulation
!    USE ISO_C_BINDING, ONLY: C_INT, C_PTR, C_DOUBLE
!    TYPE (C_PTR)                               :: fmiComponent               ! Pointer to FMU instance
!    REAL(kind=C_DOUBLE)                        :: curCommPoint               ! Current communication point
!    REAL(kind=C_DOUBLE)                        :: commStepSize               ! Communication step size
!    INTEGER (C_INT)                            :: newStep
!    INTEGER(kind=C_INT)                        :: index                        ! Index of the FMU
!    END FUNCTION fmiDoStep
!    END INTERFACE

!    DO i = 1, NumFMUObjects
!        DO j = 1, FMU(i)%NumInstances
!            IF (FlagReIni) THEN
!                ! Get from FMUs, values that will be set in EnergyPlus (Schedule)
!                DO k = 1, FMUTemp(i)%Instance(j)%NumOutputVariablesSchedule
!                    FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%RealVarValue = &
!                    FMUTemp(i)%Instance(j)%fmuOutputVariableSchedule(k)%RealVarValue
!                END DO

!                ! Get from FMUs, values that will be set in EnergyPlus (Variable)
!                DO k = 1, FMUTemp(i)%Instance(j)%NumOutputVariablesVariable
!                    FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%RealVarValue = &
!                    FMUTemp(i)%Instance(j)%fmuOutputVariableVariable(k)%RealVarValue
!                END DO

!                ! Get from FMUs, values that will be set in EnergyPlus (Actuator)
!                DO k = 1, FMUTemp(i)%Instance(j)%NumOutputVariablesActuator
!                    FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%RealVarValue = &
!                    FMUTemp(i)%Instance(j)%fmuOutputVariableActuator(k)%RealVarValue
!                END DO
!            ELSE
!                ! Get from FMUs, values that will be set in EnergyPlus (Schedule)

!                FMU(i)%Instance(j)%fmiStatus = &
!                fmiGetReal (FMU(i)%Instance(j)%fmiComponent, &
!                FMU(i)%Instance(j)%fmuOutputVariableSchedule%ValueReference, &
!                FMU(i)%Instance(j)%fmuOutputVariableSchedule%RealVarValue, &
!                FMU(i)%Instance(j)%NumOutputVariablesSchedule, &
!                FMU(i)%Instance(j)%Index)

!                IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                    CALL ShowSevereError('ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs')
!                    CALL ShowContinueError ('in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError ('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF


!                ! Get from FMUs, values that will be set in EnergyPlus (Variable)
!                FMU(i)%Instance(j)%fmiStatus = &
!                fmiGetReal (FMU(i)%Instance(j)%fmiComponent, &
!                FMU(i)%Instance(j)%fmuOutputVariableVariable%ValueReference, &
!                FMU(i)%Instance(j)%fmuOutputVariableVariable%RealVarValue, &
!                FMU(i)%Instance(j)%NumOutputVariablesVariable, &
!                FMU(i)%Instance(j)%Index)

!                IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                    CALL ShowSevereError('ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs')
!                    CALL ShowContinueError ('in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError ('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!                ! Get from FMUs, values that will be set in EnergyPlus (Actuator)
!                FMU(i)%Instance(j)%fmiStatus = &
!                fmiGetReal (FMU(i)%Instance(j)%fmiComponent, &
!                FMU(i)%Instance(j)%fmuOutputVariableActuator%ValueReference, &
!                FMU(i)%Instance(j)%fmuOutputVariableActuator%RealVarValue, &
!                FMU(i)%Instance(j)%NumOutputVariablesActuator, &
!                FMU(i)%Instance(j)%Index)

!                IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                    CALL ShowSevereError('ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to get outputs')
!                    CALL ShowContinueError ('in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError ('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF
!            END IF

!            ! Set in EnergyPlus the values of the schedules
!            DO k =1,  FMU(i)%Instance(j)%NumOutputVariablesSchedule
!                CALL ExternalInterfaceSetSchedule(FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%VarIndex, &
!                FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%RealVarValue)
!            END DO

!            ! Set in EnergyPlus the values of the variables
!            DO k =1,  FMU(i)%Instance(j)%NumOutputVariablesVariable
!                CALL ExternalInterfaceSetErlVariable(FMU(i)%Instance(j)%eplusInputVariableVariable(k)%VarIndex, &
!                FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%RealVarValue)
!            END DO

!            ! Set in EnergyPlus the values of the actuators
!            DO k =1,  FMU(i)%Instance(j)%NumOutputVariablesActuator
!                CALL ExternalInterfaceSetErlVariable(FMU(i)%Instance(j)%eplusInputVariableActuator(k)%VarIndex, &
!                FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%RealVarValue)
!            END DO

!            IF (FirstCallGetSetDoStep) THEN
!                ! Get from EnergyPlus, values that will be set in fmus
!                DO k = 1, FMU(i)%Instance(j)%NumInputVariablesInIDF
!                    !This make sure that the variables are updated at the Zone Time Step
!                    FMU(i)%Instance(j)%eplusOutputVariable(k)%RTSValue = &
!                    GetInternalVariableValue(FMU(i)%Instance(j)%eplusOutputVariable(k)%VarType, &
!                    FMU(i)%Instance(j)%eplusOutputVariable(k)%VarIndex)
!                END DO
!            ELSE
!                ! Get from EnergyPlus, values that will be set in fmus
!                DO k = 1, FMU(i)%Instance(j)%NumInputVariablesInIDF
!                    !This make sure that the variables are updated at the Zone Time Step
!                    FMU(i)%Instance(j)%eplusOutputVariable(k)%RTSValue = &
!                    GetInternalVariableValueExternalInterface(FMU(i)%Instance(j)%eplusOutputVariable(k)%VarType, &
!                    FMU(i)%Instance(j)%eplusOutputVariable(k)%VarIndex)
!                END DO
!            END IF

!            IF (.NOT.FlagReIni) THEN
!                FMU(i)%Instance(j)%fmiStatus = &
!                fmiSetReal(FMU(i)%Instance(j)%fmiComponent, &
!                FMU(i)%Instance(j)%fmuInputVariable%ValueReference, &
!                FMU(i)%Instance(j)%eplusOutputVariable%RTSValue, FMU(i)%Instance(j)%NumInputVariablesInIDF, &
!                FMU(i)%Instance(j)%Index)
!                IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                    CALL ShowSevereError('ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to set inputs')
!                    CALL ShowContinueError ('in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError ('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF
!            END IF
!            !   END IF
!            ! END DO

!            ! Call and simulate the FMUs to get values at the corresponding timestep.
!            FMU(i)%Instance(j)%fmiStatus = &
!            fmiDoStep(FMU(i)%Instance(j)%fmiComponent, &
!            tComm, hStep, fmiTrue, FMU(i)%Instance(j)%Index)
!            IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                CALL ShowSevereError('ExternalInterface/GetSetVariablesAndDoStepFMUImport: Error when trying to')
!                CALL ShowContinueError('do the coSimulation with instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'".')
!                CALL ShowContinueError('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                ErrorsFound = .true.
!                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                CALL StopExternalInterfaceIfError
!            END IF
!        END DO
!    END DO
!    ! If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
!    IF (useEMS) THEN
!        CALL ManageEMS(emsCallFromExternalInterface)
!    END IF
!    FirstCallGetSetDoStep = .FALSE.

!    END SUBROUTINE GetSetVariablesAndDoStepFMUImport


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE InstantiateInitializeFMUImport()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   08Aug2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This routine instantiates and initializes FMUs.

!    ! METHODOLOGY EMPLOYED:

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:

!    USE ISO_C_BINDING, ONLY : C_PTR, C_ASSOCIATED
!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

!    INTEGER :: i, j         ! Loop counter

!    INTERFACE
!    TYPE (C_PTR) FUNCTION fmiInstantiateSlave (fmuWorkFolder, sizeFmuWorkFolder, &
!    timeOut, & visible, interactive, loggingon, index) BIND (C, NAME="fmiEPlusInstantiateSlave")
!    ! Function called to Instantiate FMU

!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR, C_PTR, C_DOUBLE
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkFolder                 ! FMU resource folder
!    INTEGER(kind=C_INT)                  :: sizeFmuWorkFolder             ! Size of the FMU resource folder trimmed
!    REAL(kind = C_DOUBLE)                :: timeOut                      ! timeOut in milli seconds
!    INTEGER(C_INT)                       :: visible !
!    INTEGER(C_INT)                       :: interactive !
!    INTEGER(C_INT)                       :: loggingon !
!    INTEGER(kind=C_INT)                  :: index                        ! Index of FMU
!    END FUNCTION fmiInstantiateSlave
!    END INTERFACE

!    INTERFACE
!    INTEGER FUNCTION fmiInitializeSlave(fmiComponent, tStart, fmiTrue, tStop, index)BIND (C, NAME="fmiEPlusInitializeSlave")
!    ! Function called to initialize FMU
!    USE ISO_C_BINDING, ONLY: C_INT, C_PTR, C_DOUBLE
!    TYPE (C_PTR)                         :: fmiComponent                 ! Pointer to FMU instance
!    REAL(kind=C_DOUBLE)                  :: tStart                       ! Starttime for co-simulation
!    REAL(kind=C_DOUBLE)                  :: tStop                        ! Stoptime for co-simulation
!    INTEGER (kind = C_INT)               :: fmiTrue                      ! Flag set to true for initialization
!    INTEGER(kind=C_INT)                  :: index                        ! Index of FMU 
!    END FUNCTION fmiInitializeSlave
!    END INTERFACE

!    !Instantiate FMUs
!    DO i = 1, NumFMUObjects
!        DO j = 1, FMU(i)%NumInstances
!            FMU(i)%Instance(j)%fmiComponent = &
!            fmiInstantiateSlave(FMU(i)%Instance(j)%WorkingFolder, &
!            FMU(i)%Instance(j)%LenWorkingFolder, FMU(i)%TimeOut, FMU(i)%Visible, &
!            FMU(i)%Interactive, FMU(i)%LoggingOn, FMU(i)%Instance(j)%Index)
!            IF (.NOT.(C_ASSOCIATED(FMU(i)%Instance(j)%fmiComponent))) THEN
!                CALL ShowSevereError('ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to instantiate')
!                CALL ShowContinueError('instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                ErrorsFound = .true.
!                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                CALL StopExternalInterfaceIfError
!            END IF
!        END DO
!    END DO

!    ! Initialize FMUs
!    DO i = 1, NumFMUObjects
!        DO j = 1, FMU(i)%NumInstances
!            FMU(i)%Instance(j)%fmiStatus = fmiInitializeSlave(FMU(i)%Instance(j)%fmiComponent, &
!            tStart, fmiTrue, tStop, FMU(i)%Instance(j)%Index)
!            IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus .EQ. fmiOK )) THEN
!                CALL ShowSevereError('ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to initialize')
!                CALL ShowContinueError('instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                CALL ShowContinueError('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                ErrorsFound = .true.
!                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                CALL StopExternalInterfaceIfError
!            END IF
!        END DO
!    END DO
!    END SUBROUTINE InstantiateInitializeFMUImport


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE InitializeFMU()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   08Aug2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This routine reinitializes FMUs.

!    ! METHODOLOGY EMPLOYED:

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

!    INTEGER :: i, j         ! Loop counter

!    INTERFACE
!    INTEGER FUNCTION fmiInitializeSlave(fmiComponent, tStart, &
!    fmiTrue, tStop, index)BIND (C, NAME="fmiEPlusInitializeSlave")
!    ! Function called to initialize FMU
!    USE ISO_C_BINDING, ONLY: C_INT, C_PTR, C_DOUBLE
!    TYPE (C_PTR)                         :: fmiComponent                 ! Pointer to FMU instance
!    REAL(kind=C_DOUBLE)                  :: tStart                       ! Starttime for co-simulation
!    REAL(kind=C_DOUBLE)                  :: tStop                        ! Stoptime for co-simulation
!    INTEGER (kind = C_INT)               :: fmiTrue                      ! Flag set to true for initialization
!    INTEGER(kind=C_INT)                  :: index                        ! Index of FMU
!    END FUNCTION fmiInitializeSlave
!    END INTERFACE

!    ! Initialize FMUs
!    DO i = 1, NumFMUObjects
!        DO j = 1, FMU(i)%NumInstances
!            FMU(i)%Instance(j)%fmiStatus = fmiInitializeSlave(FMU(i)%Instance(j)%fmiComponent, &
!            tStart, fmiTrue, tStop, FMU(i)%Instance(j)%Index)

!            IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus .EQ. fmiOK )) THEN
!                CALL ShowSevereError('ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying to initialize')
!                CALL ShowContinueError('instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                CALL ShowContinueError('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                ErrorsFound = .true.
!                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                CALL StopExternalInterfaceIfError
!            END IF
!        END DO
!    END DO
!    END SUBROUTINE InitializeFMU

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE TerminateResetFreeFMUImport()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   08Aug2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This routine terminates the FMUs instances

!    ! METHODOLOGY EMPLOYED:

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:

!    USE ISO_C_BINDING, ONLY : C_ASSOCIATED
!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER :: i, j, k         ! Loop counter

!    INTERFACE
!    INTEGER(C_INT) FUNCTION fmiFreeSlaveInstance(fmiComponent, index) BIND (C, NAME="fmiEPlusFreeSlave")
!    ! Function called to free FMU
!    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
!    TYPE (C_PTR)                         :: fmiComponent                 ! Pointer to FMU instance
!    INTEGER(kind=C_INT)                  :: index                        ! Index of FMU 
!    END FUNCTION fmiFreeSlaveInstance
!    END INTERFACE
!    !----Needs to have function that allows to terminates FMU. Was not defined in version 1.0 -- fixme
!    DO i = 1, NumFMUObjects
!        DO j = 1, FMU(i)%NumInstances
!            IF (FMU(i)%Instance(j)%fmiStatus .NE. fmiFatal) THEN
!                ! Cleanup slaves
!                FMU(i)%Instance(j)%fmiStatus = &
!                fmiFreeSlaveInstance(FMU(i)%Instance(j)%fmiComponent, FMU(i)%Instance(j)%Index)
!            END IF
!            ! check if fmiComponent has been freed
!            IF (.NOT.(C_ASSOCIATED(FMU(i)%Instance(j)%fmiComponent))) THEN
!                CALL ShowSevereError('ExternalInterface/TerminateResetFreeFMUImport: Error when trying to terminate')
!                CALL ShowContinueError('instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                ErrorsFound = .true.
!                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                CALL StopExternalInterfaceIfError
!            END IF
!        END DO
!    END DO

!    END SUBROUTINE TerminateResetFreeFMUImport

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE InitExternalInterfaceFMUImport()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   08Aug2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This routine initializes the input and outputs variables used for the co-simulation with FMUs.

!    ! METHODOLOGY EMPLOYED:

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItem
!    USE DataInterfaces, ONLY:GetVariableKeyCountandType, GetVariableKeys
!    USE ScheduleManager, ONLY: GetDayScheduleIndex
!    USE RuntimeLanguageProcessor, ONLY: isExternalInterfaceErlVariable, FindEMSVariable
!    USE DataIPShortCuts
!    USE ISO_C_BINDING, ONLY : C_PTR
!    USE DataSystemVariables, ONLY: CheckForActualFileName

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:
!    INTERFACE
!    INTEGER(C_INT) FUNCTION getfmiVersion(fmuWorkingFolder, sizefmuWorkingFolder, &
!    fmiVersionNumber, index) BIND (C, NAME="getfmiEPlusVersion")
!    ! Function called to get FMI version number
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkingFolder         ! FMU working folder
!    INTEGER(kind=C_INT)                  :: sizefmuWorkingFolder     ! Size of the fmuWorkingFolder trimmed
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmiVersionNumber         ! FMI version number
!    INTEGER(kind=C_INT)                  :: index                    ! Index of FMU
!    END FUNCTION getfmiVersion
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION fmuUnpack(fmuName, fmuWorkingFolder, sizefmuName, sizefmuWorkingFolder) &
!    BIND (C, NAME="fmiEPlusUnpack")
!    ! Unpack the FMU
!    USE ISO_C_BINDING, ONLY: C_INT, C_CHAR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuName                  ! FMU name
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkingFolder         ! FMU working folder
!    INTEGER(kind=C_INT)                  :: sizefmuName              ! Size of the fmuName trimmed
!    INTEGER(kind=C_INT)                  :: sizefmuWorkingFolder     ! Size of the fmuWorkingFolder trimmed
!    END FUNCTION fmuUnpack
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getValueReferenceByNameFMUInputVariables(variableName, sizevariableName, index) & 
!    BIND (C, NAME="getValueReferenceByNameFMUInputVariables")
!    ! Function called to get value reference by name of FMU input variables
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT
!    CHARACTER(C_CHAR), DIMENSION(*)      :: variableName             ! FMU variable name
!    INTEGER(kind=C_INT)                  :: sizevariableName         ! Size of the fmuVariableName trimmed
!    INTEGER(kind=C_INT)                  :: index                    ! Index of FMU
!    END FUNCTION getValueReferenceByNameFMUInputVariables
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION getValueReferenceByNameFMUOutputVariables(variableName, sizevariableName, index) &
!    BIND (C, NAME="getValueReferenceByNameFMUOutputVariables")
!    ! Function called to get value reference by name of FMU output variables
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT
!    CHARACTER(C_CHAR), DIMENSION(*)      :: variableName             ! FMU variable name
!    INTEGER(kind=C_INT)                  :: sizevariableName         ! Size of the fmuVariableName trimmed
!    INTEGER(kind=C_INT)                  :: index                    ! Index of FMU
!    END FUNCTION getValueReferenceByNameFMUOutputVariables
!    END INTERFACE

!    INTERFACE
!    INTEGER (C_INT) FUNCTION addFMURootFolderName(fmuOutputWorkingFolder, fmuWorkingFolder, sizefmuWorkingFolder) &
!    BIND (C, NAME="addFMURootFolderName")
!    ! Function called to build path to folder where FMU is unpacked
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_PTR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuOutputWorkingFolder   ! FMU output working folder
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkingFolder         ! FMU working folder
!    INTEGER(kind=C_INT)                  :: sizefmuWorkingFolder     ! Size of the fmuWorkingFolder trimmed
!    END FUNCTION addFMURootFolderName
!    END INTERFACE


!    INTERFACE
!    INTEGER (C_INT) FUNCTION model_ID_GUID(fmuWorkingFolder, sizefmuWorkingFolder,&
!    numInputs, numOutputs) BIND (C, NAME="model_ID_GUID")
!    ! Function called to get model ID as well as model GUID
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT, C_PTR
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkingFolder         ! FMU working folder
!    INTEGER(kind=C_INT)                  :: sizefmuWorkingFolder     ! Size of the fmuWorkingFolder trimmed
!    INTEGER(kind=C_INT)                  :: numInputs                ! Number of FMU inputs
!    INTEGER(kind=C_INT)                  :: numOutputs               ! Number of FMU outputs
!    END FUNCTION model_ID_GUID
!    END INTERFACE

!    INTERFACE
!    INTEGER(C_INT) FUNCTION addLibPathCurrentWorkingFolder(fmuWorkingFolder_wLib, fmuWorkingFolder, &
!    sizefmuWorkingFolder, index) BIND (C, NAME="addLibPathCurrentWorkingFolder")
!    ! Function called to build path to the .dll/.so of the FMU
!    USE ISO_C_BINDING, ONLY: C_CHAR, C_INT
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkingFolder_wLib    ! Path to binaries
!    CHARACTER(kind=C_CHAR), DIMENSION(*) :: fmuWorkingFolder         ! FMU working folder
!    INTEGER(kind=C_INT)                  :: sizefmuWorkingFolder     ! Size of the fmuWorkingFolder trimmed
!    INTEGER(kind=C_INT)                  :: index                    ! Index of fmu
!    END FUNCTION addLibPathCurrentWorkingFolder
!    END INTERFACE

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER :: i, j, k, l, Loop         ! Loop counter
!    INTEGER                        :: retVal              ! Return value of function call, used for error handling
!    INTEGER                        :: NumAlphas  = 0      ! Number of Alphas for each GetObjectItem call
!    INTEGER                        :: NumNumbers = 0      ! Number of Numbers for each GetObjectItem call
!    INTEGER                        :: IOStatus = 0        ! Used in GetObjectItem
!    INTEGER                        :: NumFMUInputVariables = 0     ! Number of FMU input variables
!    INTEGER                        :: varType      = 0 ! 0=not found, 1=integer, 2=real, 3=meter
!    INTEGER                        :: numKey       = 0 ! Number of keys found
!    INTEGER                        :: varAvgSum    = 0 ! Variable  is Averaged=1 or Summed=2
!    INTEGER                        :: varStepType  = 0 ! Variable time step is Zone=1 or HVAC=2
!    CHARACTER(len=10)              :: varUnit         ! Units sting, may be blank
!    CHARACTER(len=1000)            :: Name_NEW         ! Units sting, may be blank
!    CHARACTER(len=1000)            :: Name_OLD         ! Units sting, may be blank

!    INTEGER, DIMENSION(1):: keyIndexes ! Array index for
!    INTEGER, DIMENSION(1):: varTypes ! Array index for
!    !INTEGER :: keyIndex ! Array index for
!    CHARACTER(len=MaxNameLength), DIMENSION(1):: NamesOfKeys      ! Specific key name
!    INTEGER :: retValue
!    INTEGER :: retValfmiVersion
!    INTEGER :: retValfmiPathLib
!    CHARACTER(len=MaxNameLength), DIMENSION(5)::           NameListInstances
!    LOGICAL IsNotOK
!    LOGICAL IsBlank
!    LOGICAL, SAVE :: FirstCallIni= .TRUE.  ! First time, input has been read
!    LOGICAL :: fileExist
!    CHARACTER(len=300) :: tempFullFileName
!    CHARACTER(len=MaxNameLength),ALLOCATABLE,DIMENSION(:) :: strippedFileName   ! remove path from entered file name
!    CHARACTER(len=300),ALLOCATABLE,DIMENSION(:) :: fullFileName   ! entered file name/found
!    INTEGER :: pos
!    INTEGER :: FOUND

!    IF (FirstCallIni) THEN
!        CALL DisplayString('Initializing FunctionalMockupUnitImport interface')
!        ! do one time initializations
!        CALL ValidateRunControl
!        ALLOCATE(FMU(NumFMUObjects))

!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Add the fmus root folder name to the current working folder /currentWorkingFolder/tmp-fmus/... (9-characters)
!        LEN_FMU_ROOT_DIR = LEN_TRIM(CurrentWorkingFolder) + 9
!        retValue = addFMURootFolderName(FMURootWorkingFolder, TRIM(CurrentWorkingFolder), LEN(TRIM(CurrentWorkingFolder)))
!        IF (retValue .NE. 0 ) THEN
!            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: FMU root folder')
!            CALL ShowContinueError('could not be added to working directory.')
!            ErrorsFound = .true.
!            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!            CALL StopExternalInterfaceIfError
!        END IF
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Get and store the names of all FMUs in EnergyPlus data structure
!        ALLOCATE(strippedFileName(NumFMUObjects))
!        strippedFileName=' '
!        ALLOCATE(fullFileName(NumFMUObjects))
!        fullFileName=' '
!        cCurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport'
!        DO Loop = 1, NumFMUObjects
!            CALL GetObjectItem(cCurrentModuleObject, Loop, cAlphaArgs, &
!            NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!            AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!            ! Get the FMU name
!            FMU(Loop)%Name  = cAlphaArgs(1)
!            CALL CheckForActualFileName(cAlphaArgs(1),fileExist,tempFullFileName)
!            IF (fileExist) THEN
!                pos=INDEX(FMU(Loop)%Name,PathChar,.true.)  ! look backwards
!                IF (pos > 0) THEN
!                    strippedFileName(Loop)=FMU(Loop)%Name(pos+1:)
!                ELSE  ! pos == 0, look for alt path char
!                    pos=INDEX(FMU(Loop)%Name,AltPathChar,.true.)  ! look backwards
!                    IF (pos > 0) THEN
!                        strippedFileName(Loop)=FMU(Loop)%Name(pos+1:)
!                    ELSE
!                        strippedFileName(Loop)=FMU(Loop)%Name
!                    ENDIF
!                ENDIF
!                fullFileName(Loop)=tempFullFileName
!            ELSE
!                CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport:')
!                CALL ShowContinueError('file not located="'//trim(cAlphaArgs(1))//'".')
!                ErrorsFound=.true.
!            ENDIF
!            ! Get fmu time out
!            FMU(Loop)%TimeOut  = rNumericArgs(1)
!            ! Get fmu logging on
!            FMU(Loop)%LoggingOn = rNumericArgs(2)
!        END DO

!        ! check for dups that aren't the same file
!        ! this is windows code...
!        DO j=1,NumFMUObjects
!            DO k=2,NumFMUObjects
!                IF (.not. SameString(strippedFileName(j),strippedFileName(k))) CYCLE
!                ! base file names are the same
!                IF (SameString(fullFileName(j),fullFileName(k))) CYCLE
!                CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport:')
!                CALL ShowContinueError('duplicate file names (but not same file) entered.')
!                CALL ShowContinueError('...entered file name="'//trim(FMU(j)%Name)//'"')
!                CALL ShowContinueError('...   full file name="'//trim(fullFileName(j))//'"')
!                CALL ShowContinueError('...entered file name="'//trim(FMU(k)%Name)//'"')
!                CALL ShowContinueError('...   full file name="'//trim(fullFileName(k))//'"')
!                CALL ShowContinueError('...name collision but not same file name.')
!                ErrorsFound=.true.
!            ENDDO
!        ENDDO
!        IF (ErrorsFound) THEN
!            DEALLOCATE(strippedFileName)
!            DEALLOCATE(fullFileName)
!            CALL StopExternalInterfaceIfError
!        ENDIF

!        ! get the names of the input variables each fmu(and the names of the
!        ! corresponding output variables in EnergyPlus --).
!        cCurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport:From:Variable'
!        NumFMUInputVariables = GetNumObjectsFound(cCurrentModuleObject)
!        ! Determine the number of instances for each FMUs
!        DO i =1, NumFMUObjects
!            Name_New = ""
!            Name_OLD = ""
!            j = 1
!            k = 1
!            ALLOCATE(FMU(i)%Instance(NumFMUInputVariables))
!            ALLOCATE(checkInstanceName(NumFMUInputVariables))
!            DO l = 1, NumFMUInputVariables
!                CALL GetObjectItem(cCurrentModuleObject, l, cAlphaArgs, &
!                NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                IF (SameString(cAlphaArgs(3), FMU(i)%Name)) THEN
!                    Name_NEW = cAlphaArgs(4)
!                    IF(.NOT. SameString(Name_OLD, Name_NEW)) THEN
!                        FOUND = FindItem (Name_New, checkInstanceName%Name, NumFMUInputVariables)
!                        IF (FOUND == 0) THEN
!                            checkInstanceName(l)%Name = Name_New
!                            FMU(i)%NumInstances = j
!                            FMU(i)%Instance(j)%Name = Name_New
!                            j = j+1
!                            Name_OLD = Name_NEW
!                        ENDIF
!                    END IF
!                    FMU(i)%TotNumInputVariablesInIDF = k
!                    k = k+1
!                END IF
!            END DO
!            DEALLOCATE(checkInstanceName)
!        END DO

!        DO i = 1, NumFMUObjects
!            IF (FMU(i)%NumInstances .EQ. 0 ) THEN
!                CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: The FMU "'//TRIM(FMU(i)%Name)//'" does')
!                CALL ShowContinueError('not have any instances or any input variable. An FMU should have at least one instance')
!                CALL ShowContinueError('or one input variable defined in input file. Check FMU object in the input file.')
!                ErrorsFound = .true.
!                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                CALL StopExternalInterfaceIfError
!            END IF
!            IF (NumFMUInputVariables .GT. 0 .AND. FMU(i)%TotNumInputVariablesInIDF .EQ. 0 ) THEN
!                CALL ShowWarningError('InitExternalInterfaceFMUImport: The FMU "'//TRIM(FMU(i)%Name)//'"')
!                CALL ShowContinueError('is defined but has no input variables.')
!                CALL ShowContinueError('Check the input field of the corresponding object')
!                CALL ShowContinueError('ExternalInterface:FunctionalMockupUnitImport:From:Variable.')
!            END IF
!        END DO

!        ! write output folder where FMUs will be unpacked later on.
!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                FMU(i)%Instance(j)%WorkingFolder = TRIM(FMURootWorkingFolder(1:LEN_FMU_ROOT_DIR))&
!                //TRIM(strippedFileName(i))//'_'//TRIM(FMU(i)%Instance(j)%Name)
!            END DO
!        END DO

!        ! parse the fmu defined in the idf using the fmuUnpack.
!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                ! get the length of working folder trimmed
!                FMU(i)%Instance(j)%LenWorkingFolder=LEN_TRIM(FMU(i)%Instance(j)%WorkingFolder)
!                ! unpack fmus
!                retVal = fmuUnpack ( fullFileName(i),FMU(i)%Instance(j)%WorkingFolder, &
!                LEN_TRIM(fullFileName(i)), FMU(i)%Instance(j)%LenWorkingFolder)
!                IF ( retVal .NE. 0 ) THEN
!                    CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to')
!                    CALL ShowContinueError('unpack the FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError('Check if the FMU exists. Also check if the FMU folder is not write protected.')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!                ! determine modelID and modelGUID of all FMU instances
!                FMU(i)%Instance(j)%Index = model_ID_GUID (FMU(i)%Instance(j)%WorkingFolder, & 
!                FMU(i)%Instance(j)%LenWorkingFolder, & FMU(i)%Instance(j)%NumInputVariablesInFMU, &
!                FMU(i)%Instance(j)%NumOutputVariablesInFMU)
!                IF ( FMU(i)%Instance(j)%Index .LT. 0 ) THEN
!                    CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to')
!                    CALL ShowContinueError('get the model ID and model GUID')
!                    CALL ShowContinueError('of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError('Check if modelDescription.xml exists in the folder where the FMU has been unpacked.')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!                ! get the path to the binaries
!                retValfmiPathLib = addLibPathCurrentWorkingFolder (FMU(i)%Instance(j)%WorkingFolder_wLib, &
!                FMU(i)%Instance(j)%WorkingFolder, FMU(i)%Instance(j)%LenWorkingFolder, &
!                FMU(i)%Instance(j)%Index)
!                IF ( retValfmiPathLib .NE. 0 ) THEN
!                    CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to')
!                    CALL ShowContinueError('get the path to the binaries of instance')
!                    CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError('Check if binaries folder exists where the FMU has been unpacked.')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!                ! get the length of the working folder with libraries
!                FMU(i)%Instance(j)%LenWorkingFolder_wLib = LEN(TRIM(FMU(i)%Instance(j)%WorkingFolder_wLib))

!                ! determine the FMI version
!                retValfmiVersion = getfmiVersion (FMU(i)%Instance(j)%WorkingFolder_wLib,  &
!                FMU(i)%Instance(j)%LenWorkingFolder_wLib, FMU(i)%Instance(j)%fmiVersionNumber, &
!                FMU(i)%Instance(j)%Index)
!                IF ( retValfmiVersion .NE. 0 ) THEN
!                    CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to')
!                    CALL ShowContinueError('load FMI functions library of instance')
!                    CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmiVersionNumber)//'".')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!                IF (FMU(i)%Instance(j)%fmiVersionNumber(1:3) /= '1.0') THEN
!                    CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when getting version')
!                    CALL ShowContinueError('number of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                    CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'".')
!                    CALL ShowContinueError('The version number found ("'//TRIM(FMU(i)%Instance(j)%fmiVersionNumber(1:3))//'")')
!                    CALL ShowContinueError('differs from version 1.0 which is currently supported.')
!                    ErrorsFound = .true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!            END DO
!        END DO

!        DEALLOCATE(strippedFileName)
!        DEALLOCATE(fullFileName)

!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                ALLOCATE(FMU(i)%Instance(j)%fmuInputVariable(NumFMUInputVariables))
!                ALLOCATE(FMU(i)%Instance(j)%checkfmuInputVariable(NumFMUInputVariables))
!                ALLOCATE(FMU(i)%Instance(j)%eplusOutputVariable(NumFMUInputVariables))
!                k = 1
!                DO l = 1, NumFMUInputVariables
!                    CALL GetObjectItem(cCurrentModuleObject, l, cAlphaArgs, &
!                    NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                    IF (SameString(cAlphaArgs(3), FMU(i)%Name).AND.SameString(cAlphaArgs(4), FMU(i)%Instance(j)%Name)) THEN
!                        FMU(i)%Instance(j)%fmuInputVariable(k)%Name = cAlphaArgs(5)
!                        FMU(i)%Instance(j)%eplusOutputVariable(k)%VarKey = cAlphaArgs(1)
!                        FMU(i)%Instance(j)%eplusOutputVariable(k)%Name = cAlphaArgs(2)
!                        ! verify whether we have duplicate FMU input variables in the idf
!                        CALL verifyName(FMU(i)%Instance(j)%fmuInputVariable(k)%Name, &
!                        FMU(i)%Instance(j)%checkfmuInputVariable%Name, NumFMUInputVariables, IsNotOK,IsBlank,&
!                        'The FMU input variable "'//TRIM(FMU(i)%Instance(j)%fmuInputVariable(k)%Name)//&
!                        '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'" has duplicates. &
!                        &Please check the input file again and delete duplicated entries.')
!                        IF (IsNotOK) THEN
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ELSE
!                            FMU(i)%Instance(j)%checkfmuInputVariable(k)%Name   = FMU(i)%Instance(j)%fmuInputVariable(k)%Name
!                        ENDIF
!                        FMU(i)%Instance(j)%fmuInputVariable(k)%ValueReference = &
!                        getValueReferenceByNameFMUInputVariables(FMU(i)%Instance(j)%fmuInputVariable(k)%Name,&
!                        LEN (TRIM(FMU(i)%Instance(j)%fmuInputVariable(k)%Name)), FMU(i)%Instance(j)%Index)

!                        IF (FMU(i)%Instance(j)%fmuInputVariable(k)%ValueReference .EQ. -999) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to')
!                            CALL ShowContinueError('get the value reference of FMU input variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuInputVariable(k)%Name)//'" of instance '// &
!                            '"'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'". Please check the name of input variable')
!                            CALL ShowContinueError('in the input file and in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        IF (FMU(i)%Instance(j)%fmuInputVariable(k)%ValueReference .EQ. -1) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to')
!                            CALL ShowContinueError('get the value reference of FMU input variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuInputVariable(k)%Name)//'" of instance '// &
!                            '"'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU ')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Name)//'". This variable is not an FMU input variable.')
!                            CALL ShowContinueError('Please check the causality of the variable in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF
!                        CALL GetReportVariableKey(FMU(i)%Instance(j)%eplusOutputVariable(k)%VarKey, 1, &
!                        FMU(i)%Instance(j)%eplusOutputVariable(k)%Name, keyIndexes, varTypes)
!                        FMU(i)%Instance(j)%eplusOutputVariable(k)%VarIndex = keyIndexes(1)
!                        FMU(i)%Instance(j)%eplusOutputVariable(k)%VarType  = varTypes(1)
!                        FMU(i)%Instance(j)%NumInputVariablesInIDF = k
!                        k = k+1
!                    END IF
!                END DO
!                IF (NumFMUInputVariables .GT. 0 .AND. FMU(i)%Instance(j)%NumInputVariablesInIDF .EQ. 0 ) THEN
!                    CALL ShowWarningError('InitExternalInterfaceFMUImport: The instance "'//TRIM(FMU(i)%Instance(j)%Name)//  &
!                    '"of FMU "'//TRIM(FMU(i)%Name)//'"')
!                    CALL ShowContinueError('is defined but has no input variables. Check the input field of the')
!                    CALL ShowContinueError('corresponding object: ExternalInterface:FunctionalMockupUnitImport:From:Variable.')
!                END IF
!            END DO
!        END DO

!        DO i =1, NumFMUObjects
!            DO j=1, FMU(i)%NumInstances
!                ! check whether the number of input variables in fmu is bigger than in the idf
!                IF (FMU(i)%Instance(j)%NumInputVariablesInFMU .GT. FMU(i)%Instance(j)%NumInputVariablesInIDF) THEN
!                    CALL ShowSevereError('InitExternalInterfaceFMUImport: The number of input variables defined in input file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumInputVariablesInIDF))//')')
!                    CALL ShowContinueError('of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//  &
!                    '" is less than the number of input variables')
!                    CALL ShowContinueError('in the modelDescription file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumInputVariablesInFMU))//').')
!                    CALL ShowContinueError('Check the input file and the modelDescription file again.')
!                    ErrorsFound=.true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF
!                ! check whether the number of input variables in fmu is less than in the idf
!                IF (FMU(i)%Instance(j)%NumInputVariablesInFMU .LT. FMU(i)%Instance(j)%NumInputVariablesInIDF) THEN
!                    CALL ShowSevereError('InitExternalInterfaceFMUImport: The number of input variables defined in input file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumInputVariablesInIDF))//')')
!                    CALL ShowContinueError('of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)// &
!                    '" is bigger than the number of input variables')
!                    CALL ShowContinueError('in the modelDescription file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumInputVariablesInFMU))//').')
!                    CALL ShowContinueError('Check the input file and the modelDescription file again.')
!                    ErrorsFound=.true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF
!            END DO
!        END DO

!        ! get the names of the output variables each fmu (and the names of the
!        ! corresponding input variables in EnergyPlus -- schedule).
!        cCurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport:To:Schedule'
!        NumFMUInputVariables = GetNumObjectsFound(cCurrentModuleObject)

!        DO i =1, NumFMUObjects
!            j = 1
!            DO k = 1, NumFMUInputVariables
!                CALL GetObjectItem(cCurrentModuleObject, k, cAlphaArgs, &
!                NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                IF (SameString(cAlphaArgs(3), FMU(i)%Name)) THEN
!                    FMU(i)%TotNumOutputVariablesSchedule = j
!                    j = j+1
!                END IF
!            END DO
!        END DO

!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                ALLOCATE(FMU(i)%Instance(j)%fmuOutputVariableSchedule(NumFMUInputVariables))
!                ALLOCATE(FMU(i)%Instance(j)%eplusInputVariableSchedule(NumFMUInputVariables))
!                k = 1
!                DO l = 1, NumFMUInputVariables
!                    CALL GetObjectItem(cCurrentModuleObject, l, cAlphaArgs, &
!                    NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                    IF (SameString(cAlphaArgs(3), FMU(i)%Name).AND.SameString(cAlphaArgs(4), FMU(i)%Instance(j)%Name)) THEN
!                        FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%Name = cAlphaArgs(5)
!                        FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%Name = cAlphaArgs(1)
!                        FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%InitialValue = rNumericArgs(1)
!                        ! get the value reference by using the FMU name and the variable name.
!                        FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%ValueReference = &
!                        getValueReferenceByNameFMUOutputVariables(FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%Name, &
!                        LEN(TRIM(FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%Name)), FMU(i)%Instance(j)%Index)
!                        IF (FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%ValueReference .EQ. -999) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to '// &
!                            'get the value reference of the FMU output variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%Name)// &
!                            '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'" that will be mapped to a schedule.')
!                            CALL ShowContinueError('Please check the name of output variables in the input file and')
!                            CALL ShowContinueError('in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        IF (FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%ValueReference .EQ. -1) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying to '// &
!                            'get the value reference of the FMU output variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%Name)// &
!                            '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'" that will be mapped to a schedule.')
!                            CALL ShowContinueError('This variable is not an FMU output variable.')
!                            CALL ShowContinueError('Please check the causality of the variable in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%VarIndex = &
!                        GetDayScheduleIndex(FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%Name)
!                        FMU(i)%Instance(j)%NumOutputVariablesSchedule = k
!                        IF (FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%VarIndex .LE. 0) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport:declares variable "'// &
!                            TRIM(FMU(i)%Instance(j)%eplusInputVariableSchedule(k)%Name)//'",')
!                            CALL ShowContinueError('but variable is not a schedule variable.')
!                            ErrorsFound = .true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        END IF
!                        k = k+1
!                    END IF
!                END DO
!            END DO
!        END DO

!        ! get the names of the output variables each fmu (and the names of the
!        ! corresponding input variables in EnergyPlus -- variable).
!        cCurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport:To:Variable'
!        NumFMUInputVariables = GetNumObjectsFound(cCurrentModuleObject)

!        DO i =1, NumFMUObjects
!            j = 1
!            DO k = 1, NumFMUInputVariables
!                CALL GetObjectItem(cCurrentModuleObject, k, cAlphaArgs, &
!                NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                IF (SameString(cAlphaArgs(2), FMU(i)%Name)) THEN
!                    FMU(i)%TotNumOutputVariablesVariable = j
!                    j = j+1
!                END IF
!            END DO
!        END DO

!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                ALLOCATE(FMU(i)%Instance(j)%fmuOutputVariableVariable(NumFMUInputVariables))
!                ALLOCATE(FMU(i)%Instance(j)%eplusInputVariableVariable(NumFMUInputVariables))
!                k = 1
!                DO l = 1, NumFMUInputVariables
!                    CALL GetObjectItem(cCurrentModuleObject, l, cAlphaArgs, &
!                    NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                    IF (SameString(cAlphaArgs(2), FMU(i)%Name).AND.SameString(cAlphaArgs(3), FMU(i)%Instance(j)%Name)) THEN
!                        FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%Name = cAlphaArgs(4)
!                        FMU(i)%Instance(j)%eplusInputVariableVariable(k)%Name = cAlphaArgs(1)

!                        ! get the value reference by using the FMU name and the variable name.
!                        FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%ValueReference = &
!                        getValueReferenceByNameFMUOutputVariables(FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%Name, &
!                        LEN(TRIM(FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%Name)), FMU(i)%Instance(j)%Index)
!                        IF (FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%ValueReference .EQ. -999) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying '// &
!                            'to get the value reference of the FMU output variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%Name)// &
!                            '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'" that will be mapped to a variable.')
!                            CALL ShowContinueError('Please check the name of output variables in the input file '// &
!                            'and in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        IF (FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%ValueReference .EQ. -1) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying '// &
!                            'to get the value reference of the FMU output variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%Name)// &
!                            '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'" that will be mapped to a variable.')
!                            CALL ShowContinueError('This variable is not an FMU output variable. '// &
!                            'Please check the causality of the variable in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        FMU(i)%Instance(j)%eplusInputVariableVariable(k)%VarIndex = &
!                        FindEMSVariable(FMU(i)%Instance(j)%eplusInputVariableVariable(k)%Name, 0)
!                        FMU(i)%Instance(j)%NumOutputVariablesVariable = k
!                        IF (FMU(i)%Instance(j)%eplusInputVariableVariable(k)%VarIndex.LE. 0) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport:declares variable "'// &
!                            TRIM(FMU(i)%Instance(j)%eplusInputVariableVariable(k)%Name)//'",')
!                            CALL ShowContinueError('but variable is not an EMS variable.')
!                            ErrorsFound = .true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        END IF
!                        k = k+1
!                    END IF
!                END DO
!                IF ( FMU(i)%Instance(j)%NumOutputVariablesVariable .GE. 1) THEN
!                    useEMS = .true.
!                END IF
!            END DO
!        END DO

!        ! get the names of the output variables each fmu (and the names of the
!        ! corresponding input variables in EnergyPlus -- actuator).
!        cCurrentModuleObject='ExternalInterface:FunctionalMockupUnitImport:To:Actuator'
!        NumFMUInputVariables = GetNumObjectsFound(cCurrentModuleObject)

!        DO i =1, NumFMUObjects
!            j = 1
!            DO k = 1, NumFMUInputVariables
!                CALL GetObjectItem(cCurrentModuleObject, k, cAlphaArgs, &
!                NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                IF (SameString(cAlphaArgs(5), FMU(i)%Name)) THEN
!                    FMU(i)%TotNumOutputVariablesActuator = j
!                    j = j+1
!                END IF
!            END DO
!        END DO

!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                ALLOCATE(FMU(i)%Instance(j)%fmuOutputVariableActuator(NumFMUInputVariables))
!                ALLOCATE(FMU(i)%Instance(j)%eplusInputVariableActuator(NumFMUInputVariables))
!                k = 1
!                DO l = 1, NumFMUInputVariables
!                    CALL GetObjectItem(cCurrentModuleObject, l, cAlphaArgs, &
!                    NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
!                    IF (SameString(cAlphaArgs(5), FMU(i)%Name).AND.SameString(cAlphaArgs(6), FMU(i)%Instance(j)%Name)) THEN
!                        FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%Name = cAlphaArgs(7)
!                        FMU(i)%Instance(j)%eplusInputVariableActuator(k)%Name = cAlphaArgs(1)

!                        ! get the value reference by using the FMU name and the variable name.
!                        FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%ValueReference = &
!                        getValueReferenceByNameFMUOutputVariables(FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%Name, &
!                        LEN(TRIM(FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%Name)), FMU(i)%Instance(j)%Index)
!                        IF (FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%ValueReference .EQ. -999) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying '// &
!                            'to get the value reference of the FMU output variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%Name)// &
!                            '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'" that will be mapped to an actuator.')
!                            CALL ShowContinueError('Please check the name of output variables in the input file '// &
!                            'and in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        IF (FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%ValueReference .EQ. -1) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport: Error when trying '// &
!                            'to get the value reference of the FMU output variable')
!                            CALL ShowContinueError('"'//TRIM(FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%Name)// &
!                            '" of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'" that will be mapped to an actuator.')
!                            CALL ShowContinueError('This variable is not an FMU output variable. '// &
!                            'Please check the causality of the variable in the modelDescription file.')
!                            ErrorsFound=.true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        ENDIF

!                        FMU(i)%Instance(j)%eplusInputVariableActuator(k)%VarIndex = &
!                        FindEMSVariable(FMU(i)%Instance(j)%eplusInputVariableActuator(k)%Name, 0)
!                        FMU(i)%Instance(j)%NumOutputVariablesActuator = k
!                        IF (FMU(i)%Instance(j)%eplusInputVariableActuator(k)%VarIndex.LE. 0) THEN
!                            CALL ShowSevereError('ExternalInterface/InitExternalInterfaceFMUImport:declares variable "'// &
!                            TRIM(FMU(i)%Instance(j)%eplusInputVariableActuator(k)%Name)//'",')
!                            CALL ShowContinueError('but variable is not an EMS variable.')
!                            ErrorsFound = .true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        END IF
!                        k = k+1
!                    END IF
!                END DO
!                ! set the flag useEMs to true. This will be used then to update the erl variables in erl data structure
!                IF (FMU(i)%Instance(j)%NumOutputVariablesActuator .GE. 1 ) THEN
!                    useEMS = .true.
!                END IF
!            END DO
!        END DO

!        ! parse the fmu defined in the idf using the fmuUnpack with the flag --unpack.
!        DO i = 1, NumFMUObjects
!            DO j = 1, FMU(i)%NumInstances
!                FMU(i)%Instance(j)%NumOutputVariablesInIDF =  FMU(i)%Instance(j)%NumOutputVariablesSchedule + &
!                FMU(i)%Instance(j)%NumOutputVariablesVariable + FMU(i)%Instance(j)%NumOutputVariablesActuator
!                ! check whether the number of output variables in fmu is bigger than in the idf
!                IF (FMU(i)%Instance(j)%NumOutputVariablesInFMU .GT. FMU(i)%Instance(j)%NumOutputVariablesInIDF) THEN
!                    CALL ShowSevereError('InitExternalInterfaceFMUImport: The number of output variables defined in input file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumOutputVariablesInIDF))//')')
!                    CALL ShowContinueError('of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//  &
!                    '" is less than the number of output variables')
!                    CALL ShowContinueError('in the modelDescription file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumOutputVariablesInFMU))//').')
!                    CALL ShowContinueError('Check the input file and the modelDescription file again.')
!                    ErrorsFound=.true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF
!                ! check whether the number of output variables in fmu is less than in the idf
!                IF (FMU(i)%Instance(j)%NumOutputVariablesInFMU .LT. FMU(i)%Instance(j)%NumOutputVariablesInIDF) THEN
!                    CALL ShowSevereError('InitExternalInterfaceFMUImport: The number of output variables defined in input file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumInputVariablesInIDF))//')')
!                    CALL ShowContinueError('of instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)// &
!                    '" is bigger than the number of output variables')
!                    CALL ShowContinueError('in the modelDescription file ('//&
!                    TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumOutputVariablesInFMU))//').')
!                    CALL ShowContinueError('Check the input file and the modelDescription file again.')
!                    ErrorsFound=.true.
!                    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                    CALL StopExternalInterfaceIfError
!                END IF

!                CALL DisplayString('Number of inputs in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" &
!                &of FMU "'//TRIM(FMU(i)%Name)//'" = "'//TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumInputVariablesInIDF))//'".')
!                CALL DisplayString('Number of outputs in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'" &
!                &of FMU "'//TRIM(FMU(i)%Name)//'" = "'//TRIM(TrimSigDigits(FMU(i)%Instance(j)%NumOutputVariablesInIDF))//'".')
!            END DO
!        END DO
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        CALL StopExternalInterfaceIfError
!        FirstCallIni = .FALSE.
!    END IF
!    END SUBROUTINE InitExternalInterfaceFMUImport

!    REAL(r64) FUNCTION GetCurSimStartTimeSeconds()

!    ! FUNCTION INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   August 2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS FUNCTION:
!    !  Get the current month and day in the runperiod and convert
!    !  it into seconds.

!    ! METHODOLOGY EMPLOYED:
!    ! <description>

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE DataEnvironment, ONLY: Month, DayofMonth, CurrentYearIsLeapYear
!    USE DataGlobals, ONLY: HourOfDay

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! FUNCTION ARGUMENT DEFINITIONS:

!    REAL(r64) :: simtime

!    ! FUNCTION PARAMETER DEFINITIONS:
!    ! na

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! FUNCTION LOCAL VARIABLE DECLARATIONS:


!    IF(.not. CurrentYearIsLeapYear) THEN
!        IF (Month .EQ. 1) THEN
!            simtime = 0
!        ELSEIF (Month .EQ. 2) THEN
!            simtime = 31
!        ELSEIF (Month .EQ. 3)THEN
!            simtime = 59
!        ELSEIF (Month .EQ. 4) THEN
!            simtime = 90
!        ELSEIF (Month .EQ. 5) THEN
!            simtime = 120
!        ELSEIF (Month .EQ. 6) THEN
!            simtime = 151
!        ELSEIF (Month .EQ. 7) THEN
!            simtime = 181
!        ELSEIF (Month .EQ. 8) THEN
!            simtime = 212
!        ELSEIF (Month .EQ. 9) THEN
!            simtime = 243
!        ELSEIF (Month .EQ. 10) THEN
!            simtime = 273
!        ELSEIF (Month .EQ. 11) THEN
!            simtime = 304
!        ELSEIF (Month .EQ. 12) THEN
!            simtime = 334
!        ELSE
!            simtime = 0
!        ENDIF
!    ELSE
!        IF (Month .EQ. 1) THEN
!            simtime = 0
!        ELSEIF (Month .EQ. 2) THEN
!            simtime = 31
!        ELSEIF (Month .EQ. 3)THEN
!            simtime = 59 + 1
!        ELSEIF (Month .EQ. 4) THEN
!            simtime = 90 + 1
!        ELSEIF (Month .EQ. 5) THEN
!            simtime = 120 + 1
!        ELSEIF (Month .EQ. 6) THEN
!            simtime = 151 + 1
!        ELSEIF (Month .EQ. 7) THEN
!            simtime = 181 + 1
!        ELSEIF (Month .EQ. 8) THEN
!            simtime = 212 + 1
!        ELSEIF (Month .EQ. 9) THEN
!            simtime = 243 + 1
!        ELSEIF (Month .EQ. 10) THEN
!            simtime = 273 + 1
!        ELSEIF (Month .EQ. 11) THEN
!            simtime = 304 + 1
!        ELSEIF (Month .EQ. 12) THEN
!            simtime = 334 + 1
!        ELSE
!            simtime = 0
!        ENDIF
!    END IF

!    simtime = 24 * (simtime +(DayOfMonth-1))! day of month does not need to be substracted??
!    !simtime = 24 * (simtime +(DayOfMonth))! days to hours
!    simtime = 60 * (simtime + (HourOfDay-1))  ! hours to minutes
!    simtime = 60 * (simtime)  ! minutes to seconds

!    GetCurSimStartTimeSeconds = simtime

!    END FUNCTION GetCurSimStartTimeSeconds
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE CalcExternalInterfaceFMUImport()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Thierry S. Nouidui, Michael Wetter, Wangda Zuo
!    !       DATE WRITTEN   08Aug2011
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine organizes the data exchange between FMU and EnergyPlus.

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE DataEnvironment, ONLY: TotalOverallSimDays, TotDesDays
!    USE ScheduleManager, ONLY: GetDayScheduleIndex, ExternalInterfaceSetSchedule
!    USE RuntimeLanguageProcessor, ONLY: isExternalInterfaceErlVariable, FindEMSVariable
!    USE DataInterfaces, ONLY:GetVariableKeyCountandType, GetVariableKeys
!    USE RuntimeLanguageProcessor, ONLY: ExternalInterfaceSetErlVariable
!    USE EMSManager, ONLY: ManageEMS
!    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
!    USE DataIPShortCuts
!    USE DataGlobals, ONLY: WarmupFlag, KindOfSim, ksRunPeriodWeather, TimeStepZone, emsCallFromExternalInterface
!    USE DataSystemVariables, ONLY: UpdateDataDuringWarmupExternalInterface
!    USE ISO_C_BINDING, ONLY : C_PTR


!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! These parameters are also declared in an interface below.


!    INTEGER, PARAMETER :: IntegerVar = 1      ! Integer variable
!    INTEGER, PARAMETER :: RealVar = 2         ! Real variable


!    INTEGER :: i, j, k, l         ! Loop counter
!    ! INTERFACE BLOCK SPECIFICATIONS
!    ! na

!    ! DERIVED TYPE DEFINITIONS

!    INTERFACE
!    INTEGER FUNCTION fmiGetReal(fmiComponent, valRef, &
!    fmuVariableValue, numOutputs, index) BIND (C, NAME="fmiEPlusGetReal")
!    ! Function called to get real values from FMU outputs
!    USE ISO_C_BINDING, ONLY: C_INT, C_PTR, C_DOUBLE
!    TYPE (C_PTR)                         :: fmiComponent                 ! Pointer to FMU instance
!    INTEGER(kind=C_INT), DIMENSION(*)    :: valRef                       ! Parameter fmiValueReference
!    REAL (kind=C_DOUBLE), DIMENSION(*)   :: fmuVariableValue             ! FMU output variables
!    INTEGER(kind=C_INT)                  :: numOutputs                   ! Number of input variables
!    INTEGER(kind=C_INT)                  :: index                        ! Index of the FMU 
!    END FUNCTION fmiGetReal
!    END INTERFACE


!    INTERFACE
!    INTEGER FUNCTION fmiSetReal(fmiComponent, valRef, &
!    fmuVariableValue, numInputs, index) BIND (C, NAME="fmiEPlusSetReal")
!    ! Function called to set real values to FMU inputs
!    USE ISO_C_BINDING, ONLY: C_INT, C_DOUBLE, C_PTR
!    TYPE (C_PTR)                              :: fmiComponent                ! Pointer to FMU instance
!    INTEGER(kind=C_INT) , DIMENSION(*)        :: valRef                      ! Parameter fmiValueReference
!    REAL (kind=C_DOUBLE), DIMENSION(*)        :: fmuVariableValue            ! FMU input variables
!    INTEGER(kind=C_INT)                       :: numInputs                   ! Number of input variables
!    INTEGER(kind=C_INT)                       :: index                       ! Index of the FMU 
!    END FUNCTION fmiSetReal
!    END INTERFACE

!    INTERFACE
!    INTEGER FUNCTION fmiDoStep(fmiComponent, &
!    curCommPoint, commStepSize, newStep, index) BIND (C, NAME="fmiEPlusDoStep")
!    !  Function called to do one step of the co-simulation
!    USE ISO_C_BINDING, ONLY: C_INT, C_PTR, C_DOUBLE
!    TYPE (C_PTR)                               :: fmiComponent               ! Pointer to FMU instance
!    REAL(kind=C_DOUBLE)                        :: curCommPoint               ! Current communication point
!    REAL(kind=C_DOUBLE)                        :: commStepSize               ! Communication step size
!    INTEGER (C_INT)                            :: newStep
!    INTEGER(kind=C_INT)                        :: index                        ! Index of the FMU
!    END FUNCTION fmiDoStep
!    END INTERFACE

!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

!    INTEGER                        :: retVal          ! Return value of function call, used for error handling
!    INTEGER                        :: NumAlphas  = 0 ! Number of Alphas for each GetObjectItem call
!    INTEGER                        :: NumNumbers = 0 ! Number of Numbers for each GetObjectItem call
!    INTEGER                        :: IOStatus = 0  ! Used in GetObjectItem
!    INTEGER                        :: NumFMUInputVariables = 0     ! Number of FMU input variables

!    INTEGER :: NumNumeric  ! Number of numbers being input
!    LOGICAL :: IsNotOK               ! Flag to verify name
!    LOGICAL :: IsBlank               ! Flag for blank name
!    LOGICAL, SAVE :: FirstCallFlag = .TRUE.               ! Flag for first call
!    LOGICAL, SAVE :: FirstCallDesignDays = .TRUE.         ! Flag fo first call during warmup
!    LOGICAL, SAVE :: FirstCallWUp = .TRUE.                ! Flag fo first call during warmup
!    LOGICAL, SAVE :: FirstCallTStep = .TRUE.              ! Flag for first call during time stepping
!    INTEGER :: Count

!    CHARACTER(len=MaxNameLength), DIMENSION(5) :: Alphas
!    INTEGER NumAlpha, NumNumber, IOStat
!    INTEGER :: Num

!    CHARACTER(len=MaxNameLength), DIMENSION(maxVar) :: curVals  ! Names of schedules (i.e., schedule names)
!    INTEGER curNumInpVal                                        ! current number of input values for the InputValType
!    CHARACTER(len=maxErrMsgLength)    :: validateErrMsg         ! error returned when xml Schema validate failed
!    INTEGER                           :: errMsgLen              ! the length of the error message


!    INTEGER                           :: varType      = 0 ! 0=not found, 1=integer, 2=real, 3=meter
!    INTEGER                           :: numKey       = 0 ! Number of keys found
!    INTEGER                           :: varAvgSum    = 0 ! Variable  is Averaged=1 or Summed=2
!    INTEGER                           :: varStepType  = 0 ! Variable time step is Zone=1 or HVAC=2
!    CHARACTER(len=10)                 :: varUnits         ! Units sting, may be blank
!    CHARACTER(len=1000)               :: tempChar         ! Units sting, may be blank


!    INTEGER                           :: Loop      ! Loop counter
!    INTEGER                           :: NumTSObjects

!    INTEGER, DIMENSION(1):: keyIndexes ! Array index for
!    CHARACTER(len=MaxNameLength), DIMENSION(1):: NamesOfKeys      ! Specific key name

!    IF (WarmupFlag .AND. (KindOfSim .NE. ksRunPeriodWeather)) THEN ! No data exchange during design days
!        IF (FirstCallDesignDays) THEN
!            CALL ShowWarningError('ExternalInterface/CalcExternalInterfaceFMUImport: '//  &
!            'ExternalInterface does not exchange data during design days.')
!        END IF
!        FirstCallDesignDays = .FALSE.
!    END IF
!    IF (WarmupFlag .AND. (KindOfSim .EQ. ksRunPeriodWeather)) THEN ! Data exchange after design days
!        IF (FirstCallWUp) THEN
!            ! set the report during warmup to true so that variables are also updated during the warmup
!            UpdateDataDuringWarmupExternalInterface = .TRUE.
!            hStep = (60.0d0 * TimeStepZone) * 60.0d0
!            tStart = GetCurSimStartTimeSeconds()
!            tStop =  tStart + 24.0d0* 3600.0d0
!            tComm = tStart

!            ! instantiate and initialize the unpack fmus
!            CALL InstantiateInitializeFMUImport ()

!            ! allocate memory for a temporary FMU that will be used at the end of the warmup
!            ALLOCATE(FMUTemp(NumFMUObjects))
!            DO i =1, NumFMUObjects
!                ALLOCATE(FMUTemp(i)%Instance(FMU(i)%NumInstances))
!            END DO
!            DO i = 1, NumFMUObjects
!                DO j = 1, FMU(i)%NumInstances
!                    ALLOCATE(FMUTemp(i)%Instance(j)%fmuInputVariable(FMU(i)%Instance(j)%NumInputVariablesInIDF))
!                    ALLOCATE(FMUTemp(i)%Instance(j)%eplusOutputVariable(FMU(i)%Instance(j)%NumInputVariablesInIDF))
!                    ALLOCATE(FMUTemp(i)%Instance(j)%fmuOutputVariableSchedule(FMU(i)%Instance(j)%NumOutputVariablesSchedule))
!                    ALLOCATE(FMUTemp(i)%Instance(j)%fmuOutputVariableVariable(FMU(i)%Instance(j)%NumOutputVariablesVariable))
!                    ALLOCATE(FMUTemp(i)%Instance(j)%fmuOutputVariableActuator(FMU(i)%Instance(j)%NumOutputVariablesActuator))
!                END DO
!            END DO

!            CALL GetSetVariablesAndDoStepFMUImport ()
!            tComm = tComm + hStep
!            FirstCallWUp = .FALSE.

!        ELSE
!            IF (tComm .LT. tStop) THEN
!                CALL GetSetVariablesAndDoStepFMUImport ()
!                ! Advance the communication time step
!                tComm = tComm + hStep
!            ELSE
!                DO i = 1, NumFMUObjects
!                    DO j = 1, FMU(i)%NumInstances
!                        FMUTemp(i)%Instance(j)%NumInputVariablesInIDF = FMU(i)%Instance(j)%NumInputVariablesInIDF
!                        DO k = 1, FMU(i)%Instance(j)%NumInputVariablesInIDF
!                            FMUTemp(i)%Instance(j)%fmuInputVariable(k)%ValueReference = &
!                            FMU(i)%Instance(j)%fmuInputVariable(k)%ValueReference
!                            FMUTemp(i)%Instance(j)%eplusOutputVariable(k)%RTSValue = &
!                            FMU(i)%Instance(j)%eplusOutputVariable(k)%RTSValue
!                            FMUTemp(i)%Instance(j)%eplusOutputVariable(k)%ITSValue = &
!                            FMU(i)%Instance(j)%eplusOutputVariable(k)%ITSValue
!                            FMUTemp(i)%Instance(j)%eplusOutputVariable(k)%VarType  = &
!                            FMU(i)%Instance(j)%eplusOutputVariable(k)%VarType
!                        END DO

!                        FMUTemp(i)%Instance(j)%NumOutputVariablesSchedule = FMU(i)%Instance(j)%NumOutputVariablesSchedule
!                        ! save values that will be set in EnergyPlus (Schedule)
!                        DO k = 1, FMU(i)%Instance(j)%NumOutputVariablesSchedule
!                            FMUTemp(i)%Instance(j)%fmuOutputVariableSchedule(k)%RealVarValue = &
!                            FMU(i)%Instance(j)%fmuOutputVariableSchedule(k)%RealVarValue
!                        END DO

!                        ! save values that will be set in EnergyPlus (Variable)
!                        FMUTemp(i)%Instance(j)%NumOutputVariablesVariable = FMU(i)%Instance(j)%NumOutputVariablesVariable
!                        DO k = 1, FMU(i)%Instance(j)%NumOutputVariablesVariable
!                            FMUTemp(i)%Instance(j)%fmuOutputVariableVariable(k)%RealVarValue =  &
!                            FMU(i)%Instance(j)%fmuOutputVariableVariable(k)%RealVarValue
!                        END DO

!                        ! save values that will be set in EnergyPlus (Actuator)
!                        FMUTemp(i)%Instance(j)%NumOutputVariablesActuator = FMU(i)%Instance(j)%NumOutputVariablesActuator
!                        DO k = 1, FMU(i)%Instance(j)%NumOutputVariablesActuator
!                            FMUTemp(i)%Instance(j)%fmuOutputVariableActuator(k)%RealVarValue =  &
!                            FMU(i)%Instance(j)%fmuOutputVariableActuator(k)%RealVarValue
!                        END DO
!                    END DO
!                END DO

!                CALL StopExternalInterfaceIfError

!                ! Terminate all FMUs
!                CALL TerminateResetFreeFMUImport()

!                ! Reset the communication time step
!                tComm = tStart

!                ! Reinstantiate and reinitialize the FMUs
!                CALL InstantiateInitializeFMUImport()

!                ! Set the values that have been saved in the FMUs-- saveFMUStateVariables ()
!                DO i = 1, NumFMUObjects
!                    DO j = 1, FMU(i)%NumInstances
!                        FMU(i)%Instance(j)%fmiStatus = fmiSetReal(FMU(i)%Instance(j)%fmiComponent, &
!                        FMU(i)%Instance(j)%fmuInputVariable%ValueReference, &
!                        FMUTemp(i)%Instance(j)%eplusOutputVariable%RTSValue, &
!                        FMUTemp(i)%Instance(j)%NumInputVariablesInIDF, &
!                        FMU(i)%Instance(j)%Index)
!                        IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                            CALL ShowSevereError('ExternalInterface/CalcExternalInterfaceFMUImport: Error when trying '// &
!                            'to set an input value in instance "'//TRIM(FMU(i)%Instance(j)%Name)//'"')
!                            CALL ShowContinueError('of FMU "'//TRIM(FMU(i)%Name)//'". &
!                            &Error Code = '//TRIM(TrimSigDigits(FMU(i)%Instance(j)%fmiStatus))//'.')
!                            ErrorsFound = .true.
!                            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            CALL StopExternalInterfaceIfError
!                        END IF
!                    END DO
!                END DO
!                ! set the flag to reinitialize states to be true
!                FlagReIni = .TRUE.
!                CALL GetSetVariablesAndDoStepFMUImport
!                FlagReIni = .FALSE.
!                ! advance one time step ahead for the next calculation
!                tComm = tComm + hStep
!            END IF
!        END IF
!    END IF
!    ! BeginSimulation
!    IF (.NOT.(WarmupFlag) .AND. (KindOfSim .EQ. ksRunPeriodWeather)) THEN
!        IF (FirstCallTStep) THEN
!            ! reset the UpdateDataDuringWarmupExternalInterface to be false.
!            UpdateDataDuringWarmupExternalInterface = .FALSE.
!            ! The time is computed in seconds for FMU
!            tStart = GetCurSimStartTimeSeconds()
!            tStop = tStart + (TotalOverallSimDays - TotDesDays ) * 24.0d0 * 3600.0d0
!            tComm = tStart

!            ! Terminate all FMUs
!            CALL TerminateResetFreeFMUImport()

!            ! Reinstantiate and reinitialize the FMUs
!            CALL InstantiateInitializeFMUImport()

!            ! Set the values that have been saved in the FMUs-- saveFMUStateVariables ()
!            DO i = 1, NumFMUObjects
!                DO j = 1, FMU(i)%NumInstances
!                    FMU(i)%Instance(j)%fmiStatus = fmiSetReal(FMU(i)%Instance(j)%fmiComponent, &
!                    FMUTemp(i)%Instance(j)%fmuInputVariable%ValueReference, &
!                    FMUTemp(i)%Instance(j)%eplusOutputVariable%RTSValue, &
!                    FMUTemp(i)%Instance(j)%NumInputVariablesInIDF, &
!                    FMU(i)%Instance(j)%Index)

!                    IF ( .NOT. (FMU(i)%Instance(j)%fmiStatus.EQ. fmiOK)) THEN
!                        CALL ShowSevereError('ExternalInterface/CalcExternalInterfaceFMUImport: ')
!                        CALL ShowContinueError('Error when trying to set inputs in instance ')
!                        CALL ShowContinueError ('"'//TRIM(FMU(i)%Instance(j)%Name)//'" of FMU "'//TRIM(FMU(i)%Name)//'".')
!                        CALL ShowContinueError ('Error Code = "'//TrimSigDigits(FMU(i)%Instance(j)%fmiStatus)//'".')
!                        ErrorsFound = .true.
!                        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                        CALL StopExternalInterfaceIfError
!                    END IF
!                    !   END IF
!                    ! END DO
!                END DO
!            END DO
!            ! set the flag to reinitialize states to be true
!            FlagReIni = .TRUE.
!            CALL GetSetVariablesAndDoStepFMUImport ()
!            FlagReIni = .FALSE.
!            ! advance one time step ahead for the next calculation
!            tComm = tComm + hStep
!            FirstCallTStep = .FALSE.
!        ELSE
!            IF (tComm .NE. tStop) THEN
!                CALL GetSetVariablesAndDoStepFMUImport ()
!                tComm = tComm + hStep
!            ELSE
!                ! Terminate reset and free Slaves
!                CALL TerminateResetFreeFMUImport ()
!                DO i = 1, NumFMUObjects
!                    DO j = 1, FMU(i)%NumInstances
!                        ! Deallocate used objects
!                        DEALLOCATE (FMUTemp(i)%Instance(j)%fmuInputVariable)
!                        DEALLOCATE (FMUTemp(i)%Instance(j)%eplusOutputVariable)
!                        DEALLOCATE (FMUTemp(i)%Instance(j)%fmuOutputVariableSchedule)
!                        DEALLOCATE (FMUTemp(i)%Instance(j)%fmuOutputVariableVariable)
!                        DEALLOCATE (FMUTemp(i)%Instance(j)%fmuOutputVariableActuator)
!                    END DO
!                END DO

!                DO i = 1, NumFMUObjects
!                    ! Deallocate used objects
!                    DEALLOCATE (FMUTemp(i)%Instance)
!                END DO

!                DEALLOCATE (FMUTemp)

!                DO i = 1, NumFMUObjects
!                    DO j = 1, FMU(i)%NumInstances
!                        DEALLOCATE (FMU(i)%Instance(j)%eplusInputVariableSchedule)
!                        DEALLOCATE (FMU(i)%Instance(j)%fmuOutputVariableSchedule)
!                        DEALLOCATE (FMU(i)%Instance(j)%eplusInputVariableVariable)
!                        DEALLOCATE (FMU(i)%Instance(j)%fmuOutputVariableVariable)
!                        DEALLOCATE (FMU(i)%Instance(j)%eplusInputVariableActuator)
!                        DEALLOCATE (FMU(i)%Instance(j)%fmuOutputVariableActuator)
!                        DEALLOCATE (FMU(i)%Instance(j)%fmuInputVariable)
!                        DEALLOCATE (FMU(i)%Instance(j)%checkfmuInputVariable)

!                    END DO
!                END DO

!                DO i = 1, NumFMUObjects
!                    DEALLOCATE (FMU(i)%Instance)
!                END DO
!                DEALLOCATE (FMU)
!            END IF
!        END IF
!    ENDIF
!    RETURN
!    END SUBROUTINE CalcExternalInterfaceFMUImport

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE ValidateRunControl

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   December 2009
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine ensures that the RunControl object is valid.

!    ! METHODOLOGY EMPLOYED:
!    ! Use GetObjectItem from the Input Processor

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
!    USE DataIPShortCuts

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! na

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER                        :: NumAlphas  = 0 ! Number of Alphas for each GetObjectItem call
!    INTEGER                        :: NumNumbers = 0 ! Number of Numbers for each GetObjectItem call
!    INTEGER                        :: IOStatus = 0  ! Used in GetObjectItem

!    INTEGER :: NumRunControl

!    cCurrentModuleObject='SimulationControl'
!    NumRunControl = GetNumObjectsFound(cCurrentModuleObject)
!    IF (NumRunControl > 0) THEN
!        CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!        AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
!        IF (cAlphaArgs(5).EQ.'NO') THEN ! This run does not have a weather file simulation.
!            CALL ShowSevereError(  'ExternalInterface: Error in idf file, section SimulationControl:')
!            CALL ShowContinueError('When using the ExternalInterface, a run period from the weather file must be specified')
!            CALL ShowContinueError('in the idf file, because the ExternalInterface interface is not active during')
!            CALL ShowContinueError('warm-up and during sizing.')
!            ErrorsFound = .true.
!        END IF
!    END IF
!    END SUBROUTINE ValidateRunControl

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE CalcExternalInterface()

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   2Dec2007
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! na

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE DataGlobals, ONLY: SimTimeSteps, MinutesPerTimeStep, emsCallFromExternalInterface
!    USE DataInterfaces, ONLY: GetInternalVariableValueExternalInterface, GetInternalVariableValue
!    USE ScheduleManager, ONLY: ExternalInterfaceSetSchedule
!    USE RuntimeLanguageProcessor, ONLY: ExternalInterfaceSetErlVariable
!    USE EMSManager, ONLY: ManageEMS

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! These parameters are also declared in an interface below.
!    ! Change all together.
!    INTEGER, PARAMETER :: nDblMax = 1024   ! Maximum number of doubles
!    INTEGER, PARAMETER :: nIntMax = 0      ! Maximum number of integers
!    INTEGER, PARAMETER :: nBooMax = 0      ! Maximum number of booleans


!    ! INTERFACE BLOCK SPECIFICATIONS:
!    INTERFACE
!    INTEGER(C_INT) FUNCTION exchangeDoublesWithSocket(socketFD, &
!    flaWri, flaRea, &
!    nDblWri, nDblRea, &
!    simTimWri, dblValWri, &
!    simTimRea, dblValRea) BIND (C, NAME="exchangedoubleswithsocket")
!    ! Exchanges data with the socket
!    USE ISO_C_BINDING, ONLY: C_INT
!    USE DataPrecisionGlobals

!    ! These parameters are also declared in an interface below.
!    ! Change all together.
!    INTEGER(C_INT), PARAMETER :: nDblMax = 1024       ! Maximum number of doubles

!    INTEGER(C_INT) socketFD                           ! socket file descriptor
!    INTEGER(C_INT) flaWri                             ! flag to write to the socket
!    INTEGER(C_INT) flaRea                             ! flag read from the socket
!    INTEGER(C_INT) nDblWri                            ! number of doubles to write to socket
!    INTEGER(C_INT) nDblRea                            ! number of doubles to read from socket
!    REAL(r64) simTimWri                               ! simulation time to write to socket
!    REAL(r64) simTimRea                               ! simulation time to read from socket
!    REAL(r64), DIMENSION(nDblMax) :: dblValWri        ! dbl values to be written to the socket
!    REAL(r64), DIMENSION(nDblMax) :: dblValRea        ! dbl values to be read from the socket
!    END FUNCTION exchangeDoublesWithSocket
!    END INTERFACE

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    INTERFACE
!    INTEGER(C_INT) FUNCTION exchangeDoublesWithSocketFMU(socketFD, &
!    flaWri, flaRea, &
!    nDblWri, nDblRea, &
!    simTimWri, dblValWri, &
!    simTimRea, dblValRea, epexport) BIND (C, NAME="exchangedoubleswithsocketFMU")
!    ! Exchanges data with the socket
!    USE ISO_C_BINDING, ONLY: C_INT
!    USE DataPrecisionGlobals

!    ! These parameters are also declared in an interface below.
!    ! Change all together.
!    INTEGER(C_INT), PARAMETER :: nDblMax = 1024       ! Maximum number of doubles

!    INTEGER(C_INT) socketFD                           ! socket file descriptor
!    INTEGER(C_INT) flaWri                             ! flag to write to the socket
!    INTEGER(C_INT) flaRea                             ! flag read from the socket
!    INTEGER(C_INT) nDblWri                            ! number of doubles to write to socket
!    INTEGER(C_INT) nDblRea                            ! number of doubles to read from socket
!    REAL(r64) simTimWri                               ! simulation time to write to socket
!    REAL(r64) simTimRea                               ! simulation time to read from socket
!    REAL(r64), DIMENSION(nDblMax) :: dblValWri        ! dbl values to be written to the socket
!    REAL(r64), DIMENSION(nDblMax) :: dblValRea        ! dbl values to be read from the socket
!    INTEGER(C_INT) epexport                           ! FMU Export flag
!    END FUNCTION exchangeDoublesWithSocketFMU
!    END INTERFACE



!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER      :: i, j    ! Loop counter
!    INTEGER      :: retVal  ! Return value from socket

!    INTEGER flaWri   ! flag to write to the socket
!    INTEGER flaRea   ! flag read from the socket
!    INTEGER nDblWri ! number of doubles to write to socket
!    INTEGER nDblRea ! number of doubles to read from socket
!    REAL(r64) :: curSimTim ! current simulation time
!    REAL(r64) :: preSimTim ! previous time step's simulation time

!    REAL(r64), DIMENSION(nDblMax) :: dblValWri
!    REAL(r64), DIMENSION(nDblMax) :: dblValRea
!    character*5 retValCha
!    LOGICAL                              :: continueSimulation ! Flag, true if simulation should continue
!    LOGICAL, SAVE                        :: firstCall = .true.
!    LOGICAL, SAVE                        :: showContinuationWithoutUpdate = .true.

!    IF (firstCall) THEN
!        CALL DisplayString('ExternalInterface starts first data exchange.')
!        simulationStatus = 2
!        !firstCall = .false. ! bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui
!        preSimTim = 0 ! In the first call, E+ did not reset SimTimeSteps to zero
!    ELSE
!        preSimTim = SimTimeSteps * MinutesPerTimeStep * 60.0d0
!    ENDIF

!    ! Socket asked to terminate simulation, but simulation continues
!    IF (noMoreValues .AND. showContinuationWithoutUpdate) THEN
!        IF (haveExternalInterfaceBCVTB) THEN
!            CALL ShowWarningError('ExternalInterface: Continue simulation without updated values from server at t =' &
!            //TrimSigDigits(preSimTim/3600, 2) // ' hours')
!        END IF
!        showContinuationWithoutUpdate = .false.
!    ENDIF


!    ! Usual branch, control is configured and simulation should continue
!    IF (ConfiguredControlPoints .AND. (.NOT.noMoreValues)) THEN
!        ! Data to be exchanged
!        nDblWri = SIZE(varTypes)
!        nDblRea = 0
!        flaWri  = 0
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Get EnergyPlus variables
!        IF (firstcall) then ! bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui
!            DO i = 1, nDblWri
!                dblValWri(i) = GetInternalVariableValue(varTypes(i), keyVarIndexes(i))
!            ENDDO
!        ELSE
!            DO i = 1, nDblWri
!                dblValWri(i) = GetInternalVariableValueExternalInterface(varTypes(i), keyVarIndexes(i))
!            ENDDO
!        END IF

!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Exchange data with socket
!        retVal = 0
!        flaRea = 0
!        IF (haveExternalInterfaceBCVTB) THEN
!            retVal = exchangeDoublesWithSocket(socketFD, &
!            flaWri, flaRea, &
!            nDblWri, nDblRea, &
!            preSimTim, dblValWri, &
!            curSimTim, dblValRea)
!        ELSEIF (haveExternalInterfaceFMUExport) THEN
!            retVal = exchangeDoublesWithSocketFMU(socketFD, &
!            flaWri, flaRea, &
!            nDblWri, nDblRea, &
!            preSimTim, dblValWri, &
!            curSimTim, dblValRea, &
!            FMUExportActivate)
!        ENDIF
!        continueSimulation = .true.
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Check for errors, in which case we terminate the simulation loop
!        ! Added a check since the FMUExport is terminated with the flaRea set to 1.
!        IF (haveExternalInterfaceBCVTB .OR. (haveExternalInterfaceFMUExport .AND. (flaRea .EQ. 0))) THEN
!            IF (retVal .NE. 0) THEN
!                continueSimulation = .false.
!                write (retValCha,1000) retVal
!                CALL ShowSevereError('ExternalInterface: Socket communication received error value "' &
!                //TRIM(retValCha)// '" at time = ' &
!                //TRIM(TrimSigDigits(preSimTim/3600,2 ))// ' hours.')
!                write (retValCha,1000) flaRea
!                CALL ShowContinueError('ExternalInterface: Flag from server "' &
!                //TRIM(retValCha)// '".')
!                ErrorsFound = .true.
!                CALL StopExternalInterfaceIfError
!            END IF
!        END IF
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Check communication flag
!        IF (flaRea .NE. 0) THEN
!            ! No more values will be received in future steps
!            ! Added a check since the FMUExport  is terminated with the flaRea set to 1.
!            noMoreValues = .true.
!            write (retValCha,1000) flaRea
!            IF (haveExternalInterfaceBCVTB) THEN
!                CALL ShowSevereError('ExternalInterface: Received end of simulation flag at time = ' &
!                //TRIM(TrimSigDigits(preSimTim/3600,2 ))// ' hours.')
!                CALL StopExternalInterfaceIfError
!            END IF
!        END IF
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! Make sure we get the right number of double values, unless retVal != 0
!        IF ( (flaRea .EQ. 0 ) .AND. (.NOT. ErrorsFound) .AND. &
!        continueSimulation .AND. (nDblRea .NE. SIZE(varInd) ) ) THEN
!            CALL ShowSevereError('ExternalInterface: Received "' &
!            //TRIM(TrimSigDigits(nDblRea))// '" double values, expected "' &
!            //TRIM(TrimSigDigits(SIZE(varInd)))// '".')
!            ErrorsFound = .true.
!            CALL StopExternalInterfaceIfError
!        END IF

!1000    format(I2)
!        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        ! No errors found. Assign exchanged variables
!        IF ( (flaRea .EQ. 0 ) .AND. continueSimulation ) THEN
!            DO i = 1, SIZE(varInd)
!                IF ( (inpVarTypes(i) .EQ. indexSchedule) ) THEN
!                    CALL ExternalInterfaceSetSchedule(varInd(i), dblValRea(i))
!                ELSE IF ( (inpVarTypes(i) .EQ. indexVariable) .OR. (inpVarTypes(i) .EQ. indexActuator) ) THEN
!                    CALL ExternalInterfaceSetErlVariable(varInd(i), dblValRea(i))
!                ELSE
!                    CALL ShowContinueError('ExternalInterface: Error in finding the type of the input variable for EnergyPlus')
!                    CALL ShowContinueError('variable index: '//TrimSigDigits(i, 2)//'. Variable will not be updated.')
!                ENDIF
!            ENDDO
!        ENDIF
!    ENDIF
!    ! If we have Erl variables, we need to call ManageEMS so that they get updated in the Erl data structure
!    IF (useEMS) THEN
!        CALL ManageEMS(emsCallFromExternalInterface)
!    END IF

!    firstCall = .false. ! bug fix causing external interface to send zero at the beginning of sim, Thierry Nouidui
!    RETURN

!    END SUBROUTINE CalcExternalInterface

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE GetReportVariableKey(varKeys, numberOfKeys, varNames, keyVarIndexes, varTypes)
!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   2Dec2007
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! Gets the sensor key index and type for the specified variable key and name

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE DataInterfaces, ONLY: GetVariableKeyCountandType, GetVariableKeys

!    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    INTEGER, INTENT(IN)                                :: numberOfKeys  ! Number of keys=size(varKeys)
!    CHARACTER(len=*), INTENT(IN), DIMENSION(:)         :: varKeys       ! Standard variable name
!    CHARACTER(len=*), INTENT(IN), DIMENSION(:)         :: varNames      ! Standard variable name
!    INTEGER, INTENT(OUT), DIMENSION(:)                 :: keyVarIndexes ! Array index
!    INTEGER, INTENT(OUT), DIMENSION(:)                 :: varTypes      ! Types of variables in keyVarIndexes

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! na

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    ! na
!    INTEGER                           :: varType      = 0 ! 0=not found, 1=integer, 2=real, 3=meter
!    INTEGER                           :: numKeys      = 0 ! Number of keys found
!    INTEGER                           :: varAvgSum    = 0 ! Variable  is Averaged=1 or Summed=2
!    INTEGER                           :: varStepType  = 0 ! Variable time step is Zone=1 or HVAC=2
!    CHARACTER(len=10)                 :: varUnits         ! Units sting, may be blank
!    INTEGER, DIMENSION(:), ALLOCATABLE :: keyIndexes ! Array index for
!    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NamesOfKeys      ! Specific key name
!    INTEGER                           :: Loop, iKey       ! Loop counter

!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    ! Get pointers for variables to be sent to Ptolemy
!    DO Loop=1, numberOfKeys
!        CALL GetVariableKeyCountandType(varNames(Loop),numKeys,varType,varAvgSum,varStepType,varUnits)
!        IF ( varType .NE. 0 ) THEN
!            ALLOCATE(NamesOfKeys(numKeys))
!            ALLOCATE(keyIndexes(numKeys))
!            CALL GetVariableKeys(varNames(Loop), varType, NamesOfKeys, keyIndexes)
!            ! Find key index whose keyName is equal to keyNames(Loop)
!            LoopKey: DO iKey = 1, SIZE(NamesOfKeys)
!                IF ( TRIM( NamesOfKeys(iKey) ) == TRIM(varKeys(Loop)) ) THEN
!                    keyVarIndexes(Loop) = keyIndexes( iKey )
!                    varTypes(Loop) = varType
!                    EXIT LoopKey
!                END IF
!            END DO LoopKey
!            DEALLOCATE(keyIndexes)
!            DEALLOCATE(NamesOfKeys)
!        ENDIF
!        IF (( varType == 0 ).OR. (iKey > SIZE(NamesOfKeys)) ) THEN
!            CALL ShowSevereError('ExternalInterface: Simulation model has no variable "' &
!            //TRIM(varNames(Loop))// '" with key "'//TRIM(varKeys(Loop))//'".')
!            ErrorsFound = .true.
!        ENDIF
!    END DO

!    RETURN

!    END SUBROUTINE GetReportVariableKey


!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE WarnIfExternalInterfaceObjectsAreUsed(ObjectWord)

!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   December 2009
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine writes a warning if ExternalInterface objects are used in the
!    ! idf file, but the ExternalInterface link is not specified.

!    ! METHODOLOGY EMPLOYED:
!    ! Use GetObjectItem from the Input Processor

!    ! REFERENCES:
!    ! na

!    ! USE STATEMENTS:
!    USE InputProcessor

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    CHARACTER(len=*),INTENT(IN) :: ObjectWord

!    ! SUBROUTINE PARAMETER DEFINITIONS:
!    ! na

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! DERIVED TYPE DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER :: NumObjects   ! Number of objects found in idf file

!    NumObjects = GetNumObjectsFound(ObjectWord)
!    IF (NumObjects > 0) THEN
!        CALL ShowWarningError( 'IDF file contains object "'//TRIM(ObjectWord)//'",')
!        CALL ShowContinueError('but object "ExternalInterface" with appropriate key entry is not specified. '//  &
!        'Values will not be updated.')
!    END IF

!    END SUBROUTINE WarnIfExternalInterfaceObjectsAreUsed

!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    SUBROUTINE VerifyExternalInterfaceObject
!    ! SUBROUTINE INFORMATION:
!    !       AUTHOR         Michael Wetter
!    !       DATE WRITTEN   12Dec2009
!    !       MODIFIED       na
!    !       RE-ENGINEERED  na

!    ! PURPOSE OF THIS SUBROUTINE:
!    ! This subroutine verifies the correctness of the fields of
!    ! the ExternalInterface object in the idf file

!    ! USE STATEMENTS:
!    USE InputProcessor, ONLY: GetObjectItem, SameString
!    USE DataIPShortCuts

!    ! METHODOLOGY EMPLOYED:
!    ! na

!    ! REFERENCES:
!    ! na

!    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

!    ! INTERFACE BLOCK SPECIFICATIONS:
!    ! na

!    ! SUBROUTINE ARGUMENT DEFINITIONS:
!    ! na

!    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!    INTEGER retVal ! Return value, needed to catch return value of function call

!    INTEGER                        :: NumAlphas  = 0 ! Number of Alphas for each GetObjectItem call
!    INTEGER                        :: NumNumbers = 0 ! Number of Numbers for each GetObjectItem call
!    INTEGER                        :: IOStatus = 0  ! Used in GetObjectItem

!    cCurrentModuleObject='ExternalInterface'
!    CALL GetObjectItem(cCurrentModuleObject, 1, cAlphaArgs, NumAlphas, rNumericArgs, NumNumbers, IOStatus, &
!    AlphaFieldnames=cAlphaFieldNames, NumericFieldNames=cNumericFieldNames)
!    IF ((.NOT. SameString(cAlphaArgs(1), 'PtolemyServer')).AND.(.NOT. SameString(cAlphaArgs(1), 'FunctionalMockupUnitImport'))&
!    .AND.(.NOT. SameString(cAlphaArgs(1), 'FunctionalMockupUnitExport'))) THEN
!        CALL ShowSevereError('VerifyExternalInterfaceObject: '//trim(cCurrentModuleObject)//   &
!        ', invalid '//trim(cAlphaFieldNames(1))//'="'//trim(cAlphaArgs(1))//'".')
!        CALL ShowContinueError('only "PtolemyServer or FunctionalMockupUnitImport or FunctionalMockupUnitExport" allowed.')
!        ErrorsFound = .TRUE.
!    ENDIF

!    END SUBROUTINE VerifyExternalInterfaceObject



    !     NOTICE
    !
    !     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

    END MODULE ExternalInterface

