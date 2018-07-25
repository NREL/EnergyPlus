MODULE SetVersion

USE DataStringGlobals
USE DataVCompareGlobals

PUBLIC

CONTAINS

SUBROUTINE SetThisVersionVariables()
      VerString='Conversion 8.9 => 9.0'
      VersionNum=9.0
      sVersionNum='9.0'
      IDDFileNameWithPath=TRIM(ProgramPath)//'V8-9-0-Energy+.idd'
      NewIDDFileNameWithPath=TRIM(ProgramPath)//'V9-0-0-Energy+.idd'
      RepVarFileNameWithPath=TRIM(ProgramPath)//'Report Variables 8-9-0 to 9-0-0.csv'
END SUBROUTINE

END MODULE

SUBROUTINE CreateNewIDFUsingRules(EndOfFile,DiffOnly,InLfn,AskForInput,InputFileName,ArgFile,ArgIDFExtension)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2002
          !       MODIFIED       For each release
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates new IDFs based on the rules specified by
          ! developers.  This will result in a more complete transition but
          ! takes more time to create.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE DataVCompareGlobals
  USE VCompareGlobalRoutines
  USE DataStringGlobals, ONLY: ProgNameConversion
  USE General
  USE DataGlobals, ONLY: ShowMessage, ShowContinueError, ShowFatalError, ShowSevereError, ShowWarningError

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: EndOfFile
  LOGICAL, INTENT(IN)    :: DiffOnly
  INTEGER, INTENT(IN)    :: InLfn
  LOGICAL, INTENT(IN)    :: AskForInput
  CHARACTER(len=*), INTENT(IN) :: InputFileName
  LOGICAL, INTENT(IN)    :: ArgFile
  CHARACTER(len=*), INTENT(IN) :: ArgIDFExtension

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER IoS
  INTEGER DotPos
  INTEGER Status
  INTEGER NA
  INTEGER NN
  INTEGER CurArgs
  INTEGER DifLfn
  INTEGER xCount
  INTEGER Num
  INTEGER, EXTERNAL :: GetNewUnitNumber
  INTEGER Arg
  LOGICAL, SAVE :: FirstTime=.true.
  CHARACTER(len=30) UnitsArg
  CHARACTER(len=MaxNameLength) ::  ObjectName
  CHARACTER(len=30), EXTERNAL :: TrimTrailZeros
  CHARACTER(len=MaxNameLength) ::  UCRepVarName=blank
  CHARACTER(len=MaxNameLength) ::  UCCompRepVarName=blank
  LOGICAL DelThis
  INTEGER pos
  INTEGER pos2
  LOGICAL ExitBecauseBadFile
  LOGICAL StillWorking
  LOGICAL NoDiff
  LOGICAL checkrvi
  LOGICAL NoVersion
  LOGICAL DiffMinFields  ! Set to true when diff number of min-fields between the two objects
  LOGICAL Written
  INTEGER :: Var
  INTEGER :: CurVar
  LOGICAL ArgFileBeingDone
  LOGICAL LatestVersion
  CHARACTER(len=10) :: LocalFileExtension=' '
  LOGICAL :: WildMatch

  ! For exposed foundation perimeter objects
  INTEGER NumPerimObjs
  INTEGER PArgs
  INTEGER PerimNum
  CHARACTER(len=MaxNameLength*2), ALLOCATABLE, DIMENSION(:) :: PFldNames
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: PFldDefaults
  CHARACTER(len=20), ALLOCATABLE, DIMENSION(:) :: PFldUnits
  INTEGER PObjMinFlds
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: PAOrN
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: PReqFld
  INTEGER PNumArgs   ! Number of Arguments in a definition
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: POutArgs

  LOGICAL :: ConnComp
  LOGICAL :: ConnCompCtrl
  LOGICAL :: FileExist
  CHARACTER(len=MaxNameLength) :: CreatedOutputName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: DeleteThisRecord
  INTEGER :: COutArgs
  CHARACTER(len=16) :: UnitsField
  LOGICAL :: ScheduleTypeLimitsAnyNumber
  LOGICAL :: cycling
  LOGICAL :: continuous
  CHARACTER(len=MaxNameLength) :: OutScheduleName
  LOGICAL :: isDElightOutVar

  REAL :: GLHETempVal = 0.0
  INTEGER :: TempArgsNum = 0

  LOGICAL :: ErrFlag

  INTEGER :: I, CurField, NewField, KAindex=0, SearchNum
  INTEGER :: AlphaNumI
  REAL :: SaveNumber

  ! for Schedule:Compact from 8.8 to 8.9
  CHARACTER(len=MaxNameLength) ::  UpperInArg=blank

  ! for new dx cooling coil from 8.8 to 8.9
  CHARACTER(len=MaxNameLength) ::  CoolingCoilType=blank
  INTEGER :: SpeedNum = 0
  CHARACTER(len=2) :: SpeedNumChar=blank
  INTEGER :: NumberOfSpeeds = 1
  INTEGER :: ModeNum = 0
  CHARACTER(len=16) :: ModeName=blank
  INTEGER :: NumberOfModes = 1
  REAL :: DXTempValue1 = 0.0
  REAL :: DXTempValue2 = 0.0
  REAL :: DXRatio = 0.0
  REAL :: DXSpeedRatio = 0.0
  INTEGER :: DXSpeedStartArgNum = 0
  INTEGER :: DXNomSpeedStartArgNum = 0
  TYPE MultiStageDXPerformanceType
    CHARACTER(len=MaxNameLength) :: Name=blank
    CHARACTER(len=MaxNameLength) :: GrossRatedTotCoolCap=blank
    CHARACTER(len=MaxNameLength) :: GrossRatedSHR=blank
    CHARACTER(len=MaxNameLength) :: GrossRatedCOP=blank
    CHARACTER(len=MaxNameLength) :: RatedAirFlowRate=blank
    CHARACTER(len=MaxNameLength) :: FractionBypassed=blank
    CHARACTER(len=MaxNameLength) :: TotCapFTCurveName=blank
    CHARACTER(len=MaxNameLength) :: TotCapFFCurveName=blank
    CHARACTER(len=MaxNameLength) :: EIRFTCurveName=blank
    CHARACTER(len=MaxNameLength) :: EIRFFCurveName=blank
    CHARACTER(len=MaxNameLength) :: PLFCurveName=blank
    CHARACTER(len=MaxNameLength) :: NomTimeForCondensRemoval=blank
    CHARACTER(len=MaxNameLength) :: RatioOfInitialMoisture=blank
    CHARACTER(len=MaxNameLength) :: MaxCyclingRate=blank
    CHARACTER(len=MaxNameLength) :: LatentCapTimeConst=blank
    CHARACTER(len=MaxNameLength) :: CondInletNode=blank
    CHARACTER(len=MaxNameLength) :: CondType=blank
    CHARACTER(len=MaxNameLength) :: EvapCondEff=blank
    CHARACTER(len=MaxNameLength) :: EvapCondAirFlowRate=blank
    CHARACTER(len=MaxNameLength) :: EvapCondPumpPower=blank
    CHARACTER(len=MaxNameLength) :: SHRFTCurveName=blank
    CHARACTER(len=MaxNameLength) :: SHRFFCurveName=blank
  END TYPE
  TYPE (MultiStageDXPerformanceType), DIMENSION(:), ALLOCATABLE :: MultiStageDXPerformance
  INTEGER :: NumMultiStageDXPerf = 0
  INTEGER :: iDXPerf = 0
  INTEGER :: iDXNomPerf = 0
  INTEGER :: iDXStg1Perf = 0
  CHARACTER(len=MaxNameLength) :: DXNomPerfName=blank
  CHARACTER(len=MaxNameLength) :: DXStg1PerfName=blank

  If (FirstTime) THEN  ! do things that might be applicable only to this new version
    FirstTime=.false.
  EndIf

  StillWorking=.true.
  ArgFileBeingDone=.false.
  LatestVersion=.false.
  NoVersion=.true.
  LocalFileExtension=ArgIDFExtension
  EndOfFile=.false.
  IOS=0

  DO WHILE (StillWorking)

    ExitBecauseBadFile=.false.
    DO WHILE (.not. EndOfFile)
      IF (AskForInput) THEN
        WRITE(*,*) 'Enter input file name, with path'
        write(*,fmta,advance='no') '-->'
        READ(*,fmta) FullFileName
      ELSE
        IF (.not. ArgFile) THEN
          READ(InLfn,*,IOSTAT=IoS) FullFileName
        ELSEIF (.not. ArgFileBeingDone) THEN
          FullFileName=InputFileName
          IOS=0
          ArgFileBeingDone=.true.
        ELSE
          FullFileName=Blank
          IOS=1
        ENDIF
        IF (FullFileName(1:1) == '!') THEN
          FullFileName=Blank
          CYCLE
        ENDIF
      ENDIF
      UnitsArg=Blank
      IF (IoS /= 0) FullFileName=Blank
      FullFileName=ADJUSTL(FullFileName)
      IF (FullFileName /= Blank) THEN
        CALL DisplayString('Processing IDF -- '//TRIM(FullFileName))
        WRITE(Auditf,fmta) ' Processing IDF -- '//TRIM(FullFileName)
        DotPos=SCAN(FullFileName,'.',.true.) ! Scan backward looking for extension,
        IF (DotPos /= 0) THEN
          FileNamePath=FullFileName(1:DotPos-1)
          LocalFileExtension=MakeLowerCase(FullFileName(DotPos+1:))
        ELSE
          FileNamePath=FullFileName
          WRITE(*,*) ' assuming file extension of .idf'
          WRITE(Auditf,fmta) ' ..assuming file extension of .idf'
          FullFileName=TRIM(FullFileName)//'.idf'
          LocalFileExtension='idf'
        ENDIF
        ! Process the old input
        DifLfn=GetNewUnitNumber()
        INQUIRE(File=TRIM(FullFileName),EXIST=FileOK)
        IF (.not. FileOK) THEN
          WRITE(*,*) 'File not found='//TRIM(FullFileName)
          WRITE(Auditf,*) 'File not found='//TRIM(FullFileName)
          EndOfFile=.true.
          ExitBecauseBadFile=.true.
          EXIT
        ENDIF
        IF (LocalFileExtension == 'idf' .or. LocalFileExtension == 'imf') THEN
          checkrvi=.false.
          ConnComp=.false.
          ConnCompCtrl=.false.
          IF (DiffOnly) THEN
            OPEN(DifLfn,FILE=TRIM(FileNamePath)//'.'//TRIM(LocalFileExtension)//'dif')
          ELSE
            OPEN(DifLfn,FILE=TRIM(FileNamePath)//'.'//TRIM(LocalFileExtension)//'new')
          ENDIF
          IF (LocalFileExtension == 'imf') THEN
            CALL ShowWarningError('Note: IMF file being processed.  No guarantee of perfection.  Please check new file carefully.',Auditf)
            ProcessingIMFFile=.true.
          ELSE
            ProcessingIMFFile=.false.
          ENDIF
          CALL ProcessInput(IDDFileNameWithPath,NewIDDFileNameWithPath,FullFileName)
          IF (FatalError) THEN
            ExitBecauseBadFile=.true.
            EXIT
          ENDIF

          ! Clean up from any previous passes, then re-allocate
          IF(ALLOCATED(DeleteThisRecord)) DEALLOCATE(DeleteThisRecord)
          IF(ALLOCATED(Alphas)) DEALLOCATE(Alphas)
          IF(ALLOCATED(Numbers)) DEALLOCATE(Numbers)
          IF(ALLOCATED(InArgs)) DEALLOCATE(InArgs)
          IF(ALLOCATED(TempArgs)) DEALLOCATE(TempArgs)
          IF(ALLOCATED(AorN)) DEALLOCATE(AorN)
          IF(ALLOCATED(ReqFld)) DEALLOCATE(ReqFld)
          IF(ALLOCATED(FldNames)) DEALLOCATE(FldNames)
          IF(ALLOCATED(FldDefaults)) DEALLOCATE(FldDefaults)
          IF(ALLOCATED(FldUnits)) DEALLOCATE(FldUnits)
          IF(ALLOCATED(NwAorN)) DEALLOCATE(NwAorN)
          IF(ALLOCATED(NwReqFld)) DEALLOCATE(NwReqFld)
          IF(ALLOCATED(NwFldNames)) DEALLOCATE(NwFldNames)
          IF(ALLOCATED(NwFldDefaults)) DEALLOCATE(NwFldDefaults)
          IF(ALLOCATED(NwFldUnits)) DEALLOCATE(NwFldUnits)
          IF(ALLOCATED(OutArgs)) DEALLOCATE(OutArgs)
          IF(ALLOCATED(PAorN)) DEALLOCATE(PAorN)
          IF(ALLOCATED(PReqFld)) DEALLOCATE(PReqFld)
          IF(ALLOCATED(PFldNames)) DEALLOCATE(PFldNames)
          IF(ALLOCATED(PFldDefaults)) DEALLOCATE(PFldDefaults)
          IF(ALLOCATED(PFldUnits)) DEALLOCATE(PFldUnits)
          IF(ALLOCATED(POutArgs)) DEALLOCATE(POutArgs)
          IF(ALLOCATED(MatchArg)) DEALLOCATE(MatchArg)
          ALLOCATE(Alphas(MaxAlphaArgsFound),Numbers(MaxNumericArgsFound))
          ALLOCATE(InArgs(MaxTotalArgs))
          ALLOCATE(TempArgs(MaxTotalArgs))
          ALLOCATE(AorN(MaxTotalArgs),ReqFld(MaxTotalArgs),FldNames(MaxTotalArgs),FldDefaults(MaxTotalArgs),FldUnits(MaxTotalArgs))
          ALLOCATE(NwAorN(MaxTotalArgs),NwReqFld(MaxTotalArgs),NwFldNames(MaxTotalArgs),NwFldDefaults(MaxTotalArgs),NwFldUnits(MaxTotalArgs))
          ALLOCATE(PAorN(MaxTotalArgs),PReqFld(MaxTotalArgs),PFldNames(MaxTotalArgs),PFldDefaults(MaxTotalArgs),PFldUnits(MaxTotalArgs))
          ALLOCATE(OutArgs(MaxTotalArgs))
          ALLOCATE(POutArgs(MaxTotalArgs))
          ALLOCATE(MatchArg(MaxTotalArgs))
          ALLOCATE(DeleteThisRecord(NumIDFRecords))
          DeleteThisRecord=.false.

          NoVersion=.true.
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'VERSION') CYCLE
            NoVersion=.false.
            EXIT
          ENDDO

          ScheduleTypeLimitsAnyNumber=.false.
          DO Num=1,NumIDFRecords
            IF (.not. SameString(IDFRecords(Num)%Name,'ScheduleTypeLimits')) CYCLE
            IF (.not. SameString(IDFRecords(Num)%Alphas(1),'Any Number')) CYCLE
            ScheduleTypeLimitsAnyNumber=.true.
            EXIT
          ENDDO

          DO Num=1,NumIDFRecords
            IF (DeleteThisRecord(Num)) THEN
              Write(DifLfn,fmta) '! Deleting: '//TRIM(IDFRecords(Num)%Name)//'="'//TRIM(IDFRecords(Num)%Alphas(1))//'".'
            ENDIF
          ENDDO

          ! PREPROCESSING FOR COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE
          !
          IF (.NOT. ALLOCATED(MultiStageDXPerformance)) THEN
            ! count number of CoilPerformance:DX:Cooling objects
            DO Num=1,NumIDFRecords
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COILPERFORMANCE:DX:COOLING') THEN
                NumMultiStageDXPerf = NumMultiStageDXPerf + 1
              ENDIF
            ENDDO
            ALLOCATE(MultiStageDXPerformance(NumMultiStageDXPerf))
            ! read the CoilPerformance:DX:Cooling object into the array for later use with Coil:Cooling:DX:TwoStageWithHumidityControlMode
            iDXPerf = 0
            DO Num=1,NumIDFRecords
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COILPERFORMANCE:DX:COOLING') THEN
                iDXPerf = iDXPerf + 1
                MultiStageDXPerformance(iDXPerf)%Name=IDFRecords(Num)%Alphas(1)
                MultiStageDXPerformance(iDXPerf)%GrossRatedTotCoolCap=IDFRecords(Num)%Numbers(1)
                IF (IDFRecords(Num)%NumNumbers .GE. 2) MultiStageDXPerformance(iDXPerf)%GrossRatedSHR=IDFRecords(Num)%Numbers(2)
                IF (IDFRecords(Num)%NumNumbers .GE. 3) MultiStageDXPerformance(iDXPerf)%GrossRatedCOP=IDFRecords(Num)%Numbers(3)
                IF (IDFRecords(Num)%NumNumbers .GE. 4) MultiStageDXPerformance(iDXPerf)%RatedAirFlowRate=IDFRecords(Num)%Numbers(4)
                IF (IDFRecords(Num)%NumNumbers .GE. 5) MultiStageDXPerformance(iDXPerf)%FractionBypassed=IDFRecords(Num)%Numbers(5)
                IF (IDFRecords(Num)%NumAlphas .GE. 2) MultiStageDXPerformance(iDXPerf)%TotCapFTCurveName=IDFRecords(Num)%Alphas(2)
                IF (IDFRecords(Num)%NumAlphas .GE. 3) MultiStageDXPerformance(iDXPerf)%TotCapFFCurveName=IDFRecords(Num)%Alphas(3)
                IF (IDFRecords(Num)%NumAlphas .GE. 4) MultiStageDXPerformance(iDXPerf)%EIRFTCurveName=IDFRecords(Num)%Alphas(4)
                IF (IDFRecords(Num)%NumAlphas .GE. 5) MultiStageDXPerformance(iDXPerf)%EIRFFCurveName=IDFRecords(Num)%Alphas(5)
                IF (IDFRecords(Num)%NumAlphas .GE. 6) MultiStageDXPerformance(iDXPerf)%PLFCurveName=IDFRecords(Num)%Alphas(6)
                IF (IDFRecords(Num)%NumNumbers .GE. 6) MultiStageDXPerformance(iDXPerf)%NomTimeForCondensRemoval=IDFRecords(Num)%Numbers(6)
                IF (IDFRecords(Num)%NumNumbers .GE. 7) MultiStageDXPerformance(iDXPerf)%RatioOfInitialMoisture=IDFRecords(Num)%Numbers(7)
                IF (IDFRecords(Num)%NumNumbers .GE. 8) MultiStageDXPerformance(iDXPerf)%MaxCyclingRate=IDFRecords(Num)%Numbers(8)
                IF (IDFRecords(Num)%NumNumbers .GE. 9) MultiStageDXPerformance(iDXPerf)%LatentCapTimeConst=IDFRecords(Num)%Numbers(9)
                IF (IDFRecords(Num)%NumAlphas .GE. 7) MultiStageDXPerformance(iDXPerf)%CondInletNode=IDFRecords(Num)%Alphas(7)
                IF (IDFRecords(Num)%NumAlphas .GE. 8) MultiStageDXPerformance(iDXPerf)%CondType=IDFRecords(Num)%Alphas(8)
                IF (IDFRecords(Num)%NumNumbers .GE. 10) MultiStageDXPerformance(iDXPerf)%EvapCondEff=IDFRecords(Num)%Numbers(10)
                IF (IDFRecords(Num)%NumNumbers .GE. 11) MultiStageDXPerformance(iDXPerf)%EvapCondAirFlowRate=IDFRecords(Num)%Numbers(11)
                IF (IDFRecords(Num)%NumNumbers .GE. 12) MultiStageDXPerformance(iDXPerf)%EvapCondPumpPower=IDFRecords(Num)%Numbers(12)
                IF (IDFRecords(Num)%NumAlphas .GE. 9) MultiStageDXPerformance(iDXPerf)%SHRFTCurveName=IDFRecords(Num)%Alphas(9)
                IF (IDFRecords(Num)%NumAlphas .GE. 10) MultiStageDXPerformance(iDXPerf)%SHRFFCurveName=IDFRecords(Num)%Alphas(10)

!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%Name='//TRIM(MultiStageDXPerformance(iDXPerf)%Name),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%GrossRatedTotCoolCap='//TRIM(MultiStageDXPerformance(iDXPerf)%GrossRatedTotCoolCap),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%GrossRatedSHR='//TRIM(MultiStageDXPerformance(iDXPerf)%GrossRatedSHR),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%GrossRatedCOP='//TRIM(MultiStageDXPerformance(iDXPerf)%GrossRatedCOP),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%RatedAirFlowRate='//TRIM(MultiStageDXPerformance(iDXPerf)%RatedAirFlowRate),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%FractionBypassed='//TRIM(MultiStageDXPerformance(iDXPerf)%FractionBypassed),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%TotCapFTCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%TotCapFTCurveName),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%TotCapFFCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%TotCapFFCurveName),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%EIRFTCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%EIRFTCurveName),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%EIRFFCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%EIRFFCurveName),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%PLFCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%PLFCurveName),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%NomTimeForCondensRemoval='//TRIM(MultiStageDXPerformance(iDXPerf)%NomTimeForCondensRemoval),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%RatioOfInitialMoisture='//TRIM(MultiStageDXPerformance(iDXPerf)%RatioOfInitialMoisture),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%MaxCyclingRate='//TRIM(MultiStageDXPerformance(iDXPerf)%MaxCyclingRate),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%LatentCapTimeConst='//TRIM(MultiStageDXPerformance(iDXPerf)%LatentCapTimeConst),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%CondInletNode='//TRIM(MultiStageDXPerformance(iDXPerf)%CondInletNode),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%CondType='//TRIM(MultiStageDXPerformance(iDXPerf)%CondType),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%EvapCondEff='//TRIM(MultiStageDXPerformance(iDXPerf)%EvapCondEff),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%EvapCondAirFlowRate='//TRIM(MultiStageDXPerformance(iDXPerf)%EvapCondAirFlowRate),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%EvapCondPumpPower='//TRIM(MultiStageDXPerformance(iDXPerf)%EvapCondPumpPower),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%SHRFTCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%SHRFTCurveName),Auditf)
!                CALL ShowWarningError('MultiStageDXPerformance(iDXPerf)%SHRFFCurveName='//TRIM(MultiStageDXPerformance(iDXPerf)%SHRFFCurveName),Auditf)
              ENDIF
            ENDDO
          ENDIF

          DO Num=1,NumIDFRecords

            IF (DeleteThisRecord(Num)) CYCLE
            DO xcount=IDFRecords(Num)%CommtS+1,IDFRecords(Num)%CommtE
              WRITE(DifLfn,fmta) TRIM(Comments(xcount))
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ''
            ENDDO
            IF (NoVersion .and. Num == 1) THEN
              CALL GetNewObjectDefInIDD('VERSION',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
              OutArgs(1) = sVersionNum
              CurArgs=1
              CALL WriteOutIDFLinesAsComments(DifLfn,'Version',CurArgs,OutArgs,NwFldNames,NwFldUnits)
            ENDIF

     ! deleted objects.  no transition.
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'PROGRAMCONTROL') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'SKY RADIANCE DISTRIBUTION') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'AIRFLOW MODEL') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'GENERATOR:FC:BATTERY DATA') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'AIRFLOWNETWORK:MULTIZONE:SITEWINDCONDITIONS') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'WATER HEATER:SIMPLE') THEN
              WRITE(DifLfn,fmta) '! ** The WATER HEATER:SIMPLE object has been deleted'
              CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning','The WATER HEATER:SIMPLE object has been deleted')
              CYCLE
            ENDIF

            ObjectName=IDFRecords(Num)%Name
            IF (FindItemInList(ObjectName,ObjectDef%Name,NumObjectDefs) /= 0) THEN
              CALL GetObjectDefInIDD(ObjectName,NumArgs,AorN,ReqFld,ObjMinFlds,FldNames,FldDefaults,FldUnits)
              NumAlphas=IDFRecords(Num)%NumAlphas
              NumNumbers=IDFRecords(Num)%NumNumbers
              Alphas(1:NumAlphas)=IDFRecords(Num)%Alphas(1:NumAlphas)
              Numbers(1:NumNumbers)=IDFRecords(Num)%Numbers(1:NumNumbers)
              CurArgs=NumAlphas+NumNumbers
              InArgs=Blank
              OutArgs=Blank
              TempArgs=Blank
              NA=0
              NN=0
              DO Arg=1,CurArgs
                IF (AorN(Arg)) THEN
                  NA=NA+1
                  InArgs(Arg)=Alphas(NA)
                ELSE
                  NN=NN+1
                  InArgs(Arg)=Numbers(NN)
                ENDIF
              ENDDO
            ELSE
              WRITE(Auditf,fmta) 'Object="'//TRIM(ObjectName)//'" does not seem to be on the "old" IDD.'
              WRITE(Auditf,fmta) '... will be listed as comments (no field names) on the new output file.'
              WRITE(Auditf,fmta) '... Alpha fields will be listed first, then numerics.'
              NumAlphas=IDFRecords(Num)%NumAlphas
              NumNumbers=IDFRecords(Num)%NumNumbers
              Alphas(1:NumAlphas)=IDFRecords(Num)%Alphas(1:NumAlphas)
              Numbers(1:NumNumbers)=IDFRecords(Num)%Numbers(1:NumNumbers)
              DO Arg=1,NumAlphas
                OutArgs(Arg)=Alphas(Arg)
              ENDDO
              NN=NumAlphas+1
              DO Arg=1,NumNumbers
                OutArgs(NN)=Numbers(Arg)
                NN=NN+1
              ENDDO
              CurArgs=NumAlphas+NumNumbers
              NwFldNames=Blank
              NwFldUnits=Blank
              CALL WriteOutIDFLinesAsComments(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
              CYCLE
            ENDIF

            Nodiff=.true.       ! Nodiff is true by default
            DiffMinFields=.false.
            Written=.false.

            IF (FindItemInList(MakeUPPERCase(ObjectName),NotInNew,SIZE(NotInNew)) == 0) THEN
              CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
              ! Check minfields
              IF (ObjMinFlds /= NwObjMinFlds) THEN
                DiffMinFields=.true.
              ELSE
                DiffMinFields=.false.
              ENDIF
            ENDIF

            IF (.not. MakingPretty) THEN

              SELECT CASE (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)))

              CASE ('VERSION')
                IF ((InArgs(1)(1:3)) == sVersionNum .and. ArgFile) THEN
                  CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                  CLOSE(diflfn,STATUS='DELETE')
                  LatestVersion=.true.
                  EXIT
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1) = sVersionNum
                nodiff=.false.

    ! changes for this version, pick one of the spots to add rules, this will reduce the possibility of merge conflicts

!             CASE('OBJECTNAMEHERE')
!                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                 nodiff=.false.
!                 OutArgs(1)=InArgs(1)
!                 OutArgs(2) = 'SequentialLoad'
!                 OutArgs(3:CurArgs+1)=InArgs(2:CurArgs)
!                 CurArgs = CurArgs + 1

              ! If your original object starts with A, insert the rules here

             CASE('AIRLOOPHVAC:UNITARYSYSTEM')
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                 nodiff=.true.
                 ! replace cooling coil object type name
                 CoolingCoilType = InArgs(15)
                 IF ( SameString( CoolingCoilType(1:15), "Coil:Cooling:DX" ) ) THEN
                     OutArgs(15) = "Coil:Cooling:DX"
                 END IF

              ! If your original object starts with B, insert the rules here

              ! If your original object starts with C, insert the rules here

             CASE('COILSYSTEM:COOLING:DX:HEATEXCHANGERASSISTED')
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                 nodiff=.true.
                 ! replace cooling coil object type name
                 CoolingCoilType = InArgs(4)
                 IF ( SameString( CoolingCoilType(1:15), "Coil:Cooling:DX" ) ) THEN
                     OutArgs(4) = "Coil:Cooling:DX"
                 END IF

             CASE('COIL:COOLING:DX:SINGLESPEED')
                 nodiff=.false.
                 ObjectName='Coil:Cooling:DX'
                 ! store the date for later
                 TempArgs=InArgs
                 TempArgsNum=CurArgs

                 ! write the Coil:Cooling:DX object
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TempArgs(1) ! Name
                 OutArgs(2)=TempArgs(8) ! Evaporator Inlet Node Name
                 OutArgs(3)=TempArgs(9) ! Evaporator Outlet Node Name
                 OutArgs(4)=TempArgs(2) ! Availability Schedule Name
                 OutArgs(5)=TempArgs(35) ! Condenser Zone Name
                 OutArgs(6)=TempArgs(20) ! Condenser Inlet Node Name
                 OutArgs(7)=TRIM(TempArgs(1)) // TRIM(' Condenser Outlet Node') ! Condenser Outlet Node Name
                 OutArgs(8)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Performance Object Name
                 OutArgs(9)=TempArgs(28) ! Condensate Collection Water Storage Tank Name
                 OutArgs(10)=TempArgs(27) ! Evaporative Condenser Supply Water Storage Tank Name
                 CurArgs=10
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Performance'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Name
                 OutArgs(2)=TempArgs(25) ! Crankcase Heater Capacity
                 OutArgs(3)=TempArgs(15) ! Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
                 OutArgs(4)=TempArgs(26) ! Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation
                 !OutArgs(5)='' ! Unit Internal Static Air Pressure - Only defined for Coil:Cooling:DX:TwoSpeed previously
                 !OutArgs(6)='' ! Method for Switching Operating Modes - Only defined for Coil:Cooling:DX:TwoStageWithHumidityControlMode previously
                 !OutArgs(7)='' ! Operating Mode Number Schedule Name
                 OutArgs(8)=TempArgs(29) ! Evaporative Condenser Basin Heater Capacity
                 OutArgs(9)=TempArgs(30) ! Evaporative Condenser Basin Heater Setpoint Temperature
                 OutArgs(10)=TempArgs(31) ! Evaporative Condenser Basin Heater Operating Schedule Name
                 OutArgs(11)='Electricity' ! Compressor Fuel Type
                 OutArgs(12)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Operating Mode 1 Name
                 CurArgs=12
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Operating Mode object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:OperatingMode'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Name
                 OutArgs(2)=TempArgs(3) ! Rated Gross Total Cooling Capacity
                 OutArgs(3)=TempArgs(6) ! Rated Evaporator Air Flow Rate
                 OutArgs(4)=TempArgs(23) ! Rated Condenser Air Flow Rate
                 OutArgs(5)=TempArgs(18) ! Maximum Cycling Rate
                 OutArgs(6)=TempArgs(17) ! Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity
                 OutArgs(7)=TempArgs(19) ! Latent Capacity Time Constant
                 OutArgs(8)=TempArgs(16) ! Nominal Time for Condensate Removal to Begin
                 !OutArgs(9)='' ! Apply Latent Degradation to Speeds Greater than 1 - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 OutArgs(10)=TempArgs(21) ! Condenser Type
                 OutArgs(11)=TempArgs(24) ! Nominal Evaporative Condenser Pump Power
                 !OutArgs(12)='' ! Capacity Control Method
                 OutArgs(13)='1' ! Nominal Speed Number
                 OutArgs(14)=TRIM(TempArgs(1)) // TRIM(' Speed 1 Performance') ! Speed 1 Name
                 CurArgs=14
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Speed Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Speed 1 Performance') ! Name
                 OutArgs(2)='1.0' ! Gross Total Cooling Capacity Fraction
                 OutArgs(3)='1.0' ! Evaporator Air Flow Rate Fraction
                 OutArgs(4)='1.0' ! Condenser Air Flow Rate Fraction
                 OutArgs(5)=TempArgs(4) ! Gross Sensible Heat Ratio
                 OutArgs(6)=TempArgs(5) ! Gross Cooling COP
                 OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                 OutArgs(8)=TempArgs(7) ! Rated Evaporator Fan Power Per Volume Flow Rate
                 OutArgs(9)='1.0' ! Evaporative Condenser Pump Power Fraction
                 OutArgs(10)=TempArgs(22) ! Evaporative Condenser Effectiveness
                 OutArgs(11)=TempArgs(10) ! Total Cooling Capacity Function of Temperature Curve Name
                 OutArgs(12)=TempArgs(11) ! Total Cooling Capacity Function of Air Flow Fraction Curve Name
                 OutArgs(13)=TempArgs(12) ! Energy Input Ratio Function of Temperature Curve Name
                 OutArgs(14)=TempArgs(13) ! Energy Input Ratio Function of Air Flow Fraction Curve Name
                 OutArgs(15)=TempArgs(14) ! Part Load Fraction Correlation Curve Name
                 !OutArgs(16)='' ! Rated Waste Heat Fraction of Power Input - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 !OutArgs(17)='' ! Waste Heat Function of Temperature Curve Name - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 OutArgs(18)=TempArgs(32) ! Sensible Heat Ratio Modifier Function of Temperature Curve Name
                 OutArgs(19)=TempArgs(33) ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name
                 CurArgs=19
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! already written
                 Written = .true.
    
             CASE('COIL:COOLING:DX:TWOSPEED')
                 nodiff=.false.
                 ObjectName='Coil:Cooling:DX'
                 ! store the date for later
                 TempArgs=InArgs
                 TempArgsNum=CurArgs

                 ! write the Coil:Cooling:DX object
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TempArgs(1) ! Name
                 OutArgs(2)=TempArgs(8) ! Evaporator Inlet Node Name
                 OutArgs(3)=TempArgs(9) ! Evaporator Outlet Node Name
                 OutArgs(4)=TempArgs(2) ! Availability Schedule Name
                 OutArgs(5)=TempArgs(39) ! Condenser Zone Name - Coil:Cooling:DX:VariableSpeed never had this
                 OutArgs(6)=TempArgs(21) ! Condenser Inlet Node Name
                 OutArgs(7)=TRIM(TempArgs(1)) // TRIM(' Condenser Outlet Node') ! Condenser Outlet Node Name
                 OutArgs(8)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Performance Object Name
                 OutArgs(9)=TempArgs(31) ! Condensate Collection Water Storage Tank Name
                 OutArgs(10)=TempArgs(30) ! Evaporative Condenser Supply Water Storage Tank Name
                 CurArgs=10
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Performance'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Name
                 OutArgs(2)='0.0' ! Crankcase Heater Capacity - Coil:Cooling:DX:TwoSpeed never had this
                 OutArgs(3)=TempArgs(23) ! Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
                 !OutArgs(4)='' ! Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation - Coil:Cooling:DX:TwoSpeed never had this
                 !OutArgs(5)=TempArgs(7) ! Unit Internal Static Air Pressure
                 !OutArgs(6)='' ! Method for Switching Operating Modes - Only defined for Coil:Cooling:DX:TwoStageWithHumidityControl previously
                 !OutArgs(7)='' ! Operating Mode Number Schedule Name
                 OutArgs(8)=TempArgs(32) ! Evaporative Condenser Basin Heater Capacity
                 OutArgs(9)=TempArgs(33) ! Evaporative Condenser Basin Heater Setpoint Temperature
                 OutArgs(10)=TempArgs(34) ! Evaporative Condenser Basin Heater Operating Schedule Name
                 OutArgs(11)='Electricity' ! Compressor Fuel Type
                 OutArgs(12)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Operating Mode 1 Name
                 CurArgs=12
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Operating Mode object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:OperatingMode'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Name
                 OutArgs(2)=TempArgs(3) ! Rated Gross Total Cooling Capacity
                 OutArgs(3)=TempArgs(6) ! Rated Evaporator Air Flow Rate
                 !OutArgs(4)=TempArgs(25) ! Rated Condenser Air Flow Rate
                 !OutArgs(5)='' ! Maximum Cycling Rate - Coil:Cooling:DX:TwoSpeed never had this
                 !OutArgs(6)='' ! Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity - Coil:Cooling:DX:TwoSpeed never had this
                 !OutArgs(7)='' ! Latent Capacity Time Constant - Coil:Cooling:DX:TwoSpeed never had this
                 !OutArgs(8)='' ! Nominal Time for Condensate Removal to Begin - Coil:Cooling:DX:TwoSpeed never had this
                 !OutArgs(9)='' ! Apply Latent Degradation to Speeds Greater than 1 - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 OutArgs(10)=TempArgs(22) ! Condenser Type
                 OutArgs(11)=TempArgs(26) ! Nominal Evaporative Condenser Pump Power
                 OutArgs(12)='MultiSpeed' ! Capacity Control Method

                 ErrFlag=.false.
                 NumberOfSpeeds=2

                 OutArgs(13)='2' ! Nominal Speed Number
                 CurArgs=13
                 DO SpeedNum=1,NumberOfSpeeds 
                   CurArgs=CurArgs+1
                   SpeedNumChar=RoundSigDigits(SpeedNum,0)
                   OutArgs(CurArgs)=TRIM(TempArgs(1)) // ' Speed ' // TRIM(SpeedNumChar) //' Performance' ! Speed n Name
                 ENDDO
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Low Speed Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // ' Speed 1 Performance' ! Name

                 ! Capacity is scaled by the capacity at high speed - but only if it's not autosized
                 ErrFlag=.false.
                 IF (SameString(TempArgs(3), 'AUTOSIZE') .OR. SameString(TempArgs(15), 'AUTOSIZE')) THEN
                   ! If the high or low speed capacity is autosized then use old default ratio of 0.3333 for low speed fraction
                   OutArgs(2)='0.3333' ! Gross Total Cooling Capacity Fraction
                   CALL ShowWarningError( &
                     'Coil:Cooling:DX:TwoSpeed (old)='//trim(TempArgs(1))//  &
                     ' Low Speed Gross Rated Total Cooling Capacity='//trim(TempArgs(15))//' was replaced with default sizing fraction.'//  &
                     ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed 1 Performance, Gross Total Cooling Capacity Fraction = 0.3333.',Auditf)
                 ELSE
                   DXTempValue1 = ProcessNumber(TempArgs(15),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(15))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   ErrFlag=.false.
                   DXTempValue2 = ProcessNumber(TempArgs(3),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(3))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                     DXRatio = DXTempValue1/DXTempValue2
                     OutArgs(2)=RoundSigDigits(DXRatio,4) ! Gross Total Cooling Capacity Fraction
                   ELSE
                     OutArgs(2)='' ! Gross Total Cooling Capacity Fraction
                   ENDIF
                 ENDIF

                 ! Evaporator air flow is scaled by the flow at high speed - but only if it's not autosized
                 ErrFlag=.false.
                 IF (SameString(TempArgs(6), 'AUTOSIZE') .OR. SameString(TempArgs(18), 'AUTOSIZE')) THEN
                   ! If the high or low speed air flow is autosized then use old default ratio of 0.3333 for low speed fraction
                   OutArgs(3)='0.3333' ! Evaporator Air Flow Rate Fraction
                   CALL ShowWarningError( &
                     'Coil:Cooling:DX:TwoSpeed (old)='//trim(TempArgs(1))//  &
                     ' Low Speed Gross Rated Air Flow Rate='//trim(TempArgs(18))//' was replaced with default sizing fraction.'//  &
                     ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed 1 Performance, Evaporator Air Flow Rate Fraction = 0.3333.',Auditf)
                 ELSE
                   DXTempValue1 = ProcessNumber(TempArgs(18),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(18))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   ErrFlag=.false.
                   DXTempValue2 = ProcessNumber(TempArgs(6),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(6))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                     DXRatio = DXTempValue1/DXTempValue2
                     OutArgs(3)=RoundSigDigits(DXRatio,4) ! Evaporator Air Flow Rate Fraction
                   ELSE
                     OutArgs(3)='' ! Evaporator Air Flow Rate Fraction
                   ENDIF
                   ENDIF
                 
                 ! Condenser air flow is scaled by the flow at high speed
                 ErrFlag=.false.
                 IF (SameString(TempArgs(25), 'AUTOSIZE') .OR. SameString(TempArgs(28), 'AUTOSIZE')) THEN
                   ! If the high or low speed air flow is autosized then use old default ratio of 0.3333 for low speed fraction
                   OutArgs(4)='0.3333' ! Evaporator Air Flow Rate Fraction
                   CALL ShowWarningError( &
                     'Coil:Cooling:DX:TwoSpeed (old)='//trim(TempArgs(1))//  &
                     ' Low Speed Evaporative Condenser Air Flow Rate='//trim(TempArgs(28))//' was replaced with default sizing fraction.'//  &
                     ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed 1 Performance, Condenser Air Flow Rate Fraction = 0.3333.',Auditf)
                 ELSE
                   DXTempValue1 = ProcessNumber(TempArgs(28),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(28))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   ErrFlag=.false.
                   DXTempValue2 = ProcessNumber(TempArgs(25),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(25))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                     DXRatio = DXTempValue1/DXTempValue2
                     OutArgs(4)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                   ELSE
                     OutArgs(4)='' ! Condenser Air Flow Rate Fraction
                   ENDIF
                 ENDIF

                 OutArgs(5)=TempArgs(16) ! Gross Sensible Heat Ratio
                 OutArgs(6)=TempArgs(17) ! Gross Cooling COP
                 OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                 !OutArgs(8)='' ! Rated Evaporator Fan Power Per Volume Flow Rate - Coil:Cooling:DX:TwoSpeed never had this

                 ! Evaporative Condenser Pump Power is scaled by the power at high speed
                 ErrFlag=.false.
                 IF (SameString(TempArgs(26), 'AUTOSIZE') .OR. SameString(TempArgs(29), 'AUTOSIZE')) THEN
                   ! If the high or low speed air flow is autosized then use old default ratio of 0.3333 for low speed fraction
                   OutArgs(9)='0.3333' ! Evaporator Air Flow Rate Fraction
                   CALL ShowWarningError( &
                     'Coil:Cooling:DX:TwoSpeed (old)='//trim(TempArgs(1))//  &
                     ' Low Speed Evaporative Condenser Air Flow Rate='//trim(TempArgs(29))//' was replaced with default sizing fraction.'//  &
                     ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed 1 Performance, Condenser Pump Power Fraction = 0.3333.',Auditf)
                 ELSE
                   DXTempValue1 = ProcessNumber(TempArgs(29),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(29))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   ErrFlag=.false.
                   DXTempValue2 = ProcessNumber(TempArgs(26),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoSpeed field, ['//  &
                        trim(TempArgs(26))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                   IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                     DXRatio = DXTempValue1/DXTempValue2
                     OutArgs(9)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                   ELSE
                     OutArgs(9)='' ! Evaporative Condenser Pump Power Fraction
                   ENDIF
                 ENDIF

                 OutArgs(10)=TempArgs(27) ! Evaporative Condenser Effectiveness
                 OutArgs(11)=TempArgs(19) ! Total Cooling Capacity Function of Temperature Curve Name
                 OutArgs(12)=TempArgs(11) ! Total Cooling Capacity Function of Air Flow Fraction Curve Name - use the High Speed Curve here?
                 OutArgs(13)=TempArgs(20) ! Energy Input Ratio Function of Temperature Curve Name
                 OutArgs(14)=TempArgs(13) ! Energy Input Ratio Function of Air Flow Fraction Curve Name - use the High Speed Curve here?
                 OutArgs(15)=TempArgs(14) ! Part Load Fraction Correlation Curve Name - Coil:Cooling:DX:VariableSpeed just had one, use it for every speed
                 !OutArgs(16)='' ! Rated Waste Heat Fraction of Power Input - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 !OutArgs(17)='' ! Waste Heat Function of Temperature Curve Name - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 OutArgs(18)=TempArgs(37) ! Sensible Heat Ratio Modifier Function of Temperature Curve Name
                 OutArgs(19)=TempArgs(38) ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name
                 CurArgs=19
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the High Speed Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // ' Speed 2 Performance' ! Name
                 OutArgs(2)='1.0' ! Gross Total Cooling Capacity Fraction
                 OutArgs(3)='1.0' ! Evaporator Air Flow Rate Fraction
                 OutArgs(4)='1.0' ! Condenser Air Flow Rate Fraction
                 OutArgs(5)=TempArgs(4) ! Gross Sensible Heat Ratio
                 OutArgs(6)=TempArgs(5) ! Gross Cooling COP
                 OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                 !OutArgs(8)='' ! Rated Evaporator Fan Power Per Volume Flow Rate - Coil:Cooling:DX:TwoSpeed never had this
                 OutArgs(9)='1.0' ! Evaporative Condenser Pump Power Fraction
                 OutArgs(10)=TempArgs(24) ! Evaporative Condenser Effectiveness
                 OutArgs(11)=TempArgs(10) ! Total Cooling Capacity Function of Temperature Curve Name
                 OutArgs(12)=TempArgs(11) ! Total Cooling Capacity Function of Air Flow Fraction Curve Name
                 OutArgs(13)=TempArgs(12) ! Energy Input Ratio Function of Temperature Curve Name
                 OutArgs(14)=TempArgs(13) ! Energy Input Ratio Function of Air Flow Fraction Curve Name
                 OutArgs(15)=TempArgs(14) ! Part Load Fraction Correlation Curve Name
                 !OutArgs(16)='' ! Rated Waste Heat Fraction of Power Input - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 !OutArgs(17)='' ! Waste Heat Function of Temperature Curve Name - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 OutArgs(18)=TempArgs(35) ! Sensible Heat Ratio Modifier Function of Temperature Curve Name
                 OutArgs(19)=TempArgs(36) ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name
                 CurArgs=19
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! already written
                 Written = .true.
    
             CASE('COIL:COOLING:DX:MULTISPEED')
                 nodiff=.false.
                 ObjectName='Coil:Cooling:DX'
                 ! store the date for later
                 TempArgs=InArgs
                 TempArgsNum=CurArgs

                 ! write the Coil:Cooling:DX object
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TempArgs(1) ! Name
                 OutArgs(2)=TempArgs(3) ! Evaporator Inlet Node Name
                 OutArgs(3)=TempArgs(4) ! Evaporator Outlet Node Name
                 OutArgs(4)=TempArgs(2) ! Availability Schedule Name
                 OutArgs(5)=TempArgs(95) ! Condenser Zone Name
                 OutArgs(6)=TempArgs(5) ! Condenser Inlet Node Name
                 OutArgs(7)=TRIM(TempArgs(1)) // TRIM(' Condenser Outlet Node') ! Condenser Outlet Node Name
                 OutArgs(8)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Performance Object Name
                 OutArgs(9)=TempArgs(9) ! Condensate Collection Water Storage Tank Name
                 OutArgs(10)=TempArgs(8) ! Evaporative Condenser Supply Water Storage Tank Name
                 CurArgs=10
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Performance'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Name
                 OutArgs(2)=TempArgs(12) ! Crankcase Heater Capacity
                 OutArgs(3)=TempArgs(7) ! Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
                 OutArgs(4)=TempArgs(13) ! Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation
                 !OutArgs(5)='' ! Unit Internal Static Air Pressure - Only defined for Coil:Cooling:DX:TwoSpeed previously
                 !OutArgs(6)='' ! Method for Switching Operating Modes - Only defined for Coil:Cooling:DX:TwoStageWithHumidityControl previously
                 !OutArgs(7)='' ! Operating Mode Number Schedule Name
                 OutArgs(8)=TempArgs(14) ! Evaporative Condenser Basin Heater Capacity
                 OutArgs(9)=TempArgs(15) ! Evaporative Condenser Basin Heater Setpoint Temperature
                 OutArgs(10)=TempArgs(16) ! Evaporative Condenser Basin Heater Operating Schedule Name
                 OutArgs(11)=TempArgs(17) ! Compressor Fuel Type
                 OutArgs(12)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Operating Mode 1 Name
                 CurArgs=12
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Operating Mode object
                 OutArgs=Blank
                 ErrFlag=.false.
                 NumberOfSpeeds=ProcessNumber(TempArgs(18),ErrFlag)
                 IF (ErrFlag) THEN
                   CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field 4, ['//  &
                      trim(TempArgs(4))//'], Name='//TRIM(TempArgs(1)),Auditf)
                 ENDIF

                 ! Save starting position of nominal speed values for here for scaling later
                 DXNomSpeedStartArgNum = 19 + 19*(NumberOfSpeeds-1)

                 ObjectName='Coil:Cooling:DX:CurveFit:OperatingMode'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Name
                 OutArgs(2)=TempArgs(DXNomSpeedStartArgNum) ! Rated Gross Total Cooling Capacity
                 OutArgs(3)=TempArgs(DXNomSpeedStartArgNum+3) ! Rated Evaporator Air Flow Rate
                 OutArgs(4)=TempArgs(DXNomSpeedStartArgNum+17) ! Rated Condenser Air Flow Rate
                 OutArgs(5)=TempArgs(31) ! Maximum Cycling Rate
                 OutArgs(6)=TempArgs(30) ! Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity
                 OutArgs(7)=TempArgs(32) ! Latent Capacity Time Constant
                 OutArgs(8)=TempArgs(29) ! Nominal Time for Condensate Removal to Begin
                 OutArgs(9)=TempArgs(11) ! Apply Latent Degradation to Speeds Greater than 1
                 CALL ShowWarningError( &
                   'Coil:Cooling:DX:MultiSpeed (old)="' //trim(TempArgs(1))// '".' //  &
                   ' Using Speed 1 values for Nominal Time for Condensate Removal to Begin, Ratio of Initial Moisture Evaporation Rate, '//  &
                   ' Maximum Cycling Rate, and Latent Capacity Time Constant in Coil:Cooling:DX:CurveFit:OperatingMode='//TRIM(TempArgs(1)) // '.',Auditf)
                 
                 OutArgs(10)=TempArgs(6) ! Condenser Type
                 OutArgs(11)=TempArgs(DXNomSpeedStartArgNum+18) ! Nominal Evaporative Condenser Pump Power
                 OutArgs(12)='MultiSpeed' ! Capacity Control Method


                 OutArgs(13)=TempArgs(18) ! Nominal Speed Number - For Coil:Cooling:DX:Multispeed this is same as number of speeds
                 CurArgs=13
                 DO SpeedNum=1,NumberOfSpeeds 
                   CurArgs=CurArgs+1
                   SpeedNumChar=RoundSigDigits(SpeedNum,0)
                   OutArgs(CurArgs)=TRIM(TempArgs(1)) // ' Speed ' // TRIM(SpeedNumChar) //' Performance' ! Speed n Name
                 ENDDO
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 DO SpeedNum=1,NumberOfSpeeds 
                     ! write the Speed Performance objects
                     SpeedNumChar=RoundSigDigits(SpeedNum,0)
                     DXSpeedStartArgNum = 19 + 19*(SpeedNum - 1)
                     DXSpeedRatio = FLOAT(SpeedNum)/FLOAT(NumberOfSpeeds)
                     OutArgs=Blank
                     ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                     CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                     OutArgs(1)=TRIM(TempArgs(1)) // ' Speed ' // TRIM(SpeedNumChar) // ' Performance' ! Name

                     ! Capacity is scaled by the capacity at nominal speed - but only if it's not autosized
                     IF (SameString(TempArgs(DXSpeedStartArgNum), 'AUTOSIZE') .OR. SameString(TempArgs(DXNomSpeedStartArgNum), 'AUTOSIZE')) THEN
                       ! If the high or low speed capacity is autosized then use old default ratio of 0.3333 for low speed fraction
                       OutArgs(2)=RoundSigDigits(DXSpeedRatio,4) ! Gross Total Cooling Capacity Fraction
                       CALL ShowWarningError( &
                         'Coil:Cooling:DX:MultiSpeed (old)="' //trim(TempArgs(1))// '".' //  &
                         ' Speed ' // TRIM(RoundSigDigits(SpeedNum,0)) // ' Gross Rated Total Cooling Capacity='//trim(TempArgs(DXNomSpeedStartArgNum))//' was replaced with default sizing fraction.'//  &
                         ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed ' // RoundSigDigits(SpeedNum,0) // &
                         ' Performance, Gross Total Cooling Capacity Fraction = ' // RoundSigDigits(DXSpeedRatio,4) // '.',Auditf)
                     ELSE
                        ! Capacity is scaled by the capacity at the nominal speed
                       ErrFlag=.false.
                       DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXSpeedStartArgNum))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       ErrFlag=.false.
                       DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXNomSpeedStartArgNum))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                         DXRatio = DXTempValue1/DXTempValue2
                         OutArgs(2)=RoundSigDigits(DXRatio,4) ! Gross Total Cooling Capacity Fraction
                       ELSE
                         OutArgs(2)='' ! Gross Total Cooling Capacity Fraction
                       ENDIF
                     ENDIF

                     ! Evaporator air flow is scaled by the flow at nominal speed - but only if it's not autosized
                     IF (SameString(TempArgs(DXSpeedStartArgNum+3), 'AUTOSIZE') .OR. SameString(TempArgs(DXNomSpeedStartArgNum+3), 'AUTOSIZE')) THEN
                       ! If the high or low speed capacity is autosized then use old default ratio of 0.3333 for low speed fraction
                       OutArgs(3)=RoundSigDigits(DXSpeedRatio,4) ! Evaporator Air Flow Rate Fraction
                       CALL ShowWarningError( &
                         'Coil:Cooling:DX:MultiSpeed (old)="' //trim(TempArgs(1))// '".' //  &
                         ' Speed ' // TRIM(RoundSigDigits(SpeedNum,0)) // ' Rated Air Flow Rate='//trim(TempArgs(DXNomSpeedStartArgNum+3))//' was replaced with default sizing fraction.'//  &
                         ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed ' // RoundSigDigits(SpeedNum,0) // &
                         ' Performance, Evaporator Air Flow Rate Fraction = ' // RoundSigDigits(DXSpeedRatio,4) // '.',Auditf)
                     ELSE
                       ! Evaporator air flow is scaled by the flow at nominal speed
                       ErrFlag=.false.
                       DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum+3),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXSpeedStartArgNum+3))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       ErrFlag=.false.
                       DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum+3),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXNomSpeedStartArgNum+3))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                         DXRatio = DXTempValue1/DXTempValue2
                         OutArgs(3)=RoundSigDigits(DXRatio,4) ! Evaporator Air Flow Rate Fraction
                       ELSE
                         OutArgs(3)='' ! Evaporator Air Flow Rate Fraction
                       ENDIF
                     ENDIF
                     
                     ! Condenser air flow is scaled by the flow at nominal speed - but only if it's not autosized
                     IF (SameString(TempArgs(DXSpeedStartArgNum+17), 'AUTOSIZE') .OR. SameString(TempArgs(DXNomSpeedStartArgNum+17), 'AUTOSIZE')) THEN
                       ! If the high or low speed capacity is autosized then use old default ratio of 0.3333 for low speed fraction
                       OutArgs(4)=RoundSigDigits(DXSpeedRatio,4) ! Condenser Air Flow Rate Fraction
                       CALL ShowWarningError( &
                         'Coil:Cooling:DX:MultiSpeed (old)="' //trim(TempArgs(1))// '".' //  &
                         ' Speed ' // TRIM(RoundSigDigits(SpeedNum,0)) // ' Evaporative Condenser Air Flow Rate='//trim(TempArgs(DXNomSpeedStartArgNum+17))//' was replaced with default sizing fraction.'//  &
                         ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed ' // RoundSigDigits(SpeedNum,0) // &
                         ' Performance, Evaporator Air Flow Rate Fraction = ' // RoundSigDigits(DXSpeedRatio,4) // '.',Auditf)
                     ELSE
                       ! Condenser air flow is scaled by the flow at the nominal speed
                       ErrFlag=.false.
                       DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum+17),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXSpeedStartArgNum+17))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       ErrFlag=.false.
                       DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum+17),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXNomSpeedStartArgNum+17))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                         DXRatio = DXTempValue1/DXTempValue2
                         OutArgs(4)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                       ELSE
                         OutArgs(4)='' ! Condenser Air Flow Rate Fraction
                       ENDIF
                     ENDIF

                     OutArgs(5)=TempArgs(DXSpeedStartArgNum+1) ! Gross Sensible Heat Ratio
                     OutArgs(6)=TempArgs(DXSpeedStartArgNum+2) ! Gross Cooling COP
                     OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                     OutArgs(8)=TempArgs(DXSpeedStartArgNum+4) ! Rated Evaporator Fan Power Per Volume Flow Rate

                     ! Evaporative Condenser Pump Power is scaled by the power at nominal speed - but only if it's not autosized
                     IF (SameString(TempArgs(DXSpeedStartArgNum+18), 'AUTOSIZE') .OR. SameString(TempArgs(DXNomSpeedStartArgNum+18), 'AUTOSIZE')) THEN
                       ! If the high or low speed capacity is autosized then use old default ratio of 0.3333 for low speed fraction
                       OutArgs(9)=RoundSigDigits(DXSpeedRatio,4) ! Evaporative Condenser Pump Power Fraction
                       CALL ShowWarningError( &
                         'Coil:Cooling:DX:MultiSpeed (old)="' //trim(TempArgs(1))// '".' //  &
                         ' Speed ' // TRIM(RoundSigDigits(SpeedNum,0)) // ' Rated Evaporative Condenser Pump Power Consumption='//trim(TempArgs(DXNomSpeedStartArgNum+18))//' was replaced with default sizing fraction.'//  &
                         ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(TempArgs(1)) // ' Speed ' // RoundSigDigits(SpeedNum,0) // &
                         ' Performance, Evaporator Air Flow Rate Fraction = ' // RoundSigDigits(DXSpeedRatio,4) // '.',Auditf)
                     ELSE
                       ! Evaporative Condenser Pump Power is scaled by the power at the nominal speed
                       ErrFlag=.false.
                       DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum+18),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXSpeedStartArgNum+18))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       ErrFlag=.false.
                       DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum+18),ErrFlag)
                       IF (ErrFlag) THEN
                         CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:MultiSpeed field, ['//  &
                            trim(TempArgs(DXNomSpeedStartArgNum+18))//'], Name='//TRIM(TempArgs(1)),Auditf)
                       ENDIF
                       IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                         DXRatio = DXTempValue1/DXTempValue2
                         OutArgs(9)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                       ELSE
                         OutArgs(9)='' ! Evaporative Condenser Pump Power Fraction
                       ENDIF
                     ENDIF

                     OutArgs(10)=TempArgs(DXSpeedStartArgNum+16) ! Evaporative Condenser Effectiveness
                     OutArgs(11)=TempArgs(DXSpeedStartArgNum+5) ! Total Cooling Capacity Function of Temperature Curve Name
                     OutArgs(12)=TempArgs(DXSpeedStartArgNum+6) ! Total Cooling Capacity Function of Air Flow Fraction Curve Name
                     OutArgs(13)=TempArgs(DXSpeedStartArgNum+7) ! Energy Input Ratio Function of Temperature Curve Name
                     OutArgs(14)=TempArgs(DXSpeedStartArgNum+8) ! Energy Input Ratio Function of Air Flow Fraction Curve Name
                     OutArgs(15)=TempArgs(DXSpeedStartArgNum+9) ! Part Load Fraction Correlation Curve Name
                     OutArgs(16)=TempArgs(DXSpeedStartArgNum+14) ! Rated Waste Heat Fraction of Power Input
                     OutArgs(17)=TempArgs(DXSpeedStartArgNum+15) ! Waste Heat Function of Temperature Curve Name
                     !OutArgs(18)='' ! Sensible Heat Ratio Modifier Function of Temperature Curve Name - Coil:Cooling:DX:MultiSpeed never had this
                     !OutArgs(19)='' ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name - Coil:Cooling:DX:MultiSpeed never had this
                     CurArgs=19
                     CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                 ENDDO
                 ! already written
                 Written = .true.
    
             CASE('COIL:COOLING:DX:VARIABLESPEED')
                 nodiff=.false.
                 ObjectName='Coil:Cooling:DX'
                 ! store the date for later
                 TempArgs=InArgs
                 TempArgsNum=CurArgs

                 ! write the Coil:Cooling:DX object
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TempArgs(1) ! Name
                 OutArgs(2)=TempArgs(2) ! Evaporator Inlet Node Name
                 OutArgs(3)=TempArgs(3) ! Evaporator Outlet Node Name
                 !OutArgs(4)='' ! Availability Schedule Name - Coil:Cooling:DX:VariableSpeed never had this
                 !OutArgs(5)='' ! Condenser Zone Name - Coil:Cooling:DX:VariableSpeed never had this
                 OutArgs(6)=TempArgs(11) ! Condenser Inlet Node Name
                 OutArgs(7)=TRIM(TempArgs(1)) // TRIM(' Condenser Outlet Node') ! Condenser Outlet Node Name
                 OutArgs(8)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Performance Object Name
                 OutArgs(9)=TempArgs(18) ! Condensate Collection Water Storage Tank Name
                 OutArgs(10)=TempArgs(17) ! Evaporative Condenser Supply Water Storage Tank Name
                 CurArgs=10
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Performance'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Name
                 OutArgs(2)=TempArgs(14) ! Crankcase Heater Capacity
                 OutArgs(3)=TempArgs(16) ! Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
                 OutArgs(4)=TempArgs(15) ! Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation
                 !OutArgs(5)='' ! Unit Internal Static Air Pressure - Only defined for Coil:Cooling:DX:TwoSpeed previously
                 !OutArgs(6)='' ! Method for Switching Operating Modes - Only defined for Coil:Cooling:DX:TwoStageWithHumidityControl previously
                 !OutArgs(7)='' ! Operating Mode Number Schedule Name
                 OutArgs(8)=TempArgs(19) ! Evaporative Condenser Basin Heater Capacity
                 OutArgs(9)=TempArgs(20) ! Evaporative Condenser Basin Heater Setpoint Temperature
                 OutArgs(10)=TempArgs(21) ! Evaporative Condenser Basin Heater Operating Schedule Name
                 OutArgs(11)='Electricity' ! Compressor Fuel Type
                 OutArgs(12)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Operating Mode 1 Name
                 CurArgs=12
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Operating Mode object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:OperatingMode'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Name
                 OutArgs(2)=TempArgs(6) ! Rated Gross Total Cooling Capacity
                 OutArgs(3)=TempArgs(7) ! Rated Evaporator Air Flow Rate
                 !OutArgs(4)='' ! Rated Condenser Air Flow Rate - Coil:Cooling:DX:VariableSpeed never had this
                 !OutArgs(5)='' ! Maximum Cycling Rate - Coil:Cooling:DX:VariableSpeed never had this
                 OutArgs(6)=TempArgs(9) ! Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity
                 !OutArgs(7)='' ! Latent Capacity Time Constant - Coil:Cooling:DX:VariableSpeed never had this
                 OutArgs(8)=TempArgs(8) ! Nominal Time for Condensate Removal to Begin
                 !OutArgs(9)='' ! Apply Latent Degradation to Speeds Greater than 1 - Only defined for Coil:Cooling:DX:MultiSpeed previously
                 OutArgs(10)=TempArgs(12) ! Condenser Type
                 OutArgs(11)=TempArgs(13) ! Nominal Evaporative Condenser Pump Power
                 OutArgs(12)='VariableSpeed' ! Capacity Control Method

                 ErrFlag=.false.
                 NumberOfSpeeds=ProcessNumber(TempArgs(4),ErrFlag)
                 IF (ErrFlag) THEN
                   CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field 4, ['//  &
                      trim(TempArgs(4))//'], Name='//TRIM(TempArgs(1)),Auditf)
                 ENDIF

                 OutArgs(13)=TempArgs(5) ! Nominal Speed Number
                 ! Save starting position of nominal speed values for scaling later
                 ErrFlag=.false.
                 DXNomSpeedStartArgNum = 22 + 10*(ProcessNumber(TempArgs(5),ErrFlag)-1)
                 IF (ErrFlag) THEN
                   CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field 5, ['//  &
                      trim(TempArgs(5))//'], Name='//TRIM(TempArgs(1)),Auditf)
                 ENDIF

                 CurArgs=13
                 DO SpeedNum=1,NumberOfSpeeds 
                   CurArgs=CurArgs+1
                   SpeedNumChar=RoundSigDigits(SpeedNum,0)
                   OutArgs(CurArgs)=TRIM(TempArgs(1)) // ' Speed ' // TRIM(SpeedNumChar) //' Performance' ! Speed n Name
                 ENDDO
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 DO SpeedNum=1,NumberOfSpeeds 
                     ! write the Speed Performance objects
                     SpeedNumChar=RoundSigDigits(SpeedNum,0)
                     DXSpeedStartArgNum = 22 + 10*(SpeedNum - 1)
                     OutArgs=Blank
                     ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                     CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                     OutArgs(1)=TRIM(TempArgs(1)) // ' Speed ' // TRIM(SpeedNumChar) // ' Performance' ! Name

                     ! Capacity is scaled by the capacity at the nominal speed
                     ErrFlag=.false.
                     DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum),ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field, ['//  &
                          trim(TempArgs(DXSpeedStartArgNum))//'], Name='//TRIM(TempArgs(1)),Auditf)
                     ENDIF
                     ErrFlag=.false.
                     DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum),ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field, ['//  &
                          trim(TempArgs(DXNomSpeedStartArgNum))//'], Name='//TRIM(TempArgs(1)),Auditf)
                     ENDIF
                     IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                       DXRatio = DXTempValue1/DXTempValue2
                       OutArgs(2)=RoundSigDigits(DXRatio,4) ! Gross Total Cooling Capacity Fraction
                     ELSE
                       OutArgs(2)='' ! Gross Total Cooling Capacity Fraction
                     ENDIF

                     ! Evaporator air flow is scaled by the flow at the nominal speed
                     ErrFlag=.false.
                     DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum+3),ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field, ['//  &
                          trim(TempArgs(DXSpeedStartArgNum+3))//'], Name='//TRIM(TempArgs(1)),Auditf)
                     ENDIF
                     ErrFlag=.false.
                     DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum+3),ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field, ['//  &
                          trim(TempArgs(DXNomSpeedStartArgNum+3))//'], Name='//TRIM(TempArgs(1)),Auditf)
                     ENDIF
                     IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                       DXRatio = DXTempValue1/DXTempValue2
                       OutArgs(3)=RoundSigDigits(DXRatio,4) ! Evaporator Air Flow Rate Fraction
                     ELSE
                       OutArgs(3)='' ! Evaporator Air Flow Rate Fraction
                     ENDIF
                     
                     ! Condenser air flow is scaled by the flow at the nominal speed
                     ErrFlag=.false.
                     DXTempValue1 = ProcessNumber(TempArgs(DXSpeedStartArgNum+4),ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field, ['//  &
                          trim(TempArgs(DXSpeedStartArgNum+4))//'], Name='//TRIM(TempArgs(1)),Auditf)
                     ENDIF
                     ErrFlag=.false.
                     DXTempValue2 = ProcessNumber(TempArgs(DXNomSpeedStartArgNum+4),ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:VariableSpeed field, ['//  &
                          trim(TempArgs(DXNomSpeedStartArgNum+4))//'], Name='//TRIM(TempArgs(1)),Auditf)
                     ENDIF
                     IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                       DXRatio = DXTempValue1/DXTempValue2
                       OutArgs(4)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                     ELSE
                       OutArgs(4)='' ! Condenser Air Flow Rate Fraction
                     ENDIF

                     OutArgs(5)=TempArgs(DXSpeedStartArgNum+1) ! Gross Sensible Heat Ratio
                     OutArgs(6)=TempArgs(DXSpeedStartArgNum+2) ! Gross Cooling COP
                     OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                     !OutArgs(8)='' ! Rated Evaporator Fan Power Per Volume Flow Rate - Coil:Cooling:DX:VariableSpeed never had this
                     !OutArgs(9)='' ! Evaporative Condenser Pump Power Fraction - Only defined for Coil:Cooling:DX:MultiSpeed and TwoSpeed previously
                     OutArgs(10)=TempArgs(DXSpeedStartArgNum+5) ! Evaporative Condenser Effectiveness
                     OutArgs(11)=TempArgs(DXSpeedStartArgNum+6) ! Total Cooling Capacity Function of Temperature Curve Name
                     OutArgs(12)=TempArgs(DXSpeedStartArgNum+7) ! Total Cooling Capacity Function of Air Flow Fraction Curve Name
                     OutArgs(13)=TempArgs(DXSpeedStartArgNum+8) ! Energy Input Ratio Function of Temperature Curve Name
                     OutArgs(14)=TempArgs(DXSpeedStartArgNum+9) ! Energy Input Ratio Function of Air Flow Fraction Curve Name
                     OutArgs(15)=TempArgs(10) ! Part Load Fraction Correlation Curve Name - Coil:Cooling:DX:VariableSpeed just had one, use it for every speed
                     !OutArgs(16)='' ! Rated Waste Heat Fraction of Power Input - Only defined for Coil:Cooling:DX:MultiSpeed previously
                     !OutArgs(17)='' ! Waste Heat Function of Temperature Curve Name - Only defined for Coil:Cooling:DX:MultiSpeed previously
                     !OutArgs(18)='' ! Sensible Heat Ratio Modifier Function of Temperature Curve Name - Coil:Cooling:DX:VariableSpeed never had this
                     !OutArgs(19)='' ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name - Coil:Cooling:DX:VariableSpeed never had this
                     CurArgs=19
                     CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                 ENDDO
                 ! already written
                 Written = .true.
    
             CASE('COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE')
                 nodiff=.false.
                 ObjectName='Coil:Cooling:DX'
                 ! store the date for later
                 TempArgs=InArgs
                 TempArgsNum=CurArgs

                 ! write the Coil:Cooling:DX object for TwoStageWithHumidityControlMode
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TempArgs(1) ! Name
                 OutArgs(2)=TempArgs(3) ! Evaporator Inlet Node Name
                 OutArgs(3)=TempArgs(4) ! Evaporator Outlet Node Name
                 OutArgs(4)=TempArgs(2) ! Availability Schedule Name
                 OutArgs(5)=TempArgs(95) ! Condenser Zone Name
                 OutArgs(6)=TempArgs(5) ! Condenser Inlet Node Name
                 OutArgs(7)=TRIM(TempArgs(1)) // TRIM(' Condenser Outlet Node') ! Condenser Outlet Node Name
                 OutArgs(8)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Performance Object Name
                 OutArgs(9)=TempArgs(18) ! Condensate Collection Water Storage Tank Name
                 OutArgs(10)=TempArgs(17) ! Evaporative Condenser Supply Water Storage Tank Name
                 CurArgs=10
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Performance object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:Performance'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Performance') ! Name
                 OutArgs(2)=TempArgs(5) ! Crankcase Heater Capacity
                 OutArgs(3)=TempArgs(19) ! Minimum Outdoor Dry-Bulb Temperature for Compressor Operation
                 OutArgs(4)=TempArgs(6) ! Maximum Outdoor Dry-Bulb Temperature for Crankcase Heater Operation
                 !OutArgs(5)='' ! Unit Internal Static Air Pressure - Only defined for Coil:Cooling:DX:TwoSpeed previously
                 OutArgs(6)='HumidityControl' ! Method for Switching Operating Modes
                 !OutArgs(7)='' ! Operating Mode Number Schedule Name
                 OutArgs(8)=TempArgs(20) ! Evaporative Condenser Basin Heater Capacity
                 OutArgs(9)=TempArgs(21) ! Evaporative Condenser Basin Heater Setpoint Temperature
                 OutArgs(10)=TempArgs(22) ! Evaporative Condenser Basin Heater Operating Schedule Name
                 OutArgs(11)='Electricity' ! Compressor Fuel Type
                 OutArgs(12)=TRIM(TempArgs(1))// TRIM(' Normal Operating Mode ') ! Operating Mode 1 Name
                 CurArgs=12
                 IF (TempArgs(8) == blank) THEN
                   NumberOfModes = 1
                 ELSE
                   ErrFlag=.false.
                   NumberOfModes=ProcessNumber(TempArgs(8),ErrFlag)+1
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoStageWithHumidityControlMode field 8, ['//  &
                        trim(TempArgs(8))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                 ENDIF
                 IF (NumberOfModes == 2) THEN
                   OutArgs(13)=TRIM(TempArgs(1))// TRIM(' Dehumidification Operating Mode') ! Operating Mode 1 Name
                   CurArgs=13
                 ENDIF
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Operating Mode objects
                 IF (TempArgs(7) == blank) THEN
                   NumberOfSpeeds = 1
                 ELSE
                   ErrFlag=.false.
                   NumberOfSpeeds=ProcessNumber(TempArgs(7),ErrFlag)
                   IF (ErrFlag) THEN
                     CALL ShowSevereError('Invalid Number, Coil:Cooling:DX:TwoStageWithHumidityControlMode field 7, ['//  &
                        trim(TempArgs(7))//'], Name='//TRIM(TempArgs(1)),Auditf)
                   ENDIF
                 ENDIF
                 DO ModeNum=1,NumberOfModes 
                   OutArgs=Blank
                   ! Save index of Nominal Performance Object for this mode
                   IF ( ModeNum == 1 ) THEN
                     DXStg1PerfName = TempArgs(10)
                     ModeName = 'Normal'
                     IF ( NumberOfSpeeds == 1 ) THEN
                       DXNomPerfName = TempArgs(10)
                     ELSE
                       DXNomPerfName = TempArgs(12)
                     ENDIF
                   ELSE
                     ModeName = 'Dehumidification'
                     DXStg1PerfName = TempArgs(14)
                     IF ( NumberOfSpeeds == 1 ) THEN
                       DXNomPerfName = TempArgs(14)
                     ELSE
                       DXNomPerfName = TempArgs(16)
                     ENDIF
                   ENDIF

                   iDXNomPerf = 0
                   iDXStg1Perf = 0
                   DO iDXPerf=1,NumMultiStageDXPerf
                     IF ( SameString( DXNomPerfName, MultiStageDXPerformance(iDXPerf)%Name) ) THEN
                       iDXNomPerf = iDXPerf
                     ENDIF   
                     IF ( SameString( DXStg1PerfName, MultiStageDXPerformance(iDXPerf)%Name) ) THEN
                       iDXStg1Perf = iDXPerf
                     ENDIF   
                   ENDDO
  
                   ObjectName='Coil:Cooling:DX:CurveFit:OperatingMode'
                   CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                   OutArgs(1)=TRIM(TempArgs(1))//' ' // TRIM(ModeName) // TRIM(' Operating Mode') ! Name
                   OutArgs(2)=MultiStageDXPerformance(iDXNomPerf)%GrossRatedTotCoolCap ! Rated Gross Total Cooling Capacity
                   OutArgs(3)=MultiStageDXPerformance(iDXNomPerf)%RatedAirFlowRate ! Rated Evaporator Air Flow Rate
                   OutArgs(4)=MultiStageDXPerformance(iDXNomPerf)%EvapCondAirFlowRate ! Rated Condenser Air Flow Rate
                   OutArgs(5)=MultiStageDXPerformance(iDXStg1Perf)%MaxCyclingRate ! Maximum Cycling Rate
                   OutArgs(6)=MultiStageDXPerformance(iDXStg1Perf)%RatioOfInitialMoisture ! Ratio of Initial Moisture Evaporation Rate and Steady State Latent Capacity
                   OutArgs(7)=MultiStageDXPerformance(iDXStg1Perf)%LatentCapTimeConst ! Latent Capacity Time Constant
                   OutArgs(8)=MultiStageDXPerformance(iDXStg1Perf)%NomTimeForCondensRemoval ! Nominal Time for Condensate Removal to Begin
                   OutArgs(9)='Yes' ! Apply Latent Degradation to Speeds Greater than 1 - Yes for Coil:Cooling:DX:TwoStageWithHumidityControlMode
                   CALL ShowWarningError( &
                     'Coil:Cooling:DX:TwoStageWithHumidityControlMode (old)="' //trim(TempArgs(1))// '".' //  &
                     ' Using Stage 1 values for Nominal Time for Condensate Removal to Begin, Ratio of Initial Moisture Evaporation Rate, '//  &
                     ' Maximum Cycling Rate, Latent Capacity Time Constant, and Condenser Type in Coil:Cooling:DX:CurveFit:OperatingMode='//TRIM(TempArgs(1)) // '.',Auditf)
                   
                   OutArgs(10)=MultiStageDXPerformance(iDXStg1Perf)%CondType ! Condenser Type
                   OutArgs(11)=MultiStageDXPerformance(iDXStg1Perf)%EvapCondPumpPower ! Nominal Evaporative Condenser Pump Power
                   OutArgs(12)='MultiSpeed' ! Capacity Control Method
  
  
                   OutArgs(13)=RoundSigDigits(NumberOfSpeeds,0) ! Nominal Speed Number - For Coil:Cooling:DX:TwoStageWithHumidityControlMode this is same as number of speeds
                   CurArgs=13
                   DO SpeedNum=1,NumberOfSpeeds 
                     CurArgs=CurArgs+1
                     SpeedNumChar=RoundSigDigits(SpeedNum,0)
                     OutArgs(CurArgs)=TRIM(TempArgs(1))//' ' // TRIM(ModeName) // ' Mode Stage ' // TRIM(SpeedNumChar) //' Performance' ! Speed n Name
                   ENDDO
                   CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
  
                   IF( NumberOfSpeeds == 2 ) THEN
                       ! write the Stage 1 Performance object
                       SpeedNumChar='1'
                       DXSpeedRatio = 0.5
                       OutArgs=Blank
                       ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                       CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                       OutArgs(1)=TRIM(TempArgs(1))//' ' // TRIM(ModeName) // ' Mode Stage 1 Performance' ! Name
  
                       ! Capacity is scaled by the capacity at nominal speed - but only if it's not autosized
                       IF (SameString(MultiStageDXPerformance(iDXStg1Perf)%GrossRatedTotCoolCap, 'AUTOSIZE') .OR. SameString(MultiStageDXPerformance(iDXNomPerf)%GrossRatedTotCoolCap, 'AUTOSIZE')) THEN
                         ! If the high or low speed value is autosized then use old default ratio of 0.5 for low speed fraction
                         OutArgs(2)='0.50' ! Gross Total Cooling Capacity Fraction
                         CALL ShowWarningError( &
                           'Coil:Cooling:DX:TwoStageWithHumidityControlMode (old)="' //trim(TempArgs(1))// '".' //  &
                           ' Stage 1 Gross Rated Total Cooling Capacity='//trim(MultiStageDXPerformance(iDXStg1Perf)%GrossRatedTotCoolCap)//' was replaced with default sizing fraction.'//  &
                           ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(OutArgs(1))// &
                           ', Gross Total Cooling Capacity Fraction = 0.50.',Auditf)
                       ELSE
                          ! Capacity is scaled by the capacity at the nominal speed
                         ErrFlag=.false.
                         DXTempValue1 = ProcessNumber(MultiStageDXPerformance(iDXStg1Perf)%GrossRatedTotCoolCap,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXStg1Perf)%Name)//  &
                              ', Gross Rated Total Cooling Capacity='//trim(MultiStageDXPerformance(iDXStg1Perf)%GrossRatedTotCoolCap),Auditf)
                         ENDIF
                         ErrFlag=.false.
                         DXTempValue2 = ProcessNumber(MultiStageDXPerformance(iDXNomPerf)%GrossRatedTotCoolCap,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXNomPerf)%Name)//  &
                              ', Gross Rated Total Cooling Capacity='//trim(MultiStageDXPerformance(iDXNomPerf)%GrossRatedTotCoolCap),Auditf)
                         ENDIF
                         IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                           DXRatio = DXTempValue1/DXTempValue2
                           OutArgs(2)=RoundSigDigits(DXRatio,4) ! Gross Total Cooling Capacity Fraction
                         ELSE
                           OutArgs(2)='' ! Gross Total Cooling Capacity Fraction
                         ENDIF
                       ENDIF
  
                       ! Evaporator air flow is scaled by the flow at nominal speed - but only if it's not autosized
                       IF (SameString(MultiStageDXPerformance(iDXNomPerf)%RatedAirFlowRate, 'AUTOSIZE') .OR. SameString(MultiStageDXPerformance(iDXNomPerf)%RatedAirFlowRate, 'AUTOSIZE')) THEN
                         ! If the high or low speed value is autosized then use old default ratio of 0.5 for low speed fraction
                         OutArgs(3)='0.50' ! Evaporator Air Flow Rate Fraction
                         CALL ShowWarningError( &
                           'Coil:Cooling:DX:TwoStageWithHumidityControlMode (old)="' //trim(TempArgs(1))// '".' //  &
                           ' Stage 1 Rated Air Flow Rate='//trim(MultiStageDXPerformance(iDXStg1Perf)%RatedAirFlowRate)//' was replaced with default sizing fraction.'//  &
                           ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(OutArgs(1))// &
                           ' , Evaporator Air Flow Rate Fraction = 0.50.',Auditf)
                       ELSE
                         ! Evaporator air flow is scaled by the flow at nominal speed
                         ErrFlag=.false.
                         DXTempValue1 = ProcessNumber(MultiStageDXPerformance(iDXStg1Perf)%RatedAirFlowRate,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXStg1Perf)%Name)//  &
                              'm Rated Air Flow Rate='//trim(MultiStageDXPerformance(iDXStg1Perf)%RatedAirFlowRate),Auditf)
                         ENDIF
                         ErrFlag=.false.
                         DXTempValue2 = ProcessNumber(MultiStageDXPerformance(iDXNomPerf)%RatedAirFlowRate,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXNomPerf)%Name)//  &
                              'm Rated Air Flow Rate='//trim(MultiStageDXPerformance(iDXNomPerf)%RatedAirFlowRate),Auditf)
                         ENDIF
                         IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                           DXRatio = DXTempValue1/DXTempValue2
                           OutArgs(3)=RoundSigDigits(DXRatio,4) ! Evaporator Air Flow Rate Fraction
                         ELSE
                           OutArgs(3)='' ! Evaporator Air Flow Rate Fraction
                         ENDIF
                       ENDIF
                       
                       ! Condenser air flow is scaled by the flow at nominal speed - but only if it's not autosized
                       IF (SameString(MultiStageDXPerformance(iDXStg1Perf)%EvapCondAirFlowRate, 'AUTOSIZE') .OR. SameString(MultiStageDXPerformance(iDXNomPerf)%EvapCondAirFlowRate, 'AUTOSIZE')) THEN
                         ! If the high or low speed value is autosized then use old default ratio of 0.5 for low speed fraction
                         OutArgs(4)='0.50' ! Condenser Air Flow Rate Fraction
                         CALL ShowWarningError( &
                           'Coil:Cooling:DX:TwoStageWithHumidityControlMode (old)="' //trim(TempArgs(1))// '".' //  &
                           ' Stage 1 Gross Rated Total Cooling Capacity='//trim(MultiStageDXPerformance(iDXStg1Perf)%EvapCondAirFlowRate)//' was replaced with default sizing fraction.'//  &
                           ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(OutArgs(1))// &
                           ', Condenser Air Flow Rate Fraction = 0.50.',Auditf)
                       ELSE
                         ! Condenser air flow is scaled by the flow at the nominal speed
                         ErrFlag=.false.
                         DXTempValue1 = ProcessNumber(MultiStageDXPerformance(iDXStg1Perf)%EvapCondAirFlowRate,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXStg1Perf)%Name)//  &
                              ', Evaporative Condenser Air Flow Rate='//trim(MultiStageDXPerformance(iDXStg1Perf)%EvapCondAirFlowRate),Auditf)
                         ENDIF
                         ErrFlag=.false.
                         DXTempValue2 = ProcessNumber(MultiStageDXPerformance(iDXNomPerf)%EvapCondAirFlowRate,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXNomPerf)%Name)//  &
                              ', Evaporative Condenser Air Flow Rate='//trim(MultiStageDXPerformance(iDXNomPerf)%EvapCondAirFlowRate),Auditf)
                         ENDIF
                         IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                           DXRatio = DXTempValue1/DXTempValue2
                           OutArgs(4)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                         ELSE
                           OutArgs(4)='' ! Condenser Air Flow Rate Fraction
                         ENDIF
                       ENDIF
  
                       OutArgs(5)=MultiStageDXPerformance(iDXStg1Perf)%GrossRatedSHR ! Gross Sensible Heat Ratio
                       OutArgs(6)=MultiStageDXPerformance(iDXStg1Perf)%GrossRatedCOP ! Gross Cooling COP
                       IF (MultiStageDXPerformance(iDXStg1Perf)%FractionBypassed == blank) THEN
                         OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                       ELSE
                         ErrFlag=.false.
                         DXTempValue1=1.0 - ProcessNumber(MultiStageDXPerformance(iDXStg1Perf)%FractionBypassed,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXStg1Perf)%Name)//  &
                              ', Fraction of Air Flow Bypassed Around Coil='//trim(MultiStageDXPerformance(iDXStg1Perf)%FractionBypassed),Auditf)
                         ELSE
                           OutArgs(7)=RoundSigDigits(DXTempValue1,4) ! Active Fraction of Coil Face Area
                         ENDIF
                       ENDIF
                       !OutArgs(8)='' ! Rated Evaporator Fan Power Per Volume Flow Rate - Coil:Cooling:DX:TwoStageWithHumidityControlMode never had this
  
                       ! Evaporative Condenser Pump Power is scaled by the power at nominal speed - but only if it's not autosized
                       IF (SameString(MultiStageDXPerformance(iDXStg1Perf)%EvapCondPumpPower, 'AUTOSIZE') .OR. SameString(MultiStageDXPerformance(iDXNomPerf)%EvapCondPumpPower, 'AUTOSIZE')) THEN
                         ! If the high or low speed value is autosized then use old default ratio of 0.5 for low speed fraction
                         OutArgs(9)='0.50' ! Evaporative Condenser Pump Power Fraction
                         CALL ShowWarningError( &
                           'Coil:Cooling:DX:TwoStageWithHumidityControlMode (old)="' //trim(TempArgs(1))// '".' //  &
                           ' Stage 1 Evaporative Condenser Pump Rated Power Consumption='//trim(MultiStageDXPerformance(iDXStg1Perf)%EvapCondPumpPower)//' was replaced with default sizing fraction.'//  &
                           ' Coil:Cooling:DX:CurveFit:Speed='//TRIM(OutArgs(1))// &
                           ', Evaporative Condenser Pump Power Fraction = 0.50.',Auditf)
                       ELSE
                         ! Evaporative Condenser Pump Power is scaled by the power at the nominal speed
                         ErrFlag=.false.
                         DXTempValue1 = ProcessNumber(MultiStageDXPerformance(iDXStg1Perf)%EvapCondPumpPower,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXStg1Perf)%Name)//  &
                              ', Evaporative Condenser Pump Rated Power Consumption='//trim(MultiStageDXPerformance(iDXStg1Perf)%EvapCondPumpPower),Auditf)
                         ENDIF
                         ErrFlag=.false.
                         DXTempValue2 = ProcessNumber(MultiStageDXPerformance(iDXNomPerf)%EvapCondPumpPower,ErrFlag)
                         IF (ErrFlag) THEN
                           CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXNomPerf)%Name)//  &
                              ', Evaporative Condenser Pump Rated Power Consumption='//trim(MultiStageDXPerformance(iDXNomPerf)%EvapCondPumpPower),Auditf)
                         ENDIF
                         IF ((DXTempValue1 .GT. 0.0) .AND. (DXTempValue2 .GT. 0.0)) THEN
                           DXRatio = DXTempValue1/DXTempValue2
                           OutArgs(9)=RoundSigDigits(DXRatio,4) ! Condenser Air Flow Rate Fraction
                         ELSE
                           OutArgs(9)='' ! Evaporative Condenser Pump Power Fraction
                         ENDIF
                       ENDIF
  
                       OutArgs(10)=MultiStageDXPerformance(iDXStg1Perf)%EvapCondEff ! Evaporative Condenser Effectiveness
                       OutArgs(11)=MultiStageDXPerformance(iDXStg1Perf)%TotCapFTCurveName ! Total Cooling Capacity Function of Temperature Curve Name
                       OutArgs(12)=MultiStageDXPerformance(iDXStg1Perf)%TotCapFFCurveName ! Total Cooling Capacity Function of Air Flow Fraction Curve Name
                       OutArgs(13)=MultiStageDXPerformance(iDXStg1Perf)%EIRFTCurveName ! Energy Input Ratio Function of Temperature Curve Name
                       OutArgs(14)=MultiStageDXPerformance(iDXStg1Perf)%EIRFFCurveName ! Energy Input Ratio Function of Air Flow Fraction Curve Name
                       OutArgs(15)=MultiStageDXPerformance(iDXStg1Perf)%PLFCurveName ! Part Load Fraction Correlation Curve Name
                       !OutArgs(16)='' ! Rated Waste Heat Fraction of Power Input - Only defined for Coil:Cooling:DX:MultiSpeed previously
                       !OutArgs(17)='' ! Waste Heat Function of Temperature Curve Name - Only defined for Coil:Cooling:DX:MultiSpeed previously
                       OutArgs(18)=MultiStageDXPerformance(iDXStg1Perf)%SHRFTCurveName ! Sensible Heat Ratio Modifier Function of Temperature Curve Name
                       OutArgs(19)=MultiStageDXPerformance(iDXStg1Perf)%SHRFFCurveName ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name
                       CurArgs=19
                       CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                   ENDIF

                   ! write the Nominal Speed Performance object
                   OutArgs=Blank
                   ObjectName='Coil:Cooling:DX:CurveFit:Speed'
                   CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                   IF (NumberOfSpeeds == 1) THEN
                     OutArgs(1)=TRIM(TempArgs(1))//' ' // TRIM(ModeName) // ' Mode Stage 1 Performance' ! Name
                   ELSE
                     OutArgs(1)=TRIM(TempArgs(1))//' ' // TRIM(ModeName) // ' Mode Stage 2 Performance' ! Name
                   ENDIF

                   OutArgs(2)='1.0' ! Gross Total Cooling Capacity Fraction
                   OutArgs(3)='1.0' ! Evaporator Air Flow Rate Fraction
                   OutArgs(4)='1.0' ! Condenser Air Flow Rate Fraction
                   OutArgs(5)=MultiStageDXPerformance(iDXNomPerf)%GrossRatedSHR ! Gross Sensible Heat Ratio
                   OutArgs(6)=MultiStageDXPerformance(iDXNomPerf)%GrossRatedCOP ! Gross Cooling COP
                   IF (MultiStageDXPerformance(iDXNomPerf)%FractionBypassed == blank) THEN
                     OutArgs(7)='1.0' ! Active Fraction of Coil Face Area
                   ELSE
                     ErrFlag=.false.
                     DXTempValue1=1.0 - ProcessNumber(MultiStageDXPerformance(iDXNomPerf)%FractionBypassed,ErrFlag)
                     IF (ErrFlag) THEN
                       CALL ShowSevereError('Invalid Number, CoilPerformance:DX:Cooling (old) Name='//TRIM(MultiStageDXPerformance(iDXNomPerf)%Name)//  &
                          ', Fraction of Air Flow Bypassed Around Coil='//trim(MultiStageDXPerformance(iDXNomPerf)%FractionBypassed),Auditf)
                     ELSE
                       OutArgs(7)=RoundSigDigits(DXTempValue1,4) ! Active Fraction of Coil Face Area
                     ENDIF
                   ENDIF
                   !OutArgs(8)='' ! Rated Evaporator Fan Power Per Volume Flow Rate - Coil:Cooling:DX:TwoStageWithHumidityControlMode never had this
                   OutArgs(9)='1.0' ! Evaporative Condenser Pump Power Fraction
                   OutArgs(10)=MultiStageDXPerformance(iDXNomPerf)%EvapCondEff ! Evaporative Condenser Effectiveness
                   OutArgs(11)=MultiStageDXPerformance(iDXNomPerf)%TotCapFTCurveName ! Total Cooling Capacity Function of Temperature Curve Name
                   OutArgs(12)=MultiStageDXPerformance(iDXNomPerf)%TotCapFFCurveName ! Total Cooling Capacity Function of Air Flow Fraction Curve Name
                   OutArgs(13)=MultiStageDXPerformance(iDXNomPerf)%EIRFTCurveName ! Energy Input Ratio Function of Temperature Curve Name
                   OutArgs(14)=MultiStageDXPerformance(iDXNomPerf)%EIRFFCurveName ! Energy Input Ratio Function of Air Flow Fraction Curve Name
                   OutArgs(15)=MultiStageDXPerformance(iDXNomPerf)%PLFCurveName ! Part Load Fraction Correlation Curve Name
                   !OutArgs(16)='' ! Rated Waste Heat Fraction of Power Input - Only defined for Coil:Cooling:DX:MultiSpeed previously
                   !OutArgs(17)='' ! Waste Heat Function of Temperature Curve Name - Only defined for Coil:Cooling:DX:MultiSpeed previously
                   OutArgs(18)=MultiStageDXPerformance(iDXNomPerf)%SHRFTCurveName ! Sensible Heat Ratio Modifier Function of Temperature Curve Name
                   OutArgs(19)=MultiStageDXPerformance(iDXNomPerf)%SHRFFCurveName ! Sensible Heat Ratio Modifier Function of Flow Fraction Curve Name
                   CurArgs=19
                   CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ENDDO
                 ! already written
                 Written = .true.
    
             CASE('COILPERFORMANCE:DX:COOLING')
                 ! skip all of these as they have already been processed as part of COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE
                 Written = .true.

              ! If your original object starts with D, insert the rules here

              ! If your original object starts with E, insert the rules here

              ! If your original object starts with F, insert the rules here

              ! If your original object starts with G, insert the rules here

              ! If your original object starts with H, insert the rules here

              ! If your original object starts with I, insert the rules here

              ! If your original object starts with L, insert the rules here

              ! If your original object starts with M, insert the rules here

              ! If your original object starts with N, insert the rules here

              ! If your original object starts with O, insert the rules here

              ! If your original object starts with P, insert the rules here

              ! If your original object starts with R, insert the rules here

              ! If your original object starts with S, insert the rules here

              ! If your original object starts with T, insert the rules here

              ! If your original object starts with U, insert the rules here

              ! If your original object starts with V, insert the rules here

              ! If your original object starts with W, insert the rules here

              ! If your original object starts with Z, insert the rules here

    !!!   Changes for report variables, meters, tables -- update names
              CASE('OUTPUT:VARIABLE')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                IF (OutArgs(1) == Blank) THEN
                  OutArgs(1)='*'
                  nodiff=.false.
                ENDIF

                CALL ScanOutputVariablesForReplacement(  &
                   2,  &
                   DelThis,  &
                   checkrvi,  &
                   nodiff,  &
                   ObjectName,  &
                   DifLfn,      &
                   .true.,  & !OutVar
                   .false., & !MtrVar
                   .false., & !TimeBinVar
                   CurArgs, &
                   Written, &
                   .false.)
                IF (DelThis) CYCLE

              CASE ('OUTPUT:METER','OUTPUT:METER:METERFILEONLY','OUTPUT:METER:CUMULATIVE','OUTPUT:METER:CUMULATIVE:METERFILEONLY')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                CALL ScanOutputVariablesForReplacement(  &
                   1,  &
                   DelThis,  &
                   checkrvi,  &
                   nodiff,  &
                   ObjectName,  &
                   DifLfn,      &
                   .false.,  & !OutVar
                   .true., & !MtrVar
                   .false., & !TimeBinVar
                   CurArgs, &
                   Written, &
                   .false.)
                IF (DelThis) CYCLE

              CASE('OUTPUT:TABLE:TIMEBINS')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                IF (OutArgs(1) == Blank) THEN
                  OutArgs(1)='*'
                  nodiff=.false.
                ENDIF
                CALL ScanOutputVariablesForReplacement(  &
                   2,  &
                   DelThis,  &
                   checkrvi,  &
                   nodiff,  &
                   ObjectName,  &
                   DifLfn,      &
                   .false.,  & !OutVar
                   .false., & !MtrVar
                   .true., & !TimeBinVar
                   CurArgs, &
                   Written, &
                   .false.)
                IF (DelThis) CYCLE

!ExternalInterface:FunctionalMockupUnitImport:From:Variable, field 2
!ExternalInterface:FunctionalMockupUnitExport:From:Variable, field 2
              CASE('EXTERNALINTERFACE:FUNCTIONALMOCKUPUNITIMPORT:FROM:VARIABLE',  &
                   'EXTERNALINTERFACE:FUNCTIONALMOCKUPUNITEXPORT:FROM:VARIABLE')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                IF (OutArgs(1) == Blank) THEN
                  OutArgs(1)='*'
                  nodiff=.false.
                ENDIF
                CALL ScanOutputVariablesForReplacement(  &
                   2,  &
                   DelThis,  &
                   checkrvi,  &
                   nodiff,  &
                   ObjectName,  &
                   DifLfn,      &
                   .false.,  & !OutVar
                   .false., & !MtrVar
                   .false., & !TimeBinVar
                   CurArgs, &
                   Written, &
                   .false.)
                IF (DelThis) CYCLE

!EnergyManagementSystem:Sensor, field 3
              CASE('ENERGYMANAGEMENTSYSTEM:SENSOR')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                CALL ScanOutputVariablesForReplacement(  &
                   3,  &
                   DelThis,  &
                   checkrvi,  &
                   nodiff,  &
                   ObjectName,  &
                   DifLfn,      &
                   .false.,  & !OutVar
                   .false., & !MtrVar
                   .false., & !TimeBinVar
                   CurArgs, &
                   Written, &
                   .true.)
                IF (DelThis) CYCLE

              CASE('OUTPUT:TABLE:MONTHLY')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.true.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                CurVar=3
                DO Var=3,CurArgs,2
                  UCRepVarName=MakeUPPERCase(InArgs(Var))
                  OutArgs(CurVar)=InArgs(Var)
                  OutArgs(CurVar+1)=InArgs(Var+1)
                  pos=INDEX(UCRepVarName,'[')
                  IF (pos > 0) THEN
                    UCRepVarName=UCRepVarName(1:pos-1)
                    OutArgs(CurVar)=InArgs(Var)(1:pos-1)
                    OutArgs(CurVar+1)=InArgs(Var+1)
                  ENDIF
                  DelThis=.false.
                  DO Arg=1,NumRepVarNames
                    UCCompRepVarName=MakeUPPERCase(OldRepVarName(Arg))
                    IF (UCCompRepVarName(Len_Trim(UCCompRepVarName):Len_Trim(UCCompRepVarName)) == '*') THEN
                      WildMatch=.true.
                      UCCompRepVarName(Len_Trim(UCCompRepVarName):Len_Trim(UCCompRepVarName))=' '
                      pos=INDEX(TRIM(UCRepVarname),TRIM(UCCompRepVarName))
                    ELSE
                      WildMatch=.false.
                      pos=0
                      if (UCRepVarName == UCCompRepVarName) pos=1
                    ENDIF
                    IF (pos > 0 .and. pos /= 1) CYCLE
                    IF (pos > 0) THEN
                      IF (NewRepVarName(Arg) /= '<DELETE>') THEN
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        IF (NewRepVarCaution(Arg) /= Blank .and. .not. SameString(NewRepVarCaution(Arg)(1:6),'Forkeq') ) THEN
                          IF (.not. OTMVarCaution(Arg)) THEN  ! caution message not written yet
                            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                               'Output Table Monthly (old)="'//trim(OldRepVarName(Arg))//  &
                               '" conversion to Output Table Monthly (new)="'//  &
                               trim(NewRepVarName(Arg))//'" has the following caution "'//trim(NewRepVarCaution(Arg))//'".')
                            write(diflfn,fmtA) ' '
                            OTMVarCaution(Arg)=.true.
                          ENDIF
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ELSE
                        DelThis=.true.
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+1)) THEN
                        IF (.not. SameString(NewRepVarCaution(Arg)(1:6),'Forkeq')) THEN
                          ! Adding a var field.
                          CurVar=CurVar+2
                          IF (.not. WildMatch) THEN
                            OutArgs(CurVar)=NewRepVarName(Arg+1)
                          ELSE
                            OutArgs(CurVar)=TRIM(NewRepVarName(Arg+1))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                          ENDIF
                          IF (NewRepVarCaution(Arg+1) /= Blank) THEN
                            IF (.not. OTMVarCaution(Arg+1)) THEN  ! caution message not written yet
                              CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                                 'Output Table Monthly (old)="'//trim(OldRepVarName(Arg))//  &
                                 '" conversion to Output Table Monthly (new)="'//  &
                                 trim(NewRepVarName(Arg+1))//'" has the following caution "'//trim(NewRepVarCaution(Arg+1))//'".')
                              write(diflfn,fmtA) ' '
                              OTMVarCaution(Arg+1)=.true.
                            ENDIF
                          ENDIF
                          OutArgs(CurVar+1)=InArgs(Var+1)
                          nodiff=.false.
                        ENDIF
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+2)) THEN  ! only 1 more... for ForkEq
                        ! Adding a var field.
                        CurVar=CurVar+2
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg+2)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg+2))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        IF (NewRepVarCaution(Arg+2) /= Blank) THEN
                          IF (.not. OTMVarCaution(Arg+2)) THEN  ! caution message not written yet
                            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                               'Output Table Monthly (old)="'//trim(OldRepVarName(Arg))//  &
                               '" conversion to Output Table Monthly (new)="'//  &
                               trim(NewRepVarName(Arg+2))//'" has the following caution "'//trim(NewRepVarCaution(Arg+2))//'".')
                            write(diflfn,fmtA) ' '
                            OTMVarCaution(Arg+2)=.true.
                          ENDIF
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ENDIF
                      EXIT
                    ENDIF
                  ENDDO
                  IF (.not. DelThis) CurVar=CurVar+2
                ENDDO
                CurArgs=CurVar-1

              CASE('METER:CUSTOM')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                CurVar=4
                DO Var=4,CurArgs,2
                  UCRepVarName=MakeUPPERCase(InArgs(Var))
                  OutArgs(CurVar)=InArgs(Var)
                  OutArgs(CurVar+1)=InArgs(Var+1)
                  pos=INDEX(UCRepVarName,'[')
                  IF (pos > 0) THEN
                    UCRepVarName=UCRepVarName(1:pos-1)
                    OutArgs(CurVar)=InArgs(Var)(1:pos-1)
                    OutArgs(CurVar+1)=InArgs(Var+1)
                  ENDIF
                  DelThis=.false.
                  DO Arg=1,NumRepVarNames
                    UCCompRepVarName=MakeUPPERCase(OldRepVarName(Arg))
                    IF (UCCompRepVarName(Len_Trim(UCCompRepVarName):Len_Trim(UCCompRepVarName)) == '*') THEN
                      WildMatch=.true.
                      UCCompRepVarName(Len_Trim(UCCompRepVarName):Len_Trim(UCCompRepVarName))=' '
                      pos=INDEX(TRIM(UCRepVarname),TRIM(UCCompRepVarName))
                    ELSE
                      WildMatch=.false.
                      pos=0
                      if (UCRepVarName == UCCompRepVarName) pos=1
                    ENDIF
                    IF (pos > 0 .and. pos /= 1) CYCLE
                    IF (pos > 0) THEN
                      IF (NewRepVarName(Arg) /= '<DELETE>') THEN
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        IF (NewRepVarCaution(Arg) /= Blank .and. .not. SameString(NewRepVarCaution(Arg)(1:6),'Forkeq') ) THEN
                          IF (.not. CMtrVarCaution(Arg)) THEN  ! caution message not written yet
                            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                               'Custom Meter (old)="'//trim(OldRepVarName(Arg))//  &
                               '" conversion to Custom Meter (new)="'//  &
                               trim(NewRepVarName(Arg))//'" has the following caution "'//trim(NewRepVarCaution(Arg))//'".')
                            write(diflfn,fmtA) ' '
                            CMtrVarCaution(Arg)=.true.
                          ENDIF
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ELSE
                        DelThis=.true.
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+1)) THEN
                        IF (.not. SameString(NewRepVarCaution(Arg)(1:6),'Forkeq')) THEN
                          ! Adding a var field.
                          CurVar=CurVar+2
                          IF (.not. WildMatch) THEN
                            OutArgs(CurVar)=NewRepVarName(Arg+1)
                          ELSE
                            OutArgs(CurVar)=TRIM(NewRepVarName(Arg+1))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                          ENDIF
                          IF (NewRepVarCaution(Arg+1) /= Blank .and. .not. SameString(NewRepVarCaution(Arg+1)(1:6),'Forkeq') ) THEN
                            IF (.not. CMtrVarCaution(Arg+1)) THEN  ! caution message not written yet
                              CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                                 'Custom Meter (old)="'//trim(OldRepVarName(Arg))//  &
                                 '" conversion to Custom Meter (new)="'//  &
                                 trim(NewRepVarName(Arg+1))//'" has the following caution "'//trim(NewRepVarCaution(Arg+1))//'".')
                              write(diflfn,fmtA) ' '
                              CMtrVarCaution(Arg+1)=.true.
                            ENDIF
                          ENDIF
                          OutArgs(CurVar+1)=InArgs(Var+1)
                          nodiff=.false.
                        ENDIF
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+2)) THEN
                        ! Adding a var field.
                        CurVar=CurVar+2
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg+2)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg+2))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        IF (NewRepVarCaution(Arg+2) /= Blank) THEN
                          IF (.not. CMtrVarCaution(Arg+2)) THEN  ! caution message not written yet
                            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                               'Custom Meter (old)="'//trim(OldRepVarName(Arg))//  &
                               '" conversion to Custom Meter (new)="'//  &
                               trim(NewRepVarName(Arg+2))//'" has the following caution "'//trim(NewRepVarCaution(Arg+2))//'".')
                            write(diflfn,fmtA) ' '
                            CMtrVarCaution(Arg+2)=.true.
                          ENDIF
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ENDIF
                      EXIT
                    ENDIF
                  ENDDO
                  IF (.not. DelThis) CurVar=CurVar+2
                ENDDO
                CurArgs=CurVar
                DO Arg=CurVar,1,-1
                  IF (OutArgs(Arg) == Blank) THEN
                    CurArgs=CurArgs-1
                  ELSE
                    EXIT
                  ENDIF
                ENDDO

              CASE('METER:CUSTOMDECREMENT')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                CurVar=4   ! In case Source Meter would change
                DO Var=4,CurArgs,2
                  UCRepVarName=MakeUPPERCase(InArgs(Var))
                  OutArgs(CurVar)=InArgs(Var)
                  OutArgs(CurVar+1)=InArgs(Var+1)
                  pos=INDEX(UCRepVarName,'[')
                  IF (pos > 0) THEN
                    UCRepVarName=UCRepVarName(1:pos-1)
                    OutArgs(CurVar)=InArgs(Var)(1:pos-1)
                    OutArgs(CurVar+1)=InArgs(Var+1)
                  ENDIF
                  DelThis=.false.
                  DO Arg=1,NumRepVarNames
                    UCCompRepVarName=MakeUPPERCase(OldRepVarName(Arg))
                    IF (UCCompRepVarName(Len_Trim(UCCompRepVarName):Len_Trim(UCCompRepVarName)) == '*') THEN
                      WildMatch=.true.
                      UCCompRepVarName(Len_Trim(UCCompRepVarName):Len_Trim(UCCompRepVarName))=' '
                      pos=INDEX(TRIM(UCRepVarname),TRIM(UCCompRepVarName))
                    ELSE
                      WildMatch=.false.
                      pos=0
                      if (UCRepVarName == UCCompRepVarName) pos=1
                    ENDIF
                    IF (pos > 0 .and. pos /= 1) CYCLE
                    IF (pos > 0) THEN
                      IF (NewRepVarName(Arg) /= '<DELETE>') THEN
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        IF (NewRepVarCaution(Arg) /= Blank .and. .not. SameString(NewRepVarCaution(Arg)(1:6),'Forkeq') ) THEN
                          IF (.not. CMtrDVarCaution(Arg)) THEN  ! caution message not written yet
                            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                               'Custom Decrement Meter (old)="'//trim(OldRepVarName(Arg))//  &
                               '" conversion to Custom Meter (new)="'//  &
                               trim(NewRepVarName(Arg))//'" has the following caution "'//trim(NewRepVarCaution(Arg))//'".')
                            write(diflfn,fmtA) ' '
                            CMtrDVarCaution(Arg)=.true.
                          ENDIF
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ELSE
                        DelThis=.true.
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+1)) THEN
                        IF (.not. SameString(NewRepVarCaution(Arg)(1:6),'Forkeq')) THEN
                          ! Adding a var field.
                          CurVar=CurVar+2
                          IF (.not. WildMatch) THEN
                            OutArgs(CurVar)=NewRepVarName(Arg+1)
                          ELSE
                            OutArgs(CurVar)=TRIM(NewRepVarName(Arg+1))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                          ENDIF
                          IF (NewRepVarCaution(Arg+1) /= Blank .and. .not. SameString(NewRepVarCaution(Arg+1)(1:6),'Forkeq') ) THEN
                            IF (.not. CMtrDVarCaution(Arg+1)) THEN  ! caution message not written yet
                              CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                                 'Custom Decrement Meter (old)="'//trim(OldRepVarName(Arg))//  &
                                 '" conversion to Custom Decrement Meter (new)="'//  &
                                 trim(NewRepVarName(Arg+1))//'" has the following caution "'//trim(NewRepVarCaution(Arg+1))//'".')
                              write(diflfn,fmtA) ' '
                              CMtrDVarCaution(Arg+1)=.true.
                            ENDIF
                          ENDIF
                          OutArgs(CurVar+1)=InArgs(Var+1)
                          nodiff=.false.
                        ENDIF
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+2)) THEN
                        ! Adding a var field.
                        CurVar=CurVar+2
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg+2)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg+2))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        IF (NewRepVarCaution(Arg+2) /= Blank) THEN
                          IF (.not. CMtrDVarCaution(Arg+2)) THEN  ! caution message not written yet
                            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
                               'Custom Decrement Meter (old)="'//trim(OldRepVarName(Arg))//  &
                               '" conversion to Custom Meter (new)="'//  &
                               trim(NewRepVarName(Arg+2))//'" has the following caution "'//trim(NewRepVarCaution(Arg+2))//'".')
                            write(diflfn,fmtA) ' '
                            CMtrDVarCaution(Arg+2)=.true.
                          ENDIF
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ENDIF
                      EXIT
                    ENDIF
                  ENDDO
                  IF (.not. DelThis) CurVar=CurVar+2
                ENDDO
                CurArgs=CurVar
                DO Arg=CurVar,1,-1
                  IF (OutArgs(Arg) == Blank) THEN
                    CurArgs=CurArgs-1
                  ELSE
                    EXIT
                  ENDIF
                ENDDO

              CASE DEFAULT
                  IF (FindItemInList(ObjectName,NotInNew,SIZE(NotInNew)) /= 0) THEN
                    WRITE(Auditf,fmta) 'Object="'//TRIM(ObjectName)//'" is not in the "new" IDD.'
                    WRITE(Auditf,fmta) '... will be listed as comments on the new output file.'
                    CALL WriteOutIDFLinesAsComments(DifLfn,ObjectName,CurArgs,InArgs,FldNames,FldUnits)
                    Written=.true.
                    !CYCLE
                  ELSE
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                    NoDiff=.true.
                  ENDIF

              END SELECT

            ELSE   !!! Making Pretty

              ! Just making pretty -- no changes as above.
              CALL GetNewObjectDefInIDD(IDFRecords(Num)%Name,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
              OutArgs(1:CurArgs)=InArgs(1:CurArgs)
            ENDIF

            IF (DiffMinFields .and. nodiff) THEN
              ! Change in min-fields
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                NoDiff=.false.
                DO Arg=CurArgs+1,NwObjMinFlds
                  OutArgs(Arg)=NwFldDefaults(Arg)
                ENDDO
                CurArgs=MAX(NwObjMinFlds,CurArgs)
            ENDIF

            IF (NoDiff .and. DiffOnly) CYCLE

            !! reformat for better readability
            !! BUILDING,SOLUTION ALGORITHM,OUTSIDE CONVECTION ALGORITHM,INSIDE CONVECTION ALGORITHM,REPORT VARIABLE,
            !! SURFACE:HEATTRANSFER,SURFACE:HEATTRANSFER:SUBSURFACE:SHADING:DETACHED,
            !! SURFACE:SHADING:DETACHED:FIXED,SURFACE:SHADING:DETACHED:BUILDING,
            !! SURFACE:SHADING:ATTACHED,
            !! WINDOWGLASSSPECTRALDATA,
            !! FLUIDPROPERTYTEMPERATURES,
            !! FLUIDPROPERTYSATURATED,FLUIDPROPERTYSUPERHEATED,FLUIDPROPERTYCONCENTRATION
            IF (.not. Written) THEN
              CALL CheckSpecialObjects(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits,Written)
            ENDIF

            IF (.not. Written) THEN
              CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
            ENDIF

          ENDDO  ! IDFRecords

          IF (IDFRecords(NumIDFRecords)%CommtE /= CurComment) THEN
            DO xcount=IDFRecords(NumIDFRecords)%CommtE+1,CurComment
              WRITE(DifLfn,fmta) TRIM(Comments(xcount))
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ''
            ENDDO
          ENDIF

          IF (GetNumSectionsFound('Report Variable Dictionary') > 0) THEN
            ObjectName='Output:VariableDictionary'
            CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
            nodiff=.false.
            OutArgs(1)='Regular'
            CurArgs=1
            CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
          ENDIF

          INQUIRE(FILE=trim(FileNamePath)//'.rvi',EXIST=FileExist)
!          IF (FileExist) THEN
!            CALL writePreprocessorObject(DifLfn,PrognameConversion,'Warning',  &
!               'rvi file associated with this input is being processed. Review for accuracy.')
!            write(diflfn,fmtA) ' '
!          ENDIF
          CLOSE(DifLfn)
          CALL ProcessRviMviFiles(FileNamePath,'rvi')
          CALL ProcessRviMviFiles(FileNamePath,'mvi')
          CALL CloseOut
        ELSE  ! not a idf or imf
          CALL ProcessRviMviFiles(FileNamePath,'rvi')
          CALL ProcessRviMviFiles(FileNamePath,'mvi')
        ENDIF
      ELSE  ! Full name == Blank
        EndOfFile=.true.
      ENDIF

      CALL CreateNewName('Reallocate',CreatedOutputName,' ')

    ENDDO

    IF (.not. ExitBecauseBadFile) THEN
      StillWorking=.false.
      EXIT
    ELSE
      IF (.not. ArgFileBeingDone) THEN
        EndOfFile=.false.
      ELSE
        EndOfFile=.true.
        StillWorking=.false.
      ENDIF
    ENDIF
  ENDDO

  IF (ArgFileBeingDone .and. .not. LatestVersion .and. .not. ExitBecauseBadFile) THEN
    ! If this is true, then there was a "arg IDF File" on the command line and some files need to be renamed
    ErrFlag=.false.
    CALL copyfile(TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension),TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'old',ErrFlag)
    CALL copyfile(TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'new',TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension),ErrFlag)
    INQUIRE(File=TRIM(FileNamePath)//'.rvi',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.rvi',TRIM(FileNamePath)//'.rviold',ErrFlag)
    ENDIF
    INQUIRE(File=TRIM(FileNamePath)//'.rvinew',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.rvinew',TRIM(FileNamePath)//'.rvi',ErrFlag)
    ENDIF
    INQUIRE(File=TRIM(FileNamePath)//'.mvi',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.mvi',TRIM(FileNamePath)//'.mviold',ErrFlag)
    ENDIF
    INQUIRE(File=TRIM(FileNamePath)//'.mvinew',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.mvinew',TRIM(FileNamePath)//'.mvi',ErrFlag)
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CreateNewIDFUsingRules
