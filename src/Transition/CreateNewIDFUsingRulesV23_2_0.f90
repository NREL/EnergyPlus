MODULE SetVersion

USE DataStringGlobals
USE DataVCompareGlobals

PUBLIC

CONTAINS

SUBROUTINE SetThisVersionVariables()
      ! TODO: Update this section as appropriate
      VerString='Conversion 23.1 => 23.2'
      VersionNum=23.2
      ! Starting with version 22.1, the version string requires 4 characters
      ! The original sVersionNum variable is a 3 character length string
      ! If we just change that variable to be 4 characters, it could break everything before 22.1
      ! So instead, let's just move forward with a new 4 character string and use that in this file and the future
      ! If we get to version 100.1 and we are still using this Fortran transition then well....we can deal with it then
      sVersionNum = '***'
      sVersionNumFourChars='23.2'
      IDDFileNameWithPath=TRIM(ProgramPath)//'V23-1-0-Energy+.idd'
      NewIDDFileNameWithPath=TRIM(ProgramPath)//'V23-2-0-Energy+.idd'
      RepVarFileNameWithPath=TRIM(ProgramPath)//'Report Variables 23-1-0 to 23-2-0.csv'
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
  INTEGER CurVarIterator
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
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: POutArgs

  LOGICAL :: ConnComp
  LOGICAL :: ConnCompCtrl
  LOGICAL :: FileExist
  CHARACTER(len=MaxNameLength) :: CreatedOutputName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: DeleteThisRecord
  INTEGER :: COutArgs
  CHARACTER(len=16) :: UnitsField

  LOGICAL :: ErrFlag

  INTEGER :: I, CurField, NewField, KAindex=0, SearchNum
  INTEGER :: AlphaNumI
  REAL :: SaveNumber

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                     I N S E R T    L O C A L    V A R I A B L E S    H E R E                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! TODO: Move to V10_0_0.f90 when available
  ! For Defaulting now-required RunPeriod Name
  INTEGER :: TotRunPeriods = 0
  INTEGER :: runPeriodNum = 0
  INTEGER :: iterateRunPeriod = 0
  INTEGER :: wwhpEqFtCoolIndex = 0
  INTEGER :: wwhpEqFtHeatIndex = 0
  INTEGER :: wahpEqFtCoolIndex = 0
  INTEGER :: wahpEqFtHeatIndex = 0
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: CurrentRunPeriodNames
  integer :: Num1
  CHARACTER(len=MaxNameLength) :: SurroundingField1, SurroundingField2, matchedSurroundingName
  CHARACTER(len=20) :: PotentialRunPeriodName
  ! END OF TODO

  TYPE CoilLatentTransitionInfo
      CHARACTER(len=MaxNameLength) :: ParentType
      CHARACTER(len=MaxNameLength) :: ParentName
      CHARACTER(len=MaxNameLength) :: NewCurveName
      CHARACTER(len=MaxNameLength) :: HeatingCoilType
      CHARACTER(len=MaxNameLength) :: CoolingCoilType
      CHARACTER(len=MaxNameLength) :: HeatingCoilName
      CHARACTER(len=MaxNameLength) :: CoolingCoilName
      CHARACTER(len=MaxNameLength) :: cMaxCyclingRate = ''
      CHARACTER(len=MaxNameLength) :: cHeatPumpTimeConst = ''
      CHARACTER(len=MaxNameLength) :: cFractionOnCycle = ''
      CHARACTER(len=MaxNameLength) :: cHPDelayTime = ''
      LOGICAL :: ActuallyCreateCurve = .false.
  END TYPE CoilLatentTransitionInfo
  TYPE(CoilLatentTransitionInfo), ALLOCATABLE, DIMENSION(:) :: CoilLatentStuff
  INTEGER :: NumCoilLatentStuff = 0
  INTEGER :: Num3
  INTEGER :: CoilLatentStuffCounter = 0
  REAL :: latentTau
  REAL :: latentNmax
  REAL :: latentA
  REAL :: latentCd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                            E N D    O F    I N S E R T    L O C A L    V A R I A B L E S    H E R E                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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

          ! Clean up from any previous passes, then re-allocate. These are for the 'standard' stuff, not your own
          ! Do not add anything here!
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
          IF(ALLOCATED(POutArgs)) DEALLOCATE(POutArgs)
          IF(ALLOCATED(OutArgs)) DEALLOCATE(OutArgs)
          ALLOCATE(Alphas(MaxAlphaArgsFound),Numbers(MaxNumericArgsFound))
          ALLOCATE(InArgs(MaxTotalArgs))
          ALLOCATE(TempArgs(MaxTotalArgs))
          ALLOCATE(AorN(MaxTotalArgs),ReqFld(MaxTotalArgs),FldNames(MaxTotalArgs),FldDefaults(MaxTotalArgs),FldUnits(MaxTotalArgs))
          ALLOCATE(NwAorN(MaxTotalArgs),NwReqFld(MaxTotalArgs),NwFldNames(MaxTotalArgs),NwFldDefaults(MaxTotalArgs),NwFldUnits(MaxTotalArgs))
          ALLOCATE(OutArgs(MaxTotalArgs))
          ALLOCATE(POutArgs(MaxTotalArgs))
          ALLOCATE(DeleteThisRecord(NumIDFRecords))
          DeleteThisRecord=.false.

          NoVersion=.true.
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'VERSION') CYCLE
            NoVersion=.false.
            EXIT
          ENDDO

          DO Num=1,NumIDFRecords
            IF (DeleteThisRecord(Num)) THEN
              Write(DifLfn,fmta) '! Deleting: '//TRIM(IDFRecords(Num)%Name)//'="'//TRIM(IDFRecords(Num)%Alphas(1))//'".'
            ENDIF
          ENDDO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                    P R E P R O C E S S I N G                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Do any kind of Preprocessing that is needed here (eg: a first pass on objects to store some attributes etc)
          ALLOCATE(CoilLatentStuff(NumIDFRecords))  ! Excessively large, but better than resizing the array over and over
          DO Num=1,NumIDFRecords
              SELECT CASE (MakeUPPERCase(IDFRecords(Num)%Name))
              CASE ('AIRLOOPHVAC:UNITARYSYSTEM')
                  NumCoilLatentStuff = NumCoilLatentStuff + 1
                  CoilLatentStuff(NumCoilLatentStuff)%ParentType = 'AIRLOOPHVAC:UNITARYSYSTEM'
                  CoilLatentStuff(NumCoilLatentStuff)%ParentName = MakeUPPERCase(IDFRecords(Num)%Alphas(1))
                  CoilLatentStuff(NumCoilLatentStuff)%NewCurveName = TRIM(IDFRecords(Num)%Alphas(1)) // "-AutogeneratedPLFCurve"
                  CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType = MakeUPPERCase(IDFRecords(Num)%Alphas(12))
                  IF (CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilName = MakeUPPERCase(IDFRecords(Num)%Alphas(13))
                  CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType = MakeUPPERCase(IDFRecords(Num)%Alphas(14))
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:DX:VARIABLESPEED') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilName = MakeUPPERCase(IDFRecords(Num)%Alphas(15))
                  IF (IDFRecords(Num)%NumNumbers >= 19) CoilLatentStuff(NumCOilLatentStuff)%cMaxCyclingRate = IDFRecords(Num)%Numbers(19)
                  IF (IDFRecords(Num)%NumNumbers >= 20) CoilLatentStuff(NumCOilLatentStuff)%cHeatPumpTimeConst = IDFRecords(Num)%Numbers(20)
                  IF (IDFRecords(Num)%NumNumbers >= 21) CoilLatentStuff(NumCOilLatentStuff)%cFractionOnCycle = IDFRecords(Num)%Numbers(21)
                  IF (IDFRecords(Num)%NumNumbers >= 22) CoilLatentStuff(NumCOilLatentStuff)%cHPDelayTime = IDFRecords(Num)%Numbers(22)
              CASE ('AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR')
                  NumCoilLatentStuff = NumCoilLatentStuff + 1
                  CoilLatentStuff(NumCoilLatentStuff)%ParentType = 'AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR'
                  CoilLatentStuff(NumCoilLatentStuff)%ParentName = MakeUPPERCase(IDFRecords(Num)%Alphas(1))
                  CoilLatentStuff(NumCoilLatentStuff)%NewCurveName = TRIM(IDFRecords(Num)%Alphas(1)) // "-AutogeneratedPLFCurve"
                  CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType = MakeUPPERCase(IDFRecords(Num)%Alphas(8))
                  IF (CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilName = MakeUPPERCase(IDFRecords(Num)%Alphas(9))
                  CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType = MakeUPPERCase(IDFRecords(Num)%Alphas(10))
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilName = MakeUPPERCase(IDFRecords(Num)%Alphas(11))
                  IF (IDFRecords(Num)%NumNumbers >= 4) CoilLatentStuff(NumCOilLatentStuff)%cMaxCyclingRate = IDFRecords(Num)%Numbers(4)
                  IF (IDFRecords(Num)%NumNumbers >= 5) CoilLatentStuff(NumCOilLatentStuff)%cHeatPumpTimeConst = IDFRecords(Num)%Numbers(5)
                  IF (IDFRecords(Num)%NumNumbers >= 6) CoilLatentStuff(NumCOilLatentStuff)%cFractionOnCycle = IDFRecords(Num)%Numbers(6)
                  IF (IDFRecords(Num)%NumNumbers >= 7) CoilLatentStuff(NumCOilLatentStuff)%cHPDelayTime = IDFRecords(Num)%Numbers(7)
              CASE ('ZONEHVAC:WATERTOAIRHEATPUMP')
                  NumCoilLatentStuff = NumCoilLatentStuff + 1
                  CoilLatentStuff(NumCoilLatentStuff)%ParentType = 'ZONEHVAC:WATERTOAIRHEATPUMP'
                  CoilLatentStuff(NumCoilLatentStuff)%ParentName = MakeUPPERCase(IDFRecords(Num)%Alphas(1))
                  CoilLatentStuff(NumCoilLatentStuff)%NewCurveName = TRIM(IDFRecords(Num)%Alphas(1)) // "-AutogeneratedPLFCurve"
                  CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType = MakeUPPERCase(IDFRecords(Num)%Alphas(9))
                  IF (CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilType == 'COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  CoilLatentStuff(NumCoilLatentStuff)%HeatingCoilName = MakeUPPERCase(IDFRecords(Num)%Alphas(10))
                  CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType = MakeUPPERCase(IDFRecords(Num)%Alphas(11))
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  IF (CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilType == 'COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT') CoilLatentStuff(NumCoilLatentStuff)%ActuallyCreateCurve = .true.
                  CoilLatentStuff(NumCoilLatentStuff)%CoolingCoilName = MakeUPPERCase(IDFRecords(Num)%Alphas(12))
                  IF (IDFRecords(Num)%NumNumbers >= 7) CoilLatentStuff(NumCOilLatentStuff)%cMaxCyclingRate = IDFRecords(Num)%Numbers(7)
                  IF (IDFRecords(Num)%NumNumbers >= 8) CoilLatentStuff(NumCOilLatentStuff)%cHeatPumpTimeConst = IDFRecords(Num)%Numbers(8)
                  IF (IDFRecords(Num)%NumNumbers >= 9) CoilLatentStuff(NumCOilLatentStuff)%cFractionOnCycle = IDFRecords(Num)%Numbers(9)
                  IF (IDFRecords(Num)%NumNumbers >= 10) CoilLatentStuff(NumCOilLatentStuff)%cHPDelayTime = IDFRecords(Num)%Numbers(10)
              END SELECT
          ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                       P R O C E S S I N G                                                        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          CALL DisplayString('Processing IDF -- Processing idf objects . . .')
          DO Num=1,NumIDFRecords

            IF (DeleteThisRecord(Num)) CYCLE
            DO xcount=IDFRecords(Num)%CommtS+1,IDFRecords(Num)%CommtE
              WRITE(DifLfn,fmta) TRIM(Comments(xcount))
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ''
            ENDDO
            IF (NoVersion .and. Num == 1) THEN
              CALL GetNewObjectDefInIDD('VERSION',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
              OutArgs(1) = sVersionNumFourChars
              CurArgs=1
              CALL ShowWarningError('No version found in file, defaulting to '//sVersionNumFourChars,Auditf)
              CALL WriteOutIDFLinesAsComments(DifLfn,'Version',CurArgs,OutArgs,NwFldNames,NwFldUnits)
            ENDIF

     ! deleted objects.  no transition.
     ! eg:  IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'PROGRAMCONTROL') CYCLE

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   IF NOT ONLY MAKING PRETTY    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            IF (.not. MakingPretty) THEN

              SELECT CASE (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)))

              CASE ('VERSION')
                IF ((InArgs(1)(1:4)) == sVersionNumFourChars .and. ArgFile) THEN
                  CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                  CLOSE(diflfn,STATUS='DELETE')
                  LatestVersion=.true.
                  EXIT
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1) = sVersionNumFourChars
                NoDiff=.false.

    ! changes for this version, pick one of the spots to add rules, this will reduce the possibility of merge conflicts

!             CASE('OBJECTNAMEHERE')
!                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                 nodiff=.false.
!                 OutArgs(1)=InArgs(1)
!                 OutArgs(2) = 'SequentialLoad'
!                 OutArgs(3:CurArgs+1)=InArgs(2:CurArgs)
!                 CurArgs = CurArgs + 1

              ! If your original object starts with A, insert the rules here

              ! If your original object starts with B, insert the rules here

              ! If your original object starts with C, insert the rules here
              CASE('COIL:COOLING:DX:TWOSPEED')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:6)=InArgs(1:6)
                OutArgs(7) = ''  ! new high speed 2017 rated field
                OutArgs(8) = ''  ! new high speed 2023 rated field
                OutArgs(9:20)=InArgs(7:18)
                OutArgs(21) = ''  ! new low speed 2017 rated field
                OutArgs(22) = ''  ! new low speed 2023 rated field
                OutArgs(23:CurArgs+4)=InArgs(19:CurArgs)
                CurArgs = CurArgs + 4

              CASE('COIL:COOLING:DX:VARIABLESPEED')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:9)=InArgs(1:9)
                DO Num3 = 1, NumCoilLatentStuff
                    IF (('COIL:COOLING:DX:VARIABLESPEED' /= CoilLatentStuff(Num3)%CoolingCoilType) .OR. (InArgs(1) /= CoilLatentStuff(Num3)%CoolingCoilName)) CYCLE
                    OutArgs(10) = CoilLatentStuff(Num3)%cMaxCyclingRate
                    OutArgs(11) = CoilLatentStuff(Num3)%cHeatPumpTimeConst
                    OutArgs(12) = CoilLatentStuff(Num3)%cHPDelayTime
                    EXIT
                END DO
                CurArgs = CurArgs + 3
                ! manipulate all the speeds regardless of CurArgs count
                OutArgs(13:28)=InArgs(10:25)
                OutArgs(29) = ''  ! new speed 1 2017 rated field
                OutArgs(30) = ''  ! new speed 1 2023 rated field
                OutArgs(31:40)=InArgs(26:35)
                OutArgs(41)='' ! new speed 2 2017 rated field
                OutArgs(42)='' ! new speed 2 2023 rated field
                OutArgs(43:52)=InArgs(36:45)
                OutArgs(53)='' ! new speed 3 2017 rated field
                OutArgs(54)='' ! new speed 3 2023 rated field
                OutArgs(55:64)=InArgs(46:55)
                OutArgs(65)='' ! new speed 4 2017 rated field
                OutArgs(66)='' ! new speed 4 2023 rated field
                OutArgs(67:76)=InArgs(56:65)
                OutArgs(77)='' ! new speed 5 2017 rated field
                OutArgs(78)='' ! new speed 5 2023 rated field
                OutArgs(79:88)=InArgs(66:75)
                OutArgs(89)='' ! new speed 6 2017 rated field
                OutArgs(90)='' ! new speed 6 2023 rated field
                OutArgs(91:100)=InArgs(76:85)
                OutArgs(101)='' ! new speed 7 2017 rated field
                OutArgs(102)='' ! new speed 7 2023 rated field
                OutArgs(103:112)=InArgs(86:95)
                OutArgs(113)='' ! new speed 8 2017 rated field
                OutArgs(114)='' ! new speed 8 2023 rated field
                OutArgs(115:124)=InArgs(96:105)
                OutArgs(125)='' ! new speed 9 2017 rated field
                OutArgs(126)='' ! new speed 9 2023 rated field
                OutArgs(127:136)=InArgs(106:115)
                OutArgs(137)='' ! new speed 10 2017 rated field
                OutArgs(138)='' ! new speed 10 2023 rated field
                OutArgs(139:CurArgs+20)=InArgs(116:CurArgs)
                ! But then only modify CurArgs based on the number of fields
                IF (CurArgs >= 25) CurArgs = CurArgs + 2  ! this will always trigger for speed 1
                IF (CurArgs >= 35) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 45) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 55) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 65) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 75) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 85) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 95) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 105) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs >= 115) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs

              CASE('COIL:HEATING:DX:VARIABLESPEED')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                ! manipulate all the speeds regardless of CurArgs count
                OutArgs(1:21)=InArgs(1:21)
                OutArgs(22) = ''  ! new speed 1 2017 rated field
                OutArgs(23) = ''  ! new speed 1 2023 rated field
                OutArgs(24:30)=InArgs(22:28)
                OutArgs(31)='' ! new speed 2 2017 rated field
                OutArgs(32)='' ! new speed 2 2023 rated field
                OutArgs(33:39)=InArgs(29:35)
                OutArgs(40)='' ! new speed 3 2017 rated field
                OutArgs(41)='' ! new speed 3 2023 rated field
                OutArgs(42:48)=InArgs(36:42)
                OutArgs(49)='' ! new speed 4 2017 rated field
                OutArgs(50)='' ! new speed 4 2023 rated field
                OutArgs(51:57)=InArgs(43:49)
                OutArgs(58)='' ! new speed 5 2017 rated field
                OutArgs(59)='' ! new speed 5 2023 rated field
                OutArgs(60:66)=InArgs(50:56)
                OutArgs(67)='' ! new speed 6 2017 rated field
                OutArgs(68)='' ! new speed 6 2023 rated field
                OutArgs(69:75)=InArgs(57:63)
                OutArgs(76)='' ! new speed 7 2017 rated field
                OutArgs(77)='' ! new speed 7 2023 rated field
                OutArgs(78:84)=InArgs(64:70)
                OutArgs(85)='' ! new speed 8 2017 rated field
                OutArgs(86)='' ! new speed 8 2023 rated field
                OutArgs(87:93)=InArgs(71:77)
                OutArgs(94)='' ! new speed 9 2017 rated field
                OutArgs(95)='' ! new speed 9 2023 rated field
                OutArgs(96:102)=InArgs(78:84)
                OutArgs(103)='' ! new speed 10 2017 rated field
                OutArgs(104)='' ! new speed 10 2023 rated field
                OutArgs(105:CurArgs+20)=InArgs(85:CurArgs)
                ! But then only modify CurArgs based on the number of fields
                IF (CurArgs .GE. 21) CurArgs = CurArgs + 2  ! this will always trigger for speed 1
                IF (CurArgs .GE. 38) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 35) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 42) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 49) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 56) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 63) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 70) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 77) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
                IF (CurArgs .GE. 84) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs

!              CASE('COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT')
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:6)=InArgs(1:6)
!                OutArgs(7) = ''  ! 2017 rated field
!                OutArgs(8) = ''  ! 2023 rated field
!                OutArgs(9:CurArgs+2)=InArgs(7:CurArgs)
!                CurArgs = CurArgs + 2
!
!              CASE('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT')
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                ! manipulate all the speeds regardless of CurArgs count
!                OutArgs(1:18)=InArgs(1:18)
!                OutArgs(19) = ''  ! new speed 1 2017 rated field
!                OutArgs(20) = ''  ! new speed 1 2023 rated field
!                OutArgs(21:33)=InArgs(19:31)
!                OutArgs(34)='' ! new speed 2 2017 rated field
!                OutArgs(35)='' ! new speed 2 2023 rated field
!                OutArgs(36:48)=InArgs(32:44)
!                OutArgs(49)='' ! new speed 3 2017 rated field
!                OutArgs(50)='' ! new speed 3 2023 rated field
!                OutArgs(51:63)=InArgs(45:57)
!                OutArgs(64)='' ! new speed 4 2017 rated field
!                OutArgs(65)='' ! new speed 4 2023 rated field
!                OutArgs(66:78)=InArgs(58:70)
!                OutArgs(79)='' ! new speed 5 2017 rated field
!                OutArgs(80)='' ! new speed 5 2023 rated field
!                OutArgs(81:93)=InArgs(71:83)
!                OutArgs(94)='' ! new speed 6 2017 rated field
!                OutArgs(95)='' ! new speed 6 2023 rated field
!                OutArgs(96:108)=InArgs(84:96)
!                OutArgs(109)='' ! new speed 7 2017 rated field
!                OutArgs(110)='' ! new speed 7 2023 rated field
!                OutArgs(111:123)=InArgs(97:109)
!                OutArgs(124)='' ! new speed 8 2017 rated field
!                OutArgs(125)='' ! new speed 8 2023 rated field
!                OutArgs(126:138)=InArgs(110:122)
!                OutArgs(139)='' ! new speed 9 2017 rated field
!                OutArgs(140)='' ! new speed 9 2023 rated field
!                OutArgs(141:153)=InArgs(123:135)
!                OutArgs(154)='' ! new speed 10 2017 rated field
!                OutArgs(155)='' ! new speed 10 2023 rated field
!                OutArgs(156:CurArgs+20)=InArgs(136:CurArgs)
!                ! But then only modify CurArgs based on the number of fields
!                IF (CurArgs .GE. 18) CurArgs = CurArgs + 2  ! this will always trigger for speed 1
!                IF (CurArgs .GE. 31) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 44) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 57) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 70) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 83) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 96) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 109) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 122) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs
!                IF (CurArgs .GE. 135) CurArgs = CurArgs + 2  ! only do this speed if we have that many inputs

              ! If your original object starts with D, insert the rules here

              ! If your original object starts with E, insert the rules here

              ! If your original object starts with F, insert the rules here

              ! If your original object starts with G, insert the rules here

              ! If your original object starts with H, insert the rules here
              CASE('HVACTEMPLATE:ZONE:WATERTOAIRHEATPUMP')  ! PR 10043
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                !- remove N20 F29 Fraction of On-Cycle Power Use
                OutArgs(1:28)=InArgs(1:28)
                OutArgs(29:CurArgs-1)=InArgs(30:CurArgs)
                CurArgs = CurArgs - 1

              CASE('AIRLOOPHVAC:UNITARYSYSTEM')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  nodiff=.false.
                  OutArgs(1:41)=InArgs(1:41)
                  OutArgs(42:CurArgs-4) = InArgs(46:CurArgs)
                  CurArgs = CurArgs - 4
                  CALL WriteOutIDFLines(DifLfn,'AirLoopHVAC:UnitarySystem',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('AIRLOOPHVAC:UNITARYSYSTEM' /= CoilLatentStuff(Num3)%ParentType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%ParentName)) CYCLE
                      nodiff=.false.
                      CALL GetNewObjectDefInIDD('Curve:Linear',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                      IF (TRIM(CoilLatentStuff(Num3)%cHeatPumpTimeConst) == '') THEN
                          latentTau = 60
                      ELSE
                          READ(CoilLatentStuff(Num3)%cHeatPumpTimeConst, *) latentTau
                      END IF
                      IF (TRIM(CoilLatentStuff(Num3)%cMaxCyclingRate) == '') THEN
                          latentNmax = 2.5  ! does this need to be converted to seconds?
                      ELSE
                          READ(CoilLatentStuff(Num3)%cMaxCyclingRate, *) latentNmax
                      END IF
                      latentA = 4 * latentTau * (latentNmax / 3600.0)
                      latentCd = latentA * (1 - EXP(-1 / latentA))
                      OutArgs(1) = CoilLatentStuff(Num3)%NewCurveName
                      WRITE(OutArgs(2), *) (1 - latentCd)
                      WRITE(OutArgs(3), *) latentCd
                      OutArgs(4) = "0.0"
                      OutArgs(5) = "1.0"
                      OutArgs(6) = "0.0"
                      OutArgs(7) = "1.0"
                      OutArgs(8) = "Dimensionless"
                      OutArgs(9) = "Dimensionless"
                      CurArgs = 9
                      CALL WriteOutIDFLines(DifLfn,'Curve:Linear',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                      EXIT
                  END DO
                  Written = .TRUE.

              CASE('AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  nodiff=.false.
                  OutArgs(1:14)=InArgs(1:14)
                  OutArgs(15:CurArgs-4) = InArgs(19:CurArgs)
                  CurArgs = CurArgs - 4
                  CALL WriteOutIDFLines(DifLfn,'AirLoopHVAC:UnitaryHeatPump:WaterToAir',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('AIRLOOPHVAC:UNITARYHEATPUMP:WATERTOAIR' /= CoilLatentStuff(Num3)%ParentType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%ParentName)) CYCLE
                      nodiff=.false.
                      CALL GetNewObjectDefInIDD('Curve:Linear',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                      IF (TRIM(CoilLatentStuff(Num3)%cHeatPumpTimeConst) == '') THEN
                          latentTau = 60
                      ELSE
                          READ(CoilLatentStuff(Num3)%cHeatPumpTimeConst, *) latentTau
                      END IF
                      IF (TRIM(CoilLatentStuff(Num3)%cMaxCyclingRate) == '') THEN
                          latentNmax = 2.5  ! does this need to be converted to seconds?
                      ELSE
                          READ(CoilLatentStuff(Num3)%cMaxCyclingRate, *) latentNmax
                      END IF
                      latentA = 4 * latentTau * (latentNmax / 3600.0)
                      latentCd = latentA * (1 - EXP(-1 / latentA))
                      OutArgs(1) = CoilLatentStuff(Num3)%NewCurveName
                      WRITE(OutArgs(2), *) (1 - latentCd)
                      WRITE(OutArgs(3), *) latentCd
                      OutArgs(4) = "0.0"
                      OutArgs(5) = "1.0"
                      OutArgs(6) = "0.0"
                      OutArgs(7) = "1.0"
                      OutArgs(8) = "Dimensionless"
                      OutArgs(9) = "Dimensionless"
                      CurArgs = 9
                      CALL WriteOutIDFLines(DifLfn,'Curve:Linear',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                      EXIT
                  END DO
                  Written = .TRUE.

              CASE('ZONEHVAC:WATERTOAIRHEATPUMP')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  nodiff=.false.
                  OutArgs(1:18)=InArgs(1:18)
                  OutArgs(19:CurArgs-4) = InArgs(23:CurArgs)
                  CurArgs = CurArgs - 4
                  CALL WriteOutIDFLines(DifLfn,'ZoneHVAC:WaterToAirHeatPump',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('ZONEHVAC:WATERTOAIRHEATPUMP' /= CoilLatentStuff(Num3)%ParentType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%ParentName)) CYCLE
                      nodiff=.false.
                      CALL GetNewObjectDefInIDD('Curve:Linear',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                      IF (TRIM(CoilLatentStuff(Num3)%cHeatPumpTimeConst) == '') THEN
                          latentTau = 60
                      ELSE
                          READ(CoilLatentStuff(Num3)%cHeatPumpTimeConst, *) latentTau
                      END IF
                      IF (TRIM(CoilLatentStuff(Num3)%cMaxCyclingRate) == '') THEN
                          latentNmax = 2.5  ! does this need to be converted to seconds?
                      ELSE
                          READ(CoilLatentStuff(Num3)%cMaxCyclingRate, *) latentNmax
                      END IF
                      latentA = 4 * latentTau * (latentNmax / 3600.0)
                      latentCd = latentA * (1 - EXP(-1 / latentA))
                      OutArgs(1) = CoilLatentStuff(Num3)%NewCurveName
                      WRITE(OutArgs(2), *) (1 - latentCd)
                      WRITE(OutArgs(3), *) latentCd
                      OutArgs(4) = "0.0"
                      OutArgs(5) = "1.0"
                      OutArgs(6) = "0.0"
                      OutArgs(7) = "1.0"
                      OutArgs(8) = "Dimensionless"
                      OutArgs(9) = "Dimensionless"
                      CurArgs = 9
                      CALL WriteOutIDFLines(DifLfn,'Curve:Linear',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                      EXIT
                  END DO
                  Written = .TRUE.

!              CASE('COIL:COOLING:DX:VARIABLESPEED')  ! PR 10043
!                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                  DO Num3 = 1, NumCoilLatentStuff
!                      IF (('COIL:COOLING:DX:VARIABLESPEED' /= CoilLatentStuff(Num3)%CoolingCoilType) .OR. (InArgs(1) /= CoilLatentStuff(Num3)%CoolingCoilName)) CYCLE
!                      nodiff=.false.
!                      OutArgs(1:9)=InArgs(1:9)
!                      OutArgs(10) = CoilLatentStuff(Num3)%cMaxCyclingRate
!                      OutArgs(11) = CoilLatentStuff(Num3)%cHeatPumpTimeConst
!                      OutArgs(12) = CoilLatentStuff(Num3)%cHPDelayTime
!                      OutArgs(13:CurArgs+3) = InArgs(10:CurArgs)
!                      CurArgs = CurArgs + 3
!                      EXIT
!                  END DO

              CASE('COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION' /= CoilLatentStuff(Num3)%CoolingCoilType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%CoolingCoilName)) CYCLE
                      nodiff=.false.
                      OutArgs(1:27)=InArgs(1:27)
                      OutArgs(28) = CoilLatentStuff(Num3)%NewCurveName
                      OutArgs(29) = CoilLatentStuff(Num3)%cMaxCyclingRate
                      OutArgs(30) = CoilLatentStuff(Num3)%cHeatPumpTimeConst
                      OutArgs(31) = CoilLatentStuff(Num3)%cHPDelayTime
                      CurArgs = 31
                      EXIT
                  END DO

              CASE('COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION' /= CoilLatentStuff(Num3)%HeatingCoilType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%HeatingCoilName)) CYCLE
                      nodiff=.false.
                      OutArgs(1:24)=InArgs(1:24)
                      OutArgs(25) = CoilLatentStuff(Num3)%NewCurveName
                      CurArgs = 25
                      EXIT
                  END DO

              CASE('COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('COIL:COOLING:WATERTOAIRHEATPUMP:EQUATIONFIT' /= CoilLatentStuff(Num3)%CoolingCoilType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%CoolingCoilName)) CYCLE
                      nodiff=.false.
                      OutArgs(1:18)=InArgs(1:18)
                      OutArgs(19) = CoilLatentStuff(Num3)%NewCurveName
                      OutArgs(20:21) = InArgs(19:20)
                      OutArgs(22) = CoilLatentStuff(Num3)%cMaxCyclingRate
                      OutArgs(23) = CoilLatentStuff(Num3)%cHeatPumpTimeConst
                      OutArgs(24) = CoilLatentStuff(Num3)%cHPDelayTime
                      CurArgs = 24
                      EXIT
                  END DO

              CASE('COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('COIL:HEATING:WATERTOAIRHEATPUMP:EQUATIONFIT' /= CoilLatentStuff(Num3)%HeatingCoilType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%HeatingCoilName)) CYCLE
                      nodiff=.false.
                      OutArgs(1:24)=InArgs(1:24)
                      OutArgs(25) = CoilLatentStuff(Num3)%NewCurveName
                      CurArgs = 25
                      EXIT
                  END DO

              CASE('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT')  ! PR 10043
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  DO Num3 = 1, NumCoilLatentStuff
                      IF (('COIL:COOLING:WATERTOAIRHEATPUMP:VARIABLESPEEDEQUATIONFIT' /= CoilLatentStuff(Num3)%CoolingCoilType) .OR. (MakeUPPERCase(InArgs(1)) /= CoilLatentStuff(Num3)%CoolingCoilName)) CYCLE
                      nodiff=.false.
                      OutArgs(1:12)=InArgs(1:12)
                      OutArgs(13) = CoilLatentStuff(Num3)%cMaxCyclingRate
                      OutArgs(14) = CoilLatentStuff(Num3)%cHeatPumpTimeConst
                      OutArgs(15) = CoilLatentStuff(Num3)%cHPDelayTime
                      CurArgs = CurArgs + 3
                      EXIT
                  END DO

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


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                   Changes for report variables, meters, tables -- update names                                   !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !!!   Changes for other objects that reference meter names -- update names
              CASE('DEMANDMANAGERASSIGNMENTLIST',  &
                   'UTILITYCOST:TARIFF')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.

                CALL ScanOutputVariablesForReplacement(  &
                   2,  &
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

              CASE('ELECTRICLOADCENTER:DISTRIBUTION')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.

               ! Field 6  A5,  \field Generator Track Meter Scheme Meter Name
                CALL ScanOutputVariablesForReplacement(  &
                   6,  &
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

               ! Field 12    A11, \field Storage Control Track Meter Name
                CALL ScanOutputVariablesForReplacement(  &
                   12,  &
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

              ! ANY OTHER OBJECT
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   IF ONLY MAKING PRETTY    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

          CALL DisplayString('Processing IDF -- Processing idf objects complete.')
          IF (IDFRecords(NumIDFRecords)%CommtE /= CurComment) THEN
            DO xcount=IDFRecords(NumIDFRecords)%CommtE+1,CurComment
              WRITE(DifLfn,fmta) TRIM(Comments(xcount))
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ''
            ENDDO
          ENDIF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                E N D    O F   P R O C E S S I N G                                                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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
