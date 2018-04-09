MODULE SetVersion

USE DataStringGlobals
USE DataVCompareGlobals

PUBLIC

CONTAINS

SUBROUTINE SetThisVersionVariables()
      VerString='Conversion 8.8 => 8.9'
      VersionNum=8.9
      sVersionNum='8.9'
      IDDFileNameWithPath=TRIM(ProgramPath)//'V8-8-0-Energy+.idd'
      NewIDDFileNameWithPath=TRIM(ProgramPath)//'V8-9-0-Energy+.idd'
      RepVarFileNameWithPath=TRIM(ProgramPath)//'Report Variables 8-8-0 to 8-9-0.csv'
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
  REAL :: DXTempValue1 = 0.0
  REAL :: DXTempValue2 = 0.0
  REAL :: DXRatio = 0.0
  INTEGER :: DXSpeedStartArgNum = 0
  INTEGER :: DXNomSpeedStartArgNum = 0

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

    ! changes for this version - 8.8 --> 8.9

             CASE('ZONEHVAC:EQUIPMENTLIST')
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 nodiff=.false.
                 OutArgs(1)=InArgs(1)
                 OutArgs(2) = 'SequentialLoad'
                 OutArgs(3:CurArgs+1)=InArgs(2:CurArgs)
                 CurArgs = CurArgs + 1

              CASE('GROUNDHEATEXCHANGER:VERTICAL')
                 nodiff=.false.
                 ObjectName='GroundHeatExchanger:System'
                 ! store the date for later
                 TempArgs=InArgs
                 TempArgsNum=CurArgs

                 ! write the GLHE:System object
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1:4)=TempArgs(1:4) ! no change
                 OutArgs(5)='Site:GroundTemperature:Undisturbed:KusudaAchenbach'
                 OutArgs(6)=TRIM(TempArgs(1)) // TRIM(' Ground Temps')
                 OutArgs(7:8)=TempArgs(8:9)
                 OutArgs(9)=TRIM(TempArgs(1)) // TRIM(' Response Factors')
                 CurArgs=9
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the GLHE:Props object
                 ObjectName='GroundHeatExchanger:Vertical:Properties'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Properties')
                 OutArgs(2)='1'
                 OutArgs(3)=TempArgs(6)
                 !OutArgs(4)=TempArgs(7)
                 READ(TempArgs(7), *) GLHETempVal
                 WRITE(OutArgs(4), *) GLHETempVal * 2
                 OutArgs(5)=TempArgs(11)
                 OutArgs(6)='3.90E+06'
                 OutArgs(7)=TempArgs(12)
                 OutArgs(8)='1.77E+06'
                 OutArgs(9)=TempArgs(13)
                 OutArgs(10)=TempArgs(15)
                 OutArgs(11)=TempArgs(14)
                 CurArgs=11
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the ground temps object
                 ObjectName='Site:GroundTemperature:Undisturbed:KusudaAchenbach'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Ground Temps')
                 OutArgs(2)=TempArgs(8)
                 OutArgs(3)='920'
                 GLHETempVal = 0.0
                 READ(TempArgs(9), *) GLHETempVal
                 WRITE(OutArgs(4), *) GLHETempVal / 920
                 OutArgs(5)=TempArgs(10)
                 OutArgs(6)='3.2'
                 OutArgs(7)='8'
                 CurArgs=7
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ObjectName='GroundHeatExchanger:ResponseFactors'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Response Factors')
                 OutArgs(2)=TRIM(TempArgs(1)) // TRIM(' Properties')
                 OutArgs(3)=TempArgs(5)
                 OutArgs(4)=TempArgs(17)
                 CurArgs=4
                 I = 0
                 DO WHILE (.TRUE.)
                   I = I + 1
                   CurField = 2*(I-1) + 19
                   IF (CurField > TempArgsNum) EXIT
                   OutArgs(CurArgs + 1)=TempArgs(CurField)
                   OutArgs(CurArgs + 2)=TempArgs(CurField + 1)
                   CurArgs=CurArgs + 2
                 END DO

                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! already written
                 Written = .true.

               CASE('BRANCH')
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                 nodiff=.true.
                 ! replace GLHE object type name
                 ! object types are on fields: 3, 7, 11, 15, ...
                 I = 0
                 DO WHILE (.TRUE.)
                   I = I + 1
                   CurField = 4*(I-1) + 3
                   IF ( CurField > CurArgs ) EXIT
                   IF ( SameString( InArgs(CurField), "GroundHeatExchanger:Vertical" ) ) THEN
                     OutArgs(CurField) = "GroundHeatExchanger:System"
                   END IF
                 END DO

               CASE('CONDENSEREQUIPMENTLIST')
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                 nodiff=.true.
                 ! replace GLHE object type name
                 ! object types are on fields: 2, 4, 6, 8, ...
                 I = 0
                 DO WHILE (.TRUE.)
                   I = I + 1
                   CurField = 2*(I-1) + 2
                   IF ( CurField > CurArgs ) EXIT
                   IF ( SameString( InArgs(CurField), "GroundHeatExchanger:Vertical" ) ) THEN
                     OutArgs(CurField) = "GroundHeatExchanger:System"
                   END IF
                 END DO

             CASE('SCHEDULE:DAY:INTERVAL')
                 ObjectName='Schedule:Day:Interval'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 nodiff=.false.
                 OutArgs=InArgs
                 IF (SameString(InArgs(3), 'YES')) THEN
                   OutArgs(3) = 'Average'
                 ENDIF

             CASE('SCHEDULE:DAY:LIST')
                 ObjectName='Schedule:Day:List'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 nodiff=.false.
                 OutArgs=InArgs
                 IF (SameString(InArgs(3), 'YES')) THEN
                   OutArgs(3) = 'Average'
                 ENDIF

             CASE('SCHEDULE:COMPACT')
                 ObjectName='Schedule:Compact'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 nodiff=.false.
                 OutArgs=InArgs
                 DO Arg=3,CurArgs
                    UpperInArg = MakeUpperCase(InArgs(Arg))
                    IF ( ( INDEX(UpperInArg,"INTERPOLATE") .GT. 0 ).AND. (INDEX(UpperInArg,"YES") .GT. 0 ) ) THEN
                      OutArgs(Arg) = "Interpolate:Average"
                    ENDIF
                 ENDDO

    ! changes for this new DX coil
             CASE('AIRLOOPHVAC:UNITARYSYSTEM')
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                 nodiff=.true.
                 ! replace cooling coil object type name
                 CoolingCoilType = InArgs(15)
                 IF ( SameString( CoolingCoilType(1:15), "Coil:Cooling:DX" ) ) THEN
                     OutArgs(15) = "Coil:Cooling:DX"
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
                 OutArgs(11)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Operating Mode 1 Name
                 CurArgs=11
                 CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                 ! write the Operating Mode object
                 OutArgs=Blank
                 ObjectName='Coil:Cooling:DX:CurveFit:OperatingMode'
                 CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                 OutArgs(1)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Name
                 OutArgs(2)=TempArgs(3) ! Rated Gross Total Cooling Capacity
                 OutArgs(3)=TempArgs(6) ! Rated Evaporator Air Flow Rate
                 OutArgs(4)=TempArgs(22) ! Rated Condenser Air Flow Rate
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
                 OutArgs(11)=TRIM(TempArgs(1)) // TRIM(' Operating Mode') ! Operating Mode 1 Name
                 CurArgs=11
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
