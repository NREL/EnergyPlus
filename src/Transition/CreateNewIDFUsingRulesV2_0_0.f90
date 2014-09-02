!SUBROUTINE CreateNewIDFUsingRulesV2_0_0(EndOfFile,DiffOnly,InLfn,AskForInput,InputFileName,ArgFile,ArgIDFExtension)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2002
          !       MODIFIED       For each release
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates new IDFs based on the rules specified by
          ! developers.  This will result in a more complete transition but
          ! takes more time to create.   This routine is specifically for rules
          ! 1.4 to 2.0.

          ! METHODOLOGY EMPLOYED:
          ! Note that some rules may be applied here that would not necessarily run in the
          ! version being transitioned to.  One assumes that the final transition file version will be
          ! the current or current-1 version.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE DataVCompareGlobals
  USE VCompareGlobalRoutines
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
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER IoS
  INTEGER DotPos
  INTEGER NA
  INTEGER NN
  INTEGER CurArgs
  INTEGER DifLfn
  INTEGER xCount
  INTEGER Num
  INTEGER, EXTERNAL :: GetNewUnitNumber
  INTEGER, EXTERNAL :: FindNumber
  INTEGER Arg
  CHARACTER(len=30) UnitsArg
  CHARACTER(len=MaxNameLength) ObjectName
  CHARACTER(len=30), EXTERNAL :: TrimTrailZeros
  CHARACTER(len=MaxNameLength) UCRepVarName
  CHARACTER(len=MaxNameLength) UCCompRepVarName
  LOGICAL DelThis
  INTEGER pos
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
  LOGICAL :: FileExist
  LOGICAL :: ErrFlag

  LOGICAL :: COMISSim=.false.
  LOGICAL :: ADSSim=.false.
  LOGICAL :: allzeroes=.false.
  CHARACTER(len=MaxNameLength) NewName
  CHARACTER(len=MaxNameLength) :: CreatedOutputName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: DeleteThisRecord

  StillWorking=.true.
  ArgFileBeingDone=.false.
  LatestVersion=.false.
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

          ALLOCATE(Alphas(MaxAlphaArgsFound),Numbers(MaxNumericArgsFound))
          ALLOCATE(InArgs(MaxTotalArgs))
          ALLOCATE(AorN(MaxTotalArgs),ReqFld(MaxTotalArgs),FldNames(MaxTotalArgs),FldDefaults(MaxTotalArgs),FldUnits(MaxTotalArgs))
          ALLOCATE(NwAorN(MaxTotalArgs),NwReqFld(MaxTotalArgs),NwFldNames(MaxTotalArgs),NwFldDefaults(MaxTotalArgs),NwFldUnits(MaxTotalArgs))
          ALLOCATE(OutArgs(MaxTotalArgs))
          ALLOCATE(MatchArg(MaxTotalArgs))
          ALLOCATE(DeleteThisRecord(NumIDFRecords))
          DeleteThisRecord=.false.

          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COMIS SIMULATION') COMISSim=.true.
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'ADS SIMULATION') ADSSim=.true.
          ENDDO
          IF (COMISSim .and. ADSSim) THEN
            WRITE(*,*) 'File contains both COMIS and ADS Simulation objects='//TRIM(FullFileName)
            WRITE(*,*) 'Please contact EnergyPlus Support (energyplus-support@gard.com) for help in transitioning this file.'
            WRITE(Auditf,fmta) ' ..File contains both COMIS and ADS Simulation objects='//TRIM(FullFileName)
            WRITE(Auditf,fmta) ' ..Please contact EnergyPlus Support (energyplus-support@gard.com) for help in transitioning this file.'
            ExitBecauseBadFile=.true.
            EXIT
          ENDIF

          NoVersion=.true.
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'VERSION') CYCLE
            NoVersion=.false.
            EXIT
          ENDDO

          DO Num=1,NumIDFRecords

            DO xcount=IDFRecords(Num)%CommtS+1,IDFRecords(Num)%CommtE
              WRITE(DifLfn,fmta) TRIM(Comments(xcount))
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ' '
            ENDDO

            IF (NoVersion .and. Num == 1) THEN
              CALL GetNewObjectDefInIDD('VERSION',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
              OutArgs(1)='2.0'
              CurArgs=1
              CALL WriteOutIDFLinesAsComments(DifLfn,'VERSION',CurArgs,OutArgs,NwFldNames,NwFldUnits)
            ENDIF

     ! deleted objects.  no transition.
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'SKY RADIANCE DISTRIBUTION') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'AIRFLOW MODEL') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'GENERATOR:FC:BATTERY DATA') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'WATER HEATER:SIMPLE') THEN
              WRITE(DifLfn,fmta) ! The WATER HEATER:SIMPLE object has been deleted'
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
                  IF (InArgs(1)(1:3) == '2.0' .and. ArgFile) THEN
                    CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                    CLOSE(diflfn,STATUS='DELETE')
                    LatestVersion=.true.
                    EXIT
                  ENDIF
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)='2.0'
                  nodiff=.false.

      !!!    Changes for this version

                CASE('FURNACE:BLOWTHRU:HEATONLY', 'UNITARYSYSTEM:BLOWTHRU:HEATONLY')
                  IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'FURNACE:BLOWTHRU:HEATONLY') THEN
                    NewName='FURNACE:HEATONLY'
                    CALL GetNewObjectDefInIDD(NewName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  ELSE
                    NewName='UNITARYSYSTEM:HEATONLY'
                    CALL GetNewObjectDefInIDD(NewName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  ENDIF
                  OutArgs(1:4)=InArgs(1:4)
                  IF (MakeUPPERCase(InArgs(5)) == 'CYCFANCYCCOIL') THEN
                    OutArgs(5)=TRIM(NewName)//' '//TRIM(InArgs(1))//' Cycling Schedule'
                    allzeroes=.true.
                  ELSEIF (MakeUPPERCase(InArgs(5)) == 'CONTFANCYCCOIL') THEN
                    OutArgs(5)=TRIM(NewName)//' '//TRIM(InArgs(1))//' Continuous Schedule'
                    allzeroes=.false.
                  ELSE
                    OutArgs(5)='Invalid option '//TRIM(InArgs(5))
                    allzeroes=.true.
                  ENDIF
                  OutArgs(6:11)=InArgs(7:12)
                  OutArgs(12)='blow through'
                  OutArgs(13:14)=InArgs(14:15)
                  CurArgs=14
                  !  Need to write this out plus one more object....
                  CALL WriteOutIDFLines(DifLfn,NewName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  nodiff=.false.

                  IF (OutArgs(5)(1:7) == 'Invalid') CYCLE

                  ! Add ScheduleType
                  CALL GetNewObjectDefInIDD('SCHEDULETYPE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=TRIM(OutArgs(5))//' Type'
                  CurArgs=1
                  CALL WriteOutIDFLines(DifLfn,'SCHEDULETYPE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  ! Add Compact Schedule
                  CALL GetNewObjectDefInIDD('SCHEDULE:COMPACT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(5)
                  OutArgs(2)=TRIM(OutArgs(5))//' Type'
                  OutArgs(3)='Through: 12/31'
                  OutArgs(4)='For: AllDays'
                  OutArgs(5)='Until: 24:00'
                  if (allzeroes) then
                    OutArgs(6)='0'
                  else
                    OutArgs(6)='1'
                  endif
                  CurArgs=6
                  CALL WriteOutIDFLines(DifLfn,'SCHEDULE:COMPACT',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  Written=.true.
                  !CYCLE


                CASE('FURNACE:BLOWTHRU:HEATCOOL', 'UNITARYSYSTEM:BLOWTHRU:HEATCOOL')
                  IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'FURNACE:BLOWTHRU:HEATCOOL') THEN
                    NewName='FURNACE:HEATCOOL'
                    CALL GetNewObjectDefInIDD(NewName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  ELSE
                    NewName='UNITARYSYSTEM:HEATCOOL'
                    CALL GetNewObjectDefInIDD(NewName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  ENDIF
                  OutArgs(1:4)=InArgs(1:4)
                  IF (MakeUPPERCase(InArgs(5)) == 'CYCFANCYCCOIL') THEN
                    OutArgs(5)=TRIM(NewName)//' '//TRIM(InArgs(1))//' Cycling Schedule'
                    allzeroes=.true.
                  ELSEIF (MakeUPPERCase(InArgs(5)) == 'CONTFANCYCCOIL') THEN
                    OutArgs(5)=TRIM(NewName)//' '//TRIM(InArgs(1))//' Continuous Schedule'
                    allzeroes=.false.
                  ELSE
                    OutArgs(5)='Invalid option '//TRIM(InArgs(5))
                    allzeroes=.true.
                  ENDIF
                  OutArgs(6:7)=InArgs(7:8)
                  OutArgs(8)=InArgs(8)
                  if (allzeroes) then
                    OutArgs(9)='0'
                  else
                    OutArgs(9)=InArgs(8)
                  endif
                  OutArgs(10:13)=InArgs(9:12)
                  OutArgs(14)='blow through'
                  OutArgs(15:16)=InArgs(14:15)
                  OutArgs(17:18)=InArgs(18:19)
                  if (makeUPPERcase(inargs(22)) == 'NO') THEN
                    inargs(22)='None'
                  elseif (makeUPPERcase(inargs(22)) == 'YES') THEN
                    inargs(22)='CoolReheat'
                  else
                    inargs(22)=Blank
                  endif
                  OutArgs(19:21)=InArgs(22:24)
                  CurArgs=21

                  CALL WriteOutIDFLines(DifLfn,NewName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  nodiff=.false.

                  IF (OutArgs(5)(1:7) == 'Invalid') CYCLE

                  ! Add ScheduleType
                  CALL GetNewObjectDefInIDD('SCHEDULETYPE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=TRIM(OutArgs(5))//' Type'
                  CurArgs=1
                  CALL WriteOutIDFLines(DifLfn,'SCHEDULETYPE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  ! Add Compact Schedule
                  CALL GetNewObjectDefInIDD('SCHEDULE:COMPACT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(5)
                  OutArgs(2)=TRIM(OutArgs(5))//' Type'
                  OutArgs(3)='Through: 12/31'
                  OutArgs(4)='For: AllDays'
                  OutArgs(5)='Until: 24:00'
                  if (allzeroes) then
                    OutArgs(6)='0'
                  else
                    OutArgs(6)='1'
                  endif
                  CurArgs=6
                  CALL WriteOutIDFLines(DifLfn,'SCHEDULE:COMPACT',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  CYCLE

                CASE ('BRANCH')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  nodiff=.true.
                  DO Arg=3,CurArgs,5
                    SELECT CASE (MakeUPPERCase(OutArgs(Arg)))
                      CASE('FURNACE:BLOWTHRU:HEATONLY')
                        OutArgs(Arg)='Furnace:HeatOnly'
                        nodiff=.false.
                      CASE('UNITARYSYSTEM:BLOWTHRU:HEATONLY')
                        OutArgs(Arg)='UnitarySystem:HeatOnly'
                        nodiff=.false.
                      CASE('FURNACE:BLOWTHRU:HEATCOOL')
                        OutArgs(Arg)='Furnace:HeatCool'
                        nodiff=.false.
                      CASE('UNITARYSYSTEM:BLOWTHRU:HEATCOOL')
                        OutArgs(Arg)='UnitarySystem:HeatCool'
                        nodiff=.false.
                      CASE('DOMESTIC HOT WATER')
                        OutArgs(Arg)='Water Use Connections'
                        nodiff=.false.
                      CASE DEFAULT
                    END SELECT
                  ENDDO

                CASE ('UNIT VENTILATOR')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:2)=InArgs(1:2)
                  nodiff=.false.
                  OutArgs(9:11)=InArgs(3:5)
                  OutArgs(15)=InArgs(6)
                  OutArgs(3:4)=InArgs(7:8)
                  OutArgs(6)=InArgs(9)
                  OutArgs(8)=InArgs(10)
                  OutArgs(12:14)=InArgs(11:13)
                  OutArgs(7)=InArgs(14)
                  OutArgs(5)=InArgs(15)
                  if (InArgs(28) /= blank) then
                    OutArgs(16)='Heating and Cooling'
                  else
                    OutArgs(16)='Heating'
                  endif
                  OutArgs(17:18)=InArgs(16:17)
                  OutArgs(19:20)=InArgs(20:21)
                  OutArgs(22)=InArgs(22)
                  OutArgs(23:24)=InArgs(26:27)
                  OutArgs(21)=InArgs(28)
                  CurArgs=24
                  CALL WriteOutIDFLines(DifLfn,'UNIT VENTILATOR',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(9)
                  CurArgs=2
                  OutArgs(2)='-1'
                  CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  Written=.true.
                  !CYCLE

                CASE ('COMPRESSOR RACK:REFRIGERATED CASE')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:6)=InArgs(1:6)
                  OutArgs(7)='Air Cooled'
                  OutArgs(8:14)=Blank
                  IF (MakeUPPERCase(OutArgs(2)) == 'OUTDOORS') THEN
                    OutArgs(15)='COMPRESSOR RACK:REFRIGERATED CASE '//TRIM(OutArgs(1))//' Condenser Node'
                  ELSE
                    OutArgs(15)=Blank
                  ENDIF
                  OutArgs(16)='General'
                  OutArgs(17:CurArgs+10)=InArgs(7:CurArgs)
                  CurArgs=CurArgs+10
                  nodiff=.false.
                  CALL WriteOutIDFLines(DifLfn,'COMPRESSOR RACK:REFRIGERATED CASE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  IF (MakeUPPERCase(OutArgs(2)) == 'OUTDOORS') THEN
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(15)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ENDIF
                  Written=.true.
                  !CYCLE

                CASE ('UNITARYSYSTEM:HEATPUMP:WATERTOAIR')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:23)=InArgs(1:23)
                  OutArgs(24)='Water to Air HP '//TRIM(InArgs(1))//' Condenser Node'
                  OutArgs(25:26)=InArgs(24:25)
                  CurArgs=26
                  CALL WriteOutIDFLines(DifLfn,'UNITARYSYSTEM:HEATPUMP:WATERTOAIR',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)='Water to Air HP '//TRIM(InArgs(1))//' Condenser Node'
                  CurArgs=2
                  OutArgs(2)='-1'
                  CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  Written=.true.
                  !CYCLE

                CASE ('UNITARYSYSTEM:HEATPUMP:AIRTOAIR')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:4)=InArgs(1:4)
                  OutArgs(5:6)=InArgs(5)
                  IF (MakeUPPERCase(InArgs(25)) == 'CYCFANCYCCOMP') THEN
                    OutArgs(7)='0'
                  ELSEIF (MakeUPPERCase(InArgs(25)) == 'CONTFANCYCCOMP') THEN
                    OutArgs(7)=InArgs(5)
                  ELSE
                    OutArgs(7)=Blank
                  ENDIF
                  OutArgs(8:13)=InArgs(6:11)
                  OutArgs(14:15)=InArgs(15:16)
                  OutArgs(16:17)=InArgs(19:20)
                  OutArgs(18:20)=InArgs(22:24)
                  IF (MakeUPPERCase(InArgs(25)) == 'CYCFANCYCCOMP') THEN
                    OutArgs(21)='UNITARYSYSTEM:HEATPUMP:AIRTOAIR '//TRIM(InArgs(1))//' Cycling Schedule'
                    allzeroes=.true.
                  ELSEIF (MakeUPPERCase(InArgs(25)) == 'CONTFANCYCCOMP') THEN
                    OutArgs(21)='UNITARYSYSTEM:HEATPUMP:AIRTOAIR '//TRIM(InArgs(1))//' Continuous Schedule'
                    allzeroes=.false.
                  ELSE
                    OutArgs(21)='Invalid option '//TRIM(InArgs(25))
                    allzeroes=.true.
                  ENDIF
                  CurArgs=21
                  CALL WriteOutIDFLines(DifLfn,'UNITARYSYSTEM:HEATPUMP:AIRTOAIR',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  nodiff=.false.

                  IF (OutArgs(21)(1:7) == 'Invalid') CYCLE

                  ! Add ScheduleType
                  CALL GetNewObjectDefInIDD('SCHEDULETYPE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=TRIM(OutArgs(21))//' Type'
                  CurArgs=1
                  CALL WriteOutIDFLines(DifLfn,'SCHEDULETYPE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  ! Add Compact Schedule
                  CALL GetNewObjectDefInIDD('SCHEDULE:COMPACT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(21)
                  OutArgs(2)=TRIM(OutArgs(21))//' Type'
                  OutArgs(3)='Through: 12/31'
                  OutArgs(4)='For: AllDays'
                  OutArgs(5)='Until: 24:00'
                  if (allzeroes) then
                    OutArgs(6)='0'
                  else
                    OutArgs(6)='1'
                  endif
                  CurArgs=6
                  CALL WriteOutIDFLines(DifLfn,'SCHEDULE:COMPACT',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  Written=.true.
                  !CYCLE

                CASE ('COIL:DX:COOLINGBYPASSFACTOREMPIRICAL')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:18)=InArgs(1:18)
                  ! min args = 14
                  IF (CurArgs > 18) THEN
                    OutArgs(19)='Cooling Bypass DX Cooling Coil '//TRIM(InArgs(1))//' Condenser Node'
                    OutArgs(20:26)=InArgs(19:25)
                    CurArgs=CurArgs+1
                  ENDIF
                  CALL WriteOutIDFLines(DifLfn,'COIL:DX:COOLINGBYPASSFACTOREMPIRICAL',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  IF (CurArgs > 18) THEN
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)='Cooling Bypass DX Cooling Coil '//TRIM(InArgs(1))//' Condenser Node'
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ENDIF
                  Written=.true.
                  !CYCLE

                CASE ('COIL:DX:MULTISPEED:COOLINGEMPIRICAL')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:20)=InArgs(1:20)
                  ! min args = 20
                  IF (CurArgs > 20) THEN
                    OutArgs(21)='Multispeed Cooling DX Cooling Coil '//TRIM(InArgs(1))//' Condenser Node'
                    OutArgs(22:30)=InArgs(21:29)
                    CurArgs=CurArgs+1
                  ENDIF
                  CALL WriteOutIDFLines(DifLfn,'COIL:DX:MULTISPEED:COOLINGEMPIRICAL',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  IF (CurArgs > 20) THEN
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)='Multispeed Cooling DX Cooling Coil '//TRIM(InArgs(1))//' Condenser Node'
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ENDIF
                  Written=.true.
                  !CYCLE

                CASE ('COILPERFORMANCE:DX:COOLINGBYPASSFACTOREMPIRICAL')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:16)=InArgs(1:16)
                  ! min args = 12
                  IF (CurArgs > 16) THEN
                    OutArgs(17)='CoilPerformance:DX '//TRIM(InArgs(1))//' Condenser Node'
                    OutArgs(22:30)=InArgs(21:29)
                    CurArgs=CurArgs+1
                  ENDIF
                  CALL WriteOutIDFLines(DifLfn,'COILPERFORMANCE:DX:COOLINGBYPASSFACTOREMPIRICAL',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                  IF (CurArgs > 16) THEN
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)='CoilPerformance:DX '//TRIM(InArgs(1))//' Condenser Node'
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ENDIF
                  Written=.true.
                  !CYCLE

                CASE ('ZONE SIZING')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:7)=InArgs(1:7)
                  OutArgs(8)='0.0'
                  OutArgs(9:12)=InArgs(8:11)
                  OutArgs(13:15)='  '
                  OutArgs(16:17)=InArgs(12:13)
                  OutArgs(18:20)='  '
                  CurArgs=20
                  nodiff=.false.

                CASE ('DOMESTIC HOT WATER')
                  ! Transition to new object name=> Water Use Equipment
                  ! Put Water connections object first..

                  IF (InArgs(2) /= Blank .or. InArgs(3) /= Blank) THEN
                    CALL GetNewObjectDefInIDD('WATER USE CONNECTIONS',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=InArgs(1)
                    OutArgs(2:3)=InArgs(2:3)
                    OutArgs(4:6)=Blank
                    OutArgs(7)=InArgs(6)
                    OutArgs(8:10)=Blank
                    OutArgs(11)=InArgs(1)
                    CurArgs=11
                    CALL WriteOutIDFLines(DifLfn,'WATER USE CONNECTIONS',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ENDIF

                  CALL GetNewObjectDefInIDD('WATER USE EQUIPMENT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2)=InArgs(7)
                  OutArgs(3)=InArgs(4)
                  OutArgs(4)=InArgs(5)
                  OutArgs(5:6)=Blank
                  OutArgs(7)=InArgs(6)
                  CurArgs=7
                  CALL WriteOutIDFLines(DifLfn,'WATER USE EQUIPMENT',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  Written=.true.
                  !CYCLE

                CASE ('CHILLER:ELECTRIC')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(2)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(2)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(7) == Blank) THEN
                      OutArgs(7)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    IF (OutArgs(8) == Blank) THEN
                      OutArgs(8)=TRIM(InArgs(1))//' CONDENSER OUTLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:ELECTRIC',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(7)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CYCLE
                  ENDIF
                  nodiff=.true.

                CASE ('CHILLER:ENGINEDRIVEN')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(2)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(2)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(7) == Blank) THEN
                      OutArgs(7)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    IF (OutArgs(8) == Blank) THEN
                      OutArgs(8)=TRIM(InArgs(1))//' CONDENSER OUTLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:ENGINEDRIVEN',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(7)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CYCLE
                  ENDIF
                  nodiff=.true.

                CASE ('CHILLER:COMBUSTION TURBINE')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(2)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(2)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(7) == Blank) THEN
                      OutArgs(7)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    IF (OutArgs(8) == Blank) THEN
                      OutArgs(8)=TRIM(InArgs(1))//' CONDENSER OUTLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:COMBUSTION TURBINE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(7)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CYCLE
                  ENDIF
                  nodiff=.true.

                CASE ('CHILLER:ELECTRIC:EIR')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(19)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(19)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(17) == Blank) THEN
                      OutArgs(17)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    IF (OutArgs(18) == Blank) THEN
                      OutArgs(18)=TRIM(InArgs(1))//' CONDENSER OUTLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:ELECTRIC:EIR',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(17)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CYCLE
                  ENDIF
                  nodiff=.true.

                CASE ('CHILLER:CONST COP')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(10)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(10)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(8) == Blank) THEN
                      OutArgs(8)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    IF (OutArgs(9) == Blank) THEN
                      OutArgs(9)=TRIM(InArgs(1))//' CONDENSER OUTLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:CONST COP',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(8)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CYCLE
                  ENDIF
                  nodiff=.true.

                CASE ('CHILLER:DIRECT FIRED ABSORPTION')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(30)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(30)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(10) == Blank) THEN
                      OutArgs(10)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:DIRECT FIRED ABSORPTION',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(10)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    Written=.true.
                    !CYCLE
                  ELSE
                    nodiff=.true.
                  ENDIF

                CASE ('CHILLER:SPARK:ECHILLER')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (MakeUPPERCase(InArgs(2)) /= 'WATER COOLED' .and. MakeUPPERCase(InArgs(2)) /= 'WATER-COOLED') THEN
                    IF (OutArgs(5) == Blank) THEN
                      OutArgs(5)=TRIM(InArgs(1))//' CONDENSER INLET NODE'
                    ENDIF
                    ! Make outside air node...
                    CALL WriteOutIDFLines(DifLfn,'CHILLER:SPARK:ECHILLER',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    CALL GetNewObjectDefInIDD('OUTSIDE AIR NODE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=OutArgs(5)
                    CurArgs=2
                    OutArgs(2)='-1'
                    CALL WriteOutIDFLines(DifLfn,'OUTSIDE AIR NODE',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    Written=.true.
                    !CYCLE
                  ELSE
                    nodiff=.true.
                  ENDIF

                CASE ('COIL:DX:COOLINGHEATEXCHANGERASSISTED')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2:5)=InArgs(4:7)
                  CurArgs=5
                  nodiff=.false.

                CASE ('COIL:WATER:COOLINGHEATEXCHANGERASSISTED')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2:5)=InArgs(4:7)
                  CurArgs=5
                  nodiff=.false.

              CASE('WINDOWSHADINGCONTROL')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('InteriorNonInsulatingShade',InArgs(2))) then
                  OutArgs(2)='InteriorShade'
                endif
                if (samestring('ExteriorNonInsulatingShade',InArgs(2))) then
                  OutArgs(2)='ExteriorShade'
                endif
                if (samestring('InteriorInsulatingShade',InArgs(2))) then
                  OutArgs(2)='InteriorShade'
                endif
                if (samestring('ExteriorInsulatingShade',InArgs(2))) then
                  OutArgs(2)='ExteriorShade'
                endif
                if (samestring('Schedule',InArgs(4))) then
                  OutArgs(4)='OnIfScheduleAllows'
                endif
                if (samestring('SolarOnWindow',InArgs(4))) then
                  OutArgs(4)='OnIfHighSolarOnWindow'
                endif
                if (samestring('HorizontalSolar',InArgs(4))) then
                  OutArgs(4)='OnIfHighHorizontalSolar'
                endif
                if (samestring('OutsideAirTemp',InArgs(4))) then
                  OutArgs(4)='OnIfHighOutsideAirTemp'
                endif
                if (samestring('ZoneAirTemp',InArgs(4))) then
                  OutArgs(4)='OnIfHighZoneAirTemp'
                endif
                if (samestring('ZoneCooling',InArgs(4))) then
                  OutArgs(4)='OnIfHighZoneCooling'
                endif
                if (samestring('Glare',InArgs(4))) then
                  OutArgs(4)='OnIfHighGlare'
                endif
                if (samestring('DaylightIlluminance',InArgs(4))) then
                  OutArgs(4)='MeetDaylightIlluminanceSetpoint'
                endif

! Old -- constant changes.

                CASE ('BUILDING')
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                  IF (CurArgs == 8) THEN
                    nodiff=.false.
                    IF (MakeUPPERCase(OutArgs(8)) == 'YES') THEN
                      OutArgs(6)=TRIM(OutArgs(6))//'WithReflections'
                      OutArgs(8)=Blank
                      CurArgs=7
                    ELSEIF (MakeUPPERCase(OutArgs(8)) == 'NO') THEN
                      OutArgs(8)=Blank
                      CurArgs=7
                    ENDIF
                  ENDIF

      !!!   Changes for report variables, meters, tables -- update names

              CASE ('REPORT VARIABLE')
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

              CASE ('REPORT METER','REPORT METERFILEONLY','REPORT CUMULATIVE METER','REPORT CUMULATIVE METERFILEONLY')
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

              CASE ('REPORT:TABLE:TIMEBINS')
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

              CASE ('REPORT:TABLE:MONTHLY')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                IF (OutArgs(1) == Blank) THEN
                  OutArgs(1)='*'
                  nodiff=.false.
                ENDIF
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
                    ELSE
                      WildMatch=.false.
                    ENDIF
                    pos=INDEX(TRIM(UCRepVarname),TRIM(UCCompRepVarName))
                    IF (pos > 0 .and. pos /= 1) CYCLE
                    IF (pos > 0) THEN
                      IF (NewRepVarName(Arg) /= '<DELETE>') THEN
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ELSE
                        DelThis=.true.
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+1)) THEN
                        ! Adding a var field.
                        CurVar=CurVar+2
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg+1)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg+1))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
                        ENDIF
                        OutArgs(CurVar+1)=InArgs(Var+1)
                        nodiff=.false.
                      ENDIF
                      IF (OldRepVarName(Arg) == OldRepVarName(Arg+2)) THEN
                        ! Adding a var field.
                        CurVar=CurVar+2
                        IF (.not. WildMatch) THEN
                          OutArgs(CurVar)=NewRepVarName(Arg+2)
                        ELSE
                          OutArgs(CurVar)=TRIM(NewRepVarName(Arg+2))//OutArgs(CurVar)(Len_Trim(UCCompRepVarName)+1:)
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
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ' '
            ENDDO
          ENDIF

          CLOSE(DifLfn)
          IF (checkrvi) THEN
            CALL ProcessRviMviFiles(FileNamePath,'rvi')
            CALL ProcessRviMviFiles(FileNamePath,'mvi')
          ENDIF
          CALL CloseOut
        ELSE  ! not a idf or imf
          CALL ProcessRviMviFiles(FileNamePath,'rvi')
          CALL ProcessRviMviFiles(FileNamePath,'mvi')
        ENDIF
      ELSE  ! Full name == Blank
        EndOfFile=.true.
      ENDIF

      CALL CreateNewName('Reallocate',CreatedOutputName,' ')

      IF (Allocated(DeleteThisRecord)) THEN
        DEALLOCATE(DeleteThisRecord)
        DEALLOCATE(Alphas)
        DEALLOCATE(Numbers)
        DEALLOCATE(InArgs)
        DEALLOCATE(AorN)
        DEALLOCATE(ReqFld)
        DEALLOCATE(FldNames)
        DEALLOCATE(FldDefaults)
        DEALLOCATE(FldUnits)
        DEALLOCATE(NwAorN)
        DEALLOCATE(NwReqFld)
        DEALLOCATE(NwFldNames)
        DEALLOCATE(NwFldDefaults)
        DEALLOCATE(NwFldUnits)
        DEALLOCATE(OutArgs)
        DEALLOCATE(MatchArg)
      ENDIF

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
    ! If this is true, then there was a "arg IDF File" on the command line and some files need to be
                         ! renamed.
    ErrFlag=.false.
    CALL copyfile(TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension),TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'old',ErrFlag)
!    SysResult=SystemQQ('copy "'//TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'" "'//  &
!                                    TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'old"')
    CALL copyfile(TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'new',TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension),ErrFlag)
!    SysResult=SystemQQ('copy "'//TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'new" "'//  &
!                                  TRIM(FileNamePath)//'.'//TRIM(ArgIDFExtension)//'"')
    INQUIRE(File=TRIM(FileNamePath)//'.rvi',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.rvi',TRIM(FileNamePath)//'.rviold',ErrFlag)
!      SysResult=SystemQQ('copy "'//TRIM(FileNamePath)//'.rvi" "'//TRIM(FileNamePath)//'.rviold"')
    ENDIF
    INQUIRE(File=TRIM(FileNamePath)//'.rvinew',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.rvinew',TRIM(FileNamePath)//'.rvi',ErrFlag)
!      SysResult=SystemQQ('copy "'//TRIM(FileNamePath)//'.rvinew" "'//TRIM(FileNamePath)//'.rvi"')
    ENDIF
    INQUIRE(File=TRIM(FileNamePath)//'.mvi',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.mvi',TRIM(FileNamePath)//'.mviold',ErrFlag)
!      SysResult=SystemQQ('copy "'//TRIM(FileNamePath)//'.mvi" "'//TRIM(FileNamePath)//'.mviold"')
    ENDIF
    INQUIRE(File=TRIM(FileNamePath)//'.mvinew',EXIST=FileExist)
    IF (FileExist) THEN
      CALL copyfile(TRIM(FileNamePath)//'.mvinew',TRIM(FileNamePath)//'.mvi',ErrFlag)
!      SysResult=SystemQQ('copy "'//TRIM(FileNamePath)//'.mvinew" "'//TRIM(FileNamePath)//'.mvi"')
    ENDIF
  ENDIF

  RETURN

!END SUBROUTINE CreateNewIDFUsingRulesV2_0_0
