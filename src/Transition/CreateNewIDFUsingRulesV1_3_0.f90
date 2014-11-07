!SUBROUTINE CreateNewIDFUsingRules_V1_3_0(EndOfFile,DiffOnly,InLfn,AskForInput,InputFileName,ArgFile,ArgIDFExtension)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2002
          !       MODIFIED       For each release
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates new IDFs based on the rules specified by
          ! developers.  This will result in a more complete transition but
          ! takes more time to create.   This routine is specifically for rules
          ! 1.2.3 to 1.3.

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
  CHARACTER(len=MaxNameLength) :: CreatedOutputName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: DeleteThisRecord


  LOGICAL :: COMISSim=.false.
  LOGICAL :: ADSSim=.false.
  LOGICAL :: ELAElement=.false.
  LOGICAL :: PLRElement=.false.
  LOGICAL :: DelElement=.false.
  INTEGER :: yNum
  LOGICAL :: TZExt1=.false.
  LOGICAL :: TZExt2=.false.
  INTEGER :: Node1
  INTEGER :: Node2
  INTEGER :: xNum
  LOGICAL :: ErrFlag=.false.

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
              OutArgs(1)='1.3'
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
                IF (InArgs(1)(1:3) == '1.3' .and. ArgFile) THEN
                  CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                  CLOSE(diflfn,STATUS='DELETE')
                  LatestVersion=.true.
                  EXIT
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='1.3'
                nodiff=.false.

    !!!    Changes for this version
              CASE('COIL:WATER:SIMPLECOOLING')
                CALL GetNewObjectDefInIDD('COIL:WATER:COOLING',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                ObjectName='COIL:Water:Cooling'
                OutArgs(1:2)=InArgs(1:2)
                OutArgs(3:9)='autosize'
                OutArgs(10:13)=InArgs(6:9)
                OutArgs(14)='SimpleAnalysis'
                OutArgs(15)='CrossFlow'
                CurArgs=15
                nodiff=.false.

              CASE('UNITARYSYSTEM:HEATPUMP:WATERTOAIR')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:11)=InArgs(1:11)
                OutArgs(10)='Coil:WaterToAirHP:ParameterEstimation:Heating'
                OutArgs(12)='0.001'
                OutArgs(13:14)=InArgs(11:12)
                OutArgs(13)='Coil:WaterToAirHP:ParameterEstimation:Cooling'
                OutArgs(15)='0.001'
                OutArgs(16:21)=InArgs(14:19)
                ! delete field 20 -- supplemental heating capacity
                OutArgs(22:25)=InArgs(21:24)
                CurArgs=25
                nodiff=.false.

              CASE('COIL:WATERTOAIRHP:HEATING')
                CALL GetNewObjectDefInIDD('COIL:WATERTOAIRHP:PARAMETERESTIMATION:HEATING',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.false.

              CASE('COIL:WATERTOAIRHP:COOLING')
                CALL GetNewObjectDefInIDD('COIL:WATERTOAIRHP:PARAMETERESTIMATION:COOLING',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.false.

              CASE('LIGHTS')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                IF (MakeUPPERCase(OutArgs(9)) == 'GENERALLIGHTS' .or. OutArgs(9) == Blank) THEN
                  DO xNum=1,NumIDFRecords
                    IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'DAYLIGHTING:DETAILED') THEN
                      IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(OutArgs(2)))) THEN
                        OutArgs(8)='1.0'
                      ENDIF
                    ENDIF
                    IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'DAYLIGHTING:DELIGHT') THEN
                      IF (SameString(TRIM(IDFRecords(xNum)%Alphas(2)),TRIM(OutArgs(2)))) THEN
                        OutArgs(8)='1.0'
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
                nodiff=.false.

              CASE('ELECTRIC EQUIPMENT')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                IF (CurArgs >= 8) THEN
                  IF (OutArgs(8) /= Blank) THEN
                    READ(OutArgs(8),*) xNum
                    IF (xNum == 0) THEN
                      OutArgs(8)='General'
                    ELSE
                      WRITE(OutArgs(8),"('Category ',I2.2)") xNum
                    ENDIF
                  ENDIF
                ENDIF
                nodiff=.false.

              CASE('BRANCH')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                DO Arg=3,CurArgs,5
                  SELECT CASE (MakeUPPERCase(OutArgs(Arg)))
                    CASE('COIL:WATERTOAIRHP:HEATING')
                      OutArgs(Arg)='Coil:WaterToAirHP:ParameterEstimation:Heating'
                      nodiff=.false.
                    CASE('COIL:WATERTOAIRHP:COOLING')
                      OutArgs(Arg)='Coil:WaterToAirHP:ParameterEstimation:Cooling'
                      nodiff=.false.
                    CASE('COIL:WATER:SIMPLECOOLING')
                      OutArgs(Arg)='COIL:Water:Cooling'
                      nodiff=.false.
                    CASE DEFAULT
                  END SELECT
                ENDDO

   !!! Changes for COMIS objects

              CASE ('COMIS SIMULATION')   ! only if no ADS simulation?
                ObjectName='AIRFLOWNETWORK SIMULATION'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='AirflowNetwork 1'
                IF (MakeUPPERCase(InArgs(1)) == 'VENT') THEN
                  OutArgs(2)='MultiZone Without Distribution'
                ELSE
                  OutArgs(2)='No MultiZone or Distribution'
                ENDIF
                OutArgs(3)=InArgs(16)
                IF (OutArgs(3) == ' ') OutArgs(3)='Input'
                OutArgs(4)=InArgs(15)
                OutArgs(5)=InArgs(17)
                OutArgs(6)=InArgs(12)
                OutArgs(7)=InArgs(11)
                OutArgs(8)=InArgs(6)
                OutArgs(9)=InArgs(5)
                OutArgs(10)=InArgs(4)
                IF (ProcessNumber(InArgs(4),ErrFlag) == 1.0) OutArgs(10)='-0.5'
                OutArgs(11:12)=InArgs(13:14)
                OutArgs(13:14)=InArgs(18:19)
                IF (OutArgs(13) == ' ') OutArgs(13)='0.0'
                IF (OutArgs(14) == ' ') OutArgs(14)='1.0'
                nodiff=.false.
                CurArgs=14

              CASE ('COMIS ZONE DATA')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:ZONE'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                OutArgs(3)=InArgs(2)
                OutArgs(2)=InArgs(3)
                OutArgs(4:9)=InArgs(4:9)
                CurArgs=9
                IF (OutArgs(9) == ' ') CurArgs=8
                nodiff=.false.

              CASE ('COMIS SURFACE DATA')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                IF (CurArgs > 4) THEN
                  OutArgs(5)=InArgs(6)
                  OutArgs(6)=InArgs(5)
                  OutArgs(7)=InArgs(7)
                  IF (OutArgs(7) == ' ') OutArgs(7)='1.0'
                  DO Arg=8,CurArgs
                    OutArgs(Arg)=InArgs(Arg)
                  ENDDO
                ENDIF
                nodiff=.false.

              CASE ('COMIS SITE WIND CONDITIONS')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:SITE WIND CONDITIONS'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                OutArgs(2)=InArgs(3)
                nodiff=.false.
                CurArgs=2

              CASE ('COMIS EXTERNAL NODE')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:EXTERNAL NODE'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                nodiff=.false.
                CurArgs=1

              CASE ('COMIS STANDARD CONDITIONS FOR CRACK DATA')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:REFERENCE CRACK CONDITIONS'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='ReferenceCrackConditions'
                OutArgs(2)=InArgs(1)
                OutArgs(3)=RoundSigDigits(ProcessNumber(InArgs(2),ErrFlag)*1000.,0)
                OutArgs(4)=RoundSigDigits(ProcessNumber(InArgs(3),ErrFlag)/1000.,3)
                nodiff=.false.
                CurArgs=4

              CASE ('COMIS AIR FLOW:CRACK')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE CRACK DATA'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                OutArgs(4)='ReferenceCrackConditions'
                nodiff=.false.
                CurArgs=4

              CASE ('COMIS AIR FLOW:OPENING')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:COMPONENT DETAILED OPENING'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                CurArgs=MIN(26,CurArgs)
                nodiff=.false.

              CASE ('COMIS CP ARRAY')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:WIND PRESSURE COEFFICIENT ARRAY'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.false.

              CASE ('COMIS CP VALUES')
                ObjectName='AIRFLOWNETWORK:MULTIZONE:WIND PRESSURE COEFFICIENT VALUES'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.false.

   !!! Changes for ADS objects
              CASE ('ADS SIMULATION')
                ObjectName='AIRFLOWNETWORK SIMULATION'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                IF (MakeUPPERCase(InArgs(2)) == 'ADS') THEN
                  OutArgs(2)='MultiZone with Distribution only during Fan Operation'
                ELSE
                  OutArgs(2)='No MultiZone or Distribution'
                ENDIF
                OutArgs(3)='Surface-Average Calculation'
                OutArgs(4)=' '
                OutArgs(5)='LowRise'
                OutArgs(6:9)=InArgs(3:6)
                OutArgs(10)=InArgs(8)
                OutArgs(11)='10'
                OutArgs(12)='0.14'
                OutArgs(13)='0.0'
                OutArgs(14)='1.0'
                nodiff=.false.
                CurArgs=14
                !  Need to write this out plus one more object....
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ObjectName='AIRFLOWNETWORK:MULTIZONE:REFERENCE CRACK CONDITIONS'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='ReferenceCrackConditions'
                OutArgs(2)='20.0'
                OutArgs(3)='101325'
                OutArgs(4)='0.0'
                nodiff=.false.
                CurArgs=4
                !  Need to write this out plus one more object....
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ObjectName='AIRFLOWNETWORK:MULTIZONE:SITE WIND CONDITIONS'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='0.0'
                OutArgs(2)='0.18'
                nodiff=.false.
                CurArgs=2

              CASE ('ADS NODE DATA')
                IF (MakeUPPERCase(InArgs(2)) == 'THERMAL ZONE') THEN
                  ObjectName='AIRFLOWNETWORK:MULTIZONE:ZONE'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(6)
                  OutArgs(2)='NoVent'
                  OutArgs(3)=' '
                  OutArgs(4)='1.0'
                  OutArgs(5)='0.0'
                  OutArgs(6)='100.0'
                  OutArgs(7)='0.0'
                  OutArgs(8)='300000.0'
                  nodiff=.false.
                  CurArgs=8
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'EXTERNAL' .and. MakeUPPERCase(InArgs(3)) == 'OTHER') THEN
                  CYCLE
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'OTHER' .and. (MakeUPPERCase(InArgs(3)) == 'MIXER'   .or.  &
                              MakeUPPERCase(InArgs(3)) == 'SPLITTER' .or. MakeUPPERCase(InArgs(3)) == 'OUTSIDE AIR SYSTEM')) THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:NODE'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2)=' '
                  OutArgs(3)=InArgs(3)
                  OutArgs(4)=InArgs(5)
                  nodiff=.false.
                  CurArgs=4
                ELSE
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:NODE'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(3)=InArgs(2)
                  IF (MakeUPPERCase(OutArgs(3)) == 'EXTERNAL') OutArgs(3)='OA Mixer Outside Air Stream Node'
                  OutArgs(2)=InArgs(3)
                  OutArgs(4)=InArgs(5)
                  nodiff=.false.
                  CurArgs=4
                ENDIF

              CASE ('ADS ELEMENT DATA')
                IF (MakeUPPERCase(InArgs(2)) == 'DWC' .and. (MakeUPPERCase(InArgs(4)) == 'OTHER'  &
                       .or. MakeUPPERCase(InArgs(4)) == 'SUPPLY CONNECTION'                       &
                       .or. MakeUPPERCase(InArgs(4)) == 'RETURN CONNECTION') ) THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DUCT'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  IF (NumNumbers <= 9) THEN
                    OutArgs(1)=InArgs(1)
                    OutArgs(2:8)=InArgs(5:11)
                    CurArgs=8
                    nodiff=.false.
                  ELSE
                    OutArgs(1)=InArgs(1)
                    OutArgs(2:6)=InArgs(5:9)
                    OutArgs(7:8)=InArgs(13:14)
                    CurArgs=8
                    nodiff=.false.
                  ENDIF
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'DWC' .and. MakeUPPERCase(InArgs(4)) == 'SINGLE DUCT:CONST VOLUME:REHEAT') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT TERMINAL UNIT'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:2)=InArgs(3:4)
                  OutArgs(3:4)=InArgs(5:6)
                  CurArgs=4
                  nodiff=.false.
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'CVF') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT CONSTANT VOLUME FAN'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(3)
                  CurArgs=1
                  nodiff=.false.
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'DWC' .and. MakeUPPERCase(InArgs(4)) == 'COIL:DX:COOLINGBYPASSFACTOREMPIRICAL') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT COIL'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:4)=InArgs(3:6)
                  CurArgs=4
                  nodiff=.false.
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'DWC' .and. MakeUPPERCase(InArgs(4)) == 'COIL:GAS:HEATING') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT COIL'
                  OutArgs(1:4)=InArgs(3:6)
                  CurArgs=4
                  nodiff=.false.
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'DWC' .and. MakeUPPERCase(InArgs(4)) == 'COIL:ELECTRIC:HEATING') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT COIL'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1:4)=InArgs(3:6)
                  CurArgs=4
                  nodiff=.false.
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'PLR') THEN
                  !  Find Linkage Data with Element=this one
                  TZExt1=.false.
                  TZExt2=.false.
                  Node1=0
                  Node2=0
                  DO xNum=1,NumIDFRecords
                    IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS LINKAGE DATA') THEN
                      IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == MakeUPPERCase(InArgs(1))) THEN
                        DO yNum=1,NumIDFRecords
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Name)) /= 'ADS NODE DATA') CYCLE
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.   &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL')  THEN
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(1))) == MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) ) THEN
                              !  Found first node
                              IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE') TZExt1=.true.
                              Node1=yNum
                            ENDIF
                          ENDIF
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.   &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL')  THEN
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(1))) == MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(3))) ) THEN
                            !  Found second node
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE') TZExt2=.true.
                            Node2=yNum
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDIF
                  ENDDO
                  IF (Node1 == 0 .or. Node2 == 0) THEN
                    ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT LEAK'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    IF (NumNumbers <=2) THEN
                      OutArgs(1)=InArgs(1)
                      IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                      OutArgs(2)=InArgs(5)
                      OutArgs(3)=InArgs(6)
                      CurArgs=3
                      nodiff=.false.
                    ELSE
                      OutArgs(1)=InArgs(1)
                      IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                      OutArgs(2)=InArgs(5)
                      OutArgs(3)=InArgs(8)
                      CurArgs=3
                      nodiff=.false.
                    ENDIF
                  ELSEIF (TZExt1 .and. TZExt2) THEN
                    ! Link between two zones.
                    ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE CRACK DATA'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    IF (NumNumbers <=2) THEN
                      OutArgs(1)=InArgs(1)
                      IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                      OutArgs(2:3)=InArgs(5:6)
                      OutArgs(4)='ReferenceCrackConditions'
                      CurArgs=4
                    ELSE
                      OutArgs(1)=InArgs(1)
                      IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                      OutArgs(2)=InArgs(5)
                      OutArgs(3)=InArgs(8)
                      OutArgs(4)='ReferenceCrackConditions'
                      CurArgs=4
                    ENDIF
                    !  Need to write this out plus one more object....
                    CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)='Surface in Zone='//TRIM(IDFRecords(Node1)%Alphas(4))//' - InterZonal Surface'
                    OutArgs(2)=InArgs(1)
                    OutArgs(3)=' '
                    OutArgs(4)='1.0'
!                    OutArgs(5)='1.0'
                    CurArgs=4
                    nodiff=.false.
                  ELSE
                    ! Link between zone and external/outside
                    ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE CRACK DATA'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    IF (NumNumbers <=2) THEN
                      OutArgs(1)=InArgs(1)
                      IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                      OutArgs(2:3)=InArgs(5:6)
                      OutArgs(4)='ReferenceCrackConditions'
                      CurArgs=4
                    ELSE
                      OutArgs(1)=InArgs(1)
                      IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                      OutArgs(2)=InArgs(5)
                      OutArgs(3)=InArgs(8)
                      OutArgs(4)='ReferenceCrackConditions'
                      CurArgs=4
                    ENDIF
                    !  Need to write this out plus one more object....
                    CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    IF (TZExt1) THEN
                      OutArgs(1)='Surface in Zone='//TRIM(IDFRecords(Node1)%Alphas(4))
                    ELSE
                      OutArgs(1)='Surface in Zone='//TRIM(IDFRecords(Node2)%Alphas(4))
                    ENDIF
                    IF (.not. TZExt1) THEN
                      OutArgs(1)=TRIM(OutArgs(1))//' - facing='//TRIM(IDFRecords(Node1)%Numbers(1))
                    ELSE
                      OutArgs(1)=TRIM(OutArgs(1))//' - facing='//TRIM(IDFRecords(Node2)%Numbers(1))
                    ENDIF
                    OutArgs(2)=InArgs(1)
                    OutArgs(3)=' '
                    OutArgs(4)='1.0'
!                    OutArgs(5)='1.0'
                    CurArgs=4
                    nodiff=.false.
                  ENDIF
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'FAN') THEN
                  CYCLE
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'ELA') THEN
                  ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE EFFECTIVE LEAKAGE AREA'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2:5)=InArgs(5:8)
                  CurArgs=5
                  !  Find Linkage Data with Element=this one
                  Node1=0
                  Node2=0
                  DO xNum=1,NumIDFRecords
                    IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS LINKAGE DATA') THEN
                      IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == MakeUPPERCase(InArgs(1))) THEN
                        DO yNum=1,NumIDFRecords
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Name)) /= 'ADS NODE DATA') CYCLE
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.   &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL')  THEN
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(1))) == MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) ) THEN
                              !  Found first node
                              Node1=yNum
                            ENDIF
                          ENDIF
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.   &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL')  THEN
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(1))) == MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(3))) ) THEN
                            !  Found second node
                            Node2=yNum
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDIF
                  ENDDO
                  IF (Node1 == 0 .or. Node2 == 0) THEN
                    WRITE(DifLfn,fmta) ' ! Cannot match ADS Element='//TRIM(InArgs(1))//' to proper ADS Linkage/Nodes'
                    WRITE(DifLfn,fmta) ' ! Will have to complete manually'
                  ENDIF
                  !  Need to write this out plus one more object....
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  IF (Node1 == 0 .or. Node2 == 0) CYCLE

                  ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  IF (MakeUPPERCase(IDFRecords(Node1)%Alphas(2)) == 'THERMAL ZONE') THEN
                    OutArgs(1)='Surface in Zone='//TRIM(IDFRecords(Node1)%Alphas(4))
                  ELSEIF (MakeUPPERCase(IDFRecords(Node2)%Alphas(2)) == 'THERMAL ZONE') THEN
                    OutArgs(1)='Surface in Zone='//TRIM(IDFRecords(Node2)%Alphas(4))
                  ENDIF
                  IF (MakeUPPERCase(IDFRecords(Node1)%Alphas(2)) == 'EXTERNAL') THEN
                    OutArgs(1)=TRIM(OutArgs(1))//' - facing='//TRIM(IDFRecords(Node1)%Numbers(1))
                  ELSEIF (MakeUPPERCase(IDFRecords(Node2)%Alphas(2)) == 'EXTERNAL') THEN
                    OutArgs(1)=TRIM(OutArgs(1))//' - facing='//TRIM(IDFRecords(Node2)%Numbers(1))
                  ENDIF
                  IF (MakeUPPERCase(IDFRecords(Node1)%Alphas(2)) == 'THERMAL ZONE' .and. MakeUPPERCase(IDFRecords(Node2)%Alphas(2)) == 'THERMAL ZONE') THEN
                    OutArgs(1)=TRIM(OutArgs(1))//' - InterZonal Surface'
                  ENDIF
                  OutArgs(2)=InArgs(1)
                  OutArgs(3)=' '
                  OutArgs(4)='1.0'
!                  OutArgs(5)='1.0'
                  CurArgs=4
                  nodiff=.false.

                ELSEIF (MakeUPPERCase(InArgs(2)) == 'DOR') THEN
                  ObjectName='AIRFLOWNETWORK:MULTIZONE:COMPONENT SIMPLE OPENING'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  IF (ProcessNumber(InArgs(5),Errflag) == 0.0) InArgs(5)='0.0001'
                  OutArgs(2)=InArgs(5)
                  OutArgs(3:4)=InArgs(8:9)
                  OutArgs(5)=InArgs(12)
                  nodiff=.false.
                  CurArgs=5
                  !  Find Linkage Data with Element=this one
                  Node1=0
                  Node2=0
                  DO xNum=1,NumIDFRecords
                    IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS LINKAGE DATA') THEN
                      IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == MakeUPPERCase(InArgs(1))) THEN
                        DO yNum=1,NumIDFRecords
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Name)) /= 'ADS NODE DATA') CYCLE
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.   &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL')  THEN
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(1))) == MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) ) THEN
                              !  Found first node
                              Node1=yNum
                            ENDIF
                          ENDIF
                          IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.   &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL')  THEN
                            IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(1))) == MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(3))) ) THEN
                            !  Found second node
                            Node2=yNum
                            ENDIF
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDIF
                  ENDDO
                  IF (Node1 == 0 .or. Node2 == 0) THEN
                    WRITE(DifLfn,fmta) ' ! Cannot match ADS Element='//TRIM(InArgs(1))//' to proper ADS Linkage/Nodes'
                    WRITE(DifLfn,fmta) ' ! Will have to complete manually'
                  ENDIF
                  !  Need to write this out plus one more object....
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  IF (Node1 == 0 .or. Node2 == 0) CYCLE

                  ObjectName='AIRFLOWNETWORK:MULTIZONE:SURFACE'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  IF (MakeUPPERCase(IDFRecords(Node1)%Alphas(2)) == 'THERMAL ZONE') THEN
                    OutArgs(1)='SubSurface in Zone='//TRIM(IDFRecords(Node1)%Alphas(4))
                  ELSEIF (MakeUPPERCase(IDFRecords(Node2)%Alphas(2)) == 'THERMAL ZONE') THEN
                    OutArgs(1)='SubSurface in Zone='//TRIM(IDFRecords(Node2)%Alphas(4))
                  ENDIF
                  IF (MakeUPPERCase(IDFRecords(Node1)%Alphas(2)) == 'EXTERNAL') THEN
                    OutArgs(1)=TRIM(OutArgs(1))//' - facing='//TRIM(IDFRecords(Node1)%Numbers(1))
                  ELSEIF (MakeUPPERCase(IDFRecords(Node2)%Alphas(2)) == 'EXTERNAL') THEN
                    OutArgs(1)=TRIM(OutArgs(1))//' - facing='//TRIM(IDFRecords(Node2)%Numbers(1))
                  ENDIF
                  IF (MakeUPPERCase(IDFRecords(Node1)%Alphas(2)) == 'THERMAL ZONE' .and. MakeUPPERCase(IDFRecords(Node2)%Alphas(2)) == 'THERMAL ZONE') THEN
                    OutArgs(1)=TRIM(OutArgs(1))//' - InterZonal Surface'
                  ENDIF
                  OutArgs(2)=InArgs(1)
                  OutArgs(3)=' '
                  OutArgs(4)='1.0'
!                  OutArgs(5)='1.0'
                  CurArgs=4
                  nodiff=.false.
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'MRR') THEN
                  CYCLE
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'DMP') THEN
                  CYCLE
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'ELR') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT LEAKAGE RATIO'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2:5)=InArgs(5:8)
                  nodiff=.false.
                  CurArgs=5
                ELSEIF (MakeUPPERCase(InArgs(2)) == 'CPD') THEN
                  ObjectName='AIRFLOWNETWORK:DISTRIBUTION:CONSTANT PRESSURE DROP'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)
                  OutArgs(2)=InArgs(6)
                  nodiff=.false.
                  CurArgs=2
                ELSE
                  WRITE(Auditf,fmta) ' Cannot convert ADS Element Data Object='//TRIM(InArgs(1))
                  CYCLE
                ENDIF

              CASE ('ADS LINKAGE DATA')
                ObjectName='AIRFLOWNETWORK:DISTRIBUTION:LINKAGE'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:2)=InArgs(1:2)
                OutArgs(3)=InArgs(4)
                OutArgs(4)=InArgs(6)
                OutArgs(5)=InArgs(7)
!                write(diflfn,*) '! default=',trim(inargs(1))
                ELAElement=.false.
                PLRElement=.false.
                DelElement=.false.
                TZExt1=.false.
                TZExt2=.false.
                DO xNum=1,NumIDFRecords
                  IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS ELEMENT DATA') THEN
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(6))) .and.   &  ! Match this element name
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'ELA') THEN
                      ELAElement=.true.
                      EXIT
                    ENDIF
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(6))) .and.   &  ! Match this element name
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'DMP' .or.  &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'MRR') THEN
                      DelElement=.true.
                      EXIT
                    ENDIF
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(6))) .and.   &  ! Match this element name
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'PLR') THEN
                      DO yNum=1,NumIDFRecords
                        IF (MakeUPPERCase(TRIM(IDFRecords(yNum)%Name)) == 'ADS NODE DATA') THEN
                          IF (SameString(TRIM(IDFRecords(yNum)%Alphas(1)),TRIM(InArgs(2))) .and.   &
                             (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.  &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL') ) THEN
                            TZExt1=.true.
                          ENDIF
                          IF (SameString(TRIM(IDFRecords(yNum)%Alphas(1)),TRIM(InArgs(4))) .and.   &
                             (MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'THERMAL ZONE' .or.  &
                              MakeUPPERCase(TRIM(IDFRecords(yNum)%Alphas(2))) == 'EXTERNAL') ) THEN
                            TZExt2=.true.
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDIF
                ENDDO
                IF (ELAElement) CYCLE
                IF (DelElement) CYCLE
                IF (TZExt1 .and. TZExt2) CYCLE
                DO xNum=1,NumIDFRecords
                  IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS NODE DATA') THEN
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(2))) .and.   &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'THERMAL ZONE') THEN
!                      write(diflfn,fmta) ' ! match on ads node data -- f2='//trim(IDFRecords(xNum)%Alphas(4))
                      OutArgs(2)=IDFRecords(xNum)%Alphas(4)
                    ENDIF
                  ENDIF
                ENDDO
                DO xNum=1,NumIDFRecords
                  IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS NODE DATA') THEN
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(4))) .and.   &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'THERMAL ZONE') THEN
!                      write(diflfn,fmta) ' !  match on ads node data -- f4='//trim(IDFRecords(xNum)%Alphas(4))
                      OutArgs(3)=IDFRecords(xNum)%Alphas(4)
                    ENDIF
                  ENDIF
                ENDDO
                DO xNum=1,NumIDFRecords
                  IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS ELEMENT DATA') THEN
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(6))) .and.   &  ! Match this element name
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'DWC'      .and.   &
                        (MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == 'COIL:DX:COOLINGBYPASSFACTOREMPIRICAL' .or.  &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == 'SINGLE DUCT:CONST VOLUME:REHEAT' .or.  &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == 'COIL:GAS:HEATING' .or.  &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == 'COIL:ELECTRIC:HEATING') ) THEN
!                         write(diflfn,fmta) ' ! matched element data name/a4 -- use f6='//trim(IDFRecords(xNum)%Alphas(3))
!                         write(diflfn,fmta) ' ! from element data='//trim(outargs(6))
                      OutArgs(4)=IDFRecords(xNum)%Alphas(3)
                    ENDIF
                  ENDIF
                  IF (MakeUPPERCase(TRIM(IDFRecords(xNum)%Name)) == 'ADS ELEMENT DATA') THEN
                    IF (SameString(TRIM(IDFRecords(xNum)%Alphas(1)),TRIM(InArgs(6))) .and.   &  ! Match this element name
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(2))) == 'CVF'      .and.   &
                         MakeUPPERCase(TRIM(IDFRecords(xNum)%Alphas(4))) == 'FAN:SIMPLE:CONSTVOLUME' ) THEN
!                         write(diflfn,fmta) ' ! matched element data name/a4 -- use f6='//trim(IDFRecords(xNum)%Alphas(3))
!                         write(diflfn,fmta) ' ! from element data='//trim(outargs(6))
                      OutArgs(4)=IDFRecords(xNum)%Alphas(3)
                    ENDIF
                  ENDIF
                ENDDO
                CurArgs=6
                nodiff=.false.

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

!END SUBROUTINE CreateNewIDFUsingRulesV1_3_0
