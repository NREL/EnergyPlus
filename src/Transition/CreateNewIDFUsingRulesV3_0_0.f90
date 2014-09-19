!SUBROUTINE CreateNewIDFUsingRulesV3_0_0(EndOfFile,DiffOnly,InLfn,AskForInput,InputFileName,ArgFile,ArgIDFExtension)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2002
          !       MODIFIED       For each release
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates new IDFs based on the rules specified by
          ! developers.  This will result in a more complete transition but
          ! takes more time to create.   This routine is specifically for rules
          ! 2.1.0 to 2.2.0.

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
  INTEGER Num1
  INTEGER Num2
  INTEGER Num3
  INTEGER Num4
  INTEGER Num5
  INTEGER Num6
  INTEGER Num7
  INTEGER PumpNum
  INTEGER, EXTERNAL :: GetNewUnitNumber
  INTEGER, EXTERNAL :: FindNumber
  INTEGER Arg
  LOGICAL, SAVE :: FirstTime=.true.
  CHARACTER(len=30) UnitsArg
  CHARACTER(len=MaxNameLength) ::  ObjectName
  CHARACTER(len=30), EXTERNAL :: TrimTrailZeros
  CHARACTER(len=MaxNameLength) ::  UCRepVarName=blank
  CHARACTER(len=MaxNameLength) ::  UCCompRepVarName=blank
  CHARACTER(len=MaxNameLength) ::  PrimarySideName=blank
  CHARACTER(len=MaxNameLength) ::  SecondarySideName=blank
  CHARACTER(len=MaxNameLength) ::  BranchPrimaryName=blank
  CHARACTER(len=MaxNameLength) ::  BranchListPrimaryName=blank
  CHARACTER(len=MaxNameLength) ::  PlantLoopPrimaryName=blank
  CHARACTER(len=MaxNameLength) ::  BranchSecondaryName=blank
  CHARACTER(len=MaxNameLength) ::  BranchListSecondaryName=blank
  CHARACTER(len=MaxNameLength) ::  PlantLoopSecondaryName=blank
  CHARACTER(len=MaxNameLength) ::  PlantOldDemandBranchListName=blank
  CHARACTER(len=MaxNameLength) ::  PlantOldDemandConnectorListName=blank
  CHARACTER(len=MaxNameLength) ::  SecondarySetpointManagerNodeName=blank
  CHARACTER(len=MaxNameLength) ::  SecondarySetpointManagerNodeListName=blank
  CHARACTER(len=MaxNameLength) ::  TestName=blank
  CHARACTER(len=MaxNameLength) ::  PlantLoopPrimaryDemandInletNodeName=blank
  CHARACTER(len=MaxNameLength) ::  PlantLoopPrimaryDemandOutletNodeName=blank
  CHARACTER(len=MaxNameLength) ::  PlantLoopSecondarySupplyInletNodeName=blank
  CHARACTER(len=MaxNameLength) ::  PlantLoopSecondarySupplyOutletNodeName=blank
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
  LOGICAL :: ConnComp
  LOGICAL :: ConnCompCtrl
  LOGICAL :: FileExist
  CHARACTER(len=MaxNameLength) :: CreatedOutputName
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: DeleteThisRecord
  LOGICAL :: Success
  INTEGER :: PrimaryLoopNum
  INTEGER :: InNum
  INTEGER :: TotGenCount
  INTEGER :: TotPVCount
  INTEGER :: InNum2
  INTEGER :: InNum3
  REAL :: AvePV

  LOGICAL :: ErrFlag

  If (FirstTime) THEN  ! do things that might be applicable only to this new version
    CALL ReadRenamedObjects('V3-0-0-ObjectRenames.txt')
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

          ALLOCATE(Alphas(MaxAlphaArgsFound),Numbers(MaxNumericArgsFound))
          ALLOCATE(InArgs(MaxTotalArgs))
          ALLOCATE(AorN(MaxTotalArgs),ReqFld(MaxTotalArgs),FldNames(MaxTotalArgs),FldDefaults(MaxTotalArgs),FldUnits(MaxTotalArgs))
          ALLOCATE(NwAorN(MaxTotalArgs),NwReqFld(MaxTotalArgs),NwFldNames(MaxTotalArgs),NwFldDefaults(MaxTotalArgs),NwFldUnits(MaxTotalArgs))
          ALLOCATE(OutArgs(MaxTotalArgs))
          ALLOCATE(MatchArg(MaxTotalArgs))
          ALLOCATE(DeleteThisRecord(NumIDFRecords))
          DeleteThisRecord=.false.

          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'CONNECTION COMPONENT:PLANTLOOP') ConnComp=.true.
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'CONNECTION COMPONENT:PLANTLOOP:CONTROLLED') ConnCompCtrl=.true.
          ENDDO
          IF (ConnComp .or. ConnCompCtrl) THEN
            PrimarySideName=blank
            SecondarySideName=blank
            BranchPrimaryName=blank
            BranchListPrimaryName=blank
            PlantLoopPrimaryName=blank
            BranchSecondaryName=blank
            BranchListSecondaryName=blank
            PlantLoopSecondaryName=blank
            SecondarySetpointManagerNodeName=blank
            SecondarySetpointManagerNodeListName=blank
            PrimaryLoopNum=0
            DO Num=1,NumIDFRecords
              IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'CONNECTION COMPONENT:PLANTLOOP' .and.   &
                  MakeUPPERCase(IDFRecords(Num)%Name) /= 'CONNECTION COMPONENT:PLANTLOOP:CONTROLLED') CYCLE
              ! will match on inlet sides only (these should be sufficient to match both?)
              PrimarySideName=MakeUPPERCase(IDFRecords(Num)%Alphas(2))
              SecondarySideName=MakeUPPERCase(IDFRecords(Num)%Alphas(4))
              write(diflfn,fmta) '! connection component, name='//trim(IDFRecords(Num)%Alphas(1))//  &
                 ', primary side='//trim(PrimarySideName)//   &
                 ', secondary side='//trim(SecondarySideName)
              DeleteThisRecord(Num)=.true.
              DO Num1=1,NumIDFRecords
                IF (MakeUPPERCase(IDFRecords(Num1)%Name) /= 'BRANCH') CYCLE
                IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(4)) == PrimarySideName) THEN
                   BranchPrimaryName=MakeUPPERCase(IDFRecords(Num1)%Alphas(1))
                   DeleteThisRecord(Num1)=.true.
                   write(diflfn,fmta) '! primary side branch='//trim(BranchPrimaryName)
                ENDIF
                IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(4)) == SecondarySideName) THEN
                   BranchSecondaryName=MakeUPPERCase(IDFRecords(Num1)%Alphas(1))
                   DeleteThisRecord(Num1)=.true.
                   write(diflfn,fmta) '! secondary side branch='//trim(BranchSecondaryName)
                ENDIF
              ENDDO
              PumpNum=0
              DO Num1=1,NumIDFRecords
                IF (MakeUPPERCase(IDFRecords(Num1)%Name) /= 'BRANCH LIST') CYCLE
                DO Num2=2,IDFRecords(Num1)%NumAlphas
                  IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(Num2)) == BranchPrimaryName) THEN
                     BranchListPrimaryName=MakeUPPERCase(IDFRecords(Num1)%Alphas(1))
                     DO Num3=Num2,IDFRecords(Num1)%NumAlphas-1
                       IDFRecords(Num1)%Alphas(Num3)=IDFRecords(Num1)%Alphas(Num3+1)
                     ENDDO
                     write(diflfn,fmta) '! primary side branch list='//trim(BranchListPrimaryName)
                  ENDIF
                  IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(Num2)) == BranchSecondaryName) THEN
                     BranchListSecondaryName=MakeUPPERCase(IDFRecords(Num1)%Alphas(1))
                     DO Num3=Num2,IDFRecords(Num1)%NumAlphas-1
                       IDFRecords(Num1)%Alphas(Num3)=IDFRecords(Num1)%Alphas(Num3+1)
                     ENDDO
                     write(diflfn,fmta) '! secondary side branch list='//trim(BranchListSecondaryName)
                  ENDIF
                ENDDO
              ENDDO
              ! find loop 1 that will get converted.
              ! secondary loop
              DO Num1=1,NumIDFRecords
                IF (MakeUPPERCase(IDFRecords(Num1)%Name) /= 'PLANT LOOP') CYCLE
                IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(7)) == BranchListSecondaryName) THEN
                  ! supply side
                  DeleteThisRecord(Num1)=.true.
                  write(diflfn,fmta) '! secondary plant loop (from supply side='//  &
                     trim(IDFRecords(Num1)%Alphas(1))
                  SecondarySetpointManagerNodeName=IDFRecords(Num1)%Alphas(4)
                  SecondarySetpointManagerNodeListName=SecondarySetpointManagerNodeName
                  write(diflfn,fmta) '! secondary set point manager='//trim(SecondarySetpointManagerNodeName)
                  PlantLoopSecondarySupplyInletNodeName=MakeUPPERCase(IDFRecords(Num1)%Alphas(5))
                  PlantLoopSecondarySupplyOutletNodeName=MakeUPPERCase(IDFRecords(Num1)%Alphas(6))
                  EXIT
                ENDIF
                IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(11)) == BranchListSecondaryName) THEN
                  ! demand side
                  write(diflfn,fmta) '! secondary plant loop (from demand side)='//  &
                     trim(IDFRecords(Num1)%Alphas(1))
                  SecondarySetpointManagerNodeName=IDFRecords(Num1)%Alphas(4)
                  SecondarySetpointManagerNodeListName=SecondarySetpointManagerNodeName
                  write(diflfn,fmta) '! secondary set point manager='//trim(SecondarySetpointManagerNodeName)
                  PlantLoopSecondarySupplyInletNodeName=MakeUPPERCase(IDFRecords(Num1)%Alphas(5))
                  PlantLoopSecondarySupplyOutletNodeName=MakeUPPERCase(IDFRecords(Num1)%Alphas(6))
                ENDIF
              ENDDO
              ! primary loop
              DO Num2=1,NumIDFRecords
                IF (MakeUPPERCase(IDFRecords(Num2)%Name) /= 'PLANT LOOP') CYCLE
                IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(7)) == BranchListPrimaryName) THEN
                 ! supply side
                  write(diflfn,fmta) '! primary plant loop (from supply side='//  &
                     trim(IDFRecords(Num2)%Alphas(1))
                ENDIF
                IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(11)) == BranchListPrimaryName) THEN
                  Alphas(1:IDFRecords(Num2)%NumAlphas)=IDFRecords(Num2)%Alphas(1:IDFRecords(Num2)%NumAlphas)
                  DEALLOCATE(IDFRecords(Num2)%Alphas)
                  ALLOCATE(IDFRecords(Num2)%Alphas(16))
                  IDFRecords(Num2)%Alphas=blank
                  IDFRecords(Num2)%Alphas(1:IDFRecords(Num2)%NumAlphas)=Alphas(1:IDFRecords(Num2)%NumAlphas)
                  ! convert demand side
                  write(diflfn,fmta) '! primary plant loop (from demand side='//  &
                     trim(IDFRecords(Num2)%Alphas(1))
                  PlantOldDemandBranchListName=MakeUPPERCase(IDFRecords(Num2)%Alphas(11))
                  PlantOldDemandConnectorListName=MakeUPPERCase(IDFRecords(Num2)%Alphas(12))
                  PlantLoopPrimaryDemandInletNodeName=MakeUPPERCase(IDFRecords(Num2)%Alphas(9))
                  PlantLoopPrimaryDemandOutletNodeName=MakeUPPERCase(IDFRecords(Num2)%Alphas(10))
                  IDFRecords(Num2)%Alphas(9)=IDFRecords(Num1)%Alphas(9)
                  IDFRecords(Num2)%Alphas(10)=IDFRecords(Num1)%Alphas(10)
                  IDFRecords(Num2)%Alphas(11)=IDFRecords(Num1)%Alphas(11)
                  IDFRecords(Num2)%Alphas(12)=IDFRecords(Num1)%Alphas(12)
                  PrimaryLoopNum=Num2
                  IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'CONNECTION COMPONENT:PLANTLOOP') THEN
                    ! only one way pipe
                    IDFRecords(Num2)%Alphas(16)='Common Pipe'
                    IDFRecords(Num2)%NumAlphas=16
                  ELSE
                    IDFRecords(Num2)%Alphas(16)='Two Way Common Pipe'
                    IDFRecords(Num2)%NumAlphas=16
                  ENDIF
                  ! Go mark stuff on old loops for deletion.
                  DO Num3=1,NumIDFRecords
                    ! Num1 is the secondary plant loop
                    IF (MakeUPPERCase(IDFRecords(Num3)%Name) == 'PLANT OPERATION SCHEMES') THEN
                      IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(3)) == MakeUPPERCase(IDFRecords(Num3)%Alphas(1)) ) THEN
                        DeleteThisRecord(Num3)=.true.
                        write(diflfn,fmta) '! secondary plant loop, op scheme='//trim(IDFRecords(Num3)%Alphas(1))
                        ! need to drill down and delete controls and plant equipment lists that reference the connection (?)
                        ! or just the items that reference?
                        DO Num4=2,IDFRecords(Num3)%NumAlphas,3
                          DO Num5=1,NumIDFRecords
                            IF (MakeUPPERCase(IDFRecords(Num5)%Name) /= MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4)) ) CYCLE
                            ! found right control scheme
                            IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(1)) /= MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4+1)) ) CYCLE
                            ! found right name
                            DeleteThisRecord(Num5)=.true.
                            ! Now delete plant equipment list for this control scheme
                            DO Num6=2,IDFRecords(Num5)%NumAlphas
                              ! search out Plant Equipment Lists
                              DO Num7=1,NumIDFRecords
                                IF (MakeUPPERCase(IDFRecords(Num7)%Name) /= 'PLANT EQUIPMENT LIST') CYCLE
                                IF (MakeUPPERCase(IDFRecords(Num7)%Alphas(1)) /= MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6))) CYCLE
                                DeleteThisRecord(Num7)=.true.
                              ENDDO
                            ENDDO
                          ENDDO
                        ENDDO
                      ENDIF
                    ENDIF
                    IF (MakeUPPERCase(IDFRecords(Num3)%Name) == 'BRANCH LIST') THEN
                      IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(7)) == MakeUPPERCase(IDFRecords(Num3)%Alphas(1)) ) THEN
                        DeleteThisRecord(Num3)=.true.
                        write(diflfn,fmta) '! secondary plant loop, supply branch list='//trim(IDFRecords(Num3)%Alphas(1))
                        ! mark branches in branch list for deletion.
                        DO Num4=2,IDFRecords(Num3)%NumAlphas
                          DO Num5=1,NumIDFRecords
                            IF (MakeUPPERCase(IDFRecords(Num5)%Name) == 'BRANCH') THEN
                              IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4)) == MakeUPPERCase(IDFRecords(Num5)%Alphas(1)) ) THEN
                                DeleteThisRecord(Num5)=.true.
                                write(diflfn,fmta) '! secondary plant loop, branch='//trim(IDFRecords(Num5)%Alphas(1))
                                ! delete components on this branch
                                DO Num6=2,IDFRecords(Num5)%NumAlphas,5
                                  TestName=MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6))
                                  IF (TestName(1:4) == 'PUMP') THEN
                                    ! dont delete supply side pump
                                    DO Num7=1,NumIDFRecords
                                      IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6)) == MakeUPPERCase(IDFRecords(Num7)%Name) ) THEN
                                        IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6+1)) == MakeUPPERCase(IDFRecords(Num7)%Alphas(1)) ) THEN
                                          PumpNum=Num7
                                          write(diflfn,fmta) '! secondary plant loop, supply side pump='//  &
                                             trim(IDFRecords(Num7)%Name)//':'//trim(IDFRecords(Num7)%Alphas(1))
                                          EXIT
                                        ENDIF
                                      ENDIF
                                    ENDDO
                                  ELSE
                                    DO Num7=1,NumIDFRecords
                                      IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6)) == MakeUPPERCase(IDFRecords(Num7)%Name)) THEN
                                        IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6+1)) == MakeUPPERCase(IDFRecords(Num7)%Alphas(1)) ) THEN
                                          DeleteThisRecord(Num7)=.true.
                                          write(diflfn,fmta) '! secondary plant loop, branch component='//  &
                                             trim(IDFRecords(Num7)%Name)//':'//trim(IDFRecords(Num7)%Alphas(1))
                                        ENDIF
                                      ENDIF
                                    ENDDO
                                  ENDIF
                                ENDDO  ! delete components on branch (demand side)
                              ENDIF
                            ENDIF
                          ENDDO
                        ENDDO  ! mark branches for deletion
                      ENDIF
                    ENDIF ! branch list
                    IF (MakeUPPERCase(IDFRecords(Num3)%Name) == 'CONNECTOR LIST') THEN
                      IF (MakeUPPERCase(IDFRecords(Num1)%Alphas(8)) == MakeUPPERCase(IDFRecords(Num3)%Alphas(1)) ) THEN
                        DeleteThisRecord(Num3)=.true.
                        write(diflfn,fmta) '! secondary plant loop, supply connector list='//trim(IDFRecords(Num3)%Alphas(1))
                        ! mark components in connector list for deletion.
                        DO Num4=2,IDFRecords(Num3)%NumAlphas,2
                          DO Num5=1,NumIDFRecords
                            IF (MakeUPPERCase(IDFRecords(Num5)%Name) == MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4))) THEN
                              IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(1)) == MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4+1))) THEN
                                write(diflfn,fmta) '! secondary plant loop, supply connector list item='//  &
                                   trim(IDFRecords(Num5)%Name)//':'//trim(IDFRecords(Num5)%Alphas(1))
                                DeleteThisRecord(Num5)=.true.
                              ENDIF
                            ENDIF
                          ENDDO
                        ENDDO
                      ENDIF
                    ENDIF ! connector list
                    IF (MakeUPPERCase(IDFRecords(Num3)%Name) == 'BRANCH LIST') THEN
                      IF (PlantOldDemandBranchListName == MakeUPPERCase(IDFRecords(Num3)%Alphas(1)) ) THEN
                        DeleteThisRecord(Num3)=.true.
                        write(diflfn,fmta) '! primary plant loop, demand branch list='//trim(IDFRecords(Num3)%Alphas(1))
                        ! mark branches in branch list for deletion.
                        DO Num4=2,IDFRecords(Num3)%NumAlphas
                          DO Num5=1,NumIDFRecords
                            IF (MakeUPPERCase(IDFRecords(Num5)%Name) == 'BRANCH') THEN
                              IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4)) == MakeUPPERCase(IDFRecords(Num5)%Alphas(1)) ) THEN
                                DeleteThisRecord(Num5)=.true.
                                write(diflfn,fmta) '! primary plant loop, branch='//trim(IDFRecords(Num5)%Alphas(1))
                                ! delete components on this branch
                                DO Num6=2,IDFRecords(Num5)%NumAlphas,5
                                  TestName=MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6))
                                  IF (TestName(1:4) == 'PUMP') THEN
                                    ! dont delete supply side pump
                                    DO Num7=1,NumIDFRecords
                                      IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6)) == MakeUPPERCase(IDFRecords(Num7)%Name) ) THEN
                                        IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6+1)) == MakeUPPERCase(IDFRecords(Num7)%Alphas(1)) ) THEN
                                          PumpNum=Num7
                                          write(diflfn,fmta) '! primary plant loop, demand side pump='//  &
                                             trim(IDFRecords(Num7)%Name)//':'//trim(IDFRecords(Num7)%Alphas(1))
                                          EXIT
                                        ENDIF
                                      ENDIF
                                    ENDDO
                                  ELSE
                                    DO Num7=1,NumIDFRecords
                                      IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6)) == MakeUPPERCase(IDFRecords(Num7)%Name)) THEN
                                        IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(Num6+1)) == MakeUPPERCase(IDFRecords(Num7)%Alphas(1)) ) THEN
                                          DeleteThisRecord(Num7)=.true.
                                          write(diflfn,fmta) '! primary plant loop, branch component='//  &
                                             trim(IDFRecords(Num7)%Name)//':'//trim(IDFRecords(Num7)%Alphas(1))
                                        ENDIF
                                      ENDIF
                                    ENDDO
                                  ENDIF
                                ENDDO  ! delete components on primary plant loop branch (demand side)
                              ENDIF
                            ENDIF
                          ENDDO
                        ENDDO  ! mark branches for deletion
                      ENDIF
                    ENDIF ! branch list
                    IF (MakeUPPERCase(IDFRecords(Num3)%Name) == 'CONNECTOR LIST') THEN
                      IF (PlantOldDemandConnectorListName == MakeUPPERCase(IDFRecords(Num3)%Alphas(1)) ) THEN
                        DeleteThisRecord(Num3)=.true.
                        write(diflfn,fmta) '! primary plant loop, demand connector list='//trim(IDFRecords(Num3)%Alphas(1))
                        ! mark components in connector list for deletion.
                        DO Num4=2,IDFRecords(Num3)%NumAlphas,2
                          DO Num5=1,NumIDFRecords
                            IF (MakeUPPERCase(IDFRecords(Num5)%Name) == MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4))) THEN
                              IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(1)) == MakeUPPERCase(IDFRecords(Num3)%Alphas(Num4+1))) THEN
                                write(diflfn,fmta) '! primary plant loop, demand connector list item='//  &
                                   trim(IDFRecords(Num5)%Name)//':'//trim(IDFRecords(Num5)%Alphas(1))
                                DeleteThisRecord(Num5)=.true.
                              ENDIF
                            ENDIF
                          ENDDO
                        ENDDO
                      ENDIF
                    ENDIF ! connector list
                    ! put pump in the right place.
                    IF (PumpNum /= 0) THEN
                      DO Num4=1,NumIDFRecords
                        IF (MakeUPPERCase(IDFRecords(Num4)%Name) /= 'BRANCH LIST') CYCLE
                        IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(11)) /= MakeUPPERCase(IDFRecords(Num4)%Alphas(1))) CYCLE
                        ! Found branch list, find branch inlet -- branch inlet is #1 on Branch List
                        DO Num5=1,NumIDFRecords
                          IF (MakeUPPERCase(IDFRecords(Num5)%Name) /= 'BRANCH') CYCLE
                          IF (MakeUPPERCase(IDFRecords(Num5)%Alphas(1)) /= MakeUPPERCase(IDFRecords(Num4)%Alphas(2))) CYCLE
                          ! got branch.
                          ! first "delete" the thing being replaced
                          DO Num6=1,NumIDFRecords
                            IF (MakeUpperCase(IDFRecords(Num6)%Name) /= MakeUPPERCase(IDFRecords(Num5)%Alphas(2))) CYCLE
                            IF (MakeUpperCase(IDFRecords(Num6)%Alphas(1)) /= MakeUPPERCase(IDFRecords(Num5)%Alphas(3))) CYCLE
                            DeleteThisRecord(Num6)=.true.
                            EXIT
                          ENDDO
                          IDFRecords(Num5)%Alphas(2)=IDFRecords(PumpNum)%Name
                          IDFRecords(Num5)%Alphas(3)=IDFRecords(PumpNum)%Alphas(1)
                          IDFRecords(PumpNum)%Alphas(2)=IDFRecords(Num5)%Alphas(4)
                          IDFRecords(PumpNum)%Alphas(3)=IDFRecords(Num5)%Alphas(5)
                          ! Make Branch Active, not Passive
                          IDFRecords(Num5)%Alphas(6)='Active'
                          write(diflfn,fmta) '! pump onto branch='//  &
                            trim(IDFRecords(Num5)%Name)//':'//trim(IDFRecords(Num5)%Alphas(1))
                          ! reset PumpNum now
                          PumpNum=0
                          EXIT
                        ENDDO
                      ENDDO
                    ENDIF  ! Pump
                  ENDDO
                ENDIF
              ENDDO  ! Primary loop stuff
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'CONNECTION COMPONENT:PLANTLOOP') THEN
                ! Delete setpoint manager from original secondary
                Success=.false.
                DO Num2=1,NumIDFRecords
                  ! determine whether node list or node name
                  IF (MakeUPPERCase(IDFRecords(Num2)%Name) /= 'NODE LIST') CYCLE
                  IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(1)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName)) CYCLE
                  Success=.true.
                  EXIT
                ENDDO
                IF (.not. Success) THEN  ! wasn't a node list name, must have been a Node Name
                  DO Num2=1,NumIDFRecords
                    ! determine whether node list or node name
                    IF (MakeUPPERCase(IDFRecords(Num2)%Name) /= 'NODE LIST') CYCLE
                    DO Num3=2,IDFRecords(Num2)%NumAlphas
                      IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(Num3)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      SecondarySetpointManagerNodeListName=IDFRecords(Num2)%Alphas(1)
                      Success=.true.
                      EXIT
                    ENDDO
                  ENDDO
                ENDIF
                Success=.false.
                DO Num3=1,NumIDFRecords
                  SELECT CASE (MakeUPPERCase(IDFRecords(Num3)%Name))
                    CASE('SET POINT MANAGER:SCHEDULED')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(4)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(4)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:SCHEDULED:DUALSETPOINT')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:OUTSIDE AIR')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:SINGLE ZONE REHEAT','SET POINT MANAGER:SINGLE ZONE HEATING',  &
                         'SET POINT MANAGER:SINGLE ZONE COOLING','SET POINT MANAGER:MIXED AIR')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(6)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(6)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:SINGLE ZONE MIN HUM','SET POINT MANAGER:WARMEST','SET POINT MANAGER:COLDEST')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:SINGLE ZONE MAX HUM')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(4)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(4)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:OUTSIDE AIR PRETREAT')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(7)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(7)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE('SET POINT MANAGER:WARMEST TEMP FLOW')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      DeleteThisRecord(Num3)=.true.
                      Success=.true.
                    CASE DEFAULT
                  END SELECT
                ENDDO
              ELSE  ! CONNECTION COMPONENT:PLANTLOOP:CONTROLLED
                    ! same scheme as one way, except you don't delete it, you point it to a different place
                IF (PrimaryLoopNum == 0) CYCLE
                Success=.false.
                DO Num2=1,NumIDFRecords
                  ! determine whether node list or node name
                  IF (MakeUPPERCase(IDFRecords(Num2)%Name) /= 'NODE LIST') CYCLE
                  IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(1)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName)) CYCLE
                  Success=.true.
                  EXIT
                ENDDO
                IF (.not. Success) THEN  ! wasn't a node list name, must have been a Node Name
                  DO Num2=1,NumIDFRecords
                    ! determine whether node list or node name
                    IF (MakeUPPERCase(IDFRecords(Num2)%Name) /= 'NODE LIST') CYCLE
                    DO Num3=2,IDFRecords(Num2)%NumAlphas
                      IF (MakeUPPERCase(IDFRecords(Num2)%Alphas(Num3)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      SecondarySetpointManagerNodeListName=IDFRecords(Num2)%Alphas(1)
                      Success=.true.
                      EXIT
                    ENDDO
                  ENDDO
                ENDIF
                Success=.false.
                DO Num3=1,NumIDFRecords
                  SELECT CASE (MakeUPPERCase(IDFRecords(Num3)%Name))
                    ! check set point managers
                    CASE('SET POINT MANAGER:SCHEDULED','SET POINT MANAGER:SINGLE ZONE MAX HUM')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(4)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(4)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      write(DifLfn,fmta) '! replacing setpoint node on primary loop with reference '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                      IDFRecords(Num3)%Alphas(4)=IDFRecords(PrimaryLoopNum)%Alphas(9)
                      Success=.true.
                    CASE('SET POINT MANAGER:SCHEDULED:DUALSETPOINT','SET POINT MANAGER:SINGLE ZONE MIN HUM',  &
                         'SET POINT MANAGER:WARMEST','SET POINT MANAGER:COLDEST','SET POINT MANAGER:WARMEST TEMP FLOW')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(5)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      write(DifLfn,fmta) '! replacing setpoint node on primary loop with reference '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                      IDFRecords(Num3)%Alphas(5)=IDFRecords(PrimaryLoopNum)%Alphas(9)
                      Success=.true.
                    CASE('SET POINT MANAGER:OUTSIDE AIR')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      write(DifLfn,fmta) '! replacing setpoint node on primary loop with reference '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                      IDFRecords(Num3)%Alphas(3)=IDFRecords(PrimaryLoopNum)%Alphas(9)
                      Success=.true.
                    CASE('SET POINT MANAGER:SINGLE ZONE REHEAT','SET POINT MANAGER:SINGLE ZONE HEATING',  &
                         'SET POINT MANAGER:SINGLE ZONE COOLING','SET POINT MANAGER:MIXED AIR')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(6)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(6)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      write(DifLfn,fmta) '! replacing setpoint node on primary loop with reference '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                      IDFRecords(Num3)%Alphas(6)=IDFRecords(PrimaryLoopNum)%Alphas(9)
                      Success=.true.
                    CASE('SET POINT MANAGER:OUTSIDE AIR PRETREAT')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(7)) /= MakeUPPERCase(SecondarySetpointManagerNodeListName) .and.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(7)) /= MakeUPPERCase(SecondarySetpointManagerNodeName)) CYCLE
                      write(DifLfn,fmta) '! replacing setpoint node on primary loop with reference '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                      IDFRecords(Num3)%Alphas(7)=IDFRecords(PrimaryLoopNum)%Alphas(9)
                      Success=.true.
                    ! check system availability managers too.
                    CASE('SYSTEM AVAILABILITY MANAGER:DIFFERENTIAL THERMOSTAT')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopSecondarySupplyInletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopSecondarySupplyOutletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopPrimaryDemandInletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopPrimaryDemandOutletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) == PlantLoopSecondarySupplyInletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) == PlantLoopSecondarySupplyOutletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) == PlantLoopPrimaryDemandInletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(3)) == PlantLoopPrimaryDemandOutletNodeName) THEN
                        write(DifLfn,fmta) '! found node to be deleted on System Availability Manager, '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                        write(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//', Severe,'
                        write(DifLfn,fmta) TRIM(IDFRecords(Num3)%Name)//','
                        write(DifLfn,fmta) trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))//','
                        write(DifLfn,fmta) 'contains a reference to a primary/secondary plant loop node that will be removed,'
                        write(DifLfn,fmta) 'due to transition of Connection Component:Plant Loop:Controlled,'
                        write(DifLfn,fmta) trim(MakeUPPERCase(IDFRecords(Num)%Alphas(1)))//'.,'
                        write(DifLfn,fmta) 'This will not transition automatically.  Please contact EnergyPlus Support.;'
                      ENDIF
                    CASE('SYSTEM AVAILABILITY MANAGER:HIGH TEMPERATURE TURN OFF','SYSTEM AVAILABILITY MANAGER:HIGH TEMPERATURE TURN ON',   &
                         'SYSTEM AVAILABILITY MANAGER:LOW TEMPERATURE TURN OFF','SYSTEM AVAILABILITY MANAGER:LOW TEMPERATURE TURN ON')
                      IF (MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopSecondarySupplyInletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopSecondarySupplyOutletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopPrimaryDemandInletNodeName .or.   &
                          MakeUPPERCase(IDFRecords(Num3)%Alphas(2)) == PlantLoopPrimaryDemandOutletNodeName) THEN
                        write(DifLfn,fmta) '! found node to be deleted on System Availability Manager, '//    &
                           TRIM(IDFRecords(Num3)%Name)//':'//trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))
                        write(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//', Severe,'
                        write(DifLfn,fmta) TRIM(IDFRecords(Num3)%Name)//','
                        write(DifLfn,fmta) trim(MakeUPPERCase(IDFRecords(Num3)%Alphas(1)))//','
                        write(DifLfn,fmta) 'contains a reference to a primary/secondary plant loop node that will be removed,'
                        write(DifLfn,fmta) 'due to transition of Connection Component:Plant Loop:Controlled,'
                        write(DifLfn,fmta) trim(MakeUPPERCase(IDFRecords(Num)%Alphas(1)))//'.,'
                        write(DifLfn,fmta) 'This will not transition automatically.  Please contact EnergyPlus Support.;'
                      ENDIF
                    CASE DEFAULT
                  END SELECT
                ENDDO
              ENDIF
            ENDDO
          ENDIF

          NoVersion=.true.
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'VERSION') CYCLE
            NoVersion=.false.
            EXIT
          ENDDO
!
          DO Num=1,NumIDFRecords
            IF (DeleteThisRecord(Num)) THEN
              Write(DifLfn,fmta) '! Deleting: '//TRIM(IDFRecords(Num)%Name)//':'//TRIM(IDFRecords(Num)%Alphas(1))
            ENDIF
          ENDDO

          DO Num=1,NumIDFRecords

            IF (DeleteThisRecord(Num)) CYCLE
            DO xcount=IDFRecords(Num)%CommtS+1,IDFRecords(Num)%CommtE
              WRITE(DifLfn,fmta) TRIM(Comments(xcount))
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ' '
            ENDDO
            IF (NoVersion .and. Num == 1) THEN
              CALL GetNewObjectDefInIDD('VERSION',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
              OutArgs(1)='3.0'
              CurArgs=1
              CALL WriteOutIDFLinesAsComments(DifLfn,'Version',CurArgs,OutArgs,NwFldNames,NwFldUnits)
            ENDIF

     ! deleted objects.  no transition.
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'SKY RADIANCE DISTRIBUTION') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'AIRFLOW MODEL') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'GENERATOR:FC:BATTERY DATA') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'WATER HEATER:SIMPLE') THEN
              WRITE(DifLfn,fmta) '! The WATER HEATER:SIMPLE object has been deleted'
              WRITE(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//',warning,The WATER HEATER:SIMPLE object has been deleted;'
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
                IF (InArgs(1)(1:3) == '3.0' .and. ArgFile) THEN
                  CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                  CLOSE(diflfn,STATUS='DELETE')
                  LatestVersion=.true.
                  EXIT
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='3.0'
                nodiff=.false.

    !!!    Changes for this version
! Used automated trans rules from autotrans.f90 INCLUDE 'iddtrans.f90'
! File generated by AutoTrans
!     Generated: 20081002 162610.015
 ! Count of objects:          473
 ! Count of fields:         10202
 ! Count of keys:            4856
 ! Count of objects:          473
 ! Count of fields:         10202
 ! Count of keys:            4856
!              CASE('VERSION')
!                ObjectName='Version'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('BUILDING')
                ObjectName='Building'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('TIMESTEP IN HOUR')
                ObjectName='Timestep'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM CONVERGENCE LIMITS')
                ObjectName='ConvergenceLimits'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('INSIDE CONVECTION ALGORITHM')
                ObjectName='SurfaceConvectionAlgorithm:Inside'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTSIDE CONVECTION ALGORITHM')
                ObjectName='SurfaceConvectionAlgorithm:Outside'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SOLUTION ALGORITHM')
                ObjectName='HeatBalanceAlgorithm'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CTF',InArgs(1))) then
                  OutArgs(1)='ConductionTransferFunction'
                endif
                if (samestring('EMPD',InArgs(1))) then
                  OutArgs(1)='MoisturePenetrationDepthConductionTransferFunction'
                endif
                if (samestring('CondFD',InArgs(1))) then
                  OutArgs(1)='ConductionFiniteDifference'
                endif
                if (samestring('HAMT',InArgs(1))) then
                  OutArgs(1)='CombinedHeatAndMoistureFiniteElement'
                endif

              CASE('SHADOWING CALCULATIONS')
                ObjectName='ShadowCalculation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DEBUG OUTPUT')
                ObjectName='Output:DebuggingData'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DIAGNOSTICS')
                ObjectName='Output:Diagnostics'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PREPROCESSOR MESSAGE')
                ObjectName='Output:PreprocessorMessage'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('information',InArgs(2))) then
                  OutArgs(2)='Information'
                endif
                if (samestring('warning',InArgs(2))) then
                  OutArgs(2)='Warning'
                endif
                if (samestring('severe',InArgs(2))) then
                  OutArgs(2)='Severe'
                endif
                if (samestring('fatal',InArgs(2))) then
                  OutArgs(2)='Fatal'
                endif

              CASE('ZONE VOLUME CAPACITANCE MULTIPLIER')
                ObjectName='ZoneCapacitanceMultiplier'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('RUN CONTROL')
                ObjectName='SimulationControl'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CUSTOM METER')
                ObjectName='Meter:Custom'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CUSTOM METER:DECREMENT')
                ObjectName='Meter:CustomDecrement'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('RUNPERIOD')
                ObjectName='RunPeriod'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('RUNPERIOD:DESIGN')
                ObjectName='SizingPeriod:WeatherFileDays'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('RUNPERIOD:DESIGNSELECTIONFROMWEATHERFILE')
                ObjectName='SizingPeriod:WeatherFileConditionType'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Summer Extreme',InArgs(2))) then
                  OutArgs(2)='SummerExtreme'
                endif
                if (samestring('Summer Typical',InArgs(2))) then
                  OutArgs(2)='SummerTypical'
                endif
                if (samestring('Winter Extreme',InArgs(2))) then
                  OutArgs(2)='WinterExtreme'
                endif
                if (samestring('Winter Typical',InArgs(2))) then
                  OutArgs(2)='WinterTypical'
                endif
                if (samestring('Autumn Typical',InArgs(2))) then
                  OutArgs(2)='AutumnTypical'
                endif
                if (samestring('Spring Typical',InArgs(2))) then
                  OutArgs(2)='SpringTypical'
                endif
                if (samestring('Wet Season',InArgs(2))) then
                  OutArgs(2)='WetSeason'
                endif
                if (samestring('Dry Season',InArgs(2))) then
                  OutArgs(2)='DrySeason'
                endif
                if (samestring('No Dry Season',InArgs(2))) then
                  OutArgs(2)='NoDrySeason'
                endif
                if (samestring('No Wet Season',InArgs(2))) then
                  OutArgs(2)='NoWetSeason'
                endif
                if (samestring('Tropical Hot',InArgs(2))) then
                  OutArgs(2)='TropicalHot'
                endif
                if (samestring('Tropical Cold',InArgs(2))) then
                  OutArgs(2)='TropicalCold'
                endif

              CASE('SPECIALDAYPERIOD')
                ObjectName='RunPeriodControl:SpecialDays'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTSAVINGPERIOD')
                ObjectName='RunPeriodControl:DaylightSavingTime'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('LOCATION')
                ObjectName='Site:Location'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WEATHER STATION')
                ObjectName='Site:WeatherStation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SITE ATMOSPHERIC VARIATION')
                ObjectName='Site:HeightVariation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DESIGNDAY')
                ObjectName='SizingPeriod:DesignDay'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Wet-Bulb',InArgs(15))) then
                  OutArgs(15)='WetBulb'
                endif
                if (samestring('Dew-Point',InArgs(15))) then
                  OutArgs(15)='DewPoint'
                endif
                if (samestring('Humidity-Ratio',InArgs(15))) then
                  OutArgs(15)='HumidityRatio'
                endif
                if (samestring('Delta',InArgs(17))) then
                  OutArgs(17)='DifferenceSchedule'
                endif
                if (samestring('Multiplier',InArgs(17))) then
                  OutArgs(17)='MultiplierSchedule'
                endif
                if (samestring('Default Multipliers',InArgs(17))) then
                  OutArgs(17)='DefaultMultipliers'
                endif
                if (samestring('ASHRAE ClearSky',InArgs(19))) then
                  OutArgs(19)='ASHRAEClearSky'
                endif
                if (samestring('Zhang Huang',InArgs(19))) then
                  OutArgs(19)='ZhangHuang'
                endif

              CASE('GROUNDTEMPERATURES')
                ObjectName='Site:GroundTemperature:BuildingSurface'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GROUNDTEMPERATURES:SURFACE')
                ObjectName='Site:GroundTemperature:Shallow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GROUNDTEMPERATURES:DEEP')
                ObjectName='Site:GroundTemperature:Deep'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GROUNDREFLECTANCES')
                ObjectName='Site:GroundReflectance'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SNOW GROUND REFLECTANCE MODIFIERS')
                ObjectName='Site:GroundReflectance:SnowModifier'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WATER MAINS TEMPERATURES')
                ObjectName='Site:WaterMainsTemperature'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SCHEDULE',InArgs(1))) then
                  OutArgs(1)='Schedule'
                endif
                if (samestring('CORRELATION',InArgs(1))) then
                  OutArgs(1)='Correlation'
                endif

              CASE('SITE PRECIPITATION')
                ObjectName='Site:Precipitation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SCHEDULED DESIGN',InArgs(1))) then
                  OutArgs(1)='ScheduleAndDesignLevel'
                endif

              CASE('ROOF IRRIGATION')
                ObjectName='RoofIrrigation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SCHEDULED DESIGN',InArgs(1))) then
                  OutArgs(1)='Schedule'
                endif
                if (samestring('SMART SCHEDULE',InArgs(1))) then
                  OutArgs(1)='SmartSchedule'
                endif

              CASE('MATERIAL:REGULAR')
                ObjectName='Material'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:REGULAR-R')
                ObjectName='Material:NoMass'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:IRT')
                ObjectName='Material:InfraredTransparent'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:AIR')
                ObjectName='Material:AirGap'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:WINDOWGLASS')
                ObjectName='WindowMaterial:Glazing'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:WINDOWGLASS:ALTINPUT')
                ObjectName='WindowMaterial:Glazing:RefractionExtinctionMethod'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:WINDOWGAS')
                ObjectName='WindowMaterial:Gas'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:WINDOWGASMIXTURE')
                ObjectName='WindowMaterial:GasMixture'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:WINDOWSHADE')
                ObjectName='WindowMaterial:Shade'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIAL:WINDOWBLIND')
                ObjectName='WindowMaterial:Blind'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('HORIZONTAL',InArgs(2))) then
                  OutArgs(2)='Horizontal'
                endif
                if (samestring('VERTICAL',InArgs(2))) then
                  OutArgs(2)='Vertical'
                endif

              CASE('MATERIAL:WINDOWSCREEN')
                ObjectName='WindowMaterial:Screen'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Do Not Model',InArgs(2))) then
                  OutArgs(2)='DoNotModel'
                endif
                if (samestring('Model As Direct Beam',InArgs(2))) then
                  OutArgs(2)='ModelAsDirectBeam'
                endif
                if (samestring('Model As Diffuse',InArgs(2))) then
                  OutArgs(2)='ModelAsDiffuse'
                endif

              CASE('MATERIAL:ECOROOF')
                ObjectName='Material:RoofVegetation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:EMPD')
                ObjectName='MaterialProperty:MoisturePenetrationDepth:Settings'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:TEMPERATUREDEP:CONDFD')
                ObjectName='MaterialProperty:PhaseChange'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:HAMT-PROPERTIES')
                ObjectName='MaterialProperty:HeatAndMoistureTransfer:Settings'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:HAMT-ISOTHERM')
                ObjectName='MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:HAMT-SUCTION')
                ObjectName='MaterialProperty:HeatAndMoistureTransfer:Suction'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:HAMT-REDISTRIBUTION')
                ObjectName='MaterialProperty:HeatAndMoistureTransfer:Redistribution'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:HAMT-MU')
                ObjectName='MaterialProperty:HeatAndMoistureTransfer:Diffusion'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MATERIALPROPERTY:MOISTURE:HAMT-THERMAL')
                ObjectName='MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WINDOWGLASSSPECTRALDATA')
                ObjectName='MaterialProperty:GlazingSpectralData'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONSTRUCTION')
                ObjectName='Construction'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONSTRUCTION WITH INTERNAL SOURCE')
                ObjectName='Construction:InternalSource'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONSTRUCTION FROM WINDOW5 DATA FILE')
                ObjectName='Construction:WindowDataFile'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE')
                ObjectName='Zone'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE LIST')
                ObjectName='ZoneList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE GROUP')
                ObjectName='ZoneGroup'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SURFACEGEOMETRY')
                ObjectName='GlobalGeometryRules'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CounterClockWise',InArgs(2))) then
                  OutArgs(2)='Counterclockwise'
                endif
                if (samestring('ClockWise',InArgs(2))) then
                  OutArgs(2)='Clockwise'
                endif
                if (samestring('relative',InArgs(3))) then
                  OutArgs(3)='Relative'
                endif
                if (samestring('world',InArgs(3))) then
                  OutArgs(3)='World'
                endif
                if (samestring('relative',InArgs(4))) then
                  OutArgs(4)='Relative'
                endif
                if (samestring('world',InArgs(4))) then
                  OutArgs(4)='World'
                endif

              CASE('SURFACE:HEATTRANSFER')
                ObjectName='BuildingSurface:Detailed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('FLOOR',InArgs(2))) then
                  OutArgs(2)='Floor'
                endif
                if (samestring('WALL',InArgs(2))) then
                  OutArgs(2)='Wall'
                endif
                if (samestring('CEILING',InArgs(2))) then
                  OutArgs(2)='Ceiling'
                endif
                if (samestring('ROOF',InArgs(2))) then
                  OutArgs(2)='Roof'
                endif
                if (samestring('OtherZoneSurface',InArgs(5))) then
                  OutArgs(5)='Surface'
                endif
                if (samestring('OtherZone',InArgs(5))) then
                  OutArgs(5)='Surface'
                endif
                if (samestring('UnenteredOtherZoneSurface',InArgs(5))) then
                  OutArgs(5)='Zone'
                endif
                if (samestring('ExteriorEnvironment',InArgs(5))) then
                  OutArgs(5)='Outdoors'
                endif
                if (samestring('OtherSideCoeff',InArgs(5))) then
                  OutArgs(5)='OtherSideCoefficients'
                endif

              CASE('SURFACE:HEATTRANSFER:SUB')
                ObjectName='FenestrationSurface:Detailed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('WINDOW',InArgs(2))) then
                  OutArgs(2)='Window'
                endif
                if (samestring('DOOR',InArgs(2))) then
                  OutArgs(2)='Door'
                endif
                if (samestring('GLASSDOOR',InArgs(2))) then
                  OutArgs(2)='GlassDoor'
                endif
                if (samestring('TDD:DOME',InArgs(2))) then
                  OutArgs(2)='TubularDaylightDome'
                endif
                if (samestring('TDD:DIFFUSER',InArgs(2))) then
                  OutArgs(2)='TubularDaylightDiffuser'
                endif

              CASE('SURFACE:HEATTRANSFER:INTERNALMASS')
                ObjectName='InternalMass'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SURFACE:SHADING:DETACHED:FIXED')
                ObjectName='Shading:Site:Detailed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SURFACE:SHADING:DETACHED:BUILDING')
                ObjectName='Shading:Building:Detailed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SURFACE:SHADING:ATTACHED')
                ObjectName='Shading:Zone:Detailed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SHADING SURFACE REFLECTANCE')
                ObjectName='ShadingProperty:Reflectance'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WINDOWSHADINGCONTROL')
                ObjectName='WindowProperty:ShadingControl'
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
                  OutArgs(4)='OnIfHighOutdoorAirTemperature'
                endif
                if (samestring('ZoneAirTemp',InArgs(4))) then
                  OutArgs(4)='OnIfHighZoneAirTemperature'
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
                if (samestring('OnIfHighOutsideAirTemp',InArgs(4))) then
                  OutArgs(4)='OnIfHighOutdoorAirTemperature'
                endif
                if (samestring('OnIfHighZoneAirTemp',InArgs(4))) then
                  OutArgs(4)='OnIfHighZoneAirTemperature'
                endif
                if (samestring('OnNightIfLowOutsideTemp/OffDay',InArgs(4))) then
                  OutArgs(4)='OnNightIfLowOutdoorTempAndOffDay'
                endif
                if (samestring('OnNightIfLowInsideTemp/OffDay',InArgs(4))) then
                  OutArgs(4)='OnNightIfLowInsideTempAndOffDay'
                endif
                if (samestring('OnNightIfHeating/OffDay',InArgs(4))) then
                  OutArgs(4)='OnNightIfHeatingAndOffDay'
                endif
                if (samestring('OnNightIfLowOutsideTemp/OnDayIfCooling',InArgs(4))) then
                  OutArgs(4)='OnNightIfLowOutdoorTempAndOnDayIfCooling'
                endif
                if (samestring('OnNightIfHeating/OnDayIfCooling',InArgs(4))) then
                  OutArgs(4)='OnNightIfHeatingAndOnDayIfCooling'
                endif
                if (samestring('OffNight/OnDayIfCoolingAndHighSolarOnWindow',InArgs(4))) then
                  OutArgs(4)='OffNightAndOnDayIfCoolingAndHighSolarOnWindow'
                endif
                if (samestring('OnNight/OnDayIfCoolingAndHighSolarOnWindow',InArgs(4))) then
                  OutArgs(4)='OnNightAndOnDayIfCoolingAndHighSolarOnWindow'
                endif
                if (samestring('OnIfHighOutsideAirTempAndHighSolarOnWindow',InArgs(4))) then
                  OutArgs(4)='OnIfHighOutdoorAirTempAndHighSolarOnWindow'
                endif
                if (samestring('OnIfHighOutsideAirTempAndHighHorizontalSolar',InArgs(4))) then
                  OutArgs(4)='OnIfHighOutdoorAirTempAndHighHorizontalSolar'
                endif

              CASE('WINDOWFRAMEANDDIVIDER')
                ObjectName='WindowProperty:FrameAndDivider'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WINDOWGAPAIRFLOWCONTROL')
                ObjectName='WindowProperty:AirflowControl'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('InsideAir',InArgs(2))) then
                  OutArgs(2)='IndoorAir'
                endif
                if (samestring('OutsideAir',InArgs(2))) then
                  OutArgs(2)='OutdoorAir'
                endif
                if (samestring('InsideAir',InArgs(3))) then
                  OutArgs(3)='IndoorAir'
                endif
                if (samestring('OutsideAir',InArgs(3))) then
                  OutArgs(3)='OutdoorAir'
                endif
                if (samestring('AlwaysOnAtMaxFlow',InArgs(5))) then
                  OutArgs(5)='AlwaysOnAtMaximumFlow'
                endif

              CASE('STORMWINDOW')
                ObjectName='WindowProperty:StormWindow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MOVABLEINSULATION')
                ObjectName='SurfaceControl:MovableInsulation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Exterior',InArgs(1))) then
                  OutArgs(1)='Outside'
                endif
                if (samestring('Interior',InArgs(1))) then
                  OutArgs(1)='Inside'
                endif

              CASE('OTHERSIDECOEFFICIENTS')
                ObjectName='SurfaceProperty:OtherSideCoefficients'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OTHERSIDECONDITIONSMODEL')
                ObjectName='SurfaceProperty:OtherSideConditionsModel'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Transpired Collector',InArgs(2))) then
                  OutArgs(2)='TranspiredCollector'
                endif
                if (samestring('Vented PV Cavity',InArgs(2))) then
                  OutArgs(2)='VentedPhotoVoltaicCavity'
                endif
                if (samestring('Hybrid PV Transpired Collector',InArgs(2))) then
                  OutArgs(2)='HybridPhotoVoltaicTranspiredCollector'
                endif

              CASE('CONVECTIONCOEFFICIENTS')
                ObjectName='SurfaceProperty:ConvectionCoefficients'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Exterior',InArgs(2))) then
                  OutArgs(2)='Outside'
                endif
                if (samestring('Interior',InArgs(2))) then
                  OutArgs(2)='Inside'
                endif
                if (samestring('value',InArgs(3))) then
                  OutArgs(3)='Value'
                endif
                if (samestring('schedule',InArgs(3))) then
                  OutArgs(3)='Schedule'
                endif
                if (samestring('Exterior',InArgs(6))) then
                  OutArgs(6)='Outside'
                endif
                if (samestring('Interior',InArgs(6))) then
                  OutArgs(6)='Inside'
                endif
                if (samestring('value',InArgs(7))) then
                  OutArgs(7)='Value'
                endif
                if (samestring('schedule',InArgs(7))) then
                  OutArgs(7)='Schedule'
                endif

              CASE('CONVECTIONCOEFFICIENTS:MULTIPLESURFACE')
                ObjectName='SurfaceProperty:ConvectionCoefficients:MultipleSurface'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Exterior',InArgs(2))) then
                  OutArgs(2)='Outside'
                endif
                if (samestring('Interior',InArgs(2))) then
                  OutArgs(2)='Inside'
                endif
                if (samestring('value',InArgs(3))) then
                  OutArgs(3)='Value'
                endif
                if (samestring('schedule',InArgs(3))) then
                  OutArgs(3)='Schedule'
                endif
                if (samestring('Exterior',InArgs(6))) then
                  OutArgs(6)='Outside'
                endif
                if (samestring('Interior',InArgs(6))) then
                  OutArgs(6)='Inside'
                endif
                if (samestring('value',InArgs(7))) then
                  OutArgs(7)='Value'
                endif
                if (samestring('schedule',InArgs(7))) then
                  OutArgs(7)='Schedule'
                endif

              CASE('VAPORCOEFFICIENTS')
                ObjectName='SurfaceProperties:VaporCoefficients'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SURFACE:HEATTRANSFER:EXTERIORNATURALVENTEDCAVITY')
                ObjectName='SurfaceProperty:ExteriorNaturalVentedCavity'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GEOMETRYTRANSFORM')
                ObjectName='GeometryTransform'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('USERVIEWFACTORS')
                ObjectName='ZoneProperty:UserViewFactors'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ROOMAIR MODEL')
                ObjectName='RoomAirModelType'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('MIXING',InArgs(3))) then
                  OutArgs(3)='Mixing'
                endif
                if (samestring('USER DEFINED',InArgs(3))) then
                  OutArgs(3)='UserDefined'
                endif
                if (samestring('MUNDT',InArgs(3))) then
                  OutArgs(3)='OneNodeDisplacementVentilation'
                endif
                if (samestring('UCSD DISPLACEMENT VENTILATION',InArgs(3))) then
                  OutArgs(3)='ThreeNodeDisplacementVentilation'
                endif
                if (samestring('UCSD CROSS VENTILATION',InArgs(3))) then
                  OutArgs(3)='CrossVentilation'
                endif
                if (samestring('UCSD UFAD INTERIOR',InArgs(3))) then
                  OutArgs(3)='UnderFloorAirDistributionInterior'
                endif
                if (samestring('UCSD UFAD EXTERIOR',InArgs(3))) then
                  OutArgs(3)='UnderFloorAirDistributionExterior'
                endif
                if (samestring('DIRECT',InArgs(4))) then
                  OutArgs(4)='Direct'
                endif
                if (samestring('INDIRECT',InArgs(4))) then
                  OutArgs(4)='Indirect'
                endif

              CASE('USER DEFINED ROOMAIR TEMPERATURES')
                ObjectName='RoomAir:TemperaturePattern:UserDefined'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ROOMAIR TEMPERATURE PATTERN:CONSTANT GRADIENT')
                ObjectName='RoomAir:TemperaturePattern:ConstantGradient'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ROOMAIR TEMPERATURE PATTERN:TWO GRADIENT INTERPOLATION')
                ObjectName='RoomAir:TemperaturePattern:TwoGradient'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Outdoor Environment Drybulb Temperature',InArgs(8))) then
                  OutArgs(8)='OutdoorDryBulbTemperature'
                endif
                if (samestring('Zone Drybulb Temperature',InArgs(8))) then
                  OutArgs(8)='ZoneDryBulbTemperature'
                endif
                if (samestring('Delta Outdoor and Zone Temperature',InArgs(8))) then
                  OutArgs(8)='ZoneAndOutdoorTemperatureDifference'
                endif
                if (samestring('Sensible Cooling Load',InArgs(8))) then
                  OutArgs(8)='SensibleCoolingLoad'
                endif
                if (samestring('Sensible Heating Load',InArgs(8))) then
                  OutArgs(8)='SensibleHeatingLoad'
                endif

              CASE('ROOMAIR TEMPERATURE PATTERN:NON-DIMENSIONAL HEIGHT')
                ObjectName='RoomAir:TemperaturePattern:NondimensionalHeight'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ROOMAIR TEMPERATURE PATTERN:SURFACE MAPPING')
                ObjectName='RoomAir:TemperaturePattern:SurfaceMapping'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ROOMAIR NODE')
                ObjectName='RoomAir:Node'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('INLET',InArgs(2))) then
                  OutArgs(2)='Inlet'
                endif
                if (samestring('FLOOR',InArgs(2))) then
                  OutArgs(2)='Floor'
                endif
                if (samestring('CONTROL',InArgs(2))) then
                  OutArgs(2)='Control'
                endif
                if (samestring('CEILING',InArgs(2))) then
                  OutArgs(2)='Ceiling'
                endif
                if (samestring('MUNDTROOM',InArgs(2))) then
                  OutArgs(2)='MundtRoom'
                endif
                if (samestring('RETURN',InArgs(2))) then
                  OutArgs(2)='Return'
                endif

              CASE('MUNDT MODEL CONTROLS')
                ObjectName='RoomAirSettings:OneNodeDisplacementVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('UCSD DISPLACEMENT VENTILATION MODEL CONTROLS')
                ObjectName='RoomAirSettings:ThreeNodeDisplacementVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('UCSD CROSS VENTILATION MODEL CONTROLS')
                ObjectName='RoomAirSettings:CrossVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('JET',InArgs(3))) then
                  OutArgs(3)='Jet'
                endif
                if (samestring('RECIRCULATION',InArgs(3))) then
                  OutArgs(3)='Recirculation'
                endif

              CASE('UCSD UFAD INTERIOR MODEL CONTROLS')
                ObjectName='RoomAirSettings:UnderFloorAirDistributionInterior'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SWIRL',InArgs(11))) then
                  OutArgs(11)='Swirl'
                endif
                if (samestring('VARIABLE AREA',InArgs(11))) then
                  OutArgs(11)='VariableArea'
                endif
                if (samestring('DISPLACEMENT',InArgs(11))) then
                  OutArgs(11)='Displacement'
                endif

              CASE('UCSD UFAD EXTERIOR MODEL CONTROLS')
                ObjectName='RoomAirSettings:UnderFloorAirDistributionExterior'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SWIRL',InArgs(10))) then
                  OutArgs(10)='Swirl'
                endif
                if (samestring('VARIABLE AREA',InArgs(10))) then
                  OutArgs(10)='VariableArea'
                endif
                if (samestring('DISPLACEMENT',InArgs(10))) then
                  OutArgs(10)='Displacement'
                endif
                if (samestring('LINEAR BAR GRILLE',InArgs(10))) then
                  OutArgs(10)='LinearBarGrille'
                endif

              CASE('SCHEDULETYPE')
                ObjectName='ScheduleTypeLimits'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CONTINUOUS',InArgs(3))) then
                  OutArgs(3)='Continuous'
                endif
                if (samestring('DISCRETE',InArgs(3))) then
                  OutArgs(3)='Discrete'
                endif

              CASE('DAYSCHEDULE')
                ObjectName='Schedule:Day:Hourly'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYSCHEDULE:INTERVAL')
                ObjectName='Schedule:Day:Interval'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYSCHEDULE:LIST')
                ObjectName='Schedule:Day:List'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WEEKSCHEDULE')
                ObjectName='Schedule:Week:Daily'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WEEKSCHEDULE:COMPACT')
                ObjectName='Schedule:Week:Compact'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SCHEDULE')
                ObjectName='Schedule:Year'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SCHEDULE:COMPACT')
                ObjectName='Schedule:Compact'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SCHEDULE:FILE:COMMA')
                ObjectName='Schedule:File'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PEOPLE')
                ObjectName='People'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('people',InArgs(4))) then
                  OutArgs(4)='People'
                endif
                if (samestring('people/area',InArgs(4))) then
                  OutArgs(4)='People/Area'
                endif
                if (samestring('area/person',InArgs(4))) then
                  OutArgs(4)='Area/Person'
                endif

              CASE('ANGLEFACTORLIST')
                ObjectName='ComfortViewFactorAngles'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('LIGHTS')
                ObjectName='Lights'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Lighting Level',InArgs(4))) then
                  OutArgs(4)='LightingLevel'
                endif
                if (samestring('Watts/area',InArgs(4))) then
                  OutArgs(4)='Watts/Area'
                endif
                if (samestring('Watts/person',InArgs(4))) then
                  OutArgs(4)='Watts/Person'
                endif

              CASE('ELECTRIC EQUIPMENT')
                ObjectName='ElectricEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Equipment Level',InArgs(4))) then
                  OutArgs(4)='EquipmentLevel'
                endif
                if (samestring('Watts/area',InArgs(4))) then
                  OutArgs(4)='Watts/Area'
                endif
                if (samestring('Watts/person',InArgs(4))) then
                  OutArgs(4)='Watts/Person'
                endif

              CASE('GAS EQUIPMENT')
                ObjectName='GasEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Equipment Level',InArgs(4))) then
                  OutArgs(4)='EquipmentLevel'
                endif
                if (samestring('Watts/area',InArgs(4))) then
                  OutArgs(4)='Watts/Area'
                endif
                if (samestring('Watts/person',InArgs(4))) then
                  OutArgs(4)='Watts/Person'
                endif

              CASE('HOT WATER EQUIPMENT')
                ObjectName='HotWaterEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Equipment Level',InArgs(4))) then
                  OutArgs(4)='EquipmentLevel'
                endif
                if (samestring('Watts/area',InArgs(4))) then
                  OutArgs(4)='Watts/Area'
                endif
                if (samestring('Watts/person',InArgs(4))) then
                  OutArgs(4)='Watts/Person'
                endif

              CASE('STEAM EQUIPMENT')
                ObjectName='SteamEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Equipment Level',InArgs(4))) then
                  OutArgs(4)='EquipmentLevel'
                endif
                if (samestring('Watts/area',InArgs(4))) then
                  OutArgs(4)='Watts/Area'
                endif
                if (samestring('Watts/person',InArgs(4))) then
                  OutArgs(4)='Watts/Person'
                endif

              CASE('OTHER EQUIPMENT')
                ObjectName='OtherEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Equipment Level',InArgs(4))) then
                  OutArgs(4)='EquipmentLevel'
                endif
                if (samestring('Watts/area',InArgs(4))) then
                  OutArgs(4)='Watts/Area'
                endif
                if (samestring('Watts/person',InArgs(4))) then
                  OutArgs(4)='Watts/Person'
                endif

              CASE('BASEBOARD HEAT')
                ObjectName='ZoneBaseboard:OutdoorTemperatureControlled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('EXTERIORLIGHTS')
                ObjectName='Exterior:Lights'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Schedule Only',InArgs(4))) then
                  OutArgs(4)='ScheduleNameOnly'
                endif
                if (samestring('Astronomical Clock',InArgs(4))) then
                  OutArgs(4)='AstronomicalClock'
                endif

              CASE('EXTERIORFUELEQUIPMENT')
                ObjectName='Exterior:FuelEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('PurchasedHeating',InArgs(2))) then
                  OutArgs(2)='DistrictHeating'
                endif
                if (samestring('PurchasedCooling',InArgs(2))) then
                  OutArgs(2)='DistrictCooling'
                endif

              CASE('EXTERIORWATEREQUIPMENT')
                ObjectName='Exterior:WaterEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING:DETAILED')
                ObjectName='Daylighting:Controls'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING:DELIGHT')
                ObjectName='Daylighting:DELight:Controls'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING:DELIGHT:REFERENCE POINT')
                ObjectName='Daylighting:DELight:ReferencePoint'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING:DELIGHT:COMPLEX FENESTRATION')
                ObjectName='Daylighting:DELight:ComplexFenestration'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING:ILLUMINANCE MAP')
                ObjectName='Output:IlluminanceMap'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING:ILLUMINANCE MAP:STYLE')
                ObjectName='OutputControl:IlluminanceMap:Style'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('comma',InArgs(1))) then
                  OutArgs(1)='Comma'
                endif
                if (samestring('tab',InArgs(1))) then
                  OutArgs(1)='Tab'
                endif
                if (samestring('fixed',InArgs(1))) then
                  OutArgs(1)='Fixed'
                endif

              CASE('DAYLIGHTING DEVICE:TUBULAR')
                ObjectName='DaylightingDevice:Tubular'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DAYLIGHTING DEVICE:SHELF')
                ObjectName='DaylightingDevice:Shelf'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('LIGHT WELL')
                ObjectName='DaylightingDevice:LightWell'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('INFILTRATION')
                ObjectName='ZoneInfiltration'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/zone',InArgs(4))) then
                  OutArgs(4)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(4))) then
                  OutArgs(4)='Flow/Area'
                endif
                if (samestring('flow/exteriorarea',InArgs(4))) then
                  OutArgs(4)='Flow/ExteriorArea'
                endif
                if (samestring('flow/exteriorwallarea',InArgs(4))) then
                  OutArgs(4)='Flow/ExteriorWallArea'
                endif
                if (samestring('ACH',InArgs(4))) then
                  OutArgs(4)='AirChanges/Hour'
                endif

              CASE('VENTILATION')
                ObjectName='ZoneVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/zone',InArgs(4))) then
                  OutArgs(4)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(4))) then
                  OutArgs(4)='Flow/Area'
                endif
                if (samestring('flow/person',InArgs(4))) then
                  OutArgs(4)='Flow/Person'
                endif
                if (samestring('ACH',InArgs(4))) then
                  OutArgs(4)='AirChanges/Hour'
                endif

              CASE('MIXING')
                ObjectName='ZoneMixing'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/zone',InArgs(4))) then
                  OutArgs(4)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(4))) then
                  OutArgs(4)='Flow/Area'
                endif
                if (samestring('flow/person',InArgs(4))) then
                  OutArgs(4)='Flow/Person'
                endif
                if (samestring('ACH',InArgs(4))) then
                  OutArgs(4)='AirChanges/Hour'
                endif

              CASE('CROSS MIXING')
                ObjectName='ZoneCrossMixing'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/zone',InArgs(4))) then
                  OutArgs(4)='Flow/Zone'
                endif
                if (samestring('flow/person',InArgs(4))) then
                  OutArgs(4)='Flow/Person'
                endif
                if (samestring('flow/area',InArgs(4))) then
                  OutArgs(4)='Flow/Area'
                endif
                if (samestring('ACH',InArgs(4))) then
                  OutArgs(4)='AirChanges/Hour'
                endif

              CASE('EARTHTUBE')
                ObjectName='ZoneEarthtube'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Heavy and saturated',InArgs(15))) then
                  OutArgs(15)='HeavyAndSaturated'
                endif
                if (samestring('Heavy and damp',InArgs(15))) then
                  OutArgs(15)='HeavyAndDamp'
                endif
                if (samestring('Heavy and dry',InArgs(15))) then
                  OutArgs(15)='HeavyAndDry'
                endif
                if (samestring('Light and dry',InArgs(15))) then
                  OutArgs(15)='LightAndDry'
                endif

              CASE('COOLTOWER:SHOWER')
                ObjectName='ZoneCoolTower:Shower'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Water Flow Schedule',InArgs(5))) then
                  OutArgs(5)='WaterFlowSchedule'
                endif
                if (samestring('Wind-driven Flow',InArgs(5))) then
                  OutArgs(5)='WindDrivenFlow'
                endif

              CASE('THERMALCHIMNEY')
                ObjectName='ZoneThermalChimney'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SIZING PARAMETERS')
                ObjectName='Sizing:Parameters'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SIZING:OUTPUT:STYLE')
                ObjectName='OutputControl:Sizing:Style'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('comma',InArgs(1))) then
                  OutArgs(1)='Comma'
                endif
                if (samestring('tab',InArgs(1))) then
                  OutArgs(1)='Tab'
                endif
                if (samestring('fixed',InArgs(1))) then
                  OutArgs(1)='Fixed'
                endif

              CASE('ZONE SIZING')
                ObjectName='Sizing:Zone'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(6))) then
                  OutArgs(6)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(6))) then
                  OutArgs(6)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(6))) then
                  OutArgs(6)='Flow/Area'
                endif
                if (samestring('sum',InArgs(6))) then
                  OutArgs(6)='Sum'
                endif
                if (samestring('maximum',InArgs(6))) then
                  OutArgs(6)='Maximum'
                endif
                if (samestring('flow/zone',InArgs(11))) then
                  OutArgs(11)='Flow/Zone'
                endif
                if (samestring('design day',InArgs(11))) then
                  OutArgs(11)='DesignDay'
                endif
                if (samestring('design day with limit',InArgs(11))) then
                  OutArgs(11)='DesignDayWithLimit'
                endif
                if (samestring('flow/zone',InArgs(16))) then
                  OutArgs(16)='Flow/Zone'
                endif
                if (samestring('design day',InArgs(16))) then
                  OutArgs(16)='DesignDay'
                endif
                if (samestring('design day with limit',InArgs(16))) then
                  OutArgs(16)='DesignDayWithLimit'
                endif

              CASE('SYSTEM SIZING')
                ObjectName='Sizing:System'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('sensible',InArgs(2))) then
                  OutArgs(2)='Sensible'
                endif
                if (samestring('coincident',InArgs(11))) then
                  OutArgs(11)='Coincident'
                endif
                if (samestring('noncoincident',InArgs(11))) then
                  OutArgs(11)='NonCoincident'
                endif
                if (samestring('yes',InArgs(12))) then
                  OutArgs(12)='Yes'
                endif
                if (samestring('no',InArgs(12))) then
                  OutArgs(12)='No'
                endif
                if (samestring('yes',InArgs(13))) then
                  OutArgs(13)='Yes'
                endif
                if (samestring('no',InArgs(13))) then
                  OutArgs(13)='No'
                endif
                if (samestring('flow/system',InArgs(16))) then
                  OutArgs(16)='Flow/System'
                endif
                if (samestring('design day',InArgs(16))) then
                  OutArgs(16)='DesignDay'
                endif
                if (samestring('flow/system',InArgs(18))) then
                  OutArgs(18)='Flow/System'
                endif
                if (samestring('design day',InArgs(18))) then
                  OutArgs(18)='DesignDay'
                endif

              CASE('PLANT SIZING')
                ObjectName='Sizing:Plant'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('heating',InArgs(2))) then
                  OutArgs(2)='Heating'
                endif
                if (samestring('cooling',InArgs(2))) then
                  OutArgs(2)='Cooling'
                endif
                if (samestring('condenser',InArgs(2))) then
                  OutArgs(2)='Condenser'
                endif
                if (SameString(OutArgs(2),'Heating')) THEN
                  DO InNum=1,NumIDFRecords
                    IF (SameString(IDFRecords(InNum)%Name,'PLANT LOOP')) THEN
                      IF (SameString(IDFRecords(InNum)%Alphas(1),OutArgs(1))) THEN
                        IF (SameString(IDFRecords(InNum)%Alphas(2),'Steam')) THEN
                          OutArgs(2)='Steam'
                        ENDIF
                        EXIT
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF

              CASE('CURVE:BICUBIC')
                ObjectName='Curve:Bicubic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CURVE:BIQUADRATIC')
                ObjectName='Curve:Biquadratic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CURVE:TRIQUADRATIC')
                ObjectName='Curve:Triquadratic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CURVE:CUBIC')
                ObjectName='Curve:Cubic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CURVE:QUADRATIC')
                ObjectName='Curve:Quadratic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CURVE:QUADRATICLINEAR')
                ObjectName='Curve:QuadraticLinear'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CURVE:LINEAR')
                ObjectName='Curve:Linear'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('NODE LIST')
                ObjectName='NodeList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('BRANCH LIST')
                ObjectName='BranchList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONNECTOR LIST')
                ObjectName='ConnectorList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 is object type
                CALL ReplaceRenamedObjectFields(InArgs(2),OutArgs(2),ErrFlag)
 ! field 4 is object type
                CALL ReplaceRenamedObjectFields(InArgs(4),OutArgs(4),ErrFlag)

              CASE('BRANCH')
                ObjectName='Branch'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 and every 5 thereafter is object type
                do Arg=3,CurArgs,5
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                  if (samestring('ACTIVE',InArgs(Arg+4))) then
                    OutArgs(Arg+4)='Active'
                  endif
                  if (samestring('PASSIVE',InArgs(Arg+4))) then
                    OutArgs(Arg+4)='Passive'
                  endif
                  if (samestring('SERIESACTIVE',InArgs(Arg+4))) then
                    OutArgs(Arg+4)='SeriesActive'
                  endif
                  if (samestring('BYPASS',InArgs(Arg+4))) then
                    OutArgs(Arg+4)='Bypass'
                  endif
                enddo

              CASE('PIPE')
                ObjectName='Pipe:Adiabatic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PIPE:STEAM')
                ObjectName='Pipe:Adiabatic:Steam'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PIPE:INTERIOR')
                ObjectName='Pipe:Indoor'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('ZONE',InArgs(6))) then
                  OutArgs(6)='Zone'
                endif
                if (samestring('SCHEDULE',InArgs(6))) then
                  OutArgs(6)='Schedule'
                endif

              CASE('PIPE:EXTERIOR')
                ObjectName='Pipe:Outdoor'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PIPE:UNDERGROUND')
                ObjectName='Pipe:Underground'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DUCT')
                ObjectName='Duct'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PLANT LOOP')
                ObjectName='PlantLoop'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('OPTIMAL',InArgs(18))) then
                  OutArgs(18)='Optimal'
                endif
                if (samestring('SEQUENTIAL',InArgs(18))) then
                  OutArgs(18)='Sequential'
                endif
                if (samestring('UNIFORM',InArgs(18))) then
                  OutArgs(18)='Uniform'
                endif
                if (samestring('SingleSetPoint',InArgs(20))) then
                  OutArgs(20)='SingleSetpoint'
                endif
                if (samestring('DualSetPointDeadband',InArgs(20))) then
                  OutArgs(20)='DualSetpointDeadband'
                endif
                if (samestring('COMMON PIPE',InArgs(21))) then
                  OutArgs(21)='CommonPipe'
                endif
                if (samestring('TWO WAY COMMON PIPE',InArgs(21))) then
                  OutArgs(21)='TwoWayCommonPipe'
                endif
                if (samestring('NONE',InArgs(21))) then
                  OutArgs(21)='None'
                endif

              CASE('CONDENSER LOOP')
                ObjectName='CondenserLoop'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('WATER',InArgs(2))) then
                  OutArgs(2)='Water'
                endif

              CASE('CONNECTION COMPONENT:PLANTLOOP')
                ObjectName='PlantLoopConnection'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONNECTION COMPONENT:PLANTLOOP:CONTROLLED')
                ObjectName='PlantLoopConnection:Controlled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PLANT OPERATION SCHEMES')
                ObjectName='PlantEquipmentOperationSchemes'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 3 thereafter is object type/choice
                do Arg=2,CurArgs,3
                  if (samestring('Cooling Load Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:CoolingLoad'
                  endif
                  if (samestring('Heating Load Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:HeatingLoad'
                  endif
                  if (samestring('Uncontrolled Loop Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:Uncontrolled'
                  endif
                  if (samestring('Component SetPoint Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:ComponentSetpoint'
                  endif
                enddo

              CASE('CONDENSER OPERATION SCHEMES')
                ObjectName='CondenserEquipmentOperationSchemes'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 3 thereafter is object type/choice
                do Arg=2,CurArgs,3
                  if (samestring('Uncontrolled Loop Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:Uncontrolled'
                  endif
                  if (samestring('Cooling Load Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:CoolingLoad'
                  endif
                  if (samestring('Heating Load Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:HeatingLoad'
                  endif
                  if (samestring('Outdoor Drybulb Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorDryBulb'
                  endif
                  if (samestring('Outdoor Wetbulb Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorWetBulb'
                  endif
                  if (samestring('Outdoor Rhpercent Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorRelativeHumidity'
                  endif
                  if (samestring('Outdoor Dewpoint Range Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorDewpoint'
                  endif
                  if (samestring('Outdoor Drybulb Temperature Difference Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorDryBulbDifference'
                  endif
                  if (samestring('Outdoor Wetbulb Temperature Difference Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorWetBulbDifference'
                  endif
                  if (samestring('Outdoor Dewpoint Temperature Difference Based Operation',InArgs(Arg))) then
                    OutArgs(Arg)='PlantEquipmentOperation:OutdoorDewpointDifference'
                  endif
                enddo

              CASE('UNCONTROLLED LOOP OPERATION')
                ObjectName='PlantEquipmentOperation:Uncontrolled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COOLING LOAD RANGE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:CoolingLoad'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HEATING LOAD RANGE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:HeatingLoad'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTDOOR DRYBULB RANGE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorDryBulb'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTDOOR WETBULB RANGE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorWetBulb'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTDOOR RHPERCENT RANGE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorRelativeHumidity'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COMPONENT SETPOINT BASED OPERATION')
                ObjectName='PlantEquipmentOperation:ComponentSetpoint'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 6 thereafter is object type
                do Arg=2,CurArgs,6
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                  if (samestring('HEATING',InArgs(Arg+5))) then
                    OutArgs(Arg+5)='Heating'
                  endif
                  if (samestring('COOLING',InArgs(Arg+5))) then
                    OutArgs(Arg+5)='Cooling'
                  endif
                  if (samestring('DUAL',InArgs(Arg+5))) then
                    OutArgs(Arg+5)='Dual'
                  endif
                enddo

              CASE('OUTDOOR DRYBULB TEMPERATURE DIFFERENCE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorDryBulbDifference'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do Arg=5,CurArgs,4
                  if (samestring('ON',InArgs(Arg))) then
                    OutArgs(Arg)='On'
                  endif
                  if (samestring('OFF',InArgs(Arg))) then
                    OutArgs(Arg)='Off'
                  endif
                enddo

              CASE('OUTDOOR WETBULB TEMPERATURE DIFFERENCE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorWetBulbDifference'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do Arg=5,CurArgs,4
                  if (samestring('ON',InArgs(Arg))) then
                    OutArgs(Arg)='On'
                  endif
                  if (samestring('OFF',InArgs(Arg))) then
                    OutArgs(Arg)='Off'
                  endif
                enddo

              CASE('OUTDOOR DEWPOINT RANGE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorDewpoint'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTDOOR DEWPOINT TEMPERATURE DIFFERENCE BASED OPERATION')
                ObjectName='PlantEquipmentOperation:OutdoorDewpointDifference'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do Arg=5,CurArgs,4
                  if (samestring('ON',InArgs(Arg))) then
                    OutArgs(Arg)='On'
                  endif
                  if (samestring('OFF',InArgs(Arg))) then
                    OutArgs(Arg)='Off'
                  endif
                enddo

              CASE('PLANT EQUIPMENT LIST')
                ObjectName='PlantEquipmentList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 2nd thereafter is object type
                do Arg=2,CurArgs,2
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                enddo

              CASE('CONDENSER EQUIPMENT LIST')
                ObjectName='CondenserEquipmentList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 2nd thereafter is object type
                do Arg=2,CurArgs,2
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                enddo

              CASE('SPLITTER')
                ObjectName='Connector:Splitter'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('MIXER')
                ObjectName='Connector:Mixer'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('VALVE:TEMPERING')
                ObjectName='TemperingValve'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIR PRIMARY LOOP')
                ObjectName='AirLoopHVAC'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONTROLLER LIST')
                ObjectName='AirLoopHVAC:ControllerList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 2 thereafter is object type/choice
                do Arg=2,CurArgs,2
                  if (samestring('CONTROLLER:SIMPLE',InArgs(Arg))) then
                    OutArgs(Arg)='Controller:WaterCoil'
                  endif
                  if (samestring('CONTROLLER:OUTSIDE AIR',InArgs(Arg))) then
                    OutArgs(Arg)='Controller:OutdoorAir'
                  endif
                enddo

              CASE('AIR LOOP EQUIPMENT LIST')
                ObjectName='AirLoopHVAC:OutdoorAirSystem:EquipmentList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 2 thereafter is object type
                do Arg=2,CurArgs,2
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                enddo

              CASE('OUTSIDE AIR SYSTEM')
                ObjectName='AirLoopHVAC:OutdoorAirSystem'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTSIDE AIR NODE')
                ObjectName='OutdoorAir:Node'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTSIDE AIR INLET NODE LIST')
                ObjectName='OutdoorAir:NodeList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('OUTSIDE AIR MIXER')
                ObjectName='OutdoorAir:Mixer'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK SIMULATION')
                ObjectName='AirflowNetwork:SimulationControl'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('MULTIZONE WITH DISTRIBUTION',InArgs(2))) then
                  OutArgs(2)='MultizoneWithDistribution'
                endif
                if (samestring('MULTIZONE WITHOUT DISTRIBUTION',InArgs(2))) then
                  OutArgs(2)='MultizoneWithoutDistribution'
                endif
                if (samestring('MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION',InArgs(2))) then
                  OutArgs(2)='MultizoneWithDistributionOnlyDuringFanOperation'
                endif
                if (samestring('NO MULTIZONE OR DISTRIBUTION',InArgs(2))) then
                  OutArgs(2)='NoMultizoneOrDistribution'
                endif
                if (samestring('INPUT',InArgs(3))) then
                  OutArgs(3)='Input'
                endif
                if (samestring('SURFACE-AVERAGE CALCULATION',InArgs(3))) then
                  OutArgs(3)='SurfaceAverageCalculation'
                endif
                if (samestring('LOWRISE',InArgs(5))) then
                  OutArgs(5)='LowRise'
                endif
                if (samestring('HIGHRISE',InArgs(5))) then
                  OutArgs(5)='HighRise'
                endif
                if (samestring('Linear Initialization Method',InArgs(7))) then
                  OutArgs(7)='LinearInitializationMethod'
                endif
                if (samestring('Zero Node Pressures',InArgs(7))) then
                  OutArgs(7)='ZeroNodePressures'
                endif

              CASE('AIRFLOWNETWORK:MULTIZONE:ZONE')
                ObjectName='AirflowNetwork:MultiZone:Zone'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMPERATURE',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('ENTHALPIC',InArgs(2))) then
                  OutArgs(2)='Enthalpy'
                endif
                if (samestring('CONSTANT',InArgs(2))) then
                  OutArgs(2)='Constant'
                endif
                if (samestring('NOVENT',InArgs(2))) then
                  OutArgs(2)='NoVent'
                endif

              CASE('AIRFLOWNETWORK:MULTIZONE:SURFACE')
                ObjectName='AirflowNetwork:MultiZone:Surface'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMPERATURE',InArgs(5))) then
                  OutArgs(5)='Temperature'
                endif
                if (samestring('ENTHALPIC',InArgs(5))) then
                  OutArgs(5)='Enthalpy'
                endif
                if (samestring('CONSTANT,',InArgs(5))) then
                  OutArgs(5)='Constant'
                endif
                if (samestring('NOVENT',InArgs(5))) then
                  OutArgs(5)='NoVent'
                endif
                if (samestring('ZONELEVEL',InArgs(5))) then
                  OutArgs(5)='ZoneLevel'
                endif
                if (samestring('ADJACENT TEMPERATURE',InArgs(5))) then
                  OutArgs(5)='AdjacentTemperature'
                endif
                if (samestring('ADJACENT ENTHALPIC',InArgs(5))) then
                  OutArgs(5)='AdjacentEnthalpy'
                endif

              CASE('AIRFLOWNETWORK:MULTIZONE:REFERENCE CRACK CONDITIONS')
                ObjectName='AirflowNetwork:MultiZone:ReferenceCrackConditions'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:SURFACE CRACK DATA')
                ObjectName='AirflowNetwork:MultiZone:Surface:Crack'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:SURFACE EFFECTIVE LEAKAGE AREA')
                ObjectName='AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:COMPONENT DETAILED OPENING')
                ObjectName='AirflowNetwork:MultiZone:Component:DetailedOpening'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Non-pivoted',InArgs(4))) then
                  OutArgs(4)='NonPivoted'
                endif
                if (samestring('Horizontally pivoted',InArgs(4))) then
                  OutArgs(4)='HorizontallyPivoted'
                endif

              CASE('AIRFLOWNETWORK:MULTIZONE:COMPONENT SIMPLE OPENING')
                ObjectName='AirflowNetwork:MultiZone:Component:SimpleOpening'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:COMPONENT ZONE EXHAUST FAN')
                ObjectName='AirflowNetwork:MultiZone:Component:ZoneExhaustFan'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:SITE WIND CONDITIONS')
                ObjectName='AirflowNetwork:MultiZone:SiteWindConditions'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:EXTERNAL NODE')
                ObjectName='AirflowNetwork:MultiZone:ExternalNode'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:WIND PRESSURE COEFFICIENT ARRAY')
                ObjectName='AirflowNetwork:MultiZone:WindPressureCoefficientArray'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:MULTIZONE:WIND PRESSURE COEFFICIENT VALUES')
                ObjectName='AirflowNetwork:MultiZone:WindPressureCoefficientValues'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:DISTRIBUTION:NODE')
                ObjectName='AirflowNetwork:Distribution:Node'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('MIXER',InArgs(3))) then
                  OutArgs(3)='AirLoopHVAC:ZoneMixer'
                endif
                if (samestring('SPLITTER',InArgs(3))) then
                  OutArgs(3)='AirLoopHVAC:ZoneSplitter'
                endif
                if (samestring('OUTSIDE AIR SYSTEM',InArgs(3))) then
                  OutArgs(3)='AirLoopHVAC:OutdoorAirSystem'
                endif
                if (samestring('OA MIXER OUTSIDE AIR STREAM NODE',InArgs(3))) then
                  OutArgs(3)='OAMixerOutdoorAirStreamNode'
                endif
                if (samestring('OTHER',InArgs(3))) then
                  OutArgs(3)='Other'
                endif

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT LEAK')
                ObjectName='AirflowNetwork:Distribution:Component:Leak'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT LEAKAGE RATIO')
                ObjectName='AirflowNetwork:Distribution:Component:LeakageRatio'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DUCT')
                ObjectName='AirflowNetwork:Distribution:Component:Duct'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT CONSTANT VOLUME FAN')
                ObjectName='AirflowNetwork:Distribution:Component:ConstantVolumeFan'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(2))) then
                  OutArgs(2)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(2))) then
                  OutArgs(2)='Fan:ConstantVolume'
                endif

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT COIL')
                ObjectName='AirflowNetwork:Distribution:Component:Coil'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 is object type/choice
                if (samestring('COIL:DX:COOLINGBYPASSFACTOREMPIRICAL',InArgs(2))) then
                  OutArgs(2)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:GAS:HEATING',InArgs(2))) then
                  OutArgs(2)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(2))) then
                  OutArgs(2)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:DX:HEATINGEMPIRICAL',InArgs(2))) then
                  OutArgs(2)='Coil:Heating:DX:SingleSpeed'
                endif
                if (samestring('COIL:Water:Cooling',InArgs(2))) then
                  OutArgs(2)='Coil:Cooling:Water'
                endif
                if (samestring('COIL:Water:SimpleHeating',InArgs(2))) then
                  OutArgs(2)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Water:DetailedFlatCooling',InArgs(2))) then
                  OutArgs(2)='Coil:Cooling:Water:DetailedGeometry'
                endif
                if (samestring('COIL:DX:MultiMode:CoolingEmpirical',InArgs(2))) then
                  OutArgs(2)='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                endif
                if (samestring('COIL:DX:MultiSpeed:Cooling',InArgs(2))) then
                  OutArgs(2)='Coil:Cooling:DX:MultiSpeed'
                endif
                if (samestring('COIL:DX:MultiSpeed:Heating',InArgs(2))) then
                  OutArgs(2)='Coil:Heating:DX:MultiSpeed'
                endif

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT TERMINAL UNIT')
                ObjectName='AirflowNetwork:Distribution:Component:TerminalUnit'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 is object type/choice
                if (samestring('SINGLE DUCT:CONST VOLUME:REHEAT',InArgs(2))) then
                  OutArgs(2)='AirTerminal:SingleDuct:ConstantVolume:Reheat'
                endif

              CASE('AIRFLOWNETWORK:DISTRIBUTION:COMPONENT CONSTANT PRESSURE DROP')
                ObjectName='AirflowNetwork:Distribution:Component:ConstantPressureDrop'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
                ObjectName='AirflowNetwork:Distribution:Linkage'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER LIST')
                ObjectName='AvailabilityManagerAssignmentList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 and every 2 thereafter is object type/choice
                do Arg=2,CurArgs,2
                if (samestring('SYSTEM AVAILABILITY MANAGER:SCHEDULED',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:Scheduled'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:SCHEDULED ON',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:ScheduledOn'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:SCHEDULED OFF',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:ScheduledOff'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:NIGHT CYCLE',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:NightCycle'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:DIFFERENTIAL THERMOSTAT',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:DifferentialThermostat'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:HIGH TEMPERATURE TURN OFF',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:HighTemperatureTurnOff'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:HIGH TEMPERATURE TURN ON',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:HighTemperatureTurnOn'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:LOW TEMPERATURE TURN OFF',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:LowTemperatureTurnOff'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:LOW TEMPERATURE TURN ON',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:LowTemperatureTurnOn'
                  endif
                  if (samestring('SYSTEM AVAILABILITY MANAGER:NIGHT VENTILATION',InArgs(Arg))) then
                    OutArgs(Arg)='AvailabilityManager:NightVentilation'
                  endif
                enddo

              CASE('SYSTEM AVAILABILITY MANAGER:SCHEDULED')
                ObjectName='AvailabilityManager:Scheduled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:SCHEDULED ON')
                ObjectName='AvailabilityManager:ScheduledOn'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:SCHEDULED OFF')
                ObjectName='AvailabilityManager:ScheduledOff'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:NIGHT CYCLE')
                ObjectName='AvailabilityManager:NightCycle'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Stay Off',InArgs(4))) then
                  OutArgs(4)='StayOff'
                endif
                if (samestring('Cycle On Any',InArgs(4))) then
                  OutArgs(4)='CycleOnAny'
                endif
                if (samestring('Cycle On Control Zone',InArgs(4))) then
                  OutArgs(4)='CycleOnControlZone'
                endif
                if (samestring('Cycle On Any - Zone Fans Only',InArgs(4))) then
                  OutArgs(4)='CycleOnAnyZoneFansOnly'
                endif

              CASE('SYSTEM AVAILABILITY MANAGER:DIFFERENTIAL THERMOSTAT')
                ObjectName='AvailabilityManager:DifferentialThermostat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:HIGH TEMPERATURE TURN OFF')
                ObjectName='AvailabilityManager:HighTemperatureTurnOff'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:HIGH TEMPERATURE TURN ON')
                ObjectName='AvailabilityManager:HighTemperatureTurnOn'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:LOW TEMPERATURE TURN OFF')
                ObjectName='AvailabilityManager:LowTemperatureTurnOff'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:LOW TEMPERATURE TURN ON')
                ObjectName='AvailabilityManager:LowTemperatureTurnOn'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:NIGHT VENTILATION')
                ObjectName='AvailabilityManager:NightVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SYSTEM AVAILABILITY MANAGER:HYBRID VENTILATION CONTROL')
                ObjectName='AvailabilityManager:HybridVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SET POINT MANAGER:SCHEDULED')
                ObjectName='SetpointManager:Scheduled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('MAXTEMP',InArgs(2))) then
                  OutArgs(2)='MaximumTemperature'
                endif
                if (samestring('MINTEMP',InArgs(2))) then
                  OutArgs(2)='MinimumTemperature'
                endif
                if (samestring('HUMRAT',InArgs(2))) then
                  OutArgs(2)='HumidityRatio'
                endif
                if (samestring('MAXHUMRAT',InArgs(2))) then
                  OutArgs(2)='MaximumHumidityRatio'
                endif
                if (samestring('MINHUMRAT',InArgs(2))) then
                  OutArgs(2)='MinimumHumidityRatio'
                endif
                if (samestring('MASSFLOW',InArgs(2))) then
                  OutArgs(2)='MassFlowRate'
                endif
                if (samestring('MAXMASSFLOW',InArgs(2))) then
                  OutArgs(2)='MaximumMassFlowRate'
                endif
                if (samestring('MINMASSFLOW',InArgs(2))) then
                  OutArgs(2)='MinimumMassFlowRate'
                endif

              CASE('SET POINT MANAGER:SCHEDULED:DUALSETPOINT')
                ObjectName='SetpointManager:Scheduled:DualSetpoint'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif

              CASE('SET POINT MANAGER:OUTSIDE AIR')
                ObjectName='SetpointManager:OutdoorAirReset'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif

              CASE('SET POINT MANAGER:SINGLE ZONE REHEAT')
                ObjectName='SetpointManager:SingleZone:Reheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif

              CASE('SET POINT MANAGER:SINGLE ZONE HEATING')
                ObjectName='SetpointManager:SingleZone:Heating'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif

              CASE('SET POINT MANAGER:SINGLE ZONE COOLING')
                ObjectName='SetpointManager:SingleZone:Cooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif

              CASE('SET POINT MANAGER:SINGLE ZONE MIN HUM')
                ObjectName='SetpointManager:SingleZone:Humidity:Minimum'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SET POINT MANAGER:SINGLE ZONE MAX HUM')
                ObjectName='SetpointManager:SingleZone:Humidity:Maximum'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SET POINT MANAGER:MIXED AIR')
                ObjectName='SetpointManager:MixedAir'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif

              CASE('SET POINT MANAGER:OUTSIDE AIR PRETREAT')
                ObjectName='SetpointManager:OutdoorAirPretreat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('HUMRAT',InArgs(2))) then
                  OutArgs(2)='HumidityRatio'
                endif
                if (samestring('MAXHUMRAT',InArgs(2)) .or. samestring('HUMRATMAX',InArgs(2))) then
                  OutArgs(2)='MaximumHumidityRatio'
                endif
                if (samestring('MINHUMRAT',InArgs(2)) .or. samestring('HUMRATMIN',InArgs(2))) then
                  OutArgs(2)='MinimumHumidityRatio'
                endif

              CASE('SET POINT MANAGER:WARMEST')
                ObjectName='SetpointManager:Warmest'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('MaxTemp',InArgs(6))) then
                  OutArgs(6)='MaximumTemperature'
                endif

              CASE('SET POINT MANAGER:COLDEST')
                ObjectName='SetpointManager:Coldest'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('MinTemp',InArgs(6))) then
                  OutArgs(6)='MinimumTemperature'
                endif

              CASE('SET POINT MANAGER:RETURN AIR BYPASS FLOW')
                ObjectName='SetpointManager:ReturnAirBypassFlow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('FLOW',InArgs(2))) then
                  OutArgs(2)='Flow'
                endif

              CASE('SET POINT MANAGER:WARMEST TEMP FLOW')
                ObjectName='SetpointManager:WarmestTemperatureFlow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('TempFirst',InArgs(6))) then
                  OutArgs(6)='TemperatureFirst'
                endif

              CASE('CONTROLLER:SIMPLE')
                ObjectName='Controller:WaterCoil'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMP',InArgs(2))) then
                  OutArgs(2)='Temperature'
                endif
                if (samestring('HUMRAT',InArgs(2))) then
                  OutArgs(2)='HumidityRatio'
                endif
                if (samestring('TEMPandHUMRAT',InArgs(2))) then
                  OutArgs(2)='TemperatureAndHumidityRatio'
                endif

              CASE('CONTROLLER:OUTSIDE AIR')
                ObjectName='Controller:OutdoorAir'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1)=InArgs(1)
                OutArgs(2)=InArgs(16)
                OutArgs(3)=InArgs(17)
                OutArgs(4)=InArgs(7)
                OutArgs(5)=InArgs(8)
                OutArgs(6)=InArgs(9)
                OutArgs(7)=InArgs(10)
                If (.not. SameString(InArgs(2),'No Economizer')) THEN
                  If (InArgs(15) == Blank) THEN
                    If (SameString(InArgs(3),'Return Air Temp Limit') .and. SameString(InArgs(4),'No Return Air Enthalpy Limit'))THEN
                      OutArgs(8)='DifferentialDryBulb'
                    ElseIf (SameString(InArgs(4),'Return Air Enthalpy Limit') .and. SameString(InArgs(3),'No Return Air Temp Limit')) THEN
                      OutArgs(8) = 'DifferentialEnthalpy'
                    ElseIf (SameString(InArgs(3),'Return Air Temp Limit') .and. SameString(InArgs(4),'Return Air Enthalpy Limit')) THEN
                      OutArgs(8) = 'DifferentialDryBulbAndEnthalpy'
                    ElseIf (SameString(InArgs(3),'No Return Air Temp Limit') .and. SameString(InArgs(4),'No Return Air Enthalpy Limit')) THEN
                      If (InArgs(11) /= Blank .and. InArgs(13) /= Blank .and. InArgs(14) == Blank) THEN
                        OutArgs(8) = 'FixedDryBulb'
                      ElseIf (InArgs(11) == Blank .and. InArgs(13) /= Blank .and. InArgs(14) /= Blank) THEN
                        OutArgs(8) = 'FixedEnthalpy'
                      ElseIf (InArgs(11) /= Blank .and. InArgs(13) /= Blank .and. InArgs(14) /= Blank) Then
                        OutArgs(8) = 'FixedDewPointAndDryBulb'
                      ElseIf (InArgs(11) /= Blank .and. InArgs(13) == Blank .and. InArgs(14) == Blank) Then
                        OutArgs(8) = 'FixedDryBulb'
                      ElseIf (InArgs(11) == Blank .and. InArgs(13) /= Blank .and. InArgs(14) == Blank) Then
                        OutArgs(8) = 'FixedEnthalpy'
                      ElseIf (InArgs(11) /= Blank .and. InArgs(13) == Blank .and. InArgs(14) /= Blank) Then
                        OutArgs(8) = 'FixedDewPointAndDryBulb'
                      ElseIf (InArgs(11) == Blank .and. InArgs(13) == Blank .and. InArgs(14) /= Blank) THEN
                        OutArgs(8) = 'FixedDewPointAndDryBulb'
                      ElseIf (InArgs(11) == Blank .and. InArgs(13) == Blank .and. InArgs(14) == Blank) Then
                        OutArgs(8) = 'NoEconomizer'
                      End IF
                    End IF
                  ElseIf (InArgs(15) /= Blank) THEN
                    If (SameString(InArgs(3),'Return Air Temp Limit') .and. SameString(InArgs(4),'No Return Air Enthalpy Limit')) Then
                      OutArgs(8) = 'DifferentialDryBulb'
                    ElseIf (SameString(InArgs(4),'Return Air Enthalpy Limit') .and. SameString(InArgs(3),'No Return Air Temp Limit')) Then
                      OutArgs(8) = 'DifferentialEnthalpy'
                    ElseIf (SameString(InArgs(3),'Return Air Temp Limit') .and. SameString(InArgs(4),'Return Air Enthalpy Limit')) Then
                      OutArgs(8) = 'DifferentialDryBulbAndEnthalpy'
                    ElseIf (SameString(InArgs(3),'No Return Air Temp Limit') .and. SameString(InArgs(4),'No Return Air Enthalpy Limit')) Then
                      OutArgs(8) = 'ElectronicEnthalpy'
                    Endif
                  Else
                    OutArgs(8) = 'NoEconomizer'
                  ENDIF
                ELSE
                  OutArgs(8) = 'NoEconomizer'
                EndIF
                IF (SameString(InArgs(2),'Bypass')) THEN
                  OutArgs(9)='MinimumFlowWithBypass'
                ELSE
                  OutArgs(9)='ModulateFlow'
                ENDIF
                OutArgs(10)=InArgs(11)
                OutArgs(11:13)=InArgs(13:15)
                OutArgs(14)=InArgs(12)
                OutArgs(15:16)=InArgs(5:6)
                if (samestring('NO LOCKOUT',OutArgs(15))) then
                  OutArgs(15)='NoLockout'
                endif
                if (samestring('LOCKOUT WITH HEATING',OutArgs(15))) then
                  OutArgs(15)='LockoutWithHeating'
                endif
                if (samestring('LOCKOUT WITH COMPRESSOR',OutArgs(15))) then
                  OutArgs(15)='LockoutWithCompressor'
                endif
                if (samestring('FIXED MINIMUM',OutArgs(16))) then
                  OutArgs(16)='FixedMinimum'
                endif
                if (samestring('PROPORTIONAL MINIMUM',OutArgs(16))) then
                  OutArgs(16)='ProportionalMinimum'
                endif
                OutArgs(17)=InArgs(18)
                OutArgs(18:19)=Blank
                OutArgs(20:25)=InArgs(19:24)
                if (samestring('YES',OutArgs(22))) then
                  OutArgs(22)='Yes'
                endif
                if (samestring('NO',OutArgs(22))) then
                  OutArgs(22)='No'
                endif
                DO CurArgs=25,1,-1
                  IF (OutArgs(CurArgs) /= Blank) EXIT
                ENDDO

              CASE('VENTILATION:MECHANICAL')
                ObjectName='Controller:MechanicalVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CONTROLLER:STAND ALONE ERV')
                ObjectName='ZoneHVAC:EnergyRecoveryVentilator:Controller'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('EXHAUST AIR TEMP LIMIT',InArgs(7))) then
                  OutArgs(7)='ExhaustAirTemperatureLimit'
                endif
                if (samestring('NO EXHAUST AIR TEMP LIMIT',InArgs(7))) then
                  OutArgs(7)='NoExhaustAirTemperatureLimit'
                endif
                if (samestring('EXHAUST AIR ENTHALPY LIMIT',InArgs(8))) then
                  OutArgs(8)='ExhaustAirEnthalpyLimit'
                endif
                if (samestring('NO EXHAUST AIR ENTHALPY LIMIT',InArgs(8))) then
                  OutArgs(8)='NoExhaustAirEnthalpyLimit'
                endif
                if (samestring('YES',InArgs(10))) then
                  OutArgs(10)='Yes'
                endif
                if (samestring('NO',InArgs(10))) then
                  OutArgs(10)='No'
                endif

              CASE('CONTROLLED ZONE EQUIP CONFIGURATION')
                ObjectName='ZoneHVAC:EquipmentConnections'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE EQUIPMENT LIST')
                ObjectName='ZoneHVAC:EquipmentList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2  and every 4 thereafter is object type/choice
                do Arg=2,CurArgs,4
                  if (samestring('AIR DISTRIBUTION UNIT',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:AirDistributionUnit'
                  endif
                  if (samestring('DIRECT AIR',InArgs(Arg))) then
                    OutArgs(Arg)='AirTerminal:SingleDuct:Uncontrolled'
                  endif
                  if (samestring('AIR CONDITIONER:WINDOW:CYCLING',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:WindowAirConditioner'
                  endif
                  if (samestring('PACKAGEDTERMINAL:HEATPUMP:AIRTOAIR',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:PackagedTerminalHeatPump'
                  endif
                  if (samestring('PACKAGEDTERMINAL:AIRCONDITIONER',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:PackagedTerminalAirConditioner'
                  endif
                  if (samestring('FAN COIL UNIT:4 PIPE',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:FourPipeFanCoil'
                  endif
                  if (samestring('UNIT VENTILATOR',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:UnitVentilator'
                  endif
                  if (samestring('UNIT HEATER',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:UnitHeater'
                  endif
                  if (samestring('PURCHASED AIR',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:IdealLoadsAirSystem'
                  endif
                  if (samestring('BASEBOARD HEATER:WATER',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:Baseboard:RadiantConvective:Water'
                  endif
                  if (samestring('BASEBOARD HEATER:WATER:CONVECTIVE',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:Baseboard:Convective:Water'
                  endif
                  if (samestring('BASEBOARD HEATER:ELECTRIC:CONVECTIVE',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:Baseboard:Convective:Electric'
                  endif
                  if (samestring('HIGH TEMP RADIANT SYSTEM',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:HighTemperatureRadiant'
                  endif
                  if (samestring('LOW TEMP RADIANT SYSTEM:HYDRONIC',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:LowTemperatureRadiant:VariableFlow'
                  endif
                  if (samestring('LOW TEMP RADIANT SYSTEM:CONSTANT FLOW',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
                  endif
                  if (samestring('LOW TEMP RADIANT SYSTEM:ELECTRIC',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:LowTemperatureRadiant:Electric'
                  endif
                  if (samestring('VENTILATED SLAB',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:VentilatedSlab'
                  endif
                  if (samestring('ZONE EXHAUST FAN',InArgs(Arg))) then
                    OutArgs(Arg)='Fan:ZoneExhaust'
                  endif
                  if (samestring('ENERGY RECOVERY VENTILATOR:STAND ALONE',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHVAC:EnergyRecoveryVentilator'
                  endif
                  if (samestring('HEAT PUMP:WATER HEATER',InArgs(Arg))) then
                    OutArgs(Arg)='WaterHeater:HeatPump'
                  endif
                enddo

              CASE('AIR DISTRIBUTION UNIT')
                ObjectName='ZoneHVAC:AirDistributionUnit'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 is object type/choice
                if (samestring('DUAL DUCT:CONST VOLUME',InArgs(3))) then
                  OutArgs(3)='AirTerminal:DualDuct:ConstantVolume'
                endif
                if (samestring('DUAL DUCT:VAV',InArgs(3))) then
                  OutArgs(3)='AirTerminal:DualDuct:VAV'
                endif
                if (samestring('SINGLE DUCT:CONST VOLUME:REHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:ConstantVolume:Reheat'
                endif
                if (samestring('SINGLE DUCT:VAV:REHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:VAV:Reheat'
                endif
                if (samestring('SINGLE DUCT:VAV:NOREHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:VAV:NoReheat'
                endif
                if (samestring('SINGLE DUCT:SERIES PIU:REHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:SeriesPIU:Reheat'
                endif
                if (samestring('SINGLE DUCT:PARALLEL PIU:REHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:ParallelPIU:Reheat'
                endif
                if (samestring('SINGLE DUCT:CONST VOLUME:4 PIPE INDUC',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction'
                endif
                if (samestring('SINGLE DUCT:VAV:REHEAT:VS FAN',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan'
                endif
                if (samestring('SINGLE DUCT:VAVHEATANDCOOL:REHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat'
                endif
                if (samestring('SINGLE DUCT:VAVHEATANDCOOL:NOREHEAT',InArgs(3))) then
                  OutArgs(3)='AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat'
                endif

              CASE('PURCHASED AIR')
                ObjectName='ZoneHVAC:IdealLoadsAirSystem'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('LIMIT',InArgs(7))) then
                  OutArgs(7)='Limit'
                endif
                if (samestring('NO LIMIT',InArgs(7))) then
                  OutArgs(7)='NoLimit'
                endif
                if (samestring('NO LIMIT',InArgs(9))) then
                  OutArgs(9)='NoLimit'
                endif
                if (samestring('OUTSIDE AIR',InArgs(11))) then
                  OutArgs(11)='OutdoorAir'
                endif
                if (samestring('NO OUTSIDE AIR',InArgs(11))) then
                  OutArgs(11)='NoOutdoorAir'
                endif

              CASE('FAN COIL UNIT:4 PIPE')
                ObjectName='ZoneHVAC:FourPipeFanCoil'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 21 is object type/choice
                if (samestring('COIL:Water:Cooling',InArgs(21))) then
                  OutArgs(21)='Coil:Cooling:Water'
                endif
                if (samestring('COIL:Water:DetailedFlatCooling',InArgs(21))) then
                  OutArgs(21)='Coil:Cooling:Water:DetailedGeometry'
                endif
                if (samestring('COIL:Water:CoolingHeatExchangerAssisted',InArgs(21))) then
                  OutArgs(21)='CoilSystem:Cooling:Water:HeatExchangerAssisted'
                endif

              CASE('AIR CONDITIONER:WINDOW:CYCLING')
                ObjectName='ZoneHVAC:WindowAirConditioner'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('blow through',InArgs(13))) then
                  OutArgs(13)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(13))) then
                  OutArgs(13)='DrawThrough'
                endif
 ! field 15 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(15))) then
                  OutArgs(15)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(15))) then
                  OutArgs(15)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif

              CASE('PACKAGEDTERMINAL:AIRCONDITIONER')
                ObjectName='ZoneHVAC:PackagedTerminalAirConditioner'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 12 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(12))) then
                  OutArgs(12)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(12))) then
                  OutArgs(12)='Fan:ConstantVolume'
                endif
 ! field 14 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(14))) then
                  OutArgs(14)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(14))) then
                  OutArgs(14)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:WATER:SIMPLEHEATING',InArgs(14))) then
                  OutArgs(14)='Coil:Heating:Water'
                endif
                if (samestring('COIL:STEAM:AIRHEATING',InArgs(14))) then
                  OutArgs(14)='Coil:Heating:Steam'
                endif
 ! field 16 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(16))) then
                  OutArgs(16)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(16))) then
                  OutArgs(16)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
                if (samestring('blow through',InArgs(18))) then
                  OutArgs(18)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(18))) then
                  OutArgs(18)='DrawThrough'
                endif

              CASE('PACKAGEDTERMINAL:HEATPUMP:AIRTOAIR')
                ObjectName='ZoneHVAC:PackagedTerminalHeatPump'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 12 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(12))) then
                  OutArgs(12)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(12))) then
                  OutArgs(12)='Fan:ConstantVolume'
                endif
 ! field 14 is object type/choice
                if (samestring('COIL:DX:HeatingEmpirical',InArgs(14))) then
                  OutArgs(14)='Coil:Heating:DX:SingleSpeed'
                endif
 ! field 18 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(18))) then
                  OutArgs(18)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(18))) then
                  OutArgs(18)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
 ! field 21 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(21))) then
                  OutArgs(21)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(21))) then
                  OutArgs(21)='Coil:Heating:Electric'
                endif
                if (samestring('blow through',InArgs(25))) then
                  OutArgs(25)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(25))) then
                  OutArgs(25)='DrawThrough'
                endif

              CASE('ENERGY RECOVERY VENTILATOR:STAND ALONE')
                ObjectName='ZoneHVAC:EnergyRecoveryVentilator'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('UNIT VENTILATOR')
                ObjectName='ZoneHVAC:UnitVentilator'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('FIXED AMOUNT',InArgs(4))) then
                  OutArgs(4)='FixedAmount'
                endif
                if (samestring('VARIABLE PERCENT',InArgs(4))) then
                  OutArgs(4)='VariablePercent'
                endif
                if (samestring('FIXED TEMPERATURE',InArgs(4))) then
                  OutArgs(4)='FixedTemperature'
                endif
                if (samestring('NONE',InArgs(16))) then
                  OutArgs(16)='None'
                endif
                if (samestring('HEATING',InArgs(16))) then
                  OutArgs(16)='Heating'
                endif
                if (samestring('COOLING',InArgs(16))) then
                  OutArgs(16)='Cooling'
                endif
                if (samestring('HEATING AND COOLING',InArgs(16))) then
                  OutArgs(16)='HeatingAndCooling'
                endif
 ! field 17 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(17))) then
                  OutArgs(17)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(17))) then
                  OutArgs(17)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(17))) then
                  OutArgs(17)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(17))) then
                  OutArgs(17)='Coil:Heating:Steam'
                endif
 ! field 21 is object type/choice
                if (samestring('COIL:Water:Cooling',InArgs(21))) then
                  OutArgs(21)='Coil:Cooling:Water'
                endif
                if (samestring('COIL:Water:DetailedFlatCooling',InArgs(21))) then
                  OutArgs(21)='Coil:Cooling:Water:DetailedGeometry'
                endif
                if (samestring('COIL:Water:CoolingHeatExchangerAssisted',InArgs(21))) then
                  OutArgs(21)='CoilSystem:Cooling:Water:HeatExchangerAssisted'
                endif

              CASE('UNIT HEATER')
                ObjectName='ZoneHVAC:UnitHeater'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('ONOFF',InArgs(7))) then
                  OutArgs(7)='OnOff'
                endif
                if (samestring('CONTINUOUS',InArgs(7))) then
                  OutArgs(7)='Continuous'
                endif
 ! field 9 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(9))) then
                  OutArgs(9)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(9))) then
                  OutArgs(9)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(9))) then
                  OutArgs(9)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(9))) then
                  OutArgs(9)='Coil:Heating:Steam'
                endif

              CASE('COMPRESSOR RACK:REFRIGERATED CASE')
                ObjectName='Refrigeration:CompressorRack'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Air Cooled',InArgs(7))) then
                  OutArgs(7)='AirCooled'
                endif
                if (samestring('Evap Cooled',InArgs(7))) then
                  OutArgs(7)='EvaporativelyCooled'
                endif
                if (samestring('Water Cooled',InArgs(7))) then
                  OutArgs(7)='WaterCooled'
                endif
                if (samestring('Variable Flow',InArgs(10))) then
                  OutArgs(10)='VariableFlow'
                endif
                if (samestring('Constant Flow',InArgs(10))) then
                  OutArgs(10)='ConstantFlow'
                endif

              CASE('CASE:REFRIGERATED')
                ObjectName='Refrigeration:Case'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Case Temperature Method',InArgs(11))) then
                  OutArgs(11)='CaseTemperatureMethod'
                endif
                if (samestring('Relative Humidity Method',InArgs(11))) then
                  OutArgs(11)='RelativeHumidityMethod'
                endif
                if (samestring('Dewpoint Method',InArgs(11))) then
                  OutArgs(11)='DewpointMethod'
                endif
                if (samestring('Dewpoint Method',InArgs(20))) then
                  OutArgs(20)='DewpointMethod'
                endif
                if (samestring('Heat Balance Method',InArgs(20))) then
                  OutArgs(20)='HeatBalanceMethod'
                endif
                if (samestring('Off-Cycle',InArgs(25))) then
                  OutArgs(25)='OffCycle'
                endif
                if (samestring('Hot-Gas',InArgs(25))) then
                  OutArgs(25)='HotGas'
                endif
                if (samestring('Hot-Gas with Temperature Termination',InArgs(25))) then
                  OutArgs(25)='HotGasWithTemperatureTermination'
                endif
                if (samestring('Electric with Temperature Termination',InArgs(25))) then
                  OutArgs(25)='ElectricWithTemperatureTermination'
                endif
                if (samestring('Case Temperature Method',InArgs(28))) then
                  OutArgs(28)='CaseTemperatureMethod'
                endif
                if (samestring('Relative Humidity Method',InArgs(28))) then
                  OutArgs(28)='RelativeHumidityMethod'
                endif
                if (samestring('Dewpoint Method',InArgs(28))) then
                  OutArgs(28)='DewpointMethod'
                endif

              CASE('REFRIGERATION:SYSTEM')
                ObjectName='Refrigeration:System'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('LIST:REFRIGERATION:COMPRESSORS')
                ObjectName='Refrigeration:CompressorList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('LIST:REFRIGERATION:CASES')
                ObjectName='Refrigeration:CaseList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REFRIGERATION:CONDENSER:AIRCOOLED')
                ObjectName='Refrigeration:Condenser:AirCooled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REFRIGERATION:CONDENSER:EVAPORATIVECOOLED')
                ObjectName='Refrigeration:Condenser:EvaporativeCooled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REFRIGERATION:CONDENSER:WATERCOOLED')
                ObjectName='Refrigeration:Condenser:WaterCooled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REFRIGERATION:SUBCOOLER')
                ObjectName='Refrigeration:Subcooler'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REFRIGERATION:COMPRESSOR')
                ObjectName='Refrigeration:Compressor'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FURNACE:HEATONLY')
                ObjectName='AirLoopHVAC:Unitary:Furnace:HeatOnly'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 10 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(10))) then
                  OutArgs(10)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(10))) then
                  OutArgs(10)='Fan:ConstantVolume'
                endif
                if (samestring('blow through',InArgs(12))) then
                  OutArgs(12)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(12))) then
                  OutArgs(12)='DrawThrough'
                endif
 ! field 13 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Electric'
                endif

              CASE('FURNACE:HEATCOOL')
                ObjectName='AirLoopHVAC:Unitary:Furnace:HeatCool'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 12 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(12))) then
                  OutArgs(12)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(12))) then
                  OutArgs(12)='Fan:ConstantVolume'
                endif
                if (samestring('blow through',InArgs(14))) then
                  OutArgs(14)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(14))) then
                  OutArgs(14)='DrawThrough'
                endif
 ! field 15 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(15))) then
                  OutArgs(15)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(15))) then
                  OutArgs(15)='Coil:Heating:Electric'
                endif
 ! field 17 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(17))) then
                  OutArgs(17)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(17))) then
                  OutArgs(17)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
                if (samestring('NONE',InArgs(19))) then
                  OutArgs(19)='None'
                endif
                if (samestring('MULTIMODE',InArgs(19))) then
                  OutArgs(19)='Multimode'
                endif
                if (samestring('COOLREHEAT',InArgs(19))) then
                  OutArgs(19)='CoolReheat'
                endif
 ! field 20 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:DESUPERHEATER:HEATING',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Desuperheater'
                endif

              CASE('UNITARYSYSTEM:HEATONLY')
                ObjectName='AirLoopHVAC:UnitaryHeatOnly'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 10 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(10))) then
                  OutArgs(10)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(10))) then
                  OutArgs(10)='Fan:ConstantVolume'
                endif
                if (samestring('blow through',InArgs(12))) then
                  OutArgs(12)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(12))) then
                  OutArgs(12)='DrawThrough'
                endif
 ! field 13 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Electric'
                endif

              CASE('UNITARYSYSTEM:HEATCOOL')
                ObjectName='AirLoopHVAC:UnitaryHeatCool'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 12 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(12))) then
                  OutArgs(12)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(12))) then
                  OutArgs(12)='Fan:ConstantVolume'
                endif
                if (samestring('blow through',InArgs(14))) then
                  OutArgs(14)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(14))) then
                  OutArgs(14)='DrawThrough'
                endif
 ! field 15 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(15))) then
                  OutArgs(15)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(15))) then
                  OutArgs(15)='Coil:Heating:Electric'
                endif
 ! field 17 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(17))) then
                  OutArgs(17)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(17))) then
                  OutArgs(17)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
                if (samestring('NONE',InArgs(19))) then
                  OutArgs(19)='None'
                endif
                if (samestring('MULTIMODE',InArgs(19))) then
                  OutArgs(19)='Multimode'
                endif
                if (samestring('COOLREHEAT',InArgs(19))) then
                  OutArgs(19)='CoolReheat'
                endif
 ! field 20 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:DESUPERHEATER:HEATING',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Desuperheater'
                endif

              CASE('UNITARYSYSTEM:HEATPUMP:AIRTOAIR')
                ObjectName='AirLoopHVAC:UnitaryHeatPump:AirToAir'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 10 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(10))) then
                  OutArgs(10)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(10))) then
                  OutArgs(10)='Fan:ConstantVolume'
                endif
 ! field 12 is object type/choice
                if (samestring('COIL:DX:HeatingEmpirical',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:DX:SingleSpeed'
                endif
 ! field 14 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(14))) then
                  OutArgs(14)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(14))) then
                  OutArgs(14)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
 ! field 16 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(16))) then
                  OutArgs(16)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(16))) then
                  OutArgs(16)='Coil:Heating:Electric'
                endif
                if (samestring('blow through',InArgs(20))) then
                  OutArgs(20)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(20))) then
                  OutArgs(20)='DrawThrough'
                endif

              CASE('UNITARYSYSTEM:HEATPUMP:WATERTOAIR')
                ObjectName='AirLoopHVAC:UnitaryHeatPump:WaterToAir'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 8 is object type/choice
                if (samestring('FAN:Simple:OnOff',InArgs(8))) then
                  OutArgs(8)='Fan:OnOff'
                endif
 ! field 10 is object type/choice
                if (samestring('COIL:WaterToAirHP:ParameterEstimation:Heating',InArgs(10))) then
                  OutArgs(10)='Coil:Heating:WaterToAirHeatPump:ParameterEstimation'
                endif
                if (samestring('COIL:WaterToAirHP:EquationFit:Heating',InArgs(10))) then
                  OutArgs(10)='Coil:Heating:WaterToAirHeatPump:EquationFit'
                endif
 ! field 13 is object type/choice
                if (samestring('COIL:WaterToAirHP:ParameterEstimation:Cooling',InArgs(13))) then
                  OutArgs(13)='Coil:Cooling:WaterToAirHeatPump:ParameterEstimation'
                endif
                if (samestring('COIL:WaterToAirHP:EquationFit:Cooling',InArgs(13))) then
                  OutArgs(13)='Coil:Cooling:WaterToAirHeatPump:EquationFit'
                endif
 ! field 20 is object type/choice
                if (samestring('COIL:Gas:Heating',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(20))) then
                  OutArgs(20)='Coil:Heating:Electric'
                endif
                if (samestring('blow through',InArgs(25))) then
                  OutArgs(25)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(25))) then
                  OutArgs(25)='DrawThrough'
                endif

              CASE('UNITARYSYSTEM:VAV:CHANGEOVERBYPASS')
                ObjectName='AirLoopHVAC:UnitaryHeatCool:VAVChangeoverBypass'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 15 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(15))) then
                  OutArgs(15)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(15))) then
                  OutArgs(15)='Fan:ConstantVolume'
                endif
                if (samestring('blow through',InArgs(17))) then
                  OutArgs(17)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(17))) then
                  OutArgs(17)='DrawThrough'
                endif
 ! field 19 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(19))) then
                  OutArgs(19)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(19))) then
                  OutArgs(19)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
                if (samestring('COIL:DX:MultiMode:CoolingEmpirical',InArgs(19))) then
                  OutArgs(19)='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                endif
 ! field 21 is object type/choice
                if (samestring('COIL:DX:HEATINGEMPIRICAL',InArgs(21))) then
                  OutArgs(21)='Coil:Heating:DX:SingleSpeed'
                endif
                if (samestring('COIL:GAS:HEATING',InArgs(21))) then
                  OutArgs(21)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(21))) then
                  OutArgs(21)='Coil:Heating:Electric'
                endif
                if (samestring('Cooling Priority',InArgs(23))) then
                  OutArgs(23)='CoolingPriority'
                endif
                if (samestring('Heating Priority',InArgs(23))) then
                  OutArgs(23)='HeatingPriority'
                endif
                if (samestring('Zone Priority',InArgs(23))) then
                  OutArgs(23)='ZonePriority'
                endif

              CASE('UNITARYSYSTEM:MULTISPEEDHEATPUMP:AIRTOAIR')
                ObjectName='AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 7 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(7))) then
                  OutArgs(7)='Fan:OnOff'
                endif
                if (samestring('FAN:SIMPLE:CONSTVOLUME',InArgs(7))) then
                  OutArgs(7)='Fan:ConstantVolume'
                endif
                if (samestring('blow through',InArgs(9))) then
                  OutArgs(9)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(9))) then
                  OutArgs(9)='DrawThrough'
                endif
 ! field 11 is object type/choice
                if (samestring('COIL:DX:MultiSpeed:Heating',InArgs(11))) then
                  OutArgs(11)='Coil:Heating:DX:MultiSpeed'
                endif
 ! field 14 is object type/choice
                if (samestring('COIL:DX:MultiSpeed:Cooling',InArgs(14))) then
                  OutArgs(14)='Coil:Cooling:DX:MultiSpeed'
                endif
 ! field 16 is object type/choice
                if (samestring('COIL:GAS:HEATING',InArgs(16))) then
                  OutArgs(16)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:ELECTRIC:HEATING',InArgs(16))) then
                  OutArgs(16)='Coil:Heating:Electric'
                endif

              CASE('DXSYSTEM:AIRLOOP')
                ObjectName='AirLoopHVAC:UnitaryCoolOnly'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 6 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(6))) then
                  OutArgs(6)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:CoolingHeatExchangerAssisted',InArgs(6))) then
                  OutArgs(6)='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                endif
                if (samestring('COIL:DX:MultiSpeed:CoolingEmpirical',InArgs(6))) then
                  OutArgs(6)='Coil:Cooling:DX:TwoSpeed'
                endif
                if (samestring('COIL:DX:MultiMode:CoolingEmpirical',InArgs(6))) then
                  OutArgs(6)='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                endif

              CASE('DIRECT AIR')
                ObjectName='AirTerminal:SingleDuct:Uncontrolled'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DUAL DUCT:CONST VOLUME')
                ObjectName='AirTerminal:DualDuct:ConstantVolume'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DUAL DUCT:VAV')
                ObjectName='AirTerminal:DualDuct:VAV'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE DUCT:CONST VOLUME:REHEAT')
                ObjectName='AirTerminal:SingleDuct:ConstantVolume:Reheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 7 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(7))) then
                  OutArgs(7)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(7))) then
                  OutArgs(7)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(7))) then
                  OutArgs(7)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(7))) then
                  OutArgs(7)='Coil:Heating:Steam'
                endif

              CASE('SINGLE DUCT:VAV:REHEAT')
                ObjectName='AirTerminal:SingleDuct:VAV:Reheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 8 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Steam'
                endif
                if (samestring('NORMAL',InArgs(14))) then
                  OutArgs(14)='Normal'
                endif
                if (samestring('REVERSE ACTION',InArgs(14))) then
                  OutArgs(14)='Reverse'
                endif

              CASE('SINGLE DUCT:VAV:REHEAT:VS FAN')
                ObjectName='AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 10 is object type/choice
                if (samestring('FAN:SIMPLE:VariableVolume',InArgs(10))) then
                  OutArgs(10)='Fan:VariableVolume'
                endif
 ! field 12 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Steam'
                endif

              CASE('SINGLE DUCT:VAVHEATANDCOOL:REHEAT')
                ObjectName='AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 8 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(8))) then
                  OutArgs(8)='Coil:Heating:Steam'
                endif

              CASE('SINGLE DUCT:VAV:NOREHEAT')
                ObjectName='AirTerminal:SingleDuct:VAV:NoReheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE DUCT:VAVHEATANDCOOL:NOREHEAT')
                ObjectName='AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE DUCT:SERIES PIU:REHEAT')
                ObjectName='AirTerminal:SingleDuct:SeriesPIU:Reheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 12 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Steam'
                endif

              CASE('SINGLE DUCT:PARALLEL PIU:REHEAT')
                ObjectName='AirTerminal:SingleDuct:ParallelPIU:Reheat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 13 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(13))) then
                  OutArgs(13)='Coil:Heating:Steam'
                endif

              CASE('SINGLE DUCT:CONST VOLUME:4 PIPE INDUC')
                ObjectName='AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 10 is object type/choice
                if (samestring('COIL:Water:SimpleHeating',InArgs(10))) then
                  OutArgs(10)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(10))) then
                  OutArgs(10)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(10))) then
                  OutArgs(10)='Coil:Heating:Gas'
                endif
 ! field 15 is object type/choice
                if (samestring('COIL:Water:Cooling',InArgs(15))) then
                  OutArgs(15)='Coil:Cooling:Water'
                endif
                if (samestring('COIL:Water:DetailedFlatCooling',InArgs(15))) then
                  OutArgs(15)='Coil:Cooling:Water:DetailedGeometry'
                endif

              CASE('BASEBOARD HEATER:WATER')
                ObjectName='ZoneHVAC:Baseboard:RadiantConvective:Water'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('BASEBOARD HEATER:WATER:CONVECTIVE')
                ObjectName='ZoneHVAC:Baseboard:Convective:Water'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('BASEBOARD HEATER:ELECTRIC:CONVECTIVE')
                ObjectName='ZoneHVAC:Baseboard:Convective:Electric'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('LOW TEMP RADIANT SYSTEM:HYDRONIC')
                ObjectName='ZoneHVAC:LowTemperatureRadiant:VariableFlow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('MAT',InArgs(7))) then
                  OutArgs(7)='MeanAirTemperature'
                endif
                if (samestring('MRT',InArgs(7))) then
                  OutArgs(7)='MeanRadiantTemperature'
                endif
                if (samestring('OPERATIVE',InArgs(7))) then
                  OutArgs(7)='OperativeTemperature'
                endif
                if (samestring('ODB',InArgs(7))) then
                  OutArgs(7)='OutdoorDryBulbTemperature'
                endif
                if (samestring('OWB',InArgs(7))) then
                  OutArgs(7)='OutdoorWetBulbTemperature'
                endif

              CASE('LOW TEMP RADIANT SYSTEM:CONSTANT FLOW')
                ObjectName='ZoneHVAC:LowTemperatureRadiant:ConstantFlow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('MAT',InArgs(7))) then
                  OutArgs(7)='MeanAirTemperature'
                endif
                if (samestring('MRT',InArgs(7))) then
                  OutArgs(7)='MeanRadiantTemperature'
                endif
                if (samestring('OPERATIVE',InArgs(7))) then
                  OutArgs(7)='OperativeTemperature'
                endif
                if (samestring('ODB',InArgs(7))) then
                  OutArgs(7)='OutdoorDryBulbTemperature'
                endif
                if (samestring('OWB',InArgs(7))) then
                  OutArgs(7)='OutdoorWetBulbTemperature'
                endif

              CASE('LOW TEMP RADIANT SYSTEM:ELECTRIC')
                ObjectName='ZoneHVAC:LowTemperatureRadiant:Electric'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('MAT',InArgs(6))) then
                  OutArgs(6)='MeanAirTemperature'
                endif
                if (samestring('MRT',InArgs(6))) then
                  OutArgs(6)='MeanRadiantTemperature'
                endif
                if (samestring('OPERATIVE',InArgs(6))) then
                  OutArgs(6)='OperativeTemperature'
                endif
                if (samestring('ODB',InArgs(6))) then
                  OutArgs(6)='OutdoorDryBulbTemperature'
                endif
                if (samestring('OWB',InArgs(6))) then
                  OutArgs(6)='OutdoorWetBulbTemperature'
                endif

              CASE('RADIANT SYSTEM SURFACE GROUP')
                ObjectName='ZoneHVAC:LowTemperatureRadiant:SurfaceGroup'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HIGH TEMP RADIANT SYSTEM')
                ObjectName='ZoneHVAC:HighTemperatureRadiant'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('GAS',InArgs(5))) then
                  OutArgs(5)='Gas'
                endif
                if (samestring('ELECTRIC',InArgs(5))) then
                  OutArgs(5)='Electric'
                endif
                if (samestring('MAT',InArgs(10))) then
                  OutArgs(10)='MeanAirTemperature'
                endif
                if (samestring('MRT',InArgs(10))) then
                  OutArgs(10)='MeanRadiantTemperature'
                endif
                if (samestring('OPERATIVE',InArgs(10))) then
                  OutArgs(10)='OperativeTemperature'
                endif
                if (samestring('MATSP',InArgs(10))) then
                  OutArgs(10)='MeanAirTemperatureSetpoint'
                endif
                if (samestring('MRTSP',InArgs(10))) then
                  OutArgs(10)='MeanRadiantTemperatureSetpoint'
                endif
                if (samestring('OPERATIVESP',InArgs(10))) then
                  OutArgs(10)='OperativeTemperatureSetpoint'
                endif

              CASE('VENTILATED SLAB')  ! new in V3.0
                ObjectName='ZoneHVAC:VentilatedSlab'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('VARIABLE PERCENT',InArgs(6))) then
                  OutArgs(6)='VariablePercent'
                endif
                if (samestring('FIXED TEMPERATURE',InArgs(6))) then
                  OutArgs(6)='FixedTemperature'
                endif
                if (samestring('FIXED AMOUNT',InArgs(6))) then
                  OutArgs(6)='FixedAmount'
                endif
                if (samestring('Slab Only',InArgs(14))) then
                  OutArgs(14)='SlabOnly'
                endif
                if (samestring('Slab and Zone',InArgs(14))) then
                  OutArgs(14)='SlabAndZone'
                endif
                if (samestring('MAT',InArgs(15))) then
                  OutArgs(15)='MeanAirTemperature'
                endif
                if (samestring('MRT',InArgs(15))) then
                  OutArgs(15)='MeanRadiantTemperature'
                endif
                if (samestring('OPT',InArgs(15))) then
                  OutArgs(15)='OperativeTemperature'
                endif
                if (samestring('ODB',InArgs(15))) then
                  OutArgs(15)='OutdoorDryBulbTemperature'
                endif
                if (samestring('OWD',InArgs(15))) then
                  OutArgs(15)='OutdoorWetBulbTemperature'
                endif
                if (samestring('SUR',InArgs(15))) then
                  OutArgs(15)='SurfaceTemperature'
                endif
                if (samestring('DPTZ',InArgs(15))) then
                  OutArgs(15)='ZoneAirDewPointTemperature'
                endif
                if (samestring('NONE',InArgs(32))) then
                  OutArgs(32)='None'
                endif
                if (samestring('HEATING',InArgs(32))) then
                  OutArgs(32)='Heating'
                endif
                if (samestring('COOLING',InArgs(32))) then
                  OutArgs(32)='Cooling'
                endif
                if (samestring('HEATING AND COOLING',InArgs(32))) then
                  OutArgs(32)='HeatingAndCooling'
                endif
 ! field 33 is object type
                if (samestring('COIL:Water:SimpleHeating',InArgs(33))) then
                  OutArgs(33)='Coil:Heating:Water'
                endif
                if (samestring('COIL:Electric:Heating',InArgs(33))) then
                  OutArgs(33)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(33))) then
                  OutArgs(33)='Coil:Heating:Gas'
                endif
                if (samestring('COIL:Steam:AirHeating',InArgs(33))) then
                  OutArgs(33)='Coil:Heating:Steam'
                endif
 ! field 36 is object type/choice
                if (samestring('COIL:Water:Cooling',InArgs(36))) then
                  OutArgs(36)='Coil:Cooling:Water'
                endif
                if (samestring('COIL:Water:DetailedFlatCooling',InArgs(36))) then
                  OutArgs(36)='Coil:Cooling:Water:DetailedGeometry'
                endif
                if (samestring('COIL:Water:CoolingHeatExchangerAssisted',InArgs(36))) then
                  OutArgs(36)='CoilSystem:Cooling:Water:HeatExchangerAssisted'
                endif

              CASE('ZONE CONTROL:HUMIDISTAT')
                ObjectName='ZoneControl:Humidistat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE CONTROL:THERMOSTATIC')
                ObjectName='ZoneControl:Thermostat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 4 and every 2 thereafter is object type
                do Arg=4,CurArgs,2
                  if (samestring('Single Heating Setpoint',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:SingleHeating'
                  endif
                  if (samestring('Single Cooling SetPoint',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:SingleCooling'
                  endif
                  if (samestring('Single Heating Cooling Setpoint',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:SingleHeatingOrCooling'
                  endif
                  if (samestring('Dual Setpoint with Deadband',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:DualSetpoint'
                  endif
                enddo

              CASE('ZONE CONTROL:THERMOSTATIC:OPERATIVE TEMPERATURE')
                ObjectName='ZoneControl:Thermostat:OperativeTemperature'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CONSTANT',InArgs(2))) then
                  OutArgs(2)='Constant'
                endif
                if (samestring('SCHEDULED',InArgs(2))) then
                  OutArgs(2)='Scheduled'
                endif

              CASE('ZONE CONTROL:THERMAL COMFORT')
                ObjectName='ZoneControl:Thermostat:ThermalComfort'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Specific Object',InArgs(3))) then
                  OutArgs(3)='SpecificObject'
                endif
                if (samestring('Object Average',InArgs(3))) then
                  OutArgs(3)='ObjectAverage'
                endif
                if (samestring('People Average',InArgs(3))) then
                  OutArgs(3)='PeopleAverage'
                endif
 ! field 8 and every 2 thereafter is object type/choice
                do Arg=8,CurArgs,2
                  if (samestring('Single Thermal Comfort Heating Setpoint:Fanger',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating'
                  endif
                  if (samestring('Single Thermal Comfort Cooling SetPoint:Fanger',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling'
                  endif
                  if (samestring('Single Thermal Comfort Heating Cooling Setpoint:Fanger',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling'
                  endif
                  if (samestring('Dual Thermal Comfort Setpoint with Deadband:Fanger',InArgs(Arg))) then
                    OutArgs(Arg)='ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint'
                  endif
                enddo

              CASE('SINGLE HEATING SETPOINT')
                ObjectName='ThermostatSetpoint:SingleHeating'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE COOLING SETPOINT')
                ObjectName='ThermostatSetpoint:SingleCooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE HEATING COOLING SETPOINT')
                ObjectName='ThermostatSetpoint:SingleHeatingOrCooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DUAL SETPOINT WITH DEADBAND')
                ObjectName='ThermostatSetpoint:DualSetpoint'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE THERMAL COMFORT HEATING SETPOINT:FANGER')
                ObjectName='ThermostatSetpoint:ThermalComfort:Fanger:SingleHeating'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE THERMAL COMFORT COOLING SETPOINT:FANGER')
                ObjectName='ThermostatSetpoint:ThermalComfort:Fanger:SingleCooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SINGLE THERMAL COMFORT HEATING COOLING SETPOINT:FANGER')
                ObjectName='ThermostatSetpoint:ThermalComfort:Fanger:SingleHeatingOrCooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DUAL THERMAL COMFORT SETPOINT WITH DEADBAND:FANGER')
                ObjectName='ThermostatSetpoint:ThermalComfort:Fanger:DualSetpoint'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE SUPPLY AIR PATH')
                ObjectName='AirLoopHVAC:SupplyPath'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 and every 2 thereafter is object type
                do Arg=3,CurArgs,2
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                enddo

              CASE('ZONE RETURN AIR PATH')
                ObjectName='AirLoopHVAC:ReturnPath'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 and every 2 thereafter is object type
                do Arg=3,CurArgs,2
                  CALL ReplaceRenamedObjectFields(InArgs(Arg),OutArgs(Arg),ErrFlag)
                enddo

              CASE('ZONE RETURN PLENUM')
                ObjectName='AirLoopHVAC:ReturnPlenum'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE SUPPLY PLENUM')
                ObjectName='AirLoopHVAC:SupplyPlenum'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE SPLITTER')
                ObjectName='AirLoopHVAC:ZoneSplitter'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE MIXER')
                ObjectName='AirLoopHVAC:ZoneMixer'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PLANT LOAD PROFILE')
                ObjectName='LoadProfile:Plant'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SOLAR COLLECTOR PARAMETERS:FLAT PLATE')
                ObjectName='SolarCollectorPerformance:FlatPlate'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('WATER',InArgs(3))) then
                  OutArgs(3)='Water'
                endif
                if (samestring('INLET',InArgs(5))) then
                  OutArgs(5)='Inlet'
                endif
                if (samestring('AVERAGE',InArgs(5))) then
                  OutArgs(5)='Average'
                endif
                if (samestring('OUTLET',InArgs(5))) then
                  OutArgs(5)='Outlet'
                endif

              CASE('SOLAR COLLECTOR:FLAT PLATE')
                ObjectName='SolarCollector:FlatPlate:Water'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SOLARCOLLECTOR:FLATPLATE:PHOTOVOLTAICTHERMAL')
                ObjectName='SolarCollector:FlatPlate:PhotovoltaicThermal'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SOLARCOLLECTORPERFORMANCE:PHOTOVOLTAICTHERMAL:SIMPLE')
                ObjectName='SolarCollectorPerformance:PhotovoltaicThermal:Simple'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('SOLAR COLLECTOR:UNGLAZED TRANSPIRED')
                ObjectName='SolarCollector:UnglazedTranspired'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Kutscher 1994',InArgs(17))) then
                  OutArgs(17)='Kutscher1994'
                endif
                if (samestring('Van Decker Hollands Brunger 2001',InArgs(17))) then
                  OutArgs(17)='VanDeckerHollandsBrunger2001'
                endif

              CASE('SOLAR COLLECTOR:UNGLAZED TRANSPIRED:MULTISYSTEM')
                ObjectName='SolarCollector:UnglazedTranspired:Multisystem'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('BOILER:SIMPLE')
                ObjectName='Boiler:HotWater'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:4)=InArgs(1:4)
                OutArgs(5)=trim(InArgs(1))//' Efficiency Curve'
                OutArgs(6:10)=InArgs(5:9)
                OutArgs(11:15)=InArgs(13:17)
                CurArgs=15
                DO Arg=15,1,-1
                  IF (OutArgs(Arg) /= ' ') EXIT
                  CurArgs=CurArgs-1
                ENDDO
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                CALL GetNewObjectDefInIDD('Curve:Quadratic',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=OutArgs(5)
                OutArgs(2:4)=InArgs(10:12)
                OutArgs(5)='0'
                OutArgs(6)='1'
                CurArgs=6
                CALL WriteOutIDFLines(DifLfn,'Curve:Quadratic',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                Written=.true.
                !CYCLE

              CASE('BOILER:STEAM')
                ObjectName='Boiler:Steam'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('BOILER:SPARK:SBOILER')
                ObjectName='Boiler:HotWater:SPARK'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CHILLER:ELECTRIC')
                ObjectName='Chiller:Electric'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('AIR COOLED',InArgs(2))) then
                  OutArgs(2)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(2))) then
                  OutArgs(2)='WaterCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(2))) then
                  OutArgs(2)='EvaporativelyCooled'
                endif

              CASE('CHILLER:ELECTRIC:EIR')
                ObjectName='Chiller:Electric:EIR'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('AIR COOLED',InArgs(19))) then
                  OutArgs(19)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(19))) then
                  OutArgs(19)='WaterCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(19))) then
                  OutArgs(19)='EvaporativelyCooled'
                endif

              CASE('CHILLER:ELECTRIC:REFORMULATEDEIR')
                ObjectName='Chiller:Electric:ReformulatedEIR'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('CHILLER:COMBUSTION TURBINE')
                ObjectName='Chiller:CombustionTurbine'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('AIR COOLED',InArgs(2))) then
                  OutArgs(2)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(2))) then
                  OutArgs(2)='WaterCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(2))) then
                  OutArgs(2)='EvaporativelyCooled'
                endif

              CASE('CHILLER:ABSORPTION')
                ObjectName='Chiller:Absorption'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Hot Water',InArgs(24))) then
                  OutArgs(24)='HotWater'
                endif

              CASE('CHILLER:INDIRECTABSORPTION')
                ObjectName='Chiller:Absorption:Indirect'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Hot Water',InArgs(26))) then
                  OutArgs(26)='HotWater'
                endif

              CASE('CHILLER:DIRECT FIRED ABSORPTION')
                ObjectName='ChillerHeater:Absorption:DirectFired'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('LEAVING-CONDENSER',InArgs(29))) then
                  OutArgs(29)='LeavingCondenser'
                endif
                if (samestring('ENTERING-CONDENSER',InArgs(29))) then
                  OutArgs(29)='EnteringCondenser'
                endif
                if (samestring('AIR-COOLED',InArgs(30))) then
                  OutArgs(30)='AirCooled'
                endif
                if (samestring('WATER-COOLED',InArgs(30))) then
                  OutArgs(30)='WaterCooled'
                endif

              CASE('CHILLER:CONST COP')
                ObjectName='Chiller:ConstantCOP'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('AIR COOLED',InArgs(10))) then
                  OutArgs(10)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(10))) then
                  OutArgs(10)='WaterCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(10))) then
                  OutArgs(10)='EvaporativelyCooled'
                endif

              CASE('CHILLER:ENGINEDRIVEN')
                ObjectName='Chiller:EngineDriven'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('AIR COOLED',InArgs(2))) then
                  OutArgs(2)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(2))) then
                  OutArgs(2)='WaterCooled'
                endif
                if (samestring('Evap COOLED',InArgs(2))) then
                  OutArgs(2)='EvaporativelyCooled'
                endif

              CASE('CHILLER:SPARK:ECHILLER')
                ObjectName='Chiller:Electric:SPARK'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('AIR COOLED',InArgs(2))) then
                  OutArgs(2)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(2))) then
                  OutArgs(2)='WaterCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(2))) then
                  OutArgs(2)='EvaporativelyCooled'
                endif
                if (samestring('REFRIGERANT 12',InArgs(7))) then
                  OutArgs(7)='Refrigerant12'
                endif
                if (samestring('REFRIGERANT 134a',InArgs(7))) then
                  OutArgs(7)='Refrigerant134a'
                endif
                if (samestring('REFRIGERANT 114',InArgs(7))) then
                  OutArgs(7)='Refrigerant114'
                endif
                if (samestring('REFRIGERANT 22',InArgs(7))) then
                  OutArgs(7)='Refrigerant22'
                endif
                if (samestring('REFRIGERANT 502',InArgs(7))) then
                  OutArgs(7)='Refrigerant502'
                endif
                if (samestring('REFRIGERANT 717',InArgs(7))) then
                  OutArgs(7)='Refrigerant717'
                endif

              CASE('HEATPUMP:WATERTOWATER:EQUATIONFIT:HEATING')
                ObjectName='HeatPump:WaterToWater:EquationFit:Heating'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HEATPUMP:WATERTOWATER:EQUATIONFIT:COOLING')
                ObjectName='HeatPump:WaterToWater:EquationFit:Cooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:COOLING')
                ObjectName='HeatPump:WaterToWater:ParameterEstimation:Cooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HEATPUMP:WATERTOWATER:PARAMETERESTIMATION:HEATING')
                ObjectName='HeatPump:WaterToWater:ParameterEstimation:Heating'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PURCHASED:CHILLED WATER')
                ObjectName='DistrictCooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PURCHASED:HOT WATER')
                ObjectName='DistrictHeating'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('THERMAL STORAGE:ICE:SIMPLE')
                ObjectName='ThermalStorage:Ice:Simple'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('IOC Internal',InArgs(2))) then
                  OutArgs(2)='IceOnCoilInternal'
                endif
                if (samestring('IOC External',InArgs(2))) then
                  OutArgs(2)='IceOnCoilExternal'
                endif

              CASE('THERMAL STORAGE:ICE:DETAILED')
                ObjectName='ThermalStorage:Ice:Detailed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 6 is object type
                CALL ReplaceRenamedObjectFields(InArgs(6),OutArgs(6),ErrFlag)
 ! field 8 is object type
                CALL ReplaceRenamedObjectFields(InArgs(8),OutArgs(8),ErrFlag)

              CASE('THERMALSTORAGE:CHILLEDWATER:MIXED')
                ObjectName='ThermalStorage:ChilledWater:Mixed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Exterior',InArgs(7))) then
                  OutArgs(7)='Outdoors'
                endif

              CASE('THERMALSTORAGE:CHILLEDWATER:STRATIFIED')
                ObjectName='ThermalStorage:ChilledWater:Stratified'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Exterior',InArgs(11))) then
                  OutArgs(11)='Outdoors'
                endif

              CASE('WATER HEATER:MIXED')
                ObjectName='WaterHeater:Mixed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CYCLE',InArgs(6))) then
                  OutArgs(6)='Cycle'
                endif
                if (samestring('MODULATE',InArgs(6))) then
                  OutArgs(6)='Modulate'
                endif
                if (samestring('PurchasedHeating',InArgs(11))) then
                  OutArgs(11)='DistrictHeating'
                endif
                if (samestring('PurchasedHeating',InArgs(15))) then
                  OutArgs(15)='DistrictHeating'
                endif
                if (samestring('PurchasedHeating',InArgs(18))) then
                  OutArgs(18)='DistrictHeating'
                endif
                if (samestring('SCHEDULE',InArgs(20))) then
                  OutArgs(20)='Schedule'
                endif
                if (samestring('ZONE',InArgs(20))) then
                  OutArgs(20)='Zone'
                endif
                if (samestring('OUTSIDE AIR NODE',InArgs(20))) then
                  OutArgs(20)='Outdoors'
                endif

              CASE('WATER HEATER:STRATIFIED')
                ObjectName='WaterHeater:Stratified'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('VERTICAL CYLINDER',InArgs(5))) then
                  OutArgs(5)='VerticalCylinder'
                endif
                if (samestring('HORIZONTAL CYLINDER',InArgs(5))) then
                  OutArgs(5)='HorizontalCylinder'
                endif
                if (samestring('OTHER',InArgs(5))) then
                  OutArgs(5)='Other'
                endif
                if (samestring('MASTER-SLAVE',InArgs(8))) then
                  OutArgs(8)='MasterSlave'
                endif
                if (samestring('PurchasedHeating',InArgs(17))) then
                  OutArgs(17)='DistrictHeating'
                endif
                if (samestring('PurchasedHeating',InArgs(20))) then
                  OutArgs(20)='DistrictHeating'
                endif
                if (samestring('PurchasedHeating',InArgs(24))) then
                  OutArgs(24)='DistrictHeating'
                endif
                if (samestring('SCHEDULE',InArgs(27))) then
                  OutArgs(27)='Schedule'
                endif
                if (samestring('ZONE',InArgs(27))) then
                  OutArgs(27)='Zone'
                endif
                if (samestring('OUTSIDE AIR NODE',InArgs(27))) then
                  OutArgs(27)='Outdoors'
                endif
                if (samestring('FIXED',InArgs(48))) then
                  OutArgs(48)='Fixed'
                endif
                if (samestring('SEEKING',InArgs(48))) then
                  OutArgs(48)='Seeking'
                endif

              CASE('WATER HEATER:SIZING')
                ObjectName='WaterHeater:Sizing'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Peak Draw',InArgs(2))) then
                  OutArgs(2)='PeakDraw'
                endif
                if (samestring('Residential HUD-FHA Minimum',InArgs(2))) then
                  OutArgs(2)='ResidentialHUD-FHAMinimum'
                endif
                if (samestring('Per Person',InArgs(2))) then
                  OutArgs(2)='PerPerson'
                endif
                if (samestring('Per Floor Area',InArgs(2))) then
                  OutArgs(2)='PerFloorArea'
                endif
                if (samestring('Per Unit',InArgs(2))) then
                  OutArgs(2)='PerUnit'
                endif
                if (samestring('Per Solar Collector Area',InArgs(2))) then
                  OutArgs(2)='PerSolarCollectorArea'
                endif

              CASE('HEAT PUMP:WATER HEATER')
                ObjectName='WaterHeater:HeatPump'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SCHEDULE',InArgs(9))) then
                  OutArgs(9)='Schedule'
                endif
                if (samestring('ZONE AIR ONLY',InArgs(9))) then
                  OutArgs(9)='ZoneAirOnly'
                endif
                if (samestring('OUTDOOR AIR ONLY',InArgs(9))) then
                  OutArgs(9)='OutdoorAirOnly'
                endif
                if (samestring('ZONE AND OUTDOOR AIR',InArgs(9))) then
                  OutArgs(9)='ZoneAndOutdoorAir'
                endif
 ! field 17 is object type/choice
                if (samestring('WATER HEATER:MIXED',InArgs(17))) then
                  OutArgs(17)='WaterHeater:Mixed'
                endif
 ! field 21 is object type/choice
                if (samestring('COIL:DX:HEATPUMPWATERHEATER',InArgs(21))) then
                  OutArgs(21)='Coil:WaterHeating:AirToWaterHeatPump'
                endif
                if (samestring('SCHEDULE',InArgs(24))) then
                  OutArgs(24)='Schedule'
                endif
                if (samestring('ZONE',InArgs(24))) then
                  OutArgs(24)='Zone'
                endif
                if (samestring('EXTERIOR',InArgs(24))) then
                  OutArgs(24)='Outdoors'
                endif
 ! field 26 is object type/choice
                if (samestring('FAN:SIMPLE:ONOFF',InArgs(26))) then
                  OutArgs(26)='Fan:OnOff'
                endif
                if (samestring('blow through',InArgs(28))) then
                  OutArgs(28)='BlowThrough'
                endif
                if (samestring('draw through',InArgs(28))) then
                  OutArgs(28)='DrawThrough'
                endif
                if (samestring('ZONE',InArgs(31))) then
                  OutArgs(31)='Zone'
                endif
                if (samestring('EXTERIOR',InArgs(31))) then
                  OutArgs(31)='Outdoors'
                endif

              CASE('COOLING TOWER:SINGLE SPEED')
                ObjectName='CoolingTower:SingleSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('UA and Design Water Flow Rate',InArgs(10))) then
                  OutArgs(10)='UFactorTimesAreaAndDesignWaterFlowRate'
                endif
                if (samestring('Nominal Capacity',InArgs(10))) then
                  OutArgs(10)='NominalCapacity'
                endif
                if (samestring('USER LOSS FACTOR',InArgs(13))) then
                  OutArgs(13)='LossFactor'
                endif
                if (samestring('SATURATED EXIT',InArgs(13))) then
                  OutArgs(13)='SaturatedExit'
                endif
                if (samestring('CONCENTRATION RATIO',InArgs(16))) then
                  OutArgs(16)='ConcentrationRatio'
                endif
                if (samestring('SCHEDULED RATE',InArgs(16))) then
                  OutArgs(16)='ScheduledRate'
                endif

              CASE('COOLING TOWER:TWO SPEED')
                ObjectName='CoolingTower:TwoSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('UA and Design Water Flow Rate',InArgs(13))) then
                  OutArgs(13)='UFactorTimesAreaAndDesignWaterFlowRate'
                endif
                if (samestring('Nominal Capacity',InArgs(13))) then
                  OutArgs(13)='NominalCapacity'
                endif
                if (samestring('USER LOSS FACTOR',InArgs(17))) then
                  OutArgs(17)='LossFactor'
                endif
                if (samestring('SATURATED EXIT',InArgs(17))) then
                  OutArgs(17)='SaturatedExit'
                endif
                if (samestring('CONCENTRATION RATIO',InArgs(20))) then
                  OutArgs(20)='ConcentrationRatio'
                endif
                if (samestring('SCHEDULED RATE',InArgs(20))) then
                  OutArgs(20)='ScheduledRate'
                endif

              CASE('COOLING TOWER:VARIABLE SPEED')
                ObjectName='CoolingTower:VariableSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CoolTools CrossFlow',InArgs(4))) then
                  OutArgs(4)='CoolToolsCrossFlow'
                endif
                if (samestring('CoolTools User Defined',InArgs(4))) then
                  OutArgs(4)='CoolToolsUserDefined'
                endif
                if (samestring('YorkCalc User Defined',InArgs(4))) then
                  OutArgs(4)='YorkCalcUserDefined'
                endif
                if (samestring('USER LOSS FACTOR',InArgs(18))) then
                  OutArgs(18)='LossFactor'
                endif
                if (samestring('SATURATED EXIT',InArgs(18))) then
                  OutArgs(18)='SaturatedExit'
                endif
                if (samestring('CONCENTRATION RATIO',InArgs(21))) then
                  OutArgs(21)='ConcentrationRatio'
                endif
                if (samestring('SCHEDULED RATE',InArgs(21))) then
                  OutArgs(21)='ScheduledRate'
                endif

              CASE('COOLING TOWER:VARIABLE SPEED:COOLTOOLS MODEL COEFFICIENTS')
                ObjectName='CoolingTowerPerformance:CoolTools'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COOLING TOWER:VARIABLE SPEED:YORKCALC MODEL COEFFICIENTS')
                ObjectName='CoolingTowerPerformance:YorkCalc'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FLUID COOLER:SINGLE SPEED') ! new in V3.0
                ObjectName='FluidCooler:SingleSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('UA and Design Water Flow Rate',InArgs(4))) then
                  OutArgs(4)='UFactorTimesAreaAndDesignWaterFlowRate'
                endif
                if (samestring('Nominal Capacity',InArgs(4))) then
                  OutArgs(4)='NominalCapacity'
                endif

              CASE('FLUID COOLER:TWO SPEED') ! new in V3.0
                ObjectName='FluidCooler:TwoSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('UA and Design Water Flow Rate',InArgs(4))) then
                  OutArgs(4)='UFactorTimesAreaAndDesignWaterFlowRate'
                endif
                if (samestring('Nominal Capacity',InArgs(4))) then
                  OutArgs(4)='NominalCapacity'
                endif

              CASE('GROUND HEAT EXCHANGER:VERTICAL')
                ObjectName='GroundHeatExchanger:Vertical'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GROUND HEAT EXCHANGER:POND')
                ObjectName='GroundHeatExchanger:Pond'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GROUND HEAT EXCHANGER:SURFACE')
                ObjectName='GroundHeatExchanger:Surface'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('GROUND',InArgs(11))) then
                  OutArgs(11)='Ground'
                endif
                if (samestring('EXPOSED',InArgs(11))) then
                  OutArgs(11)='Exposed'
                endif

              CASE('HEAT EXCHANGER:HYDRONIC:FREE COOLING')
                ObjectName='HeatExchanger:Hydronic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 is object type
                CALL ReplaceRenamedObjectFields(InArgs(3),OutArgs(3),ErrFlag)
                if (samestring('WETBULB',InArgs(11))) then
                  OutArgs(11)='WetBulbTemperature'
                endif
                if (samestring('DRYBULB',InArgs(11))) then
                  OutArgs(11)='DryBulbTemperature'
                endif
                if (samestring('SCHEDULE',InArgs(11))) then
                  OutArgs(11)='Schedule'
                endif
                if (samestring('LOOP',InArgs(11))) then
                  OutArgs(11)='Loop'
                endif
                if (samestring('IDEAL',InArgs(13))) then
                  OutArgs(13)='Ideal'
                endif
                if (samestring('UA-EFFECTIVENESS',InArgs(13))) then
                  OutArgs(13)='UFactorTimesAreaEffectiveness'
                endif

              CASE('HEAT EXCHANGER:PLATE:FREE COOLING')
                ObjectName='HeatExchanger:Plate'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 is object type
                CALL ReplaceRenamedObjectFields(InArgs(3),OutArgs(3),ErrFlag)
                if (samestring('IDEAL',InArgs(10))) then
                  OutArgs(10)='Ideal'
                endif
                if (samestring('UA-EFFECTIVENESS',InArgs(10))) then
                  OutArgs(10)='UFactorTimesAreaEffectiveness'
                endif

              CASE('HEAT EXCHANGER:WATERSIDE ECONOMIZER')
                ObjectName='HeatExchanger:WatersideEconomizer'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('PLATEFRAME',InArgs(2))) then
                  OutArgs(2)='PlateFrame'
                endif
                if (samestring('COUNTERFLOW',InArgs(2))) then
                  OutArgs(2)='CounterFlow'
                endif
                if (samestring('PARALLELFLOW',InArgs(2))) then
                  OutArgs(2)='ParallelFlow'
                endif
                if (samestring('IDEAL',InArgs(2))) then
                  OutArgs(2)='Ideal'
                endif

              CASE('PUMP:VARIABLE SPEED')
                ObjectName='Pump:VariableSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CONTINUOUS',InArgs(14))) then
                  OutArgs(14)='Continuous'
                endif
                if (samestring('INTERMITTENT',InArgs(14))) then
                  OutArgs(14)='Intermittent'
                endif

              CASE('PUMP:CONSTANT SPEED')
                ObjectName='Pump:ConstantSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CONTINUOUS',InArgs(9))) then
                  OutArgs(9)='Continuous'
                endif
                if (samestring('INTERMITTENT',InArgs(9))) then
                  OutArgs(9)='Intermittent'
                endif

              CASE('PUMP:CONDENSATE')
                ObjectName='Pump:VariableSpeed:Condensate'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HEADERED PUMPS:SIMPLE:VARIABLE SPEED')
                ObjectName='HeaderedPumps:VariableSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SEQUENTIAL',InArgs(6))) then
                  OutArgs(6)='Sequential'
                endif
                if (samestring('CONTINUOUS',InArgs(16))) then
                  OutArgs(16)='Continuous'
                endif
                if (samestring('INTERMITTENT',InArgs(16))) then
                  OutArgs(16)='Intermittent'
                endif

              CASE('HEADERED PUMPS:SIMPLE:CONSTANT SPEED')
                ObjectName='HeaderedPumps:ConstantSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SEQUENTIAL',InArgs(6))) then
                  OutArgs(6)='Sequential'
                endif
                if (samestring('CONTINUOUS',InArgs(11))) then
                  OutArgs(11)='Continuous'
                endif
                if (samestring('INTERMITTENT',InArgs(11))) then
                  OutArgs(11)='Intermittent'
                endif

              CASE('COIL:WATER:COOLING')
                ObjectName='Coil:Cooling:Water'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SIMPLEANALYSIS',InArgs(14))) then
                  OutArgs(14)='SimpleAnalysis'
                endif
                if (samestring('DETAILEDANALYSIS',InArgs(14))) then
                  OutArgs(14)='DetailedAnalysis'
                endif
                if (samestring('CROSSFLOW',InArgs(15))) then
                  OutArgs(15)='CrossFlow'
                endif
                if (samestring('COUNTERFLOW',InArgs(15))) then
                  OutArgs(15)='CounterFlow'
                endif

              CASE('COIL:WATER:SIMPLEHEATING')
                ObjectName='Coil:Heating:Water'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('UA and Design Water Flow Rate',InArgs(9))) then
                  OutArgs(9)='UFactorTimesAreaAndDesignWaterFlowRate'
                endif
                if (samestring('Nominal Capacity',InArgs(9))) then
                  OutArgs(9)='NominalCapacity'
                endif

              CASE('COIL:STEAM:AIRHEATING')
                ObjectName='Coil:Heating:Steam'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('TEMPERATURESETPOINTCONTROL',InArgs(10))) then
                  OutArgs(10)='TemperatureSetpointControl'
                endif
                if (samestring('ZONELOADCONTROL',InArgs(10))) then
                  OutArgs(10)='ZoneLoadControl'
                endif

              CASE('COIL:ELECTRIC:HEATING')
                ObjectName='Coil:Heating:Electric'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COIL:DESUPERHEATER:HEATING')
                ObjectName='Coil:Heating:Desuperheater'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 6 is object type/choice
                if (samestring('COIL:DX:COOLINGBYPASSFACTOREMPIRICAL',InArgs(6))) then
                  OutArgs(6)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:MULTISPEED:COOLINGEMPIRICAL',InArgs(6))) then
                  OutArgs(6)='Coil:Cooling:DX:TwoSpeed'
                endif
                if (samestring('COIL:DX:MULTIMODE:COOLINGEMPIRICAL',InArgs(6))) then
                  OutArgs(6)='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                endif
                if (samestring('COMPRESSOR RACK:REFRIGERATED CASE',InArgs(6))) then
                  OutArgs(6)='Refrigeration:CompressorRack'
                endif
                if (samestring('Refrigeration:CONDENSER',InArgs(6))) then
                  OutArgs(6)='Refrigeration:Condenser'
                endif

              CASE('COIL:GAS:HEATING')
                ObjectName='Coil:Heating:Gas'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COIL:WATER:DETAILEDFLATCOOLING')
                ObjectName='Coil:Cooling:Water:DetailedGeometry'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COIL:DX:COOLINGBYPASSFACTOREMPIRICAL')
                ObjectName='Coil:Cooling:DX:SingleSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CycFanCycComp',InArgs(14))) then
                  OutArgs(14)='CyclingFanAndCompressor'
                endif
                if (samestring('ContFanCycComp',InArgs(14))) then
                  OutArgs(14)='ContinuousFanWithCyclingCompressor'
                endif
                if (samestring('AIR COOLED',InArgs(20))) then
                  OutArgs(20)='AirCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(20))) then
                  OutArgs(20)='EvaporativelyCooled'
                endif

              CASE('COIL:DX:MULTISPEED:COOLINGEMPIRICAL')
                ObjectName='Coil:Cooling:DX:TwoSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CycFanCycComp',InArgs(14))) then
                  OutArgs(14)='CyclingFanAndCompressor'
                endif
                if (samestring('ContFanCycComp',InArgs(14))) then
                  OutArgs(14)='ContinuousFanWithCyclingCompressor'
                endif
                if (samestring('VarFanVarComp',InArgs(14))) then
                  OutArgs(14)='VariableSpeedFanAndCompressor'
                endif
                if (samestring('VarFanUnloadComp',InArgs(14))) then
                  OutArgs(14)='VariableSpeedFanWithCompressorUnloading'
                endif
                if (samestring('AIR COOLED',InArgs(22))) then
                  OutArgs(22)='AirCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(22))) then
                  OutArgs(22)='EvaporativelyCooled'
                endif

              CASE('COIL:DX:MULTIMODE:COOLINGEMPIRICAL')
                ObjectName='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do Arg=9,CurArgs,2
 ! field 9 and every 2 thereafter is object type/choice
                  if (samestring('CoilPerformance:DX:CoolingBypassFactorEmpirical',InArgs(Arg))) then
                    OutArgs(Arg)='CoilPerformance:DX:Cooling'
                  endif
                enddo

              CASE('COILPERFORMANCE:DX:COOLINGBYPASSFACTOREMPIRICAL')
                ObjectName='CoilPerformance:DX:Cooling'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CycFanCycComp',InArgs(12))) then
                  OutArgs(12)='CyclingFanAndCompressor'
                endif
                if (samestring('ContFanCycComp',InArgs(12))) then
                  OutArgs(12)='ContinuousFanWithCyclingCompressor'
                endif
                if (samestring('AIR COOLED',InArgs(18))) then
                  OutArgs(18)='AirCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(18))) then
                  OutArgs(18)='EvaporativelyCooled'
                endif

              CASE('COIL:DX:MULTISPEED:COOLING')
                ObjectName='Coil:Cooling:DX:MultiSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CycFanCycComp',InArgs(5))) then
                  OutArgs(5)='CyclingFanAndCompressor'
                endif
                if (samestring('ContFanCycComp',InArgs(5))) then
                  OutArgs(5)='ContinuousFanWithCyclingCompressor'
                endif
                if (samestring('AIR COOLED',InArgs(7))) then
                  OutArgs(7)='AirCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(7))) then
                  OutArgs(7)='EvaporativelyCooled'
                endif

              CASE('COIL:DX:HEATINGEMPIRICAL')
                ObjectName='Coil:Heating:DX:SingleSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CycFanCycComp',InArgs(14))) then
                  OutArgs(14)='CyclingFanAndCompressor'
                endif
                if (samestring('ContFanCycComp',InArgs(14))) then
                  OutArgs(14)='ContinuousFanWithCyclingCompressor'
                endif
                if (samestring('reverse-cycle',InArgs(19))) then
                  OutArgs(19)='ReverseCycle'
                endif
                if (samestring('resistive',InArgs(19))) then
                  OutArgs(19)='Resistive'
                endif
                if (samestring('timed',InArgs(20))) then
                  OutArgs(20)='Timed'
                endif
                if (samestring('on-demand',InArgs(20))) then
                  OutArgs(20)='OnDemand'
                endif

              CASE('COIL:DX:MULTISPEED:HEATING')
                ObjectName='Coil:Heating:DX:MultiSpeed'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CycFanCycComp',InArgs(5))) then
                  OutArgs(5)='CyclingFanAndCompressor'
                endif
                if (samestring('ContFanCycComp',InArgs(5))) then
                  OutArgs(5)='ContinuousFanWithCyclingCompressor'
                endif
                if (samestring('reverse-cycle',InArgs(11))) then
                  OutArgs(11)='ReverseCycle'
                endif
                if (samestring('resistive',InArgs(11))) then
                  OutArgs(11)='Resistive'
                endif
                if (samestring('timed',InArgs(12))) then
                  OutArgs(12)='Timed'
                endif
                if (samestring('on-demand',InArgs(12))) then
                  OutArgs(12)='OnDemand'
                endif

              CASE('COIL:DX:COOLINGHEATEXCHANGERASSISTED')
                ObjectName='CoilSystem:Cooling:DX:HeatExchangerAssisted'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 is object type/choice
                if (samestring('Heat Exchanger:Air to Air:Flat Plate',InArgs(2))) then
                  OutArgs(2)='HeatExchanger:AirToAir:FlatPlate'
                endif
                if (samestring('Heat Exchanger:Air to Air:Generic',InArgs(2))) then
                  OutArgs(2)='HeatExchanger:AirToAir:SensibleAndLatent'
                endif
                if (samestring('Heat Exchanger:Desiccant:BalancedFlow',InArgs(2))) then
                  OutArgs(2)='HeatExchanger:Desiccant:BalancedFlow'
                endif
 ! field 4 is object type/choice
                if (samestring('COIL:DX:CoolingBypassFactorEmpirical',InArgs(4))) then
                  OutArgs(4)='Coil:Cooling:DX:SingleSpeed'
                endif

              CASE('COIL:DX:HEATPUMPWATERHEATER')
                ObjectName='Coil:WaterHeating:AirToWaterHeatPump'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Dry-bulb temperature',InArgs(21))) then
                  OutArgs(21)='DryBulbTemperature'
                endif
                if (samestring('Wet-bulb temperature',InArgs(21))) then
                  OutArgs(21)='WetBulbTemperature'
                endif

              CASE('COIL:WATER:COOLINGHEATEXCHANGERASSISTED')
                ObjectName='CoilSystem:Cooling:Water:HeatExchangerAssisted'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 2 is object type/choice
                if (samestring('Heat Exchanger:Air to Air:Flat Plate',InArgs(2))) then
                  OutArgs(2)='HeatExchanger:AirToAir:FlatPlate'
                endif
                if (samestring('Heat Exchanger:Air to Air:Generic',InArgs(2))) then
                  OutArgs(2)='HeatExchanger:AirToAir:SensibleAndLatent'
                endif
 ! field 4 is object type/choice
                if (samestring('COIL:Water:Cooling',InArgs(4))) then
                  OutArgs(4)='Coil:Cooling:Water'
                endif
                if (samestring('COIL:Water:DetailedFlatCooling',InArgs(4))) then
                  OutArgs(4)='Coil:Cooling:Water:DetailedGeometry'
                endif

              CASE('COIL:WATERTOAIRHP:PARAMETERESTIMATION:COOLING')
                ObjectName='Coil:Cooling:WaterToAirHeatPump:ParameterEstimation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COIL:WATERTOAIRHP:PARAMETERESTIMATION:HEATING')
                ObjectName='Coil:Heating:WaterToAirHeatPump:ParameterEstimation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COIL:WATER:DESUPERHEATERHEATING')
                ObjectName='Coil:WaterHeating:Desuperheater'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 12 is object type/choice
                if (samestring('WATER HEATER:MIXED',InArgs(12))) then
                  OutArgs(12)='WaterHeater:Mixed'
                endif
 ! field 14 is object type/choice
                if (samestring('COIL:DX:COOLINGBYPASSFACTOREMPIRICAL',InArgs(14))) then
                  OutArgs(14)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:DX:MULTISPEED:COOLINGEMPIRICAL',InArgs(14))) then
                  OutArgs(14)='Coil:Cooling:DX:TwoSpeed'
                endif
                if (samestring('COIL:DX:MULTIMODE:COOLINGEMPIRICAL',InArgs(14))) then
                  OutArgs(14)='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                endif
                if (samestring('COMPRESSOR RACK:REFRIGERATED CASE',InArgs(14))) then
                  OutArgs(14)='Refrigeration:CompressorRack'
                endif

              CASE('COIL:WATERTOAIRHP:EQUATIONFIT:COOLING')
                ObjectName='Coil:Cooling:WaterToAirHeatPump:EquationFit'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COIL:WATERTOAIRHP:EQUATIONFIT:HEATING')
                ObjectName='Coil:Heating:WaterToAirHeatPump:EquationFit'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FAN:SIMPLE:CONSTVOLUME')
                ObjectName='Fan:ConstantVolume'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FAN:SIMPLE:VARIABLEVOLUME')
                ObjectName='Fan:VariableVolume'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FAN:SIMPLE:ONOFF')
                ObjectName='Fan:OnOff'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ZONE EXHAUST FAN')
                ObjectName='Fan:ZoneExhaust'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FAN:NIGHT VENT PERFORMANCE')
                ObjectName='FanPerformance:NightVentilation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('EVAPCOOLER:DIRECT:CELDEKPAD')
                ObjectName='EvaporativeCooler:Direct:CelDekPad'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('EVAPCOOLER:INDIRECT:CELDEKPAD')
                ObjectName='EvaporativeCooler:Indirect:CelDekPad'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('EVAPCOOLER:INDIRECT:WETCOIL')
                ObjectName='EvaporativeCooler:Indirect:WetCoil'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('EVAPCOOLER:INDIRECT:RDDSPECIAL')
                ObjectName='EvaporativeCooler:Indirect:ResearchSpecial'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('HUMIDIFIER:STEAM:ELECTRICAL')
                ObjectName='Humidifier:Steam:Electric'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DESICCANT DEHUMIDIFIER:SOLID')
                ObjectName='Dehumidifier:Desiccant:NoFans'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('FIXED LEAVING HUMRAT SETPOINT:BYPASS',InArgs(7))) then
                  OutArgs(7)='LeavingMaximumHumidityRatioSetpoint'
                endif
                if (samestring('NODE LEAVING HUMRAT SETPOINT:BYPASS',InArgs(7))) then
                  OutArgs(7)='SystemNodeMaximumHumidityRatioSetpoint'
                endif
 ! field 12 is object type/choice
                if (samestring('COIL:Electric:Heating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Electric'
                endif
                if (samestring('COIL:Gas:Heating',InArgs(12))) then
                  OutArgs(12)='Coil:Heating:Gas'
                endif
 ! field 14 is object type/choice
                if (samestring('FAN:SIMPLE:VariableVolume',InArgs(14))) then
                  OutArgs(14)='Fan:VariableVolume'
                endif
                if (samestring('FAN:SIMPLE:ConstVolume',InArgs(14))) then
                  OutArgs(14)='Fan:ConstantVolume'
                endif
                if (samestring('DEFAULT',InArgs(16))) then
                  OutArgs(16)='Default'
                endif
                if (samestring('USER CURVES',InArgs(16))) then
                  OutArgs(16)='UserCurves'
                endif

              CASE('DESICCANT DEHUMIDIFIER')
                ObjectName='Dehumidifier:Desiccant:System'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 3 is object type/choice
                if (samestring('Heat Exchanger:Desiccant:BalancedFlow',InArgs(3))) then
                  OutArgs(3)='HeatExchanger:Desiccant:BalancedFlow'
                endif
 ! field 6 is object type/choice
                if (samestring('Fan:Simple:OnOff',InArgs(6))) then
                  OutArgs(6)='Fan:OnOff'
                endif
                if (samestring('Fan:Simple:ConstVolume',InArgs(6))) then
                  OutArgs(6)='Fan:ConstantVolume'
                endif
                if (samestring('Blow through',InArgs(8))) then
                  OutArgs(8)='BlowThrough'
                endif
                if (samestring('Draw through',InArgs(8))) then
                  OutArgs(8)='DrawThrough'
                endif
 ! field 9 is object type/choice
                if (samestring('Coil:Electric:Heating',InArgs(9))) then
                  OutArgs(9)='Coil:Heating:Electric'
                endif
                if (samestring('Coil:Gas:Heating',InArgs(9))) then
                  OutArgs(9)='Coil:Heating:Gas'
                endif
 ! field 12 is object type/choice
                if (samestring('Coil:DX:CoolingBypassFactorEmpirical',InArgs(12))) then
                  OutArgs(12)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('Coil:DX:Multimode:CoolingEmpirical',InArgs(12))) then
                  OutArgs(12)='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
                endif

              CASE('HEAT EXCHANGER:AIR TO AIR:FLAT PLATE')
                ObjectName='HeatExchanger:AirToAir:FlatPlate'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Counter Flow',InArgs(3))) then
                  OutArgs(3)='CounterFlow'
                endif
                if (samestring('Parallel Flow',InArgs(3))) then
                  OutArgs(3)='ParallelFlow'
                endif
                if (samestring('Cross Flow Both Unmixed',InArgs(3))) then
                  OutArgs(3)='CrossFlowBothUnmixed'
                endif
                if (samestring('yes',InArgs(4))) then
                  OutArgs(4)='Yes'
                endif
                if (samestring('no',InArgs(4))) then
                  OutArgs(4)='No'
                endif

              CASE('HEAT EXCHANGER:AIR TO AIR:GENERIC')
                ObjectName='HeatExchanger:AirToAir:SensibleAndLatent'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Exhaust air recirculation',InArgs(19))) then
                  OutArgs(19)='ExhaustAirRecirculation'
                endif
                if (samestring('Exhaust only',InArgs(19))) then
                  OutArgs(19)='ExhaustOnly'
                endif
                if (samestring('Minimum exhaust temperature',InArgs(19))) then
                  OutArgs(19)='MinimumExhaustTemperature'
                endif

              CASE('HEAT EXCHANGER:DESICCANT:BALANCEDFLOW')
                ObjectName='HeatExchanger:Desiccant:BalancedFlow'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
 ! field 7 is object type/choice
                if (samestring('Heat Exchanger:Desiccant:BalancedFlow:Performance Data Type 1',InArgs(7))) then
                  OutArgs(7)='HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1'
                endif

              CASE('HEAT EXCHANGER:DESICCANT:BALANCEDFLOW:PERFORMANCE DATA TYPE 1')
                ObjectName='HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('DEMAND MANAGER LIST')
                ObjectName='DemandManagerAssignmentList'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('SEQUENTIAL',InArgs(8))) then
                  OutArgs(8)='Sequential'
                endif
                if (samestring('ALL',InArgs(8))) then
                  OutArgs(8)='All'
                endif
                do Arg=1,CurArgs,2
 ! field 9 and every 2 thereafter is object type/choice
                  if (samestring('DEMAND MANAGER:EXTERIOR LIGHTS',InArgs(Arg))) then
                    OutArgs(Arg)='DemandManager:ExteriorLights'
                  endif
                  if (samestring('DEMAND MANAGER:LIGHTS',InArgs(Arg))) then
                    OutArgs(Arg)='DemandManager:Lights'
                  endif
                  if (samestring('DEMAND MANAGER:ELECTRIC EQUIPMENT',InArgs(Arg))) then
                    OutArgs(Arg)='DemandManager:ElectricEquipment'
                  endif
                  if (samestring('DEMAND MANAGER:THERMOSTATS',InArgs(Arg))) then
                    OutArgs(Arg)='DemandManager:Thermostats'
                  endif
                enddo

              CASE('DEMAND MANAGER:EXTERIOR LIGHTS')
                ObjectName='DemandManager:ExteriorLights'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('OFF',InArgs(3))) then
                  OutArgs(3)='Off'
                endif
                if (samestring('FIXED',InArgs(3))) then
                  OutArgs(3)='Fixed'
                endif
                if (samestring('ALL',InArgs(7))) then
                  OutArgs(7)='All'
                endif
                if (samestring('ROTATE MANY',InArgs(7))) then
                  OutArgs(7)='RotateMany'
                endif
                if (samestring('ROTATE ONE',InArgs(7))) then
                  OutArgs(7)='RotateOne'
                endif

              CASE('DEMAND MANAGER:LIGHTS')
                ObjectName='DemandManager:Lights'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('OFF',InArgs(3))) then
                  OutArgs(3)='Off'
                endif
                if (samestring('FIXED',InArgs(3))) then
                  OutArgs(3)='Fixed'
                endif
                if (samestring('ALL',InArgs(7))) then
                  OutArgs(7)='All'
                endif
                if (samestring('ROTATE MANY',InArgs(7))) then
                  OutArgs(7)='RotateMany'
                endif
                if (samestring('ROTATE ONE',InArgs(7))) then
                  OutArgs(7)='RotateOne'
                endif

              CASE('DEMAND MANAGER:ELECTRIC EQUIPMENT')
                ObjectName='DemandManager:ElectricEquipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('OFF',InArgs(3))) then
                  OutArgs(3)='Off'
                endif
                if (samestring('FIXED',InArgs(3))) then
                  OutArgs(3)='Fixed'
                endif
                if (samestring('ALL',InArgs(7))) then
                  OutArgs(7)='All'
                endif
                if (samestring('ROTATE MANY',InArgs(7))) then
                  OutArgs(7)='RotateMany'
                endif
                if (samestring('ROTATE ONE',InArgs(7))) then
                  OutArgs(7)='RotateOne'
                endif

              CASE('DEMAND MANAGER:THERMOSTATS')
                ObjectName='DemandManager:Thermostats'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('OFF',InArgs(3))) then
                  OutArgs(3)='Off'
                endif
                if (samestring('FIXED',InArgs(3))) then
                  OutArgs(3)='Fixed'
                endif
                if (samestring('ALL',InArgs(8))) then
                  OutArgs(8)='All'
                endif
                if (samestring('ROTATE MANY',InArgs(8))) then
                  OutArgs(8)='RotateMany'
                endif
                if (samestring('ROTATE ONE',InArgs(8))) then
                  OutArgs(8)='RotateOne'
                endif

              CASE('ELECTRIC LOAD CENTER:DISTRIBUTION')
                ObjectName='ElectricLoadCenter:Distribution'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:2)=InArgs(1:2)
                SELECT CASE(MakeUPPERCase(InArgs(3)))
                  CASE('BASELOAD')
                    OutArgs(3)='Baseload'
                  CASE('DEMAND LIMIT')
                    OutArgs(3)='DemandLimit'
                  CASE('TRACK ELECTRICAL')
                    OutArgs(3)='TrackElectrical'
                  CASE('TRACK SCHEDULE')
                    OutArgs(3)='TrackSchedule'
                  CASE('TRACK METER')
                    OutArgs(3)='TrackMeter'
                  CASE('FOLLOW THERMAL')
                    OutArgs(3)='FollowThermal'
                  CASE('FOLLOW THERMAL LIMIT ELECTRICAL')
                    OutArgs(3)='FollowThermalLimitElectrical'
                  CASE DEFAULT
                    OutArgs(3)=InArgs(3)
                    WRITE(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//',Warning,'
                    WRITE(DifLfn,fmta) 'ELECTRIC LOAD CENTER:DISTRIBUTION/ElectricLoadCenter:Distribution - ,'
                    WRITE(DifLfn,fmta) 'Illegal Generator Operation Scheme Type specified=,'
                    WRITE(DifLfn,fmta) trim(InArgs(3))//','
                    WRITE(DifLfn,fmta) 'Unit='//TRIM(InArgs(1))//';'
                END SELECT
                OutArgs(4:6)=InArgs(4:6)
                TotGenCount=0
                TotPVCount=0
                DO InNum=1,NumIDFRecords
                  IF (SameString(IDFRecords(InNum)%Name,'ELECTRIC LOAD CENTER:GENERATORS')) THEN
                    IF (SameString(IDFRecords(InNum)%Alphas(1),InArgs(2))) THEN
                      DO InNum2=3,IDFRecords(InNum)%NumAlphas,3
                        TotGenCount=TotGenCount+1
                        IF (INDEX(MakeUPPERCase(IDFRecords(InNum)%Alphas(InNum2)),'PV') > 0) TotPVCount=TotPVCount+1
                      ENDDO
                      EXIT
                    ENDIF
                  ENDIF
                ENDDO
                IF (TotGenCount == TotPVCount) THEN
                  OutArgs(7)='DirectCurrentWithInverter'
                ELSEIF (TotPVCount == 0) THEN
                  OutArgs(7)='AlternatingCurrent'
                ELSE
                  OutArgs(7)='MixedPVandOtherInGenerators'
                  WRITE(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//',Severe,'//  &
                     'ELECTRIC LOAD CENTER:DISTRIBUTION - Mixed PV and other Generators specified='//trim(InArgs(2))//','
                  WRITE(DifLfn,fmta) 'Unit='//TRIM(InArgs(1))//';'
                ENDIF
                IF (OutArgs(7) == 'DirectCurrentWithInverter') THEN
                  ! add inverter object
                  CurArgs=8
                  OutArgs(8)=InArgs(1)(1:min(len_trim(InArgs(1)),90))//' Inverter'  ! From original electric center object
                  CALL WriteOutIDFLines(DifLfn,'ElectricLoadCenter:Distribution',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  CALL GetNewObjectDefInIDD('ElectricLoadCenter:Inverter:Simple',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)(1:min(len_trim(InArgs(1)),90))//' Inverter'  ! From original electric center object
                  OutArgs(2)=trim(OutArgs(1))//' always on'
                  OutArgs(3:4)=' '
                  AvePV=0.0
                  TotPVCount=0
                  DO InNum=1,NumIDFRecords
                    IF (SameString(IDFRecords(InNum)%Name,'ELECTRIC LOAD CENTER:GENERATORS')) THEN
                      IF (SameString(IDFRecords(InNum)%Alphas(1),InArgs(2))) THEN
                        DO InNum2=3,IDFRecords(InNum)%NumAlphas,3
                          IF (INDEX(MakeUPPERCase(IDFRecords(InNum)%Alphas(InNum2)),'PV') > 0) THEN
                            IF (SameString(IDFRecords(InNum)%Alphas(InNum2),'GENERATOR:PV:SIMPLE') .or.  &
                                SameString(IDFRecords(InNum)%Alphas(InNum2),'GENERATOR:PV:EQUIVALENT ONE-DIODE') .or.   &
                                SameString(IDFRecords(InNum)%Alphas(InNum2),'GENERATOR:PV:SANDIA')) THEN
                              TotPVCount=TotPVCount+1
                              DO InNum3=1,NumIDFRecords
                                IF (SameString(IDFRecords(InNum3)%Name,'GENERATOR:PV:SIMPLE')) THEN
                                  IF (.not. SameString(IDFRecords(InNum3)%Alphas(1),IDFRecords(InNum)%Alphas(InNum2-1))) CYCLE
                                  errflag=.false.
                                  AvePV=AvePV+ProcessNumber(IDFRecords(InNum3)%Numbers(2),errflag)
                                  EXIT
                                ENDIF
                                IF (SameString(IDFRecords(InNum3)%Name,'GENERATOR:PV:EQUIVALENT ONE-DIODE')) THEN
                                  IF (.not. SameString(IDFRecords(InNum3)%Alphas(1),IDFRecords(InNum)%Alphas(InNum2-1))) CYCLE
                                  errflag=.false.
                                  AvePV=AvePV+ProcessNumber(IDFRecords(InNum3)%Numbers(21),errflag)
                                  EXIT
                                ENDIF
                                IF (SameString(IDFRecords(InNum3)%Name,'GENERATOR:PV:SANDIA')) THEN
                                  IF (.not. SameString(IDFRecords(InNum3)%Alphas(1),IDFRecords(InNum)%Alphas(InNum2-1))) CYCLE
                                  errflag=.false.
                                  AvePV=AvePV+ProcessNumber(IDFRecords(InNum3)%Numbers(3),errflag)
                                  EXIT
                                ENDIF
                              ENDDO
                            ENDIF
                          ENDIF
                        ENDDO
                        EXIT
                      ENDIF
                    ENDIF
                  ENDDO
                  IF (TotPVCount > 0) THEN
                    AvePV=AvePV/REAL(TotPVCount)
                  ELSE
                    AvePV=0.0
                  ENDIF
                  WRITE(OutArgs(5),'(F20.3)') AvePV
                  OutArgs(5)=adjustl(OutArgs(5))
                  CurArgs=5
                  CALL WriteOutIDFLines(DifLfn,'ElectricLoadCenter:Inverter:Simple',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ! now availability schedule
                  CALL GetNewObjectDefInIDD('Schedule:Compact',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(2)
                  OutArgs(2)='Any Number'
                  OutArgs(3)='Through: 12/31'
                  OutArgs(4)='For: AllDays'
                  OutArgs(5)='Until: 24:00'
                  OutArgs(6)='1'
                  CurArgs=6
                  CALL WriteOutIDFLines(DifLfn,'Schedule:Compact',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ELSE
                  CurArgs=7
                  CALL WriteOutIDFLines(DifLfn,'ElectricLoadCenter:Distribution',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ENDIF
                Written=.true.
                !CYCLE

              CASE('ELECTRIC LOAD CENTER:GENERATORS')
                ObjectName='ElectricLoadCenter:Generators'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do Arg=3,CurArgs,5
 ! field 3 and every 5 thereafter is object type/choice
                  if (samestring('GENERATOR:IC ENGINE',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:InternalCombustionEngine'
                  endif
                  if (samestring('GENERATOR:COMBUSTION TURBINE',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:CombustionTurbine'
                  endif
                  if (samestring('GENERATOR:FUEL CELL',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:FuelCell'
                  endif
                  if (samestring('GENERATOR:MICRO CHP',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:MicroCHP'
                  endif
                  if (samestring('GENERATOR:PV:EQUIVALENT ONE-DIODE',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:Photovoltaic'
                  endif
                  if (samestring('GENERATOR:PV:SANDIA',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:Photovoltaic'
                  endif
                  if (samestring('GENERATOR:PV:SIMPLE',InArgs(Arg))) then
                    OutArgs(Arg)='Generator:Photovoltaic'
                  endif
                enddo


              CASE('ELECTRICLOADCENTER:INVERTER:SIMPLE')
                ObjectName='ElectricLoadCenter:Inverter:Simple'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ELECTRICLOADCENTER:INVERTER:FUNCTIONOFPOWER')
                ObjectName='ElectricLoadCenter:Inverter:FunctionOfPower'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ELECTRICLOADCENTER:INVERTER:LOOKUPTABLE')
                ObjectName='ElectricLoadCenter:Inverter:LookUpTable'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ELECTRICLOADCENTER:STORAGE:SIMPLE')
                ObjectName='ElectricLoadCenter:Storage:Simple'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:IC ENGINE')
                ObjectName='Generator:InternalCombustionEngine'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:COMBUSTION TURBINE')
                ObjectName='Generator:CombustionTurbine'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:MICROTURBINE')
                ObjectName='Generator:MicroTurbine'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Plant Control',InArgs(22))) then
                  OutArgs(22)='PlantControl'
                endif
                if (samestring('Internal Control',InArgs(22))) then
                  OutArgs(22)='InternalControl'
                endif

              CASE('GENERATOR:PV:SIMPLE')
                ObjectName='Generator:Photovoltaic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1)=InArgs(1)
                OutArgs(2)=InArgs(3)
                OutArgs(3)='PhotovoltaicPerformance:Simple'
                OutArgs(4)=trim(InArgs(1))//' Performance'
                SELECT CASE(MakeUPPERCase(InArgs(6)))
                  CASE('DECOUPLED')
                    OutArgs(5)='Decoupled'
                  CASE('INTEGRATED')
                    OutArgs(5)='IntegratedSurfaceOutsideFace'
                  CASE('INTEGRATED TRANSPIRED COLLECTOR')
                    OutArgs(5)='IntegratedTranspiredCollector'
                  CASE('INTEGRATED EXTERIOR VENTED CAVITY')
                    OutArgs(5)='IntegratedExteriorVentedCavity'
                  CASE DEFAULT
                    OutArgs(5)=InArgs(6)
                    WRITE(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//',Severe,'
                    WRITE(DifLfn,fmta) 'GENERATOR:PV:SIMPLE/Generator:Photovoltaic - '
                    WRITE(DifLfn,fmta) 'invalid HeatTransfer Surface Integration Mode=,'
                    WRITE(DifLfn,fmta) trim(InArgs(6))//','
                    WRITE(DifLfn,fmta) 'Unit='//TRIM(InArgs(1))//';'
                END SELECT
                OutArgs(6:7)='1.0'
                CurArgs=7
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                CALL GetNewObjectDefInIDD('PhotovoltaicPerformance:Simple',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=trim(InArgs(1))//' Performance'
                OutArgs(2)=InArgs(4)
                IF (SameString(InArgs(7),'FIXED')) THEN
                  OutArgs(3)='Fixed'
                ELSEIF (SameString(InArgs(7),'SCHEDULED')) THEN
                  OutArgs(3)='Scheduled'
                ELSE
                  OutArgs(3)=InArgs(7)
                ENDIF
                OutArgs(4)=InArgs(8)
                OutArgs(5)=InArgs(9)
                CurArgs=5
                CALL WriteOutIDFLines(DifLfn,'PhotovoltaicPerformance:Simple',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                Written=.true.
                !CYCLE

              CASE('GENERATOR:PV:EQUIVALENT ONE-DIODE')
                ObjectName='Generator:Photovoltaic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1)=InArgs(1)
                OutArgs(2)=InArgs(3)
                OutArgs(3)='PhotovoltaicPerformance:EquivalentOne-Diode'
                OutArgs(4)=trim(InArgs(1))//' Performance'
                SELECT CASE(MakeUPPERCase(InArgs(4)))
                  CASE('DECOUPLED NOCT CONDITIONS')
                    OutArgs(5)='Decoupled'
                  CASE('DECOUPLED ULLEBERG DYNAMIC')
                    OutArgs(5)='DecoupledUllebergDynamic'
                  CASE('INTEGRATED SURFACE OUTSIDE FACE')
                    OutArgs(5)='IntegratedSurfaceOutsideFace'
                  CASE('INTEGRATED TRANSPIRED COLLECTOR')
                    OutArgs(5)='IntegratedTranspiredCollector'
                  CASE('INTEGRATED EXTERIOR VENTED CAVITY')
                    OutArgs(5)='IntegratedExteriorVentedCavity'
                  CASE DEFAULT
                    OutArgs(5)=InArgs(4)
                    WRITE(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//',Severe,'
                    WRITE(DifLfn,fmta) 'GENERATOR:PV:EQUIVALENT ONE-DIODE/Generator:Photovoltaic - '
                    WRITE(DifLfn,fmta) 'invalid Integration and Cell Temperature Mode=,'
                    WRITE(DifLfn,fmta) trim(InArgs(4))//','
                    WRITE(DifLfn,fmta) 'Unit='//TRIM(InArgs(1))//';'
                END SELECT
                OutArgs(6)=InArgs(6)
                OutArgs(7)=InArgs(7)
                CurArgs=7
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                CALL GetNewObjectDefInIDD('PhotovoltaicPerformance:EquivalentOne-Diode',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=trim(InArgs(1))//' Performance'
                errflag=.false.
                IF (ProcessNumber(InArgs(11),errflag) > 100000.) THEN
                  OutArgs(2)='CrystallineSilicon'
                ELSE
                  OutArgs(2)='AmorphousSilicon'
                ENDIF
                OutArgs(3)=InArgs(5)
                OutArgs(4:20)=InArgs(8:24)
                CurArgs=20
                CALL WriteOutIDFLines(DifLfn,'PhotovoltaicPerformance:EquivalentOne-Diode',CurArgs,OutArgs,NwFldNames,NwFldUnits)
                Written=.true.
                !CYCLE

              CASE('GENERATOR:PV:SANDIA')
                ObjectName='Generator:Photovoltaic'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1)=InArgs(1)
                OutArgs(2)=InArgs(3)
                OutArgs(3)='PhotovoltaicPerformance:Sandia'
                OutArgs(4)=InArgs(4)
                SELECT CASE(MakeUPPERCase(InArgs(5)))
                  CASE('SANDIA RACK')
                    OutArgs(5)='Decoupled'
                  CASE('EPLUS INTEGRATED')
                    OutArgs(5)='IntegratedSurfaceOutsideFace'
                  CASE('INTEGRATED TRANSPIRED COLLECTOR')
                    OutArgs(5)='IntegratedTranspiredCollector'
                  CASE('INTEGRATED EXTERIOR VENTED CAVITY')
                    OutArgs(5)='IntegratedExteriorVentedCavity'
                  CASE DEFAULT
                    OutArgs(5)=InArgs(4)
                    WRITE(DifLfn,fmta) 'Output:PreprocessorMessage,'//trim(ProgNameConversion)//',Severe,'
                    WRITE(DifLfn,fmta) 'GENERATOR:PV:SANDIA/Generator:Photovoltaic - '
                    WRITE(DifLfn,fmta) 'invalid Simulation Temperature modeling Mode=,'
                    WRITE(DifLfn,fmta) trim(InArgs(5))//','
                    WRITE(DifLfn,fmta) 'Unit='//TRIM(InArgs(1))//';'
                END SELECT
                OutArgs(6)=InArgs(7)
                OutArgs(7)=InArgs(6)
                CurArgs=7

              CASE('PV MODULE:SANDIA PARAMETERS')
                ObjectName='PhotovoltaicPerformance:Sandia'
                CALL GetNewObjectDefInIDD('PhotovoltaicPerformance:Sandia',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PHOTOVOLTAICPERFORMANCE:SIMPLE')
                ObjectName='PhotovoltaicPerformance:Simple'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PHOTOVOLTAICPERFORMANCE:EQUIVALENTONE-DIODE')
                ObjectName='PhotovoltaicPerformance:EquivalentOne-Diode'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('PHOTOVOLTAICPERFORMANCE:SANDIA')
                ObjectName='PhotovoltaicPerformance:Sandia'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:FUEL CELL')
                ObjectName='Generator:FuelCell'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:FUEL CELL:POWER MODULE')
                ObjectName='Generator:FuelCell:PowerModule'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Constant Rate',InArgs(22))) then
                  OutArgs(22)='ConstantRate'
                endif
                if (samestring('UA for Process Gas Temperature',InArgs(22))) then
                  OutArgs(22)='UAForProcessGasTemperature'
                endif
                if (samestring('Quadratic Function of Fuel Rate',InArgs(22))) then
                  OutArgs(22)='QuadraticFunctionOfFuelRate'
                endif

              CASE('GENERATOR:FUEL CELL:AIR SUPPLY')
                ObjectName='Generator:FuelCell:AirSupply'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Air Ratio by Stoics',InArgs(5))) then
                  OutArgs(5)='AirRatiobyStoics'
                endif
                if (samestring('Quadratic Function of Electric Power',InArgs(5))) then
                  OutArgs(5)='QuadraticFunctionofElectricPower'
                endif
                if (samestring('Quadratic Function of Fuel Rate',InArgs(5))) then
                  OutArgs(5)='QuadraticFunctionofFuelRate'
                endif
                if (samestring('No Recovery',InArgs(10))) then
                  OutArgs(10)='NoRecovery'
                endif
                if (samestring('Recover Burner Inverter Storage',InArgs(10))) then
                  OutArgs(10)='RecoverBurnerInverterStorage'
                endif
                if (samestring('Recover Auxiliary Burner',InArgs(10))) then
                  OutArgs(10)='RecoverAuxiliaryBurner'
                endif
                if (samestring('Recover Inverter and Storage',InArgs(10))) then
                  OutArgs(10)='RecoverInverterandStorage'
                endif
                if (samestring('Recover Inverter',InArgs(10))) then
                  OutArgs(10)='RecoverInverter'
                endif
                if (samestring('Recover Electrical Storage',InArgs(10))) then
                  OutArgs(10)='RecoverElectricalStorage'
                endif
                ! args(11)
                if (samestring('Ambient Air',InArgs(11))) then
                  OutArgs(11)='AmbientAir'
                endif
                if (samestring('User Defined Constituents',InArgs(11))) then
                  OutArgs(11)='UserDefinedConstituents'
                endif
                do Arg=13,CurArgs,2
                  if (samestring('CARBON DIOXIDE',InArgs(Arg))) then
                    OutArgs(Arg)='CarbonDioxide'
                  endif
                  if (samestring('NITROGEN',InArgs(Arg))) then
                    OutArgs(Arg)='Nitrogen'
                  endif
                  if (samestring('OXYGEN',InArgs(Arg))) then
                    OutArgs(Arg)='Oxygen'
                  endif
                  if (samestring('WATER',InArgs(Arg))) then
                    OutArgs(Arg)='Water'
                  endif
                  if (samestring('ARGON',InArgs(Arg))) then
                    OutArgs(Arg)='Argon'
                  endif
                enddo

              CASE('GENERATOR:FUEL CELL:WATER SUPPLY')
                ObjectName='Generator:FuelCell:WaterSupply'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Temperature from Air Node',InArgs(5))) then
                  OutArgs(5)='TemperatureFromAirNode'
                endif
                if (samestring('Temperature from Water Node',InArgs(5))) then
                  OutArgs(5)='TemperatureFromWaterNode'
                endif
                if (samestring('Temperature from Schedule',InArgs(5))) then
                  OutArgs(5)='TemperatureFromSchedule'
                endif
                if (samestring('Mains Water Temperature',InArgs(5))) then
                  OutArgs(5)='MainsWaterTemperature'
                endif

              CASE('GENERATOR:FUEL CELL:AUXILIARY HEATER')
                ObjectName='Generator:FuelCell:AuxiliaryHeater'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Surrounding Zone',InArgs(6))) then
                  OutArgs(6)='SurroundingZone'
                endif
                if (samestring('Air Inlet for Fuel Cell',InArgs(6))) then
                  OutArgs(6)='AirInletForFuelCell'
                endif

              CASE('GENERATOR:FUEL CELL:EXHAUST GAS TO WATER HEAT EXCHANGER')
                ObjectName='Generator:FuelCell:ExhaustGasToWaterHeatExchanger'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Fixed Effectiveness',InArgs(6))) then
                  OutArgs(6)='FixedEffectiveness'
                endif
                if (samestring('LMTD Empirical UAeff',InArgs(6))) then
                  OutArgs(6)='EmpiricalUAeff'
                endif
                if (samestring('LMTD Fundemental UAeff',InArgs(6))) then
                  OutArgs(6)='FundementalUAeff'
                endif

              CASE('GENERATOR:FUEL CELL:ELECTRICAL STORAGE')
                ObjectName='Generator:FuelCell:ElectricalStorage'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Simple Efficiency with Constraints',InArgs(2))) then
                  OutArgs(2)='SimpleEfficiencyWithConstraints'
                endif

              CASE('GENERATOR:FUEL CELL:INVERTER')
                ObjectName='Generator:FuelCell:Inverter'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:FUEL CELL:STACK COOLER')
                ObjectName='Generator:FuelCell:StackCooler'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:MICRO CHP')
                ObjectName='Generator:MicroCHP'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('GENERATOR:MICRO CHP:NON NORMALIZED PARAMETERS')
                ObjectName='Generator:MicroCHP:NonNormalizedParameters'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Plant Control',InArgs(8))) then
                  OutArgs(8)='PlantControl'
                endif
                if (samestring('Internal Control',InArgs(8))) then
                  OutArgs(8)='InternalControl'
                endif
                if (samestring('Nominal Engine Temperature',InArgs(19))) then
                  OutArgs(19)='NominalEngineTemperature'
                endif
                if (samestring('Time Delay',InArgs(19))) then
                  OutArgs(19)='TimeDelay'
                endif
                if (samestring('Mandatory Cool-Down',InArgs(27))) then
                  OutArgs(27)='MandatoryCoolDown'
                endif
                if (samestring('Optional Cool-Down',InArgs(27))) then
                  OutArgs(27)='OptionalCoolDown'
                endif

              CASE('GENERATOR:FUEL SUPPLY')
                ObjectName='Generator:FuelSupply'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Temperature from Air Node',InArgs(2))) then
                  OutArgs(2)='TemperatureFromAirNode'
                endif
                if (samestring('Gaseous Constituents',InArgs(7))) then
                  OutArgs(7)='GaseousConstituents'
                endif
                if (samestring('Liquid Generic',InArgs(7))) then
                  OutArgs(7)='LiquidGeneric'
                endif
                DO Arg=13,CurArgs,2
                  if (samestring('CARBON DIOXIDE',InArgs(Arg))) then
                    OutArgs(Arg)='CarbonDioxide'
                  endif
                  if (samestring('NITROGEN',InArgs(Arg))) then
                    OutArgs(Arg)='Nitrogen'
                  endif
                  if (samestring('OXYGEN',InArgs(Arg))) then
                    OutArgs(Arg)='Oxygen'
                  endif
                  if (samestring('WATER',InArgs(Arg))) then
                    OutArgs(Arg)='Water'
                  endif
                  if (samestring('ARGON',InArgs(Arg))) then
                    OutArgs(Arg)='Argon'
                  endif
                  if (samestring('HYDROGEN',InArgs(Arg))) then
                    OutArgs(Arg)='Hydrogen'
                  endif
                  if (samestring('METHANE',InArgs(Arg))) then
                    OutArgs(Arg)='Methane'
                  endif
                  if (samestring('ETHANE',InArgs(Arg))) then
                    OutArgs(Arg)='Ethane'
                  endif
                  if (samestring('PROPANE',InArgs(Arg))) then
                    OutArgs(Arg)='Propane'
                  endif
                  if (samestring('BUTANE',InArgs(Arg))) then
                    OutArgs(Arg)='Butane'
                  endif
                  if (samestring('PENTANE',InArgs(Arg))) then
                    OutArgs(Arg)='Pentane'
                  endif
                  if (samestring('HEXANE',InArgs(Arg))) then
                    OutArgs(Arg)='Hexane'
                  endif
                  if (samestring('METHANOL',InArgs(Arg))) then
                    OutArgs(Arg)='Methanol'
                  endif
                  if (samestring('ETHANOL',InArgs(Arg))) then
                    OutArgs(Arg)='Ethanol'
                  endif
                ENDDO

              CASE('WATER USE EQUIPMENT')
                ObjectName='WaterUse:Equipment'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('WATER USE CONNECTIONS')
                ObjectName='WaterUse:Connections'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('NONE',InArgs(8))) then
                  OutArgs(8)='None'
                endif
                if (samestring('IDEAL',InArgs(8))) then
                  OutArgs(8)='Ideal'
                endif
                if (samestring('COUNTERFLOW',InArgs(8))) then
                  OutArgs(8)='CounterFlow'
                endif
                if (samestring('CROSSFLOW',InArgs(8))) then
                  OutArgs(8)='CrossFlow'
                endif
                if (samestring('PLANT',InArgs(9))) then
                  OutArgs(9)='Plant'
                endif
                if (samestring('EQUIPMENT',InArgs(9))) then
                  OutArgs(9)='Equipment'
                endif
                if (samestring('PLANT AND EQUIPMENT',InArgs(9))) then
                  OutArgs(9)='PlantAndEquipment'
                endif

              CASE('WATER STORAGE TANK')
                ObjectName='WaterUse:Storage'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('NONE',InArgs(8))) then
                  OutArgs(8)='None'
                endif
                if (samestring('MAINS',InArgs(8))) then
                  OutArgs(8)='Mains'
                endif
                if (samestring('GROUNDWATER WELL',InArgs(8))) then
                  OutArgs(8)='GroundwaterWell'
                endif
                if (samestring('OTHER TANK',InArgs(8))) then
                  OutArgs(8)='OtherTank'
                endif
                if (samestring('SCHEDULED TEMPERATURE',InArgs(13))) then
                  OutArgs(13)='ScheduledTemperature'
                endif
                if (samestring('THERMAL MODEL',InArgs(13))) then
                  OutArgs(13)='ThermalModel'
                endif
                if (samestring('SCHEDULE',InArgs(15))) then
                  OutArgs(15)='Schedule'
                endif
                if (samestring('ZONE',InArgs(15))) then
                  OutArgs(15)='Zone'
                endif
                if (samestring('EXTERIOR',InArgs(15))) then
                  OutArgs(15)='Outdoors'
                endif

              CASE('GROUNDWATER WELL')
                ObjectName='WaterUse:Well'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CONSTANT',InArgs(10))) then
                  OutArgs(10)='Constant'
                endif
                if (samestring('SCHEDULED',InArgs(10))) then
                  OutArgs(10)='Scheduled'
                endif

              CASE('RAINWATER COLLECTOR')
                ObjectName='WaterUse:RainCollector'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('CONSTANT',InArgs(3))) then
                  OutArgs(3)='Constant'
                endif
                if (samestring('SCHEDULED',InArgs(3))) then
                  OutArgs(3)='Scheduled'
                endif

              CASE('FLUIDNAMES')
                ObjectName='FluidProperties:Names'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do Arg=2,CurArgs,2
                  if (samestring('REFRIGERANT',InArgs(Arg))) then
                    OutArgs(Arg)='Refrigerant'
                  endif
                  if (samestring('GLYCOL',InArgs(Arg))) then
                    OutArgs(Arg)='Glycol'
                  endif
                enddo

              CASE('GLYCOLCONCENTRATIONS')
                ObjectName='FluidProperties:GlycolConcentrations'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FLUIDPROPERTYTEMPERATURES')
                ObjectName='FluidProperties:Temperatures'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FLUIDPROPERTYSATURATED')
                ObjectName='FluidProperties:Saturated'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('ENTHALPY',InArgs(2))) then
                  OutArgs(2)='Enthalpy'
                endif
                if (samestring('DENSITY',InArgs(2))) then
                  OutArgs(2)='Density'
                endif
                if (samestring('SPECIFICHEAT',InArgs(2))) then
                  OutArgs(2)='SpecificHeat'
                endif
                if (samestring('PRESSURE',InArgs(2))) then
                  OutArgs(2)='Pressure '
                endif
                if (samestring('FLUID',InArgs(3))) then
                  OutArgs(3)='Fluid'
                endif
                if (samestring('FLUIDGAS',InArgs(3))) then
                  OutArgs(3)='FluidGas'
                endif

              CASE('FLUIDPROPERTYSUPERHEATED')
                ObjectName='FluidProperties:Superheated'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('ENTHALPY',InArgs(2))) then
                  OutArgs(2)='Enthalpy'
                endif
                if (samestring('DENSITY',InArgs(2))) then
                  OutArgs(2)='Density'
                endif
                if (samestring('SPECIFICHEAT',InArgs(2))) then
                  OutArgs(2)='SpecificHeat'
                endif

              CASE('FLUIDPROPERTYCONCENTRATION')
                ObjectName='FluidProperties:Concentration'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('DENSITY',InArgs(2))) then
                  OutArgs(2)='Density'
                endif
                if (samestring('SPECIFICHEAT',InArgs(2))) then
                  OutArgs(2)='SpecificHeat'
                endif
                if (samestring('CONDUCTIVITY',InArgs(2))) then
                  OutArgs(2)='Conductivity'
                endif
                if (samestring('VISCOSITY',InArgs(2))) then
                  OutArgs(2)='Viscosity'
                endif

              CASE('COMPACT HVAC:THERMOSTAT')
                ObjectName='HVACTemplate:Thermostat'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COMPACT HVAC:ZONE:PURCHASED AIR')
                ObjectName='HVACTemplate:Zone:IdealLoadsAirSystem'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('COMPACT HVAC:ZONE:FAN COIL')
                ObjectName='HVACTemplate:Zone:FanCoil'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(5))) then
                  OutArgs(5)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(5))) then
                  OutArgs(5)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(5))) then
                  OutArgs(5)='Flow/Area'
                endif
                if (samestring('sum',InArgs(5))) then
                  OutArgs(5)='Sum'
                endif
                if (samestring('maximum',InArgs(5))) then
                  OutArgs(5)='Maximum'
                endif
                if (samestring('Chilled Water',InArgs(14))) then
                  OutArgs(14)='ChilledWater'
                endif
                if (samestring('Chilled Water Detailed Flat Model',InArgs(14))) then
                  OutArgs(14)='ChilledWaterDetailedFlatModel'
                endif
                if (samestring('Hot Water',InArgs(17))) then
                  OutArgs(17)='HotWater'
                endif

              CASE('COMPACT HVAC:ZONE:PTAC')
                ObjectName='HVACTemplate:Zone:PTAC'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(7))) then
                  OutArgs(7)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(7))) then
                  OutArgs(7)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(7))) then
                  OutArgs(7)='Flow/Area'
                endif
                if (samestring('sum',InArgs(7))) then
                  OutArgs(7)='Sum'
                endif
                if (samestring('maximum',InArgs(7))) then
                  OutArgs(7)='Maximum'
                endif
                if (samestring('Blow Through',InArgs(13))) then
                  OutArgs(13)='BlowThrough'
                endif
                if (samestring('Draw Through',InArgs(13))) then
                  OutArgs(13)='DrawThrough'
                endif
                if (samestring('Single-speed DX',InArgs(17))) then
                  OutArgs(17)='SingleSpeedDX'
                endif
                if (samestring('Hot Water',InArgs(22))) then
                  OutArgs(22)='HotWater'
                endif

              CASE('COMPACT HVAC:ZONE:PTHP')
                ObjectName='HVACTemplate:Zone:PTHP'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(7))) then
                  OutArgs(7)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(7))) then
                  OutArgs(7)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(7))) then
                  OutArgs(7)='Flow/Area'
                endif
                if (samestring('sum',InArgs(7))) then
                  OutArgs(7)='Sum'
                endif
                if (samestring('maximum',InArgs(7))) then
                  OutArgs(7)='Maximum'
                endif
                if (samestring('Blow Through',InArgs(13))) then
                  OutArgs(13)='BlowThrough'
                endif
                if (samestring('Draw Through',InArgs(13))) then
                  OutArgs(13)='DrawThrough'
                endif
                if (samestring('Single-speed DX',InArgs(17))) then
                  OutArgs(17)='SingleSpeedDX'
                endif
                if (samestring('Single-speed DX Heat Pump',InArgs(22))) then
                  OutArgs(22)='SingleSpeedDXHeatPump'
                endif
                if (samestring('Reverse-cycle',InArgs(28))) then
                  OutArgs(28)='ReverseCycle'
                endif
                if (samestring('On-demand',InArgs(29))) then
                  OutArgs(29)='OnDemand'
                endif

              CASE('COMPACT HVAC:ZONE:UNITARY')
                ObjectName='HVACTemplate:Zone:Unitary'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(6))) then
                  OutArgs(6)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(6))) then
                  OutArgs(6)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(6))) then
                  OutArgs(6)='Flow/Area'
                endif
                if (samestring('sum',InArgs(6))) then
                  OutArgs(6)='Sum'
                endif
                if (samestring('maximum',InArgs(6))) then
                  OutArgs(6)='Maximum'
                endif
                if (samestring('Hot Water',InArgs(12))) then
                  OutArgs(12)='HotWater'
                endif

              CASE('COMPACT HVAC:ZONE:VAV')
                ObjectName='HVACTemplate:Zone:VAV'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(7))) then
                  OutArgs(7)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(7))) then
                  OutArgs(7)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(7))) then
                  OutArgs(7)='Flow/Area'
                endif
                if (samestring('sum',InArgs(7))) then
                  OutArgs(7)='Sum'
                endif
                if (samestring('maximum',InArgs(7))) then
                  OutArgs(7)='Maximum'
                endif
                if (samestring('Hot Water',InArgs(11))) then
                  OutArgs(11)='HotWater'
                endif
                if (samestring('Reverse Action',InArgs(13))) then
                  OutArgs(13)='Reverse'
                endif
                if (samestring('Hot Water',InArgs(16))) then
                  OutArgs(16)='HotWater'
                endif

              CASE('COMPACT HVAC:ZONE:VAV:FAN POWERED')
                ObjectName='HVACTemplate:Zone:VAV:FanPowered'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('flow/person',InArgs(10))) then
                  OutArgs(10)='Flow/Person'
                endif
                if (samestring('flow/zone',InArgs(10))) then
                  OutArgs(10)='Flow/Zone'
                endif
                if (samestring('flow/area',InArgs(10))) then
                  OutArgs(10)='Flow/Area'
                endif
                if (samestring('sum',InArgs(10))) then
                  OutArgs(10)='Sum'
                endif
                if (samestring('maximum',InArgs(10))) then
                  OutArgs(10)='Maximum'
                endif
                if (samestring('Hot Water',InArgs(14))) then
                  OutArgs(14)='HotWater'
                endif
                if (samestring('Hot Water',InArgs(21))) then
                  OutArgs(21)='HotWater'
                endif

              CASE('COMPACT HVAC:SYSTEM:UNITARY')
                ObjectName='HVACTemplate:System:Unitary'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Single-speed DX',InArgs(10))) then
                  OutArgs(10)='SingleSpeedDX'
                endif
                If (SameString(InArgs(23),'Return air temperature')) THEN
                  OutArgs(23)='DifferentialDryBulb'
                ElseIf (SameString(InArgs(23),'Return air enthalpy')) THEN
                  OutArgs(23)= 'DifferentialEnthalpy'
                ElseIf (SameString(InArgs(23),'Return air temperature and enthalpy')) THEN
                  OutArgs(23)= 'DifferentialDryBulbAndEnthalpy'
                ElseIf (SameString(InArgs(23),'None')) THEN
                  OutArgs(23)= 'NoEconomizer'
                Else
                  OutArgs(23)= 'NoEconomizer'
                Endif
                if (samestring('NO LOCKOUT',InArgs(24))) then
                  OutArgs(24)='NoLockout'
                endif
                if (samestring('LOCKOUT WITH HEATING',InArgs(24))) then
                  OutArgs(24)='LockoutWithHeating'
                endif
                if (samestring('LOCKOUT WITH COMPRESSOR',InArgs(24))) then
                  OutArgs(24)='LockoutWithCompressor'
                endif
                if (samestring('Blow Through',InArgs(30))) then
                  OutArgs(30)='BlowThrough'
                endif
                if (samestring('Draw Through',InArgs(30))) then
                  OutArgs(30)='DrawThrough'
                endif
                if (samestring('Stay Off',InArgs(31))) then
                  OutArgs(31)='StayOff'
                endif
                if (samestring('Cycle On Any',InArgs(31))) then
                  OutArgs(31)='CycleOnAny'
                endif
                if (samestring('Cycle On Control Zone',InArgs(31))) then
                  OutArgs(31)='CycleOnControlZone'
                endif
                if (samestring('Cool-Reheat Heating Coil',InArgs(36))) then
                  OutArgs(36)='CoolReheatHeatingCoil'
                endif
                if (samestring('Cool-Reheat Desuperheater',InArgs(36))) then
                  OutArgs(36)='CoolReheatDesuperheater'
                endif
                if (samestring('Electric Steam',InArgs(39))) then
                  OutArgs(39)='ElectricSteam'
                endif

              CASE('COMPACT HVAC:SYSTEM:VAV')
                ObjectName='HVACTemplate:System:VAV'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Chilled Water',InArgs(9))) then
                  OutArgs(9)='ChilledWater'
                endif
                if (samestring('Chilled Water Detailed Flat Model',InArgs(9))) then
                  OutArgs(9)='ChilledWaterDetailedFlatModel'
                endif
                if (samestring('Hot Water',InArgs(13))) then
                  OutArgs(13)='HotWater'
                endif
                if (samestring('FIXED MINIMUM',InArgs(27))) then
                  OutArgs(27)='FixedMinimum'
                endif
                if (samestring('PROPORTIONAL MINIMUM',InArgs(27))) then
                  OutArgs(27)='ProportionalMinimum'
                endif
                If (SameString(InArgs(29),'Return air temperature')) THEN
                  OutArgs(29)='DifferentialDryBulb'
                ElseIf (SameString(InArgs(29),'Return air enthalpy')) THEN
                  OutArgs(29)= 'DifferentialEnthalpy'
                ElseIf (SameString(InArgs(29),'Return air temperature and enthalpy')) THEN
                  OutArgs(29)= 'DifferentialDryBulbAndEnthalpy'
                ElseIf (SameString(InArgs(29),'None')) THEN
                  OutArgs(29)= 'NoEconomizer'
                Else
                  OutArgs(29)= 'NoEconomizer'
                Endif
                if (samestring('NO LOCKOUT',InArgs(30))) then
                  OutArgs(30)='NoLockout'
                endif
                if (samestring('Draw Through',InArgs(36))) then
                  OutArgs(36)='DrawThrough'
                endif
                if (samestring('Blow Through',InArgs(36))) then
                  OutArgs(36)='BlowThrough'
                endif
                if (samestring('Inlet Vane Dampers',InArgs(37))) then
                  OutArgs(37)='InletVaneDampers'
                endif
                if (samestring('Outlet Dampers',InArgs(37))) then
                  OutArgs(37)='OutletDampers'
                endif
                if (samestring('Variable Speed Motor',InArgs(37))) then
                  OutArgs(37)='VariableSpeedMotor'
                endif
                if (samestring('ASHRAE 90.1-2004 Appendix G',InArgs(37))) then
                  OutArgs(37)='ASHRAE90.1-2004AppendixG'
                endif
                if (samestring('Stay Off',InArgs(38))) then
                  OutArgs(38)='StayOff'
                endif
                if (samestring('Cycle On Any',InArgs(38))) then
                  OutArgs(38)='CycleOnAny'
                endif
                if (samestring('Cycle On Control Zone',InArgs(38))) then
                  OutArgs(38)='CycleOnControlZone'
                endif
                if (samestring('Cycle On Any - Zone Fans Only',InArgs(38))) then
                  OutArgs(38)='CycleOnAnyZoneFansOnly'
                endif
                if (samestring('Outdoor Air Temperature Reset',InArgs(43))) then
                  OutArgs(43)='OutdoorAirTemperatureReset'
                endif
                if (samestring('Outdoor Air Temperature Reset',InArgs(44))) then
                  OutArgs(44)='OutdoorAirTemperatureReset'
                endif
                if (samestring('Cool-Reheat',InArgs(45))) then
                  OutArgs(45)='CoolReheat'
                endif
                if (samestring('Electric Steam',InArgs(48))) then
                  OutArgs(48)='ElectricSteam'
                endif

              CASE('COMPACT HVAC:PLANT:CHILLED WATER LOOP')
                ObjectName='HVACTemplate:Plant:ChilledWaterLoop'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('INTERMITTENT',InArgs(3))) then
                  OutArgs(3)='Intermittent'
                endif
                if (samestring('CONTINUOUS',InArgs(3))) then
                  OutArgs(3)='Continuous'
                endif
                if (samestring('User Defined',InArgs(4))) then
                  OutArgs(4)='UserDefined'
                endif
                if (samestring('Constant Flow No Secondary',InArgs(8))) then
                  OutArgs(8)='ConstantPrimaryNoSecondary'
                endif
                if (samestring('Constant Flow',InArgs(8))) then
                  OutArgs(8)='ConstantPrimaryNoSecondary'
                endif
                if (samestring('Variable Flow No Secondary',InArgs(8))) then
                  OutArgs(8)='VariablePrimaryNoSecondary'
                endif
                if (samestring('Variable Flow',InArgs(8))) then
                  OutArgs(8)='VariablePrimaryNoSecondary'
                endif
                if (samestring('Constant Primary Variable Secondary',InArgs(8))) then
                  OutArgs(8)='ConstantPrimaryVariableSecondary'
                endif
                if (samestring('Variable Primary Constant Secondary',InArgs(8))) then
                  OutArgs(8)='VariablePrimaryConstantSecondary'
                endif
                if (samestring('User Defined',InArgs(11))) then
                  OutArgs(11)='UserDefined'
                endif
                if (samestring('Outdoor Wet Bulb',InArgs(13))) then
                  OutArgs(13)='OutdoorWetBulbTemperature'
                endif
                if (samestring('Specified Setpoint',InArgs(13))) then
                  OutArgs(13)='SpecifiedSetpoint'
                endif
                if (samestring('Outdoor Air Temperature Reset',InArgs(17))) then
                  OutArgs(17)='OutdoorAirTemperatureReset'
                endif

              CASE('COMPACT HVAC:PLANT:CHILLER')
                ObjectName='HVACTemplate:Plant:Chiller'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Purchased Chilled Water',InArgs(2))) then
                  OutArgs(2)='DistrictChilledWater'
                endif
                if (samestring('Electric Centrifugal Chiller',InArgs(2))) then
                  OutArgs(2)='ElectricCentrifugalChiller'
                endif
                if (samestring('Electric Reciprocating Chiller',InArgs(2))) then
                  OutArgs(2)='ElectricReciprocatingChiller'
                endif
                if (samestring('AIR COOLED',InArgs(5))) then
                  OutArgs(5)='AirCooled'
                endif
                if (samestring('WATER COOLED',InArgs(5))) then
                  OutArgs(5)='WaterCooled'
                endif
                if (samestring('EVAP COOLED',InArgs(5))) then
                  OutArgs(5)='EvaporativelyCooled'
                endif

              CASE('COMPACT HVAC:PLANT:TOWER')
                ObjectName='HVACTemplate:Plant:Tower'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Single Speed',InArgs(2))) then
                  OutArgs(2)='SingleSpeed'
                endif
                if (samestring('Two Speed',InArgs(2))) then
                  OutArgs(2)='TwoSpeed'
                endif

              CASE('COMPACT HVAC:PLANT:HOT WATER LOOP')
                ObjectName='HVACTemplate:Plant:HotWaterLoop'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('INTERMITTENT',InArgs(3))) then
                  OutArgs(3)='Intermittent'
                endif
                if (samestring('CONTINUOUS',InArgs(3))) then
                  OutArgs(3)='Continuous'
                endif
                if (samestring('User Defined',InArgs(4))) then
                  OutArgs(4)='UserDefined'
                endif
                if (samestring('Variable Flow',InArgs(8))) then
                  OutArgs(8)='VariableFlow'
                endif
                if (samestring('Constant Flow',InArgs(8))) then
                  OutArgs(8)='ConstantFlow'
                endif
                if (samestring('Outdoor Air Temperature Reset',InArgs(10))) then
                  OutArgs(10)='OutdoorAirTemperatureReset'
                endif

              CASE('COMPACT HVAC:PLANT:BOILER')
                ObjectName='HVACTemplate:Plant:Boiler'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Purchased Hot Water',InArgs(2))) then
                  OutArgs(2)='DistrictHotWater'
                endif
                if (samestring('Hot Water Boiler',InArgs(2))) then
                  OutArgs(2)='HotWaterBoiler'
                endif

              CASE('ECONOMICS:COMPONENT COST:ADJUSTMENTS')
                ObjectName='ComponentCost:Adjustments'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:COMPONENT COST:REFERENCE')
                ObjectName='ComponentCost:Reference'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:COMPONENT COST:LINE ITEM')
                ObjectName='ComponentCost:LineItem'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('GENERAL',InArgs(3))) then
                  OutArgs(3)='General'
                endif
                if (samestring('CONSTRUCTION',InArgs(3))) then
                  OutArgs(3)='Construction'
                endif
                if (samestring('COIL:DX',InArgs(3))) then
                  OutArgs(3)='Coil:DX'
                endif
                if (samestring('COIL:DX:COOLINGBYPASSFACTOREMPIRICAL',InArgs(3))) then
                  OutArgs(3)='Coil:Cooling:DX:SingleSpeed'
                endif
                if (samestring('COIL:GAS:HEATING',InArgs(3))) then
                  OutArgs(3)='Coil:Heating:Gas'
                endif
                if (samestring('CHILLER:ELECTRIC',InArgs(3))) then
                  OutArgs(3)='Chiller:Electric'
                endif
                if (samestring('DAYLIGHTING:DETAILED',InArgs(3))) then
                  OutArgs(3)='Daylighting:Controls'
                endif
                if (samestring('SURFACE:SHADING:ATTACHED',InArgs(3))) then
                  OutArgs(3)='Shading:Zone:Detailed'
                endif
                if (samestring('LIGHTS',InArgs(3))) then
                  OutArgs(3)='Lights'
                endif
                if (samestring('GENERATOR:PV:SIMPLE',InArgs(3))) then
                  OutArgs(3)='Generator:Photovoltaic'
                endif

              CASE('ECONOMICS:TARIFF')
                ObjectName='UtilityCost:Tariff'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('quarterHour',InArgs(9))) then
                  OutArgs(9)='QuarterHour'
                endif
                if (samestring('halfHour',InArgs(9))) then
                  OutArgs(9)='HalfHour'
                endif
                if (samestring('fullHour',InArgs(9))) then
                  OutArgs(9)='FullHour'
                endif
                if (samestring('day',InArgs(9))) then
                  OutArgs(9)='Day'
                endif
                if (samestring('week',InArgs(9))) then
                  OutArgs(9)='Week'
                endif
                if (samestring('buyFromUtility',InArgs(15))) then
                  OutArgs(15)='BuyFromUtility'
                endif
                if (samestring('sellToUtility',InArgs(15))) then
                  OutArgs(15)='SellToUtility'
                endif
                if (samestring('netMetering',InArgs(15))) then
                  OutArgs(15)='NetMetering'
                endif

              CASE('ECONOMICS:QUALIFY')
                ObjectName='UtilityCost:Qualify'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:CHARGE:SIMPLE')
                ObjectName='UtilityCost:Charge:Simple'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:CHARGE:BLOCK')
                ObjectName='UtilityCost:Charge:Block'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:RATCHET')
                ObjectName='UtilityCost:Ratchet'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:VARIABLE')
                ObjectName='UtilityCost:Variable'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:COMPUTATION')
                ObjectName='UtilityCost:Computation'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('ECONOMICS:CURRENCYTYPE')
                ObjectName='CurrencyType'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

!              CASE('REPORT VARIABLE')
!                ObjectName='Output:Variable'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!
!              CASE('REPORT METER')
!                ObjectName='Output:Meter'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!
!              CASE('REPORT METERFILEONLY')
!                ObjectName='Output:Meter:MeterFileOnly'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!
!              CASE('REPORT CUMULATIVE METER')
!                ObjectName='Output:Meter:Cumulative'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!
!              CASE('REPORT CUMULATIVE METERFILEONLY')
!                ObjectName='Output:Meter:Cumulative:MeterFileOnly'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!
              CASE('OUTPUT:SQLITE')
                ObjectName='Output:SQLite'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REPORT ENVIRONMENTAL IMPACT FACTORS')
                ObjectName='Output:EnvironmentalImpactFactors'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('timestep',InArgs(1))) then
                  OutArgs(1)='Timestep'
                endif
                if (samestring('hourly',InArgs(1))) then
                  OutArgs(1)='Hourly'
                endif
                if (samestring('daily',InArgs(1))) then
                  OutArgs(1)='Daily'
                endif
                if (samestring('monthly',InArgs(1))) then
                  OutArgs(1)='Monthly'
                endif
                if (samestring('runperiod',InArgs(1))) then
                  OutArgs(1)='RunPeriod'
                endif

              CASE('ENVIRONMENTAL IMPACT FACTORS')
                ObjectName='EnvironmentalImpactFactors'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('FUEL FACTORS')
                ObjectName='FuelFactors'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)

              CASE('REPORT:SURFACECOLORSCHEME')
                ObjectName='OutputControl:SurfaceColorScheme'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                DO Arg=2,CurArgs,2
                  if (samestring('Detached Building Shades',InArgs(Arg))) then
                    OutArgs(Arg)='DetachedBuildingShades'
                  endif
                  if (samestring('Detached Fixed Shades',InArgs(Arg))) then
                    OutArgs(Arg)='DetachedFixedShades'
                  endif
                  if (samestring('Attached Building Shades',InArgs(Arg))) then
                    OutArgs(Arg)='AttachedBuildingShades'
                  endif
                  if (samestring('Daylight Reference Point1',InArgs(Arg))) then
                    OutArgs(Arg)='DaylightReferencePoint1'
                  endif
                  if (samestring('Daylight Reference Point2',InArgs(Arg))) then
                    OutArgs(Arg)='DaylightReferencePoint2'
                  endif
                  if (samestring('TDD:Domes',InArgs(Arg))) then
                    OutArgs(Arg)='TubularDaylightDomes'
                  endif
                  if (samestring('TDD:Diffusers',InArgs(Arg))) then
                    OutArgs(Arg)='TubularDaylightDiffusers'
                  endif
                ENDDO

              CASE('REPORT')
                ObjectName='Output:Reports'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('Variable Dictionary',InArgs(1))) then
                  OutArgs(1)='VariableDictionary'
                endif
                if (samestring('LINES',InArgs(2))) then
                  OutArgs(2)='Lines'
                endif
                if (samestring('DETAILS',InArgs(2))) then
                  OutArgs(2)='Details'
                endif
                if (samestring('hourly',InArgs(2))) then
                  OutArgs(2)='Hourly'
                endif
                if (samestring('timestep',InArgs(2))) then
                  OutArgs(2)='Timestep'
                endif
                if (samestring('Triangulate 3DFace',InArgs(3))) then
                  OutArgs(3)='Triangulate3DFace'
                endif
                if (samestring('Thick Polyline',InArgs(3))) then
                  OutArgs(3)='ThickPolyline'
                endif
                if (samestring('Regular Polyline',InArgs(3))) then
                  OutArgs(3)='RegularPolyline'
                endif

              CASE('REPORT:TABLE:STYLE')
                ObjectName='OutputControl:Table:Style'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('comma',InArgs(1))) then
                  OutArgs(1)='Comma'
                endif
                if (samestring('tab',InArgs(1))) then
                  OutArgs(1)='Tab'
                endif
                if (samestring('fixed',InArgs(1))) then
                  OutArgs(1)='Fixed'
                endif
                if (samestring('comma and HTML',InArgs(1))) then
                  OutArgs(1)='CommaAndHTML'
                endif
                if (samestring('tab and HTML',InArgs(1))) then
                  OutArgs(1)='TabAndHTML'
                endif
                if (samestring('all',InArgs(1))) then
                  OutArgs(1)='All'
                endif

              CASE('REPORT:TABLE:PREDEFINED')
                ObjectName='Output:Table:SummaryReports'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                DO Arg=1,CurArgs
                  if (samestring('All Summary',InArgs(Arg))) then
                    OutArgs(Arg)='AllSummary'
                  endif
                  if (samestring('All Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='AllMonthly'
                  endif
                  if (samestring('All Summary and Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='AllSummaryAndMonthly'
                  endif
                  if (samestring('Annual Building Utility Performance Summary',InArgs(Arg))) then
                    OutArgs(Arg)='AnnualBuildingUtilityPerformanceSummary'
                  endif
                  if (samestring('Input Verification and Results Summary',InArgs(Arg))) then
                    OutArgs(Arg)='InputVerificationandResultsSummary'
                  endif
                  if (samestring('Climatic Data Summary',InArgs(Arg))) then
                    OutArgs(Arg)='ClimaticDataSummary'
                  endif
                  if (samestring('Climate Summary',InArgs(Arg))) then
                    OutArgs(Arg)='ClimaticDataSummary'
                  endif
                  if (samestring('Equipment Summary',InArgs(Arg))) then
                    OutArgs(Arg)='EquipmentSummary'
                  endif
                  if (samestring('Envelope Summary',InArgs(Arg))) then
                    OutArgs(Arg)='EnvelopeSummary'
                  endif
                  if (samestring('Surface Shadowing Summary',InArgs(Arg))) then
                    OutArgs(Arg)='SurfaceShadowingSummary'
                  endif
                  if (samestring('Shading Summary',InArgs(Arg))) then
                    OutArgs(Arg)='ShadingSummary'
                  endif
                  if (samestring('Lighting Summary',InArgs(Arg))) then
                    OutArgs(Arg)='LightingSummary'
                  endif
                  if (samestring('HVAC Sizing Summary',InArgs(Arg))) then
                    OutArgs(Arg)='HVACSizingSummary'
                  endif
                  if (samestring('System Summary',InArgs(Arg))) then
                    OutArgs(Arg)='SystemSummary'
                  endif
                  if (samestring('Component Sizing Summary',InArgs(Arg))) then
                    OutArgs(Arg)='ComponentSizingSummary'
                  endif
                  if (samestring('Outside Air Summary',InArgs(Arg))) then
                    OutArgs(Arg)='OutdoorAirSummary'
                  endif
                  if (samestring('Object Count Summary',InArgs(Arg))) then
                    OutArgs(Arg)='ObjectCountSummary'
                  endif
                  if (samestring('Component Cost Economics Summary',InArgs(Arg))) then
                    OutArgs(Arg)='ComponentCostEconomicsSummary'
                  endif
                  if (samestring('Zone Cooling Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneCoolingSummaryMonthly'
                  endif
                  if (samestring('Zone Heating Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneHeatingSummaryMonthly'
                  endif
                  if (samestring('Zone Electric Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneElectricSummaryMonthly'
                  endif
                  if (samestring('Space Gains Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='SpaceGainsMonthly'
                  endif
                  if (samestring('Peak Space Gains Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakSpaceGainsMonthly'
                  endif
                  if (samestring('Space Gain Components at Cooling Peak Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='SpaceGainComponentsAtCoolingPeakMonthly'
                  endif
                  if (samestring('Energy Consumption - Electricity & Natural Gas Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EnergyConsumptionElectricityNaturalGasMonthly'
                  endif
                  if (samestring('Energy Consumption - Electricity Generated & Propane Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EnergyConsumptionElectricityGeneratedPropaneMonthly'
                  endif
                  if (samestring('Energy Consumption - Diesel & Fuel Oil Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EnergyConsumptionDieselFuelOilMonthly'
                  endif
                  if (samestring('Energy Consumption  - Purchased Heating & Cooling Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EnergyConsumptionDistrictHeatingCoolingMonthly'
                  endif
                  if (samestring('Energy Consumption - Coal & Gasoline Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EnergyConsumptionCoalGasolineMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Electricity Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionElectricityMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Natural Gas Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionNaturalGasMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Diesel Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionDieselMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Fuel Oil Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionFuelOilMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Coal Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionCoalMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Propane Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionPropaneMonthly'
                  endif
                  if (samestring('End-Use Energy Consumption - Gasoline Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='EndUseEnergyConsumptionGasolineMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Electricity Part 1 Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseElectricityPart1Monthly'
                  endif
                  if (samestring('Peak Energy End-Use - Electricity Part 2 Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseElectricityPart2Monthly'
                  endif
                  if (samestring('Electric Components of Peak Demand Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ElectricComponentsOfPeakDemandMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Natural Gas Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseNaturalGasMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Diesel Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseDieselMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Fuel Oil Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseFuelOilMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Coal Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseCoalMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Propane Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUsePropaneMonthly'
                  endif
                  if (samestring('Peak Energy End-Use - Gasoline Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PeakEnergyEndUseGasolineMonthly'
                  endif
                  if (samestring('Setpoints Not Met With Temperatures Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='SetpointsNotMetWithTemperaturesMonthly'
                  endif
                  if (samestring('Comfort Report - Simple 55 Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ComfortReportSimple55Monthly'
                  endif
                  if (samestring('Unglazed Transpired Solar Collector Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='UnglazedTranspiredSolarCollectorSummaryMonthly'
                  endif
                  if (samestring('Occupant Comfort Data Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='OccupantComfortDataSummaryMonthly'
                  endif
                  if (samestring('Chiller Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ChillerReportMonthly'
                  endif
                  if (samestring('Tower Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='TowerReportMonthly'
                  endif
                  if (samestring('Boiler Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='BoilerReportMonthly'
                  endif
                  if (samestring('DX Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='DXReportMonthly'
                  endif
                  if (samestring('Window Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='WindowReportMonthly'
                  endif
                  if (samestring('Window Energy Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='WindowEnergyReportMonthly'
                  endif
                  if (samestring('Window Zone Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='WindowZoneSummaryMonthly'
                  endif
                  if (samestring('Window Energy Zone Summary Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='WindowEnergyZoneSummaryMonthly'
                  endif
                  if (samestring('Average Outdoor Conditions Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='AverageOutdoorConditionsMonthly'
                  endif
                  if (samestring('Outdoor Conditions Maximum Drybulb Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='OutdoorConditionsMaximumDryBulbMonthly'
                  endif
                  if (samestring('Outdoor Conditions Minimum Drybulb Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='OutdoorConditionsMinimumDryBulbMonthly'
                  endif
                  if (samestring('Outdoor Conditions Maximum Wetbulb Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='OutdoorConditionsMaximumWetBulbMonthly'
                  endif
                  if (samestring('Outdoor Conditions Maximum Dew Point Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='OutdoorConditionsMaximumDewPointMonthly'
                  endif
                  if (samestring('Outdoor Ground Conditions Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='OutdoorGroundConditionsMonthly'
                  endif
                  if (samestring('Window AC Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='WindowACReportMonthly'
                  endif
                  if (samestring('Water Heater Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='WaterHeaterReportMonthly'
                  endif
                  if (samestring('Generator Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='GeneratorReportMonthly'
                  endif
                  if (samestring('Daylighting Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='DaylightingReportMonthly'
                  endif
                  if (samestring('Coil Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='CoilReportMonthly'
                  endif
                  if (samestring('Plant Loop Demand Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PlantLoopDemandReportMonthly'
                  endif
                  if (samestring('Fan Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='FanReportMonthly'
                  endif
                  if (samestring('Pump Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='PumpReportMonthly'
                  endif
                  if (samestring('Cond Loop Demand Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='CondLoopDemandReportMonthly'
                  endif
                  if (samestring('Zone Temperature Oscillation Report Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='ZoneTemperatureOscillationReportMonthly'
                  endif
                  if (samestring('Air Loop System Energy and Water Use Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='AirLoopSystemEnergyAndWaterUseMonthly'
                  endif
                  if (samestring('Air Loop System Component Loads Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='AirLoopSystemComponentLoadsMonthly'
                  endif
                  if (samestring('Air Loop System Component Energy Use Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='AirLoopSystemComponentEnergyUseMonthly'
                  endif
                  if (samestring('Mechanical Ventilation Loads Monthly',InArgs(Arg))) then
                    OutArgs(Arg)='MechanicalVentilationLoadsMonthly'
                  endif
                ENDDO

!              CASE('REPORT:TABLE:TIMEBINS')
!                ObjectName='Output:Table:TimeBins'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!
!              CASE('REPORT:TABLE:MONTHLY')
!                ObjectName='Output:Table:Monthly'
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
!                nodiff=.false.
!                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
!                if (samestring('ValueWhenMaxMin',InArgs(4))) then
!                  OutArgs(4)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(6))) then
!                  OutArgs(6)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(8))) then
!                  OutArgs(8)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(10))) then
!                  OutArgs(10)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(12))) then
!                  OutArgs(12)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(14))) then
!                  OutArgs(14)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(16))) then
!                  OutArgs(16)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(18))) then
!                  OutArgs(18)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(20))) then
!                  OutArgs(20)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(22))) then
!                  OutArgs(22)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(24))) then
!                  OutArgs(24)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(26))) then
!                  OutArgs(26)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(28))) then
!                  OutArgs(28)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(30))) then
!                  OutArgs(30)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(32))) then
!                  OutArgs(32)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(34))) then
!                  OutArgs(34)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(36))) then
!                  OutArgs(36)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(38))) then
!                  OutArgs(38)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(40))) then
!                  OutArgs(40)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(42))) then
!                  OutArgs(42)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(44))) then
!                  OutArgs(44)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(46))) then
!                  OutArgs(46)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(48))) then
!                  OutArgs(48)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(50))) then
!                  OutArgs(50)='ValueWhenMaximumOrMinimum'
!                endif
!                if (samestring('ValueWhenMaxMin',InArgs(52))) then
!                  OutArgs(52)='ValueWhenMaximumOrMinimum'
!                endif
!


    !!!   Changes for report variables, meters, tables -- update names

              CASE('REPORT VARIABLE')
                CALL GetNewObjectDefInIDD('Output:Variable',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                ObjectName='Output:Variable'
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.false.
                if (samestring('detailed',InArgs(3))) then
                  OutArgs(3)='Detailed'
                endif
                if (samestring('timestep',InArgs(3))) then
                  OutArgs(3)='Timestep'
                endif
                if (samestring('hourly',InArgs(3))) then
                  OutArgs(3)='Hourly'
                endif
                if (samestring('daily',InArgs(3))) then
                  OutArgs(3)='Daily'
                endif
                if (samestring('monthly',InArgs(3))) then
                  OutArgs(3)='Monthly'
                endif
                if (samestring('runperiod',InArgs(3))) then
                  OutArgs(3)='RunPeriod'
                endif
                if (samestring('environment',InArgs(3))) then
                  OutArgs(3)='Environment'
                endif
                if (samestring('annual',InArgs(3))) then
                  OutArgs(3)='Annual'
                endif
                IF (OutArgs(1) == Blank) THEN
                  OutArgs(1)='*'
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
                IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'REPORT METER') THEN
                  ObjectName='Output:Meter'
                ELSEIF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'REPORT METERFILEONLY') THEN
                  ObjectName='Output:Meter:MeterFileOnly'
                ELSEIF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'REPORT CUMULATIVE METER') THEN
                  ObjectName='Output:Meter:Cumulative'
                ELSEIF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'REPORT CUMULATIVE METERFILEONLY') THEN
                  ObjectName='Output:Meter:Cumulative:MeterFileOnly'
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                if (samestring('timestep',InArgs(2))) then
                  OutArgs(2)='Timestep'
                endif
                if (samestring('hourly',InArgs(2))) then
                  OutArgs(2)='Hourly'
                endif
                if (samestring('daily',InArgs(2))) then
                  OutArgs(2)='Daily'
                endif
                if (samestring('monthly',InArgs(2))) then
                  OutArgs(2)='Monthly'
                endif
                if (samestring('runperiod',InArgs(2))) then
                  OutArgs(2)='RunPeriod'
                endif
                if (samestring('environment',InArgs(2))) then
                  OutArgs(2)='Environment'
                endif
                if (samestring('annual',InArgs(2))) then
                  OutArgs(2)='Annual'
                endif
                nodiff=.false.
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

              CASE('REPORT:TABLE:TIMEBINS')
                CALL GetNewObjectDefInIDD('Output:Table:TimeBins',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                ObjectName='Output:Table:TimeBins'
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
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

              CASE('REPORT:TABLE:MONTHLY')
                CALL GetNewObjectDefInIDD('Output:Table:Monthly',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                ObjectName='Output:Table:Monthly'
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
                DO Var=4,CurArgs,2
                  if (samestring('ValueWhenMaxMin',InArgs(Var))) then
                    OutArgs(Var)='ValueWhenMaximumOrMinimum'
                  endif
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
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ' '
            ENDDO
          ENDIF

          IF (GetNumSectionsFound('Report Variable Dictionary') > 0) THEN
            ObjectName='Output:Reports'
            CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
            nodiff=.false.
            OutArgs(1)='VariableDictionary'
            CurArgs=1
            CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
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

!END SUBROUTINE CreateNewIDFUsingRulesV3_0_0
