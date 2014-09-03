!SUBROUTINE CreateNewIDFUsingRulesV2_2_0(EndOfFile,DiffOnly,InLfn,AskForInput,InputFileName,ArgFile,ArgIDFExtension)

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
  LOGICAL :: allzeroes
  INTEGER :: OArg
  INTEGER :: NumSets  ! count number of generator sets
  INTEGER :: SetNum
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: DeleteThisRecord
  LOGICAL :: Success
  INTEGER :: PrimaryLoopNum

  INTEGER SaveNumber
  LOGICAL :: ErrFlag

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
                        write(DifLfn,fmta) 'Preprocessor Message,'//trim(ProgNameConversion)//', Severe,'
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
                        write(DifLfn,fmta) 'Preprocessor Message,'//trim(ProgNameConversion)//', Severe,'
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
              OutArgs(1)='2.2'
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
                IF (InArgs(1)(1:3) == '2.2' .and. ArgFile) THEN
                  CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                  CLOSE(diflfn,STATUS='DELETE')
                  LatestVersion=.true.
                  EXIT
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='2.2'
                nodiff=.false.

    !!!    Changes for this version

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

              CASE('BRANCH')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.true.
                DO Arg=3,CurArgs,5
                  SELECT CASE (MakeUPPERCase(OutArgs(Arg)))
                    CASE('DOMESTIC HOT WATER')
                      OutArgs(Arg)='Water Use Connections'
                      nodiff=.false.
                    CASE DEFAULT
                  END SELECT
                ENDDO

              CASE ('AIR CONDITIONER:WINDOW:CYCLING')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                nodiff=.false.
                IF (MakeUPPERCase(InArgs(12)) == 'CYCFANCYCCOMP') THEN
                  OutArgs(12)='Air Conditioner:Window '//TRIM(InArgs(1))//' Cycling Fan Schedule'
                  allzeroes=.true.
                ELSEIF (MakeUPPERCase(InArgs(12)) == 'CONTFANCYCCOMP') THEN
                  OutArgs(12)='Air Conditioner:Window '//TRIM(InArgs(1))//' Continuous Fan Schedule'
                  allzeroes=.false.
                ELSE
                  OutArgs(12)='Invalid Supply air fan operating mode '//TRIM(InArgs(12))
                ENDIF
                !  Need to write this out plus more objects....
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                IF (OutArgs(12)(1:7) == 'Invalid') CYCLE

                ! Add ScheduleType
                CALL GetNewObjectDefInIDD('SCHEDULETYPE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=TRIM(OutArgs(12))//' Type'
                CurArgs=1
                CALL WriteOutIDFLines(DifLfn,'SCHEDULETYPE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                ! Add Compact Schedule
                CALL GetNewObjectDefInIDD('SCHEDULE:COMPACT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=OutArgs(12)
                OutArgs(2)=TRIM(OutArgs(12))//' Type'
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

              CASE ('PACKAGEDTERMINAL:HEATPUMP:AIRTOAIR')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                OutArgs(5:25)=InArgs(7:27)
                CurArgs=CurArgs-2
                nodiff=.false.
                IF (MakeUPPERCase(InArgs(28)) == 'CYCFANCYCCOMP') THEN
                  OutArgs(26)='PackagedTerminal:HeatPump:AirToAir '//TRIM(InArgs(1))//' Cycling Fan Schedule'
                  allzeroes=.true.
                ELSEIF (MakeUPPERCase(InArgs(28)) == 'CONTFANCYCCOMP') THEN
                  OutArgs(26)='PackagedTerminal:HeatPump:AirToAir '//TRIM(InArgs(1))//' Continuous Fan Schedule'
                  allzeroes=.false.
                ELSE
                  OutArgs(26)='Invalid Supply air fan operating mode '//TRIM(InArgs(28))
                ENDIF
                !  Need to write this out plus more objects....
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                IF (OutArgs(28)(1:7) == 'Invalid') CYCLE

                ! Add ScheduleType
                CALL GetNewObjectDefInIDD('SCHEDULETYPE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=TRIM(OutArgs(26))//' Type'
                CurArgs=1
                CALL WriteOutIDFLines(DifLfn,'SCHEDULETYPE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                ! Add Compact Schedule
                CALL GetNewObjectDefInIDD('SCHEDULE:COMPACT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=OutArgs(26)
                OutArgs(2)=TRIM(OutArgs(26))//' Type'
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

              CASE ('UNITARYSYSTEM:HEATPUMP:WATERTOAIR')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:25)=InArgs(1:25)
                nodiff=.false.
                IF (MakeUPPERCase(InArgs(26)) == 'CYCFANCYCCOMP') THEN
                  OutArgs(26)='UnitarySystem:HeatPump:WaterToAir '//TRIM(InArgs(1))//' Cycling Fan Schedule'
                  allzeroes=.true.
                ELSEIF (MakeUPPERCase(InArgs(26)) == 'CONTFANCYCCOMP') THEN
                  OutArgs(26)='UnitarySystem:HeatPump:WaterToAir '//TRIM(InArgs(1))//' Continuous Fan Schedule'
                  allzeroes=.false.
                ELSE
                  OutArgs(26)='Invalid Supply air fan operating mode '//TRIM(InArgs(26))
                ENDIF
                !  Need to write this out plus more objects....
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                IF (OutArgs(26)(1:7) == 'Invalid') CYCLE

                ! Add ScheduleType
                CALL GetNewObjectDefInIDD('SCHEDULETYPE',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=TRIM(OutArgs(26))//' Type'
                CurArgs=1
                CALL WriteOutIDFLines(DifLfn,'SCHEDULETYPE',CurArgs,OutArgs,NwFldNames,NwFldUnits)

                ! Add Compact Schedule
                CALL GetNewObjectDefInIDD('SCHEDULE:COMPACT',NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=OutArgs(26)
                OutArgs(2)=TRIM(OutArgs(26))//' Type'
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

              CASE('CONTROLLER:STAND ALONE ERV')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                nodiff=.false.
                OutArgs(2:4)=InArgs(3:5)
                CurArgs=CurArgs-1
                do arg=2,4
                  if (processnumber(OutArgs(arg),errflag) == 0.0) OutArgs(arg)=Blank
                enddo
                OutArgs(5:6)=Blank
                OutArgs(7:8)=InArgs(8:9)

              CASE('ENERGY RECOVERY VENTILATOR:STAND ALONE')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                nodiff=.false.
                OutArgs(5)=InArgs(7)
                OutArgs(6:8)=InArgs(10:12)
                CurArgs=CurArgs-4

              CASE('CONTROLLER:OUTSIDE AIR')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:13)=InArgs(1:13)
                do arg=11,13
                  if (processnumber(OutArgs(arg),errflag) == 0.0) OutArgs(arg)=Blank
                enddo
                OutArgs(14:15)=Blank
                OutArgs(16:19)=InArgs(14:17)
                CurArgs=CurArgs+2


              CASE('COMPACT HVAC:SYSTEM:UNITARY')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do arg=25,27
                  if (processnumber(OutArgs(arg),errflag) == 0.0) OutArgs(arg)=Blank
                enddo

              CASE('COMPACT HVAC:SYSTEM:VAV')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                do arg=31,33
                  if (processnumber(OutArgs(arg),errflag) == 0.0) OutArgs(arg)=Blank
                enddo

              CASE ('PEOPLE')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:2)=InArgs(1:2)
                nodiff=.false.
                OutArgs(3)=InArgs(4)
                OutArgs(4)='people'
                OutArgs(5)=InArgs(3)
                OutArgs(6:7)=blank
                OutArgs(8)=InArgs(5)
                if (CurArgs >= 15) then  ! user sensible fraction
                  OutArgs(9)=InArgs(15)
                else
                  OutArgs(9)=blank
                endif
                OutArgs(10)=InArgs(6)
                if (CurArgs >= 16) then  ! ashrae comfort
                  OutArgs(11)=InArgs(16)
                else
                  OutArgs(11)=blank
                endif
                OutArgs(12)=InArgs(7)
                OutArgs(13:19)=InArgs(8:14)
                Arg=CurArgs
                DO Var=19,1,-1
                  IF (OutArgs(Var) == blank) cycle
                  CurArgs=Var
                  EXIT
                ENDDO

              CASE ('LIGHTS')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                nodiff=.false.
                OutArgs(4)='lighting level'
                OutArgs(5)=InArgs(4)
                OutArgs(6:7)=blank
                OutArgs(8:15)=InArgs(5:12)
                DO Var=15,1,-1
                  IF (OutArgs(Var) == blank) cycle
                  CurArgs=Var
                  EXIT
                ENDDO

              CASE ('ELECTRIC EQUIPMENT', 'GAS EQUIPMENT', 'HOT WATER EQUIPMENT', 'STEAM EQUIPMENT', 'OTHER EQUIPMENT')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                nodiff=.false.
                OutArgs(4)='equipment level'
                OutArgs(5)=InArgs(4)
                OutArgs(6:7)=blank
                OutArgs(8:11)=InArgs(5:8)
                DO Var=11,1,-1
                  IF (OutArgs(Var) == blank) cycle
                  CurArgs=Var
                  EXIT
                ENDDO

              CASE ('INFILTRATION')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                nodiff=.false.
                OutArgs(4)='flow/zone'
                OutArgs(5)=InArgs(4)
                OutArgs(6:8)=blank
                OutArgs(9:12)=InArgs(5:8)
                DO Var=12,1,-1
                  IF (OutArgs(Var) == blank) cycle
                  CurArgs=Var
                  EXIT
                ENDDO

              CASE ('MIXING', 'CROSS MIXING')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                nodiff=.false.
                OutArgs(4)='flow/zone'
                OutArgs(5)=InArgs(4)
                OutArgs(6:8)=blank
                OutArgs(9:12)=InArgs(5:8)
                DO Var=12,1,-1
                  IF (OutArgs(Var) == blank) cycle
                  CurArgs=Var
                  EXIT
                ENDDO

              CASE ('VENTILATION')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                nodiff=.false.
                OutArgs(4)='flow/zone'
                OutArgs(5)=InArgs(4)
                OutArgs(6:8)=blank
                OutArgs(9:15)=InArgs(7:13)
                OutArgs(16)=InArgs(5)
                OutArgs(17)=blank
                OutArgs(18)=InArgs(14)
                OutArgs(19)=blank
                OutArgs(20)=InArgs(6)
                OutArgs(21)=blank
                OutArgs(22)=InArgs(15)
                OutArgs(23)=blank
                OutArgs(24)=InArgs(16)
                OutArgs(25)=blank
                OutArgs(26)=InArgs(17)
                DO Var=26,1,-1
                  IF (OutArgs(Var) == blank) cycle
                  CurArgs=Var
                  EXIT
                ENDDO

              CASE ('GENERATOR:PV:EQUIVALENT ONE-DIODE')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                nodiff=.false.
                Errflag=.false.
                SaveNumber=ProcessNumber(InArgs(4),Errflag)
                IF (.not. Errflag) THEN
                  IF (SaveNumber == 1) THEN
                    OutArgs(4)='Decoupled NOCT Conditions'
                  ELSEIF (SaveNumber == 2) THEN
                    OutArgs(4)='Decoupled Ulleberg Dynamic'
                  ELSE
                    OutArgs(4)=Blank
                  ENDIF
                ELSE
                  OutArgs(4)=Blank
                ENDIF
                OutArgs(5:24)=InArgs(5:24)
                OutArgs(25)='1.0'
                OutArgs(26)='1.E+6'
                CurArgs=26

              CASE ('ELECTRIC LOAD CENTER:GENERATORS')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                nodiff=.false.
                NumSets=0
                do Arg=2,CurArgs,4
                  if (InArgs(Arg) /= blank) NumSets=NumSets+1
                enddo
                OArg=2
                Arg=2
                do SetNum=1,NumSets
                  OutArgs(OArg:OArg+3)=InArgs(Arg:Arg+3)
                  OutArgs(OArg+4)=blank
                  OArg=OArg+5
                  Arg=Arg+4
                enddo
                CurArgs=OArg-1

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

!END SUBROUTINE CreateNewIDFUsingRulesV2_2_0
