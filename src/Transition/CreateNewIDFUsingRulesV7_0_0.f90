!SUBROUTINE CreateNewIDFUsingRulesV7_0_0(EndOfFile,DiffOnly,InLfn,AskForInput,InputFileName,ArgFile,ArgIDFExtension)

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
  TYPE FanData
    CHARACTER(len=MaxNameLength) :: Name=blank
    CHARACTER(len=MaxNameLength) :: fType=blank
  END TYPE

  TYPE NodeListData
    CHARACTER(len=MaxNameLength) :: Name=blank
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: NodeNames
    INTEGER :: NumNodes
  END TYPE

  TYPE FluidLoop
    CHARACTER(len=MaxNameLength) :: LoopName=' '
    CHARACTER(len=MaxNameLength) :: LoopFluid=' '
    CHARACTER(len=MaxNameLength) :: SetpointNode
    LOGICAL :: IsPlant=.true.   ! false when condenser
    LOGICAL :: HasFluidCoolers=.false.   ! false when condenser
    INTEGER :: NumTotalFluids=0
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: FluidComponentType
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: FluidComponentName
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: FluidChangeName
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: InletNode
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: OutletNode
  END TYPE FluidLoop

  TYPE Component
    CHARACTER(len=MaxNameLength) :: Type=' '
    CHARACTER(len=MaxNameLength) :: Name=' '
    CHARACTER(len=MaxNameLength) :: Fluid1=' '
    CHARACTER(len=MaxNameLength) :: Fluid2=' '
    CHARACTER(len=MaxNameLength) :: InletNode1=' '
    CHARACTER(len=MaxNameLength) :: InletNode2=' '
    CHARACTER(len=MaxNameLength) :: OutletNode1=' '
    CHARACTER(len=MaxNameLength) :: OutletNode2=' '
  END TYPE Component

  TYPE SetpointTemperatureManagedNodes
    CHARACTER(len=MaxNameLength) :: CompType=' '
    CHARACTER(len=MaxNameLength) :: Name=' '
    INTEGER                      :: NodeNumField=0
    INTEGER                      :: NodeListNum=0
    CHARACTER(len=MaxNameLength) :: NodeName=' '
    CHARACTER(len=MaxNameLength) :: PlantLoopName=' '
  END TYPE SetpointTemperatureManagedNodes

  TYPE VariableFlowEquipment
    CHARACTER(len=MaxNameLength) :: CompType=' '
    CHARACTER(len=MaxNameLength) :: CompName=' '
    CHARACTER(len=MaxNameLength) :: OutletNodeName=' '
    CHARACTER(len=MaxNameLength) :: PlantLoopName=' '
    CHARACTER(len=2)             :: SPNameInsert=' '
  END TYPE VariableFlowEquipment

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
  INTEGER, EXTERNAL :: GetNewUnitNumber
!  INTEGER, EXTERNAL :: FindNumber
  INTEGER Arg
  LOGICAL, SAVE :: FirstTime=.true.
  CHARACTER(len=30) UnitsArg
  CHARACTER(len=MaxNameLength) ::  ObjectName
  CHARACTER(len=30), EXTERNAL :: TrimTrailZeros
  CHARACTER(len=MaxNameLength) ::  UCRepVarName=blank
  CHARACTER(len=MaxNameLength) ::  UCCompRepVarName=blank
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
  INTEGER :: COutArgs
  INTEGER :: Nargs
  INTEGER :: NumPlantCondLoops
  INTEGER :: NumFluids
  INTEGER :: PlantCondNum
  LOGICAL :: DoFluidScan
  TYPE(FanData), ALLOCATABLE, DIMENSION(:) :: FansInIDF
  INTEGER :: FanCount
  TYPE (FluidLoop), DIMENSION(:), ALLOCATABLE :: PlantCondLoops
  TYPE (Component), DIMENSION(:), ALLOCATABLE :: FluidComponent
  TYPE (SetpointTemperatureManagedNodes), DIMENSION(:), ALLOCATABLE :: SetpointManagedNodes
  TYPE (VariableFlowEquipment), DIMENSION(:), ALLOCATABLE :: VFEquipment
  TYPE (NodeListData), DIMENSION(:), ALLOCATABLE :: NodeLists
  INTEGER :: SPManCount
  INTEGER :: SPProcCount
  LOGICAL :: DiffFluidFlag
  LOGICAL :: DiffFluidLoopFlag
  CHARACTER(len=MaxNameLength) :: RunningLoopFluidName
  integer :: NumFluidComponents
  integer :: BNum
  integer :: BranchListPtr
  integer :: ConnectorListPtr
  integer :: PassLoop
  integer :: PassLoop1
  integer :: CListPtr
  integer :: FPtr
  integer :: xcount1
  logical :: hasfluidcoolers
  integer :: pfound
  integer :: NumVFEquipment
  integer :: numVFCount
  LOGICAL :: DoScan
  integer :: NumNodeLists
  integer :: numNodeListCount

  LOGICAL :: ErrFlag

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

          ALLOCATE(Alphas(MaxAlphaArgsFound),Numbers(MaxNumericArgsFound))
          ALLOCATE(InArgs(MaxTotalArgs))
          ALLOCATE(AorN(MaxTotalArgs),ReqFld(MaxTotalArgs),FldNames(MaxTotalArgs),FldDefaults(MaxTotalArgs),FldUnits(MaxTotalArgs))
          ALLOCATE(NwAorN(MaxTotalArgs),NwReqFld(MaxTotalArgs),NwFldNames(MaxTotalArgs),NwFldDefaults(MaxTotalArgs),NwFldUnits(MaxTotalArgs))
          ALLOCATE(OutArgs(MaxTotalArgs))
          ALLOCATE(MatchArg(MaxTotalArgs))
          ALLOCATE(DeleteThisRecord(NumIDFRecords))
          DeleteThisRecord=.false.

          NumNodeLists=0
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'NODELIST') CYCLE
            NumNodeLists=NumNodeLists+1
          ENDDO

          IF (ALLOCATED(NodeLists)) DEALLOCATE(NodeLists)

          ALLOCATE(NodeLists(NumNodeLists))

          numNodeListCount=0
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'NODELIST') CYCLE
            numNodeListCount=numNodeListCount+1
            NodeLists(numNodeListCount)%Name=IDFRecords(Num)%Alphas(1)
            ALLOCATE(NodeLists(numNodeListCount)%NodeNames(IDFRecords(Num)%NumAlphas-1))
            DO Num1=1,IDFRecords(Num)%NumAlphas-1
              NodeLists(numNodeListCount)%NodeNames(Num1)=IDFRecords(Num)%Alphas(Num1+1)
            ENDDO
            NodeLists(numNodeListCount)%NumNodes=IDFRecords(Num)%NumAlphas-1
            IF (numNodeListCount == NumNodeLists) EXIT
          ENDDO

!SetpointManager:Scheduled - a2
!SetpointManager:Scheduled:DualSetpoint - a2
!SetpointManager:OutdoorAirReset - a2
!SetpointManager:SingleZone:Reheat - a2
!SetpointManager:SingleZone:Heating - a2
!SetpointManager:SingleZone:Cooling - a2
!SetpointManager:MixedAir - a2
!SetpointManager:OutdoorAirPretreat - a2
!SetpointManager:Warmest - a2
!SetpointManager:Coldest - a2
!SetpointManager:WarmestTemperatureFlow - a2
!SetpointManager:MultiZone:Heating:Average - no field
!SetpointManager:MultiZone:Cooling:Average - no field
          SPManCount=0
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name(1:16)) /= 'SETPOINTMANAGER:') CYCLE
            IF (.not. SameString(IDFRecords(Num)%Name,'SetpointManager:Scheduled') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:Scheduled:DualSetpoint') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:OutdoorAirReset') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:SingleZone:Reheat') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:SingleZone:Heating') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:SingleZone:Cooling') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MixedAir') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:OutdoorAirPretreat') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:Warmest') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:Coldest') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:WarmestTemperatureFlow') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Heating:Average') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Cooling:Average') ) CYCLE
            IF (.not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Heating:Average') .and. &
                .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Cooling:Average') ) THEN
              IF (SameString(IDFRecords(Num)%Alphas(2),'Temperature')) SPManCount=SPManCount+1
            ELSE
              SPManCount=SPManCount+1
            ENDIF
          ENDDO
          IF (ALLOCATED(SetpointManagedNodes)) DEALLOCATE(SetpointManagedNodes)
          ALLOCATE(SetpointManagedNodes(SPManCount))
          SPProcCount=0
          IF (SPManCount > 0) THEN
            DO Num=1,NumIDFRecords
              IF (MakeUPPERCase(IDFRecords(Num)%Name(1:16)) /= 'SETPOINTMANAGER:') CYCLE
              IF (.not. SameString(IDFRecords(Num)%Name,'SetpointManager:Scheduled') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:Scheduled:DualSetpoint') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:OutdoorAirReset') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:SingleZone:Reheat') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:SingleZone:Heating') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:SingleZone:Cooling') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MixedAir') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:OutdoorAirPretreat') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:Warmest') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:Coldest') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:WarmestTemperatureFlow') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Heating:Average') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Cooling:Average') ) CYCLE
              IF (.not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Heating:Average') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'SetpointManager:MultiZone:Cooling:Average') ) THEN
                IF (.not. SameString(IDFRecords(Num)%Alphas(2),'Temperature')) CYCLE
              ENDIF
              SPProcCount=SPProcCount+1
              ! Now, know they are all temperature controlled.
              SELECT CASE(MakeUPPERCase(IDFRecords(Num)%Name))
                CASE('SETPOINTMANAGER:SCHEDULED')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(4)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=4
                CASE('SETPOINTMANAGER:SCHEDULED:DUALSETPOINT')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(5)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=5
                CASE('SETPOINTMANAGER:OUTDOORAIRRESET')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(3)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=7
                CASE('SETPOINTMANAGER:SINGLEZONE:REHEAT')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(6)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=8
                CASE('SETPOINTMANAGER:SINGLEZONE:HEATING')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(6)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=8
                CASE('SETPOINTMANAGER:SINGLEZONE:COOLING')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(6)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=8
                CASE('SETPOINTMANAGER:MIXEDAIR')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(6)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=6
                CASE('SETPOINTMANAGER:OUTDOORAIRPRETREAT')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(7)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=11
                CASE('SETPOINTMANAGER:WARMEST')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(5)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=7
                CASE('SETPOINTMANAGER:COLDEST')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(5)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=7
                CASE('SETPOINTMANAGER:WARMESTTEMPERATUREFLOW')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(5)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=7
                CASE('SETPOINTMANAGER:MULTIZONE:HEATING:AVERAGE')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(3)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=5
                CASE('SETPOINTMANAGER:MULTIZONE:COOLING:AVERAGE')
                  SetpointManagedNodes(SPProcCount)%Name=IDFRecords(Num)%Alphas(1)
                  SetpointManagedNodes(SPProcCount)%NodeName=IDFRecords(Num)%Alphas(3)
                  SetpointManagedNodes(SPProcCount)%CompType=IDFRecords(Num)%Name
                  SetpointManagedNodes(SPProcCount)%NodeNumField=5
              END SELECT
              IF (NumNodeLists > 0) THEN
                PFound=FindItemInList(SetpointManagedNodes(SPProcCount)%NodeName,NodeLists%Name,NumNodeLists)
                IF (PFound > 0) SetpointManagedNodes(SPProcCount)%NodeListNum=PFound
              ENDIF
              IF (SPProcCount == SPManCount) EXIT
            ENDDO
          ENDIF

          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'PLANTLOOP') CYCLE
            PFound=FindItemInList(IDFRecords(Num)%Alphas(4),SetpointManagedNodes%NodeName,SPProcCount)
            IF (PFound > 0) THEN
              SetpointManagedNodes(PFound)%PlantLoopName=IDFRecords(Num)%Alphas(1)
            ELSE
              DO Num2=1,SPProcCount
                IF (SetpointManagedNodes(Num2)%NodeListNum > 0) THEN
                  Num3=FindItemInList(IDFRecords(Num)%Alphas(4),  &
                     NodeLists(SetpointManagedNodes(Num2)%NodeListNum)%NodeNames,  &
                     NodeLists(SetpointManagedNodes(Num2)%NodeListNum)%NumNodes)
                  IF (Num3 > 0) THEN
                    SetpointManagedNodes(Num2)%PlantLoopName=IDFRecords(Num)%Alphas(1)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO

!Boiler:HotWater - a6 ON- a5
!Chiller:Electric:EIR - a10 ON-a6
!Chiller:Electric:ReformulatedEIR - a9 ON-a6
!Chiller:Electric - a7 ON-a4
!Chiller:Absorption:Indirect - a6 ON-a3
!Chiller:Absorption - a8 ON-a3
!Chiller:ConstantCOP - a7 ON a3
!Chiller:EngineDriven - a15 ON a4
!Chiller:CombustionTurbine - a9 ON a4
!ChillerHeater:Absorption:DirectFired - a17 ON a3
!Boiler:Steam - no field for VF ON- a4

          NumVFEquipment=0
          DO Num=1,NumIDFRecords
            IF (.not. SameString(IDFRecords(Num)%Name,'Boiler:HotWater') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Boiler:Steam') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:Electric:EIR') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:Electric:ReformulatedEIR') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:Electric') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:Absorption:Indirect') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:Absorption') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:ConstantCOP') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:EngineDriven') .and. &
                .not. SameString(IDFRecords(Num)%Name,'Chiller:CombustionTurbine') .and. &
                .not. SameString(IDFRecords(Num)%Name,'ChillerHeater:Absorption:DirectFired') ) CYCLE
            IF (SameString(IDFRecords(Num)%Name,'Boiler:HotWater')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(6),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(6) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Boiler:Steam')) THEN
              NumVFEquipment=NumVFEquipment+1
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:Electric:EIR')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(10),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(10) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:Electric:ReformulatedEIR')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(9),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(9) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:Electric')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(7),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(7) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:Absorption:Indirect')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(6),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(6) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:Absorption')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(8),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(8) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:ConstantCOP')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(7),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(7) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:EngineDriven')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(15),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(15) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'Chiller:CombustionTurbine')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(9),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(9) == ' ') THEN
                NumVFEquipment=NumVFEquipment+1
              ENDIF
            ENDIF
            IF (SameString(IDFRecords(Num)%Name,'ChillerHeater:Absorption:DirectFired')) THEN
              IF (SameString(IDFRecords(Num)%Alphas(17),'VariableFlow') .or.  &
                  IDFRecords(Num)%Alphas(17) == ' ') THEN
                NumVFEquipment=NumVFEquipment+2
              ENDIF
            ENDIF
          ENDDO

          IF (ALLOCATED(VFEquipment)) DEALLOCATE(VFEquipment)
          ALLOCATE(VFEquipment(NumVFEquipment))
          numVFCount=0
          IF (NumVFEquipment > 0) THEN
            DO Num=1,NumIDFRecords
              IF (.not. SameString(IDFRecords(Num)%Name,'Boiler:HotWater') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Boiler:Steam') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:Electric:EIR') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:Electric:ReformulatedEIR') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:Electric') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:Absorption:Indirect') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:Absorption') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:ConstantCOP') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:EngineDriven') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'Chiller:CombustionTurbine') .and. &
                  .not. SameString(IDFRecords(Num)%Name,'ChillerHeater:Absorption:DirectFired') ) CYCLE
              IF (SameString(IDFRecords(Num)%Name,'Boiler:HotWater')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(6),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(6) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(5)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Boiler:Steam')) THEN
                numVFCount=numVFCount+1
                VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(4)
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:Electric:EIR')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(10),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(10) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(6)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:Electric:ReformulatedEIR')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(9),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(9) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(6)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:Electric')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(7),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(7) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(4)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:Absorption:Indirect')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(6),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(6) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(3)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:Absorption')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(8),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(8) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(3)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:ConstantCOP')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(7),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(7) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(3)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:EngineDriven')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(15),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(15) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(4)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'Chiller:CombustionTurbine')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(9),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(9) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(4)
                ENDIF
              ENDIF
              IF (SameString(IDFRecords(Num)%Name,'ChillerHeater:Absorption:DirectFired')) THEN
                IF (SameString(IDFRecords(Num)%Alphas(17),'VariableFlow') .or.  &
                    IDFRecords(Num)%Alphas(17) == ' ') THEN
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(3)
                  VFEquipment(numVFCount)%SPNameInsert='CW'
                  numVFCount=numVFCount+1
                  VFEquipment(numVFCount)%CompType=IDFRecords(Num)%Name
                  VFEquipment(numVFCount)%CompName=IDFRecords(Num)%Alphas(1)
                  VFEquipment(numVFCount)%OutletNodeName=IDFRecords(Num)%Alphas(7)
                  VFEquipment(numVFCount)%SPNameInsert='HW'
                ENDIF
              ENDIF
              IF (numVFCount == NumVFEquipment) EXIT
            ENDDO
          ENDIF

          IF (ALLOCATED(PlantCondLoops)) THEN
            DO Num=1,SIZE(PlantCondLoops)
              IF (ALLOCATED(PlantCondLoops(Num)%FluidComponentType)) DEALLOCATE(PlantCondLoops(Num)%FluidComponentType)
              IF (ALLOCATED(PlantCondLoops(Num)%FluidComponentName)) DEALLOCATE(PlantCondLoops(Num)%FluidComponentName)
              IF (ALLOCATED(PlantCondLoops(Num)%FluidChangeName)) DEALLOCATE(PlantCondLoops(Num)%FluidChangeName)
              IF (ALLOCATED(PlantCondLoops(Num)%InletNode)) DEALLOCATE(PlantCondLoops(Num)%InletNode)
              IF (ALLOCATED(PlantCondLoops(Num)%OutletNode)) DEALLOCATE(PlantCondLoops(Num)%OutletNode)
            ENDDO
            DEALLOCATE(PlantCondLoops)
          ENDIF
          IF (ALLOCATED(FluidComponent)) THEN
            DEALLOCATE(FluidComponent)
          ENDIF

          NumPlantCondLoops=0
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'PLANTLOOP') NumPlantCondLoops=NumPlantCondLoops+1
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'CONDENSERLOOP') NumPlantCondLoops=NumPlantCondLoops+1
          ENDDO

          ALLOCATE(PlantCondLoops(NumPlantCondLoops))

          PlantCondNum=0
          IF (NumPlantCondLoops > 0) THEN
            DO Num=1,NumIDFRecords  ! find plant/condenser loop
              IF (SameString(IDFRecords(Num)%Name,'PlantLoop') .or.   &
                  SameString(IDFRecords(Num)%Name,'CondenserLoop') ) THEN
                PlantCondNum=PlantCondNum+1
                PlantCondLoops(PlantCondNum)%LoopName=IDFRecords(Num)%Alphas(1)
                PlantCondLoops(PlantCondNum)%LoopFluid=IDFRecords(Num)%Alphas(2)
                IF (SameString(IDFRecords(Num)%Name,'PlantLoop'))  THEN
                  PlantCondLoops(PlantCondNum)%IsPlant=.true.
                  PlantCondLoops(PlantCondNum)%SetpointNode=IDFRecords(Num)%Alphas(5)
                ENDIF
                IF (SameString(IDFRecords(Num)%Name,'CondenserLoop')) PlantCondLoops(PlantCondNum)%IsPlant=.false.
                DO PassLoop=1,2
                  ! Run down supply side first, branchlist then connector list
                  IF (PassLoop == 1) THEN
                    BranchListPtr=7
                    ConnectorListPtr=8
                  ELSE
                    BranchListPtr=11
                    ConnectorListPtr=12
                  ENDIF
                  DO Num1=1,NumIDFRecords
                    IF (.not. SameString(IDFRecords(Num1)%Name,'BranchList')) CYCLE
                    IF (.not. SameString(IDFRecords(Num)%Alphas(BranchListPtr),IDFRecords(Num1)%Alphas(1))) CYCLE
                    ! found plant side branch list.  now must traverse each branch
                    DO Num2=2,IDFRecords(Num1)%NumAlphas  !IDFRecords(Num1)%Alphas(Num2) is branch name
                      DO Num3=1,NumIDFRecords
                        IF (.not. SameString(IDFRecords(Num3)%Name,'Branch')) CYCLE
                        IF (.not. SameString(IDFRecords(Num1)%Alphas(Num2),IDFRecords(Num3)%Alphas(1))) CYCLE
                        ! found branch.  traverse branch looking for our fluid change items.
                        DO BNum=3,IDFRecords(Num3)%NumAlphas,5
                          IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum),'Boiler:HotWater') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Boiler:Steam') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Electric:EIR') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Electric:ReformulatedEIR') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Electric') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Absorption:Indirect') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Absorption') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:ConstantCOP') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:EngineDriven') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:CombustionTurbine') .and. &
                              .not. SameString(IDFRecords(Num3)%Alphas(BNum),'ChillerHeater:Absorption:DirectFired') ) CYCLE
                          DO Num4=1,NumVFEquipment
                            IF (.not. SameString(VFEquipment(Num4)%CompType,IDFRecords(Num3)%Alphas(BNum))) CYCLE
                            IF (.not. SameString(VFEquipment(Num4)%CompName,IDFRecords(Num3)%Alphas(BNum+1))) CYCLE
                            IF (.not. SameString(VFEquipment(Num4)%OutletNodeName,IDFRecords(Num3)%Alphas(BNum+3))) CYCLE
                            ! Found it
                            VFEquipment(Num4)%PlantLoopName=PlantCondLoops(PlantCondNum)%LoopName
                          ENDDO
                        ENDDO ! traverse branch
                      ENDDO ! find branch
                    ENDDO ! traverse branchlist
                  ENDDO ! plant/condenser side traverse
                  IF (IDFRecords(Num)%Alphas(ConnectorListPtr) /= ' ') THEN
                    DO Num1=1,NumIDFRecords
                      IF (.not. SameString(IDFRecords(Num1)%Name,'ConnectorList')) CYCLE
                      IF (.not. SameString(IDFRecords(Num)%Alphas(ConnectorListPtr),IDFRecords(Num1)%Alphas(1))) CYCLE
                      ! found plant side connector list.  must find connectors and then branch
                      !IDFRecords(Num1)%Alphas(2) is connector type
                      DO PassLoop1=1,2
                        IF (PassLoop1 == 1) THEN
                          CListPtr=2
                        ELSE
                          CListPtr=4
                        ENDIF
                        DO Num2=1,NumIDFRecords  !IDFRecords(Num1)%Alphas(3) is connector type name
                          IF (.not. SameString(IDFRecords(Num2)%Name,IDFRecords(Num1)%Alphas(CListPtr))) CYCLE
                          IF (.not. SameString(IDFRecords(Num2)%Alphas(1),IDFRecords(Num1)%Alphas(CListPtr+1))) CYCLE
                          DO Num3=2,IDFRecords(Num2)%NumAlphas
                            DO Num4=1,NumIDFRecords
                              IF (.not. SameString(IDFRecords(Num4)%Name,'Branch')) CYCLE
                              IF (.not. SameString(IDFRecords(Num2)%Alphas(Num3),IDFRecords(Num4)%Alphas(1))) CYCLE
                          ! found branch.  traverse branch looking for our fluid change items.
                              DO BNum=3,IDFRecords(Num3)%NumAlphas,5
                                IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum),'Boiler:HotWater') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Boiler:Steam') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Electric:EIR') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Electric:ReformulatedEIR') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Electric') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Absorption:Indirect') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:Absorption') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:ConstantCOP') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:EngineDriven') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'Chiller:CombustionTurbine') .and. &
                                    .not. SameString(IDFRecords(Num3)%Alphas(BNum),'ChillerHeater:Absorption:DirectFired') ) CYCLE
                                DO Num5=1,NumVFEquipment
                                  IF (.not. SameString(VFEquipment(Num5)%CompType,IDFRecords(Num3)%Alphas(BNum))) CYCLE
                                  IF (.not. SameString(VFEquipment(Num5)%CompName,IDFRecords(Num3)%Alphas(BNum+1))) CYCLE
                                  IF (.not. SameString(VFEquipment(Num4)%OutletNodeName,IDFRecords(Num3)%Alphas(BNum+3))) CYCLE
                                  ! Found it
                                  VFEquipment(Num4)%PlantLoopName=PlantCondLoops(PlantCondNum)%LoopName
                                ENDDO
                              ENDDO ! traverse branch
                            ENDDO ! find branch
                          ENDDO ! find connectortype
                        ENDDO ! traverse connectorlist
                      ENDDO  ! Pass connector list
                    ENDDO ! plant/condenser side traverse
                  ENDIF
                ENDDO ! Pass 1=supply, 2=demand
              ENDIF  ! Plant/Condenser Loop
            ENDDO  ! find plant/condenser loops
          ENDIF

!          DO Num=1,NumVFEquipment
!            write(diflfn,'(A)') '! VFEquipment='//trim(VFEquipment(Num)%CompType)//':'//  &
!              trim(VFEquipment(Num)%CompName)
!            write(diflfn,'(A)') '!   on PlantLoop='//trim(VFEquipment(Num)%PlantLoopName)//' outlet node='//  &
!              trim(VFEquipment(Num)%OutletNodeName)
!          ENDDO

          DoFluidScan=.false.
          NumPlantCondLoops=0
          NumFluids=0
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(3),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(3),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'FLUIDCOOLER:SINGLESPEED') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(13),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'FLUIDCOOLER:TWOSPEED') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(17),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'GROUNDHEATEXCHANGER:POND') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(4),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'GROUNDHEATEXCHANGER:SURFACE') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(5),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'HEATEXCHANGER:HYDRONIC') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(9),'Water')) DoFluidScan=.true.
              IF (.not. SameString(IDFRecords(Num)%Alphas(10),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'HEATEXCHANGER:PLATE') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(8),'Water')) DoFluidScan=.true.
              IF (.not. SameString(IDFRecords(Num)%Alphas(9),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'HEATEXCHANGER:WATERSIDEECONOMIZER') THEN
              IF (.not. SameString(IDFRecords(Num)%Alphas(8),'Water')) DoFluidScan=.true.
              IF (.not. SameString(IDFRecords(Num)%Alphas(9),'Water')) DoFluidScan=.true.
              NumFluids=NumFluids+1
            ENDIF
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'PLANTLOOP') NumPlantCondLoops=NumPlantCondLoops+1
            IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'CONDENSERLOOP') NumPlantCondLoops=NumPlantCondLoops+1
          ENDDO

          IF (DoFluidScan .and. NumPlantCondLoops>0) THEN
            ALLOCATE(FluidComponent(NumFluids))
            NumFluidComponents=0
            DO Num=1,NumIDFRecords
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(3)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(6)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(3)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(6)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'FLUIDCOOLER:SINGLESPEED') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(2)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(3)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'FLUIDCOOLER:TWOSPEED') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(2)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(3)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'GROUNDHEATEXCHANGER:POND') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(4)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(2)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(3)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'GROUNDHEATEXCHANGER:SURFACE') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(3)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(4)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'HEATEXCHANGER:HYDRONIC') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(9)
                FluidComponent(NumFluidComponents)%Fluid2=IDFRecords(Num)%Alphas(10)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(6)
                FluidComponent(NumFluidComponents)%InletNode2=IDFRecords(Num)%Alphas(7)
                FluidComponent(NumFluidComponents)%OutletNode2=IDFRecords(Num)%Alphas(8)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'HEATEXCHANGER:PLATE') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(8)
                FluidComponent(NumFluidComponents)%Fluid2=IDFRecords(Num)%Alphas(9)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(4)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%InletNode2=IDFRecords(Num)%Alphas(6)
                FluidComponent(NumFluidComponents)%OutletNode2=IDFRecords(Num)%Alphas(7)
              ENDIF
              IF (MakeUPPERCase(IDFRecords(Num)%Name) == 'HEATEXCHANGER:WATERSIDEECONOMIZER') THEN
                NumFluidComponents=NumFluidComponents+1
                FluidComponent(NumFluidComponents)%Type=MakeUPPERCase(IDFRecords(Num)%Name)
                FluidComponent(NumFluidComponents)%Name=IDFRecords(Num)%Alphas(1)
                FluidComponent(NumFluidComponents)%Fluid1=IDFRecords(Num)%Alphas(8)
                FluidComponent(NumFluidComponents)%Fluid2=IDFRecords(Num)%Alphas(9)
                FluidComponent(NumFluidComponents)%InletNode1=IDFRecords(Num)%Alphas(4)
                FluidComponent(NumFluidComponents)%OutletNode1=IDFRecords(Num)%Alphas(5)
                FluidComponent(NumFluidComponents)%InletNode2=IDFRecords(Num)%Alphas(6)
                FluidComponent(NumFluidComponents)%OutletNode2=IDFRecords(Num)%Alphas(7)
              ENDIF
            ENDDO

!            DO Num=1,NumFluidComponents
!              write(diflfn,'(A)') '! fluid components='//trim(FluidComponent(Num)%Type)//':'//  &
!                trim(FluidComponent(Num)%Name)
!              if (FluidComponent(Num)%Fluid2 == ' ') THEN
!                write(diflfn,'(A)') '!   fluid='//trim(FluidComponent(Num)%Fluid1)
!                write(diflfn,'(A)') '!   inlet node='//trim(FluidComponent(Num)%InletNode1)
!                write(diflfn,'(A)') '!   outlet node='//trim(FluidComponent(Num)%OutletNode1)
!              else
!                write(diflfn,'(A)') '!   fluid1='//trim(FluidComponent(Num)%Fluid1)
!                write(diflfn,'(A)') '!   fluid2='//trim(FluidComponent(Num)%Fluid2)
!                write(diflfn,'(A)') '!   inlet node1='//trim(FluidComponent(Num)%InletNode1)
!                write(diflfn,'(A)') '!   outlet node1='//trim(FluidComponent(Num)%OutletNode1)
!                write(diflfn,'(A)') '!   inlet node2='//trim(FluidComponent(Num)%InletNode2)
!                write(diflfn,'(A)') '!   outlet node2='//trim(FluidComponent(Num)%OutletNode2)
!              endif
!            ENDDO

            DO Num=1,NumPlantCondLoops
              ALLOCATE(PlantCondLoops(Num)%FluidComponentType(NumFluids))
              ALLOCATE(PlantCondLoops(Num)%FluidComponentName(NumFluids))
              ALLOCATE(PlantCondLoops(Num)%FluidChangeName(NumFluids))
              ALLOCATE(PlantCondLoops(Num)%InletNode(NumFluids))
              ALLOCATE(PlantCondLoops(Num)%OutletNode(NumFluids))
              PlantCondLoops(Num)%FluidComponentType=' '
              PlantCondLoops(Num)%FluidComponentName=' '
              PlantCondLoops(Num)%FluidChangeName=' '
              PlantCondLoops(Num)%InletNode=' '
              PlantCondLoops(Num)%OutletNode=' '
            ENDDO

            PlantCondNum=0
            DO Num=1,NumIDFRecords  ! find plant/condenser loop
              IF (SameString(IDFRecords(Num)%Name,'PlantLoop') .or.   &
                  SameString(IDFRecords(Num)%Name,'CondenserLoop') ) THEN
                PlantCondNum=PlantCondNum+1
                PlantCondLoops(PlantCondNum)%LoopName=IDFRecords(Num)%Alphas(1)
                PlantCondLoops(PlantCondNum)%LoopFluid=IDFRecords(Num)%Alphas(2)
                IF (SameString(IDFRecords(Num)%Name,'PlantLoop')) PlantCondLoops(PlantCondNum)%IsPlant=.true.
                IF (SameString(IDFRecords(Num)%Name,'CondenserLoop')) PlantCondLoops(PlantCondNum)%IsPlant=.false.
                DO PassLoop=1,2
                  ! Run down supply side first, branchlist then connector list
                  IF (PassLoop == 1) THEN
                    BranchListPtr=7
                    ConnectorListPtr=8
                  ELSE
                    BranchListPtr=11
                    ConnectorListPtr=12
                  ENDIF
                  DO Num1=1,NumIDFRecords
                    IF (.not. SameString(IDFRecords(Num1)%Name,'BranchList')) CYCLE
                    IF (.not. SameString(IDFRecords(Num)%Alphas(BranchListPtr),IDFRecords(Num1)%Alphas(1))) CYCLE
                    ! found plant side branch list.  now must traverse each branch
                    DO Num2=2,IDFRecords(Num1)%NumAlphas  !IDFRecords(Num1)%Alphas(Num2) is branch name
                      DO Num3=1,NumIDFRecords
                        IF (.not. SameString(IDFRecords(Num3)%Name,'Branch')) CYCLE
                        IF (.not. SameString(IDFRecords(Num1)%Alphas(Num2),IDFRecords(Num3)%Alphas(1))) CYCLE
                        ! found branch.  traverse branch looking for our fluid change items.
                        DO BNum=3,IDFRecords(Num3)%NumAlphas,5
                          IF (SameString(IDFRecords(Num3)%Alphas(BNum),'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'FLUIDCOOLER:SINGLESPEED') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'FLUIDCOOLER:TWOSPEED') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'GROUNDHEATEXCHANGER:POND') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'GROUNDHEATEXCHANGER:SURFACE') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:HYDRONIC') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:PLATE') .or.  &
                              SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:WATERSIDEECONOMIZER') ) THEN
                              ! find item in previous structure
                            DO Num5=1,NumFluidComponents
                              IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum),FluidComponent(Num5)%Type)) CYCLE
                              IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum+1),FluidComponent(Num5)%Name)) CYCLE
                              IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:HYDRONIC') .and.   &
                                  .not. SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:PLATE') .and.   &
                                  .not. SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:WATERSIDEECONOMIZER') ) THEN
                                IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum+2),FluidComponent(Num5)%InletNode1)) CYCLE
                                IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum+3),FluidComponent(Num5)%OutletNode1)) CYCLE
                                FPtr=1
                              ELSE
                                IF (SameString(IDFRecords(Num3)%Alphas(BNum+2),FluidComponent(Num5)%InletNode1)  .and. &
                                    SameString(IDFRecords(Num3)%Alphas(BNum+3),FluidComponent(Num5)%OutletNode1)) THEN
                                  FPtr=1
                                ELSEIF (SameString(IDFRecords(Num3)%Alphas(BNum+2),FluidComponent(Num5)%InletNode2)  .and. &
                                        SameString(IDFRecords(Num3)%Alphas(BNum+3),FluidComponent(Num5)%OutletNode2)) THEN
                                  FPtr=2
                                ELSE
                                  CYCLE
                                ENDIF
                              ENDIF
                              PlantCondLoops(PlantCondNum)%NumTotalFluids=PlantCondLoops(PlantCondNum)%NumTotalFluids+1
                              PlantCondLoops(PlantCondNum)%FluidComponentType(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Type
                              PlantCondLoops(PlantCondNum)%FluidComponentName(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Name
                              IF (FPtr == 1) THEN
                                PlantCondLoops(PlantCondNum)%FluidChangeName(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Fluid1
                              ELSE
                                PlantCondLoops(PlantCondNum)%FluidChangeName(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Fluid2
                              ENDIF
!                              write(diflfn,'(A)') '! Plant/CondenserLoop='//trim(PlantCondLoops(PlantCondNum)%LoopName)
!                              write(diflfn,'(A)') '!  matched to '//trim(FluidComponent(Num5)%Type)//':'//trim(FluidComponent(Num5)%Name)
                              IF (SameString(IDFRecords(Num3)%Alphas(BNum),'FLUIDCOOLER:SINGLESPEED') .or.  &
                                  SameString(IDFRecords(Num3)%Alphas(BNum),'FLUIDCOOLER:TWOSPEED') ) THEN
                                PlantCondLoops(PlantCondNum)%HasFluidCoolers=.true.
!                                write(diflfn,'(A)') '! has fluid cooler'
                              ENDIF
                            ENDDO
                          ENDIF
                        ENDDO ! traverse branch
                      ENDDO ! find branch
                    ENDDO ! traverse branchlist
                  ENDDO ! plant/condenser side traverse
                  IF (IDFRecords(Num)%Alphas(ConnectorListPtr) /= ' ') THEN
                    DO Num1=1,NumIDFRecords
                      IF (.not. SameString(IDFRecords(Num1)%Name,'ConnectorList')) CYCLE
                      IF (.not. SameString(IDFRecords(Num)%Alphas(ConnectorListPtr),IDFRecords(Num1)%Alphas(1))) CYCLE
                      ! found plant side connector list.  must find connectors and then branch
                      !IDFRecords(Num1)%Alphas(2) is connector type
                      DO PassLoop1=1,2
                        IF (PassLoop1 == 1) THEN
                          CListPtr=2
                        ELSE
                          CListPtr=4
                        ENDIF
                        DO Num2=1,NumIDFRecords  !IDFRecords(Num1)%Alphas(3) is connector type name
                          IF (.not. SameString(IDFRecords(Num2)%Name,IDFRecords(Num1)%Alphas(CListPtr))) CYCLE
                          IF (.not. SameString(IDFRecords(Num2)%Alphas(1),IDFRecords(Num1)%Alphas(CListPtr+1))) CYCLE
                          DO Num3=2,IDFRecords(Num2)%NumAlphas
                            DO Num4=1,NumIDFRecords
                              IF (.not. SameString(IDFRecords(Num4)%Name,'Branch')) CYCLE
                              IF (.not. SameString(IDFRecords(Num2)%Alphas(Num3),IDFRecords(Num4)%Alphas(1))) CYCLE
                          ! found branch.  traverse branch looking for our fluid change items.
                              DO BNum=3,IDFRecords(Num3)%NumAlphas,5
                                IF (SameString(IDFRecords(Num3)%Alphas(BNum),'COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'FLUIDCOOLER:SINGLESPEED') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'FLUIDCOOLER:TWOSPEED') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'GROUNDHEATEXCHANGER:POND') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'GROUNDHEATEXCHANGER:SURFACE') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:HYDRONIC') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:PLATE') .or.  &
                                    SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:WATERSIDEECONOMIZER') ) THEN
                                    ! find item in previous structure
                                  DO Num5=1,NumFluidComponents
                                    IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum),FluidComponent(Num5)%Type)) CYCLE
                                    IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum+1),FluidComponent(Num5)%Name)) CYCLE
                                    IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:HYDRONIC') .and.   &
                                        .not. SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:PLATE') .and.   &
                                        .not. SameString(IDFRecords(Num3)%Alphas(BNum),'HEATEXCHANGER:WATERSIDEECONOMIZER') ) THEN
                                      IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum+2),FluidComponent(Num5)%InletNode1)) CYCLE
                                      IF (.not. SameString(IDFRecords(Num3)%Alphas(BNum+3),FluidComponent(Num5)%OutletNode1)) CYCLE
                                      FPtr=1
                                    ELSE
                                      IF (SameString(IDFRecords(Num3)%Alphas(BNum+2),FluidComponent(Num5)%InletNode1)  .and. &
                                          SameString(IDFRecords(Num3)%Alphas(BNum+3),FluidComponent(Num5)%OutletNode1)) THEN
                                        FPtr=1
                                      ELSEIF (SameString(IDFRecords(Num3)%Alphas(BNum+2),FluidComponent(Num5)%InletNode2)  .and. &
                                              SameString(IDFRecords(Num3)%Alphas(BNum+3),FluidComponent(Num5)%OutletNode2)) THEN
                                        FPtr=2
                                      ELSE
                                        CYCLE
                                      ENDIF
                                    ENDIF
                                    PlantCondLoops(PlantCondNum)%NumTotalFluids=PlantCondLoops(PlantCondNum)%NumTotalFluids+1
                                    PlantCondLoops(PlantCondNum)%FluidComponentType(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Type
                                    PlantCondLoops(PlantCondNum)%FluidComponentName(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Name
                                    IF (FPtr == 1) THEN
                                      PlantCondLoops(PlantCondNum)%FluidChangeName(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Fluid1
                                    ELSE
                                      PlantCondLoops(PlantCondNum)%FluidChangeName(PlantCondLoops(PlantCondNum)%NumTotalFluids)=FluidComponent(Num5)%Fluid2
                                    ENDIF
!                                    write(diflfn,'(A)') '! Plant/CondenserLoop='//trim(PlantCondLoops(PlantCondNum)%LoopName)
!                                    write(diflfn,'(A)') '!  matched to '//trim(FluidComponent(Num5)%Type)//':'//trim(FluidComponent(Num5)%Name)
!                                    write(diflfn,'(A)') '! on connector:'//trim(IDFRecords(Num1)%Alphas(CListPtr+1))
                                  ENDDO
                                ENDIF
                              ENDDO ! traverse branch
                            ENDDO ! find branch
                          ENDDO ! find connectortype
                        ENDDO ! traverse connectorlist
                      ENDDO  ! Pass connector list
                    ENDDO ! plant/condenser side traverse
                  ENDIF
                ENDDO ! Pass 1=supply, 2=demand
              ENDIF  ! Plant/Condenser Loop
            ENDDO  ! find plant/condenser loops
            ! now scan the plant cond loop for differing fluids.
            DO Num1=1,NumPlantCondLoops
              DiffFluidFlag=.false.
              DiffFluidLoopFlag=.false.
              RunningLoopFluidName=' '

              DO Num2=1,PlantCondLoops(Num1)%NumTotalFluids
                IF (RunningLoopFluidName == ' ') &
                    RunningLoopFluidName = PlantCondLoops(Num1)%FluidChangeName(Num2)
                IF (.not. SameString(PlantCondLoops(Num1)%FluidChangeName(Num2),PlantCondLoops(Num1)%LoopFluid) ) &
                  DiffFluidLoopFlag=.true.
                IF (.not. SameString(RunningLoopFluidName,PlantCondLoops(Num1)%FluidChangeName(Num2))) &
                  DiffFluidFlag=.true.
              ENDDO
              IF (DiffFluidLoopFlag) THEN
                IF (PlantCondLoops(Num1)%IsPlant) THEN
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   'Fluid for PlantLoop="'//trim(PlantCondLoops(Num1)%LoopName)//'" is changed '//  &
                   'due to fluid type of components on the loop. New fluid type will likely be "'//  &
                   trim(RunningLoopFluidName)//'"')
                ELSE
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   'Fluid for CondenserLoop="'//trim(PlantCondLoops(Num1)%LoopName)//'" is changed '// &
                   'due to fluid type of components on the loop. New fluid type will likely be "'//  &
                   trim(RunningLoopFluidName)//'"')
                ENDIF
                PlantCondLoops(Num1)%LoopFluid=RunningLoopFluidName
              ENDIF
              IF (DiffFluidFlag) THEN
                IF (PlantCondLoops(Num1)%IsPlant) THEN
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   'Components on PlantLoop="'//trim(PlantCondLoops(Num1)%LoopName)//' have differing fluid types. '// &
                   'Loop fluid type will be changed to first fluid type encountered.')
                ELSE
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   'Components on CondenserLoop="'//trim(PlantCondLoops(Num1)%LoopName)//' have differing fluid types. '// &
                   'Loop fluid type will be changed to first fluid type encountered.')
                ENDIF
              ENDIF
            ENDDO
          ENDIF  ! end prescan

          NoVersion=.true.
          DO Num=1,NumIDFRecords
            IF (MakeUPPERCase(IDFRecords(Num)%Name) /= 'VERSION') CYCLE
            NoVersion=.false.
            EXIT
          ENDDO
!
          FanCount=0
          DO Num=1,NumIDFRecords
            IF (.not. SameString(IDFRecords(Num)%Name(1:3),'Fan')) CYCLE
            ! note does not include: FanPerformance:NightVentilation but this is not one of the transition fans
            FanCount=FanCount+1
          ENDDO
          ALLOCATE(FansInIDF(FanCount))
          IF (FanCount > 0) THEN
            xCount=0
            DO Num=1,NumIDFRecords
              IF (.not. SameString(IDFRecords(Num)%Name(1:3),'Fan')) CYCLE
              xCount=xCount+1
              FansInIDF(xCount)%fType=IDFRecords(Num)%Name
              FansInIDF(xCount)%Name=IDFRecords(Num)%Alphas(1)
            ENDDO
          ENDIF

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
              OutArgs(1)='7.0'
              CurArgs=1
              CALL WriteOutIDFLinesAsComments(DifLfn,'Version',CurArgs,OutArgs,NwFldNames,NwFldUnits)
            ENDIF

     ! deleted objects.  no transition.
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'SKY RADIANCE DISTRIBUTION') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'AIRFLOW MODEL') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'GENERATOR:FC:BATTERY DATA') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'AIRFLOWNETWORK:MULTIZONE:SITEWINDCONDITIONS') CYCLE
            IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'WATER HEATER:SIMPLE') THEN
              WRITE(DifLfn,fmta) '! ** The WATER HEATER:SIMPLE object has been deleted'
              CALL writePreprocessorObject(DifLfn,ProgNameConversion,'Warning','The WATER HEATER:SIMPLE object has been deleted')
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
                IF (InArgs(1)(1:3) == '7.0' .and. ArgFile) THEN
                  CALL ShowWarningError('File is already at latest version.  No new diff file made.',Auditf)
                  CLOSE(diflfn,STATUS='DELETE')
                  LatestVersion=.true.
                  EXIT
                ENDIF
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)='7.0'
                nodiff=.false.

!    !!!    Changes for this version
              CASE('ZONEHVAC:FOURPIPEFANCOIL')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:10)=InArgs(1:10)
                OutArgs(11)='OutdoorAir:Mixer'
                OutArgs(12)=InArgs(13)
                IF (FanCount > 0) THEN
                  xcount=FindItemInList(InArgs(14),FansInIDF%Name,FanCount)
                  IF (xCount > 0) THEN
                    OutArgs(13)=FansInIDF(xcount)%fType
                  ELSE
                    OutArgs(13)='invalid fan type'
                  ENDIF
                ELSE
                  OutArgs(13)='invalid fan type'
                ENDIF
                OutArgs(14)=InArgs(14)
                OutArgs(15)=InArgs(23)
                OutArgs(16:19)=InArgs(15:18)
                OutArgs(20)='Coil:Heating:Water'
                OutArgs(21:24)=InArgs(19:22)
                CurArgs=24

              CASE('ZONEHVAC:WINDOWAIRCONDITIONER')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:6)=InArgs(1:6)
                OutArgs(7)='OutdoorAir:Mixer'
                OutArgs(8)=InArgs(9)
                IF (FanCount > 0) THEN
                  xcount=FindItemInList(InArgs(10),FansInIDF%Name,FanCount)
                  IF (xCount > 0) THEN
                    OutArgs(9)=FansInIDF(xcount)%fType
                  ELSE
                    OutArgs(9)='invalid fan type'
                  ENDIF
                ELSE
                  OutArgs(9)='invalid fan type'
                ENDIF
                OutArgs(10)=InArgs(10)
                OutArgs(11)=InArgs(15)
                OutArgs(12:15)=InArgs(11:14)
                IF (CurArgs > 15) THEN
                  OutArgs(16)=InArgs(16)
                ENDIF

              CASE('ZONEHVAC:UNITVENTILATOR')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:10)=InArgs(1:10)
                OutArgs(11:13)=InArgs(12:14)
                IF (FanCount > 0) THEN
                  xcount=FindItemInList(InArgs(15),FansInIDF%Name,FanCount)
                  IF (xCount > 0) THEN
                    OutArgs(14)=FansInIDF(xcount)%fType
                  ELSE
                    OutArgs(14)='invalid fan type'
                  ENDIF
                ELSE
                  OutArgs(14)='invalid fan type'
                ENDIF
                IF (CurArgs >= 19) THEN
                  OutArgs(15:18)=InArgs(15:18)
                ELSE
                  OutArgs(15:CurArgs)=InArgs(15:CurArgs)
                ENDIF
                IF (CurArgs >= 23) THEN
                  OutArgs(19:21)=InArgs(20:22)
                ELSE
                  OutArgs(19:CurArgs)=InArgs(20:CurArgs)
                ENDIF
                IF (CurArgs >= 25) THEN
                  OutArgs(22:23)=InArgs(24:25)
                ENDIF
                CurArgs=MIN(CurArgs,23)

              CASE('ZONEHVAC:UNITHEATER')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                IF (FanCount > 0) THEN
                  xcount=FindItemInList(InArgs(5),FansInIDF%Name,FanCount)
                  IF (xCount > 0) THEN
                    OutArgs(5)=FansInIDF(xcount)%fType
                  ELSE
                    OutArgs(5)='invalid fan type'
                  ENDIF
                ELSE
                  OutArgs(5)='invalid fan type'
                ENDIF
                OutArgs(6:8)=InArgs(5:7)
                IF (CurArgs >= 13) THEN
                  OutArgs(9:12)=InArgs(9:12)
                ELSE
                  OutArgs(9:CurArgs)=InArgs(9:CurArgs)
                ENDIF
                IF (CurArgs >= 15) THEN
                  OutArgs(13:14)=InArgs(14:15)
                ELSE
                  OutArgs(13:CurArgs-1)=InArgs(14:CurArgs)
                ENDIF
                CurArgs=MIN(CurArgs,14)

              CASE('ZONEHVAC:PACKAGEDTERMINALAIRCONDITIONER')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                OutArgs(5)='OutdoorAir:Mixer'
                OutArgs(6:CurArgs+1)=InArgs(5:CurArgs)
                CurArgs=CurArgs+1

              CASE('ZONEHVAC:PACKAGEDTERMINALHEATPUMP')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                OutArgs(5)='OutdoorAir:Mixer'
                OutArgs(6:CurArgs+1)=InArgs(5:CurArgs)
                CurArgs=CurArgs+1

              CASE('ZONEHVAC:WATERTOAIRHEATPUMP')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                OutArgs(5)='OutdoorAir:Mixer'
                OutArgs(6:CurArgs+1)=InArgs(5:CurArgs)
                CurArgs=CurArgs+1

              CASE('AIRLOOPHVAC:UNITARYHEATCOOL:VAVCHANGEOVERBYPASS')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:13)=InArgs(1:13)
                OutArgs(14)='OutdoorAir:Mixer'
                OutArgs(15:CurArgs+1)=InArgs(14:CurArgs)
                CurArgs=CurArgs+1

              CASE('AIRLOOPHVAC:UNITARYHEATPUMP:AIRTOAIR:MULTISPEED')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                 ' Check the values for heating and cooling speeds for AirloopHVAC:UnitaryHeatPump:AirToAir:Multispeed - '// &
                 ' Name="'//trim(OutArgs(1))//'"')
                xcount=ProcessNumber(InArgs(26),errflag)  ! number of speeds for heating.
                IF (xcount == 4) THEN
                  OutArgs(28:31)=InArgs(28:31)
                ELSEIF (xcount == 3) THEN
                  OutArgs(28:30)=InArgs(28:30)
                  OutArgs(31)=blank
                ELSEIF (xcount == 2) THEN
                  OutArgs(28:29)=InArgs(28:29)
                  OutArgs(30:31)=blank
                ENDIF
                xcount1=ProcessNumber(InArgs(27),errflag)  ! number of speeds for cooling.
                IF (xcount == 4 .and. xcount1 == 4) THEN
                  OutArgs(32:35)=InArgs(32:35)
                ENDIF
                IF (xcount1 == 4 .and. InArgs(35) == blank) THEN
                  IF (xcount == 2) THEN
                    OutArgs(32:35)=InArgs(30:33)
                    CurArgs=35
                  ELSEIF (xcount == 3) THEN
                    OutArgs(32:35)=InArgs(31:34)
                    CurArgs=35
                  ELSEIF (xcount == 2) THEN
                    OutArgs(32:35)=InArgs(30:33)
                    CurArgs=35
                  ENDIF
                ENDIF
                IF (xcount1 == 3 .and. InArgs(34) == blank) THEN
                  IF (xcount == 2) THEN
                    OutArgs(32:34)=InArgs(30:32)
                    CurArgs=34
                  ELSEIF (xcount == 3) THEN
                    OutArgs(32:34)=InArgs(31:33)
                    CurArgs=34
                  ELSEIF (xcount == 2) THEN
                    OutArgs(32:34)=InArgs(30:32)
                    CurArgs=34
                  ENDIF
                ENDIF
                IF (xcount1 == 2 .and. InArgs(32) == blank) THEN
                  IF (xcount == 2) THEN
                    OutArgs(32:33)=InArgs(30:31)
                    CurArgs=33
                  ELSEIF (xcount == 3) THEN
                    OutArgs(32:33)=InArgs(31:32)
                    CurArgs=33
                  ELSEIF (xcount == 2) THEN
                    OutArgs(32:33)=InArgs(30:31)
                    CurArgs=33
                  ENDIF
                ENDIF

              CASE('SIZING:ZONE')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:5)=InArgs(1:5)
                OutArgs(6)='SZ DSOA '//trim(InArgs(1))
                OutArgs(7:CurArgs-3)=InArgs(10:CurArgs)
                CurArgs=CurArgs-3
                if (CurArgs < NwObjMinFlds) THEN
                  OutArgs(CurArgs+1:NwObjMinFlds)=blank
                  CurArgs=NwObjMinFlds
                endif
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)

                ! create new object DesignSpecifications:OutdoorAir
                ObjectName='DesignSpecification:OutdoorAir'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=OutArgs(6)
                OutArgs(2)=InArgs(6)
                OutArgs(3:5)=InArgs(7:9)
                CurArgs=5
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                Written=.true.
                !CYCLE

              CASE('CONTROLLER:MECHANICALVENTILATION')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:2)=InArgs(1:2)
                OutArgs(3)='No'
                OutArgs(4)=InArgs(4)
                IF (SameString(OutArgs(4),'VRP')) OutArgs(4)='VentilationRateProcedure'
                IF (SameString(OutArgs(4),'IAQP')) OutArgs(4)='IndoorAirQualityProcedure'
                IF (SameString(OutArgs(4),'VentilationRateProcedure')) OutArgs(3)='Yes'
                NArgs=CurArgs
                COutArgs=5
                do arg=6,NArgs,6
                  OutArgs(COutArgs)=InArgs(arg-1)
                  COutArgs=COutArgs+1
                  OutArgs(COutArgs)='CM DSOA '//trim(InArgs(arg-1))
                  COutArgs=COutArgs+1
                  OutArgs(COutArgs)=InArgs(arg+2)
                  COutArgs=COutArgs+1
                  OutArgs(COutArgs)=InArgs(arg+3)
                  COutArgs=COutArgs+1
                  OutArgs(COutArgs)=InArgs(arg+4)
                  COutArgs=COutArgs+1
                enddo
                COutArgs=COutArgs-1
                if (COutArgs < NwObjMinFlds) THEN
                  OutArgs(COutArgs+1:NwObjMinFlds)=blank
                  COutArgs=NwObjMinFlds
                endif
                CALL WriteOutIDFLines(DifLfn,ObjectName,COutArgs,OutArgs,NwFldNames,NwFldUnits)

                ! create new object DesignSpecification:OutdoorAir
                ObjectName='DesignSpecification:OutdoorAir'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                do arg=6,NArgs,6
                  OutArgs(1)='CM DSOA '//trim(InArgs(arg-1))
                  OutArgs(2)=InArgs(3)
                  OutArgs(3)=InArgs(arg+1)
                  OutArgs(4)=InArgs(arg)
                  COutArgs=4
                  CALL WriteOutIDFLines(DifLfn,ObjectName,COutArgs,OutArgs,NwFldNames,NwFldUnits)
                enddo
                Written=.true.
                !CYCLE

              CASE('COIL:COOLING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:2)=InArgs(1:2)
                IF (.not. SameString(InArgs(3),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                    trim(FldNames(3))//' is not Water. '//  &
                    ' Check the loop fluid type for Coil:Cooling:WaterToAirHeatPump:ParameterEstimation - '// &
                    ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(3))//'"')
                ENDIF
                OutArgs(3:CurArgs-1)=InArgs(4:CurArgs)
                CurArgs=CurArgs-1

              CASE('COIL:HEATING:WATERTOAIRHEATPUMP:PARAMETERESTIMATION')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:2)=InArgs(1:2)
                IF (.not. SameString(InArgs(3),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(3))//' is not Water. '// &
                   ' Check the loop fluid type for Coil:Heating:WaterToAirHeatPump:ParameterEstimation - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(3))//'"')
                ENDIF
                OutArgs(3:CurArgs-1)=InArgs(4:CurArgs)
                CurArgs=CurArgs-1

              CASE('FLUIDCOOLER:SINGLESPEED')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:12)=InArgs(1:12)
                IF (CurArgs >= 13) THEN
                  IF (.not. SameString(InArgs(13),'Water')) THEN
                    WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                    CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                     trim(FldNames(13))//' is not Water. '// &
                     ' Check the loop fluid type for FluidCooler:SingleSpeed - '// &
                     ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(13))//'"')
                  ENDIF
                ENDIF
                IF (CurArgs >= 15) THEN
                  OutArgs(13)=InArgs(CurArgs)
                ENDIF
                CurArgs=CurArgs-2

              CASE('FLUIDCOOLER:TWOSPEED')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:16)=InArgs(1:16)
                IF (CurArgs >= 17) THEN
                  IF (.not. SameString(InArgs(17),'Water')) THEN
                    WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                    CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                     trim(FldNames(17))//' is not Water. '// &
                     ' Check the loop fluid type for FluidCooler:TwoSpeed - '// &
                     ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(17))//'"')
                  ENDIF
                ENDIF
                IF (CurArgs >= 19) THEN
                  OutArgs(17)=InArgs(CurArgs)
                ENDIF
                CurArgs=CurArgs-2

              CASE('GROUNDHEATEXCHANGER:POND')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:3)=InArgs(1:3)
                IF (.not. SameString(InArgs(4),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(4))//' is not Water. '// &
                   ' Check the loop fluid type for GroundHeatExchanger:Pond - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(4))//'"')
                ENDIF
                OutArgs(4:CurArgs-2)=InArgs(6:CurArgs)
                CurArgs=CurArgs-2

              CASE('GROUNDHEATEXCHANGER:SURFACE')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                IF (.not. SameString(InArgs(5),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(5))//' is not Water. '// &
                   ' Check the loop fluid type for GroundHeatExchanger:Surface - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(5))//'"')
                ENDIF
                OutArgs(5:CurArgs-1)=InArgs(6:CurArgs)
                CurArgs=CurArgs-1

              CASE('HEATEXCHANGER:HYDRONIC')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:8)=InArgs(1:8)
                IF (.not. SameString(InArgs(9),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(9))//' is not Water. '// &
                   ' Check the condenser loop fluid type for HeatExchanger:Hydronic - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(9))//'"')
                ENDIF
                IF (.not. SameString(InArgs(10),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(10))//' is not Water. '// &
                   ' Check the plant loop fluid type for HeatExchanger:Hydronic - '//  &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(10))//'"')
                ENDIF
                OutArgs(9:CurArgs-2)=InArgs(11:CurArgs)
                CurArgs=CurArgs-2

              CASE('HEATEXCHANGER:PLATE')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:7)=InArgs(1:7)
                IF (.not. SameString(InArgs(8),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(8))//' is not Water. '// &
                   ' Check the condenser loop fluid type for HeatExchanger:Plate - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(8))//'"')
                ENDIF
                IF (.not. SameString(InArgs(9),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(9))//' is not Water. '//  &
                   ' Check the plant loop fluid type for HeatExchanger:Plate - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(9))//'"')
                ENDIF
                OutArgs(8:CurArgs-2)=InArgs(10:CurArgs)
                CurArgs=CurArgs-2

              CASE('HEATEXCHANGER:WATERSIDEECONOMIZER')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:7)=InArgs(1:7)
                IF (.not. SameString(InArgs(8),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(8))//' is not Water. '// &
                   ' Check the plant loop fluid type for HeatExchanger:Plate - '// &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(8))//'"')
                ENDIF
                IF (.not. SameString(InArgs(9),'Water')) THEN
                  WRITE(DifLfn,fmta) '! The Plant Fluid Type may have changed.'
                  CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                   trim(FldNames(9))//' is not Water. '// &
                   ' Check the plant loop fluid type for HeatExchanger:Plate - '//  &
                   ' Name="'//trim(OutArgs(1))//'": Fluid="'//trim(InArgs(9))//'"')
                ENDIF
                OutArgs(8:CurArgs-2)=InArgs(10:CurArgs)
                CurArgs=CurArgs-2

              CASE('PLANTLOOP')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                IF (ALLOCATED(PlantCondLoops)) THEN
                  DO Num1=1,NumPlantCondLoops
                    IF (.not. SameString(InArgs(1),PlantCondLoops(Num1)%LoopName)) CYCLE
                    IF (.not. PlantCondLoops(Num1)%IsPlant) CYCLE
                    IF (.not. SameString(PlantCondLoops(Num1)%LoopFluid,'Water') .and. &
                        .not. SameString(PlantCondLoops(Num1)%LoopFluid,'Steam') ) THEN
                      OutArgs(3)=PlantCondLoops(Num1)%LoopFluid
                      OutArgs(2)='UserDefinedFluidType'
                    ELSE
                      OutArgs(2)=PlantCondLoops(Num1)%LoopFluid
                      OutArgs(3)=blank
                    ENDIF
                    EXIT
                  ENDDO
                ELSE
                  IF (SameString(InArgs(2),'Water') .or. SameString(InArgs(2),'Steam')) THEN
                    OutArgs(2)=InArgs(2)
                    OutArgs(3)=blank
                  ELSE
                    OutArgs(3)=InArgs(2)
                    OutArgs(2)='UserDefinedFluidType'
                  ENDIF
                ENDIF
                OutArgs(4:CurArgs+1)=InArgs(3:CurArgs)
                CurArgs=CurArgs+1

              CASE('CONDENSERLOOP')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1)=InArgs(1)
                Num1=0
                hasfluidcoolers=.false.
                IF (ALLOCATED(PlantCondLoops)) THEN
                  DO Num1=1,NumPlantCondLoops
                    IF (.not. SameString(InArgs(1),PlantCondLoops(Num1)%LoopName)) CYCLE
                    IF (PlantCondLoops(Num1)%IsPlant) CYCLE
                    IF (.not. SameString(PlantCondLoops(Num1)%LoopFluid,'Water') .and. &
                        .not. SameString(PlantCondLoops(Num1)%LoopFluid,'Steam') ) THEN
                      OutArgs(3)=PlantCondLoops(Num1)%LoopFluid
                      OutArgs(2)='UserDefinedFluidType'
                    ELSE
                      OutArgs(2)=PlantCondLoops(Num1)%LoopFluid
                      OutArgs(3)=blank
                    ENDIF
                    hasfluidcoolers=PlantCondLoops(Num1)%HasFluidCoolers
                    EXIT
                  ENDDO
                ELSE
                  IF (SameString(InArgs(2),'Water') .or. SameString(InArgs(2),'Steam')) THEN
                    OutArgs(2)=InArgs(2)
                    OutArgs(3)=blank
                  ELSE
                    OutArgs(3)=InArgs(2)
                    OutArgs(2)='UserDefinedFluidType'
                  ENDIF
                ENDIF
                OutArgs(4)=InArgs(3)
                IF (SameString(InArgs(4),'AIR') .or. SameString(InArgs(4),'GROUND')) THEN
                  OutArgs(5)=InArgs(11)
                ELSE
                  OutArgs(5)=InArgs(4)
                ENDIF
                OutArgs(6:CurArgs+1)=InArgs(5:CurArgs)
                CurArgs=CurArgs+1
                if (CurArgs < NwObjMinFlds) THEN
                  OutArgs(CurArgs+1:NwObjMinFlds)=blank
                  CurArgs=NwObjMinFlds
                endif
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                IF (SameString(InArgs(4),'GROUND')) THEN
                  ObjectName='SetpointManager:FollowGroundTemperature'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=InArgs(1)//' Ground Control Temperature'
                  OutArgs(2)='Temperature'
                  OutArgs(3)='Site:GroundTemperature:Deep'
                  OutArgs(4)='0'
                  OutArgs(5)=InArgs(5)
                  OutArgs(6)=InArgs(6)
                  OutArgs(7)=InArgs(11)
                  CurArgs=7
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ENDIF
                IF (SameString(InArgs(4),'AIR')) THEN
                  IF (hasfluidcoolers) THEN
                    ObjectName='SetpointManager:FollowOutdoorAirTemperature'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=InArgs(1)//' Outdoor Air Drybulb Temperature'
                    OutArgs(2)='Temperature'
                    OutArgs(3)='OutdoorAirDryBulb'
                    OutArgs(4)='0'
                    OutArgs(5)=InArgs(5)
                    OutArgs(6)=InArgs(6)
                    OutArgs(7)=InArgs(11)
                    CurArgs=7
                    CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ELSE
                    ObjectName='SetpointManager:FollowOutdoorAirTemperature'
                    CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                    OutArgs(1)=InArgs(1)//' Outdoor Air Wetbulb Temperature'
                    OutArgs(2)='Temperature'
                    OutArgs(3)='OutdoorAirWetBulb'
                    OutArgs(4)='0'
                    OutArgs(5)=InArgs(5)
                    OutArgs(6)=InArgs(6)
                    OutArgs(7)=InArgs(11)
                    CurArgs=7
                    CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ENDIF
                ENDIF
                Written=.true.
                !CYCLE

              CASE('PIPE:INDOOR')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                IF (CurArgs > 5) THEN
                  OutArgs(5:CurArgs-1)=InArgs(6:CurArgs)
                ENDIF
                CurArgs=CurArgs-1

              CASE('PIPE:OUTDOOR')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                IF (CurArgs > 5) THEN
                  OutArgs(5:CurArgs-1)=InArgs(6:CurArgs)
                ENDIF
                CurArgs=CurArgs-1

              CASE('PIPE:UNDERGROUND')
                nodiff=.false.
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                OutArgs(1:4)=InArgs(1:4)
                IF (CurArgs > 5) THEN
                  OutArgs(5:CurArgs-1)=InArgs(6:CurArgs)
                ENDIF
                CurArgs=CurArgs-1

              CASE('BOILER:HOTWATER')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:4)=InArgs(1:4)
                OutArgs(5)='LeavingBoiler'
                OutArgs(6:CurArgs+1)=InArgs(5:CurArgs)
                CurArgs=CurArgs+1
                if (CurArgs < NwObjMinFlds) THEN
                  OutArgs(CurArgs+1:NwObjMinFlds)=blank
                  CurArgs=NwObjMinFlds
                endif
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ! Check setpoint for variable flow
                Num4=FindItemInList(InArgs(1),VFEquipment%CompName,NumVFEquipment)
                IF (Num4 > 0) THEN
                ! Is variable flow, is outlet node already in setpoint manage?
                  Num3=FindItemInList(VFEquipment(Num4)%OutletNodeName,SetpointManagedNodes%NodeName,SPManCount)
                  IF (Num3 == 0) THEN  ! nope, have to create one
                    Num2=FindItemInList(VFEquipment(Num4)%PlantLoopName,SetpointManagedNodes%PlantLoopName,SPManCount)
                    IF (Num2 > 0 .and. VFEquipment(Num4)%PlantLoopName /= ' ') THEN
                      DO Num1=1,NumIDFRecords
                        IF (.not. SameString(IDFRecords(Num1)%Name,SetpointManagedNodes(Num2)%CompType)) CYCLE
                        IF (.not. SameString(IDFRecords(Num1)%Alphas(1),SetpointManagedNodes(Num2)%Name)) CYCLE
                        ! Just duplicate this item
                        ObjectName=IDFRecords(Num1)%Name
                        CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                        CurArgs=IDFRecords(Num1)%NumAlphas+IDFRecords(Num1)%NumNumbers
                        OutArgs=Blank
                        NA=0
                        NN=0
                        DO Arg=1,CurArgs
                          IF (NwAorN(Arg)) THEN
                            NA=NA+1
                            OutArgs(Arg)=IDFRecords(Num1)%Alphas(NA)
                          ELSE
                            NN=NN+1
                            OutArgs(Arg)=IDFRecords(Num1)%Numbers(NN)
                          ENDIF
                        ENDDO
                        IF (VFEquipment(Num4)%SPNameInsert == ' ') THEN
                          OutArgs(1)=trim(InArgs(1))//' Setpoint Manager'
                        ELSE
                          OutArgs(1)=trim(InArgs(1))//' '//trim(VFEquipment(Num4)%SPNameInsert)//' Setpoint Manager'
                        ENDIF
                        OutArgs(SetpointManagedNodes(Num2)%NodeNumField)=VFEquipment(Num4)%OutletNodeName
                      ENDDO
                      CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                    ELSE ! error
                      CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                       'Could not create setpoint manager for VariableFlow Boiler:HotWater="'//trim(OutArgs(1))//'"')
                    ENDIF
                  ENDIF
                ENDIF
                Written=.true.
                !CYCLE

              CASE('GROUNDHEATEXCHANGER:VERTICAL')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:9)=InArgs(1:9)
                OutArgs(10:13)=InArgs(11:14)
                OutArgs(14:CurArgs-4)=InArgs(18:CurArgs)
                CurArgs=CurArgs-4

!              CASE('BOILER:HOTWATER')
!                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)

              CASE('CHILLER:ELECTRIC:EIR','CHILLER:ELECTRIC:REFORMULATEDEIR','CHILLER:ELECTRIC','CHILLER:ABSORPTION:INDIRECT','CHILLER:ABSORPTION',  &
                   'CHILLER:CONSTANTCOP','CHILLER:ENGINEDRIVEN','CHILLER:COMBUSTIONTURBINE','CHILLERHEATER:ABSORPTION:DIRECTFIRED')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                ! Check setpoint for variable flow
                Num4=FindItemInList(InArgs(1),VFEquipment%CompName,NumVFEquipment)
                IF (Num4 > 0) THEN
                ! Is variable flow, is outlet node already in setpoint manage?
                  Num3=FindItemInList(VFEquipment(Num4)%OutletNodeName,SetpointManagedNodes%NodeName,SPManCount)
                  IF (Num3 == 0) THEN  ! nope, have to create one
                    Num2=FindItemInList(VFEquipment(Num4)%PlantLoopName,SetpointManagedNodes%PlantLoopName,SPManCount)
                    IF (Num2 > 0 .and. VFEquipment(Num4)%PlantLoopName /= ' ') THEN
                      DO Num1=1,NumIDFRecords
                        IF (.not. SameString(IDFRecords(Num1)%Name,SetpointManagedNodes(Num2)%CompType)) CYCLE
                        IF (.not. SameString(IDFRecords(Num1)%Alphas(1),SetpointManagedNodes(Num2)%Name)) CYCLE
                        ! Just duplicate this item
                        ObjectName=IDFRecords(Num1)%Name
                        CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                        CurArgs=IDFRecords(Num1)%NumAlphas+IDFRecords(Num1)%NumNumbers
!                        InArgs=Blank
                        OutArgs=Blank
                        NA=0
                        NN=0
                        DO Arg=1,CurArgs
                          IF (NwAorN(Arg)) THEN
                            NA=NA+1
                            OutArgs(Arg)=IDFRecords(Num1)%Alphas(NA)
                          ELSE
                            NN=NN+1
                            OutArgs(Arg)=IDFRecords(Num1)%Numbers(NN)
                          ENDIF
                        ENDDO
                        IF (VFEquipment(Num4)%SPNameInsert == ' ') THEN
                          OutArgs(1)=trim(InArgs(1))//' Setpoint Manager'
                        ELSE
                          OutArgs(1)=trim(InArgs(1))//' '//trim(VFEquipment(Num4)%SPNameInsert)//' Setpoint Manager'
                        ENDIF
                        OutArgs(SetpointManagedNodes(Num2)%NodeNumField)=VFEquipment(Num4)%OutletNodeName
                      ENDDO
                      CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                      ! chiller heater...
                      IF (MakeUPPERCase(TRIM(IDFRecords(Num)%Name)) == 'CHILLERHEATER:ABSORPTION:DIRECTFIRED') THEN
                        Num4=Num4+1  ! next VF Eq is heater
                        Num3=FindItemInList(VFEquipment(Num4)%OutletNodeName,SetpointManagedNodes%NodeName,SPManCount)
                        IF (Num3 == 0) THEN  ! nope, have to create one
                          Num2=FindItemInList(VFEquipment(Num4)%PlantLoopName,SetpointManagedNodes%PlantLoopName,SPManCount)
                          IF (Num2 > 0 .and. VFEquipment(Num4)%PlantLoopName /= ' ') THEN
                            DO Num1=1,NumIDFRecords
                              IF (.not. SameString(IDFRecords(Num1)%Name,SetpointManagedNodes(Num2)%CompType)) CYCLE
                              IF (.not. SameString(IDFRecords(Num1)%Alphas(1),SetpointManagedNodes(Num2)%Name)) CYCLE
                              ! Just duplicate this item
                              ObjectName=IDFRecords(Num1)%Name
                              CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                              CurArgs=IDFRecords(Num1)%NumAlphas+IDFRecords(Num1)%NumNumbers
                              OutArgs=Blank
                              NA=0
                              NN=0
                              DO Arg=1,CurArgs
                                IF (NwAorN(Arg)) THEN
                                  NA=NA+1
                                  OutArgs(Arg)=IDFRecords(Num1)%Alphas(NA)
                                ELSE
                                  NN=NN+1
                                  OutArgs(Arg)=IDFRecords(Num1)%Numbers(NN)
                                ENDIF
                              ENDDO
                              IF (VFEquipment(Num4)%SPNameInsert == ' ') THEN
                                OutArgs(1)=trim(InArgs(1))//' Setpoint Manager'
                              ELSE
                                OutArgs(1)=trim(InArgs(1))//' '//trim(VFEquipment(Num4)%SPNameInsert)//' Setpoint Manager'
                              ENDIF
                              OutArgs(SetpointManagedNodes(Num2)%NodeNumField)=VFEquipment(Num4)%OutletNodeName
                            ENDDO
                            CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                          ENDIF
                        ENDIF
                      ENDIF
                    ELSE ! error
                      CALL writePreProcessorObject(DifLfn,ProgNameConversion,'Warning',  &
                       'Could not create setpoint manager for VariableFlow '//     &
                           trim(ObjectName)//'="'//trim(OutArgs(1))//'"')
                    ENDIF
                  ENDIF
                ENDIF
                Written=.true.
                !CYCLE

              CASE('FLUIDPROPERTIES:NAMES')
                ObjectName='FluidProperties:Name'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                DO Arg=1,CurArgs,2
                  OutArgs(1)=InArgs(Arg)
                  OutArgs(2)=InArgs(Arg+1)
                  CALL WriteOutIDFLines(DifLfn,ObjectName,2,OutArgs,NwFldNames,NwFldUnits)
                ENDDO
                Written=.true.
                !CYCLE

              CASE('FLUIDPROPERTIES:GLYCOLCONCENTRATIONS')
                ObjectName='FluidProperties:GlycolConcentration'
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                DO Arg=1,CurArgs,3
                  OutArgs(1)=InArgs(Arg)
                  IF (SameString(InArgs(Arg+1),'EthyleneGlycol') .or. SameString(InArgs(Arg+1),'PropyleneGlycol')) THEN
                    OutArgs(2)=InArgs(Arg+1)
                    OutArgs(3)=' '
                  ELSE
                    OutArgs(2)='UserDefinedGlycolType'
                    OutArgs(3)=InArgs(Arg+1)
                  ENDIF
                  OutArgs(4)=InArgs(Arg+2)
                  CALL WriteOutIDFLines(DifLfn,ObjectName,4,OutArgs,NwFldNames,NwFldUnits)
                ENDDO
                Written=.true.
                !CYCLE

              CASE('HVACTEMPLATE:SYSTEM:UNITARY')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:11)=InArgs(1:11)
                OutArgs(12)=blank
                OutArgs(13:17)=InArgs(12:16)
                OutArgs(18)=blank
                OutArgs(19:29)=InArgs(17:27)
                OutArgs(30)=blank
                OutArgs(31:47)=InArgs(28:44)
                CurArgs=47

              CASE('HVACTEMPLATE:SYSTEM:UNITARYHEATPUMP:AIRTOAIR')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:14)=InArgs(1:14)
                OutArgs(15)=blank
                OutArgs(16:20)=InArgs(15:19)
                OutArgs(21)=blank
                OutArgs(22:56)=InArgs(20:54)
                CurArgs=56

              CASE('HVACTEMPLATE:SYSTEM:VAV')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                OutArgs(1:33)=InArgs(1:33)
                OutArgs(34)=blank
                OutArgs(35:54)=InArgs(34:53)
                CurArgs=54

              CASE('ZONE')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                if (CurArgs > 9) then
                  OutArgs(1:9)=InArgs(1:9)
                  OutArgs(10)='autocalculate'
                  OutArgs(11:CurArgs+1)=InArgs(10:CurArgs)
                  CurArgs=CurArgs+1
                else
                  OutArgs(1:CurArgs)=InArgs(1:CurArgs)
                endif

              CASE('ZONEHVAC:IDEALLOADSAIRSYSTEM')
                CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                nodiff=.false.
                ! min-fields = 12
                OutArgs(1)=InArgs(1)
                OutArgs(2)=blank
                OutArgs(3)=InArgs(2)
                OutArgs(4)=blank
                OutArgs(5:8)=InArgs(3:6)
                If (SameString(InArgs(7),'Limit')) then
                  OutArgs(9)='LimitFlowRate'
                else
                  OutArgs(9)=InArgs(7)
                endif
                OutArgs(10)=InArgs(8)
                OutArgs(11)=blank
                If (SameString(InArgs(9),'Limit')) then
                  OutArgs(12)='LimitFlowRate'
                else
                  OutArgs(12)=InArgs(9)
                endif
                OutArgs(13)=InArgs(10)
                OutArgs(14)=blank
                If (CurArgs >= 13) then
                  OutArgs(15)=InArgs(13)
                else
                  OutArgs(15)=blank
                Endif
                IF (CurArgs >= 14) then
                  OutArgs(16)=InArgs(14)
                else
                  OutArgs(16)=blank
                Endif
                OutArgs(17)='ConstantSupplyHumidityRatio'
                OutArgs(18)=blank
                OutArgs(19)='ConstantSupplyHumidityRatio'
                OutArgs(20:26)=blank
                If (SameString(InArgs(11),'OutdoorAir')) then
                  OutArgs(20)=trim(OutArgs(1))//' DSOA'
                  OutArgs(21)=trim(OutArgs(1))//' Outdoor Air Inlet Node'
                  CurArgs=26
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ! Get DSOA object def
                  ObjectName='DesignSpecification:OutdoorAir'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(20)
                  OutArgs(2)='Flow/Zone'
                  OutArgs(3:4)=blank
                  OutArgs(5)=InArgs(12)
                  OutArgs(6:7)=blank
                  CurArgs=7
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                  ! Make Outdoor Air node def
                  ObjectName='OutdoorAir:Node'
                  CALL GetNewObjectDefInIDD(ObjectName,NwNumArgs,NwAorN,NwReqFld,NwObjMinFlds,NwFldNames,NwFldDefaults,NwFldUnits)
                  OutArgs(1)=OutArgs(21)
                  OutArgs(2)='-1'
                  CurArgs=2
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                else
                  CurArgs=26
                  CALL WriteOutIDFLines(DifLfn,ObjectName,CurArgs,OutArgs,NwFldNames,NwFldUnits)
                Endif
                Written=.true.
                !CYCLE

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
              if (xcount == IDFRecords(Num)%CommtE) WRITE(DifLfn,fmta) ' '
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
      IF (ALLOCATED(FansInIDF)) DEALLOCATE(FansInIDF)

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

!END SUBROUTINE CreateNewIDFUsingRules
