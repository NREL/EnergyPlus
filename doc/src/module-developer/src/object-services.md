# Object Services

It is standard practice in EnergyPlus that only objects associated with a given module be accessed from the input data file using a GetObjectItem function call and that this type of function call only be executed during GetInput. For example, the module HVACDXSystem would only access the object DXSystem:AirLoop in the GetInput subroutine or the module HVACFanComponent would only access the different fan objects as shown in the example below. This programming standard requires that all modules be "responsible" for specific objects and no other module accesses the input data file for this specific object. Using this technique, maintenance of the EnergyPlus modules is limited to specific areas of software as input data for objects evolve for any reason.

*Module HVACDXSystem - SUBROUTINE GetDXCoolingSystemInput:*

    CALL GetObjectItem('DXSYSTEM:AIRLOOP',DXCoolSysNum,AlphArray, &

                           NumAlphas,NumArray,NumNums,IOSTAT)

*Module HVACFanComponent – SUBROUTINE GetFanInput:*

    CALL GetObjectItem('FAN:SIMPLE:CONSTVOLUME',SimpFanNum,AlphArray, &

                           NumAlphas,NumArray,NumNums,IOSTAT)

    CALL GetObjectItem('FAN:SIMPLE:VARIABLEVOLUME',VarVolFanNum, &

                           AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)

    CALL GetObjectItem('ZONE EXHAUST FAN',ExhFanNum,AlphArray, &

                           NumAlphas,NumArray,NumNums,IOSTAT)

    CALL GetObjectItem('FAN:SIMPLE:ONOFF',OnOffFanNum,AlphArray, &

                           NumAlphas,NumArray,NumNums,IOSTAT)

If module developers were allowed to access the input data file for other objects not related to a particular module, both the original module and the alternate module would have to be corrected each time the object changed. This poses a severe hazard for future development of EnergyPlus.

Module developers may at times require information for specific objects from other modules. When this occurs, the Information is "mined" through function or subroutine calls located in the other module. Existing function calls may be used or added to EnergyPlus as needed. In this way, a module developer could confirm that node information provided in a parent object matched the node information specified for its children object. Other error checking may also be performed in this manner. For example, if the module HVACDXSystem needed to know the capacity of the DX cooling coil, a function call to GetCoilCapacity in the DXCoil module would provide this information. The function GetCoilCapacity would the "Get" the input from the input data file if it has not already been accessed and provide the information back to the calling module.

A variety of examples exist to aid the module developer in this area of programming.

DXCoil.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)
    FUNCTION GetMinOATCompressor(CoilType,CoilName,ErrorsFound) RESULT(MinOAT)
    FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetSupplyAirFanOperatingMode(CoilType,CoilName,ErrorsFound) RESULT(OpMode)
    FUNCTION GetCoilCondenserNode(CoilType,CoilName,ErrorsFound) RESULT(CondNode)
    FUNCTION GetHPCoolingCoilIndex(HeatingCoilType, HeatingCoilName, HeatingCoilIndex) RESULT(DXCoolingCoilIndex)
~~~~~~~~~~~~~~~~~~~~

FanCoilUnits.f90:

~~~~~~~~~~~~~~~~~~~~

    INTEGER FUNCTION GetFanCoilZoneInletAirNode(FanCoilNum)
    INTEGER FUNCTION GetFanCoilOutAirNode(FanCoilNum)
    INTEGER FUNCTION GetFanCoilReturnAirNode(FanCoilNum)
    INTEGER FUNCTION GetFanCoilMixedAirNode(FanCoilNum)
~~~~~~~~~~~~~~~~~~~~

HeatRecovery.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetSupplyInletNode(HXName,ErrorsFound)
    FUNCTION GetSupplyOutletNode(HXName,ErrorsFound)
    FUNCTION GetSecondaryInletNode(HXName,ErrorsFound)
    FUNCTION GetSecondaryOutletNode(HXName,ErrorsFound)
~~~~~~~~~~~~~~~~~~~~

HVACFanComponent.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetFanDesignVolumeFlowRate(FanType,FanName,ErrorsFound) RESULT(DesignVolumeFlowRate
    FUNCTION GetFanInletNode(FanType,FanName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetFanOutletNode(FanType,FanName,ErrorsFound) RESULT(NodeNumber)
    SUBROUTINE GetFanVolFlow(FanIndex, FanVolFlow)
    SUBROUTINE GetFanType(FanName,FanType,ErrorsFound,ThisObjectType)
~~~~~~~~~~~~~~~~~~~~

HVACHeatingCoils.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)
    FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    SUBROUTINE GetHeatingCoilIndex(HeatingCoilName,HeatingCoilIndex,ErrorsFound)
~~~~~~~~~~~~~~~~~~~~

HVACHXAssistedCoolingCoil.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)
    FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetHXDXCoilName(CoilType,CoilName,ErrorsFound) RESULT(DXCoilName)
    FUNCTION GetCoilMaxWaterFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxWaterFlowRate)
~~~~~~~~~~~~~~~~~~~~

HVACStandAloneERV.f90

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetSupplyAirFlowRate(ERVType,ERVCtrlName,ErrorsFound) RESULT(AirFlowRate)
    FUNCTION GetSupplyAirInletNode(ERVType,ERVCtrlName,ErrorsFound) RESULT(AirInletNode)
    FUNCTION GetExhaustAirInletNode(ERVType,ERVCtrlName,ErrorsFound) RESULT(AirInletNode)
    INTEGER FUNCTION GetStandAloneERVOutAirNode(StandAloneERVNum)
    INTEGER FUNCTION GetStandAloneERVZoneInletAirNode(StandAloneERVNum)
    INTEGER FUNCTION GetStandAloneERVReturnAirNode(StandAloneERVNum)
~~~~~~~~~~~~~~~~~~~~

HVACSteamCoilComponent.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilMaxWaterFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxWaterFlowRate)
~~~~~~~~~~~~~~~~~~~~

HVACWaterCoilComponent.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilMaxWaterFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxWaterFlowRate)
    FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
~~~~~~~~~~~~~~~~~~~~

HVACWaterToAir.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)
    FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
    FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)
    FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)
~~~~~~~~~~~~~~~~~~~~

MixedAir.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetOAMixerNodeNumbers(OAMixerName,ErrorsFound) RESULT(OANodeNumbers)
    FUNCTION GetNumOAMixers() RESULT(NumberOfOAMixers)
    FUNCTION GetNumOAControllers() RESULT(NumberOfOAControllers)
    FUNCTION GetOAMixerReliefNodeNumber(OAMixerNum) RESULT(ReliefNodeNumber)
    FUNCTION GetOASystemNumber(OASysName) RESULT(OASysNumber)
    FUNCTION GetOAMixerInletNodeNumber(OAMixerNumber) RESULT(OAMixerInletNodeNumber)
    FUNCTION GetOAMixerReturnNodeNumber(OAMixerNumber) RESULT(OAMixerReturnNodeNumber)
    FUNCTION GetOAMixerMixedNodeNumber(OAMixerNumber) RESULT(OAMixerMixedNodeNumber)
~~~~~~~~~~~~~~~~~~~~

PackagedTerminalHeatPump.f90:

~~~~~~~~~~~~~~~~~~~~

    INTEGER FUNCTION GetPTHPZoneInletAirNode(PTHPNum)
    INTEGER FUNCTION GetPTHPOutAirNode(PTHPNum)
    INTEGER FUNCTION GetPTHPReturnAirNode(PTHPNum)
    INTEGER FUNCTION GetPTHPMixedAirNode(PTHPNum)
~~~~~~~~~~~~~~~~~~~~

PurchasedAirManager.f90:

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetPurchasedAirOutAirMassFlow(PurchAirNum) RESULT(OutAirMassFlow)
~~~~~~~~~~~~~~~~~~~~

SetpointManager.f90:

~~~~~~~~~~~~~~~~~~~~

    LOGICAL FUNCTION IsNodeOnSetPtManager(NodeNum,SetPtType)
~~~~~~~~~~~~~~~~~~~~

UnitVentilator.f90:

~~~~~~~~~~~~~~~~~~~~

    INTEGER FUNCTION GetUnitVentilatorOutAirNode(UnitVentNum)
    INTEGER FUNCTION GetUnitVentilatorZoneInletAirNode(UnitVentNum)
    INTEGER FUNCTION GetUnitVentilatorMixedAirNode(UnitVentNum)
    INTEGER FUNCTION GetUnitVentilatorReturnAirNode(UnitVentNum)
~~~~~~~~~~~~~~~~~~~~

WindowAC.f90:

~~~~~~~~~~~~~~~~~~~~

    INTEGER FUNCTION GetWindowACZoneInletAirNode(WindACNum)
    INTEGER FUNCTION GetWindowACOutAirNode(WindACNum)
    INTEGER FUNCTION GetWindowACReturnAirNode(WindACNum)
    INTEGER FUNCTION GetWindowACMixedAirNode(WindACNum)
~~~~~~~~~~~~~~~~~~~~

These kinds of calls are highly encouraged so that data is mined from the other modules rather than being declared public.

The function call for GetCoilCapacity in module DXCoil.f90 is shown below. Note that the format for these function calls have the same format as other subroutines throughout the EnergyPlus software. The example function shown below is declared as a public routine in module DXCoils for use by other modules through a USE statement.

*Module DXCoils.f90:*

PUBLIC  GetCoilCapacity

~~~~~~~~~~~~~~~~~~~~

    FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)

              ! FUNCTION INFORMATION:
              !       AUTHOR         Linda Lawrie
              !       DATE WRITTEN   February 2006
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS FUNCTION:
              ! This function looks up the coil capacity for the given
              ! coil and returns it.  If incorrect coil type or name
              ! is given, errorsfound is returned as true and capacity
              ! is returned as negative.

              ! METHODOLOGY EMPLOYED:
              ! na

              ! REFERENCES:
              ! na

              ! USE STATEMENTS:
      USE InputProcessor,  ONLY: FindItemInList

      IMPLICIT NONE ! Enforce explicit typing of all variables in
                    ! this routine

              ! FUNCTION ARGUMENT DEFINITIONS:
      CHARACTER(len=*), INTENT(IN) :: CoilType   ! must match coil
                                                 ! types in this module
      CHARACTER(len=*), INTENT(IN) :: CoilName   ! must match coil
                                                 ! names for the coil type
      LOGICAL, INTENT(INOUT)     :: ErrorsFound  ! set to true if problem
      REAL                       :: CoilCapacity ! returned capacity of
                                                 ! matched coil

              ! FUNCTION PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS:
              ! na

              ! DERIVED TYPE DEFINITIONS:
              ! na

              ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      INTEGER :: WhichCoil

      ! Obtains and Allocates DXCoils
      IF (GetCoilsInputFlag) THEN
        CALL GetDXCoils
        GetCoilsInputFlag = .FALSE.
      END IF

      IF (CoilType == 'COIL:DX:HEATINGEMPIRICAL' .or.   &
          CoilType == 'COIL:DX:COOLINGBYPASSFACTOREMPIRICAL') THEN
        WhichCoil=FindItemInList(CoilName,DXCoil%Name,NumDXCoils)
        IF (WhichCoil /= 0) THEN
          CoilCapacity=DXCoil(WhichCoil)%RatedTotCap(1)
        ENDIF
      ELSE
        WhichCoil=0
      ENDIF

      IF (WhichCoil == 0) THEN
        CALL ShowSevereError('Could not find CoilType="' &
                 //TRIM(CoilType)//'" with Name="'//TRIM(CoilName)//'"')
        ErrorsFound=.true.
        CoilCapacity=-1000.
      ENDIF

      RETURN

    END FUNCTION GetCoilCapacity
~~~~~~~~~~~~~~~~~~~~

Note that the function name in one module can be the same as a function name in a different module. In fact, for EnergyPlus this should be the case – the module should use a generic name that is typical of its function. The calling module should use a "local name" that better specifies the type of item it is accessing. For example, if module HVACFurnace required node or capacity information from identical functions contained in modules HVACHeatingCoils and DXCoils, these function names could easily be assigned more descriptive names in the HVACFurnace module as follows.

*Module HVACFurnace.f90:*

USE HeatingCoils,    ONLY:GetHeatingCoilCapacity=>GetCoilCapacity,

GetHeatingCoilInletNode=>GetCoilInletNode

USE DXCoils,            ONLY:GetDXCoilCapacity=>GetCoilCapacity,

GetDXCoilInletNode=>GetCoilInletNode