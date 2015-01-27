// EnergyPlus Headers
#include <DataHVACGlobals.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataHVACGlobals {

	// MODULE INFORMATION:
	//       MODIFIED       Craig Wray 22Aug2010 Added Fan Component Model

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for HVAC variables which are considered
	// to be "global" in nature in EnergyPlus.

	// METHODOLOGY EMPLOYED:

	// REFERENCES:

	// OTHER NOTES:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	Real64 const SmallTempDiff( 1.0E-5 );
	Real64 const SmallMassFlow( 0.001 );
	Real64 const VerySmallMassFlow( 1.0E-30 );
	Real64 const SmallLoad( 1.0 );
	Real64 const TempControlTol( 0.1 ); // temperature control tolerance for packaged equip. [deg C]
	Real64 const SmallAirVolFlow( 0.001 );
	Real64 const SmallWaterVolFlow( 1.0E-9 );
	Real64 const BlankNumeric( -99999.0 ); // indicates numeric input field was blank
	Real64 const RetTempMax( 60.0 ); // maximum return air temperature [deg C]
	Real64 const RetTempMin( -30.0 ); // minimum return air temperature [deg C]

	// Sizing types
	int const CoolingAirflowSizing( 1 ); // request sizing for cooling air flow rate
	int const CoolingWaterflowSizing( 2 ); // request sizing for cooling water flow rate
	int const HeatingWaterflowSizing( 3 ); // request sizing for heating coil water flow rate
	int const CoolingWaterDesAirInletTempSizing( 4 ); // request sizing for cooling water coil inlet air temp
	int const CoolingWaterDesAirInletHumRatSizing( 5 ); // request sizing for cooling water coil inlet air humidity ratio
	int const CoolingWaterDesWaterInletTempSizing( 6 ); // request sizing for cooling water coil inlet water temp
	int const CoolingWaterDesAirOutletTempSizing( 7 ); // request sizing for cooling water coil outlet air temp
	int const CoolingWaterDesAirOutletHumRatSizing( 8 ); // request sizing for cooling water coil outlet air humidity ratio
	int const CoolingWaterNumofTubesPerRowSizing( 9 ); // request sizing for cooling water coil number of tubes per row
	int const HeatingWaterDesAirInletTempSizing( 10 ); // request sizing for heating water coil inlet air temp
	int const HeatingWaterDesAirInletHumRatSizing( 11 ); // request sizing for heating water coil inlet air humidity ratio
	int const HeatingWaterDesCoilLoadUsedForUASizing( 12 ); // request sizing for heating water coil capacity used for UA sizing
	int const HeatingWaterDesCoilWaterVolFlowUsedForUASizing( 13 ); // request sizing for heating water coil volume flow rate used for UA sizing
	int const HeatingAirflowSizing( 14 ); // request sizing for heating air flow rate
	int const HeatingAirflowUASizing( 15 ); // request sizing for heating air flow rate
	int const SystemAirflowSizing( 16 ); // request sizing for system air flow rate
	int const CoolingCapacitySizing( 17 ); // request sizing for cooling capacity
	int const HeatingCapacitySizing( 18 ); // request sizing for heating capacity
	int const WaterHeatingCapacitySizing( 19 ); // request sizing for water-side heating capacity
	int const WaterHeatingCoilUASizing( 20 ); // request sizing for heating coil UA
	int const SystemCapacitySizing( 21 ); // request sizing for system capacity
	int const CoolingSHRSizing( 22 ); // request sizing for cooling SHR
	int const HeatingDefrostSizing( 23 ); // request sizing for heating defrost capacity
	int const AutoCalculateSizing ( 24 ); // identifies an autocalulate input

	// Condenser Type (using same numbering scheme as for chillers)
	int const AirCooled( 1 ); // Air-cooled condenser
	int const WaterCooled( 2 ); // Water-cooled condenser
	int const EvapCooled( 3 ); // Evaporatively-cooled condenser
	int const WaterHeater( 4 ); // Condenser heats water (e.g., in water heater tank)

	// The following parameters are used for system availability status
	int const NoAction( 0 );
	int const ForceOff( 1 );
	int const CycleOn( 2 );
	int const CycleOnZoneFansOnly( 3 );
	// The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
	int const SingleHeatingSetPoint( 1 );
	int const SingleCoolingSetPoint( 2 );
	int const SingleHeatCoolSetPoint( 3 );
	int const DualSetPointWithDeadBand( 4 );
	// parameters describing air duct type
	int const Main( 1 );
	int const Cooling( 2 );
	int const Heating( 3 );
	int const Other( 4 );
	int const RAB( 5 );
	// parameters describing fan types
	int const NumAllFanTypes( 5 ); // cpw22Aug2010 (was 4)

	// fan types
	int const FanType_SimpleConstVolume( 1 );
	int const FanType_SimpleVAV( 2 );
	int const FanType_SimpleOnOff( 3 );
	int const FanType_ZoneExhaust( 4 );
	int const FanType_ComponentModel( 5 ); // cpw22Aug2010 (new)
	// Fan Minimum Flow Fraction Input Method
	int const MinFrac( 1 );
	int const FixedMin( 2 );
	// Fan mode
	int const CycFanCycCoil( 1 ); // Cycling fan, cycling coil = 1
	int const ContFanCycCoil( 2 ); // Continuous fan, cycling coil = 2
	// Fan placement
	int const BlowThru( 1 ); // fan before coil
	int const DrawThru( 2 ); // fan after coil
	// OA Controller Heat Recovery Bypass Control Types
	int const BypassWhenWithinEconomizerLimits( 0 ); // heat recovery controlled by economizer limits
	int const BypassWhenOAFlowGreaterThanMinimum( 1 ); // heat recovery ON at minimum OA in economizer mode

	FArray1D_string const cFanTypes( NumAllFanTypes, { "Fan:ConstantVolume", "Fan:VariableVolume", "Fan:OnOff", "Fan:ZoneExhaust", "Fan:ComponentModel" } ); // cpw22Aug2010 | cpw22Aug2010 (new)

	// parameters describing unitary systems
	int const NumUnitarySystemTypes( 7 );
	// Furnace/Unitary System Types
	int const Furnace_HeatOnly( 1 );
	int const Furnace_HeatCool( 2 );
	int const UnitarySys_HeatOnly( 3 );
	int const UnitarySys_HeatCool( 4 );
	int const UnitarySys_HeatPump_AirToAir( 5 );
	int const UnitarySys_HeatPump_WaterToAir( 6 );
	int const UnitarySystem_AnyCoilType( 7 );
	FArray1D_string const cFurnaceTypes( NumUnitarySystemTypes, { "AirLoopHVAC:Unitary:Furnace:HeatOnly", "AirLoopHVAC:Unitary:Furnace:HeatCool", "AirLoopHVAC:UnitaryHeatOnly", "AirLoopHVAC:UnitaryHeatCool", "AirLoopHVAC:UnitaryHeatPump:AirToAir", "AirLoopHVAC:UnitaryHeatPump:WaterToAir", "AirLoopHVAC:UnitarySystem" } );

	// parameters describing coil types
	int const NumAllCoilTypes( 29 );

	int const CoilDX_CoolingSingleSpeed( 1 );
	int const CoilDX_HeatingEmpirical( 2 );
	int const CoilDX_CoolingTwoSpeed( 3 );
	int const CoilDX_CoolingHXAssisted( 4 );
	int const CoilDX_CoolingTwoStageWHumControl( 5 );
	int const CoilDX_HeatPumpWaterHeater( 6 );
	int const CoilDX_MultiSpeedCooling( 7 );
	int const CoilDX_MultiSpeedHeating( 8 );

	int const Coil_HeatingGas( 9 );
	int const Coil_HeatingGas_MultiStage( 10 );
	int const Coil_HeatingElectric( 11 );
	int const Coil_HeatingElectric_MultiStage( 12 );
	int const Coil_HeatingDesuperheater( 13 );

	int const Coil_CoolingWater( 14 );
	int const Coil_CoolingWaterDetailed( 15 );
	int const Coil_HeatingWater( 16 );
	int const Coil_HeatingSteam( 17 );
	int const CoilWater_CoolingHXAssisted( 18 );

	int const Coil_CoolingWaterToAirHP( 19 );
	int const Coil_HeatingWaterToAirHP( 20 );
	int const Coil_CoolingWaterToAirHPSimple( 21 );
	int const Coil_HeatingWaterToAirHPSimple( 22 );
	int const CoilVRF_Cooling( 23 );
	int const CoilVRF_Heating( 24 );

	int const CoilDX_PackagedThermalStorageCooling( 25 );

	int const Coil_CoolingWaterToAirHPVSEquationFit( 26 );
	int const Coil_HeatingWaterToAirHPVSEquationFit( 27 );
	int const Coil_CoolingAirToAirVariableSpeed( 28 );
	int const Coil_HeatingAirToAirVariableSpeed( 29 );

	// Water to air HP coil types
	int const WatertoAir_Simple( 1 );
	int const WatertoAir_ParEst( 2 );
	int const WatertoAir_VarSpeedEquationFit( 3 );
	int const WatertoAir_VarSpeedLooUpTable( 4 );

	// Water to Air HP Water Flow Mode
	int const WaterCycling( 1 ); // water flow cycles with compressor
	int const WaterConstant( 2 ); // water flow is constant
	int const WaterConstantOnDemand( 3 ); // water flow is constant whenever the coil is operational - this is the only method used in EP V7.2 and earlier

	FArray1D_string const cAllCoilTypes( NumAllCoilTypes, { "Coil:Cooling:DX:SingleSpeed", "Coil:Heating:DX:SingleSpeed", "Coil:Cooling:DX:TwoSpeed", "CoilSystem:Cooling:DX:HeatExchangerAssisted", "Coil:Cooling:DX:TwoStageWithHumidityControlMode", "Coil:WaterHeating:AirToWaterHeatPump", "Coil:Cooling:DX:MultiSpeed", "Coil:Heating:DX:MultiSpeed", "Coil:Heating:Gas", "Coil:Heating:Gas:MultiStage", "Coil:Heating:Electric", "Coil:Heating:Electric:MultiStage", "Coil:Heating:Desuperheater", "Coil:Cooling:Water", "Coil:Cooling:Water:DetailedGeometry", "Coil:Heating:Water", "Coil:Heating:Steam", "CoilSystem:Cooling:Water:HeatExchangerAssisted", "Coil:Cooling:WaterToAirHeatPump:ParameterEstimation", "Coil:Heating:WaterToAirHeatPump:ParameterEstimation", "Coil:Cooling:WaterToAirHeatPump:EquationFit", "Coil:Heating:WaterToAirHeatPump:EquationFit", "Coil:Cooling:DX:VariableRefrigerantFlow", "Coil:Heating:DX:VariableRefrigerantFlow", "Coil:Cooling:DX:SingleSpeed:ThermalStorage", "Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit", "Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit", "Coil:Cooling:DX:VariableSpeed", "Coil:Heating:DX:VariableSpeed" } );

	// parameters describing coil performance types
	int const CoilPerfDX_CoolBypassEmpirical( 100 );

	// Airflow per total capacity range (Regular DX coils)
	Real64 const MaxRatedVolFlowPerRatedTotCap1( 0.00006041 ); // m3/s per watt = 450 cfm/ton
	Real64 const MinRatedVolFlowPerRatedTotCap1( 0.00004027 ); // m3/s per watt = 300 cfm/ton
	Real64 const MaxHeatVolFlowPerRatedTotCap1( 0.00008056 ); // m3/s per watt = 600 cfm/ton
	Real64 const MaxCoolVolFlowPerRatedTotCap1( 0.00006713 ); // m3/s per watt = 500 cfm/ton
	Real64 const MinOperVolFlowPerRatedTotCap1( 0.00002684 ); // m3/s per watt = 200 cfm/ton

	// 100% DOAS DX coils Airflow per total capacity ratio
	Real64 const MaxRatedVolFlowPerRatedTotCap2( 0.00003355 ); // m3/s per watt = 250 cfm/ton
	Real64 const MinRatedVolFlowPerRatedTotCap2( 0.00001677 ); // m3/s per watt = 125 cfm/ton
	Real64 const MaxHeatVolFlowPerRatedTotCap2( 0.00004026 ); // m3/s per watt = 300 cfm/ton
	Real64 const MaxCoolVolFlowPerRatedTotCap2( 0.00004026 ); // m3/s per watt = 300 cfm/ton
	Real64 const MinOperVolFlowPerRatedTotCap2( 0.00001342 ); // m3/s per watt = 100 cfm/ton

	FArray1D< Real64 > MaxRatedVolFlowPerRatedTotCap( 2, { MaxRatedVolFlowPerRatedTotCap1, MaxRatedVolFlowPerRatedTotCap2 } );
	FArray1D< Real64 > MinRatedVolFlowPerRatedTotCap( 2, { MinRatedVolFlowPerRatedTotCap1, MinRatedVolFlowPerRatedTotCap2 } );
	FArray1D< Real64 > MaxHeatVolFlowPerRatedTotCap( 2, { MaxHeatVolFlowPerRatedTotCap1, MaxHeatVolFlowPerRatedTotCap2 } );
	FArray1D< Real64 > MaxCoolVolFlowPerRatedTotCap( 2, { MaxCoolVolFlowPerRatedTotCap1, MaxCoolVolFlowPerRatedTotCap2 } );
	FArray1D< Real64 > MinOperVolFlowPerRatedTotCap( 2, { MinOperVolFlowPerRatedTotCap1, MinOperVolFlowPerRatedTotCap2 } );

	// dx coil type (DXCT)
	int const RegularDXCoil(1); // Regular DX coils or mixed air dx coils
	int const DOASDXCoil(2); // 100% DOAS DX coils
	int DXCT(1); // dx coil type: regular DX coil ==1, 100% DOAS DX coil = 2

	// Parameters describing Heat Exchanger types
	int const NumHXTypes( 3 );

	int const HX_AIRTOAIR_FLATPLATE( 1 );
	int const HX_AIRTOAIR_GENERIC( 2 );
	int const HX_DESICCANT_BALANCED( 3 );

	FArray1D_string const cHXTypes( NumHXTypes, { "HeatExchanger:AirToAir:FlatPlate", "HeatExchanger:AirToAir:SensibleAndLatent", "HeatExchanger:Desiccant:BalancedFlow" } );

	// Parameters describing air terminal mixers
	int const NumATMixerTypes( 2 );

	int const No_ATMixer( 0 );
	int const ATMixer_InletSide( 1 );
	int const ATMixer_SupplySide( 2 );

	FArray1D_string const cATMixerTypes( NumATMixerTypes, { "AirTerminal:SingleDuct:InletSideMixer", "AirTerminal:SingleDuct:SupplySideMixer" } );
	bool const ATMixerExists( true );

	// Parameters describing variable refrigerant flow terminal unit types
	int const NumVRFTUTypes( 1 );

	int const VRFTUType_ConstVolume( 1 );

	FArray1D_string const cVRFTUTypes( NumVRFTUTypes, std::string( "ZoneHVAC:TerminalUnit:VariableRefrigerantFlow" ) );

	// VRF Heating Performance Curve Temperature Type
	int const NumVRFHeatingPerformanceOATTypes( 2 );
	int const WetBulbIndicator( 1 );
	int const DryBulbIndicator( 2 );

	FArray1D_string const cVRFHeatingPerformanceOATTypes( NumVRFHeatingPerformanceOATTypes, { "WetBulbTemperature", "DryBulbTemperature" } );

	// parameter concerning the amount of change in zone temperature is needed
	// for oscillation of zone temperature to be detected.
	Real64 const OscillateMagnitude( 0.15 );

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS

	// MODULE VARIABLE DECLARATIONS:

	bool FirstTimeStepSysFlag( false ); // Set to true at the start of each sub-time step

	Real64 SysUpdateTimeInc( 0.0 ); // System Update Time Increment - the adaptive time step used by the HVAC simulation
	Real64 TimeStepSys( 0.0 ); // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
	Real64 SysTimeElapsed( 0.0 ); // elapsed system time in zone timestep (hours)
	Real64 FracTimeStepZone( 0.0 ); // System time step divided by the zone time step
	bool ShortenTimeStepSys( false ); // Logical flag that triggers shortening of system time step
	int NumOfSysTimeSteps( 1 ); // for current zone time step, number of system timesteps inside  it
	int NumOfSysTimeStepsLastZoneTimeStep( 1 ); // previous zone time step, num of system timesteps inside
	int LimitNumSysSteps( 0 );

	bool UseZoneTimeStepHistory( true ); // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
	int NumPlantLoops( 0 ); // Number of plant loops specified in simulation
	int NumCondLoops( 0 ); // Number of condenser plant loops specified in simulation
	int NumElecCircuits( 0 ); // Number of electric circuits specified in simulation
	int NumGasMeters( 0 ); // Number of gas meters specified in simulation
	int NumPrimaryAirSys( 0 ); // Number of primary HVAC air systems
	Real64 FanElecPower( 0.0 ); // fan power from last fan simulation
	Real64 OnOffFanPartLoadFraction( 1.0 ); // fan part-load fraction (Fan:OnOff)
	Real64 DXCoilTotalCapacity( 0.0 ); // DX coil total cooling capacity (eio report var for HPWHs)
	Real64 DXElecCoolingPower( 0.0 ); // Electric power consumed by DX cooling coil last DX simulation
	Real64 DXElecHeatingPower( 0.0 ); // Electric power consumed by DX heating coil last DX simulation
	Real64 ElecHeatingCoilPower( 0.0 ); // Electric power consumed by electric heating coil
	Real64 AirToAirHXElecPower( 0.0 ); // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
	// from last simulation in HeatRecovery.cc
	Real64 UnbalExhMassFlow( 0.0 ); // unbalanced zone exhaust from a zone equip component [kg/s]
	Real64 BalancedExhMassFlow( 0.0 ); // balanced zone exhaust (declared as so by user)  [kg/s]
	Real64 PlenumInducedMassFlow( 0.0 ); // secondary air mass flow rate induced from a return plenum [kg/s]
	bool TurnFansOn( false ); // If true overrides fan schedule and cycles fans on
	bool TurnFansOff( false ); // If True overides fan schedule and TurnFansOn and forces fans off
	bool ZoneCompTurnFansOn( false ); // If true overrides fan schedule and cycles fans on
	bool ZoneCompTurnFansOff( false ); // If True overides fan schedule and TurnFansOn and forces fans off
	bool SetPointErrorFlag( false ); // True if any needed setpoints not set; if true, program terminates
	bool DoSetPointTest( false ); // True one time only for sensed node setpoint test
	bool NightVentOn( false ); // set TRUE in SimAirServingZone if night ventilation is happening

	int NumTempContComps( 0 );
	Real64 HPWHInletDBTemp( 0.0 ); // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
	Real64 HPWHInletWBTemp( 0.0 ); // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
	Real64 HPWHCrankcaseDBTemp( 0.0 ); // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
	bool AirLoopInit( false ); // flag for whether InitAirLoops has been called
	bool AirLoopsSimOnce( false ); // True means that the air loops have been simulated once in this environment

	// Hybrid ventilation control part
	int NumHybridVentSysAvailMgrs( 0 ); // Number of hybrid ventilation control
	FArray1D_int HybridVentSysAvailAirLoopNum; // Airloop number in hybrid vent availability manager
	FArray1D_int HybridVentSysAvailVentCtrl; // Ventilation control action in hybrid vent availability manager
	FArray1D_int HybridVentSysAvailActualZoneNum; // Actual zone num in hybrid vent availability manager
	FArray1D_int HybridVentSysAvailANCtrlStatus; // AN control status in hybrid vent availability manager
	FArray1D_int HybridVentSysAvailMaster; // Master object name: Ventilation for simple; Zone name for AN
	FArray1D< Real64 > HybridVentSysAvailWindModifier; // Wind modifier for AirflowNetwork
	// For multispeed heat pump only
	Real64 MSHPMassFlowRateLow( 0.0 ); // Mass flow rate at low speed
	Real64 MSHPMassFlowRateHigh( 0.0 ); // Mass flow rate at high speed
	Real64 MSHPWasteHeat( 0.0 ); // Waste heat
	Real64 PreviousTimeStep( 0.0 ); // The time step length at the previous time step
	bool ShortenTimeStepSysRoomAir( false ); // Logical flag that triggers shortening of system time step

	Real64 deviationFromSetPtThresholdHtg( -0.2 ); // heating threshold for reporting setpoint deviation
	Real64 deviationFromSetPtThresholdClg( 0.2 ); // cooling threshold for reporting setpoint deviation

	bool SimAirLoopsFlag; // True when the air loops need to be (re)simulated
	bool SimElecCircuitsFlag; // True when electic circuits need to be (re)simulated
	bool SimPlantLoopsFlag; // True when the main plant loops need to be (re)simulated
	bool SimZoneEquipmentFlag; // True when zone equipment components need to be (re)simulated
	bool SimNonZoneEquipmentFlag; // True when non-zone equipment components need to be (re)simulated
	bool ZoneMassBalanceHVACReSim; // True when zone air mass flow balance and air loop needs (re)simulated

	// Object Data
	FArray1D< ZoneCompTypeData > ZoneComp;
	OptStartDataType OptStartData; // For optimum start
	FArray1D< ComponentSetPtData > CompSetPtEquip;

	//     NOTICE
	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.
	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in main.cc.
	//     NOTICE: The U.S. Government is granted for itself and others acting on its
	//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
	//     reproduce, prepare derivative works, and perform publicly and display publicly.
	//     Beginning five (5) years after permission to assert copyright is granted,
	//     subject to two possible five year renewals, the U.S. Government is granted for
	//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
	//     worldwide license in this data to reproduce, prepare derivative works,
	//     distribute copies to the public, perform publicly and display publicly, and to
	//     permit others to do so.
	//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // DataHVACGlobals

} // EnergyPlus
