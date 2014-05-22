#ifndef DataHVACGlobals_hh_INCLUDED
#define DataHVACGlobals_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataHVACGlobals {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	extern Real64 const SmallTempDiff;
	extern Real64 const SmallMassFlow;
	extern Real64 const VerySmallMassFlow;
	extern Real64 const SmallLoad;
	extern Real64 const TempControlTol; // temperature control tolerance for packaged equip. [deg C]
	extern Real64 const SmallAirVolFlow;
	extern Real64 const SmallWaterVolFlow;
	extern Real64 const BlankNumeric; // indicates numeric input field was blank
	extern Real64 const RetTempMax; // maximum return air temperature [deg C]
	extern Real64 const RetTempMin; // minimum return air temperature [deg C]

	// Condenser Type (using same numbering scheme as for chillers)
	extern int const AirCooled; // Air-cooled condenser
	extern int const WaterCooled; // Water-cooled condenser
	extern int const EvapCooled; // Evaporatively-cooled condenser
	extern int const WaterHeater; // Condenser heats water (e.g., in water heater tank)

	// The following parameters are used for system availability status
	extern int const NoAction;
	extern int const ForceOff;
	extern int const CycleOn;
	extern int const CycleOnZoneFansOnly;
	// The following parameters describe the setpoint types in TempControlType(ActualZoneNum)
	extern int const SingleHeatingSetPoint;
	extern int const SingleCoolingSetPoint;
	extern int const SingleHeatCoolSetPoint;
	extern int const DualSetPointWithDeadBand;
	// parameters describing air duct type
	extern int const Main;
	extern int const Cooling;
	extern int const Heating;
	extern int const Other;
	extern int const RAB;
	// parameters describing fan types
	extern int const NumAllFanTypes; // cpw22Aug2010 (was 4)

	// fan types
	extern int const FanType_SimpleConstVolume;
	extern int const FanType_SimpleVAV;
	extern int const FanType_SimpleOnOff;
	extern int const FanType_ZoneExhaust;
	extern int const FanType_ComponentModel; // cpw22Aug2010 (new)
	// Fan Minimum Flow Fraction Input Method
	extern int const MinFrac;
	extern int const FixedMin;
	// Fan mode
	extern int const CycFanCycCoil; // Cycling fan, cycling coil = 1
	extern int const ContFanCycCoil; // Continuous fan, cycling coil = 2
	// Fan placement
	extern int const BlowThru; // fan before coil
	extern int const DrawThru; // fan after coil
	// OA Controller Heat Recovery Bypass Control Types
	extern int const BypassWhenWithinEconomizerLimits; // heat recovery controlled by economizer limits
	extern int const BypassWhenOAFlowGreaterThanMinimum; // heat recovery ON at minimum OA in economizer mode

	extern FArray1D_Fstring const cFanTypes; // cpw22Aug2010 | cpw22Aug2010 (new)

	// parameters describing unitary systems
	extern int const NumUnitarySystemTypes;
	// Furnace/Unitary System Types
	extern int const Furnace_HeatOnly;
	extern int const Furnace_HeatCool;
	extern int const UnitarySys_HeatOnly;
	extern int const UnitarySys_HeatCool;
	extern int const UnitarySys_HeatPump_AirToAir;
	extern int const UnitarySys_HeatPump_WaterToAir;
	extern int const UnitarySystem_AnyCoilType;
	extern FArray1D_Fstring const cFurnaceTypes;

	// parameters describing coil types
	extern int const NumAllCoilTypes;

	extern int const CoilDX_CoolingSingleSpeed;
	extern int const CoilDX_HeatingEmpirical;
	extern int const CoilDX_CoolingTwoSpeed;
	extern int const CoilDX_CoolingHXAssisted;
	extern int const CoilDX_CoolingTwoStageWHumControl;
	extern int const CoilDX_HeatPumpWaterHeater;
	extern int const CoilDX_MultiSpeedCooling;
	extern int const CoilDX_MultiSpeedHeating;

	extern int const Coil_HeatingGas;
	extern int const Coil_HeatingGas_MultiStage;
	extern int const Coil_HeatingElectric;
	extern int const Coil_HeatingElectric_MultiStage;
	extern int const Coil_HeatingDesuperheater;

	extern int const Coil_CoolingWater;
	extern int const Coil_CoolingWaterDetailed;
	extern int const Coil_HeatingWater;
	extern int const Coil_HeatingSteam;
	extern int const CoilWater_CoolingHXAssisted;

	extern int const Coil_CoolingWaterToAirHP;
	extern int const Coil_HeatingWaterToAirHP;
	extern int const Coil_CoolingWaterToAirHPSimple;
	extern int const Coil_HeatingWaterToAirHPSimple;
	extern int const CoilVRF_Cooling;
	extern int const CoilVRF_Heating;

	extern int const CoilDX_PackagedThermalStorageCooling;

	extern int const Coil_CoolingWaterToAirHPVSEquationFit;
	extern int const Coil_HeatingWaterToAirHPVSEquationFit;
	extern int const Coil_CoolingAirToAirVariableSpeed;
	extern int const Coil_HeatingAirToAirVariableSpeed;

	// Water to air HP coil types
	extern int const WatertoAir_Simple;
	extern int const WatertoAir_ParEst;
	extern int const WatertoAir_VarSpeedEquationFit;
	extern int const WatertoAir_VarSpeedLooUpTable;

	// Water to Air HP Water Flow Mode
	extern int const WaterCycling; // water flow cycles with compressor
	extern int const WaterConstant; // water flow is constant
	extern int const WaterConstantOnDemand; // water flow is constant whenever the coil is operational - this is the only method used in EP V7.2 and earlier

	extern FArray1D_Fstring const cAllCoilTypes;

	// parameters describing coil performance types
	extern int const CoilPerfDX_CoolBypassEmpirical;

	// Parameters describing Heat Exchanger types
	extern int const NumHXTypes;

	extern int const HX_AIRTOAIR_FLATPLATE;
	extern int const HX_AIRTOAIR_GENERIC;
	extern int const HX_DESICCANT_BALANCED;

	extern FArray1D_Fstring const cHXTypes;

	// Parameters describing air terminal mixers
	extern int const NumATMixerTypes;

	extern int const No_ATMixer;
	extern int const ATMixer_InletSide;
	extern int const ATMixer_SupplySide;

	extern FArray1D_Fstring const cATMixerTypes;
	extern bool const ATMixerExists;

	// Parameters describing variable refrigerant flow terminal unit types
	extern int const NumVRFTUTypes;

	extern int const VRFTUType_ConstVolume;

	extern FArray1D_Fstring const cVRFTUTypes;

	// VRF Heating Performance Curve Temperature Type
	extern int const NumVRFHeatingPerformanceOATTypes;
	extern int const WetBulbIndicator;
	extern int const DryBulbIndicator;

	extern FArray1D_Fstring const cVRFHeatingPerformanceOATTypes;

	// parameter concerning the amount of change in zone temperature is needed
	// for oscillation of zone temperature to be detected.
	extern Real64 const OscillateMagnitude;

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS

	// MODULE VARIABLE DECLARATIONS:

	extern bool FirstTimeStepSysFlag; // Set to true at the start of each sub-time step

	extern Real64 SysUpdateTimeInc; // System Update Time Increment - the adaptive time step used by the HVAC simulation
	extern Real64 TimeStepSys; // System Time Increment - the adaptive time step used by the HVAC simulation (hours)
	extern Real64 SysTimeElapsed; // elapsed system time in zone timestep (hours)
	extern Real64 FracTimeStepZone; // System time step divided by the zone time step
	extern bool ShortenTimeStepSys; // Logical flag that triggers shortening of system time step
	extern int NumOfSysTimeSteps; // for current zone time step, number of system timesteps inside  it
	extern int NumOfSysTimeStepsLastZoneTimeStep; // previous zone time step, num of system timesteps inside
	extern int LimitNumSysSteps;

	extern bool UseZoneTimeStepHistory; // triggers use of zone time step history, else system time step history, for ZTM1, ZTMx
	extern int NumPlantLoops; // Number of plant loops specified in simulation
	extern int NumCondLoops; // Number of condenser plant loops specified in simulation
	extern int NumElecCircuits; // Number of electric circuits specified in simulation
	extern int NumGasMeters; // Number of gas meters specified in simulation
	extern int NumPrimaryAirSys; // Number of primary HVAC air systems
	extern Real64 FanElecPower; // fan power from last fan simulation
	extern Real64 OnOffFanPartLoadFraction; // fan part-load fraction (Fan:OnOff)
	extern Real64 DXCoilTotalCapacity; // DX coil total cooling capacity (eio report var for HPWHs)
	extern Real64 DXElecCoolingPower; // Electric power consumed by DX cooling coil last DX simulation
	extern Real64 DXElecHeatingPower; // Electric power consumed by DX heating coil last DX simulation
	extern Real64 ElecHeatingCoilPower; // Electric power consumed by electric heating coil
	extern Real64 AirToAirHXElecPower; // Electric power consumed by Heat Exchanger:Air To Air (Generic or Flat Plate)
	// from last simulation in HeatRecovery.f90
	extern Real64 UnbalExhMassFlow; // unbalanced zone exhaust from a zone equip component [kg/s]
	extern Real64 BalancedExhMassFlow; // balanced zone exhaust (declared as so by user)  [kg/s]
	extern Real64 PlenumInducedMassFlow; // secondary air mass flow rate induced from a return plenum [kg/s]
	extern bool TurnFansOn; // If true overrides fan schedule and cycles fans on
	extern bool TurnFansOff; // If True overides fan schedule and TurnFansOn and forces fans off
	extern bool ZoneCompTurnFansOn; // If true overrides fan schedule and cycles fans on
	extern bool ZoneCompTurnFansOff; // If True overides fan schedule and TurnFansOn and forces fans off
	extern bool SetPointErrorFlag; // True if any needed setpoints not set; if true, program terminates
	extern bool DoSetPointTest; // True one time only for sensed node setpoint test
	extern bool NightVentOn; // set TRUE in SimAirServingZone if night ventilation is happening

	extern int NumTempContComps;
	extern Real64 HPWHInletDBTemp; // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
	extern Real64 HPWHInletWBTemp; // Used by curve objects when calculating DX coil performance for HEAT PUMP:WATER HEATER
	extern Real64 HPWHCrankcaseDBTemp; // Used for HEAT PUMP:WATER HEATER crankcase heater ambient temperature calculations
	extern bool AirLoopInit; // flag for whether InitAirLoops has been called
	extern bool AirLoopsSimOnce; // True means that the air loops have been simulated once in this environment

	// Hybrid ventilation control part
	extern int NumHybridVentSysAvailMgrs; // Number of hybrid ventilation control
	extern FArray1D_int HybridVentSysAvailAirLoopNum; // Airloop number in hybrid vent availability manager
	extern FArray1D_int HybridVentSysAvailVentCtrl; // Ventilation control action in hybrid vent availability manager
	extern FArray1D_int HybridVentSysAvailActualZoneNum; // Actual zone num in hybrid vent availability manager
	extern FArray1D_int HybridVentSysAvailANCtrlStatus; // AN control status in hybrid vent availability manager
	extern FArray1D_int HybridVentSysAvailMaster; // Master object name: Ventilation for simple; Zone name for AN
	extern FArray1D< Real64 > HybridVentSysAvailWindModifier; // Wind modifier for AirflowNetwork
	// For multispeed heat pump only
	extern Real64 MSHPMassFlowRateLow; // Mass flow rate at low speed
	extern Real64 MSHPMassFlowRateHigh; // Mass flow rate at high speed
	extern Real64 MSHPWasteHeat; // Waste heat
	extern Real64 PreviousTimeStep; // The time step length at the previous time step
	extern bool ShortenTimeStepSysRoomAir; // Logical flag that triggers shortening of system time step

	extern Real64 deviationFromSetPtThresholdHtg; // heating threshold for reporting setpoint deviation
	extern Real64 deviationFromSetPtThresholdClg; // cooling threshold for reporting setpoint deviation

	extern bool SimAirLoopsFlag; // True when the air loops need to be (re)simulated
	extern bool SimElecCircuitsFlag; // True when electic circuits need to be (re)simulated
	extern bool SimPlantLoopsFlag; // True when the main plant loops need to be (re)simulated
	extern bool SimZoneEquipmentFlag; // True when zone equipment components need to be (re)simulated
	extern bool SimNonZoneEquipmentFlag; // True when non-zone equipment components need to be (re)simulated
	extern bool ZoneMassBalanceHVACReSim; // True when zone air mass flow balance and air loop needs (re)simulated
	// Types

	struct ComponentSetPtData
	{
		// Members
		// CHARACTER(len=MaxNameLength) :: EquipOperListName
		Fstring EquipmentType;
		Fstring EquipmentName;
		int NodeNumIn;
		int NodeNumOut;
		Real64 EquipDemand;
		Real64 DesignFlowRate;
		Fstring HeatOrCool;
		int OpType;

		// Default Constructor
		ComponentSetPtData() :
			EquipmentType( MaxNameLength ),
			EquipmentName( MaxNameLength ),
			NodeNumIn( 0 ),
			NodeNumOut( 0 ),
			EquipDemand( 0.0 ),
			DesignFlowRate( 0.0 ),
			HeatOrCool( 7 ),
			OpType( 0 )
		{}

		// Member Constructor
		ComponentSetPtData(
			Fstring const & EquipmentType,
			Fstring const & EquipmentName,
			int const NodeNumIn,
			int const NodeNumOut,
			Real64 const EquipDemand,
			Real64 const DesignFlowRate,
			Fstring const & HeatOrCool,
			int const OpType
		) :
			EquipmentType( MaxNameLength, EquipmentType ),
			EquipmentName( MaxNameLength, EquipmentName ),
			NodeNumIn( NodeNumIn ),
			NodeNumOut( NodeNumOut ),
			EquipDemand( EquipDemand ),
			DesignFlowRate( DesignFlowRate ),
			HeatOrCool( 7, HeatOrCool ),
			OpType( OpType )
		{}

	};

	struct DefineZoneCompAvailMgrs
	{
		// Members
		int NumAvailManagers; // number of availability managers for this system
		int AvailStatus; // system availability status
		int StartTime; // cycle on time (in SimTimeSteps)
		int StopTime; // cycle off time (in SimTimeSteps)
		Fstring AvailManagerListName; // name of each availability manager
		FArray1D_Fstring AvailManagerName; // name of each availability manager
		FArray1D_int AvailManagerType; // type of availability manager
		FArray1D_int AvailManagerNum; // index for availability manager
		int ZoneNum; // cycle off time (in SimTimeSteps)
		bool Input;

		// Default Constructor
		DefineZoneCompAvailMgrs() :
			NumAvailManagers( 0 ),
			AvailStatus( 0 ),
			StartTime( 0 ),
			StopTime( 0 ),
			AvailManagerListName( MaxNameLength ),
			AvailManagerName( sFstring( MaxNameLength ) ),
			ZoneNum( 0 ),
			Input( true )
		{}

		// Member Constructor
		DefineZoneCompAvailMgrs(
			int const NumAvailManagers, // number of availability managers for this system
			int const AvailStatus, // system availability status
			int const StartTime, // cycle on time (in SimTimeSteps)
			int const StopTime, // cycle off time (in SimTimeSteps)
			Fstring const & AvailManagerListName, // name of each availability manager
			FArray1_Fstring const & AvailManagerName, // name of each availability manager
			FArray1_int const & AvailManagerType, // type of availability manager
			FArray1_int const & AvailManagerNum, // index for availability manager
			int const ZoneNum, // cycle off time (in SimTimeSteps)
			bool const Input
		) :
			NumAvailManagers( NumAvailManagers ),
			AvailStatus( AvailStatus ),
			StartTime( StartTime ),
			StopTime( StopTime ),
			AvailManagerListName( MaxNameLength, AvailManagerListName ),
			AvailManagerName( AvailManagerName ),
			AvailManagerType( AvailManagerType ),
			AvailManagerNum( AvailManagerNum ),
			ZoneNum( ZoneNum ),
			Input( Input )
		{}

	};

	struct ZoneCompTypeData
	{
		// Members
		FArray1D< DefineZoneCompAvailMgrs > ZoneCompAvailMgrs;
		int TotalNumComp; // total number of components of a zone equip type

		// Default Constructor
		ZoneCompTypeData() :
			TotalNumComp( 0 )
		{}

		// Member Constructor
		ZoneCompTypeData(
			FArray1< DefineZoneCompAvailMgrs > const & ZoneCompAvailMgrs,
			int const TotalNumComp // total number of components of a zone equip type
		) :
			ZoneCompAvailMgrs( ZoneCompAvailMgrs ),
			TotalNumComp( TotalNumComp )
		{}

	};

	struct OptStartDataType
	{
		// Members
		FArray1D_int ActualZoneNum;
		FArray1D< Real64 > OccStartTime;
		FArray1D_bool OptStartFlag;

		// Default Constructor
		OptStartDataType()
		{}

		// Member Constructor
		OptStartDataType(
			FArray1_int const & ActualZoneNum,
			FArray1< Real64 > const & OccStartTime,
			FArray1_bool const & OptStartFlag
		) :
			ActualZoneNum( ActualZoneNum ),
			OccStartTime( OccStartTime ),
			OptStartFlag( OptStartFlag )
		{}

	};

	// Object Data
	extern FArray1D< ZoneCompTypeData > ZoneComp;
	extern OptStartDataType OptStartData; // For optimum start
	extern FArray1D< ComponentSetPtData > CompSetPtEquip;

} // DataHVACGlobals

} // EnergyPlus

#endif
