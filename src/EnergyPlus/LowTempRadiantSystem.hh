#ifndef LowTempRadiantSystem_hh_INCLUDED
#define LowTempRadiantSystem_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace LowTempRadiantSystem {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// System types:
	extern int const HydronicSystem; // Variable flow hydronic radiant system
	extern int const ConstantFlowSystem; // Constant flow, variable (controlled) temperature radiant system
	extern int const ElectricSystem; // Electric resistance radiant heating system
	extern std::string const cHydronicSystem;
	extern std::string const cConstantFlowSystem;
	extern std::string const cElectricSystem;
	// Operating modes:
	extern int const NotOperating; // Parameter for use with OperatingMode variable, set for heating
	extern int const HeatingMode; // Parameter for use with OperatingMode variable, set for heating
	extern int const CoolingMode; // Parameter for use with OperatingMode variable, set for cooling
	// Control types:
	extern int const MATControl; // Controls system using mean air temperature
	extern int const MRTControl; // Controls system using mean radiant temperature
	extern int const OperativeControl; // Controls system using operative temperature
	extern int const ODBControl; // Controls system using outside air dry-bulb temperature
	extern int const OWBControl; // Controls system using outside air wet-bulb temperature
	// Condensation control types:
	extern int const CondCtrlNone; // Condensation control--none, so system never shuts down
	extern int const CondCtrlSimpleOff; // Condensation control--simple off, system shuts off when condensation predicted
	extern int const CondCtrlVariedOff; // Condensation control--variable off, system modulates to keep running if possible
	// Number of Circuits per Surface Calculation Method
	extern int const OneCircuit; // there is 1 circuit per surface
	extern int const CalculateFromLength; // The number of circuits is TubeLength*SurfaceFlowFrac / CircuitLength
	extern std::string const OnePerSurf;
	extern std::string const CalcFromLength;
	// Limit temperatures to indicate that a system cannot heat or cannot cool
	extern Real64 LowTempHeating; // Used to indicate that a user does not have a heating control temperature
	extern Real64 HighTempCooling; // Used to indicate that a user does not have a cooling control temperature

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	// Standard, run-of-the-mill variables...
	extern int NumOfHydrLowTempRadSys; // Number of hydronic low tempererature radiant systems
	extern int NumOfCFloLowTempRadSys; // Number of constant flow (hydronic) low tempererature radiant systems
	extern int NumOfElecLowTempRadSys; // Number of electric low tempererature radiant systems
	extern int CFloCondIterNum; // Number of iterations for a constant flow radiant system--controls variable cond sys ctrl
	extern int TotalNumOfRadSystems; // Total number of low temperature radiant systems
	extern int OperatingMode; // Used to keep track of whether system is in heating or cooling mode
	extern int MaxCloNumOfSurfaces; // Used to set allocate size in CalcClo routine
	extern bool VarOffCond; // Set to true when in cooling for constant flow system + variable off condensation predicted
	extern Real64 LoopReqTemp; // Temperature required at the inlet of the pump (from the loop) to meet control logic
	extern Array1D< Real64 > QRadSysSrcAvg; // Average source over the time step for a particular radiant surface
	extern Array1D< Real64 > ZeroSourceSumHATsurf; // Equal to SumHATsurf for all the walls in a zone with no source
	// Record keeping variables used to calculate QRadSysSrcAvg locally
	extern Array1D< Real64 > LastQRadSysSrc; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastSysTimeElapsed; // Need to keep the last value in case we are still iterating
	extern Array1D< Real64 > LastTimeStepSys; // Need to keep the last value in case we are still iterating
	// Autosizing variables
	extern Array1D_bool MySizeFlagHydr;
	extern Array1D_bool MySizeFlagCFlo;
	extern Array1D_bool MySizeFlagElec;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE LowTempRadiantSystem

	// Types

	struct HydronicRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this radiant system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the surface(s) in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
		Array1D< Real64 > NumCircuits; // Number of fluid circuits in the surface
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this radiant system
		Real64 TubeDiameter; // tube diameter for embedded tubing
		Real64 TubeLength; // tube length embedded in radiant surface
		int ControlType; // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
		bool HeatingSystem; // .TRUE. when the system is able to heat (parameters are valid)
		Real64 WaterVolFlowMaxHeat; // maximum water flow rate for heating, m3/s
		Real64 WaterFlowMaxHeat; // maximum water flow rate for heating, kg/s
		int HotWaterInNode; // hot water inlet node
		int HotWaterOutNode; // hot water outlet node
		Real64 HotThrottlRange; // Throttling range for heating [C]
		std::string HotSetptSched; // Schedule name for the zone setpoint temperature
		int HotSetptSchedPtr; // Schedule index for the zone setpoint temperature
		int HWLoopNum;
		int HWLoopSide;
		int HWBranchNum;
		int HWCompNum;
		Real64 WaterVolFlowMaxCool; // maximum water flow rate for cooling, m3/s
		Real64 WaterFlowMaxCool; // maximum water flow rate for cooling, kg/s
		bool CoolingSystem; // .TRUE. when the system is able to cool (parameters are valid)
		int ColdWaterInNode; // cold water inlet node
		int ColdWaterOutNode; // cold water outlet node
		Real64 ColdThrottlRange; // Throttling range for cooling [C]
		std::string ColdSetptSched; // Schedule name for the zone setpoint temperature
		int ColdSetptSchedPtr; // Schedule index for the zone setpoint temperature
		int CWLoopNum;
		int CWLoopSide;
		int CWBranchNum;
		int CWCompNum;
		int GlycolIndex; // Index to Glycol (Water) Properties
		int CondErrIndex; // Error index for recurring warning messages
		int CondCtrlType; // Condensation control type (initialize to simple off)
		Real64 CondDewPtDeltaT; // Diff between surface temperature and dew point for cond. shut-off
		Real64 CondCausedTimeOff; // Amount of time condensation did or could have turned system off
		bool CondCausedShutDown; // .TRUE. when condensation predicted at surface
		int NumCircCalcMethod; // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
		Real64 CircLength; // Circuit length {m}
		// Other parameters
		bool EMSOverrideOnWaterMdot;
		Real64 EMSWaterMdotOverrideValue;
		// Report data
		Real64 WaterInletTemp; // water inlet temperature
		Real64 WaterOutletTemp; // water outlet temperature
		Real64 WaterMassFlowRate; // water mass flow rate
		Real64 HeatPower; // heating sent to panel in Watts
		Real64 HeatEnergy; // heating sent to panel in Joules
		Real64 CoolPower; // cooling sent to panel in Watts
		Real64 CoolEnergy; // cooling sent to panel in Joules
		int OutRangeHiErrorCount; // recurring errors for crazy results too high fluid temperature
		int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature
		int HeatingCapMethod; // - Method for Low Temp Radiant system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity; // -  Low Temp Radiant system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}
		int CoolingCapMethod; // - Method for Low Temp Radiant system cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedCoolingCapacity)
		Real64 ScaledCoolingCapacity; // -  Low Temp Radiant system scaled maximum cooling capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		HydronicRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			TubeDiameter( 0.0 ),
			TubeLength( 0.0 ),
			ControlType( 0 ),
			HeatingSystem( false ),
			WaterVolFlowMaxHeat( 0.0 ),
			WaterFlowMaxHeat( 0.0 ),
			HotWaterInNode( 0 ),
			HotWaterOutNode( 0 ),
			HotThrottlRange( 0.0 ),
			HotSetptSchedPtr( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			WaterVolFlowMaxCool( 0.0 ),
			WaterFlowMaxCool( 0.0 ),
			CoolingSystem( false ),
			ColdWaterInNode( 0 ),
			ColdWaterOutNode( 0 ),
			ColdThrottlRange( 0.0 ),
			ColdSetptSchedPtr( 0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			GlycolIndex( 0 ),
			CondErrIndex( 0 ),
			CondCtrlType( 1 ),
			CondDewPtDeltaT( 1.0 ),
			CondCausedTimeOff( 0.0 ),
			CondCausedShutDown( false ),
			NumCircCalcMethod( 0 ),
			CircLength( 0.0 ),
			EMSOverrideOnWaterMdot( false ),
			EMSWaterMdotOverrideValue( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			CoolPower( 0.0 ),
			CoolEnergy( 0.0 ),
			OutRangeHiErrorCount( 0 ),
			OutRangeLoErrorCount( 0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 ),
			CoolingCapMethod( 0 ),
			ScaledCoolingCapacity( 0.0 )
		{}

		// Member Constructor
		HydronicRadiantSystemData(
			std::string const & Name, // name of hydronic radiant system
			std::string const & SchedName, // availability schedule
			int const SchedPtr, // index to schedule
			std::string const & ZoneName, // Name of zone the system is serving
			int const ZonePtr, // Point to this zone in the Zone derived type
			std::string const & SurfListName, // Name of surface/surface list that is the radiant system
			int const NumOfSurfaces, // Number of surfaces included in this radiant system (coordinated control)
			Array1_int const & SurfacePtr, // Pointer to the surface(s) in the Surface derived type
			Array1_string const & SurfaceName, // Name of surfaces that are the radiant system (can be one or more)
			Array1< Real64 > const & SurfaceFlowFrac, // Fraction of flow/pipe length for a particular surface
			Array1< Real64 > const & NumCircuits, // Number of fluid circuits in the surface
			Real64 const TotalSurfaceArea, // Total surface area for all surfaces that are part of this radiant system
			Real64 const TubeDiameter, // tube diameter for embedded tubing
			Real64 const TubeLength, // tube length embedded in radiant surface
			int const ControlType, // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
			bool const HeatingSystem, // .TRUE. when the system is able to heat (parameters are valid)
			Real64 const WaterVolFlowMaxHeat, // maximum water flow rate for heating, m3/s
			Real64 const WaterFlowMaxHeat, // maximum water flow rate for heating, kg/s
			int const HotWaterInNode, // hot water inlet node
			int const HotWaterOutNode, // hot water outlet node
			Real64 const HotThrottlRange, // Throttling range for heating [C]
			std::string const & HotSetptSched, // Schedule name for the zone setpoint temperature
			int const HotSetptSchedPtr, // Schedule index for the zone setpoint temperature
			int const HWLoopNum,
			int const HWLoopSide,
			int const HWBranchNum,
			int const HWCompNum,
			Real64 const WaterVolFlowMaxCool, // maximum water flow rate for cooling, m3/s
			Real64 const WaterFlowMaxCool, // maximum water flow rate for cooling, kg/s
			bool const CoolingSystem, // .TRUE. when the system is able to cool (parameters are valid)
			int const ColdWaterInNode, // cold water inlet node
			int const ColdWaterOutNode, // cold water outlet node
			Real64 const ColdThrottlRange, // Throttling range for cooling [C]
			std::string const & ColdSetptSched, // Schedule name for the zone setpoint temperature
			int const ColdSetptSchedPtr, // Schedule index for the zone setpoint temperature
			int const CWLoopNum,
			int const CWLoopSide,
			int const CWBranchNum,
			int const CWCompNum,
			int const GlycolIndex, // Index to Glycol (Water) Properties
			int const CondErrIndex, // Error index for recurring warning messages
			int const CondCtrlType, // Condensation control type (initialize to simple off)
			Real64 const CondDewPtDeltaT, // Diff between surface temperature and dew point for cond. shut-off
			Real64 const CondCausedTimeOff, // Amount of time condensation did or could have turned system off
			bool const CondCausedShutDown, // .TRUE. when condensation predicted at surface
			int const NumCircCalcMethod, // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
			Real64 const CircLength, // Circuit length {m}
			bool const EMSOverrideOnWaterMdot,
			Real64 const EMSWaterMdotOverrideValue,
			Real64 const WaterInletTemp, // water inlet temperature
			Real64 const WaterOutletTemp, // water outlet temperature
			Real64 const WaterMassFlowRate, // water mass flow rate
			Real64 const HeatPower, // heating sent to panel in Watts
			Real64 const HeatEnergy, // heating sent to panel in Joules
			Real64 const CoolPower, // cooling sent to panel in Watts
			Real64 const CoolEnergy, // cooling sent to panel in Joules
			int const OutRangeHiErrorCount, // recurring errors for crazy results too high fluid temperature
			int const OutRangeLoErrorCount, // recurring errors for crazy results too low fluid temperature
			int const HeatingCapMethod, // - Method for Low Temp Radiant system heating capacity scaledsizing calculation (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledHeatingCapacity, // -  Low Temp Radiant system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}
			int const CoolingCapMethod, // - Method for Low Temp Radiant system cooling capacity scaledsizing calculation (CoolingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedCoolingCapacity)
			Real64 const ScaledCoolingCapacity // -  Low Temp Radiant system scaled maximum cooling capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}
		) :
			Name( Name ),
			SchedName( SchedName ),
			SchedPtr( SchedPtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			SurfListName( SurfListName ),
			NumOfSurfaces( NumOfSurfaces ),
			SurfacePtr( SurfacePtr ),
			SurfaceName( SurfaceName ),
			SurfaceFlowFrac( SurfaceFlowFrac ),
			NumCircuits( NumCircuits ),
			TotalSurfaceArea( TotalSurfaceArea ),
			TubeDiameter( TubeDiameter ),
			TubeLength( TubeLength ),
			ControlType( ControlType ),
			HeatingSystem( HeatingSystem ),
			WaterVolFlowMaxHeat( WaterVolFlowMaxHeat ),
			WaterFlowMaxHeat( WaterFlowMaxHeat ),
			HotWaterInNode( HotWaterInNode ),
			HotWaterOutNode( HotWaterOutNode ),
			HotThrottlRange( HotThrottlRange ),
			HotSetptSched( HotSetptSched ),
			HotSetptSchedPtr( HotSetptSchedPtr ),
			HWLoopNum( HWLoopNum ),
			HWLoopSide( HWLoopSide ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			WaterVolFlowMaxCool( WaterVolFlowMaxCool ),
			WaterFlowMaxCool( WaterFlowMaxCool ),
			CoolingSystem( CoolingSystem ),
			ColdWaterInNode( ColdWaterInNode ),
			ColdWaterOutNode( ColdWaterOutNode ),
			ColdThrottlRange( ColdThrottlRange ),
			ColdSetptSched( ColdSetptSched ),
			ColdSetptSchedPtr( ColdSetptSchedPtr ),
			CWLoopNum( CWLoopNum ),
			CWLoopSide( CWLoopSide ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			GlycolIndex( GlycolIndex ),
			CondErrIndex( CondErrIndex ),
			CondCtrlType( CondCtrlType ),
			CondDewPtDeltaT( CondDewPtDeltaT ),
			CondCausedTimeOff( CondCausedTimeOff ),
			CondCausedShutDown( CondCausedShutDown ),
			NumCircCalcMethod( NumCircCalcMethod ),
			CircLength( CircLength ),
			EMSOverrideOnWaterMdot( EMSOverrideOnWaterMdot ),
			EMSWaterMdotOverrideValue( EMSWaterMdotOverrideValue ),
			WaterInletTemp( WaterInletTemp ),
			WaterOutletTemp( WaterOutletTemp ),
			WaterMassFlowRate( WaterMassFlowRate ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			CoolPower( CoolPower ),
			CoolEnergy( CoolEnergy ),
			OutRangeHiErrorCount( OutRangeHiErrorCount ),
			OutRangeLoErrorCount( OutRangeLoErrorCount ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity ),
			CoolingCapMethod( CoolingCapMethod ),
			ScaledCoolingCapacity( ScaledCoolingCapacity )
		{}
	};

	struct ConstantFlowRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this radiant system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the surface(s) in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfaceFlowFrac; // Fraction of flow/pipe length for a particular surface
		Array1D< Real64 > NumCircuits; // Number of fluid circuits in the surface
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this radiant system
		Real64 TubeDiameter; // tube diameter for embedded tubing
		Real64 TubeLength; // tube length embedded in radiant surface
		int ControlType; // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
		Real64 WaterVolFlowMax; // design nominal capacity of constant flow pump (volumetric flow rate)
		Real64 ColdDesignWaterMassFlowRate;
		Real64 HotDesignWaterMassFlowRate;
		Real64 WaterMassFlowRate; // current flow rate through system (calculated)
		std::string VolFlowSched; // schedule of maximum flow at the current time
		int VolFlowSchedPtr; // index to the volumetric flow schedule
		Real64 NomPumpHead; // nominal head of the constant flow pump
		Real64 NomPowerUse; // nominal power use of the constant flow pump
		Real64 MotorEffic; // efficiency of the pump motor
		Real64 PumpEffic; // overall efficiency of the pump (calculated)
		Real64 FracMotorLossToFluid; // amount of heat generated by pump motor that is added to the fluid
		bool HeatingSystem; // .TRUE. when the system is able to heat (parameters are valid)
		int HotWaterInNode; // hot water inlet node
		int HotWaterOutNode; // hot water outlet node
		std::string HotWaterHiTempSched; // Schedule name for the highest water temperature
		int HotWaterHiTempSchedPtr; // Schedule index for the highest water temperature
		std::string HotWaterLoTempSched; // Schedule name for the lowest water temperature
		int HotWaterLoTempSchedPtr; // Schedule index for the lowest water temperature
		std::string HotCtrlHiTempSched; // Schedule name for the highest control temperature
		// (where the lowest water temperature is requested)
		int HotCtrlHiTempSchedPtr; // Schedule index for the highest control temperature
		// (where the lowest water temperature is requested)
		std::string HotCtrlLoTempSched; // Schedule name for the lowest control temperature
		// (where the highest water temperature is requested)
		int HotCtrlLoTempSchedPtr; // Schedule index for the lowest control temperature
		// (where the highest water temperature is requested)
		int HWLoopNum;
		int HWLoopSide;
		int HWBranchNum;
		int HWCompNum;
		bool CoolingSystem; // .TRUE. when the system is able to cool (parameters are valid)
		int ColdWaterInNode; // cold water inlet node
		int ColdWaterOutNode; // cold water outlet node
		std::string ColdWaterHiTempSched; // Schedule name for the highest water temperature
		int ColdWaterHiTempSchedPtr; // Schedule index for the highest water temperature
		std::string ColdWaterLoTempSched; // Schedule name for the lowest water temperature
		int ColdWaterLoTempSchedPtr; // Schedule index for the lowest water temperature
		std::string ColdCtrlHiTempSched; // Schedule name for the highest control temperature
		// (where the lowest water temperature is requested)
		int ColdCtrlHiTempSchedPtr; // Schedule index for the highest control temperature
		// (where the lowest water temperature is requested)
		std::string ColdCtrlLoTempSched; // Schedule name for the lowest control temperature
		// (where the highest water temperature is requested)
		int ColdCtrlLoTempSchedPtr; // Schedule index for the lowest control temperature
		// (where the highest water temperature is requested)
		int CWLoopNum;
		int CWLoopSide;
		int CWBranchNum;
		int CWCompNum;
		int GlycolIndex; // Index to Glycol (Water) Properties
		int CondErrIndex; // Error index for warning messages
		int CondCtrlType; // Condensation control type (initialize to simple off)
		Real64 CondDewPtDeltaT; // Diff between surface temperature and dew point for cond. shut-off
		Real64 CondCausedTimeOff; // Amount of time condensation did or could have turned system off
		bool CondCausedShutDown; // .TRUE. when condensation predicted at surface
		int NumCircCalcMethod; // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
		Real64 CircLength; // Circuit length {m}
		// Other parameters
		bool EMSOverrideOnWaterMdot;
		Real64 EMSWaterMdotOverrideValue;
		// Report data
		Real64 WaterInletTemp; // water inlet temperature
		Real64 WaterOutletTemp; // water outlet temperature
		Real64 WaterInjectionRate; // water injection mass flow rate from main loop
		Real64 WaterRecircRate; // water recirculation rate (outlet from radiant system recirculated)
		Real64 HeatPower; // heating sent to panel in Watts
		Real64 HeatEnergy; // heating sent to panel in Joules
		Real64 CoolPower; // cooling sent to panel in Watts
		Real64 CoolEnergy; // cooling sent to panel in Joules
		Real64 PumpPower; // pump power in Watts
		Real64 PumpEnergy; // pump energy consumption in Joules
		Real64 PumpMassFlowRate; // mass flow rate through the radiant system in kg/sec
		Real64 PumpHeattoFluid; // heat transfer rate from pump motor to fluid in Watts
		Real64 PumpHeattoFluidEnergy; // Pump Energy dissipated into fluid stream in Joules
		Real64 PumpInletTemp; // inlet temperature of pump (inlet temperature from loop)
		int OutRangeHiErrorCount; // recurring errors for crazy results too high fluid temperature
		int OutRangeLoErrorCount; // recurring errors for crazy results too low fluid temperature

		// Default Constructor
		ConstantFlowRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			TubeDiameter( 0.0 ),
			TubeLength( 0.0 ),
			ControlType( 0 ),
			WaterVolFlowMax( 0.0 ),
			ColdDesignWaterMassFlowRate( 0.0 ),
			HotDesignWaterMassFlowRate( 0.0 ),
			WaterMassFlowRate( 0.0 ),
			VolFlowSchedPtr( 0 ),
			NomPumpHead( 0.0 ),
			NomPowerUse( 0.0 ),
			MotorEffic( 0.0 ),
			PumpEffic( 0.0 ),
			FracMotorLossToFluid( 0.0 ),
			HeatingSystem( false ),
			HotWaterInNode( 0 ),
			HotWaterOutNode( 0 ),
			HotWaterHiTempSchedPtr( 0 ),
			HotWaterLoTempSchedPtr( 0 ),
			HotCtrlHiTempSchedPtr( 0 ),
			HotCtrlLoTempSchedPtr( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			CoolingSystem( false ),
			ColdWaterInNode( 0 ),
			ColdWaterOutNode( 0 ),
			ColdWaterHiTempSchedPtr( 0 ),
			ColdWaterLoTempSchedPtr( 0 ),
			ColdCtrlHiTempSchedPtr( 0 ),
			ColdCtrlLoTempSchedPtr( 0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			GlycolIndex( 0 ),
			CondErrIndex( 0 ),
			CondCtrlType( 1 ),
			CondDewPtDeltaT( 1.0 ),
			CondCausedTimeOff( 0.0 ),
			CondCausedShutDown( false ),
			NumCircCalcMethod( 0 ),
			CircLength( 0.0 ),
			EMSOverrideOnWaterMdot( false ),
			EMSWaterMdotOverrideValue( 0.0 ),
			WaterInletTemp( 0.0 ),
			WaterOutletTemp( 0.0 ),
			WaterInjectionRate( 0.0 ),
			WaterRecircRate( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			CoolPower( 0.0 ),
			CoolEnergy( 0.0 ),
			PumpPower( 0.0 ),
			PumpEnergy( 0.0 ),
			PumpMassFlowRate( 0.0 ),
			PumpHeattoFluid( 0.0 ),
			PumpHeattoFluidEnergy( 0.0 ),
			PumpInletTemp( 0.0 ),
			OutRangeHiErrorCount( 0 ),
			OutRangeLoErrorCount( 0 )
		{}

		// Member Constructor
		ConstantFlowRadiantSystemData(
			std::string const & Name, // name of hydronic radiant system
			std::string const & SchedName, // availability schedule
			int const SchedPtr, // index to schedule
			std::string const & ZoneName, // Name of zone the system is serving
			int const ZonePtr, // Point to this zone in the Zone derived type
			std::string const & SurfListName, // Name of surface/surface list that is the radiant system
			int const NumOfSurfaces, // Number of surfaces included in this radiant system (coordinated control)
			Array1_int const & SurfacePtr, // Pointer to the surface(s) in the Surface derived type
			Array1_string const & SurfaceName, // Name of surfaces that are the radiant system (can be one or more)
			Array1< Real64 > const & SurfaceFlowFrac, // Fraction of flow/pipe length for a particular surface
			Array1< Real64 > const & NumCircuits, // Number of fluid circuits in the surface
			Real64 const TotalSurfaceArea, // Total surface area for all surfaces that are part of this radiant system
			Real64 const TubeDiameter, // tube diameter for embedded tubing
			Real64 const TubeLength, // tube length embedded in radiant surface
			int const ControlType, // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
			Real64 const WaterVolFlowMax, // design nominal capacity of constant flow pump (volumetric flow rate)
			Real64 const ColdDesignWaterMassFlowRate,
			Real64 const HotDesignWaterMassFlowRate,
			Real64 const WaterMassFlowRate, // current flow rate through system (calculated)
			std::string const & VolFlowSched, // schedule of maximum flow at the current time
			int const VolFlowSchedPtr, // index to the volumetric flow schedule
			Real64 const NomPumpHead, // nominal head of the constant flow pump
			Real64 const NomPowerUse, // nominal power use of the constant flow pump
			Real64 const MotorEffic, // efficiency of the pump motor
			Real64 const PumpEffic, // overall efficiency of the pump (calculated)
			Real64 const FracMotorLossToFluid, // amount of heat generated by pump motor that is added to the fluid
			bool const HeatingSystem, // .TRUE. when the system is able to heat (parameters are valid)
			int const HotWaterInNode, // hot water inlet node
			int const HotWaterOutNode, // hot water outlet node
			std::string const & HotWaterHiTempSched, // Schedule name for the highest water temperature
			int const HotWaterHiTempSchedPtr, // Schedule index for the highest water temperature
			std::string const & HotWaterLoTempSched, // Schedule name for the lowest water temperature
			int const HotWaterLoTempSchedPtr, // Schedule index for the lowest water temperature
			std::string const & HotCtrlHiTempSched, // Schedule name for the highest control temperature
			int const HotCtrlHiTempSchedPtr, // Schedule index for the highest control temperature
			std::string const & HotCtrlLoTempSched, // Schedule name for the lowest control temperature
			int const HotCtrlLoTempSchedPtr, // Schedule index for the lowest control temperature
			int const HWLoopNum,
			int const HWLoopSide,
			int const HWBranchNum,
			int const HWCompNum,
			bool const CoolingSystem, // .TRUE. when the system is able to cool (parameters are valid)
			int const ColdWaterInNode, // cold water inlet node
			int const ColdWaterOutNode, // cold water outlet node
			std::string const & ColdWaterHiTempSched, // Schedule name for the highest water temperature
			int const ColdWaterHiTempSchedPtr, // Schedule index for the highest water temperature
			std::string const & ColdWaterLoTempSched, // Schedule name for the lowest water temperature
			int const ColdWaterLoTempSchedPtr, // Schedule index for the lowest water temperature
			std::string const & ColdCtrlHiTempSched, // Schedule name for the highest control temperature
			int const ColdCtrlHiTempSchedPtr, // Schedule index for the highest control temperature
			std::string const & ColdCtrlLoTempSched, // Schedule name for the lowest control temperature
			int const ColdCtrlLoTempSchedPtr, // Schedule index for the lowest control temperature
			int const CWLoopNum,
			int const CWLoopSide,
			int const CWBranchNum,
			int const CWCompNum,
			int const GlycolIndex, // Index to Glycol (Water) Properties
			int const CondErrIndex, // Error index for warning messages
			int const CondCtrlType, // Condensation control type (initialize to simple off)
			Real64 const CondDewPtDeltaT, // Diff between surface temperature and dew point for cond. shut-off
			Real64 const CondCausedTimeOff, // Amount of time condensation did or could have turned system off
			bool const CondCausedShutDown, // .TRUE. when condensation predicted at surface
			int const NumCircCalcMethod, // Calculation method for number of circuits per surface; 1=1 per surface, 2=use cicuit length
			Real64 const CircLength, // Circuit length {m}
			bool const EMSOverrideOnWaterMdot,
			Real64 const EMSWaterMdotOverrideValue,
			Real64 const WaterInletTemp, // water inlet temperature
			Real64 const WaterOutletTemp, // water outlet temperature
			Real64 const WaterInjectionRate, // water injection mass flow rate from main loop
			Real64 const WaterRecircRate, // water recirculation rate (outlet from radiant system recirculated)
			Real64 const HeatPower, // heating sent to panel in Watts
			Real64 const HeatEnergy, // heating sent to panel in Joules
			Real64 const CoolPower, // cooling sent to panel in Watts
			Real64 const CoolEnergy, // cooling sent to panel in Joules
			Real64 const PumpPower, // pump power in Watts
			Real64 const PumpEnergy, // pump energy consumption in Joules
			Real64 const PumpMassFlowRate, // mass flow rate through the radiant system in kg/sec
			Real64 const PumpHeattoFluid, // heat transfer rate from pump motor to fluid in Watts
			Real64 const PumpHeattoFluidEnergy, // Pump Energy dissipated into fluid stream in Joules
			Real64 const PumpInletTemp, // inlet temperature of pump (inlet temperature from loop)
			int const OutRangeHiErrorCount, // recurring errors for crazy results too high fluid temperature
			int const OutRangeLoErrorCount // recurring errors for crazy results too low fluid temperature
		) :
			Name( Name ),
			SchedName( SchedName ),
			SchedPtr( SchedPtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			SurfListName( SurfListName ),
			NumOfSurfaces( NumOfSurfaces ),
			SurfacePtr( SurfacePtr ),
			SurfaceName( SurfaceName ),
			SurfaceFlowFrac( SurfaceFlowFrac ),
			NumCircuits( NumCircuits ),
			TotalSurfaceArea( TotalSurfaceArea ),
			TubeDiameter( TubeDiameter ),
			TubeLength( TubeLength ),
			ControlType( ControlType ),
			WaterVolFlowMax( WaterVolFlowMax ),
			ColdDesignWaterMassFlowRate( ColdDesignWaterMassFlowRate ),
			HotDesignWaterMassFlowRate( HotDesignWaterMassFlowRate ),
			WaterMassFlowRate( WaterMassFlowRate ),
			VolFlowSched( VolFlowSched ),
			VolFlowSchedPtr( VolFlowSchedPtr ),
			NomPumpHead( NomPumpHead ),
			NomPowerUse( NomPowerUse ),
			MotorEffic( MotorEffic ),
			PumpEffic( PumpEffic ),
			FracMotorLossToFluid( FracMotorLossToFluid ),
			HeatingSystem( HeatingSystem ),
			HotWaterInNode( HotWaterInNode ),
			HotWaterOutNode( HotWaterOutNode ),
			HotWaterHiTempSched( HotWaterHiTempSched ),
			HotWaterHiTempSchedPtr( HotWaterHiTempSchedPtr ),
			HotWaterLoTempSched( HotWaterLoTempSched ),
			HotWaterLoTempSchedPtr( HotWaterLoTempSchedPtr ),
			HotCtrlHiTempSched( HotCtrlHiTempSched ),
			HotCtrlHiTempSchedPtr( HotCtrlHiTempSchedPtr ),
			HotCtrlLoTempSched( HotCtrlLoTempSched ),
			HotCtrlLoTempSchedPtr( HotCtrlLoTempSchedPtr ),
			HWLoopNum( HWLoopNum ),
			HWLoopSide( HWLoopSide ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			CoolingSystem( CoolingSystem ),
			ColdWaterInNode( ColdWaterInNode ),
			ColdWaterOutNode( ColdWaterOutNode ),
			ColdWaterHiTempSched( ColdWaterHiTempSched ),
			ColdWaterHiTempSchedPtr( ColdWaterHiTempSchedPtr ),
			ColdWaterLoTempSched( ColdWaterLoTempSched ),
			ColdWaterLoTempSchedPtr( ColdWaterLoTempSchedPtr ),
			ColdCtrlHiTempSched( ColdCtrlHiTempSched ),
			ColdCtrlHiTempSchedPtr( ColdCtrlHiTempSchedPtr ),
			ColdCtrlLoTempSched( ColdCtrlLoTempSched ),
			ColdCtrlLoTempSchedPtr( ColdCtrlLoTempSchedPtr ),
			CWLoopNum( CWLoopNum ),
			CWLoopSide( CWLoopSide ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			GlycolIndex( GlycolIndex ),
			CondErrIndex( CondErrIndex ),
			CondCtrlType( CondCtrlType ),
			CondDewPtDeltaT( CondDewPtDeltaT ),
			CondCausedTimeOff( CondCausedTimeOff ),
			CondCausedShutDown( CondCausedShutDown ),
			NumCircCalcMethod( NumCircCalcMethod ),
			CircLength( CircLength ),
			EMSOverrideOnWaterMdot( EMSOverrideOnWaterMdot ),
			EMSWaterMdotOverrideValue( EMSWaterMdotOverrideValue ),
			WaterInletTemp( WaterInletTemp ),
			WaterOutletTemp( WaterOutletTemp ),
			WaterInjectionRate( WaterInjectionRate ),
			WaterRecircRate( WaterRecircRate ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			CoolPower( CoolPower ),
			CoolEnergy( CoolEnergy ),
			PumpPower( PumpPower ),
			PumpEnergy( PumpEnergy ),
			PumpMassFlowRate( PumpMassFlowRate ),
			PumpHeattoFluid( PumpHeattoFluid ),
			PumpHeattoFluidEnergy( PumpHeattoFluidEnergy ),
			PumpInletTemp( PumpInletTemp ),
			OutRangeHiErrorCount( OutRangeHiErrorCount ),
			OutRangeLoErrorCount( OutRangeLoErrorCount )
		{}

	};

	struct ElectricRadiantSystemData
	{
		// Members
		// Input data
		std::string Name; // name of hydronic radiant system
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		std::string SurfListName; // Name of surface/surface list that is the radiant system
		int NumOfSurfaces; // Number of surfaces included in this radiant system (coordinated control)
		Array1D_int SurfacePtr; // Pointer to the surface(s) in the Surface derived type
		Array1D_string SurfaceName; // Name of surfaces that are the radiant system (can be one or more)
		Array1D< Real64 > SurfacePowerFrac; // Fraction of total power input to surface
		Real64 TotalSurfaceArea; // Total surface area for all surfaces that are part of this radiant system
		Real64 MaxElecPower; // Maximum electric power that can be supplied to surface, Watts
		int ControlType; // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
		Real64 ThrottlRange; // Throttling range for heating [C]
		std::string SetptSched; // Schedule name for the zone setpoint temperature
		int SetptSchedPtr; // Schedule index for the zone setpoint temperature
		// Other parameters
		// Report data
		Real64 ElecPower; // heating sent to panel in Watts
		Real64 ElecEnergy; // heating sent to panel in Joules
		Real64 HeatPower; // heating sent to panel in Watts (same as ElecPower)
		Real64 HeatEnergy; // heating sent to panel in Joules (same as ElecEnergy)
		int HeatingCapMethod;    // - Method for Low Temp Radiant system heating capacity scaledsizing calculation
		//- (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
		Real64 ScaledHeatingCapacity;   // -  Low Temp Radiant system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}

		// Default Constructor
		ElectricRadiantSystemData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			NumOfSurfaces( 0 ),
			TotalSurfaceArea( 0.0 ),
			MaxElecPower( 0.0 ),
			ControlType( 0 ),
			ThrottlRange( 0.0 ),
			SetptSchedPtr( 0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			HeatingCapMethod( 0 ),
			ScaledHeatingCapacity( 0.0 )
		{}

		// Member Constructor
		ElectricRadiantSystemData(
			std::string const & Name, // name of hydronic radiant system
			std::string const & SchedName, // availability schedule
			int const SchedPtr, // index to schedule
			std::string const & ZoneName, // Name of zone the system is serving
			int const ZonePtr, // Point to this zone in the Zone derived type
			std::string const & SurfListName, // Name of surface/surface list that is the radiant system
			int const NumOfSurfaces, // Number of surfaces included in this radiant system (coordinated control)
			Array1_int const & SurfacePtr, // Pointer to the surface(s) in the Surface derived type
			Array1_string const & SurfaceName, // Name of surfaces that are the radiant system (can be one or more)
			Array1< Real64 > const & SurfacePowerFrac, // Fraction of total power input to surface
			Real64 const TotalSurfaceArea, // Total surface area for all surfaces that are part of this radiant system
			Real64 const MaxElecPower, // Maximum electric power that can be supplied to surface, Watts
			int const ControlType, // Control type for the system (MAT, MRT, Op temp, ODB, OWB)
			Real64 const ThrottlRange, // Throttling range for heating [C]
			std::string const & SetptSched, // Schedule name for the zone setpoint temperature
			int const SetptSchedPtr, // Schedule index for the zone setpoint temperature
			Real64 const ElecPower, // heating sent to panel in Watts
			Real64 const ElecEnergy, // heating sent to panel in Joules
			Real64 const HeatPower, // heating sent to panel in Watts (same as ElecPower)
			Real64 const HeatEnergy, // heating sent to panel in Joules (same as ElecEnergy)
			int const HeatingCapMethod,     // - Method for Low Temp Radiant system heating capacity scalable sizing calculation
			//- (HeatingDesignCapacity, CapacityPerFloorArea, FracOfAutosizedHeatingCapacity)
			Real64 const ScaledHeatingCapacity   // - Low Temp Radiant system scaled maximum heating capacity {W} or scalable variable of zone HVAC equipment, {-}, or {W/m2}
		) :
			Name( Name ),
			SchedName( SchedName ),
			SchedPtr( SchedPtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			SurfListName( SurfListName ),
			NumOfSurfaces( NumOfSurfaces ),
			SurfacePtr( SurfacePtr ),
			SurfaceName( SurfaceName ),
			SurfacePowerFrac( SurfacePowerFrac ),
			TotalSurfaceArea( TotalSurfaceArea ),
			MaxElecPower( MaxElecPower ),
			ControlType( ControlType ),
			ThrottlRange( ThrottlRange ),
			SetptSched( SetptSched ),
			SetptSchedPtr( SetptSchedPtr ),
			ElecPower( ElecPower ),
			ElecEnergy( ElecEnergy ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			HeatingCapMethod( HeatingCapMethod ),
			ScaledHeatingCapacity( ScaledHeatingCapacity )
		{}

	};

	struct RadSysTypeData
	{
		// Members
		// This type used to track different components/types for efficiency
		std::string Name; // name of radiant system
		int SystemType; // Type of System (see System Types in Parameters)
		int CompIndex; // Index in specific system types

		// Default Constructor
		RadSysTypeData() :
			SystemType( 0 ),
			CompIndex( 0 )
		{}

		// Member Constructor
		RadSysTypeData(
			std::string const & Name, // name of radiant system
			int const SystemType, // Type of System (see System Types in Parameters)
			int const CompIndex // Index in specific system types
		) :
			Name( Name ),
			SystemType( SystemType ),
			CompIndex( CompIndex )
		{}

	};

	struct ElecRadSysNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		ElecRadSysNumericFieldData()
		{}

		// Member Constructor
		ElecRadSysNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	struct HydronicRadiantSysNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		HydronicRadiantSysNumericFieldData()
		{}

		// Member Constructor
		HydronicRadiantSysNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames(FieldNames)
		{}
	};

	// Object Data
	extern Array1D< HydronicRadiantSystemData > HydrRadSys;
	extern Array1D< ConstantFlowRadiantSystemData > CFloRadSys;
	extern Array1D< ElectricRadiantSystemData > ElecRadSys;
	extern Array1D< RadSysTypeData > RadSysTypes;
	extern Array1D< ElecRadSysNumericFieldData > ElecRadSysNumericFields;
	extern Array1D< HydronicRadiantSysNumericFieldData > HydronicRadiantSysNumericFields;

	// Functions

	void
	SimLowTempRadiantSystem(
		std::string const & CompName, // name of the low temperature radiant system
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & LoadMet, // load met by the radiant system, in Watts
		int & CompIndex
	);

	void
	GetLowTempRadiantSystem();

	void
	InitLowTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	void
	SizeLowTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	void
	CalcLowTempHydrRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	CalcLowTempHydrRadSysComps(
		int const RadSysNum, // Index for the low temperature radiant system under consideration
		Real64 & LoadMet // Load met by the low temperature radiant system, in Watts
	);

	void
	CalcLowTempCFloRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	CalcLowTempCFloRadSysComps(
		int const RadSysNum, // Index for the low temperature radiant system under consideration
		int const MainLoopNodeIn, // Node number on main loop of the inlet node to the radiant system
		bool const Iteration, // FALSE for the regular solution, TRUE when we had to loop back
		Real64 & LoadMet // Load met by the low temperature radiant system, in Watts
	);

	void
	CalcLowTempElecRadiantSystem(
		int const RadSysNum, // name of the low temperature radiant system
		Real64 & LoadMet // load met by the radiant system, in Watts
	);

	void
	UpdateLowTempRadiantSystem(
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	void
	CheckForOutOfRangeTempResult(
		int const SystemType,
		int const RadSysNum,
		Real64 const outletTemp,
		Real64 const inletTemp,
		Real64 const mdot
	);

	Real64
	CalcRadSysHXEffectTerm(
		int const RadSysNum, // Index number of radiant system under consideration !unused1208
		int const SystemType, // Type of radiant system: hydronic, constant flow, or electric
		Real64 const Temperature, // Temperature of water entering the radiant system, in C
		Real64 const WaterMassFlow, // Mass flow rate of water in the radiant system, in kg/s
		Real64 const FlowFraction, // Mass flow rate fraction for this surface in the radiant system
		Real64 const NumCircs, // Number of fluid circuits in this surface
		Real64 const TubeLength, // Length of tubing in the radiant system, in m
		Real64 const TubeDiameter, // Inside diameter of the tubing in the radiant system, in m
		int & GlycolIndex // Index for the fluid used in this radiant system
	);

	void
	UpdateRadSysSourceValAvg( bool & LowTempRadSysOn ); // .TRUE. if the radiant system has run this zone time step

	Real64
	SumHATsurf( int const ZoneNum ); // Zone number

	void
	ReportLowTempRadiantSystem(
		int const RadSysNum, // Index for the low temperature radiant system under consideration within the derived types
		int const SystemType // Type of radiant system: hydronic, constant flow, or electric
	);

	//     NOTICE

	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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

} // LowTempRadiantSystem

} // EnergyPlus

#endif
