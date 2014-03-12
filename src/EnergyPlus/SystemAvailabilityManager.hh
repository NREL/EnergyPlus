#ifndef SystemAvailabilityManager_hh_INCLUDED
#define SystemAvailabilityManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SystemAvailabilityManager {

	// Using/Aliasing
	using DataGlobals::MaxNameLength;

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const MaxDayTypes;
	extern int const StayOff;
	extern int const CycleOnAny;
	extern int const CycleOnControlZone;
	extern int const ZoneFansOnly;

	// Optimum start parameter definations
	extern int const ControlZone;
	extern int const MaximumOfZoneList;

	extern int const ConstantTemperatureGradient;
	extern int const AdaptiveTemperatureGradient;
	extern int const AdaptiveASHRAE;
	extern int const ConstantStartTime;

	// Hybrid Ventilation parameters
	extern int const HybridVentMode_No; // No hybrid ventilation control
	extern int const HybridVentMode_Temp; // Temperature control
	extern int const HybridVentMode_Enth; // Enthalpy control
	extern int const HybridVentMode_DewPoint; // Dew point control
	extern int const HybridVentMode_OA; // Outdoor air control

	extern int const HybridVentCtrl_NoAction; // No hybrid ventilation control
	extern int const HybridVentCtrl_Open; // Open windows or doors
	extern int const HybridVentCtrl_Close; // Close windows or doors

	extern int const NumValidSysAvailManagerTypes;
	extern FArray1D_Fstring const cValidSysAvailManagerTypes;
	extern int const SysAvailMgr_Scheduled;
	extern int const SysAvailMgr_ScheduledOn;
	extern int const SysAvailMgr_ScheduledOff;
	extern int const SysAvailMgr_NightCycle;
	extern int const SysAvailMgr_DiffThermo;
	extern int const SysAvailMgr_HiTempTOff;
	extern int const SysAvailMgr_HiTempTOn;
	extern int const SysAvailMgr_LoTempTOff;
	extern int const SysAvailMgr_LoTempTOn;
	extern int const SysAvailMgr_NightVent;
	extern int const SysAvailMgr_HybridVent;

	extern int const SysAvailMgr_OptimumStart;
	extern FArray1D_int const ValidSysAvailManagerTypes;
	// DERIVED TYPE DEFINITIONS

	//Not used yet

	// MODULE VARIABLE DECLARATIONS

	extern int NumSchedSysAvailMgrs;
	extern int NumSchedOnSysAvailMgrs;
	extern int NumSchedOffSysAvailMgrs;
	extern int NumNCycSysAvailMgrs;
	extern int NumDiffTSysAvailMgrs;
	extern int NumHiTurnOffSysAvailMgrs;
	extern int NumHiTurnOnSysAvailMgrs;
	extern int NumLoTurnOffSysAvailMgrs;
	extern int NumLoTurnOnSysAvailMgrs;
	extern int NumNVentSysAvailMgrs;
	extern int NumAvailManagerLists;
	extern bool GetAvailListsInput;
	extern bool GetAvailMgrInputFlag; // First time, input is "gotten"
	extern bool GetHybridInputFlag; // Flag set to make sure you get input once
	extern int NumOptStartSysAvailMgrs;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// Types

	struct DefineSchedSysAvailManager // Derived type for Scheduled Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int SchedPtr; // Schedule pointer
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineSchedSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			SchedPtr( 0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineSchedSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const SchedPtr, // Schedule pointer
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			SchedPtr( SchedPtr ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineSchedOnSysAvailManager // Derived type for Scheduled On Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int SchedPtr; // Schedule pointer
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineSchedOnSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			SchedPtr( 0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineSchedOnSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const SchedPtr, // Schedule pointer
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			SchedPtr( SchedPtr ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineSchedOffSysAvailManager // Derived type for Scheduled Off Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int SchedPtr; // Schedule pointer
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineSchedOffSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			SchedPtr( 0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineSchedOffSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const SchedPtr, // Schedule pointer
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			SchedPtr( SchedPtr ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineNightCycSysAvailManager // Derived type for Night Cycle Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int SchedPtr; // Applicability schedule pointer
		Fstring FanSched; // Fan schedule name
		int FanSchedPtr; // Fan schedule pointer
		int CtrlType; // type of control: Stay Off, Cycle On Any,
		//   Cycle On Control Zone, or Cycle On Any - Zone Fans Only
		Real64 TempTolRange; // range in degrees C of thermostat tolerance
		int CyclingTimeSteps; // period (in Loads time steps) system will cycle on.
		Fstring CtrlZoneName; // Name of the control zone
		int ZoneNum; // zone number of control zone
		int ControlledZoneNum; // controlled zone number of control zone
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineNightCycSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			SchedPtr( 0 ),
			FanSched( MaxNameLength ),
			FanSchedPtr( 0 ),
			CtrlType( 0 ),
			TempTolRange( 1.0 ),
			CyclingTimeSteps( 1 ),
			CtrlZoneName( MaxNameLength ),
			ZoneNum( 0 ),
			ControlledZoneNum( 0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineNightCycSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const SchedPtr, // Applicability schedule pointer
			Fstring const & FanSched, // Fan schedule name
			int const FanSchedPtr, // Fan schedule pointer
			int const CtrlType, // type of control: Stay Off, Cycle On Any,
			Real64 const TempTolRange, // range in degrees C of thermostat tolerance
			int const CyclingTimeSteps, // period (in Loads time steps) system will cycle on.
			Fstring const & CtrlZoneName, // Name of the control zone
			int const ZoneNum, // zone number of control zone
			int const ControlledZoneNum, // controlled zone number of control zone
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			SchedPtr( SchedPtr ),
			FanSched( MaxNameLength, FanSched ),
			FanSchedPtr( FanSchedPtr ),
			CtrlType( CtrlType ),
			TempTolRange( TempTolRange ),
			CyclingTimeSteps( CyclingTimeSteps ),
			CtrlZoneName( MaxNameLength, CtrlZoneName ),
			ZoneNum( ZoneNum ),
			ControlledZoneNum( ControlledZoneNum ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineOptStartSysAvailManager // Derived type for Optimal Start Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int SchedPtr; // Applicability schedule pointer
		Fstring FanSched; // Fan schedule name
		int FanSchedPtr; // Fan schedule pointer
		int CtrlType; // Type of control: Stay Off, ControlZone, MaximumofZoneList
		Fstring CtrlZoneName; // Name of the control zone
		int ZoneNum; // zone number of control zone
		int ControlledZoneNum; // controlled zone number of control zone
		Fstring ZoneListName; // Zone List name
		int NumOfZones; // Number of zones in the list
		FArray1D_int ZonePtrs; // Pointers to zones in the list
		Real64 MaxOptStartTime; // Maximum value of start time in hours
		int CtrlAlgType; // Control algorithm: ConstantTemperatureGradient,
		// AdaptiveTemperatureGradient, AdaptiveASHRAE, ConstantStartTime
		Real64 ConstTGradCool; // Constant temperature gradient in cooling mode, unit: degC per hour
		Real64 ConstTGradHeat; // Constant temperature gradient in heating mode, unit: degC per hour
		Real64 InitTGradCool; // Initial value for temperature gradient in cooling mode, unit: degC per hour
		Real64 InitTGradHeat; // Initial value for temperature gradient in heating mode, unit: degC per hour
		Real64 AdaptiveTGradCool; // Calculated adaptive temperature gradient in cooling mode, unit: degC per hour
		Real64 AdaptiveTGradHeat; // Calculated adaptive temperature gradient in heating mode, unit: degC per hour
		Real64 ConstStartTime; // Constant start time in hours
		int NumPreDays; // Number of previous days for adaptive control
		int AvailStatus; // reports status of availability manager
		Real64 NumHoursBeforeOccupancy;

		// Default Constructor
		DefineOptStartSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			SchedPtr( 0 ),
			FanSched( MaxNameLength ),
			FanSchedPtr( 0 ),
			CtrlType( 0 ),
			CtrlZoneName( MaxNameLength ),
			ZoneNum( 0 ),
			ControlledZoneNum( 0 ),
			ZoneListName( MaxNameLength ),
			NumOfZones( 0 ),
			MaxOptStartTime( 6.0 ),
			CtrlAlgType( 0 ),
			ConstTGradCool( 1.0 ),
			ConstTGradHeat( 1.0 ),
			InitTGradCool( 1.0 ),
			InitTGradHeat( 1.0 ),
			AdaptiveTGradCool( 1.0 ),
			AdaptiveTGradHeat( 1.0 ),
			ConstStartTime( 2.0 ),
			NumPreDays( 1 ),
			AvailStatus( 0 ),
			NumHoursBeforeOccupancy( 0.0 )
		{}

		// Member Constructor
		DefineOptStartSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const SchedPtr, // Applicability schedule pointer
			Fstring const & FanSched, // Fan schedule name
			int const FanSchedPtr, // Fan schedule pointer
			int const CtrlType, // Type of control: Stay Off, ControlZone, MaximumofZoneList
			Fstring const & CtrlZoneName, // Name of the control zone
			int const ZoneNum, // zone number of control zone
			int const ControlledZoneNum, // controlled zone number of control zone
			Fstring const & ZoneListName, // Zone List name
			int const NumOfZones, // Number of zones in the list
			FArray1_int const & ZonePtrs, // Pointers to zones in the list
			Real64 const MaxOptStartTime, // Maximum value of start time in hours
			int const CtrlAlgType, // Control algorithm: ConstantTemperatureGradient,
			Real64 const ConstTGradCool, // Constant temperature gradient in cooling mode, unit: degC per hour
			Real64 const ConstTGradHeat, // Constant temperature gradient in heating mode, unit: degC per hour
			Real64 const InitTGradCool, // Initial value for temperature gradient in cooling mode, unit: degC per hour
			Real64 const InitTGradHeat, // Initial value for temperature gradient in heating mode, unit: degC per hour
			Real64 const AdaptiveTGradCool, // Calculated adaptive temperature gradient in cooling mode, unit: degC per hour
			Real64 const AdaptiveTGradHeat, // Calculated adaptive temperature gradient in heating mode, unit: degC per hour
			Real64 const ConstStartTime, // Constant start time in hours
			int const NumPreDays, // Number of previous days for adaptive control
			int const AvailStatus, // reports status of availability manager
			Real64 const NumHoursBeforeOccupancy
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			SchedPtr( SchedPtr ),
			FanSched( MaxNameLength, FanSched ),
			FanSchedPtr( FanSchedPtr ),
			CtrlType( CtrlType ),
			CtrlZoneName( MaxNameLength, CtrlZoneName ),
			ZoneNum( ZoneNum ),
			ControlledZoneNum( ControlledZoneNum ),
			ZoneListName( MaxNameLength, ZoneListName ),
			NumOfZones( NumOfZones ),
			ZonePtrs( ZonePtrs ),
			MaxOptStartTime( MaxOptStartTime ),
			CtrlAlgType( CtrlAlgType ),
			ConstTGradCool( ConstTGradCool ),
			ConstTGradHeat( ConstTGradHeat ),
			InitTGradCool( InitTGradCool ),
			InitTGradHeat( InitTGradHeat ),
			AdaptiveTGradCool( AdaptiveTGradCool ),
			AdaptiveTGradHeat( AdaptiveTGradHeat ),
			ConstStartTime( ConstStartTime ),
			NumPreDays( NumPreDays ),
			AvailStatus( AvailStatus ),
			NumHoursBeforeOccupancy( NumHoursBeforeOccupancy )
		{}

	};

	struct DefineASHRAEAdaptiveOptimumStartCoeffs // Derived type for Differential Thermostat Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the object
		Real64 Coeff1; // 1st Coefficient of the equation
		Real64 Coeff2; // 2nd Coefficient of the equation
		Real64 Coeff3; // 3rd Coefficient of the equation
		Real64 Coeff4; // 4th Coefficient of the equation

		// Default Constructor
		DefineASHRAEAdaptiveOptimumStartCoeffs() :
			Name( MaxNameLength ),
			Coeff1( 0.0 ),
			Coeff2( 0.0 ),
			Coeff3( 0.0 ),
			Coeff4( 0.0 )
		{}

		// Member Constructor
		DefineASHRAEAdaptiveOptimumStartCoeffs(
			Fstring const & Name, // Name of the object
			Real64 const Coeff1, // 1st Coefficient of the equation
			Real64 const Coeff2, // 2nd Coefficient of the equation
			Real64 const Coeff3, // 3rd Coefficient of the equation
			Real64 const Coeff4 // 4th Coefficient of the equation
		) :
			Name( MaxNameLength, Name ),
			Coeff1( Coeff1 ),
			Coeff2( Coeff2 ),
			Coeff3( Coeff3 ),
			Coeff4( Coeff4 )
		{}

	};

	struct DefineDiffTSysAvailManager // Derived type for Differential Thermostat Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int HotNode; // "Hot" sensor node
		int ColdNode; // "Cold" sensor node
		Real64 TempDiffOn; // Temperature difference for turn on (delta C)
		Real64 TempDiffOff; // Temperature difference for turn off (delta C)
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineDiffTSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			HotNode( 0 ),
			ColdNode( 0 ),
			TempDiffOn( 0.0 ),
			TempDiffOff( 0.0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineDiffTSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const HotNode, // "Hot" sensor node
			int const ColdNode, // "Cold" sensor node
			Real64 const TempDiffOn, // Temperature difference for turn on (delta C)
			Real64 const TempDiffOff, // Temperature difference for turn off (delta C)
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			HotNode( HotNode ),
			ColdNode( ColdNode ),
			TempDiffOn( TempDiffOn ),
			TempDiffOff( TempDiffOff ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineHiLoSysAvailManager // Derived type for High/Low Temperature On/Off Sys Avail Managers
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int Node; // Sensor node
		Real64 Temp; // Temperature for on/off (C)
		int SchedPtr; // Applicability schedule pointer
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineHiLoSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			Node( 0 ),
			Temp( 0.0 ),
			SchedPtr( 0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineHiLoSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const Node, // Sensor node
			Real64 const Temp, // Temperature for on/off (C)
			int const SchedPtr, // Applicability schedule pointer
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			Node( Node ),
			Temp( Temp ),
			SchedPtr( SchedPtr ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineNightVentSysAvailManager
	{
		// Members
		Fstring Name; // Name of the manager object
		int MgrType; // Integer equivalent of availability manager type
		int SchedPtr; // Applicability schedule pointer
		Fstring FanSched; // Fan schedule name
		int FanSchedPtr; // Fan schedule pointer
		Fstring VentTempSched; // Ventilation temperature schedule
		int VentTempSchedPtr; // Ventilation temperature schedule pointer
		Real64 VentDelT; // Ventilation delta T [deltaC]
		Real64 VentTempLowLim; // ventilation temperature low limit
		Fstring CtrlZoneName; // Name of the control zone
		int ZoneNum; // zome number of control zone
		int ControlledZoneNum; // controlled zone number of control zone
		Real64 VentFlowFrac; // the night venting flow fraction
		int AvailStatus; // reports status of availability manager

		// Default Constructor
		DefineNightVentSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			SchedPtr( 0 ),
			FanSched( MaxNameLength ),
			FanSchedPtr( 0 ),
			VentTempSched( MaxNameLength ),
			VentTempSchedPtr( 0 ),
			VentDelT( 0.0 ),
			VentTempLowLim( 0.0 ),
			CtrlZoneName( MaxNameLength ),
			ZoneNum( 0 ),
			ControlledZoneNum( 0 ),
			VentFlowFrac( 0.0 ),
			AvailStatus( 0 )
		{}

		// Member Constructor
		DefineNightVentSysAvailManager(
			Fstring const & Name, // Name of the manager object
			int const MgrType, // Integer equivalent of availability manager type
			int const SchedPtr, // Applicability schedule pointer
			Fstring const & FanSched, // Fan schedule name
			int const FanSchedPtr, // Fan schedule pointer
			Fstring const & VentTempSched, // Ventilation temperature schedule
			int const VentTempSchedPtr, // Ventilation temperature schedule pointer
			Real64 const VentDelT, // Ventilation delta T [deltaC]
			Real64 const VentTempLowLim, // ventilation temperature low limit
			Fstring const & CtrlZoneName, // Name of the control zone
			int const ZoneNum, // zome number of control zone
			int const ControlledZoneNum, // controlled zone number of control zone
			Real64 const VentFlowFrac, // the night venting flow fraction
			int const AvailStatus // reports status of availability manager
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			SchedPtr( SchedPtr ),
			FanSched( MaxNameLength, FanSched ),
			FanSchedPtr( FanSchedPtr ),
			VentTempSched( MaxNameLength, VentTempSched ),
			VentTempSchedPtr( VentTempSchedPtr ),
			VentDelT( VentDelT ),
			VentTempLowLim( VentTempLowLim ),
			CtrlZoneName( MaxNameLength, CtrlZoneName ),
			ZoneNum( ZoneNum ),
			ControlledZoneNum( ControlledZoneNum ),
			VentFlowFrac( VentFlowFrac ),
			AvailStatus( AvailStatus )
		{}

	};

	struct DefineHybridVentSysAvailManager
	{
		// Members
		Fstring Name; // Name of the object
		int MgrType; // Integer equivalent of availability manager type
		Fstring AirLoopName; // Name of HVAC Air Loop
		int AirLoopNum; // HVAC Air Loop number
		Fstring ControlZoneName; // Controlled zone name
		int NodeNumOfControlledZone; // Controlled zone node number
		int ActualZoneNum; // Actual zone number
		int ControlledZoneNum; // Controlled zone number
		int ControlModeSchedPtr; // Ventilation control mode schedule pointer
		int ControlMode; // hybrid ventilation control mode
		int VentilationCtrl; // Ventilation control type: Noaction, Close, Open
		Real64 MinOutdoorTemp; // Minimum Outdoor Temperature [C]
		Real64 MaxOutdoorTemp; // Maximum Outdoor Temperature [C]
		Real64 MinOutdoorEnth; // Minimum Outdoor Enthalpy [J/kg]
		Real64 MaxOutdoorEnth; // Maximum Outdoor Enthalpy [J/kg]
		Real64 MinOutdoorDewPoint; // Minimum Outdoor Dew point temperature [C]
		Real64 MaxOutdoorDewPoint; // Maximum Outdoor Dew Point Temperature [C]
		Real64 MaxWindSpeed; // Maximum Wind speed [m/s]
		bool UseRainIndicator; // Use WeatherFile Rain Indicators
		Fstring MinOASched; // Minimum Outdoor Ventilation Air Schedule Name
		int MinOASchedPtr; // Minimum Outdoor Ventilation Air Schedule pointer
		int DewPointNoRHErrCount; // Dewpoint control mode error count without a humidistat
		int DewPointNoRHErrIndex; // Dewpoint control mode error index without a humidistat
		int DewPointErrCount; // Dewpoint control mode error count without a valid humidistat
		int DewPointErrIndex; // Dewpoint control mode error index without a valid humidistat
		int SingleHCErrCount; // Temperature and enthalpy control mode error count
		// with a singleHeatingCooling setpoint
		int SingleHCErrIndex; // Temperature and enthalpy control mode error index
		// with a singleHeatingCooling setpoint
		int OpeningFactorFWS; // Opening factor modifier as a function of wind speed
		int ANControlTypeSchedPtr; // AirflowNetwork control type schedule pointer
		int SimpleControlTypeSchedPtr; // Simple airflow object control type schedule pointer
		int VentilationPtr; // Ventilation object name pointer
		int AvailStatus; // reports status of availability manager
		Fstring VentilationName; // Ventilation object name
		bool HybridVentMgrConnectedToAirLoop; // Flag to check whether hybrid ventilation
		// manager is connected to air loop
		bool SimHybridVentSysAvailMgr; // Set to false when a zone has two hybrid ventilation
		// managers, one with air loop and one without

		// Default Constructor
		DefineHybridVentSysAvailManager() :
			Name( MaxNameLength ),
			MgrType( 0 ),
			AirLoopName( MaxNameLength ),
			AirLoopNum( 0 ),
			ControlZoneName( MaxNameLength ),
			NodeNumOfControlledZone( 0 ),
			ActualZoneNum( 0 ),
			ControlledZoneNum( 0 ),
			ControlModeSchedPtr( 0 ),
			ControlMode( 0 ),
			VentilationCtrl( 0 ),
			MinOutdoorTemp( -100.0 ),
			MaxOutdoorTemp( 100.0 ),
			MinOutdoorEnth( 0.1 ),
			MaxOutdoorEnth( 300000.0 ),
			MinOutdoorDewPoint( -100.0 ),
			MaxOutdoorDewPoint( 100.0 ),
			MaxWindSpeed( 0.0 ),
			UseRainIndicator( true ),
			MinOASched( MaxNameLength ),
			MinOASchedPtr( 0 ),
			DewPointNoRHErrCount( 0 ),
			DewPointNoRHErrIndex( 0 ),
			DewPointErrCount( 0 ),
			DewPointErrIndex( 0 ),
			SingleHCErrCount( 0 ),
			SingleHCErrIndex( 0 ),
			OpeningFactorFWS( 0 ),
			ANControlTypeSchedPtr( 0 ),
			SimpleControlTypeSchedPtr( 0 ),
			VentilationPtr( 0 ),
			AvailStatus( 0 ),
			VentilationName( MaxNameLength ),
			HybridVentMgrConnectedToAirLoop( true ),
			SimHybridVentSysAvailMgr( false )
		{}

		// Member Constructor
		DefineHybridVentSysAvailManager(
			Fstring const & Name, // Name of the object
			int const MgrType, // Integer equivalent of availability manager type
			Fstring const & AirLoopName, // Name of HVAC Air Loop
			int const AirLoopNum, // HVAC Air Loop number
			Fstring const & ControlZoneName, // Controlled zone name
			int const NodeNumOfControlledZone, // Controlled zone node number
			int const ActualZoneNum, // Actual zone number
			int const ControlledZoneNum, // Controlled zone number
			int const ControlModeSchedPtr, // Ventilation control mode schedule pointer
			int const ControlMode, // hybrid ventilation control mode
			int const VentilationCtrl, // Ventilation control type: Noaction, Close, Open
			Real64 const MinOutdoorTemp, // Minimum Outdoor Temperature [C]
			Real64 const MaxOutdoorTemp, // Maximum Outdoor Temperature [C]
			Real64 const MinOutdoorEnth, // Minimum Outdoor Enthalpy [J/kg]
			Real64 const MaxOutdoorEnth, // Maximum Outdoor Enthalpy [J/kg]
			Real64 const MinOutdoorDewPoint, // Minimum Outdoor Dew point temperature [C]
			Real64 const MaxOutdoorDewPoint, // Maximum Outdoor Dew Point Temperature [C]
			Real64 const MaxWindSpeed, // Maximum Wind speed [m/s]
			bool const UseRainIndicator, // Use WeatherFile Rain Indicators
			Fstring const & MinOASched, // Minimum Outdoor Ventilation Air Schedule Name
			int const MinOASchedPtr, // Minimum Outdoor Ventilation Air Schedule pointer
			int const DewPointNoRHErrCount, // Dewpoint control mode error count without a humidistat
			int const DewPointNoRHErrIndex, // Dewpoint control mode error index without a humidistat
			int const DewPointErrCount, // Dewpoint control mode error count without a valid humidistat
			int const DewPointErrIndex, // Dewpoint control mode error index without a valid humidistat
			int const SingleHCErrCount, // Temperature and enthalpy control mode error count
			int const SingleHCErrIndex, // Temperature and enthalpy control mode error index
			int const OpeningFactorFWS, // Opening factor modifier as a function of wind speed
			int const ANControlTypeSchedPtr, // AirflowNetwork control type schedule pointer
			int const SimpleControlTypeSchedPtr, // Simple airflow object control type schedule pointer
			int const VentilationPtr, // Ventilation object name pointer
			int const AvailStatus, // reports status of availability manager
			Fstring const & VentilationName, // Ventilation object name
			bool const HybridVentMgrConnectedToAirLoop, // Flag to check whether hybrid ventilation
			bool const SimHybridVentSysAvailMgr // Set to false when a zone has two hybrid ventilation
		) :
			Name( MaxNameLength, Name ),
			MgrType( MgrType ),
			AirLoopName( MaxNameLength, AirLoopName ),
			AirLoopNum( AirLoopNum ),
			ControlZoneName( MaxNameLength, ControlZoneName ),
			NodeNumOfControlledZone( NodeNumOfControlledZone ),
			ActualZoneNum( ActualZoneNum ),
			ControlledZoneNum( ControlledZoneNum ),
			ControlModeSchedPtr( ControlModeSchedPtr ),
			ControlMode( ControlMode ),
			VentilationCtrl( VentilationCtrl ),
			MinOutdoorTemp( MinOutdoorTemp ),
			MaxOutdoorTemp( MaxOutdoorTemp ),
			MinOutdoorEnth( MinOutdoorEnth ),
			MaxOutdoorEnth( MaxOutdoorEnth ),
			MinOutdoorDewPoint( MinOutdoorDewPoint ),
			MaxOutdoorDewPoint( MaxOutdoorDewPoint ),
			MaxWindSpeed( MaxWindSpeed ),
			UseRainIndicator( UseRainIndicator ),
			MinOASched( MaxNameLength, MinOASched ),
			MinOASchedPtr( MinOASchedPtr ),
			DewPointNoRHErrCount( DewPointNoRHErrCount ),
			DewPointNoRHErrIndex( DewPointNoRHErrIndex ),
			DewPointErrCount( DewPointErrCount ),
			DewPointErrIndex( DewPointErrIndex ),
			SingleHCErrCount( SingleHCErrCount ),
			SingleHCErrIndex( SingleHCErrIndex ),
			OpeningFactorFWS( OpeningFactorFWS ),
			ANControlTypeSchedPtr( ANControlTypeSchedPtr ),
			SimpleControlTypeSchedPtr( SimpleControlTypeSchedPtr ),
			VentilationPtr( VentilationPtr ),
			AvailStatus( AvailStatus ),
			VentilationName( MaxNameLength, VentilationName ),
			HybridVentMgrConnectedToAirLoop( HybridVentMgrConnectedToAirLoop ),
			SimHybridVentSysAvailMgr( SimHybridVentSysAvailMgr )
		{}

	};

	struct SysAvailManagerList
	{
		// Members
		Fstring Name; // Availability Manager List Name
		int NumItems;
		FArray1D_Fstring AvailManagerName;
		FArray1D_Fstring cAvailManagerType;
		FArray1D_int AvailManagerType;

		// Default Constructor
		SysAvailManagerList() :
			Name( MaxNameLength ),
			NumItems( 0 ),
			AvailManagerName( sFstring( MaxNameLength ) ),
			cAvailManagerType( sFstring( MaxNameLength ) )
		{}

		// Member Constructor
		SysAvailManagerList(
			Fstring const & Name, // Availability Manager List Name
			int const NumItems,
			FArray1_Fstring const & AvailManagerName,
			FArray1_Fstring const & cAvailManagerType,
			FArray1_int const & AvailManagerType
		) :
			Name( MaxNameLength, Name ),
			NumItems( NumItems ),
			AvailManagerName( AvailManagerName ),
			cAvailManagerType( cAvailManagerType ),
			AvailManagerType( AvailManagerType )
		{}

	};

	// Object Data
	extern FArray1D< DefineSchedSysAvailManager > SchedSysAvailMgrData;
	extern FArray1D< DefineSchedOnSysAvailManager > SchedOnSysAvailMgrData;
	extern FArray1D< DefineSchedOffSysAvailManager > SchedOffSysAvailMgrData;
	extern FArray1D< DefineNightCycSysAvailManager > NCycSysAvailMgrData;
	extern FArray1D< DefineDiffTSysAvailManager > DiffTSysAvailMgrData;
	extern FArray1D< DefineHiLoSysAvailManager > HiTurnOffSysAvailMgrData;
	extern FArray1D< DefineHiLoSysAvailManager > HiTurnOnSysAvailMgrData;
	extern FArray1D< DefineHiLoSysAvailManager > LoTurnOffSysAvailMgrData;
	extern FArray1D< DefineHiLoSysAvailManager > LoTurnOnSysAvailMgrData;
	extern FArray1D< DefineNightVentSysAvailManager > NVentSysAvailMgrData;
	extern FArray1D< DefineHybridVentSysAvailManager > HybridVentSysAvailMgrData;
	extern FArray1D< SysAvailManagerList > SysAvailMgrListData;
	extern FArray1D< DefineOptStartSysAvailManager > OptStartSysAvailMgrData;
	extern FArray1D< DefineASHRAEAdaptiveOptimumStartCoeffs > ASHRAEOptSCoeffCooling;
	extern FArray1D< DefineASHRAEAdaptiveOptimumStartCoeffs > ASHRAEOptSCoeffHeating;

	// Functions

	void
	ManageSystemAvailability();

	void
	GetSysAvailManagerInputs();

	void
	GetSysAvailManagerListInputs();

	void
	GetPlantAvailabilityManager(
		Fstring const & AvailabilityListName, // name that should be an Availability Manager List Name
		int const Loop, // which loop this is
		int const NumPlantLoops, // Total number of plant loops
		bool & ErrorsFound // true if certain errors are detected here
	);

	void
	GetAirLoopAvailabilityManager(
		Fstring const & AvailabilityListName, // name that should be an Availability Manager List Name
		int const Loop, // which loop this is
		int const NumAirLoops, // Total number of air loops
		bool & ErrorsFound // true if certain errors are detected here
	);

	void
	GetZoneEqAvailabilityManager(
		int const ZoneEquipType, // Type of ZoneHVAC:* component
		int const CompNum, // Index of a particular ZoneHVAC:* component
		bool & ErrorsFound // true if certain errors are detected here
	);

	void
	InitSysAvailManagers();

	void
	SimSysAvailManager(
		int const SysAvailType,
		Fstring const & SysAvailName,
		int & SysAvailNum,
		int const PriAirSysNum, // Primary Air System index. If being called for a ZoneHVAC:* component
		int const PreviousStatus,
		int & AvailStatus,
		Optional_int_const ZoneEquipType = _, // Type of ZoneHVAC:* equipment component
		Optional_int_const CompNum = _ // Index of ZoneHVAC:* equipment component
	);

	void
	CalcSchedSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled system availability manager
		int & AvailStatus // System status indicator
	);

	void
	CalcSchedOnSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled on system availability manager
		int & AvailStatus // System status indicator
	);

	void
	CalcSchedOffSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled off system availability manager
		int & AvailStatus // System status indicator
	);

	void
	CalcNCycSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled system availability manager
		int const PriAirSysNum, // number of the primary air system affected by this Avail. Manager
		int & AvailStatus, // System status indicator
		Optional_int_const ZoneEquipType = _, // Type of ZoneHVAC equipment component
		Optional_int_const CompNum = _ // Index of ZoneHVAC equipment component
	);

	void
	CalcOptStartSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled system availability manager
		int const PriAirSysNum, // number of the primary air system affected by this Avail. Manager
		int & AvailStatus, // System status indicator
		Optional_int_const ZoneEquipType = _, // Type of ZoneHVAC equipment component
		Optional_int_const CompNum = _ // Index of ZoneHVAC equipment component
	);

	void
	CalcNVentSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled system availability manager
		int const PriAirSysNum, // number of the primary air system affected by this Avail. Manager
		int & AvailStatus, // System status indicator
		Optional_int_const ZoneEquipType = _ // Type of zone equipment component
	);

	void
	CalcDiffTSysAvailMgr(
		int const SysAvailNum, // Number of the current scheduled system availability manager
		int const PreviousStatus, // System status for the previous timestep
		int & AvailStatus // System status indicator
	);

	void
	CalcHiTurnOffSysAvailMgr(
		int const SysAvailNum, // Number of the current scheduled system availability manager
		int & AvailStatus // System status indicator
	);

	void
	CalcHiTurnOnSysAvailMgr(
		int const SysAvailNum, // Number of the current scheduled system availability manager
		int & AvailStatus // System status indicator
	);

	void
	CalcLoTurnOffSysAvailMgr(
		int const SysAvailNum, // Number of the current scheduled system availability manager
		int & AvailStatus // System status indicator
	);

	void
	CalcLoTurnOnSysAvailMgr(
		int const SysAvailNum, // Number of the current scheduled system availability manager
		int & AvailStatus // System status indicator
	);

	int
	ValidateAndSetSysAvailabilityManagerType( Fstring const & AvailMgrName ); // name to validate

	void
	ManageHybridVentilation();

	void
	GetHybridVentilationInputs();

	void
	InitHybridVentSysAvailMgr();

	void
	CalcHybridVentSysAvailMgr(
		int const SysAvailNum, // number of the current scheduled system availability manager
		Optional_int_const PriAirSysNum = _ // number of the primary air system affected by this Avail. Manager
	);

	bool
	GetHybridVentilationControlStatus( int const ZoneNum ); // Index of zone

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // SystemAvailabilityManager

} // EnergyPlus

#endif
