#ifndef DataAirLoop_hh_INCLUDED
#define DataAirLoop_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace DataAirLoop {

	// Using/Aliasing

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	// DERIVED TYPE DEFINITIONS:

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern int NumOASystems; // Number of Outdoor Air Systems
	extern int LoopFanOperationMode; // OnOff fan operation mode
	extern Real64 LoopSystemOnMassFlowrate; // Loop mass flow rate during on cycle using an OnOff fan
	extern Real64 LoopSystemOffMassFlowrate; // Loop mass flow rate during off cycle using an OnOff fan
	extern Real64 LoopOnOffFanPartLoadRatio; // OnOff fan part load ratio
	extern Real64 LoopHeatingCoilMaxRTF; // Maximum run time fraction for electric or gas heating coil in an HVAC Air Loop
	extern Real64 LoopOnOffFanRTF; // OnOff fan run time fraction in an HVAC Air Loop
	extern Real64 LoopDXCoilRTF; // OnOff fan run time fraction in an HVAC Air Loop
	extern Real64 LoopCompCycRatio; // Loop compressor cycling ratio for multispeed heat pump
	extern bool AirLoopInputsFilled; // Set to TRUE after first pass through air loop

	// Types

	struct AirLoopZoneEquipConnectData
	{
		// Members
		std::string AirLoopName; // Name of Primary Air System
		int NumReturnNodes; // Number of return nodes connected to system
		int NumSupplyNodes; // number of supply nodes exiting primary air system
		int NumZonesCooled; // number of zones cooled by this primary air system
		int NumZonesHeated; // number of zones heated by this primary air system
		Array1D_int ZoneEquipReturnNodeNum; // Zone Equip side return air node numbers
		Array1D_int ZoneEquipSupplyNodeNum; // Zone equip side supply air node numbers
		Array1D_int AirLoopReturnNodeNum; // Air loop side return air node numbers
		Array1D_int AirLoopSupplyNodeNum; // Air loop side supply air node numbers
		Array1D_int CoolCtrlZoneNums; // Controlled zone numbers of zones cooled by this air loop
		Array1D_int HeatCtrlZoneNums; // Controlled zone numbers of zones heated by this air loop
		Array1D_int CoolZoneInletNodes; // Zone inlet node numbers of zones cooled by this air loop
		Array1D_int HeatZoneInletNodes; // Zone inlet node numbers of zones heated by this air loop
		Array1D_int TermUnitCoolInletNodes; // Air terminal unit cooling inlet node numbers for this air loop
		Array1D_int TermUnitHeatInletNodes; // Air terminal unit heating inlet node numbers for this air loop
		Array1D_int SupplyDuctType; // 1=main, 2=cooling, 3=heating, 4=other

		// Default Constructor
		AirLoopZoneEquipConnectData() :
			NumReturnNodes( 0 ),
			NumSupplyNodes( 0 ),
			NumZonesCooled( 0 ),
			NumZonesHeated( 0 )
		{}

		// Member Constructor
		AirLoopZoneEquipConnectData(
			std::string const & AirLoopName, // Name of Primary Air System
			int const NumReturnNodes, // Number of return nodes connected to system
			int const NumSupplyNodes, // number of supply nodes exiting primary air system
			int const NumZonesCooled, // number of zones cooled by this primary air system
			int const NumZonesHeated, // number of zones heated by this primary air system
			Array1_int const & ZoneEquipReturnNodeNum, // Zone Equip side return air node numbers
			Array1_int const & ZoneEquipSupplyNodeNum, // Zone equip side supply air node numbers
			Array1_int const & AirLoopReturnNodeNum, // Air loop side return air node numbers
			Array1_int const & AirLoopSupplyNodeNum, // Air loop side supply air node numbers
			Array1_int const & CoolCtrlZoneNums, // Controlled zone numbers of zones cooled by this air loop
			Array1_int const & HeatCtrlZoneNums, // Controlled zone numbers of zones heated by this air loop
			Array1_int const & CoolZoneInletNodes, // Zone inlet node numbers of zones cooled by this air loop
			Array1_int const & HeatZoneInletNodes, // Zone inlet node numbers of zones heated by this air loop
			Array1_int const & TermUnitCoolInletNodes, // Air terminal unit cooling inlet node numbers for this air loop
			Array1_int const & TermUnitHeatInletNodes, // Air terminal unit heating inlet node numbers for this air loop
			Array1_int const & SupplyDuctType // 1=main, 2=cooling, 3=heating, 4=other
		) :
			AirLoopName( AirLoopName ),
			NumReturnNodes( NumReturnNodes ),
			NumSupplyNodes( NumSupplyNodes ),
			NumZonesCooled( NumZonesCooled ),
			NumZonesHeated( NumZonesHeated ),
			ZoneEquipReturnNodeNum( ZoneEquipReturnNodeNum ),
			ZoneEquipSupplyNodeNum( ZoneEquipSupplyNodeNum ),
			AirLoopReturnNodeNum( AirLoopReturnNodeNum ),
			AirLoopSupplyNodeNum( AirLoopSupplyNodeNum ),
			CoolCtrlZoneNums( CoolCtrlZoneNums ),
			HeatCtrlZoneNums( HeatCtrlZoneNums ),
			CoolZoneInletNodes( CoolZoneInletNodes ),
			HeatZoneInletNodes( HeatZoneInletNodes ),
			TermUnitCoolInletNodes( TermUnitCoolInletNodes ),
			TermUnitHeatInletNodes( TermUnitHeatInletNodes ),
			SupplyDuctType( SupplyDuctType )
		{}

	};

	struct AirLoopOutsideAirConnectData
	{
		// Members
		bool OASysExists; // true if there is an Outside Air Sys
		int OASysInletNodeNum; // node number of return air inlet to OA sys
		int OASysOutletNodeNum; // node number of mixed air outlet of OA sys

		// Default Constructor
		AirLoopOutsideAirConnectData() :
			OASysExists( false ),
			OASysInletNodeNum( 0 ),
			OASysOutletNodeNum( 0 )
		{}

		// Member Constructor
		AirLoopOutsideAirConnectData(
			bool const OASysExists, // true if there is an Outside Air Sys
			int const OASysInletNodeNum, // node number of return air inlet to OA sys
			int const OASysOutletNodeNum // node number of mixed air outlet of OA sys
		) :
			OASysExists( OASysExists ),
			OASysInletNodeNum( OASysInletNodeNum ),
			OASysOutletNodeNum( OASysOutletNodeNum )
		{}

	};

	struct DefinePriAirSysAvailMgrs
	{
		// Members
		int NumAvailManagers; // number of availability managers for this system
		int AvailStatus; // system availability status
		int StartTime; // cycle on time (in SimTimeSteps)
		int StopTime; // cycle off time (in SimTimeSteps)
		Real64 ReqSupplyFrac; // required system flow rate (as a fraction)
		Array1D_string AvailManagerName; // name of each availability manager
		Array1D_int AvailManagerType; // type of availability manager
		Array1D_int AvailManagerNum; // index for availability manager

		// Default Constructor
		DefinePriAirSysAvailMgrs() :
			NumAvailManagers( 0 ),
			AvailStatus( 0 ),
			StartTime( 0 ),
			StopTime( 0 ),
			ReqSupplyFrac( 0.0 )
		{}

		// Member Constructor
		DefinePriAirSysAvailMgrs(
			int const NumAvailManagers, // number of availability managers for this system
			int const AvailStatus, // system availability status
			int const StartTime, // cycle on time (in SimTimeSteps)
			int const StopTime, // cycle off time (in SimTimeSteps)
			Real64 const ReqSupplyFrac, // required system flow rate (as a fraction)
			Array1_string const & AvailManagerName, // name of each availability manager
			Array1_int const & AvailManagerType, // type of availability manager
			Array1_int const & AvailManagerNum // index for availability manager
		) :
			NumAvailManagers( NumAvailManagers ),
			AvailStatus( AvailStatus ),
			StartTime( StartTime ),
			StopTime( StopTime ),
			ReqSupplyFrac( ReqSupplyFrac ),
			AvailManagerName( AvailManagerName ),
			AvailManagerType( AvailManagerType ),
			AvailManagerNum( AvailManagerNum )
		{}

	};

	struct AirLooptoZoneData // Derived type for air loop connection to zones on air loop
	{
		// Members
		int NumZones;
		Array1D_int Zone;
		Array1D_int ActualZoneNumber;

		// Default Constructor
		AirLooptoZoneData() :
			NumZones( 0 )
		{}

		// Member Constructor
		AirLooptoZoneData(
			int const NumZones,
			Array1_int const & Zone,
			Array1_int const & ActualZoneNumber
		) :
			NumZones( NumZones ),
			Zone( Zone ),
			ActualZoneNumber( ActualZoneNumber )
		{}

	};

	struct AirLoopControlData // Derived type for air control information
	{
		// Members
		std::string OACtrlName; // name of OA controller
		int OACtrlNum; // index of OA controller
		bool CyclingFan; // TRUE if currently the air loop supply fan is cycling
		bool AnyContFan; // TRUE if at any time supply fan is continuous
		int CycFanSchedPtr; // index of schedule indicating whether fan is cycling or continuous in a unitary system
		int FanOpMode; // 1=cycling fan cycling compressor; 2=constant fan cycling comptressor
		bool UnitarySys; // TRUE if a unitary system
		bool UnitarySysSimulating; // set FALSE for AirloopUnitarySystem after simulating to downstream coils can size independently
		bool Simple; // TRUE if system has 1 branch and 1 component
		bool CanNotLockoutEcono; // user input says econo lockout not allowed
		bool CanLockoutEconoWithHeating; // user input says econo lockout with heating is allowed
		bool CanLockoutEconoWithCompressor; // user input says econo lockout with compressor is allowed
		bool ReqstEconoLockoutWithHeating; // there is a request to lockout the economizer due to heating
		bool ReqstEconoLockoutWithCompressor; // there is a request to lockout the economizer due to compressor operation
		bool EconoActive; // if true economizer is active
		bool HeatRecoveryBypass; // if true heat recovery is bypassed (not active)
		bool ResimAirLoopFlag; // Same as SimAir, will trigger re-sim of air loops
		bool HeatRecoveryResimFlag; // Used to trigger new air loop sim when HX is used in OA system
		bool HeatRecoveryResimFlag2; // Used to trigger new air loop sim when HX is used in OA system
		bool CheckHeatRecoveryBypassStatus; // determines when heat recovery bypass is set
		bool EconomizerFlowLocked; // locks economizer flow for custon ERV operation
		bool HighHumCtrlActive; // if true high humidity control is active
		bool EconoLockout; // if true the economizer will be locked out (OA flow set to minimum)
		bool LoopFlowRateSet; // if true then the air loop flow rate should be set using ReqSupplyFrac
		bool NightVent; // if true then air loop is in night ventilation mode
		bool AllowWarmRestartFlag; // if true then speculative warm restart is attempted after first HVAC iteration
		bool NewFlowRateFlag; // true whenever the air mass flow rates have changed since last air loop sim
		bool ConvergedFlag; // true whenever the air loop sim was converged overall
		bool CoolingActiveFlag; // true whenever the air loop cooling coil is operating
		bool HeatingActiveFlag; // true whenever the air loop heating coil is operating
		bool OASysComponentsSimulated; // - true after OA components have been simulated
		bool AirLoopDCVFlag; // TRUE if the air loop has OA Controller specifying a Mechanical controller with DCV
		// - internal flag only

		// Default Constructor
		AirLoopControlData() :
			OACtrlNum( 0 ),
			CyclingFan( false ),
			AnyContFan( false ),
			CycFanSchedPtr( 0 ),
			FanOpMode( 0 ),
			UnitarySys( false ),
			UnitarySysSimulating( true ),
			Simple( false ),
			CanNotLockoutEcono( false ),
			CanLockoutEconoWithHeating( false ),
			CanLockoutEconoWithCompressor( false ),
			ReqstEconoLockoutWithHeating( false ),
			ReqstEconoLockoutWithCompressor( false ),
			EconoActive( false ),
			HeatRecoveryBypass( false ),
			ResimAirLoopFlag( false ),
			HeatRecoveryResimFlag( true ),
			HeatRecoveryResimFlag2( false ),
			CheckHeatRecoveryBypassStatus( false ),
			EconomizerFlowLocked( false ),
			HighHumCtrlActive( false ),
			EconoLockout( false ),
			LoopFlowRateSet( false ),
			NightVent( false ),
			AllowWarmRestartFlag( false ),
			NewFlowRateFlag( false ),
			ConvergedFlag( false ),
			CoolingActiveFlag( false ),
			HeatingActiveFlag( false ),
			OASysComponentsSimulated( false ),
			AirLoopDCVFlag( true )
		{}

		// Member Constructor
		AirLoopControlData(
			std::string const & OACtrlName, // name of OA controller
			int const OACtrlNum, // index of OA controller
			bool const CyclingFan, // TRUE if currently the air loop supply fan is cycling
			bool const AnyContFan, // TRUE if at any time supply fan is continuous
			int const CycFanSchedPtr, // index of schedule indicating whether fan is cycling or continuous in a unitary system
			int const FanOpMode, // 1=cycling fan cycling compressor; 2=constant fan cycling comptressor
			bool const UnitarySys, // TRUE if a unitary system
			bool const UnitarySysSimulating, // set FALSE for AirloopUnitarySystem after simulating to downstream coils can size independently
			bool const Simple, // TRUE if system has 1 branch and 1 component
			bool const CanNotLockoutEcono, // user input says econo lockout not allowed
			bool const CanLockoutEconoWithHeating, // user input says econo lockout with heating is allowed
			bool const CanLockoutEconoWithCompressor, // user input says econo lockout with compressor is allowed
			bool const ReqstEconoLockoutWithHeating, // there is a request to lockout the economizer due to heating
			bool const ReqstEconoLockoutWithCompressor, // there is a request to lockout the economizer due to compressor operation
			bool const EconoActive, // if true economizer is active
			bool const HeatRecoveryBypass, // if true heat recovery is bypassed (not active)
			bool const ResimAirLoopFlag, // Same as SimAir, will trigger re-sim of air loops
			bool const HeatRecoveryResimFlag, // Used to trigger new air loop sim when HX is used in OA system
			bool const HeatRecoveryResimFlag2, // Used to trigger new air loop sim when HX is used in OA system
			bool const CheckHeatRecoveryBypassStatus, // determines when heat recovery bypass is set
			bool const EconomizerFlowLocked, // locks economizer flow for custon ERV operation
			bool const HighHumCtrlActive, // if true high humidity control is active
			bool const EconoLockout, // if true the economizer will be locked out (OA flow set to minimum)
			bool const LoopFlowRateSet, // if true then the air loop flow rate should be set using ReqSupplyFrac
			bool const NightVent, // if true then air loop is in night ventilation mode
			bool const AllowWarmRestartFlag, // if true then speculative warm restart is attempted after first HVAC iteration
			bool const NewFlowRateFlag, // true whenever the air mass flow rates have changed since last air loop sim
			bool const ConvergedFlag, // true whenever the air loop sim was converged overall
			bool const CoolingActiveFlag, // true whenever the air loop cooling coil is operating
			bool const HeatingActiveFlag, // true whenever the air loop heating coil is operating
			bool const OASysComponentsSimulated, // - true after OA components have been simulated
			bool const AirLoopDCVFlag // TRUE if the air loop has OA Controller specifying a Mechanical controller with DCV
		) :
			OACtrlName( OACtrlName ),
			OACtrlNum( OACtrlNum ),
			CyclingFan( CyclingFan ),
			AnyContFan( AnyContFan ),
			CycFanSchedPtr( CycFanSchedPtr ),
			FanOpMode( FanOpMode ),
			UnitarySys( UnitarySys ),
			UnitarySysSimulating( UnitarySysSimulating ),
			Simple( Simple ),
			CanNotLockoutEcono( CanNotLockoutEcono ),
			CanLockoutEconoWithHeating( CanLockoutEconoWithHeating ),
			CanLockoutEconoWithCompressor( CanLockoutEconoWithCompressor ),
			ReqstEconoLockoutWithHeating( ReqstEconoLockoutWithHeating ),
			ReqstEconoLockoutWithCompressor( ReqstEconoLockoutWithCompressor ),
			EconoActive( EconoActive ),
			HeatRecoveryBypass( HeatRecoveryBypass ),
			ResimAirLoopFlag( ResimAirLoopFlag ),
			HeatRecoveryResimFlag( HeatRecoveryResimFlag ),
			HeatRecoveryResimFlag2( HeatRecoveryResimFlag2 ),
			CheckHeatRecoveryBypassStatus( CheckHeatRecoveryBypassStatus ),
			EconomizerFlowLocked( EconomizerFlowLocked ),
			HighHumCtrlActive( HighHumCtrlActive ),
			EconoLockout( EconoLockout ),
			LoopFlowRateSet( LoopFlowRateSet ),
			NightVent( NightVent ),
			AllowWarmRestartFlag( AllowWarmRestartFlag ),
			NewFlowRateFlag( NewFlowRateFlag ),
			ConvergedFlag( ConvergedFlag ),
			CoolingActiveFlag( CoolingActiveFlag ),
			HeatingActiveFlag( HeatingActiveFlag ),
			OASysComponentsSimulated( OASysComponentsSimulated ),
			AirLoopDCVFlag( AirLoopDCVFlag )
		{}

	};

	struct AirLoopFlowData // Derived type for air loop flow information
	{
		// Members
		Real64 ZoneExhaust; // total of zone exhaust air mass flow rate for this loop [kg/s]
		Real64 ZoneExhaustBalanced; // zone exhaust air that is balanced by simple air flow for loop [kg/s]
		Real64 DesSupply; // design supply air mass flow rate for loop [kg/s]
		Real64 SysToZoneDesFlowRatio; // System design flow divided by the sum of the zone design flows
		Real64 TotReturn; // the return air mass flow rate for this loop [kg/s]
		Real64 ReqSupplyFrac; // required flow (as a fraction of DesSupply) set by a manager
		Real64 MinOutAir; // minimum outside air mass flow rate [kg/s]
		Real64 MaxOutAir; // maximum outside air mass flow rate [kg/s]
		Real64 OAMinFrac; // minimum outside air flow fraction this time step
		Real64 Previous; // Previous mass air flow rate for this loop [kg/s]
		Real64 SupFlow; // supply air flow rate [kg/s]
		Real64 RetFlow; // return air flow rate [kg/s]
		Real64 RetFlow0; // sum of zone return flows before adjusting for total loop exhaust
		Real64 RecircFlow; // sum of zone plenum recirculated flows
		Real64 FanPLR; // Operating PLR of air loop fan
		Real64 OAFrac; // fraction of outside air to mixed air mass flow rate
		Real64 ZoneMixingFlow; // total zone mixing net flow used to cap the return flow
		bool FlowError; // error flag for flow error message

		// Default Constructor
		AirLoopFlowData() :
			ZoneExhaust( 0.0 ),
			ZoneExhaustBalanced( 0.0 ),
			DesSupply( 0.0 ),
			SysToZoneDesFlowRatio( 0.0 ),
			TotReturn( 0.0 ),
			ReqSupplyFrac( 1.0 ),
			MinOutAir( 0.0 ),
			MaxOutAir( 0.0 ),
			OAMinFrac( 0.0 ),
			Previous( 0.0 ),
			SupFlow( 0.0 ),
			RetFlow( 0.0 ),
			RetFlow0( 0.0 ),
			RecircFlow( 0.0 ),
			FanPLR( 0.0 ),
			OAFrac( 0.0 ),
			ZoneMixingFlow( 0.0 ),
			FlowError( false )
		{}

		// Member Constructor
		AirLoopFlowData(
			Real64 const ZoneExhaust, // total of zone exhaust air mass flow rate for this loop [kg/s]
			Real64 const ZoneExhaustBalanced, // zone exhaust air that is balanced by simple air flow for loop [kg/s]
			Real64 const DesSupply, // design supply air mass flow rate for loop [kg/s]
			Real64 const SysToZoneDesFlowRatio, // System design flow divided by the sum of the zone design flows
			Real64 const TotReturn, // the return air mass flow rate for this loop [kg/s]
			Real64 const ReqSupplyFrac, // required flow (as a fraction of DesSupply) set by a manager
			Real64 const MinOutAir, // minimum outside air mass flow rate [kg/s]
			Real64 const MaxOutAir, // maximum outside air mass flow rate [kg/s]
			Real64 const OAMinFrac, // minimum outside air flow fraction this time step
			Real64 const Previous, // Previous mass air flow rate for this loop [kg/s]
			Real64 const SupFlow, // supply air flow rate [kg/s]
			Real64 const RetFlow, // return air flow rate [kg/s]
			Real64 const RetFlow0, // sum of zone return flows before adjusting for total loop exhaust
			Real64 const RecircFlow, // sum of zone plenum recirculated flows
			Real64 const FanPLR, // Operating PLR of air loop fan
			Real64 const OAFrac, // fraction of outside air to mixed air mass flow rate
			Real64 const ZoneMixingFlow, // total zone mixing net flow used to cap the return flow
			bool const FlowError // error flag for flow error message
		) :
			ZoneExhaust( ZoneExhaust ),
			ZoneExhaustBalanced( ZoneExhaustBalanced ),
			DesSupply( DesSupply ),
			SysToZoneDesFlowRatio( SysToZoneDesFlowRatio ),
			TotReturn( TotReturn ),
			ReqSupplyFrac( ReqSupplyFrac ),
			MinOutAir( MinOutAir ),
			MaxOutAir( MaxOutAir ),
			OAMinFrac( OAMinFrac ),
			Previous( Previous ),
			SupFlow( SupFlow ),
			RetFlow( RetFlow ),
			RetFlow0( RetFlow0 ),
			RecircFlow( RecircFlow ),
			FanPLR( FanPLR ),
			OAFrac( OAFrac ),
			ZoneMixingFlow( ZoneMixingFlow ),
			FlowError( FlowError )
		{}

	};

	struct OAControllerData
	{
		// Members
		bool EconoActive; // if true economizer is active
		bool HighHumCtrlActive; // if true high humidity control is active

		// Default Constructor
		OAControllerData() :
			EconoActive( false ),
			HighHumCtrlActive( false )
		{}

		// Member Constructor
		OAControllerData(
			bool const EconoActive, // if true economizer is active
			bool const HighHumCtrlActive // if true high humidity control is active
		) :
			EconoActive( EconoActive ),
			HighHumCtrlActive( HighHumCtrlActive )
		{}

	};

	struct OutsideAirSysProps
	{
		// Members
		std::string Name;
		std::string ControllerListName;
		std::string ComponentListName;
		int ControllerListNum; // index of the Controller List
		int NumComponents;
		int NumControllers;
		int NumSimpleControllers; // number of CONTROLLER:SIMPLE objects in OA Sys controller list
		Array1D_string ComponentName;
		Array1D_string ComponentType;
		Array1D_int ComponentType_Num; // Parameterized (see above) Component Types this
		// module can address
		Array1D_int ComponentIndex; // Which one in list -- updated by routines called from here
		Array1D_string ControllerName;
		Array1D_string ControllerType;
		Array1D_int ControllerIndex; // Which one in list -- updated by routines called from here

		// Default Constructor
		OutsideAirSysProps() :
			ControllerListNum( 0 ),
			NumComponents( 0 ),
			NumControllers( 0 ),
			NumSimpleControllers( 0 )
		{}

		// Member Constructor
		OutsideAirSysProps(
			std::string const & Name,
			std::string const & ControllerListName,
			std::string const & ComponentListName,
			int const ControllerListNum, // index of the Controller List
			int const NumComponents,
			int const NumControllers,
			int const NumSimpleControllers, // number of CONTROLLER:SIMPLE objects in OA Sys controller list
			Array1_string const & ComponentName,
			Array1_string const & ComponentType,
			Array1_int const & ComponentType_Num, // Parameterized (see above) Component Types this
			Array1_int const & ComponentIndex, // Which one in list -- updated by routines called from here
			Array1_string const & ControllerName,
			Array1_string const & ControllerType,
			Array1_int const & ControllerIndex // Which one in list -- updated by routines called from here
		) :
			Name( Name ),
			ControllerListName( ControllerListName ),
			ComponentListName( ComponentListName ),
			ControllerListNum( ControllerListNum ),
			NumComponents( NumComponents ),
			NumControllers( NumControllers ),
			NumSimpleControllers( NumSimpleControllers ),
			ComponentName( ComponentName ),
			ComponentType( ComponentType ),
			ComponentType_Num( ComponentType_Num ),
			ComponentIndex( ComponentIndex ),
			ControllerName( ControllerName ),
			ControllerType( ControllerType ),
			ControllerIndex( ControllerIndex )
		{}

	};

	// Object Data
	extern Array1D< AirLoopZoneEquipConnectData > AirToZoneNodeInfo;
	extern Array1D< AirLoopOutsideAirConnectData > AirToOANodeInfo;
	extern Array1D< DefinePriAirSysAvailMgrs > PriAirSysAvailMgr;
	extern Array1D< AirLooptoZoneData > AirLoopZoneInfo;
	extern Array1D< AirLoopControlData > AirLoopControlInfo;
	extern Array1D< AirLoopFlowData > AirLoopFlow;
	extern Array1D< OAControllerData > OAControllerInfo;
	extern Array1D< OutsideAirSysProps > OutsideAirSys;

	// Clears the global data in DataAirLoop.
	// Needed for unit tests, should not be normally called.
	void
	clear_state();

} // DataAirLoop

} // EnergyPlus

#endif
