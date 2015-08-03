#ifndef SingleDuct_hh_INCLUDED
#define SingleDuct_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace SingleDuct {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	extern int const Normal;
	extern int const ReverseAction;
	// SysTypes represented here
	extern int const SingleDuctVAVReheat;
	extern int const SingleDuctConstVolReheat;
	extern int const SingleDuctVAVNoReheat;
	extern int const SingleDuctVAVReheatVSFan;
	extern int const SingleDuctCBVAVReheat;
	extern int const SingleDuctCBVAVNoReheat;
	// Reheat Coil Types used here
	extern int const HCoilType_None;
	extern int const HCoilType_Gas;
	extern int const HCoilType_Electric;
	extern int const HCoilType_SimpleHeating;
	extern int const HCoilType_SteamAirHeating;
	// Fan types used here
	extern int const FanType_None;
	extern int const FanType_VS;
	// Minimum Flow Fraction Input Method
	extern int const ConstantMinFrac;
	extern int const ScheduledMinFrac;
	extern int const FixedMin;
	extern int NumATMixers;

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > MassFlow1; // previous value of the terminal unit mass flow rate
	extern Array1D< Real64 > MassFlow2; // previous value of the previous value of the mass flow rate
	extern Array1D< Real64 > MassFlow3;
	extern Array1D< Real64 > MassFlowDiff;
	extern bool GetInputFlag; // Flag set to make sure you get input once
	extern bool GetATMixerFlag; // Flag set to make sure you get input once
	extern int NumConstVolSys;
	extern Array1D_bool CheckEquipName;

	// INTERFACE BLOCK SPECIFICATIONS

	extern int NumSys; // The Number of Systems found in the Input

	// Subroutine Specifications for the Module
	// Driver/Manager Routines

	// Get Input routines for module

	// Initialization routines for module

	// Algorithms for the module

	// Update routine to check convergence and update nodes

	// Reporting routines for module

	// Types

	struct SysDesignParams
	{
		// Members
		std::string SysName; // Name of the Sys
		std::string SysType; // Type of Sys ie. VAV, Mixing, Inducing, etc.
		int SysType_Num; // Numeric Equivalent for System type
		std::string Schedule; // Sys Operation Schedule
		int SchedPtr; // Pointer to the correct schedule
		std::string ReheatComp; // Type of the Reheat Coil Object
		int ReheatComp_Num; // Numeric Equivalent in this module for Coil type
		int ReheatComp_Index; // Returned Index number from other routines
		std::string ReheatName; // name of reheat coil
		int ReheatComp_PlantType; // typeOf_ number for plant type of heating coil
		std::string FanType; // Type of the Fan Object
		int Fan_Num; // Numeric Equivalent in this module for fan type
		int Fan_Index; // Returned Index number from other routines
		int ControlCompTypeNum;
		int CompErrIndex;
		std::string FanName; // name of fan
		Real64 MaxAirVolFlowRate; // Max Specified Volume Flow Rate of Sys (cooling max) [m3/sec]
		Real64 AirMassFlowRateMax; // Max Specified Mass Flow Rate of Sys (cooling max) [kg/sec]
		Real64 MaxHeatAirVolFlowRate; // Max specified volume flow rate of unit at max heating [m3/s]
		Real64 HeatAirMassFlowRateMax; // Max Specified Mass Flow Rate of unit at max heating [kg/sec]
		int ZoneMinAirFracMethod; // parameter for what method is used for min flow fraction
		Real64 ZoneMinAirFrac; // Fraction of supply air used as minimum flow
		Real64 ZoneFixedMinAir; // Absolute minimum supply air flow
		int ZoneMinAirFracSchPtr; // pointer to the schedule for min flow fraction
		bool ConstantMinAirFracSetByUser; // record if user left field blank for constant min fraction.
		bool FixedMinAirSetByUser; // record if user left field blank for constant min fraction.
		Real64 DesignMinAirFrac; // store user entered constant min flow fract for design
		Real64 DesignFixedMinAir; // store user entered constant min flow for design
		int InletNodeNum; // terminal unit inlet node number; damper inlet node number
		int OutletNodeNum; // damper outlet node number for VAV; unused by CV; coil air inlet node for VAV
		// fan outlet node, coil inlet node for VAV VS Fan
		int ReheatControlNode; // hot water inlet node for heating coil
		int ReheatCoilOutletNode; // outlet node for heating coil
		Real64 ReheatCoilMaxCapacity; // heating coil capacity, W
		int ReheatAirOutletNode; // terminal unit outlet node; heating coil air outlet node
		Real64 MaxReheatWaterVolFlow; // m3/s
		Real64 MaxReheatSteamVolFlow; // m3/s
		Real64 MaxReheatWaterFlow; // kg/s
		Real64 MaxReheatSteamFlow; // kg/s
		Real64 MinReheatWaterVolFlow; // m3/s
		Real64 MinReheatSteamVolFlow; // m3/s
		Real64 MinReheatWaterFlow; // kg/s
		Real64 MinReheatSteamFlow; // kg/s
		Real64 ControllerOffset;
		Real64 MaxReheatTemp; // C
		bool MaxReheatTempSetByUser;
		int DamperHeatingAction; // ! 1=NORMAL;  2=REVERSE ACTION
		Real64 DamperPosition;
		int ADUNum; // index of corresponding air distribution unit
		int FluidIndex; // Refrigerant index
		int ErrCount1; // iteration limit exceeded in Hot Water Flow Calc
		int ErrCount1c; // iteration limit exceeded in Hot Water Flow Calc - continue
		int ErrCount2; // bad iterations limits in hot water flow calc
		Real64 ZoneFloorArea; // Zone floor area
		int CtrlZoneNum; // Pointer to CtrlZone data structure
		int ActualZoneNum; // Pointer to Zone data Structure
		Real64 MaxAirVolFlowRateDuringReheat; // Maximum vol flow during reheat
		Real64 MaxAirVolFractionDuringReheat; // Maximum vol flow fraction during reheat
		Real64 AirMassFlowDuringReheatMax; // Maximum mass flow during reheat
		int ZoneOutdoorAirMethod; // Outdoor air method
		Real64 OutdoorAirFlowRate; // report variable for TU outdoor air flow rate
		bool NoOAFlowInputFromUser; // avoids OA calculation if no input specified by user
		int OARequirementsPtr; // - Index to DesignSpecification:OutdoorAir object
		int AirLoopNum;
		int HWLoopNum; // plant topology, loop number
		int HWLoopSide; // plant topology, loop side number
		int HWBranchIndex; // plant topology, Branch number
		int HWCompIndex; // plant topology, Component number
		std::string ZoneHVACUnitType; // type of Zone HVAC unit for air terminal mixer units
		std::string ZoneHVACUnitName; // name of Zone HVAC unit for air terminal mixer units
		int SecInNode; // zone or zone unit air node number
		// warning variables
		int IterationLimit; // Used for RegulaFalsi error -1
		int IterationFailed; // Used for RegulaFalsi error -2

		// Default Constructor
		SysDesignParams() :
			SysType_Num( 0 ),
			SchedPtr( 0 ),
			ReheatComp_Num( 0 ),
			ReheatComp_Index( 0 ),
			ReheatComp_PlantType( 0 ),
			Fan_Num( 0 ),
			Fan_Index( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			MaxAirVolFlowRate( 0.0 ),
			AirMassFlowRateMax( 0.0 ),
			MaxHeatAirVolFlowRate( 0.0 ),
			HeatAirMassFlowRateMax( 0.0 ),
			ZoneMinAirFracMethod( ConstantMinFrac ),
			ZoneMinAirFrac( 0.0 ),
			ZoneFixedMinAir( 0.0 ),
			ZoneMinAirFracSchPtr( 0 ),
			ConstantMinAirFracSetByUser( false ),
			FixedMinAirSetByUser( false ),
			DesignMinAirFrac( 0.0 ),
			DesignFixedMinAir( 0.0 ),
			InletNodeNum( 0 ),
			OutletNodeNum( 0 ),
			ReheatControlNode( 0 ),
			ReheatCoilOutletNode( 0 ),
			ReheatCoilMaxCapacity( 0.0 ),
			ReheatAirOutletNode( 0 ),
			MaxReheatWaterVolFlow( 0.0 ),
			MaxReheatSteamVolFlow( 0.0 ),
			MaxReheatWaterFlow( 0.0 ),
			MaxReheatSteamFlow( 0.0 ),
			MinReheatWaterVolFlow( 0.0 ),
			MinReheatSteamVolFlow( 0.0 ),
			MinReheatWaterFlow( 0.0 ),
			MinReheatSteamFlow( 0.0 ),
			ControllerOffset( 0.0 ),
			MaxReheatTemp( 0.0 ),
			MaxReheatTempSetByUser( false ),
			DamperHeatingAction( 0 ),
			DamperPosition( 0.0 ),
			ADUNum( 0 ),
			FluidIndex( 0 ),
			ErrCount1( 0 ),
			ErrCount1c( 0 ),
			ErrCount2( 0 ),
			ZoneFloorArea( 0.0 ),
			CtrlZoneNum( 0 ),
			ActualZoneNum( 0 ),
			MaxAirVolFlowRateDuringReheat( 0.0 ),
			MaxAirVolFractionDuringReheat( 0.0 ),
			AirMassFlowDuringReheatMax( 0.0 ),
			ZoneOutdoorAirMethod( 0 ),
			OutdoorAirFlowRate( 0.0 ),
			NoOAFlowInputFromUser( true ),
			OARequirementsPtr( 0 ),
			AirLoopNum( 0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchIndex( 0 ),
			HWCompIndex( 0 ),
			SecInNode( 0 ),
			IterationLimit( 0 ),
			IterationFailed( 0 )
		{}

		// Member Constructor
		SysDesignParams(
			std::string const & SysName, // Name of the Sys
			std::string const & SysType, // Type of Sys ie. VAV, Mixing, Inducing, etc.
			int const SysType_Num, // Numeric Equivalent for System type
			std::string const & Schedule, // Sys Operation Schedule
			int const SchedPtr, // Pointer to the correct schedule
			std::string const & ReheatComp, // Type of the Reheat Coil Object
			int const ReheatComp_Num, // Numeric Equivalent in this module for Coil type
			int const ReheatComp_Index, // Returned Index number from other routines
			std::string const & ReheatName, // name of reheat coil
			int const ReheatComp_PlantType, // typeOf_ number for plant type of heating coil
			std::string const & FanType, // Type of the Fan Object
			int const Fan_Num, // Numeric Equivalent in this module for fan type
			int const Fan_Index, // Returned Index number from other routines
			int const ControlCompTypeNum,
			int const CompErrIndex,
			std::string const & FanName, // name of fan
			Real64 const MaxAirVolFlowRate, // Max Specified Volume Flow Rate of Sys (cooling max) [m3/sec]
			Real64 const AirMassFlowRateMax, // Max Specified Mass Flow Rate of Sys (cooling max) [kg/sec]
			Real64 const MaxHeatAirVolFlowRate, // Max specified volume flow rate of unit at max heating [m3/s]
			Real64 const HeatAirMassFlowRateMax, // Max Specified Mass Flow Rate of unit at max heating [kg/sec]
			int const ZoneMinAirFracMethod, // parameter for what method is used for min flow fraction
			Real64 const ZoneMinAirFrac, // Fraction of supply air used as minimum flow
			Real64 const ZoneFixedMinAir, // Absolute minimum supply air flow
			int const ZoneMinAirFracSchPtr, // pointer to the schedule for min flow fraction
			bool const ConstantMinAirFracSetByUser, // record if user left field blank for constant min fraction.
			bool const FixedMinAirSetByUser, // record if user left field blank for constant min fraction.
			Real64 const DesignMinAirFrac, // store user entered constant min flow fract for design
			Real64 const DesignFixedMinAir, // store user entered constant min flow for design
			int const InletNodeNum, // terminal unit inlet node number; damper inlet node number
			int const OutletNodeNum, // damper outlet node number for VAV; unused by CV; coil air inlet node for VAV
			int const ReheatControlNode, // hot water inlet node for heating coil
			int const ReheatCoilOutletNode, // outlet node for heating coil
			Real64 const ReheatCoilMaxCapacity, // heating coil capacity, W
			int const ReheatAirOutletNode, // terminal unit outlet node; heating coil air outlet node
			Real64 const MaxReheatWaterVolFlow, // m3/s
			Real64 const MaxReheatSteamVolFlow, // m3/s
			Real64 const MaxReheatWaterFlow, // kg/s
			Real64 const MaxReheatSteamFlow, // kg/s
			Real64 const MinReheatWaterVolFlow, // m3/s
			Real64 const MinReheatSteamVolFlow, // m3/s
			Real64 const MinReheatWaterFlow, // kg/s
			Real64 const MinReheatSteamFlow, // kg/s
			Real64 const ControllerOffset,
			Real64 const MaxReheatTemp, // C
			bool const MaxReheatTempSetByUser,
			int const DamperHeatingAction, // ! 1=NORMAL;  2=REVERSE ACTION
			Real64 const DamperPosition,
			int const ADUNum, // index of corresponding air distribution unit
			int const FluidIndex, // Refrigerant index
			int const ErrCount1, // iteration limit exceeded in Hot Water Flow Calc
			int const ErrCount1c, // iteration limit exceeded in Hot Water Flow Calc - continue
			int const ErrCount2, // bad iterations limits in hot water flow calc
			Real64 const ZoneFloorArea, // Zone floor area
			int const CtrlZoneNum, // Pointer to CtrlZone data structure
			int const ActualZoneNum, // Pointer to Zone data Structure
			Real64 const MaxAirVolFlowRateDuringReheat, // Maximum vol flow during reheat
			Real64 const MaxAirVolFractionDuringReheat, // Maximum vol flow fraction during reheat
			Real64 const AirMassFlowDuringReheatMax, // Maximum mass flow during reheat
			int const ZoneOutdoorAirMethod, // Outdoor air method
			Real64 const OutdoorAirFlowRate, // report variable for TU outdoor air flow rate
			bool const NoOAFlowInputFromUser, // avoids OA calculation if no input specified by user
			int const OARequirementsPtr, // - Index to DesignSpecification:OutdoorAir object
			int const AirLoopNum,
			int const HWLoopNum, // plant topology, loop number
			int const HWLoopSide, // plant topology, loop side number
			int const HWBranchIndex, // plant topology, Branch number
			int const HWCompIndex, // plant topology, Component number
			std::string const & ZoneHVACUnitType, // type of Zone HVAC unit for air terminal mixer units
			std::string const & ZoneHVACUnitName, // name of Zone HVAC unit for air terminal mixer units
			int const SecInNode, // zone or zone unit air node number
			int const IterationLimit, // Used for RegulaFalsi error -1
			int const IterationFailed // Used for RegulaFalsi error -2
		) :
			SysName( SysName ),
			SysType( SysType ),
			SysType_Num( SysType_Num ),
			Schedule( Schedule ),
			SchedPtr( SchedPtr ),
			ReheatComp( ReheatComp ),
			ReheatComp_Num( ReheatComp_Num ),
			ReheatComp_Index( ReheatComp_Index ),
			ReheatName( ReheatName ),
			ReheatComp_PlantType( ReheatComp_PlantType ),
			FanType( FanType ),
			Fan_Num( Fan_Num ),
			Fan_Index( Fan_Index ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			FanName( FanName ),
			MaxAirVolFlowRate( MaxAirVolFlowRate ),
			AirMassFlowRateMax( AirMassFlowRateMax ),
			MaxHeatAirVolFlowRate( MaxHeatAirVolFlowRate ),
			HeatAirMassFlowRateMax( HeatAirMassFlowRateMax ),
			ZoneMinAirFracMethod( ZoneMinAirFracMethod ),
			ZoneMinAirFrac( ZoneMinAirFrac ),
			ZoneFixedMinAir( ZoneFixedMinAir ),
			ZoneMinAirFracSchPtr( ZoneMinAirFracSchPtr ),
			ConstantMinAirFracSetByUser( ConstantMinAirFracSetByUser ),
			FixedMinAirSetByUser( FixedMinAirSetByUser ),
			DesignMinAirFrac( DesignMinAirFrac ),
			DesignFixedMinAir( DesignFixedMinAir ),
			InletNodeNum( InletNodeNum ),
			OutletNodeNum( OutletNodeNum ),
			ReheatControlNode( ReheatControlNode ),
			ReheatCoilOutletNode( ReheatCoilOutletNode ),
			ReheatCoilMaxCapacity( ReheatCoilMaxCapacity ),
			ReheatAirOutletNode( ReheatAirOutletNode ),
			MaxReheatWaterVolFlow( MaxReheatWaterVolFlow ),
			MaxReheatSteamVolFlow( MaxReheatSteamVolFlow ),
			MaxReheatWaterFlow( MaxReheatWaterFlow ),
			MaxReheatSteamFlow( MaxReheatSteamFlow ),
			MinReheatWaterVolFlow( MinReheatWaterVolFlow ),
			MinReheatSteamVolFlow( MinReheatSteamVolFlow ),
			MinReheatWaterFlow( MinReheatWaterFlow ),
			MinReheatSteamFlow( MinReheatSteamFlow ),
			ControllerOffset( ControllerOffset ),
			MaxReheatTemp( MaxReheatTemp ),
			MaxReheatTempSetByUser( MaxReheatTempSetByUser ),
			DamperHeatingAction( DamperHeatingAction ),
			DamperPosition( DamperPosition ),
			ADUNum( ADUNum ),
			FluidIndex( FluidIndex ),
			ErrCount1( ErrCount1 ),
			ErrCount1c( ErrCount1c ),
			ErrCount2( ErrCount2 ),
			ZoneFloorArea( ZoneFloorArea ),
			CtrlZoneNum( CtrlZoneNum ),
			ActualZoneNum( ActualZoneNum ),
			MaxAirVolFlowRateDuringReheat( MaxAirVolFlowRateDuringReheat ),
			MaxAirVolFractionDuringReheat( MaxAirVolFractionDuringReheat ),
			AirMassFlowDuringReheatMax( AirMassFlowDuringReheatMax ),
			ZoneOutdoorAirMethod( ZoneOutdoorAirMethod ),
			OutdoorAirFlowRate( OutdoorAirFlowRate ),
			NoOAFlowInputFromUser( NoOAFlowInputFromUser ),
			OARequirementsPtr( OARequirementsPtr ),
			AirLoopNum( AirLoopNum ),
			HWLoopNum( HWLoopNum ),
			HWLoopSide( HWLoopSide ),
			HWBranchIndex( HWBranchIndex ),
			HWCompIndex( HWCompIndex ),
			ZoneHVACUnitType( ZoneHVACUnitType ),
			ZoneHVACUnitName( ZoneHVACUnitName ),
			SecInNode( SecInNode ),
			IterationLimit( IterationLimit ),
			IterationFailed( IterationFailed )
		{}

	};

	struct AirTerminalMixerData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		int MixerType; // type of inlet mixer, 1 = inlet side, 2 = supply side
		int ZoneHVACUnitType; // type of Zone HVAC unit. ZoneHVAC:WaterToAirHeatPump =1, ZoneHVAC:FourPipeFanCoil = 2
		std::string ZoneHVACUnitName; // name of Zone HVAC unit
		int SecInNode; // secondary air inlet node number
		int PriInNode; // primary air inlet node number
		int MixedAirOutNode; // mixed air outlet node number
		Real64 ZoneAirTemp; // zone air in temp
		Real64 ZoneAirHumRat; // zone air in hum rat
		Real64 ZoneAirEnthalpy; // zone air in enthalpy
		Real64 ZoneAirPressure; // zone air in pressure
		Real64 ZoneAirMassFlowRate; // zone air in mass flow rate
		Real64 DOASTemp; // DOAS air in temp
		Real64 DOASHumRat; // DOAS air in hum rat
		Real64 DOASEnthalpy; // DOAS air in enthalpy
		Real64 DOASPressure; // DOAS air in pressure
		Real64 DOASMassFlowRate; // DOAS air in mass flow rate
		Real64 MixedAirTemp; // mixed air in temp
		Real64 MixedAirHumRat; // mixed air in hum rat
		Real64 MixedAirEnthalpy; // mixed air in enthalpy
		Real64 MixedAirPressure; // mixed air in pressure
		Real64 MixedAirMassFlowRate; // mixed air in mass flow rate
		Real64 MaxAirMassFlowRate; // maximum air mass flow rate allowed through component

		// Default Constructor
		AirTerminalMixerData() :
			MixerType( 0 ),
			ZoneHVACUnitType( 0 ),
			SecInNode( 0 ),
			PriInNode( 0 ),
			MixedAirOutNode( 0 ),
			ZoneAirTemp( 0.0 ),
			ZoneAirHumRat( 0.0 ),
			ZoneAirEnthalpy( 0.0 ),
			ZoneAirPressure( 0.0 ),
			ZoneAirMassFlowRate( 0.0 ),
			DOASTemp( 0.0 ),
			DOASHumRat( 0.0 ),
			DOASEnthalpy( 0.0 ),
			DOASPressure( 0.0 ),
			DOASMassFlowRate( 0.0 ),
			MixedAirTemp( 0.0 ),
			MixedAirHumRat( 0.0 ),
			MixedAirEnthalpy( 0.0 ),
			MixedAirPressure( 0.0 ),
			MixedAirMassFlowRate( 0.0 ),
			MaxAirMassFlowRate( 0.0 )
		{}

		// Member Constructor
		AirTerminalMixerData(
			std::string const & Name, // name of unit
			int const MixerType, // type of inlet mixer, 1 = inlet side, 2 = supply side
			int const ZoneHVACUnitType, // type of Zone HVAC unit. ZoneHVAC:WaterToAirHeatPump =1, ZoneHVAC:FourPipeFanCoil = 2
			std::string const & ZoneHVACUnitName, // name of Zone HVAC unit
			int const SecInNode, // secondary air inlet node number
			int const PriInNode, // primary air inlet node number
			int const MixedAirOutNode, // mixed air outlet node number
			Real64 const ZoneAirTemp, // zone air in temp
			Real64 const ZoneAirHumRat, // zone air in hum rat
			Real64 const ZoneAirEnthalpy, // zone air in enthalpy
			Real64 const ZoneAirPressure, // zone air in pressure
			Real64 const ZoneAirMassFlowRate, // zone air in mass flow rate
			Real64 const DOASTemp, // DOAS air in temp
			Real64 const DOASHumRat, // DOAS air in hum rat
			Real64 const DOASEnthalpy, // DOAS air in enthalpy
			Real64 const DOASPressure, // DOAS air in pressure
			Real64 const DOASMassFlowRate, // DOAS air in mass flow rate
			Real64 const MixedAirTemp, // mixed air in temp
			Real64 const MixedAirHumRat, // mixed air in hum rat
			Real64 const MixedAirEnthalpy, // mixed air in enthalpy
			Real64 const MixedAirPressure, // mixed air in pressure
			Real64 const MixedAirMassFlowRate, // mixed air in mass flow rate
			Real64 const MaxAirMassFlowRate // maximum air mass flow rate allowed through component
		) :
			Name( Name ),
			MixerType( MixerType ),
			ZoneHVACUnitType( ZoneHVACUnitType ),
			ZoneHVACUnitName( ZoneHVACUnitName ),
			SecInNode( SecInNode ),
			PriInNode( PriInNode ),
			MixedAirOutNode( MixedAirOutNode ),
			ZoneAirTemp( ZoneAirTemp ),
			ZoneAirHumRat( ZoneAirHumRat ),
			ZoneAirEnthalpy( ZoneAirEnthalpy ),
			ZoneAirPressure( ZoneAirPressure ),
			ZoneAirMassFlowRate( ZoneAirMassFlowRate ),
			DOASTemp( DOASTemp ),
			DOASHumRat( DOASHumRat ),
			DOASEnthalpy( DOASEnthalpy ),
			DOASPressure( DOASPressure ),
			DOASMassFlowRate( DOASMassFlowRate ),
			MixedAirTemp( MixedAirTemp ),
			MixedAirHumRat( MixedAirHumRat ),
			MixedAirEnthalpy( MixedAirEnthalpy ),
			MixedAirPressure( MixedAirPressure ),
			MixedAirMassFlowRate( MixedAirMassFlowRate ),
			MaxAirMassFlowRate( MaxAirMassFlowRate )
		{}

	};

	struct SysFlowConditions
	{
		// Members
		Real64 AirMassFlowRate; // MassFlow through the Sys being Simulated [kg/Sec]
		Real64 AirMassFlowRateMaxAvail; // MassFlow through the Sys being Simulated [kg/Sec]
		Real64 AirMassFlowRateMinAvail; // MassFlow through the Sys being Simulated [kg/Sec]
		Real64 AirTemp; // (C)
		Real64 AirHumRat; // (Kg/Kg)
		Real64 AirEnthalpy; // (J/Kg)
		Real64 AirPressure;

		// Default Constructor
		SysFlowConditions() :
			AirMassFlowRate( 0.0 ),
			AirMassFlowRateMaxAvail( 0.0 ),
			AirMassFlowRateMinAvail( 0.0 ),
			AirTemp( 0.0 ),
			AirHumRat( 0.0 ),
			AirEnthalpy( 0.0 ),
			AirPressure( 0.0 )
		{}

		// Member Constructor
		SysFlowConditions(
			Real64 const AirMassFlowRate, // MassFlow through the Sys being Simulated [kg/Sec]
			Real64 const AirMassFlowRateMaxAvail, // MassFlow through the Sys being Simulated [kg/Sec]
			Real64 const AirMassFlowRateMinAvail, // MassFlow through the Sys being Simulated [kg/Sec]
			Real64 const AirTemp, // (C)
			Real64 const AirHumRat, // (Kg/Kg)
			Real64 const AirEnthalpy, // (J/Kg)
			Real64 const AirPressure
		) :
			AirMassFlowRate( AirMassFlowRate ),
			AirMassFlowRateMaxAvail( AirMassFlowRateMaxAvail ),
			AirMassFlowRateMinAvail( AirMassFlowRateMinAvail ),
			AirTemp( AirTemp ),
			AirHumRat( AirHumRat ),
			AirEnthalpy( AirEnthalpy ),
			AirPressure( AirPressure )
		{}

	};

	// Object Data
	extern Array1D< SysDesignParams > Sys;
	extern Array1D< SysFlowConditions > SysInlet;
	extern Array1D< SysFlowConditions > SysOutlet;
	extern Array1D< AirTerminalMixerData > SysATMixer;

	// Functions

	void
	SimulateSingleDuct(
		std::string const & CompName,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum,
		int & CompIndex
	);

	// Get Input Section of the Module
	//******************************************************************************

	void
	GetSysInput();

	// End of Get Input subroutines for the Module
	//******************************************************************************

	// Beginning Initialization Section of the Module
	//******************************************************************************

	void
	InitSys(
		int const SysNum,
		bool const FirstHVACIteration
	);

	void
	SizeSys( int const SysNum );

	// End Initialization Section of the Module
	//******************************************************************************

	// Begin Algorithm Section of the Module
	//******************************************************************************

	void
	SimVAV(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	CalcOAMassFlow(
		int const SysNum, // index to terminal unit
		Real64 & SAMassFlow, // outside air based on optional user input
		Real64 & AirLoopOAFrac // outside air based on optional user input
	);

	void
	SimCBVAV(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	SimVAVVS(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	SimConstVol(
		int const SysNum,
		bool const FirstHVACIteration,
		int const ZoneNum,
		int const ZoneNodeNum
	);

	void
	CalcVAVVS(
		int const SysNum, // Unit index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const ZoneNode, // zone node number
		int const HCoilType, // type of hot water coil !unused1208
		Real64 const HWFlow, // hot water flow (kg/s)
		Real64 const HCoilReq, // gas or elec coil demand requested
		int const FanType, // type of fan
		Real64 const AirFlow, // air flow rate (kg/s)
		int const FanOn, // 1 means fan is on
		Real64 & LoadMet // load met by unit (watts)
	);

	Real64
	VAVVSCoolingResidual(
		Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	Real64
	VAVVSHWNoFanResidual(
		Real64 const HWMassFlow, // hot water mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	Real64
	VAVVSHWFanOnResidual(
		Real64 const SupplyAirMassFlow, // supply air mass flow rate [kg/s]
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	Real64
	VAVVSHCFanOnResidual(
		Real64 const HeatingFrac, // fraction of maximum heating output
		Array1< Real64 > const & Par // Par(1) = REAL(SysNum)
	);

	// End Algorithm Section of the Module
	// *****************************************************************************

	// Beginning of Update subroutines for the Sys Module
	// *****************************************************************************

	void
	UpdateSys( int const SysNum );

	//        End of Update subroutines for the Sys Module
	// *****************************************************************************

	// Beginning of Reporting subroutines for the Sys Module
	// *****************************************************************************

	void
	ReportSys( int const SysNum ); // unused1208

	void
	GetHVACSingleDuctSysIndex(
		std::string const & SDSName,
		int & SDSIndex,
		bool & ErrorsFound,
		Optional_string_const ThisObjectType = _,
		Optional_int DamperInletNode = _, // Damper inlet node number
		Optional_int DamperOutletNode = _ // Damper outlet node number
	);

	void
	SimATMixer(
		std::string const & SysName,
		bool const FirstHVACIteration,
		int & SysIndex
	);

	void
	GetATMixers();

	void
	InitATMixer(
		int const ATMixerNum,
		bool const FirstHVACIteration
	);

	void
	CalcATMixer( int const SysNum );

	void
	UpdateATMixer( int const SysNum );

	void
	GetATMixerPriNode(
		std::string const & ZoneEquipName,
		int & ATMixerPriNode
	);

	void
	GetATMixerSecNode(
		std::string const & ZoneEquipName,
		int & ATMixerSecNode
	);

	void
	GetATMixerOutNode(
		std::string const & ZoneEquipName,
		int & ATMixerOutNode
	);

	void
	GetATMixer(
		std::string const & ZoneEquipName, // zone unit name name
		std::string & ATMixerName, // air terminal mixer name
		int & ATMixerNum, // air terminal mixer index
		int & ATMixerType, // air teminal mixer type
		int & ATMixerPriNode, // air terminal mixer primary air node number
		int & ATMixerSecNode, // air terminal mixer secondary air node number
		int & ATMixerOutNode // air terminal mixer outlet air node number
	);

	void
	SetATMixerPriFlow(
		int const ATMixerNum, // Air terminal mixer index
		Optional< Real64 const > PriAirMassFlowRate = _ // Air terminal mixer primary air mass flow rate [kg/s]
	);

	//        End of Reporting subroutines for the Sys Module
	// *****************************************************************************

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

} // SingleDuct

} // EnergyPlus

#endif
