#ifndef UnitVentilator_hh_INCLUDED
#define UnitVentilator_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace UnitVentilator {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// Currrent Module Unit type
	extern std::string const cMO_UnitVentilator;

	// Parameters for outside air control types:
	extern int const Heating_ElectricCoilType;
	extern int const Heating_GasCoilType;
	extern int const Heating_WaterCoilType;
	extern int const Heating_SteamCoilType;
	extern int const Cooling_CoilWaterCooling;
	extern int const Cooling_CoilDetailedCooling;
	extern int const Cooling_CoilHXAssisted;
	// OA operation modes
	extern int const VariablePercent;
	extern int const FixedTemperature;
	extern int const FixedOAControl;
	// coil operation
	extern int const On; // normal coil operation
	extern int const Off; // signal coil shouldn't run
	extern int const NoneOption;
	extern int const BothOption;
	extern int const HeatingOption;
	extern int const CoolingOption;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern bool HCoilOn; // TRUE if the heating coil (gas or electric especially) should be running
	extern int NumOfUnitVents; // Number of unit ventilators in the input file
	extern Real64 OAMassFlowRate; // Outside air mass flow rate for the unit ventilator
	extern Real64 QZnReq; // heating or cooling needed by zone [watts]
	extern Array1D_bool MySizeFlag;
	extern bool GetUnitVentilatorInputFlag; // First time, input is "gotten"
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE UnitVentilator
	//PRIVATE UpdateUnitVentilator

	// Types

	struct UnitVentilatorData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		int AirInNode; // inlet air node number
		int AirOutNode; // outlet air node number
		int FanOutletNode; // outlet node number for fan exit
		// (assumes fan is upstream of heating coil)
		int FanType_Num; // Fan type number (see DataHVACGlobals)
		std::string FanType; // type of fan
		std::string FanName; // name of fan
		int Fan_Index;
		int FanSchedPtr; // index to fan operating mode schedule
		int FanAvailSchedPtr; // index to fan availability schedule
		int OpMode; // mode of operation; 1=cycling fan, cycling coil, 2=continuous fan, cycling coil
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 MaxAirVolFlow; // m3/s
		Real64 MaxAirMassFlow; // kg/s
		int OAControlType; // type of control; options are VARIABLE PERCENT and FIXED TEMPERATURE
		std::string MinOASchedName; // schedule of fraction for minimum outside air (all controls)
		int MinOASchedPtr; // index to schedule
		std::string MaxOASchedName; // schedule of percentages for maximum outside air fraction (variable %)
		int MaxOASchedPtr; // index to schedule
		std::string TempSchedName; // schedule of temperatures for desired "mixed air"
		// temperature (fixed temp.)
		int TempSchedPtr; // index to schedule
		int OutsideAirNode; // outside air node number
		int AirReliefNode; // relief air node number
		int OAMixerOutNode; // outlet node after the outside air mixer (inlet to coils if present)
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		Real64 MinOutAirVolFlow; // m3/s
		Real64 MinOutAirMassFlow; // kg/s
		int CoilOption; // type of coil option; options are BOTH, HEATING, COOLING, AND NONE
		bool HCoilPresent; // .TRUE. if unit ventilator has a heating coil
		int HCoilType; // type of heating coil (water, gas, electric, etc.)
		std::string HCoilName; // name of heating coil
		std::string HCoilTypeCh; // type of heating coil character string (same as type on idf file).
		int HCoil_Index;
		int HCoil_PlantTypeNum;
		int HCoil_FluidIndex;
		std::string HCoilSchedName; // availability schedule for the heating coil
		int HCoilSchedPtr; // index to schedule
		Real64 HCoilSchedValue;
		Real64 MaxVolHotWaterFlow; // m3/s
		Real64 MaxVolHotSteamFlow; // m3/s
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MaxHotSteamFlow;
		Real64 MinHotSteamFlow;
		Real64 MinVolHotWaterFlow; // m3/s
		Real64 MinVolHotSteamFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		int HotControlNode; // hot water control node
		int HotCoilOutNodeNum; // outlet of coil
		Real64 HotControlOffset; // control tolerance
		int HWLoopNum; // index for plant loop with hot water coil
		int HWLoopSide; // index for plant loop side for hot water coil
		int HWBranchNum; // index for plant branch for hot water coil
		int HWCompNum; // index for plant component for hot water coil
		bool CCoilPresent; // .TRUE. if unit ventilator has a cooling coil
		std::string CCoilName; // name of cooling coil
		std::string CCoilTypeCh; // type of cooling coil as character string (same as on idf file)
		int CCoil_Index;
		std::string CCoilPlantName; // name of cooling coil for plant
		std::string CCoilPlantType; // type of cooling coil for plant
		int CCoil_PlantTypeNum;
		int CCoilType; // type of cooling coil:
		// 'Coil:Cooling:Water:DetailedGeometry' or
		// 'CoilSystem:Cooling:Water:HeatExchangerAssisted'
		std::string CCoilSchedName; // availability schedule for the cooling coil
		int CCoilSchedPtr; // index to schedule
		Real64 CCoilSchedValue;
		Real64 MaxVolColdWaterFlow; // m3/s
		Real64 MaxColdWaterFlow; // kg/s
		Real64 MinVolColdWaterFlow; // m3/s
		Real64 MinColdWaterFlow; // kg/s
		int ColdControlNode; // chilled water control node
		int ColdCoilOutNodeNum; // chilled water coil out node
		Real64 ColdControlOffset; // control tolerance
		int CWLoopNum; // index for plant loop with chilled water coil
		int CWLoopSide; // index for plant loop side for chilled water coil
		int CWBranchNum; // index for plant branch for chilled water coil
		int CWCompNum; // index for plant component for chilled water coil
		// Report data
		Real64 HeatPower; // unit heating output in watts
		Real64 HeatEnergy; // unit heating output in J
		Real64 TotCoolPower;
		Real64 TotCoolEnergy;
		Real64 SensCoolPower;
		Real64 SensCoolEnergy;
		Real64 ElecPower;
		Real64 ElecEnergy;
		std::string AvailManagerListName; // Name of an availability manager list object
		int AvailStatus;
		Real64 FanPartLoadRatio; // fan part-load ratio for time step
		Real64 PartLoadFrac; // unit ventilator part-load ratio for time step
		int ZonePtr; // pointer to a zone served by a unit ventilator
		int HVACSizingIndex; // index of a HVACSizing object for a unit ventilator
		// for unit ventilator object

		// Default Constructor
		UnitVentilatorData() :
			SchedPtr( 0 ),
			AirInNode( 0 ),
			AirOutNode( 0 ),
			FanOutletNode( 0 ),
			FanType_Num( 0 ),
			Fan_Index( 0 ),
			FanSchedPtr( 0 ),
			FanAvailSchedPtr( 0 ),
			OpMode( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			MaxAirVolFlow( 0.0 ),
			MaxAirMassFlow( 0.0 ),
			OAControlType( 0 ),
			MinOASchedPtr( 0 ),
			MaxOASchedPtr( 0 ),
			TempSchedPtr( 0 ),
			OutsideAirNode( 0 ),
			AirReliefNode( 0 ),
			OAMixerOutNode( 0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			MinOutAirVolFlow( 0.0 ),
			MinOutAirMassFlow( 0.0 ),
			CoilOption( 0 ),
			HCoilPresent( false ),
			HCoilType( 0 ),
			HCoil_Index( 0 ),
			HCoil_PlantTypeNum( 0 ),
			HCoil_FluidIndex( 0 ),
			HCoilSchedPtr( 0 ),
			HCoilSchedValue( 0.0 ),
			MaxVolHotWaterFlow( 0.0 ),
			MaxVolHotSteamFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MaxHotSteamFlow( 0.0 ),
			MinHotSteamFlow( 0.0 ),
			MinVolHotWaterFlow( 0.0 ),
			MinVolHotSteamFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			HotControlNode( 0 ),
			HotCoilOutNodeNum( 0 ),
			HotControlOffset( 0.0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			CCoilPresent( false ),
			CCoil_Index( 0 ),
			CCoil_PlantTypeNum( 0 ),
			CCoilType( 0 ),
			CCoilSchedPtr( 0 ),
			CCoilSchedValue( 0.0 ),
			MaxVolColdWaterFlow( 0.0 ),
			MaxColdWaterFlow( 0.0 ),
			MinVolColdWaterFlow( 0.0 ),
			MinColdWaterFlow( 0.0 ),
			ColdControlNode( 0 ),
			ColdCoilOutNodeNum( 0 ),
			ColdControlOffset( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			HeatPower( 0.0 ),
			HeatEnergy( 0.0 ),
			TotCoolPower( 0.0 ),
			TotCoolEnergy( 0.0 ),
			SensCoolPower( 0.0 ),
			SensCoolEnergy( 0.0 ),
			ElecPower( 0.0 ),
			ElecEnergy( 0.0 ),
			AvailStatus( 0 ),
			FanPartLoadRatio( 0.0 ),
			PartLoadFrac( 0.0 ),
			ZonePtr( 0 ),
			HVACSizingIndex( 0 )
		{}

		// Member Constructor
		UnitVentilatorData(
			std::string const & Name, // name of unit
			std::string const & SchedName, // availability schedule
			int const SchedPtr, // index to schedule
			int const AirInNode, // inlet air node number
			int const AirOutNode, // outlet air node number
			int const FanOutletNode, // outlet node number for fan exit
			int const FanType_Num, // Fan type number (see DataHVACGlobals)
			std::string const & FanType, // type of fan
			std::string const & FanName, // name of fan
			int const Fan_Index,
			int const FanSchedPtr, // index to fan operating mode schedule
			int const FanAvailSchedPtr, // index to fan availability schedule
			int const OpMode, // mode of operation; 1=cycling fan, cycling coil, 2=continuous fan, cycling coil
			int const ControlCompTypeNum,
			int const CompErrIndex,
			Real64 const MaxAirVolFlow, // m3/s
			Real64 const MaxAirMassFlow, // kg/s
			int const OAControlType, // type of control; options are VARIABLE PERCENT and FIXED TEMPERATURE
			std::string const & MinOASchedName, // schedule of fraction for minimum outside air (all controls)
			int const MinOASchedPtr, // index to schedule
			std::string const & MaxOASchedName, // schedule of percentages for maximum outside air fraction (variable %)
			int const MaxOASchedPtr, // index to schedule
			std::string const & TempSchedName, // schedule of temperatures for desired "mixed air"
			int const TempSchedPtr, // index to schedule
			int const OutsideAirNode, // outside air node number
			int const AirReliefNode, // relief air node number
			int const OAMixerOutNode, // outlet node after the outside air mixer (inlet to coils if present)
			Real64 const OutAirVolFlow, // m3/s
			Real64 const OutAirMassFlow, // kg/s
			Real64 const MinOutAirVolFlow, // m3/s
			Real64 const MinOutAirMassFlow, // kg/s
			int const CoilOption, // type of coil option; options are BOTH, HEATING, COOLING, AND NONE
			bool const HCoilPresent, // .TRUE. if unit ventilator has a heating coil
			int const HCoilType, // type of heating coil (water, gas, electric, etc.)
			std::string const & HCoilName, // name of heating coil
			std::string const & HCoilTypeCh, // type of heating coil character string (same as type on idf file).
			int const HCoil_Index,
			int const HCoil_PlantTypeNum,
			int const HCoil_FluidIndex,
			std::string const & HCoilSchedName, // availability schedule for the heating coil
			int const HCoilSchedPtr, // index to schedule
			Real64 const HCoilSchedValue,
			Real64 const MaxVolHotWaterFlow, // m3/s
			Real64 const MaxVolHotSteamFlow, // m3/s
			Real64 const MaxHotWaterFlow, // kg/s
			Real64 const MaxHotSteamFlow,
			Real64 const MinHotSteamFlow,
			Real64 const MinVolHotWaterFlow, // m3/s
			Real64 const MinVolHotSteamFlow, // m3/s
			Real64 const MinHotWaterFlow, // kg/s
			int const HotControlNode, // hot water control node
			int const HotCoilOutNodeNum, // outlet of coil
			Real64 const HotControlOffset, // control tolerance
			int const HWLoopNum, // index for plant loop with hot water coil
			int const HWLoopSide, // index for plant loop side for hot water coil
			int const HWBranchNum, // index for plant branch for hot water coil
			int const HWCompNum, // index for plant component for hot water coil
			bool const CCoilPresent, // .TRUE. if unit ventilator has a cooling coil
			std::string const & CCoilName, // name of cooling coil
			std::string const & CCoilTypeCh, // type of cooling coil as character string (same as on idf file)
			int const CCoil_Index,
			std::string const & CCoilPlantName, // name of cooling coil for plant
			std::string const & CCoilPlantType, // type of cooling coil for plant
			int const CCoil_PlantTypeNum,
			int const CCoilType, // type of cooling coil:
			std::string const & CCoilSchedName, // availability schedule for the cooling coil
			int const CCoilSchedPtr, // index to schedule
			Real64 const CCoilSchedValue,
			Real64 const MaxVolColdWaterFlow, // m3/s
			Real64 const MaxColdWaterFlow, // kg/s
			Real64 const MinVolColdWaterFlow, // m3/s
			Real64 const MinColdWaterFlow, // kg/s
			int const ColdControlNode, // chilled water control node
			int const ColdCoilOutNodeNum, // chilled water coil out node
			Real64 const ColdControlOffset, // control tolerance
			int const CWLoopNum, // index for plant loop with chilled water coil
			int const CWLoopSide, // index for plant loop side for chilled water coil
			int const CWBranchNum, // index for plant branch for chilled water coil
			int const CWCompNum, // index for plant component for chilled water coil
			Real64 const HeatPower, // unit heating output in watts
			Real64 const HeatEnergy, // unit heating output in J
			Real64 const TotCoolPower,
			Real64 const TotCoolEnergy,
			Real64 const SensCoolPower,
			Real64 const SensCoolEnergy,
			Real64 const ElecPower,
			Real64 const ElecEnergy,
			std::string const & AvailManagerListName, // Name of an availability manager list object
			int const AvailStatus,
			Real64 const FanPartLoadRatio, // fan part-load ratio for time step
			Real64 const PartLoadFrac, // unit ventilator part-load ratio for time step
			int const ZonePtr, // pointer to a zone served by a unit ventilator
			int const HVACSizingIndex // index of a HVACSizing object for a unit ventilator
		) :
			Name( Name ),
			SchedName( SchedName ),
			SchedPtr( SchedPtr ),
			AirInNode( AirInNode ),
			AirOutNode( AirOutNode ),
			FanOutletNode( FanOutletNode ),
			FanType_Num( FanType_Num ),
			FanType( FanType ),
			FanName( FanName ),
			Fan_Index( Fan_Index ),
			FanSchedPtr( FanSchedPtr ),
			FanAvailSchedPtr( FanAvailSchedPtr ),
			OpMode( OpMode ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			MaxAirVolFlow( MaxAirVolFlow ),
			MaxAirMassFlow( MaxAirMassFlow ),
			OAControlType( OAControlType ),
			MinOASchedName( MinOASchedName ),
			MinOASchedPtr( MinOASchedPtr ),
			MaxOASchedName( MaxOASchedName ),
			MaxOASchedPtr( MaxOASchedPtr ),
			TempSchedName( TempSchedName ),
			TempSchedPtr( TempSchedPtr ),
			OutsideAirNode( OutsideAirNode ),
			AirReliefNode( AirReliefNode ),
			OAMixerOutNode( OAMixerOutNode ),
			OutAirVolFlow( OutAirVolFlow ),
			OutAirMassFlow( OutAirMassFlow ),
			MinOutAirVolFlow( MinOutAirVolFlow ),
			MinOutAirMassFlow( MinOutAirMassFlow ),
			CoilOption( CoilOption ),
			HCoilPresent( HCoilPresent ),
			HCoilType( HCoilType ),
			HCoilName( HCoilName ),
			HCoilTypeCh( HCoilTypeCh ),
			HCoil_Index( HCoil_Index ),
			HCoil_PlantTypeNum( HCoil_PlantTypeNum ),
			HCoil_FluidIndex( HCoil_FluidIndex ),
			HCoilSchedName( HCoilSchedName ),
			HCoilSchedPtr( HCoilSchedPtr ),
			HCoilSchedValue( HCoilSchedValue ),
			MaxVolHotWaterFlow( MaxVolHotWaterFlow ),
			MaxVolHotSteamFlow( MaxVolHotSteamFlow ),
			MaxHotWaterFlow( MaxHotWaterFlow ),
			MaxHotSteamFlow( MaxHotSteamFlow ),
			MinHotSteamFlow( MinHotSteamFlow ),
			MinVolHotWaterFlow( MinVolHotWaterFlow ),
			MinVolHotSteamFlow( MinVolHotSteamFlow ),
			MinHotWaterFlow( MinHotWaterFlow ),
			HotControlNode( HotControlNode ),
			HotCoilOutNodeNum( HotCoilOutNodeNum ),
			HotControlOffset( HotControlOffset ),
			HWLoopNum( HWLoopNum ),
			HWLoopSide( HWLoopSide ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			CCoilPresent( CCoilPresent ),
			CCoilName( CCoilName ),
			CCoilTypeCh( CCoilTypeCh ),
			CCoil_Index( CCoil_Index ),
			CCoilPlantName( CCoilPlantName ),
			CCoilPlantType( CCoilPlantType ),
			CCoil_PlantTypeNum( CCoil_PlantTypeNum ),
			CCoilType( CCoilType ),
			CCoilSchedName( CCoilSchedName ),
			CCoilSchedPtr( CCoilSchedPtr ),
			CCoilSchedValue( CCoilSchedValue ),
			MaxVolColdWaterFlow( MaxVolColdWaterFlow ),
			MaxColdWaterFlow( MaxColdWaterFlow ),
			MinVolColdWaterFlow( MinVolColdWaterFlow ),
			MinColdWaterFlow( MinColdWaterFlow ),
			ColdControlNode( ColdControlNode ),
			ColdCoilOutNodeNum( ColdCoilOutNodeNum ),
			ColdControlOffset( ColdControlOffset ),
			CWLoopNum( CWLoopNum ),
			CWLoopSide( CWLoopSide ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			HeatPower( HeatPower ),
			HeatEnergy( HeatEnergy ),
			TotCoolPower( TotCoolPower ),
			TotCoolEnergy( TotCoolEnergy ),
			SensCoolPower( SensCoolPower ),
			SensCoolEnergy( SensCoolEnergy ),
			ElecPower( ElecPower ),
			ElecEnergy( ElecEnergy ),
			AvailManagerListName( AvailManagerListName ),
			AvailStatus( AvailStatus ),
			FanPartLoadRatio( FanPartLoadRatio ),
			PartLoadFrac( PartLoadFrac ),
			ZonePtr( ZonePtr ),
			HVACSizingIndex( HVACSizingIndex )
		{}

	};

	struct UnitVentNumericFieldData
	{
		// Members
		Array1D_string FieldNames;

		// Default Constructor
		UnitVentNumericFieldData()
		{}

		// Member Constructor
		UnitVentNumericFieldData(
			Array1_string const & FieldNames // Name of the HeatingCoil numeric field descriptions
			) :
			FieldNames( FieldNames )
		{}
	};

	// Object Data
	extern Array1D< UnitVentilatorData > UnitVent;
	extern Array1D< UnitVentNumericFieldData > UnitVentNumericFields;

	// Functions

	void
	SimUnitVentilator(
		std::string const & CompName, // name of the fan coil unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetUnitVentilatorInput();

	void
	InitUnitVentilator(
		int const UnitVentNum, // index for the current unit ventilator
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		int const ZoneNum // number of zone being served
	);

	void
	SizeUnitVentilator( int const UnitVentNum );

	void
	CalcUnitVentilator(
		int & UnitVentNum, // number of the current fan coil unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	CalcUnitVentilatorComponents(
		int const UnitVentNum, // Unit index in unit ventilator array
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		Real64 & LoadMet, // load met by unit (watts)
		Optional_int_const OpMode = _, // Fan Type
		Optional< Real64 const > PartLoadFrac = _ // Part Load Ratio of coil and fan
	);

	void
	SimUnitVentOAMixer(
		int const UnitVentNum, // Unit index in unit ventilator array
		int const FanOpMode // unit ventilator fan operating mode
	);

	//SUBROUTINE UpdateUnitVentilator

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateUnitVentilator

	void
	ReportUnitVentilator( int const UnitVentNum ); // Unit index in unit ventilator array

	int
	GetUnitVentilatorOutAirNode( int const UnitVentNum );

	int
	GetUnitVentilatorZoneInletAirNode( int const UnitVentNum );

	int
	GetUnitVentilatorMixedAirNode( int const UnitVentNum );

	int
	GetUnitVentilatorReturnAirNode( int const UnitVentNum );

	Real64
	CalcUnitVentilatorResidual(
		Real64 const PartLoadRatio, // Coil Part Load Ratio
		Array1< Real64 > const & Par // Function parameters
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

} // UnitVentilator

} // EnergyPlus

#endif
