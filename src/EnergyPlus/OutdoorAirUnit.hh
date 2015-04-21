#ifndef OutdoorAirUnit_hh_INCLUDED
#define OutdoorAirUnit_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace OutdoorAirUnit {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS

	// component types addressed by this module
	extern std::string const cMO_OutdoorAirUnit;

	extern int const WaterCoil_SimpleCool;
	extern int const WaterCoil_Cooling;
	extern int const WaterCoil_SimpleHeat;
	extern int const SteamCoil_AirHeat;
	extern int const WaterCoil_DetailedCool;
	extern int const WaterCoil_CoolingHXAsst;
	extern int const Coil_ElectricHeat;
	extern int const Coil_GasHeat;
	extern int const DXSystem;
	extern int const HeatXchngr;
	extern int const Desiccant;
	extern int const DXHeatPumpSystem;
	extern int const UnitarySystem;

	//  Control Types
	extern int const Neutral; // Controls system using zone mean air temperature
	extern int const Unconditioned; // Controls system when outdoor air temperature is identified with control temperature
	extern int const Temperature; // Controls system using temperature band

	// Operating Options
	extern int const HeatingMode; // normal heating coil operation
	extern int const CoolingMode; // normal cooling coil operation
	extern int const NeutralMode; // signal coil shouldn't run

	extern Array1D_string const CurrentModuleObjects;

	// Parameters below (CO - Current module Object.  used primarily in Get Inputs)
	// Multiple Get Input routines in this module or these would be in individual routines.
	extern int const CO_OAUnit;
	extern int const CO_OAEqList;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern int NumOfOAUnits; // Number of outdoor air unit in the input file
	extern Real64 OAMassFlowRate; // Outside air mass flow rate for the zone outdoor air unit
	extern Array1D_bool MyOneTimeErrorFlag;
	extern bool GetOutdoorAirUnitInputFlag; // Flag set to make sure you get input once

	// Autosizing variables
	extern Array1D_bool MySizeFlag;
	extern Array1D_bool CheckEquipName;

	// SUBROUTINE SPECIFICATIONS FOR MODULE OUTDOOR AIR UNIT
	//PRIVATE UpdateOutdoorAirUnit
	//PUBLIC GetOutAirCoilOutletTemp

	// Types

	struct OAEquipList
	{
		// Members
		// Equipment List Data
		std::string ComponentName;
		std::string ComponentType;
		int ComponentType_Num; // Parameterized Component Types this module can address
		int ComponentIndex; // Which one in list -- updated by routines called from here
		int CoilAirInletNode;
		int CoilAirOutletNode;
		int CoilWaterInletNode;
		int CoilWaterOutletNode;
		int CoilPlantTypeOfNum;
		int LoopNum;
		int LoopSideNum;
		int BranchNum;
		int CompNum;
		int FluidIndex; // used in Steam...
		Real64 MaxVolWaterFlow;
		Real64 MaxWaterMassFlow;
		Real64 MinVolWaterFlow;
		Real64 MinWaterMassFlow;
		// End Of Equipment list data

		// Default Constructor
		OAEquipList() :
			ComponentType_Num( 0 ),
			ComponentIndex( 0 ),
			CoilAirInletNode( 0 ),
			CoilAirOutletNode( 0 ),
			CoilWaterInletNode( 0 ),
			CoilWaterOutletNode( 0 ),
			CoilPlantTypeOfNum( 0 ),
			LoopNum( 0 ),
			LoopSideNum( 0 ),
			BranchNum( 0 ),
			CompNum( 0 ),
			FluidIndex( 0 ),
			MaxVolWaterFlow( 0.0 ),
			MaxWaterMassFlow( 0.0 ),
			MinVolWaterFlow( 0.0 ),
			MinWaterMassFlow( 0.0 )
		{}

		// Member Constructor
		OAEquipList(
			std::string const & ComponentName,
			std::string const & ComponentType,
			int const ComponentType_Num, // Parameterized Component Types this module can address
			int const ComponentIndex, // Which one in list -- updated by routines called from here
			int const CoilAirInletNode,
			int const CoilAirOutletNode,
			int const CoilWaterInletNode,
			int const CoilWaterOutletNode,
			int const CoilPlantTypeOfNum,
			int const LoopNum,
			int const LoopSideNum,
			int const BranchNum,
			int const CompNum,
			int const FluidIndex, // used in Steam...
			Real64 const MaxVolWaterFlow,
			Real64 const MaxWaterMassFlow,
			Real64 const MinVolWaterFlow,
			Real64 const MinWaterMassFlow
		) :
			ComponentName( ComponentName ),
			ComponentType( ComponentType ),
			ComponentType_Num( ComponentType_Num ),
			ComponentIndex( ComponentIndex ),
			CoilAirInletNode( CoilAirInletNode ),
			CoilAirOutletNode( CoilAirOutletNode ),
			CoilWaterInletNode( CoilWaterInletNode ),
			CoilWaterOutletNode( CoilWaterOutletNode ),
			CoilPlantTypeOfNum( CoilPlantTypeOfNum ),
			LoopNum( LoopNum ),
			LoopSideNum( LoopSideNum ),
			BranchNum( BranchNum ),
			CompNum( CompNum ),
			FluidIndex( FluidIndex ),
			MaxVolWaterFlow( MaxVolWaterFlow ),
			MaxWaterMassFlow( MaxWaterMassFlow ),
			MinVolWaterFlow( MinVolWaterFlow ),
			MinWaterMassFlow( MinWaterMassFlow )
		{}

	};

	struct OAUnitData
	{
		// Members
		// Input data
		std::string Name; // name of unit
		std::string SchedName; // availability schedule
		int SchedPtr; // index to schedule
		std::string ZoneName; // Name of zone the system is serving
		int ZonePtr; // Point to this zone in the Zone derived type
		int ZoneNodeNum; // index of zone air node in node structure
		std::string UnitControlType; // Control type for the system
		// (Neutral and setpoint temperatrue)
		int ControlType; // Unit Control type indicator
		int AirInletNode; // inlet air node number
		int AirOutletNode; // outlet air node number
		std::string SFanName; // name of supply fan
		int SFan_Index; // index in fan structure
		int SFanType; // type of fan in cFanTypes
		int SFanAvailSchedPtr; // supply fan availability sched from fan object
		int FanPlace; // fan placement; blow through and draw through
		Real64 FanCorTemp; // correction temperature
		bool FanEffect; // .TRUE. if unit has a fan type of draw through
		int SFanOutletNode; // supply fan outlet node number
		std::string ExtFanName; // name of exhaust fan
		int ExtFan_Index; // index in fan structure
		int ExtFanType; // type of fan in cFanTypes
		int ExtFanAvailSchedPtr; // exhaust fan availability sched from fan object
		bool ExtFan; // true if there is an exhaust fan
		std::string OutAirSchedName; // schedule of fraction for outside air (all controls)
		int OutAirSchedPtr; // index to schedule
		int OutsideAirNode; // outside air node number
		Real64 OutAirVolFlow; // m3/s
		Real64 OutAirMassFlow; // kg/s
		Real64 ExtAirVolFlow; // m3/s
		Real64 ExtAirMassFlow; // kg/s
		std::string ExtAirSchedName; // schedule of fraction for exhaust air
		int ExtOutAirSchedPtr; // index to schedule
		Real64 MaxAirMassFlow; // kg/s
		std::string HiCtrlTempSched; // Schedule name for the High Control Air temperature
		int HiCtrlTempSchedPtr; // Schedule index for the High Control Air temperature
		std::string LoCtrlTempSched; // Schedule name for the Low Control Air temperature
		int LoCtrlTempSchedPtr; // Schedule index for the Low Control Air temperature
		int OperatingMode; // operating condition( NeutralMode, HeatingMode, CoolingMode)
		int ControlCompTypeNum;
		int CompErrIndex;
		Real64 AirMassFlow; // kg/s
		int UnBalancedErrCount; // Counter for recurring warning message
		int UnBalancedErrIndex; // Index to recurring warning message
		int NumComponents;
		std::string ComponentListName;
		Real64 CompOutSetTemp; // component outlet setpoint temperature
		int AvailStatus;
		std::string AvailManagerListName; // Name of an availability manager list object
		Array1D< OAEquipList > OAEquip;
		// Report data
		Real64 TotCoolingRate; // Rate of total cooling delivered to the zone [W]
		Real64 TotCoolingEnergy; // Total cooling energy delivered by the OAU supply air to the zone [J]
		Real64 SensCoolingRate; // Rate of sensible cooling delivered to the zone [W]
		Real64 SensCoolingEnergy; // Sensible cooling energy delivered by the OAU supply air to the zone [J]
		Real64 LatCoolingRate; // Rate of latent cooling delivered to the zone [W]
		Real64 LatCoolingEnergy; // Latent cooling energy delivered by the OAU supply air to the zone [J]
		Real64 ElecFanRate; // Total electric use rate (power) for supply/exhaust fans [W]
		Real64 ElecFanEnergy; // Electric energy use for supply fan and exhaust fan [J]
		Real64 SensHeatingEnergy; // sensible heating energy delivered by the ERV supply air to the zone [J]
		Real64 SensHeatingRate; // rate of sensible heating delivered to the zone [W]
		Real64 LatHeatingEnergy; // latent heating energy delivered by the ERV supply air to the zone [J]
		Real64 LatHeatingRate; // rate of latent heating delivered to the zone [W]
		Real64 TotHeatingEnergy; // total heating energy delivered by the ERV supply air to the zone [J]
		Real64 TotHeatingRate; // rate of total heating delivered to the zone [W]

		// Default Constructor
		OAUnitData() :
			SchedPtr( 0 ),
			ZonePtr( 0 ),
			ZoneNodeNum( 0 ),
			ControlType( 0 ),
			AirInletNode( 0 ),
			AirOutletNode( 0 ),
			SFan_Index( 0 ),
			SFanType( 0 ),
			SFanAvailSchedPtr( 0 ),
			FanPlace( 0 ),
			FanCorTemp( 0.0 ),
			FanEffect( false ),
			SFanOutletNode( 0 ),
			ExtFan_Index( 0 ),
			ExtFanType( 0 ),
			ExtFanAvailSchedPtr( 0 ),
			ExtFan( false ),
			OutAirSchedPtr( 0 ),
			OutsideAirNode( 0 ),
			OutAirVolFlow( 0.0 ),
			OutAirMassFlow( 0.0 ),
			ExtAirVolFlow( 0.0 ),
			ExtAirMassFlow( 0.0 ),
			ExtOutAirSchedPtr( 0 ),
			MaxAirMassFlow( 0.0 ),
			HiCtrlTempSchedPtr( 0 ),
			LoCtrlTempSchedPtr( 0 ),
			OperatingMode( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			AirMassFlow( 0.0 ),
			UnBalancedErrCount( 0 ),
			UnBalancedErrIndex( 0 ),
			NumComponents( 0 ),
			CompOutSetTemp( 0.0 ),
			AvailStatus( 0 ),
			TotCoolingRate( 0.0 ),
			TotCoolingEnergy( 0.0 ),
			SensCoolingRate( 0.0 ),
			SensCoolingEnergy( 0.0 ),
			LatCoolingRate( 0.0 ),
			LatCoolingEnergy( 0.0 ),
			ElecFanRate( 0.0 ),
			ElecFanEnergy( 0.0 ),
			SensHeatingEnergy( 0.0 ),
			SensHeatingRate( 0.0 ),
			LatHeatingEnergy( 0.0 ),
			LatHeatingRate( 0.0 ),
			TotHeatingEnergy( 0.0 ),
			TotHeatingRate( 0.0 )
		{}

		// Member Constructor
		OAUnitData(
			std::string const & Name, // name of unit
			std::string const & SchedName, // availability schedule
			int const SchedPtr, // index to schedule
			std::string const & ZoneName, // Name of zone the system is serving
			int const ZonePtr, // Point to this zone in the Zone derived type
			int const ZoneNodeNum, // index of zone air node in node structure
			std::string const & UnitControlType, // Control type for the system
			int const ControlType, // Unit Control type indicator
			int const AirInletNode, // inlet air node number
			int const AirOutletNode, // outlet air node number
			std::string const & SFanName, // name of supply fan
			int const SFan_Index, // index in fan structure
			int const SFanType, // type of fan in cFanTypes
			int const SFanAvailSchedPtr, // supply fan availability sched from fan object
			int const FanPlace, // fan placement; blow through and draw through
			Real64 const FanCorTemp, // correction temperature
			bool const FanEffect, // .TRUE. if unit has a fan type of draw through
			int const SFanOutletNode, // supply fan outlet node number
			std::string const & ExtFanName, // name of exhaust fan
			int const ExtFan_Index, // index in fan structure
			int const ExtFanType, // type of fan in cFanTypes
			int const ExtFanAvailSchedPtr, // exhaust fan availability sched from fan object
			bool const ExtFan, // true if there is an exhaust fan
			std::string const & OutAirSchedName, // schedule of fraction for outside air (all controls)
			int const OutAirSchedPtr, // index to schedule
			int const OutsideAirNode, // outside air node number
			Real64 const OutAirVolFlow, // m3/s
			Real64 const OutAirMassFlow, // kg/s
			Real64 const ExtAirVolFlow, // m3/s
			Real64 const ExtAirMassFlow, // kg/s
			std::string const & ExtAirSchedName, // schedule of fraction for exhaust air
			int const ExtOutAirSchedPtr, // index to schedule
			Real64 const MaxAirMassFlow, // kg/s
			std::string const & HiCtrlTempSched, // Schedule name for the High Control Air temperature
			int const HiCtrlTempSchedPtr, // Schedule index for the High Control Air temperature
			std::string const & LoCtrlTempSched, // Schedule name for the Low Control Air temperature
			int const LoCtrlTempSchedPtr, // Schedule index for the Low Control Air temperature
			int const OperatingMode, // operating condition( NeutralMode, HeatingMode, CoolingMode)
			int const ControlCompTypeNum,
			int const CompErrIndex,
			Real64 const AirMassFlow, // kg/s
			int const UnBalancedErrCount, // Counter for recurring warning message
			int const UnBalancedErrIndex, // Index to recurring warning message
			int const NumComponents,
			std::string const & ComponentListName,
			Real64 const CompOutSetTemp, // component outlet setpoint temperature
			int const AvailStatus,
			std::string const & AvailManagerListName, // Name of an availability manager list object
			Array1< OAEquipList > const & OAEquip,
			Real64 const TotCoolingRate, // Rate of total cooling delivered to the zone [W]
			Real64 const TotCoolingEnergy, // Total cooling energy delivered by the OAU supply air to the zone [J]
			Real64 const SensCoolingRate, // Rate of sensible cooling delivered to the zone [W]
			Real64 const SensCoolingEnergy, // Sensible cooling energy delivered by the OAU supply air to the zone [J]
			Real64 const LatCoolingRate, // Rate of latent cooling delivered to the zone [W]
			Real64 const LatCoolingEnergy, // Latent cooling energy delivered by the OAU supply air to the zone [J]
			Real64 const ElecFanRate, // Total electric use rate (power) for supply/exhaust fans [W]
			Real64 const ElecFanEnergy, // Electric energy use for supply fan and exhaust fan [J]
			Real64 const SensHeatingEnergy, // sensible heating energy delivered by the ERV supply air to the zone [J]
			Real64 const SensHeatingRate, // rate of sensible heating delivered to the zone [W]
			Real64 const LatHeatingEnergy, // latent heating energy delivered by the ERV supply air to the zone [J]
			Real64 const LatHeatingRate, // rate of latent heating delivered to the zone [W]
			Real64 const TotHeatingEnergy, // total heating energy delivered by the ERV supply air to the zone [J]
			Real64 const TotHeatingRate // rate of total heating delivered to the zone [W]
		) :
			Name( Name ),
			SchedName( SchedName ),
			SchedPtr( SchedPtr ),
			ZoneName( ZoneName ),
			ZonePtr( ZonePtr ),
			ZoneNodeNum( ZoneNodeNum ),
			UnitControlType( UnitControlType ),
			ControlType( ControlType ),
			AirInletNode( AirInletNode ),
			AirOutletNode( AirOutletNode ),
			SFanName( SFanName ),
			SFan_Index( SFan_Index ),
			SFanType( SFanType ),
			SFanAvailSchedPtr( SFanAvailSchedPtr ),
			FanPlace( FanPlace ),
			FanCorTemp( FanCorTemp ),
			FanEffect( FanEffect ),
			SFanOutletNode( SFanOutletNode ),
			ExtFanName( ExtFanName ),
			ExtFan_Index( ExtFan_Index ),
			ExtFanType( ExtFanType ),
			ExtFanAvailSchedPtr( ExtFanAvailSchedPtr ),
			ExtFan( ExtFan ),
			OutAirSchedName( OutAirSchedName ),
			OutAirSchedPtr( OutAirSchedPtr ),
			OutsideAirNode( OutsideAirNode ),
			OutAirVolFlow( OutAirVolFlow ),
			OutAirMassFlow( OutAirMassFlow ),
			ExtAirVolFlow( ExtAirVolFlow ),
			ExtAirMassFlow( ExtAirMassFlow ),
			ExtAirSchedName( ExtAirSchedName ),
			ExtOutAirSchedPtr( ExtOutAirSchedPtr ),
			MaxAirMassFlow( MaxAirMassFlow ),
			HiCtrlTempSched( HiCtrlTempSched ),
			HiCtrlTempSchedPtr( HiCtrlTempSchedPtr ),
			LoCtrlTempSched( LoCtrlTempSched ),
			LoCtrlTempSchedPtr( LoCtrlTempSchedPtr ),
			OperatingMode( OperatingMode ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			AirMassFlow( AirMassFlow ),
			UnBalancedErrCount( UnBalancedErrCount ),
			UnBalancedErrIndex( UnBalancedErrIndex ),
			NumComponents( NumComponents ),
			ComponentListName( ComponentListName ),
			CompOutSetTemp( CompOutSetTemp ),
			AvailStatus( AvailStatus ),
			AvailManagerListName( AvailManagerListName ),
			OAEquip( OAEquip ),
			TotCoolingRate( TotCoolingRate ),
			TotCoolingEnergy( TotCoolingEnergy ),
			SensCoolingRate( SensCoolingRate ),
			SensCoolingEnergy( SensCoolingEnergy ),
			LatCoolingRate( LatCoolingRate ),
			LatCoolingEnergy( LatCoolingEnergy ),
			ElecFanRate( ElecFanRate ),
			ElecFanEnergy( ElecFanEnergy ),
			SensHeatingEnergy( SensHeatingEnergy ),
			SensHeatingRate( SensHeatingRate ),
			LatHeatingEnergy( LatHeatingEnergy ),
			LatHeatingRate( LatHeatingRate ),
			TotHeatingEnergy( TotHeatingEnergy ),
			TotHeatingRate( TotHeatingRate )
		{}

	};

	// Object Data
	extern Array1D< OAUnitData > OutAirUnit;

	// Functions

	void
	SimOutdoorAirUnit(
		std::string const & CompName, // name of the outdoor air unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // Sensible power supplied (W)
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int & CompIndex
	);

	void
	GetOutdoorAirUnitInputs();

	void
	InitOutdoorAirUnit(
		int const OAUnitNum, // index for the current outdoor air unit
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	SizeOutdoorAirUnit( int const OAUnitNum );

	void
	CalcOutdoorAirUnit(
		int & OAUnitNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		bool const FirstHVACIteration, // TRUE if 1st HVAC simulation of system timestep
		Real64 & PowerMet, // power supplied
		Real64 & LatOutputProvided // Latent power supplied (kg/s), negative = dehumidification
	);

	void
	SimZoneOutAirUnitComps(
		int const OAUnitNum,
		bool const FirstHVACIteration
	);

	void
	SimOutdoorAirEquipComps(
		int const OAUnitNum, // actual outdoor air unit num
		std::string const & EquipType, // the component type
		std::string const & EquipName, // the component Name
		int const EquipNum,
		int const CompTypeNum, // Component Type -- Integerized for this module
		bool const FirstHVACIteration,
		int & CompIndex,
		bool const Sim // if TRUE, simulate component
	);

	void
	CalcOAUnitCoilComps(
		int const CompNum, // actual outdoor air unit num
		bool const FirstHVACIteration,
		int const EquipIndex, // Component Type -- Integerized for this module
		Real64 & LoadMet
	);

	//SUBROUTINE UpdateOutdoorAirUnit

	// No update routine needed in this module since all of the updates happen on
	// the Node derived type directly and these updates are done by other routines.

	//END SUBROUTINE UpdateOutdoorAirUnit

	void
	ReportOutdoorAirUnit( int const OAUnitNum ); // Index for the outdoor air unit under consideration within the derived types

	int
	GetOutdoorAirUnitOutAirNode( int const OAUnitNum );

	int
	GetOutdoorAirUnitZoneInletNode( int const OAUnitNum );

	int
	GetOutdoorAirUnitReturnAirNode( int const OAUnitNum );

	//*****************************************************************************************

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // OutdoorAirUnit

} // EnergyPlus

#endif
