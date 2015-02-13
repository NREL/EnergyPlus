#ifndef PoweredInductionUnits_hh_INCLUDED
#define PoweredInductionUnits_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace PoweredInductionUnits {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS
	extern int const SingleDuct_SeriesPIU_Reheat;
	extern int const SingleDuct_ParallelPIU_Reheat;
	// coil types in this module
	extern int const HCoilType_Gas;
	extern int const HCoilType_Electric;
	extern int const HCoilType_SimpleHeating;
	extern int const HCoilType_SteamAirHeating;

	// DERIVED TYPE DEFINITIONS

	// MODULE VARIABLE DECLARATIONS:
	extern Array1D_bool CheckEquipName;
	extern bool GetPIUInputFlag; // First time, input is "gotten"

	extern int NumPIUs;
	extern int NumSeriesPIUs;
	extern int NumParallelPIUs;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	// PRIVATE UpdatePIU

	// Types

	struct PowIndUnitData
	{
		// Members
		// input data
		std::string Name; // name of unit
		std::string UnitType; // type of unit
		int UnitType_Num; // index for type of unit
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		Real64 MaxTotAirVolFlow; // m3/s  (series)
		Real64 MaxTotAirMassFlow; // kg/s  (series)
		Real64 MaxPriAirVolFlow; // m3/s
		Real64 MaxPriAirMassFlow; // kg/s
		Real64 MinPriAirFlowFrac; // minimum primary air flow fraction
		Real64 MinPriAirMassFlow; // kg/s
		Real64 MaxSecAirVolFlow; // m3/s (parallel)
		Real64 MaxSecAirMassFlow; // kg/s (parallel)
		Real64 FanOnFlowFrac; // frac of primary air flow at which fan turns on (parallel)
		Real64 FanOnAirMassFlow; // primary air mass flow rate at which fan turns on (parallel)
		int PriAirInNode; // unit primary air inlet node number
		int SecAirInNode; // unit secondary air inlet node number
		int OutAirNode; // unit air outlet node number
		int HCoilInAirNode; // unit mixed air node number
		int ControlCompTypeNum;
		int CompErrIndex;
		std::string MixerName; // name of air mixer component
		int Mixer_Num; // index for type of mixer
		std::string FanName; // name of fan component
		int Fan_Num; // index for fan type
		int Fan_Index; // store index for this fan
		std::string HCoilType; // type of heating coil component
		int HCoilType_Num; // index for heating coil type
		int HCoil_PlantTypeNum;
		std::string HCoil; // name of heating coil component
		int HCoil_Index; // index to this heating coil
		int HCoil_FluidIndex;
		Real64 MaxVolHotWaterFlow; // m3/s
		Real64 MaxVolHotSteamFlow; // m3/s
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MaxHotSteamFlow; // kg/s
		Real64 MinVolHotWaterFlow; // m3/s
		Real64 MinHotSteamFlow; // kg/s
		Real64 MinVolHotSteamFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		int HotControlNode; // hot water control node
		int HotCoilOutNodeNum; // outlet of coil
		Real64 HotControlOffset; // control tolerance
		int HWLoopNum; // index for plant loop with hot plant coil
		int HWLoopSide; // index for plant loop side for hot plant coil
		int HWBranchNum; // index for plant branch for hot plant coil
		int HWCompNum; // index for plant component for hot plant coil
		int ADUNum; // index of corresponding air distribution unit
		bool InducesPlenumAir; // True if secondary air comes from the plenum
		// Report data
		Real64 HeatingRate; // unit heat addition rate to zone [W]
		Real64 HeatingEnergy; // unit heat addition to zone [J]
		Real64 SensCoolRate; // unit sensible heat removal rate from zone [W]
		Real64 SensCoolEnergy; // unit sensible heat removal from zone [J]

		// Default Constructor
		PowIndUnitData() :
			UnitType_Num( 0 ),
			SchedPtr( 0 ),
			MaxTotAirVolFlow( 0.0 ),
			MaxTotAirMassFlow( 0.0 ),
			MaxPriAirVolFlow( 0.0 ),
			MaxPriAirMassFlow( 0.0 ),
			MinPriAirFlowFrac( 0.0 ),
			MinPriAirMassFlow( 0.0 ),
			MaxSecAirVolFlow( 0.0 ),
			MaxSecAirMassFlow( 0.0 ),
			FanOnFlowFrac( 0.0 ),
			FanOnAirMassFlow( 0.0 ),
			PriAirInNode( 0 ),
			SecAirInNode( 0 ),
			OutAirNode( 0 ),
			HCoilInAirNode( 0 ),
			ControlCompTypeNum( 0 ),
			CompErrIndex( 0 ),
			Mixer_Num( 0 ),
			Fan_Num( 0 ),
			Fan_Index( 0 ),
			HCoilType_Num( 0 ),
			HCoil_PlantTypeNum( 0 ),
			HCoil_Index( 0 ),
			HCoil_FluidIndex( 0 ),
			MaxVolHotWaterFlow( 0.0 ),
			MaxVolHotSteamFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MaxHotSteamFlow( 0.0 ),
			MinVolHotWaterFlow( 0.0 ),
			MinHotSteamFlow( 0.0 ),
			MinVolHotSteamFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			HotControlNode( 0 ),
			HotCoilOutNodeNum( 0 ),
			HotControlOffset( 0.0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			ADUNum( 0 ),
			InducesPlenumAir( false ),
			HeatingRate( 0.0 ),
			HeatingEnergy( 0.0 ),
			SensCoolRate( 0.0 ),
			SensCoolEnergy( 0.0 )
		{}

		// Member Constructor
		PowIndUnitData(
			std::string const & Name, // name of unit
			std::string const & UnitType, // type of unit
			int const UnitType_Num, // index for type of unit
			std::string const & Sched, // availability schedule
			int const SchedPtr, // index to schedule
			Real64 const MaxTotAirVolFlow, // m3/s  (series)
			Real64 const MaxTotAirMassFlow, // kg/s  (series)
			Real64 const MaxPriAirVolFlow, // m3/s
			Real64 const MaxPriAirMassFlow, // kg/s
			Real64 const MinPriAirFlowFrac, // minimum primary air flow fraction
			Real64 const MinPriAirMassFlow, // kg/s
			Real64 const MaxSecAirVolFlow, // m3/s (parallel)
			Real64 const MaxSecAirMassFlow, // kg/s (parallel)
			Real64 const FanOnFlowFrac, // frac of primary air flow at which fan turns on (parallel)
			Real64 const FanOnAirMassFlow, // primary air mass flow rate at which fan turns on (parallel)
			int const PriAirInNode, // unit primary air inlet node number
			int const SecAirInNode, // unit secondary air inlet node number
			int const OutAirNode, // unit air outlet node number
			int const HCoilInAirNode, // unit mixed air node number
			int const ControlCompTypeNum,
			int const CompErrIndex,
			std::string const & MixerName, // name of air mixer component
			int const Mixer_Num, // index for type of mixer
			std::string const & FanName, // name of fan component
			int const Fan_Num, // index for fan type
			int const Fan_Index, // store index for this fan
			std::string const & HCoilType, // type of heating coil component
			int const HCoilType_Num, // index for heating coil type
			int const HCoil_PlantTypeNum,
			std::string const & HCoil, // name of heating coil component
			int const HCoil_Index, // index to this heating coil
			int const HCoil_FluidIndex,
			Real64 const MaxVolHotWaterFlow, // m3/s
			Real64 const MaxVolHotSteamFlow, // m3/s
			Real64 const MaxHotWaterFlow, // kg/s
			Real64 const MaxHotSteamFlow, // kg/s
			Real64 const MinVolHotWaterFlow, // m3/s
			Real64 const MinHotSteamFlow, // kg/s
			Real64 const MinVolHotSteamFlow, // m3/s
			Real64 const MinHotWaterFlow, // kg/s
			int const HotControlNode, // hot water control node
			int const HotCoilOutNodeNum, // outlet of coil
			Real64 const HotControlOffset, // control tolerance
			int const HWLoopNum, // index for plant loop with hot plant coil
			int const HWLoopSide, // index for plant loop side for hot plant coil
			int const HWBranchNum, // index for plant branch for hot plant coil
			int const HWCompNum, // index for plant component for hot plant coil
			int const ADUNum, // index of corresponding air distribution unit
			bool const InducesPlenumAir, // True if secondary air comes from the plenum
			Real64 const HeatingRate, // unit heat addition rate to zone [W]
			Real64 const HeatingEnergy, // unit heat addition to zone [J]
			Real64 const SensCoolRate, // unit sensible heat removal rate from zone [W]
			Real64 const SensCoolEnergy // unit sensible heat removal from zone [J]
		) :
			Name( Name ),
			UnitType( UnitType ),
			UnitType_Num( UnitType_Num ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			MaxTotAirVolFlow( MaxTotAirVolFlow ),
			MaxTotAirMassFlow( MaxTotAirMassFlow ),
			MaxPriAirVolFlow( MaxPriAirVolFlow ),
			MaxPriAirMassFlow( MaxPriAirMassFlow ),
			MinPriAirFlowFrac( MinPriAirFlowFrac ),
			MinPriAirMassFlow( MinPriAirMassFlow ),
			MaxSecAirVolFlow( MaxSecAirVolFlow ),
			MaxSecAirMassFlow( MaxSecAirMassFlow ),
			FanOnFlowFrac( FanOnFlowFrac ),
			FanOnAirMassFlow( FanOnAirMassFlow ),
			PriAirInNode( PriAirInNode ),
			SecAirInNode( SecAirInNode ),
			OutAirNode( OutAirNode ),
			HCoilInAirNode( HCoilInAirNode ),
			ControlCompTypeNum( ControlCompTypeNum ),
			CompErrIndex( CompErrIndex ),
			MixerName( MixerName ),
			Mixer_Num( Mixer_Num ),
			FanName( FanName ),
			Fan_Num( Fan_Num ),
			Fan_Index( Fan_Index ),
			HCoilType( HCoilType ),
			HCoilType_Num( HCoilType_Num ),
			HCoil_PlantTypeNum( HCoil_PlantTypeNum ),
			HCoil( HCoil ),
			HCoil_Index( HCoil_Index ),
			HCoil_FluidIndex( HCoil_FluidIndex ),
			MaxVolHotWaterFlow( MaxVolHotWaterFlow ),
			MaxVolHotSteamFlow( MaxVolHotSteamFlow ),
			MaxHotWaterFlow( MaxHotWaterFlow ),
			MaxHotSteamFlow( MaxHotSteamFlow ),
			MinVolHotWaterFlow( MinVolHotWaterFlow ),
			MinHotSteamFlow( MinHotSteamFlow ),
			MinVolHotSteamFlow( MinVolHotSteamFlow ),
			MinHotWaterFlow( MinHotWaterFlow ),
			HotControlNode( HotControlNode ),
			HotCoilOutNodeNum( HotCoilOutNodeNum ),
			HotControlOffset( HotControlOffset ),
			HWLoopNum( HWLoopNum ),
			HWLoopSide( HWLoopSide ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			ADUNum( ADUNum ),
			InducesPlenumAir( InducesPlenumAir ),
			HeatingRate( HeatingRate ),
			HeatingEnergy( HeatingEnergy ),
			SensCoolRate( SensCoolRate ),
			SensCoolEnergy( SensCoolEnergy )
		{}

	};

	// Object Data
	extern Array1D< PowIndUnitData > PIU;

	// Functions

	void
	SimPIU(
		std::string const & CompName, // name of the PIU
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by PIU
		int const ZoneNodeNum, // zone node number of zone served by PIU
		int & CompIndex // PIU Index in PIU names
	);

	void
	GetPIUs();

	void
	InitPIU(
		int const PIUNum, // number of the current fan coil unit being simulated
		bool const FirstHVACIteration // TRUE if first zone equip this HVAC step
	);

	void
	SizePIU( int const PIUNum );

	void
	CalcSeriesPIU(
		int const PIUNum, // number of the current PIU being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNode, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	CalcParallelPIU(
		int const PIUNum, // number of the current PIU being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNode, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	ReportPIU( int const PIUNum ); // number of the current fan coil unit being simulated

	// ===================== Utilities =====================================

	bool
	PIUnitHasMixer( std::string const & CompName ); // component (mixer) name

	void
	PIUInducesPlenumAir( int const NodeNum ); // induced air node number

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

} // PoweredInductionUnits

} // EnergyPlus

#endif
