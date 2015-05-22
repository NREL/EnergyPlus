#ifndef HVACSingleDuctInduc_hh_INCLUDED
#define HVACSingleDuctInduc_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace HVACSingleDuctInduc {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern int const SingleDuct_CV_FourPipeInduc;
	extern int const SingleDuct_CV_2PipeInduc;
	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:

	extern int NumIndUnits;
	extern int NumFourPipes;
	extern Array1D_bool CheckEquipName;
	extern bool GetIUInputFlag; // First time, input is "gotten"

	// SUBROUTINE SPECIFICATIONS FOR MODULE HVACSingleDuctInduc:

	// PRIVATE UpdateIndUnit
	// PRIVATE ReportIndUnit

	// Types

	struct IndUnitData
	{
		// Members
		// input data
		std::string Name; // name of unit
		std::string UnitType; // type of unit
		int UnitType_Num; // index to type of unit
		std::string Sched; // availability schedule
		int SchedPtr; // index to schedule
		Real64 MaxTotAirVolFlow; // m3/s (autosizable)
		Real64 MaxTotAirMassFlow; // kg/s
		Real64 InducRatio; // ratio of induced air flow to primary air flow
		int PriAirInNode; // unit primary air inlet node number
		int SecAirInNode; // unit induced air inlet node number
		int OutAirNode; // unit air outlet node number
		int HWControlNode; // hot water control node
		int CWControlNode; // cold water control node
		std::string HCoilType; // type of heating coil component
		std::string HCoil; // name of heating coil component
		int HCoil_Num; // index to this coil
		int HCoil_PlantTypeNum;
		Real64 MaxVolHotWaterFlow; // m3/s (autosizable)
		Real64 MaxHotWaterFlow; // kg/s
		Real64 MinVolHotWaterFlow; // m3/s
		Real64 MinHotWaterFlow; // kg/s
		Real64 HotControlOffset; // control tolerance
		int HWLoopNum; // index for plant loop with hot water coil
		int HWLoopSide; // index for plant loop side for hot water coil
		int HWBranchNum; // index for plant branch for hot water coil
		int HWCompNum; // index for plant component for hot water coil
		int HWCoilFailNum1; // index for errors
		int HWCoilFailNum2; // index for errors
		std::string CCoilType; // type of cooling coil component
		std::string CCoil; // name of cooling coil component
		int CCoil_Num; // index to this coil
		int CCoil_PlantTypeNum;
		Real64 MaxVolColdWaterFlow; // m3/s (autosizable)
		Real64 MaxColdWaterFlow; // kg/s
		Real64 MinVolColdWaterFlow; // m3/s
		Real64 MinColdWaterFlow; // kg/s
		Real64 ColdControlOffset; // control tolerance
		int CWLoopNum; // index for plant loop with chilled water coil
		int CWLoopSide; // index for plant loop side for chilled water coil
		int CWBranchNum; // index for plant branch for chilled water coil
		int CWCompNum; // index for plant component for chilled water coil
		int CWCoilFailNum1; // index for errors
		int CWCoilFailNum2; // index for errors
		std::string MixerName; // name of air mixer component
		int Mixer_Num; // index to this mixer
		Real64 MaxPriAirMassFlow; // kg/s
		Real64 MaxSecAirMassFlow; // kg/s
		int ADUNum; // index of corresponding air distribution unit
		Real64 DesCoolingLoad; // used for reporting during coil sizing
		Real64 DesHeatingLoad; // used for reporting during coil sizing

		// Default Constructor
		IndUnitData() :
			UnitType_Num( 0 ),
			SchedPtr( 0 ),
			MaxTotAirVolFlow( 0.0 ),
			MaxTotAirMassFlow( 0.0 ),
			InducRatio( 2.5 ),
			PriAirInNode( 0 ),
			SecAirInNode( 0 ),
			OutAirNode( 0 ),
			HWControlNode( 0 ),
			CWControlNode( 0 ),
			HCoil_Num( 0 ),
			HCoil_PlantTypeNum( 0 ),
			MaxVolHotWaterFlow( 0.0 ),
			MaxHotWaterFlow( 0.0 ),
			MinVolHotWaterFlow( 0.0 ),
			MinHotWaterFlow( 0.0 ),
			HotControlOffset( 0.0 ),
			HWLoopNum( 0 ),
			HWLoopSide( 0 ),
			HWBranchNum( 0 ),
			HWCompNum( 0 ),
			HWCoilFailNum1( 0 ),
			HWCoilFailNum2( 0 ),
			CCoil_Num( 0 ),
			CCoil_PlantTypeNum( 0 ),
			MaxVolColdWaterFlow( 0.0 ),
			MaxColdWaterFlow( 0.0 ),
			MinVolColdWaterFlow( 0.0 ),
			MinColdWaterFlow( 0.0 ),
			ColdControlOffset( 0.0 ),
			CWLoopNum( 0 ),
			CWLoopSide( 0 ),
			CWBranchNum( 0 ),
			CWCompNum( 0 ),
			CWCoilFailNum1( 0 ),
			CWCoilFailNum2( 0 ),
			Mixer_Num( 0 ),
			MaxPriAirMassFlow( 0.0 ),
			MaxSecAirMassFlow( 0.0 ),
			ADUNum( 0 ),
			DesCoolingLoad( 0.0 ),
			DesHeatingLoad( 0.0 )
		{}

		// Member Constructor
		IndUnitData(
			std::string const & Name, // name of unit
			std::string const & UnitType, // type of unit
			int const UnitType_Num, // index to type of unit
			std::string const & Sched, // availability schedule
			int const SchedPtr, // index to schedule
			Real64 const MaxTotAirVolFlow, // m3/s (autosizable)
			Real64 const MaxTotAirMassFlow, // kg/s
			Real64 const InducRatio, // ratio of induced air flow to primary air flow
			int const PriAirInNode, // unit primary air inlet node number
			int const SecAirInNode, // unit induced air inlet node number
			int const OutAirNode, // unit air outlet node number
			int const HWControlNode, // hot water control node
			int const CWControlNode, // cold water control node
			std::string const & HCoilType, // type of heating coil component
			std::string const & HCoil, // name of heating coil component
			int const HCoil_Num, // index to this coil
			int const HCoil_PlantTypeNum,
			Real64 const MaxVolHotWaterFlow, // m3/s (autosizable)
			Real64 const MaxHotWaterFlow, // kg/s
			Real64 const MinVolHotWaterFlow, // m3/s
			Real64 const MinHotWaterFlow, // kg/s
			Real64 const HotControlOffset, // control tolerance
			int const HWLoopNum, // index for plant loop with hot water coil
			int const HWLoopSide, // index for plant loop side for hot water coil
			int const HWBranchNum, // index for plant branch for hot water coil
			int const HWCompNum, // index for plant component for hot water coil
			int const HWCoilFailNum1, // index for errors
			int const HWCoilFailNum2, // index for errors
			std::string const & CCoilType, // type of cooling coil component
			std::string const & CCoil, // name of cooling coil component
			int const CCoil_Num, // index to this coil
			int const CCoil_PlantTypeNum,
			Real64 const MaxVolColdWaterFlow, // m3/s (autosizable)
			Real64 const MaxColdWaterFlow, // kg/s
			Real64 const MinVolColdWaterFlow, // m3/s
			Real64 const MinColdWaterFlow, // kg/s
			Real64 const ColdControlOffset, // control tolerance
			int const CWLoopNum, // index for plant loop with chilled water coil
			int const CWLoopSide, // index for plant loop side for chilled water coil
			int const CWBranchNum, // index for plant branch for chilled water coil
			int const CWCompNum, // index for plant component for chilled water coil
			int const CWCoilFailNum1, // index for errors
			int const CWCoilFailNum2, // index for errors
			std::string const & MixerName, // name of air mixer component
			int const Mixer_Num, // index to this mixer
			Real64 const MaxPriAirMassFlow, // kg/s
			Real64 const MaxSecAirMassFlow, // kg/s
			int const ADUNum, // index of corresponding air distribution unit
			Real64 const DesCoolingLoad, // used for reporting during coil sizing
			Real64 const DesHeatingLoad // used for reporting during coil sizing
		) :
			Name( Name ),
			UnitType( UnitType ),
			UnitType_Num( UnitType_Num ),
			Sched( Sched ),
			SchedPtr( SchedPtr ),
			MaxTotAirVolFlow( MaxTotAirVolFlow ),
			MaxTotAirMassFlow( MaxTotAirMassFlow ),
			InducRatio( InducRatio ),
			PriAirInNode( PriAirInNode ),
			SecAirInNode( SecAirInNode ),
			OutAirNode( OutAirNode ),
			HWControlNode( HWControlNode ),
			CWControlNode( CWControlNode ),
			HCoilType( HCoilType ),
			HCoil( HCoil ),
			HCoil_Num( HCoil_Num ),
			HCoil_PlantTypeNum( HCoil_PlantTypeNum ),
			MaxVolHotWaterFlow( MaxVolHotWaterFlow ),
			MaxHotWaterFlow( MaxHotWaterFlow ),
			MinVolHotWaterFlow( MinVolHotWaterFlow ),
			MinHotWaterFlow( MinHotWaterFlow ),
			HotControlOffset( HotControlOffset ),
			HWLoopNum( HWLoopNum ),
			HWLoopSide( HWLoopSide ),
			HWBranchNum( HWBranchNum ),
			HWCompNum( HWCompNum ),
			HWCoilFailNum1( HWCoilFailNum1 ),
			HWCoilFailNum2( HWCoilFailNum2 ),
			CCoilType( CCoilType ),
			CCoil( CCoil ),
			CCoil_Num( CCoil_Num ),
			CCoil_PlantTypeNum( CCoil_PlantTypeNum ),
			MaxVolColdWaterFlow( MaxVolColdWaterFlow ),
			MaxColdWaterFlow( MaxColdWaterFlow ),
			MinVolColdWaterFlow( MinVolColdWaterFlow ),
			MinColdWaterFlow( MinColdWaterFlow ),
			ColdControlOffset( ColdControlOffset ),
			CWLoopNum( CWLoopNum ),
			CWLoopSide( CWLoopSide ),
			CWBranchNum( CWBranchNum ),
			CWCompNum( CWCompNum ),
			CWCoilFailNum1( CWCoilFailNum1 ),
			CWCoilFailNum2( CWCoilFailNum2 ),
			MixerName( MixerName ),
			Mixer_Num( Mixer_Num ),
			MaxPriAirMassFlow( MaxPriAirMassFlow ),
			MaxSecAirMassFlow( MaxSecAirMassFlow ),
			ADUNum( ADUNum ),
			DesCoolingLoad( DesCoolingLoad ),
			DesHeatingLoad( DesHeatingLoad )
		{}

	};

	// Object Data
	extern Array1D< IndUnitData > IndUnit;

	// Functions

	void
	SimIndUnit(
		std::string const & CompName, // name of the terminal unit
		bool const FirstHVACIteration, // TRUE if first HVAC iteration in time step
		int const ZoneNum, // index of zone served by the terminal unit
		int const ZoneNodeNum, // zone node number of zone served by the terminal unit
		int & CompIndex // which terminal unit in data structure
	);

	void
	GetIndUnits();

	void
	InitIndUnit(
		int const IUNum, // number of the current induction unit being simulated
		bool const FirstHVACIteration // TRUE if first air loop solution this HVAC step
	);

	void
	SizeIndUnit( int const IUNum );

	void
	SimFourPipeIndUnit(
		int const IUNum, // number of the current unit being simulated
		int const ZoneNum, // number of zone being served
		int const ZoneNodeNum, // zone node number
		bool const FirstHVACIteration // TRUE if 1st HVAC simulation of system timestep
	);

	void
	CalcFourPipeIndUnit(
		int const IUNum, // Unit index
		bool const FirstHVACIteration, // flag for 1st HVAV iteration in the time step
		int const ZoneNode, // zone node number
		Real64 const HWFlow, // hot water flow (kg/s)
		Real64 const CWFlow, // cold water flow (kg/s)
		Real64 & LoadMet // load met by unit (watts)
	);

	Real64
	FourPipeIUHeatingResidual(
		Real64 const HWFlow, // hot water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested zone load
	);

	Real64
	FourPipeIUCoolingResidual(
		Real64 const CWFlow, // cold water flow rate in kg/s
		Array1< Real64 > const & Par // Par(5) is the requested zone load
	);

	// ========================= Utilities =======================

	bool
	FourPipeInductionUnitHasMixer( std::string const & CompName ); // component (mixer) name

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

} // HVACSingleDuctInduc

} // EnergyPlus

#endif
