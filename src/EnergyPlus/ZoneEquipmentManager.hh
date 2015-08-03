#ifndef ZoneEquipmentManager_hh_INCLUDED
#define ZoneEquipmentManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>

namespace EnergyPlus {

namespace ZoneEquipmentManager {

	// Using/Aliasing

	// Data
	//MODULE PARAMETER DEFINITIONS
	// na

	// DERIVED TYPE DEFINITIONS

	//MODULE VARIABLE DECLARATIONS:
	extern Array1D< Real64 > AvgData; // scratch array for storing averaged data
	extern Array1D_int DefaultSimOrder;
	extern int NumOfTimeStepInDay; // number of zone time steps in a day
	extern bool GetZoneEquipmentInputFlag;

	//SUBROUTINE SPECIFICATIONS FOR MODULE ZoneEquipmentManager

	// Types

	struct SimulationOrder
	{
		// Members
		std::string EquipType;
		int EquipType_Num;
		std::string EquipName;
		int EquipPtr;
		int CoolingPriority;
		int HeatingPriority;

		// Default Constructor
		SimulationOrder() :
			EquipType_Num( 0 ),
			EquipPtr( 0 ),
			CoolingPriority( 0 ),
			HeatingPriority( 0 )
		{}

		// Member Constructor
		SimulationOrder(
			std::string const & EquipType,
			int const EquipType_Num,
			std::string const & EquipName,
			int const EquipPtr,
			int const CoolingPriority,
			int const HeatingPriority
		) :
			EquipType( EquipType ),
			EquipType_Num( EquipType_Num ),
			EquipName( EquipName ),
			EquipPtr( EquipPtr ),
			CoolingPriority( CoolingPriority ),
			HeatingPriority( HeatingPriority )
		{}

	};

	// Object Data
	extern Array1D< SimulationOrder > PrioritySimOrder;

	// Functions

	void
	ManageZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimZone,
		bool & SimAir
	);

	void
	GetZoneEquipment();

	void
	InitZoneEquipment( bool const FirstHVACIteration ); // unused 1208

	void
	SizeZoneEquipment();

	void
	SetUpZoneSizingArrays();

	void
	RezeroZoneSizingArrays();

	void
	UpdateZoneSizing( int const CallIndicator );

	void
	SimZoneEquipment(
		bool const FirstHVACIteration,
		bool & SimAir
	);

	void
	SetZoneEquipSimOrder(
		int const ControlledZoneNum,
		int const ActualZoneNum
	);

	void
	InitSystemOutputRequired(
		int const ZoneNum,
		Real64 & SysOutputProvided,
		Real64 & LatOutputProvided
	);

	void
	UpdateSystemOutputRequired(
		int const ZoneNum,
		Real64 const SysOutputProvided, // sensible output provided by zone equipment (W)
		Real64 const LatOutputProvided, // latent output provided by zone equipment (kg/s)
		Optional_int_const EquipPriorityNum = _ // index in PrioritySimOrder for this update
	);

	void
	CalcZoneMassBalance();

	void
	CalcAirFlowSimple(
		int const SysTimestepLoop = 0, // System time step index
		bool const AdjustZoneMixingFlowFlag = false // flags to adjust zone mxing mass flow rate
	);

	void
	GetStandAloneERVNodes(int const OutdoorNum); // Zone Air Balance Outdoor index

	void
	CalcZoneMixingFlowRateOfReceivingZone(
		int const ZoneNum,
		Real64 & ZoneMixingAirMassFlowRate
		);

	void
	CalcZoneMixingFlowRateOfSourceZone(int const ZoneNum);

	void
	CalcZoneLeavingConditions();

	void
	UpdateZoneEquipment( bool & SimAir );

	void
	ReportZoneEquipment();

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

} // ZoneEquipmentManager

} // EnergyPlus

#endif
