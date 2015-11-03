#ifndef ZoneAirLoopEquipmentManager_hh_INCLUDED
#define ZoneAirLoopEquipmentManager_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ZoneAirLoopEquipmentManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	extern bool GetAirDistUnitsFlag; // If TRUE, Air Distribution Data has not been read in yet
	extern bool MyOneTimeFlag;

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE ZoneAirLoopEquipmentManager

	// Functions
	void
	clear_state();

	void
	ManageZoneAirLoopEquipment(
		std::string const & ZoneAirLoopEquipName,
		bool const FirstHVACIteration,
		Real64 & SysOutputProvided,
		Real64 & NonAirSysOutput,
		Real64 & LatOutputProvided, // Latent add/removal supplied by window AC (kg/s), dehumid = negative
		int const ActualZoneNum,
		int & ControlledZoneNum,
		int & CompIndex
	);

	void
	GetZoneAirLoopEquipment();

	void
	InitZoneAirLoopEquipment(
		bool const FirstHVACIteration, // unused1208
		int const AirDistUnitNum,
		int const ZoneNum
	);

	void
	SimZoneAirLoopEquipment(
		int const AirDistUnitNum,
		Real64 & SysOutputProvided,
		Real64 & NonAirSysOutput,
		Real64 & LatOutputProvided, // Latent add/removal provided by this unit (kg/s), dehumidify = negative
		bool const FirstHVACIteration,
		int const ControlledZoneNum,
		int const ActualZoneNum
	);

	void
	UpdateZoneAirLoopEquipment();

	// void
	// ReportZoneAirLoopEquipment(
	// 	int const AirDistUnitNum
	// );

} // ZoneAirLoopEquipmentManager

} // EnergyPlus

#endif
