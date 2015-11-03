// EnergyPlus Headers
#include <DataZoneEnergyDemands.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataZoneEnergyDemands {

	// MODULE INFORMATION
	//             AUTHOR:  Russ Taylor
	//       DATE WRITTEN:  Oct 1997

	// PURPOSE OF THIS MODULE:
	// This module  contains the essential coil information that is needed by water and air
	//loop managers as well as the coil simulations

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE VARIABLE DECLARATIONS:

	Array1D_bool DeadBandOrSetback; // true if zone temperature is in the thermostat deadband
	// before any heating / cooling done
	Array1D_bool Setback; // true if zone temperature has increased
	// from previous setting
	Array1D_bool CurDeadBandOrSetback; // same as above except updated after each piece of zone equipment
	// in a zone is simulated

	// Object Data
	Array1D< ZoneSystemDemandData > ZoneSysEnergyDemand;
	Array1D< ZoneSystemMoistureDemand > ZoneSysMoistureDemand;

	void
	clear_state()
	{
		DeadBandOrSetback.deallocate();
		Setback.deallocate();
		CurDeadBandOrSetback.deallocate();
		ZoneSysEnergyDemand.deallocate();
		ZoneSysMoistureDemand.deallocate();
	
	}

} // DataZoneEnergyDemands

} // EnergyPlus
