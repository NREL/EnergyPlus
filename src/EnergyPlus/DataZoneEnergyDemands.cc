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

} // DataZoneEnergyDemands

} // EnergyPlus
