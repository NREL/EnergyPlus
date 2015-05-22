// EnergyPlus Headers
#include <DataAirSystems.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataAirSystems {

	// MODULE INFORMATION:
	//       AUTHOR         Plant code authors?
	//       DATE WRITTEN
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module contains the structures for various parts of the Plant and
	// Condenser Loops.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES: none

	// OTHER NOTES: none

	// USE STATEMENTS:
	// Use statements for data only modules (only modules that should be used here and sparingly)
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using namespace DataPlant;

	// Data
	//MODULE PARAMETER DEFINITIONS:
	// DERIVED TYPE DEFINITIONS

	// DefinePrimaryAirSystem contains the data for a primary air HVAC system

	// The ConnectionPoint derived type is used to link quickly between loops at connection points
	// and avoids the need for repetitive searches.

	// INTERFACE BLOCK SPECIFICATIONS
	// None

	// MODULE VARIABLE DECLARATIONS
	// For each type of air path, define an array of DefineAirPaths

	// Temporary arrays

	// Object Data
	Array1D< DefinePrimaryAirSystem > PrimaryAirSystem;
	Array1D< ConnectionPoint > DemandSideConnect; // Connections between loops
	Array1D< ConnectZoneComp > ZoneCompToPlant; // Connections between loops
	Array1D< ConnectZoneSubComp > ZoneSubCompToPlant; // Connections between loops
	Array1D< ConnectZoneSubSubComp > ZoneSubSubCompToPlant; // Connections between loops
	Array1D< ConnectAirSysComp > AirSysCompToPlant; // Connections between loops
	Array1D< ConnectAirSysSubComp > AirSysSubCompToPlant; // Connections between loops
	Array1D< ConnectAirSysSubSubComp > AirSysSubSubCompToPlant; // Connections between loops

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

} // DataAirSystems

} // EnergyPlus
