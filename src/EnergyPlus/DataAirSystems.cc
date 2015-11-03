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

	// Functions
	void
	clear_state(){
	
		PrimaryAirSystem.deallocate();
		DemandSideConnect.deallocate(); // Connections between loops
		ZoneCompToPlant.deallocate(); // Connections between loops
		ZoneSubCompToPlant.deallocate(); // Connections between loops
		ZoneSubSubCompToPlant.deallocate(); // Connections between loops
		AirSysCompToPlant.deallocate(); // Connections between loops
		AirSysSubCompToPlant.deallocate(); // Connections between loops
		AirSysSubSubCompToPlant.deallocate(); // Connections 
	}

} // DataAirSystems

} // EnergyPlus
