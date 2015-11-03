// EnergyPlus Headers
#include <DataBranchAirLoopPlant.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataBranchAirLoopPlant {

	// Module containing the routines dealing with the <module_name>

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   November 2011
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// Certain data needs to be shared from Branch to Airloop to Plant and this module should
	// alleviate cyclic dependencies.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// <use statements for data only modules>
	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// Parameters for tolerance
	Real64 const MassFlowTolerance( 0.000000001 ); // minimum significant mass flow rate (kg/s)

	// Pressure Curve Type: None, pressure, or generic curve (if generic it will be a postive value which is the curve manager index)
	int const PressureCurve_Error( -1 );
	int const PressureCurve_None( 0 );
	int const PressureCurve_Pressure( 1 );
	int const PressureCurve_Generic( 2 );

	// Parameters for flow Control Types for branch flow resolution inside splitter/mixers
	int const ControlType_Unknown( 0 );
	int const ControlType_Active( 1 ); // 'Active'
	int const ControlType_Passive( 2 ); // 'Passive'
	int const ControlType_SeriesActive( 3 ); // 'SeriesActive'
	int const ControlType_Bypass( 4 ); // 'Bypass
	Array1D_string const cControlType( {0,4}, { "Unknown", "Active", "Passive", "SeriesActive", "Bypass" } );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumPressureCurves( 0 );

	// Object Data
	Array1D< PlantPressureCurveData > PressureCurve;

	// SUBROUTINE SPECIFICATIONS FOR MODULE

	//=================================================================================================!

} // DataBranchAirLoopPlant

} // EnergyPlus
