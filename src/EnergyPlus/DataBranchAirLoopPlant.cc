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

} // DataBranchAirLoopPlant

} // EnergyPlus
