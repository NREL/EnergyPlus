// EnergyPlus Headers
#include <DataDaylighting.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataDaylighting {

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie/Fred Winkelmann (Re-engineered by Peter Graham Ellis)
	//       DATE WRITTEN   May 1998
	//       MODIFIED       B.Griffith added interior window data for associated exterior windows
	//       RE-ENGINEERED  April 2003

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for variables used in daylighting which
	// are shared by several modules.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	// Two kinds of reference points: used directly in daylighting, used to show illuminance map of zone
	int const MaxRefPoints( 2 ); // Maximum number of daylighting reference points, 2
	int const MaxMapRefPoints( 2500 ); // Maximum number of Illuminance Map Ref Points

	int const NotInOrAdjZoneExtWin( 0 ); // Exterior window is not in a Daylighting:Detailed zone
	// or in an adjacent zone with a shared interior window
	int const InZoneExtWin( 1 ); // Exterior window is in a Daylighting:Detailed zone
	int const AdjZoneExtWin( 2 ); // Exterior window is in a zone adjacent to a Daylighting:
	// Detailed zone with which it shares an interior window

	int const CalledForRefPoint( 101 );
	int const CalledForMapPoint( 102 );

	// Parameters for "DaylightType"
	int const NoDaylighting( 0 );
	int const DetailedDaylighting( 1 );
	int const DElightDaylighting( 2 );
	Array1D_string const DaylightTypes( 2, { "Daylighting:Controls", "Daylighting:DELight:Controls" } );

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE TYPE DECLARATIONS:

	// INTERFACE BLOCK SPECIFICATIONS: na

	// MODULE VARIABLE DECLARATIONS:
	int TotIllumMaps( 0 );
	bool mapResultsToReport( false ); // used when only partial hour has "sun up"
	bool mapResultsReported( false ); // when no map results are ever reported this will still be false
	char MapColSep; // Character for separating map columns (tab, space, comma)

	bool DFSReportSizingDays( false );
	bool DFSReportAllShadowCalculationDays( false );

	// Object Data
	Array1D< ZoneDaylightCalc > ZoneDaylight;
	Array1D< IllumMapData > IllumMap;
	Array1D< MapCalcData > IllumMapCalc;

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

} // DataDaylighting

} // EnergyPlus
