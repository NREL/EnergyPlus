// EnergyPlus Headers
#include <DataZoneControls.hh>
#include <DataPrecisionGlobals.hh>

namespace EnergyPlus {

namespace DataZoneControls {

	// Module containing the routines dealing with the zone controls.

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   October 2007
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This module has the data and structures for various types of controls
	// (humidity, temperature, comfort) within the zones.  This data was formerly
	// public data in ZoneTempPredictorCorrector.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	// <use statements for access to subroutines in other modules>

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	int NumTempControlledZones( 0 );
	int NumHumidityControlZones( 0 );
	int NumComfortControlledZones( 0 );
	int NumTStatStatements( 0 );
	int NumComfortTStatStatements( 0 );
	int NumOpTempControlledZones( 0 ); // number of zones with operative temp control
	int NumTempAndHumidityControlledZones( 0 ); // number of zones with over cool control
	bool AnyOpTempControl( false ); // flag set true if any zones have op temp control
	bool AnyZoneTempAndHumidityControl( false ); // flag set true if any zones have over cool control
	FArray1D_bool StageZoneLogic; // Logical array, A zone with staged thermostat = .TRUE.
	FArray1D< Real64 > OccRoomTSetPointHeat; // occupied heating set point for optimum start period
	FArray1D< Real64 > OccRoomTSetPointCool; // occupied cooling set point for optimum start period
	bool GetZoneAirStatsInputFlag( true ); // True when need to get input

	// Object Data
	FArray1D< ZoneHumidityControls > HumidityControlZone;
	FArray1D< ZoneTempControls > TempControlledZone;
	FArray1D< ZoneComfortControls > ComfortControlledZone;
	FArray1D< TStatObject > TStatObjects;
	FArray1D< TStatObject > ComfortTStatObjects;
	FArray1D< TStatObject > StagedTStatObjects;
	FArray1D< ZoneStagedControls > StageControlledZone;

	//     NOTICE
	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // DataZoneControls

} // EnergyPlus
