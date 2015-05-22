// EnergyPlus Headers
#include <DataHVACControllers.hh>

namespace EnergyPlus {

namespace DataHVACControllers {

	// MODULE INFORMATION:
	//       AUTHOR         Dimitri Curtil
	//       DATE WRITTEN   February 2006
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for all variables used by the HVAC controllers.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:
	// None!--This module is USEd by all other modules; it should not USE anything.

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	int const ControllerSimple_Type( 1 );
	Array1D_string const ControllerTypes( 1, std::string( "Controller:WaterCoil" ) );

	// Controller action used in modules HVACControllers and ZoneControllers
	int const iNoAction( 0 );
	int const iReverseAction( 1 );
	int const iNormalAction( 2 );
	Array1D_string const ActionTypes( {0,2}, { "No action", "Reverse action", "Normal action" } );

	// Controller mode used in modules HVACControllers and ZoneControllers
	int const iModeWrongAction( -2 ); // Controller error. E.g., bad action
	int const iModeNone( -1 ); // Controller mode not yet determined
	int const iModeOff( 0 ); // Controller off (no air flow in loop)
	int const iModeInactive( 1 ); // Controller inactive (equip not available for current step)
	int const iModeActive( 2 ); // Controller active (schedule>0 and min<actuated<max)
	int const iModeMinActive( 3 ); // Controller active and min-constrained (equip available and actuated=min)
	int const iModeMaxActive( 4 ); // Controller active and max-constrained (equip available and actuated=max)

	int const iFirstMode( iModeWrongAction ); // First operating mode in range
	int const iLastMode( iModeMaxActive ); // Last operating mode in range
	Array1D_string const ControllerModeTypes( {-2,4}, { "Wrong action mode", "No controller mode", "Off controller mode", "Inactive controller mode", "Active unconstrained controller mode", "Active min-constrained controller mode", "Active max-constrained controller mode" } );

	// Controller operation used in module HVACControllers
	int const iControllerOpColdStart( 1 ); // Reset for cold start
	int const iControllerOpWarmRestart( 2 ); // Reset for warm restart with previous solution
	int const iControllerOpIterate( 3 ); // Check convergence and estimate next guess if needed
	int const iControllerOpEnd( 4 ); // Check convergence only and trace

	// Controller restart flag used in module HVACControllers
	int const iControllerWarmRestartNone( -1 ); // Indicates that warm restart was not attempted
	int const iControllerWarmRestartFail( 0 ); // Indicates that warm restart failed
	int const iControllerWarmRestartSuccess( 1 ); // Indicates that warm restart was successful

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

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

} // DataHVACControllers

} // EnergyPlus
