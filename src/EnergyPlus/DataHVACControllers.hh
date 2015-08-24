#ifndef DataHVACControllers_hh_INCLUDED
#define DataHVACControllers_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace DataHVACControllers {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:

	extern int const ControllerSimple_Type;
	extern Array1D_string const ControllerTypes;

	// Controller action used in modules HVACControllers and ZoneControllers
	extern int const iNoAction;
	extern int const iReverseAction;
	extern int const iNormalAction;
	extern Array1D_string const ActionTypes;

	// Controller mode used in modules HVACControllers and ZoneControllers
	extern int const iModeWrongAction; // Controller error. E.g., bad action
	extern int const iModeNone; // Controller mode not yet determined
	extern int const iModeOff; // Controller off (no air flow in loop)
	extern int const iModeInactive; // Controller inactive (equip not available for current step)
	extern int const iModeActive; // Controller active (schedule>0 and min<actuated<max)
	extern int const iModeMinActive; // Controller active and min-constrained (equip available and actuated=min)
	extern int const iModeMaxActive; // Controller active and max-constrained (equip available and actuated=max)

	extern int const iFirstMode; // First operating mode in range
	extern int const iLastMode; // Last operating mode in range
	extern Array1D_string const ControllerModeTypes;

	// Controller operation used in module HVACControllers
	extern int const iControllerOpColdStart; // Reset for cold start
	extern int const iControllerOpWarmRestart; // Reset for warm restart with previous solution
	extern int const iControllerOpIterate; // Check convergence and estimate next guess if needed
	extern int const iControllerOpEnd; // Check convergence only and trace

	// Controller restart flag used in module HVACControllers
	extern int const iControllerWarmRestartNone; // Indicates that warm restart was not attempted
	extern int const iControllerWarmRestartFail; // Indicates that warm restart failed
	extern int const iControllerWarmRestartSuccess; // Indicates that warm restart was successful

} // DataHVACControllers

} // EnergyPlus

#endif
