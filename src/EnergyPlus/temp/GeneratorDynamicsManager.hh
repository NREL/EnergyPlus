#ifndef GeneratorDynamicsManager_hh_INCLUDED
#define GeneratorDynamicsManager_hh_INCLUDED

// C++ Headers
#include <string>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace GeneratorDynamicsManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	// na

	// SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

	// Functions

	void
	SetupGeneratorControlStateManager( int const GenNum ); // index of generator to setup

	void
	ManageGeneratorControlState(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int const GeneratorNum, // Generator number
		bool const RunFlagElectCenter, // TRUE when Generator operating per electric load center request
		bool const RunFlagPlant, // TRUE when generator operating per Plant request (always false)
		Real64 const ElecLoadRequest, // Generator Electrical power demand
		Real64 const ThermalLoadRequest, // cogenerator Thermal power demand
		Real64 & ElecLoadProvided, // power allowed
		int & OperatingMode, // operating mode
		Real64 & PLRforSubtimestepStartUp, // part load ratio for switch to normal from start up
		Real64 & PLRforSubtimestepShutDown, // part load ratio for switch from cool down to other
		bool const FirstHVACIteration // True is this is first HVAC iteration
	);

	void
	ManageGeneratorFuelFlow(
		int const GeneratorType, // type of Generator
		std::string const & GeneratorName, // user specified name of Generator
		int const GeneratorNum, // Generator number
		bool const RunFlag, // TRUE when Generator operating
		Real64 const FuelFlowRequest, // Generator demand mdot kg/ s
		Real64 & FuelFlowProvided, // allowed after constraints kg/s
		bool & ConstrainedIncreasingMdot, // true if request was altered because of fuel rate of change up
		bool & ConstrainedDecreasingMdot // true if request was altered because of fuel rate of change down
	);

	Real64
	FuncDetermineCWMdotForInternalFlowControl(
		int const GeneratorNum, // ID of generator
		Real64 const Pnetss, // power net steady state
		Real64 const TcwIn // temperature of cooling water at inlet
	);

} // GeneratorDynamicsManager

} // EnergyPlus

#endif
