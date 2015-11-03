#ifndef SimulationManager_hh_INCLUDED
#define SimulationManager_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

// HBIRE_USE_OMP defined, then openMP instructions are used.  Compiler may have to have switch for openmp
// HBIRE_NO_OMP defined, then old code is used without any openmp instructions

// HBIRE - loop in HeatBalanceIntRadExchange.cc
#ifdef HBIRE_USE_OMP
#undef HBIRE_NO_OMP
#else
#define HBIRE_NO_OMP
#endif

namespace SimulationManager {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// na

	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern bool RunPeriodsInInput;
	extern bool RunControlInInput;

	// SUBROUTINE SPECIFICATIONS FOR MODULE SimulationManager

	// Functions
	void
	clear_state();

	void
	ManageSimulation();

	void
	GetProjectData();

	void
	CheckForMisMatchedEnvironmentSpecifications();

	void
	CheckForRequestedReporting();

	void
	OpenOutputFiles();

	void
	CloseOutputFiles();

	void
	SetupSimulation( bool & ErrorsFound );

	void
	ReportNodeConnections();

	void
	ReportLoopConnections();

	void
	ReportParentChildren();

	void
	ReportCompSetMeterVariables();

	void
	PostIPProcessing();

	void
	CheckCachedIPErrors();

	void
	CheckThreading();

} // SimulationManager

// EXTERNAL SUBROUTINES:

void
Resimulate(
	bool & ResimExt, // Flag to resimulate the exterior energy use simulation
	bool & ResimHB, // Flag to resimulate the heat balance simulation (including HVAC)
	bool & ResimHVAC // Flag to resimulate the HVAC simulation
);


} // EnergyPlus

#endif
