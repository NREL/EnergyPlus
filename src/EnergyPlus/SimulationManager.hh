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

//     NOTICE
	//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
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


} // EnergyPlus

#endif
