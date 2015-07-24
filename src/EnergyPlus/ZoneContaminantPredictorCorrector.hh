#ifndef ZoneContaminantPredictorCorrector_hh_INCLUDED
#define ZoneContaminantPredictorCorrector_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

namespace ZoneContaminantPredictorCorrector {

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// na

	// DERIVED TYPE DEFINITIONS:
	// INTERFACE BLOCK SPECIFICATIONS:
	// na

	// MODULE VARIABLE DECLARATIONS:

	extern bool GetZoneAirContamInputFlag; // True when need to get input
	extern int TotGCGenConstant; // Number of constant generic contaminant sources and sinks
	extern int TotGCGenPDriven; // Number of pressure driven generic contaminant sources and sinks
	extern int TotGCGenCutoff; // Number of cutoff model generic contaminant sources and sinks
	extern int TotGCGenDecay; // Number of decay model generic contaminant sources and sinks
	extern int TotGCBLDiff; // Number of boudary layer diffusion generic contaminant model
	extern int TotGCDVS; // Number of deposition velocity sink generic contaminant model
	extern int TotGCDRS; // Number of deposition rate sink generic contaminant model

	// SUBROUTINE SPECIFICATIONS:

	// Functions

	void
	ManageZoneContaminanUpdates(
		int const UpdateType, // Can be iGetZoneSetPoints, iPredictStep, iCorrectStep
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	);

	void
	GetZoneContaminanInputs();

	void
	GetZoneContaminanSetPoints();

	void
	InitZoneContSetPoints();

	void
	PredictZoneContaminants(
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
	);

	void
	PushZoneTimestepHistories();

	void
	PushSystemTimestepHistories();

	void
	RevertZoneTimestepHistories();

	void
	CorrectZoneContaminants(
		bool const ShortenTimeStepSys,
		bool const UseZoneTimeStepHistory, // if true then use zone timestep history, if false use system time step history
		Real64 const PriorTimeStep // the old value for timestep length is passed for possible use in interpolating
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

} // ZoneContaminantPredictorCorrector

} // EnergyPlus

#endif
