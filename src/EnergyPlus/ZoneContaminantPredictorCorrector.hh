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

} // ZoneContaminantPredictorCorrector

} // EnergyPlus

#endif
