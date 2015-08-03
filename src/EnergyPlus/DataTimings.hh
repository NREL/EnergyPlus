#ifndef DataTimings_hh_INCLUDED
#define DataTimings_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

namespace EnergyPlus {

#ifdef EP_NO_Timings
#undef EP_Timings
#endif

namespace DataTimings {

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	extern int const MaxTimingStringLength; // string length for timing string array

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	extern int NumTimingElements;
	extern int MaxTimingElements;
	extern Real64 dailyWeatherTime;
	extern Real64 dailyExteriorEnergyUseTime;
	extern Real64 dailyHeatBalanceTime;
	extern Real64 hbdailyInit;
	extern Real64 hbdailyOutSurf;
	extern Real64 hbdailyInSurf;
	extern Real64 hbdailyHVAC;
	extern Real64 hbdailyRep;
	extern Real64 clockrate;
	extern bool lprocessingInputTiming;
	extern bool lmanageSimulationTiming;
	extern bool lcloseoutReportingTiming;

	// Following for calls to routines
#ifdef EP_Count_Calls
	extern int NumShadow_Calls;
	extern int NumShadowAtTS_Calls;
	extern int NumClipPoly_Calls;
	extern int NumInitSolar_Calls;
	extern int NumAnisoSky_Calls;
	extern int NumDetPolyOverlap_Calls;
	extern int NumCalcPerSolBeam_Calls;
	extern int NumDetShadowCombs_Calls;
	extern int NumIntSolarDist_Calls;
	extern int NumIntRadExchange_Calls;
	extern int NumIntRadExchangeZ_Calls;
	extern int NumIntRadExchangeMain_Calls;
	extern int NumIntRadExchangeOSurf_Calls;
	extern int NumIntRadExchangeISurf_Calls;
	extern int NumMaxInsideSurfIterations;
	extern int NumCalcScriptF_Calls;
#endif

	// Types

	struct timings
	{
		// Members
		std::string Element;
		Real64 rstartTime;
		Real64 currentTimeSum;
		int calls;

		// Default Constructor
		timings() :
			rstartTime( 0.0 ),
			currentTimeSum( 0.0 ),
			calls( 0 )
		{}

		// Member Constructor
		timings(
			std::string const & Element,
			Real64 const rstartTime,
			Real64 const currentTimeSum,
			int const calls
		) :
			Element( Element ),
			rstartTime( rstartTime ),
			currentTimeSum( currentTimeSum ),
			calls( calls )
		{}

	};

	// Object Data
	extern Array1D< timings > Timing;

	// Functions

	void
	epStartTime( std::string const & ctimingElementstring );

	void
	epStopTime(
		std::string const & ctimingElementstring,
		Optional_bool_const printit = _, // true if it should be printed here.
		Optional_string_const wprint = _ // only needed (and assumed, if printit is true)
	);

	void
	epSummaryTimes( Real64 & TimeUsed_CPUTime );

	Real64
	epGetTimeUsed( std::string const & ctimingElementstring );

	Real64
	epGetTimeUsedperCall( std::string const & ctimingElementstring );

	Real64
	eptime();

	Real64
	epElapsedTime();

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

} // DataTimings

} // EnergyPlus

#endif
