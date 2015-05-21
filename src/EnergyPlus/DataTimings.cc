// ObjexxFCL Headers
#include <ObjexxFCL/Array.functions.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/Time_Date.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <DataTimings.hh>
#include <DataErrorTracking.hh>
#include <DataPrecisionGlobals.hh>
#include <DataSystemVariables.hh>
#include <General.hh>
#include <UtilityRoutines.hh>
#include <Timer.h>

namespace EnergyPlus {

#ifdef EP_NO_Timings
#undef EP_Timings
#endif

namespace DataTimings {

	// MODULE INFORMATION:
	//       AUTHOR         Linda K. Lawrie
	//       DATE WRITTEN   January 2012
	//       MODIFIED       na
	//       RE-ENGINEERED  na

	// PURPOSE OF THIS MODULE:
	// This data-only module is a repository for data and routines for timing within EnergyPlus.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
	using DataSystemVariables::tabchar;
	using DataSystemVariables::DeveloperFlag;

	// Data
	// -only module should be available to other modules and routines.
	// Thus, all variables in this module must be PUBLIC.

	// MODULE PARAMETER DEFINITIONS:
	int const MaxTimingStringLength( 250 ); // string length for timing string array

	// DERIVED TYPE DEFINITIONS

	// INTERFACE BLOCK SPECIFICATIONS
	// na

	// MODULE VARIABLE DECLARATIONS:
	int NumTimingElements( 0 );
	int MaxTimingElements( 0 );
	Real64 dailyWeatherTime;
	Real64 dailyExteriorEnergyUseTime;
	Real64 dailyHeatBalanceTime;
	Real64 hbdailyInit;
	Real64 hbdailyOutSurf;
	Real64 hbdailyInSurf;
	Real64 hbdailyHVAC;
	Real64 hbdailyRep;
	Real64 clockrate;
	bool lprocessingInputTiming( false );
	bool lmanageSimulationTiming( false );
	bool lcloseoutReportingTiming( false );

	// Following for calls to routines
#ifdef EP_Count_Calls
	int NumShadow_Calls( 0 );
	int NumShadowAtTS_Calls( 0 );
	int NumClipPoly_Calls( 0 );
	int NumInitSolar_Calls( 0 );
	int NumAnisoSky_Calls( 0 );
	int NumDetPolyOverlap_Calls( 0 );
	int NumCalcPerSolBeam_Calls( 0 );
	int NumDetShadowCombs_Calls( 0 );
	int NumIntSolarDist_Calls( 0 );
	int NumIntRadExchange_Calls( 0 );
	int NumIntRadExchangeZ_Calls( 0 );
	int NumIntRadExchangeMain_Calls( 0 );
	int NumIntRadExchangeOSurf_Calls( 0 );
	int NumIntRadExchangeISurf_Calls( 0 );
	int NumMaxInsideSurfIterations( 0 );
	int NumCalcScriptF_Calls( 0 );
#endif

	// Object Data
	Array1D< timings > Timing;

	// Functions

	void
	epStartTime(
#ifdef EP_NO_Timings
		std::string const & EP_UNUSED( ctimingElementstring )
#endif
#ifdef EP_Timings
		std::string const & ctimingElementstring
#endif
		)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Implement a timing scheme using start-stop (ref: epStopTime) that will help
		// developers pinpoint problems.

		// METHODOLOGY EMPLOYED:
		// structure similar to recurring error structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:
#if defined (_OPENMP) && defined(TIMER_OMP_GET_WTIME)
		// Using/Aliasing
		using namespace omp_lib; // only here for OMP timer
#endif

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

		// Object Data
		Array1D< timings > tempTiming; // used for reallocate.

#ifdef EP_NO_Timings
		return;
#endif
#ifdef EP_Timings
		int loop; // testing if already in structure
		int found; // indicator for element
		if ( NumTimingElements == 0 ) {
			MaxTimingElements = 250;
			Timing.allocate( MaxTimingElements );
		} else if ( NumTimingElements == MaxTimingElements ) {
			tempTiming.allocate( MaxTimingElements + 250 );
			tempTiming( {1,MaxTimingElements} ) = Timing( {1,MaxTimingElements} );
			Timing.deallocate();
			MaxTimingElements += 250;
			Timing.allocate( MaxTimingElements );
			Timing( {1,MaxTimingElements} ) = tempTiming( {1,MaxTimingElements} );
			tempTiming.deallocate();
		}

		found = 0;
		for ( loop = 1; loop <= NumTimingElements; ++loop ) {
			if ( Timing( loop ).Element != ctimingElementstring ) continue;
			found = loop;
		}

		if ( found == 0 ) {
			++NumTimingElements;
			Timing( NumTimingElements ).Element = ctimingElementstring;
			found = NumTimingElements;
		}

		TSTART( Timing( found ).rstartTime );
		++Timing( found ).calls;
#endif

	}

	void
	epStopTime(
#ifdef EP_NO_Timings
		std::string const & EP_UNUSED( ctimingElementstring ),
		Optional_bool_const EP_UNUSED( printit ), // true if it should be printed here.
		Optional_string_const EP_UNUSED( wprint ) // only needed (and assumed, if printit is true)
#endif
#ifdef EP_Timings
		std::string const & ctimingElementstring,
		Optional_bool_const printit, // true if it should be printed here.
		Optional_string_const wprint // only needed (and assumed, if printit is true)
#endif
	)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Implement a timing scheme using start-stop (ref: epStartTime) that will help
		// developers pinpoint problems.

		// METHODOLOGY EMPLOYED:
		// structure similar to recurring error structure.

		// REFERENCES:
		// na

		// USE STATEMENTS:

#if defined (_OPENMP) && defined(TIMER_OMP_GET_WTIME)
		// Using/Aliasing
		using namespace omp_lib; // only here for OMP timer
#endif

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

#ifdef EP_NO_Timings
		return;
#endif
#ifdef EP_Timings
		int loop; // testing if already in structure
		int found; // indicator for element
		Real64 stoptime;
		found = 0;
		for ( loop = 1; loop <= NumTimingElements; ++loop ) {
			if ( Timing( loop ).Element != ctimingElementstring ) continue;
			found = loop;
		}

		if ( found == 0 ) {
			ShowFatalError( "epStopTime: No element=" + ctimingElementstring );
		}

		TSTOP( stoptime );
		Timing( found ).currentTimeSum += ( stoptime - Timing( found ).rstartTime );

		if ( present( printit ) ) {
			if ( printit ) {
				{ auto const SELECT_CASE_var( wprint );
				if ( SELECT_CASE_var == "PRINT_TIME0" ) {
					gio::write( "(a80,f16.4)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIME1" ) {
					gio::write( "(a70,f16.4)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIME2" ) {
					gio::write( "(a60,f10.4)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIME2i" ) {
					gio::write( "(a56,i4,f10.4)" ) << ctimingElementstring << Timing( found ).calls << Timing( found ).currentTimeSum;
				} else if ( SELECT_CASE_var == "PRINT_TIME3" ) {
					gio::write( "(a50,f10.4)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIME3i" ) {
					gio::write( "(a46,i4,f10.4)" ) << ctimingElementstring << Timing( found ).calls << Timing( found ).currentTimeSum;
				} else if ( SELECT_CASE_var == "PRINT_TIME4" ) {
					gio::write( "(a40,f10.4)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIMEX" ) {
					gio::write( "(a100,f16.6)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINTES" ) {
					gio::write( "(a80,es22.15)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIME_AF" ) {
					gio::write( "(a55,10x,f16.4)" ) << ctimingElementstring << stoptime - Timing( found ).rstartTime;
				} else if ( SELECT_CASE_var == "PRINT_TIME_AIF" ) {
					gio::write( "(a55,i10,f16.4)" ) << ctimingElementstring << Timing( found ).calls << Timing( found ).currentTimeSum;
				} else {
					gio::write( "*" ) << ctimingElementstring << Timing( found ).currentTimeSum;
				}}
			}
			//could not cover:
			//#define PRINT_TIME_AIIF(a, i1, i2, t) write(*,'(a55,i10,i10,f16.4)') a, i1, i2, t
			//#define PRINT_TIME_AIIIF(a, i1, i2, i3, t) write(*,'(a55,i10,i10,i10,f16.55)') a, i1, i2, i3, t
		}
#endif

	}

	void
	epSummaryTimes( 
#ifdef EP_NO_Timings
		Real64 & EP_UNUSED( TimeUsed_CPUTime )
#endif
#ifdef EP_Timings
		Real64 & TimeUsed_CPUTime
#endif
		)
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Print summary of timings from timing scheme using start-stop (ref: epStartTime, epStopTime) that will help
		// developers pinpoint problems.

		// METHODOLOGY EMPLOYED:
		// structure similar to recurring error structure.

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt fmtA( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:

#ifdef EP_NO_Timings
		return;
#endif
#ifdef EP_Timings
		int loop;
		int EchoInputFile;
		EchoInputFile = FindUnitNumber( outputAuditFile );
		gio::write( EchoInputFile, fmtA ) << "Timing Element" + tabchar + "# calls" + tabchar + "Time {s}" + tabchar + "Time {s} (per call)";

		for ( loop = 1; loop <= NumTimingElements; ++loop ) {
			if ( Timing( loop ).calls > 0 ) {
				gio::write( EchoInputFile, fmtA ) << Timing( loop ).Element + tabchar + RoundSigDigits( Timing( loop ).calls ) + tabchar + RoundSigDigits( Timing( loop ).currentTimeSum, 3 ) + tabchar + RoundSigDigits( Timing( loop ).currentTimeSum / double( Timing( loop ).calls ), 3 );
			} else {
				gio::write( EchoInputFile, fmtA ) << Timing( loop ).Element + tabchar + RoundSigDigits( Timing( loop ).calls ) + tabchar + RoundSigDigits( Timing( loop ).currentTimeSum, 3 ) + tabchar + RoundSigDigits( -999.0, 3 );
			}
		}
		gio::write( EchoInputFile, fmtA ) << "Time from CPU_Time" + tabchar + RoundSigDigits( TimeUsed_CPUTime, 3 );
#endif

	}

	Real64
	epGetTimeUsed( std::string const & ctimingElementstring )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provides outside function to getting time used on a particular element

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataErrorTracking::AbortProcessing;

		// Return value
		Real64 totalTimeUsed;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int loop; // testing if already in structure
		int found; // indicator for element

		found = 0;
		for ( loop = 1; loop <= NumTimingElements; ++loop ) {
			if ( Timing( loop ).Element != ctimingElementstring ) continue;
			found = loop;
		}

		if ( found == 0 && ! AbortProcessing ) {
			ShowFatalError( "epGetTimeUsed: No element=" + ctimingElementstring );
		} else {
			ShowSevereError( "epGetTimeUsed: No element=" + ctimingElementstring );
		}

		totalTimeUsed = Timing( found ).currentTimeSum;

		return totalTimeUsed;

	}

	Real64
	epGetTimeUsedperCall( std::string const & ctimingElementstring )
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provides outside function to getting time used on a particular element
		// per Call.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using DataErrorTracking::AbortProcessing;

		// Return value
		Real64 averageTimeUsed;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		int loop; // testing if already in structure
		int found; // indicator for element

		found = 0;
		for ( loop = 1; loop <= NumTimingElements; ++loop ) {
			if ( Timing( loop ).Element != ctimingElementstring ) continue;
			found = loop;
		}

		if ( found == 0 ) {
			ShowFatalError( "epGetTimeUsedperCall: No element=" + ctimingElementstring );
		} else {
			ShowSevereError( "epGetTimeUsedperCall: No element=" + ctimingElementstring );
		}

		if ( Timing( found ).calls > 0 ) {
			averageTimeUsed = Timing( found ).currentTimeSum / double( Timing( found ).calls );
		} else {
			averageTimeUsed = -999.0;
		}

		return averageTimeUsed;

	}

	Real64
	eptime()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   January 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// An alternative method for timing (to CPU_TIME) is to call the standard
		// System_Clock routine.  This is a standard alternative to CPU_TIME.
		// According to Intel documentation, the "count_rate" may differ depending on
		// the size of the integer to receive the output.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 calctime; // calculated time based on "count" and "count_rate"

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Int32 icount;

		SYSTEM_CLOCK32( icount );

		calctime = double( icount ) / clockrate; // clockrate is set by main program.

		return calctime;

	}

	Real64
	epElapsedTime()
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   February 2012
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// An alternative method for timing elapsed times is to call the standard
		// Date_And_Time routine and set the "time".

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 calctime; // calculated time based on hrs, minutes, seconds, milliseconds

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Array1D< Int32 > clockvalues( 8 );
		//value(1)   Current year
		//value(2)   Current month
		//value(3)   Current day
		//value(4)   Time difference with respect to UTC in minutes (0-59)
		//value(5)   Hour of the day (0-23)
		//value(6)   Minutes (0-59)
		//value(7)   Seconds (0-59)
		//value(8)   Milliseconds (0-999)

		date_and_time( _, _, _, clockvalues );
		calctime = clockvalues( 5 ) * 3600.0 + clockvalues( 6 ) * 60.0 + clockvalues( 7 ) + clockvalues( 8 ) / 1000.0;

		return calctime;

	}

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

} // DataTimings

} // EnergyPlus
