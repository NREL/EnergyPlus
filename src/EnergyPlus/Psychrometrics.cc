// C++ Headers
#include <cmath>
#include <cstdlib>
#include <iostream>

// ObjexxFCL Headers
#include <ObjexxFCL/bit.hh>
#include <ObjexxFCL/FArray.functions.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <Psychrometrics.hh>
#include <DataEnvironment.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <General.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

#ifdef EP_nocache_Psychrometrics
#undef EP_cache_PsyTwbFnTdbWPb
#undef EP_cache_PsyPsatFnTemp
#else
#define EP_cache_PsyTwbFnTdbWPb
#define EP_cache_PsyPsatFnTemp
#endif
#define EP_psych_errors

namespace Psychrometrics {
	// Module containing the Psychometric simulation routines

	// MODULE INFORMATION:
	//       AUTHOR         Linda Lawrie
	//       DATE WRITTEN   December 1998
	//       MODIFIED       February 2010
	//       RE-ENGINEERED  Jan 2004: Rahul Chillar

	// PURPOSE OF THIS MODULE:
	// This module provides a repository for the psychrometric routines.

	// METHODOLOGY EMPLOYED:
	// na

	// REFERENCES:
	// na

	// OTHER NOTES:
	// Todo after 2.2 release:
	// remove restriction on MAX(W, 1d-5)
	// more research on hfg calc

	// Using/Aliasing
	using namespace DataPrecisionGlobals;
#ifdef EP_psych_errors
	using namespace DataGlobals;
	using namespace DataEnvironment;
#endif

	// Use Statements for other routines
#ifdef EP_psych_errors
	using General::TrimSigDigits;
#endif

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// call for recurring errors
	int const iPsyTdpFnTdbTwbPb( 1 );
	int const iPsyRhFnTdbWPb( 2 );
	int const iPsyTwbFnTdbWPb( 3 );
	int const iPsyTwbFnTdbWPb2( 14 );
	int const iPsyTwbFnTdbWPb3( 15 ); // convergence
	int const iPsyVFnTdbWPb( 4 );
	int const iPsyWFnTdpPb( 5 );
	int const iPsyWFnTdbH( 6 );
	int const iPsyWFnTdbTwbPb( 7 );
	int const iPsyWFnTdbTwbPb2( 16 );
	int const iPsyWFnTdbRhPb( 8 );
	int const iPsyPsatFnTemp( 9 );
	int const iPsyTsatFnHPb( 10 );
	int const iPsyTsatFnPb( 11 );
	int const iPsyTsatFnPb2( 17 ); // iterations
	int const iPsyRhFnTdbRhov( 12 );
	int const iPsyRhFnTdbRhovLBnd0C( 13 );
	int const iPsyTwbFnTdbWPb_cache( 18 );
	int const iPsyPsatFnTemp_cache( 19 );
	int const NumPsychMonitors( 19 ); // Parameterization of Number of psychrometric routines that
#ifdef EP_psych_stats
	FArray1D_string const PsyRoutineNames( NumPsychMonitors, { "PsyTdpFnTdbTwbPb", "PsyRhFnTdbWPb", "PsyTwbFnTdbWPb", "PsyVFnTdbWPb", "PsyWFnTdpPb", "PsyWFnTdbH", "PsyWFnTdbTwbPb", "PsyWFnTdbRhPb", "PsyPsatFnTemp", "PsyTsatFnHPb", "PsyTsatFnPb", "PsyRhFnTdbRhov", "PsyRhFnTdbRhovLBnd0C", "PsyTwbFnTdbWPb", "PsyTwbFnTdbWPb", "PsyWFnTdbTwbPb", "PsyTsatFnPb", "PsyTwbFnTdbWPb_cache", "PsyPsatFnTemp_cache" } ); // 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 - HR | 15 - max iter | 16 - HR | 17 - max iter | 18 - PsyTwbFnTdbWPb_raw (raw calc) | 19 - PsyPsatFnTemp_raw (raw calc)

	FArray1D_bool const PsyReportIt( NumPsychMonitors, { true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, true, true } ); // PsyTdpFnTdbTwbPb     1 | PsyRhFnTdbWPb        2 | PsyTwbFnTdbWPb       3 | PsyVFnTdbWPb         4 | PsyWFnTdpPb          5 | PsyWFnTdbH           6 | PsyWFnTdbTwbPb       7 | PsyWFnTdbRhPb        8 | PsyPsatFnTemp        9 | PsyTsatFnHPb         10 | PsyTsatFnPb          11 | PsyRhFnTdbRhov       12 | PsyRhFnTdbRhovLBnd0C 13 | PsyTwbFnTdbWPb       14 - HR | PsyTwbFnTdbWPb       15 - max iter | PsyWFnTdbTwbPb       16 - HR | PsyTsatFnPb          17 - max iter | PsyTwbFnTdbWPb_cache 18 - PsyTwbFnTdbWPb_raw (raw calc) | PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
#endif

#ifndef EP_psych_errors
	Real64 const KelvinConv( 273.15 );
#endif

#ifdef EP_cache_PsyTwbFnTdbWPb
	int const twbcache_size( 1024 * 1024 );
	int const twbprecision_bits( 20 );
#endif
#ifdef EP_cache_PsyPsatFnTemp
	int const psatcache_size( 1024 * 1024 );
	int const psatprecision_bits( 24 ); // 28  // 24  // 32
#endif

	// MODULE VARIABLE DECLARATIONS:
	// na

	// MODULE VARIABLE DEFINITIONS:
	std::string String;
	bool ReportErrors( true );
	FArray1D_int iPsyErrIndex( NumPsychMonitors, NumPsychMonitors * 0 ); // Number of times error occurred
#ifdef EP_psych_stats
	FArray1D< Int64 > NumTimesCalled( NumPsychMonitors, NumPsychMonitors * 0 );
	FArray1D_int NumIterations( NumPsychMonitors, NumPsychMonitors * 0 );
#endif

	// Object Data
#ifdef EP_cache_PsyTwbFnTdbWPb
	FArray1D< cached_twb_t > cached_Twb; // DIMENSION(0:twbcache_size)
#endif
#ifdef EP_cache_PsyPsatFnTemp
	FArray1D< cached_psat_t > cached_Psat; // DIMENSION(0:psatcache_size)
#endif

	// Subroutine Specifications for the Module

	// Functions

	void
	InitializePsychRoutines()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Initializes some variables for PsychRoutines

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
		// na

#ifdef EP_cache_PsyTwbFnTdbWPb
		cached_Twb.allocate( {0,twbcache_size} );
#endif
#ifdef EP_cache_PsyPsatFnTemp
		cached_Psat.allocate( {0,psatcache_size} );
#endif

	}

	void
	ShowPsychrometricSummary()
	{

		// SUBROUTINE INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS SUBROUTINE:
		// Provides a Psychrometric summary report to the audit file.
		// Maybe later to the .eio file.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::RoundSigDigits;

		// Locals
		// SUBROUTINE ARGUMENT DEFINITIONS:
		// na

		// SUBROUTINE PARAMETER DEFINITIONS:
		static gio::Fmt const fmta( "(A)" );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// SUBROUTINE LOCAL VARIABLE DECLARATIONS:
#ifdef EP_psych_stats
		int EchoInputFile; // found unit number for "eplusout.audit"
		int Loop;
		Real64 AverageIterations;
		std::string istring;

		EchoInputFile = FindUnitNumber( "eplusout.audit" );
		if ( EchoInputFile == 0 ) return;
		if ( any_gt( NumTimesCalled, 0 ) ) {
			gio::write( EchoInputFile, fmta ) << "RoutineName,#times Called,Avg Iterations";
			for ( Loop = 1; Loop <= NumPsychMonitors; ++Loop ) {
				if ( ! PsyReportIt( Loop ) ) continue;
				gio::write( istring, "*" ) << NumTimesCalled( Loop );
				strip( istring );
				if ( NumIterations( Loop ) > 0 ) {
					AverageIterations = double( NumIterations( Loop ) ) / double( NumTimesCalled( Loop ) );
					gio::write( EchoInputFile, fmta ) << PsyRoutineNames( Loop ) + ',' + istring + ',' + RoundSigDigits( AverageIterations, 2 );
				} else {
					gio::write( EchoInputFile, fmta ) << PsyRoutineNames( Loop ) + ',' + istring;
				}
			}
		}
#endif

	}

	Real64
	PsyRhoAirFnPbTdbW(
		Real64 const pb, // barometric pressure (Pascals)
		Real64 const tdb, // dry bulb temperature (Celsius)
		Real64 const dw, // humidity ratio (kgWater/kgDryAir)
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         G. S. Wright
		//       DATE WRITTEN   June 2, 1994
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides density of air as a function of barometric
		// pressure, dry bulb temperature, and humidity ratio.

		// METHODOLOGY EMPLOYED:
		// ideal gas law
		//    universal gas const for air 287 J/(kg K)
		//    air/water molecular mass ratio 28.9645/18.01534

		// REFERENCES:
		// Wylan & Sontag, Fundamentals of Classical Thermodynamics.
		// ASHRAE handbook 1985 Fundamentals, Ch. 6, eqn. (6),(26)

		// Using/Aliasing
		using General::RoundSigDigits;

		// Return value
		Real64 rhoair; // result=> density of air

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		rhoair = pb / ( 287.0 * ( tdb + KelvinConv ) * ( 1.0 + 1.6077687 * max( dw, 1.0e-5 ) ) );
#ifdef EP_psych_errors
		if ( rhoair < 0.0 ) {
			ShowSevereError( "PsyRhoAirFnPbTdbW: RhoAir (Density of Air) is calculated <= 0 [" + RoundSigDigits( rhoair, 5 ) + "]." );
			ShowContinueError( "pb =[" + RoundSigDigits( pb, 2 ) + "], tdb=[" + RoundSigDigits( tdb, 2 ) + "], w=[" + RoundSigDigits( dw, 7 ) + "]." );
			if ( present( CalledFrom ) ) {
				ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
			} else {
				ShowContinueErrorTimeStamp( " Routine=Unknown," );
			}
			ShowFatalError( "Program terminates due to preceding condition." );
		}
#endif

		return rhoair;
	}

	Real64
	PsyCpAirFnWTdb(
		Real64 const dw, // humidity ratio {kgWater/kgDryAir}
		Real64 const T, // input temperature {Celsius}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         J. C. VanderZee
		//       DATE WRITTEN   Feb. 1994
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the heat capacity of air {J/kg-C} as function of humidity ratio.

		// METHODOLOGY EMPLOYED:
		// take numerical derivative of PsyHFnTdbW function

		// REFERENCES:
		// see PsyHFnTdbW ref. to ASHRAE Fundamentals
		// USAGE:  cpa = PsyCpAirFnWTdb(w,T)

		// USE STATEMENTS:
		// na

		// Return value
		Real64 cpa; // result => heat capacity of air {J/kg-C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 h1; // PsyHFnTdbW result of input parameters
		Real64 tt; // input temperature (T) + .1
		Real64 h2; // PsyHFnTdbW result of input humidity ratio and tt
		Real64 w; // humidity ratio

		static Real64 dwSave( -100.0 );
		static Real64 Tsave( -100.0 );
		static Real64 cpaSave( -100.0 );

		//check if last call had the same input and if it did just use the
		//saved output.
		if ( Tsave == T ) {
			if ( dwSave == dw ) {
				cpa = cpaSave;
				return cpa;
			}
		}

		w = max( dw, 1.0e-5 );
		h1 = PsyHFnTdbW( T, w, CalledFrom );
		tt = T + 0.1;
		h2 = PsyHFnTdbW( tt, w, CalledFrom );
		cpa = ( h2 - h1 ) / 0.1;

		//save values for next call
		dwSave = dw;
		Tsave = T;
		cpaSave = cpa;
		return cpa;
	}

	Real64
	PsyHfgAirFnWTdb(
		Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
		Real64 const T, // input temperature {Celsius}
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May, 2001
		//       MODIFIED       June, 2002
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides latent energy of air as function of humidity ratio and temperature.

		// METHODOLOGY EMPLOYED:
		// calculates hg and then hf and the difference is Hfg.

		// REFERENCES:
		// see ASHRAE Fundamentals Psychrometric Chapter
		// USAGE:  hfg = PsyHfgAirFnWTdb(w,T)

		// USE STATEMENTS:
		// na

		// Return value
		Real64 hfg; // result => heat of vaporization for moist air {J/kg}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 hg; // enthalpy of the gas
		Real64 hf; // enthalpy of the fluid
		//      INTEGER,SAVE :: b0cerrcount=0
		Real64 Temperature; // input temperature {Celsius} - corrected for >= 0C

		// This formulation currently does not use W since it returns results that are in J/kg and the
		//  amount of energy is on a per unit of moisture basis.

		Temperature = max( T, 0.0 );
		//      Temperature=T
		hg = 2500940.0 + 1858.95 * Temperature;
		hf = 4180.0 * Temperature;
		hfg = hg - hf;
		//4/8/08 - pending comments      hfg = hg

		return hfg;

	}

	Real64
	PsyHgAirFnWTdb(
		Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
		Real64 const T, // input temperature {Celsius}
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard Liesen
		//       DATE WRITTEN   May, 2001
		//       MODIFIED       June, 2002
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides latent energy of the moisture as a gas in the air as
		// function of humidity ratio and temperature.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// see ASHRAE Fundamentals Psychrometric Chapter
		// USAGE:  hg = PsyHgAirFnWTdb(w,T)

		// USE STATEMENTS:
		// na

		// Return value
		Real64 hg; // enthalpy of the gas {units?}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		// This formulation currently does not use W since it returns results that are in J/kg and the
		//  amount of energy is on a per unit of moisture basis.

		hg = 2500940.0 + 1858.95 * T;

		return hg;
	}

	Real64
	PsyTdpFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the dew-point temperature {C} from dry-bulb, wet-bulb and pressure.

		// METHODOLOGY EMPLOYED:
		// Needs description, as appropriate.

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 TDP; // result=> dew-point temperature {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 W; // humidity ratio
		//                          calculate dew point temperature

		W = PsyWFnTdbTwbPb( TDB, TWB, PB, CalledFrom );
		W = max( W, 1.0e-5 );

		TDP = PsyTdpFnWPb( W, PB, CalledFrom );

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTdpFnTdbTwbPb );
#endif

		//                                      VALIDITY TEST.
		if ( TDP > TWB ) {
#ifdef EP_psych_errors
			if ( TDP > TWB + 0.1 ) {
				if ( ! WarmupFlag ) { // Display error message
					if ( iPsyErrIndex( iPsyTdpFnTdbTwbPb ) == 0 ) {
						ShowWarningMessage( "Calculated Dew Point Temperature being reset (PsyTdpFnTdbTwbPb)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						String = " Dry-bulb=" + TrimSigDigits( TDB, 2 ) + " Wet-Bulb (WB)= " + TrimSigDigits( TWB, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 ) + " Humidity Ratio=" + TrimSigDigits( W, 3 );
						ShowContinueError( String );
						String = " Calculated Dew Point Temperature (DPT)= " + TrimSigDigits( TDP, 2 ) + "; Since DPT > WB, DPT will be set to WB";
						ShowContinueError( String );
					}
					ShowRecurringWarningErrorAtEnd( "Calculated Dew Point Temperature being reset (PsyTdpFnTdbTwbPb)", iPsyErrIndex( iPsyTdpFnTdbTwbPb ), TDP, TDP, _, "C", "C" );
				}

				TDP = TWB;

			} else {
				TDP = TWB;
			}
#endif
			TDP = TWB;
		}

		// TDP is the result

		return TDP;

	}

	Real64
	PsyTdpFnWPb(
		Real64 const W, // humidity ratio
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the dew-point temperature {C} from humidity ratio and pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P.99, EQN 22

		// USE STATEMENTS:
		// na

		// Return value
		Real64 TDP; // result=> dew-point temperature {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 PDEW; // pressure at dew point temperature
		Real64 W0; // limited humidity ratio

		W0 = max( W, 1.0e-5 );
		PDEW = PB * W0 / ( 0.62198 + W0 );
		TDP = PsyTsatFnPb( PDEW, CalledFrom );

		return TDP;
	}

	Real64
	PsyHFnTdbW(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the enthalpy {J/kg} from dry-bulb temperature and humidity ratio.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

		// USE STATEMENTS:
		// na

		// Return value
		Real64 H; // enthalpy {J/kg}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 W; // humidity ratio

		//                           calculate enthalpy

		W = max( dW, 1.0e-5 );
		H = 1.00484e3 * TDB + W * ( 2.50094e6 + 1.85895e3 * TDB );

		return H;
	}

	Real64
	PsyHFnTdbRhPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0 - 1.0)
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         J. C. VanderZee
		//       DATE WRITTEN   Feb. 1994
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides air enthalpy from temperature and relative humidity.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
		//   by using functions PsyWFnTdbRhPb and PsyHFnTdbW

		// USE STATEMENTS:
		// na

		// Return value
		Real64 H; // result=> enthalpy {J/kg}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 W; // humidity ratio

		W = PsyWFnTdbRhPb( TDB, RH, PB );
		W = max( W, 1.0e-5 );
		H = PsyHFnTdbW( TDB, W );

		return H;
	}

	Real64
	PsyTdbFnHW(
		Real64 const H, // enthalpy {J/kg}
		Real64 const dW, // humidity ratio
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         J. C. VanderZee
		//       DATE WRITTEN   Feb. 1994
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides air temperature from enthalpy and humidity ratio.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
		//   by inverting function PsyHFnTdbW

		// USE STATEMENTS:
		// na

		// Return value
		Real64 TDB; // result=> dry-bulb temperature {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 W; // humidity ratio

		W = max( dW, 1.0e-5 );
		TDB = ( H - 2.50094e6 * W ) / ( 1.00484e3 + 1.85895e3 * W );

		return TDB;
	}

	Real64
	PsyRhovFnTdbRh(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. J. Liesen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
		//                      Function is continuous over temperature spectrum
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the Vapor Density in air as a
		// function of dry bulb temperature, and Relative Humidity.

		// METHODOLOGY EMPLOYED:
		// ideal gas law
		// Universal gas const for water vapor 461.52 J/(kg K)

		// REFERENCES:
		// ASHRAE handbook 1993 Fundamentals, ??
		// Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
		// values from PsyRhFnTdbWPb

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RhoV; // Vapor density in air

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 pws; // saturation pressure for Tdb

		pws = PsyPsatFnTemp( Tdb, "PsyRhovFnTdbRh" );

		RhoV = ( pws * RH ) / ( 461.52 * ( Tdb + KelvinConv ) );

		return RhoV;
	}

	Real64
	PsyRhovFnTdbRhLBnd0C(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. J. Liesen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       Name change to signify derivation and temperatures were used
		//                      with 0C as minimum; LKL January 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the Vapor Density in air as a
		// function of dry bulb temperature, and Relative Humidity.

		// METHODOLOGY EMPLOYED:
		// ideal gas law
		// Universal gas const for water vapor 461.52 J/(kg K)

		// REFERENCES:
		// ASHRAE handbook 1993 Fundamentals,

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RhoV; // Vapor density in air

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		RhoV = RH / ( 461.52 * ( Tdb + KelvinConv ) ) * std::exp( 23.7093 - 4111.0 / ( ( Tdb + KelvinConv ) - 35.45 ) );

		return RhoV;
	}

	Real64
	PsyRhovFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // Barometric Pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. J. Liesen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the Vapor Density in air as a
		// function of dry bulb temperature, Humidity Ratio, and Barometric Pressure.

		// METHODOLOGY EMPLOYED:
		// ideal gas law
		// Universal gas const for water vapor 461.52 J/(kg K)

		// REFERENCES:
		// ASHRAE handbook 1993 Fundamentals,

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RhoV; // Vapor density in air

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 W; // humidity ratio

		W = max( dW, 1.0e-5 );
		RhoV = W * PB / ( 461.52 * ( Tdb + KelvinConv ) * ( W + 0.62198 ) );

		return RhoV;
	}

	Real64
	PsyRhFnTdbRhov(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. J. Liesen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       Change temperature range applied (determine pws); Aug 2007; LKL
		//                      Function is continuous over temperature spectrum
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the Relative Humidity in air as a
		// function of dry bulb temperature and Vapor Density.

		// METHODOLOGY EMPLOYED:
		// ideal gas law
		// Universal gas const for water vapor 461.52 J/(kg K)

		// REFERENCES:
		// ASHRAE handbook 1993 Fundamentals,
		// Used values from Table 2, HOF 2005, Chapter 6, to verify that these values match (at saturation)
		// values from PsyRhFnTdbWPb

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RHValue; // relative humidity value (0.0-1.0)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:
		Real64 pws; // saturation pressure for Tdb

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		if ( Rhovapor <= 0.0 ) {
			RHValue = 0.0;
		} else {
			pws = PsyPsatFnTemp( Tdb, "PsyRhFnTdbRhov" );
			RHValue = Rhovapor * 461.52 * ( Tdb + KelvinConv ) / pws;
		}

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyRhFnTdbRhov );
#endif

		//                   VALIDITY TEST
		if ( RHValue < 0.0 || RHValue > 1.0 ) {
			if ( RHValue > 1.0 ) {
#ifdef EP_psych_errors
				if ( RHValue > 1.01 ) {
					if ( ! WarmupFlag ) {
						if ( iPsyErrIndex( iPsyRhFnTdbRhov ) == 0 ) {
							String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100., 2 );
							ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov) " );
							if ( present( CalledFrom ) ) {
								ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
							} else {
								ShowContinueErrorTimeStamp( " Routine=Unknown," );
							}
							ShowContinueError( String );
							ShowContinueError( "Relative Humidity being reset to 100.0 %" );
						}
						ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov)", iPsyErrIndex( iPsyRhFnTdbRhov ), RHValue * 100., RHValue * 100., _, "%", "%" );
					}
				}
#endif
				RHValue = 1.0;
			} else { // RHValue < 0.0
#ifdef EP_psych_errors
				if ( RHValue < -0.05 ) {
					if ( ! WarmupFlag ) {
						if ( iPsyErrIndex( iPsyRhFnTdbRhov ) == 0 ) {
							String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100., 2 );
							ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov) " );
							if ( present( CalledFrom ) ) {
								ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
							} else {
								ShowContinueErrorTimeStamp( " Routine=Unknown," );
							}
							ShowContinueError( String );
							ShowContinueError( "Relative Humidity being reset to 1%" );
						}
						ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov)", iPsyErrIndex( iPsyRhFnTdbRhov ), RHValue * 100., RHValue * 100., _, "%", "%" );
					}
				}
#endif
				RHValue = .01;
			}
		} // RHValue in proper range
		return RHValue;
	}

	Real64
	PsyRhFnTdbRhovLBnd0C(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         R. J. Liesen
		//       DATE WRITTEN   July 2000
		//       MODIFIED       Name change to signify derivation and temperatures were used
		//                      with 0C as minimum; LKL January 2008
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the Relative Humidity in air as a
		// function of dry bulb temperature and Vapor Density.

		// METHODOLOGY EMPLOYED:
		// ideal gas law
		// Universal gas const for water vapor 461.52 J/(kg K)

		// REFERENCES:
		// ASHRAE handbook 1993 Fundamentals,

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RHValue; // relative humidity value (0.0-1.0)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		//      integer,save :: b0cerrcount=0

		if ( Rhovapor <= 0.0 ) {
			RHValue = 0.0;
		} else {
			RHValue = Rhovapor * 461.52 * ( Tdb + KelvinConv ) * std::exp( -23.7093 + 4111.0 / ( ( Tdb + KelvinConv ) - 35.45 ) );
		}

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyRhFnTdbRhovLBnd0C );
#endif

		//                   VALIDITY TEST
		if ( RHValue < 0.0 || RHValue > 1.0 ) {
			if ( RHValue > 1.0 ) {
#ifdef EP_psych_errors
				if ( RHValue > 1.01 ) {
					if ( ! WarmupFlag ) {
						if ( iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ) == 0 ) {
							String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100., 2 );
							ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C) " );
							if ( present( CalledFrom ) ) {
								ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
							} else {
								ShowContinueErrorTimeStamp( " Routine=Unknown," );
							}
							ShowContinueError( String );
							ShowContinueError( "Relative Humidity being reset to 100.0%" );
						}
						ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C)", iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ), RHValue * 100., RHValue * 100., _, "%", "%" );
					}
				}
#endif
				RHValue = 1.0;
			} else { // RHValue < 0.0
#ifdef EP_psych_errors
				if ( RHValue < -0.05 ) {
					if ( ! WarmupFlag ) {
						if ( iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ) == 0 ) {
							String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100., 2 );
							ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C) " );
							if ( present( CalledFrom ) ) {
								ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
							} else {
								ShowContinueErrorTimeStamp( " Routine=Unknown," );
							}
							ShowContinueError( String );
							ShowContinueError( "Relative Humidity being reset to 1%" );
						}
						ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C)", iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ), RHValue * 100., RHValue * 100., _, "%", "%" );
					}
				}
#endif
				RHValue = .01;
			}
		} // RHValue in proper range
		return RHValue;
	}

	Real64
	PsyRhFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Richard J. Liesen
		//       DATE WRITTEN   Nov 1988
		//       MODIFIED       Aug 1989, Michael J. Witte
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the relative humidity value (0.0-1.0) as a result of
		// dry-bulb temperature, humidity ratio and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK FUNDAMENTALS 1985, P6.12, EQN 10,21,23

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RHValue; // relative humidity value (0.0-1.0)

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 U; // Degree of Saturation
		Real64 PWS; // Pressure -- saturated for pure water
		Real64 W; // humidity ratio

		if ( present( CalledFrom ) ) {
			PWS = PsyPsatFnTemp( TDB, CalledFrom );
		} else {
			PWS = PsyPsatFnTemp( TDB, "PsyRhFnTdbWPb" );
		}

		//                   Find Degree Of Saturation
		W = max( dW, 1.0e-5 );
		U = W / ( 0.62198 * PWS / ( PB - PWS ) );
		//                   Calculate The Relative Humidity
		RHValue = U / ( 1.0 - ( 1.0 - U ) * ( PWS / PB ) );
#ifdef EP_psych_stats
		++NumTimesCalled( iPsyRhFnTdbWPb );
#endif

		//                   VALIDITY TEST
		if ( RHValue < 0.0 || RHValue > 1.0 ) {
			if ( RHValue > 1.0 ) {
#ifdef EP_psych_errors
				if ( RHValue > 1.01 ) {
					if ( ! WarmupFlag ) {
						if ( iPsyErrIndex( iPsyRhFnTdbWPb ) == 0 ) {
							String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( W, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100., 2 );
							ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb) " );
							if ( present( CalledFrom ) ) {
								ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
							} else {
								ShowContinueErrorTimeStamp( " Routine=Unknown," );
							}
							ShowContinueError( String );
							ShowContinueError( "Relative Humidity being reset to 100.0%" );
						}
						ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb)", iPsyErrIndex( iPsyRhFnTdbWPb ), RHValue * 100., RHValue * 100., _, "%", "%" );
					}
				}
#endif
				RHValue = 1.0;
			} else { // RHValue < 0.0
#ifdef EP_psych_errors
				if ( RHValue < -0.05 ) {
					if ( ! WarmupFlag ) {
						if ( iPsyErrIndex( iPsyRhFnTdbWPb ) == 0 ) {
							String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( W, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100., 2 );
							ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb) " );
							if ( present( CalledFrom ) ) {
								ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
							} else {
								ShowContinueErrorTimeStamp( " Routine=Unknown," );
							}
							ShowContinueError( String );
							ShowContinueError( "Relative Humidity being reset to 1%" );
						}
						ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb)", iPsyErrIndex( iPsyRhFnTdbWPb ), RHValue * 100., RHValue * 100., _, "%", "%" );
					}
				}
#endif
				RHValue = .01;
			}
		} // RHValue in proper range

		// RHValue is the result

		return RHValue;
	}

#ifdef EP_cache_PsyTwbFnTdbWPb

	Real64
	PsyTwbFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const W, // humidity ratio
		Real64 const Pb, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie/Amir Roth
		//       DATE WRITTEN   August 2011
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provide a "cache" of results for the given arguments and wetbulb (twb) output result.

		// METHODOLOGY EMPLOYED:
		// Use grid shifting and masking to provide hash into the cache. Use Equivalence to
		// make Fortran ignore "types".

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Twb_result; // result=> Temperature Wet-Bulb {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Int64 const Grid_Shift( ( 64 - 12 - twbprecision_bits ) );

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Int64 Tdb_tag;
		Int64 W_tag;
		Int64 Pb_tag;
		Int64 hash;
		Real64 Tdb_tag_r;
		Real64 W_tag_r;
		Real64 Pb_tag_r;

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTwbFnTdbWPb_cache );
#endif

		Tdb_tag = TRANSFER( Tdb, Tdb_tag );
		W_tag = TRANSFER( W, W_tag );
		Pb_tag = TRANSFER( Pb, Pb_tag );

		Tdb_tag = bit::bit_shift( Tdb_tag, -Grid_Shift );
		W_tag = bit::bit_shift( W_tag, -Grid_Shift );
		Pb_tag = bit::bit_shift( Pb_tag, -Grid_Shift );
		hash = bit::bit_and( bit::bit_xor( Tdb_tag, bit::bit_xor( W_tag, Pb_tag ) ), Int64( twbcache_size - 1 ) );

		if ( cached_Twb( hash ).iTdb != Tdb_tag || cached_Twb( hash ).iW != W_tag || cached_Twb( hash ).iPb != Pb_tag ) {
			cached_Twb( hash ).iTdb = Tdb_tag;
			cached_Twb( hash ).iW = W_tag;
			cached_Twb( hash ).iPb = Pb_tag;

			Tdb_tag_r = TRANSFER( bit::bit_shift( Tdb_tag, Grid_Shift ), Tdb_tag_r );
			W_tag_r = TRANSFER( bit::bit_shift( W_tag, Grid_Shift ), W_tag_r );
			Pb_tag_r = TRANSFER( bit::bit_shift( Pb_tag, Grid_Shift ), Pb_tag_r );

			if ( present( CalledFrom ) ) {
				cached_Twb( hash ).Twb = PsyTwbFnTdbWPb_raw( Tdb_tag_r, W_tag_r, Pb_tag_r, CalledFrom );
			} else {
				cached_Twb( hash ).Twb = PsyTwbFnTdbWPb_raw( Tdb_tag_r, W_tag_r, Pb_tag_r );
			}
		}

		//  Twbresult_last = cached_Twb(hash)%Twb
		//  Twb_result = Twbresult_last
		Twb_result = cached_Twb( hash ).Twb;

		return Twb_result;

	}

	Real64
	PsyTwbFnTdbWPb_raw(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)

#else

	Real64
	PsyTwbFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
#endif
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  Dec 2003; Rahul Chillar
		//                      2011; as time saving measure, cache some values.

		// PURPOSE OF THIS FUNCTION:
		// This function provides the wet-bulb temperature from dry-bulb temperature,
		// humidity ratio and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// Uses an Iterative procedure to calculate WetBulbTemperature

		// REFERENCES:
		// na

		// Using/Aliasing
		using General::Iterate;

		// Return value
		Real64 TWB; // result=> Temperature Wet-Bulb {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const itmax( 100 ); // Maximum No of Iterations
		static Real64 convTol( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 tBoil; // Boiling temperature of water at given pressure
		static Real64 last_Patm( -99999. ); // barometric pressure {Pascals}  (last)
		static Real64 last_tBoil( -99999. ); // Boiling temperature of water at given pressure (last)
		Real64 newW; // Humidity ratio calculated with wet bulb guess
		Real64 W; // Humidity ratio entered and corrected as necessary
		Real64 ResultX; // ResultX is the final Iteration result passed back to the calling routine
		Real64 WBT; // Current Value of WetBulbTemperature
		Real64 error; // Deviation of dependent variable in iteration
		Real64 X1; // Independent variable in ITERATE
		Real64 Y1; // Dependent variable in ITERATE
		Real64 Wstar; // Humidity  ratio as a function of Sat Press of Wet Bulb
		Real64 PSatstar; // Saturation pressure at wet bulb temperature
		int iter; // Iteration counter
		int icvg; // Iteration convergence flag
		bool FlagError; // set when errors should be flagged

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTwbFnTdbWPb );
#endif

		// CHECK TDB IN RANGE.
		FlagError = false;
#ifdef EP_psych_errors
		if ( TDB <= -100.0 || TDB >= 200.0 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyTwbFnTdbWPb ) == 0 ) {
					ShowWarningMessage( "Temperature out of range [-100. to 200.] (PsyTwbFnTdbWPb)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( " Input Temperature=" + TrimSigDigits( TDB, 2 ) );
					FlagError = true;
				}
				ShowRecurringWarningErrorAtEnd( "Temperature out of range [-100. to 200.] (PsyTwbFnTdbWPb)", iPsyErrIndex( iPsyTwbFnTdbWPb ), TDB, TDB, _, "C", "C" );
			}
		}
#endif

		W = dW;
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			if ( W <= -.0001 ) {
				if ( ! WarmupFlag ) {
					if ( iPsyErrIndex( iPsyTwbFnTdbWPb2 ) == 0 ) {
						String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( W, 3 ) + " Pressure= " + TrimSigDigits( Patm, 2 );
						ShowWarningMessage( "Entered Humidity Ratio invalid (PsyTwbFnTdbWPb)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						ShowContinueError( String );
						String = "Humidity Ratio= " + TrimSigDigits( W, 4 );
						ShowContinueError( String + " ... Humidity Ratio set to .00001" );
					}
					ShowRecurringWarningErrorAtEnd( "Entered Humidity Ratio invalid (PsyTwbFnTdbWPb)", iPsyErrIndex( iPsyTwbFnTdbWPb2 ), W, W, _, "[]", "[]" );
				}
			}
#endif
			W = 1.e-5;
		}

		// Initial temperature guess at atmospheric pressure
		if ( present( CalledFrom ) ) {
			if ( Patm != last_Patm ) {
				tBoil = PsyTsatFnPb( Patm, CalledFrom );
				last_Patm = Patm;
				last_tBoil = tBoil;
			} else {
				tBoil = last_tBoil;
			}
		} else {
			if ( Patm != last_Patm ) {
				tBoil = PsyTsatFnPb( Patm, "PsyTwbFnTdbWPb" );
				last_Patm = Patm;
				last_tBoil = tBoil;
			} else {
				tBoil = last_tBoil;
			}
		}

		// Set initial guess of WetBulbTemp=Entering Dry Bulb Temperature
		WBT = TDB;

		// Initializing  value for iter
		iter = 0;

		// Begin iteration loop
		for ( iter = 1; iter <= itmax; ++iter ) {

			// Assigning a value to WBT
			if ( WBT >= ( tBoil - 0.09 ) ) WBT = tBoil - 0.1;

			// Determine the saturation pressure for wet bulb temperature
			if ( present( CalledFrom ) ) {
				PSatstar = PsyPsatFnTemp( WBT, CalledFrom );
			} else {
				PSatstar = PsyPsatFnTemp( WBT, "PsyTwbFnTdbWPb" );
			}

			// Determine humidity ratio for given saturation pressure
			Wstar = 0.62198 * PSatstar / ( Patm - PSatstar );

			// Calculate new humidity ratio and determine difference from known
			// humidity ratio which is wStar calculated earlier
			newW = ( ( 2501.0 - 2.381 * WBT ) * Wstar - ( TDB - WBT ) ) / ( 2501.0 + 1.805 * TDB - 4.186 * WBT );

			// Check error, if not satisfied, calculate new guess and iterate
			error = W - newW;

			// Using Iterative Procedure to Calculate WetBulb
			Iterate( ResultX, convTol, WBT, error, X1, Y1, iter, icvg );
			WBT = ResultX;

			// If converged, leave iteration loop.
			if ( icvg == 1 ) break;

			// Error Trap for the Discontinious nature of PsyPsatFnTemp function (Sat Press Curve) at ~0 Deg C.
			if ( ( PSatstar > 611.000 ) && ( PSatstar < 611.25 ) && ( std::abs( error ) <= 0.0001 ) && ( iter > 4 ) ) break;

		} // End of Iteration Loop

#ifdef EP_psych_stats
		NumIterations( iPsyTwbFnTdbWPb ) += iter;
#endif

		// Wet bulb temperature has not converged after maximum specified
		// iterations. Print error message, set return error flag, and RETURN
#ifdef EP_psych_errors
		if ( iter > itmax ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyTwbFnTdbWPb3 ) == 0 ) {
					ShowWarningMessage( "WetBulb not converged after " + TrimSigDigits( iter ) + " iterations(PsyTwbFnTdbWPb)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( " Input Temperature = " + TrimSigDigits( TDB, 2 ) );
					ShowContinueError( " Input Humidity Ratio= " + TrimSigDigits( W, 6 ) );
					ShowContinueError( " Input Pressure = " + TrimSigDigits( Patm, 2 ) );
					FlagError = true;
				}
				ShowRecurringWarningErrorAtEnd( "WetBulb not converged after max iterations(PsyTwbFnTdbWPb)", iPsyErrIndex( iPsyTwbFnTdbWPb3 ) );
			}
		}
#endif

		//Result is Temperature Wet Bulb
		TWB = WBT;

#ifdef EP_psych_errors
		if ( FlagError ) {
			ShowContinueError( " Resultant Temperature= " + TrimSigDigits( WBT, 2 ) );
		}
#endif

		// If (TempWetBulb)>(Dry Bulb Temp) , Setting (TempWetBulb)=(DryBulbTemp).
		if ( TWB > TDB ) {
			TWB = TDB;
		}

#ifdef generatetestdata
		gio::write( OutputFileDebug, "*" ) << TDB << dW << Patm << Twb;
#endif

		return TWB;
	}

	Real64
	PsyVFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the specific volume from dry-bulb temperature,
		// humidity ratio and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 28

		// USE STATEMENTS:
		// na

		// Return value
		Real64 V; // result=> specific volume {m3/kg}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 w; // humidity ratio

		w = max( dW, 1.0e-5 );
		V = 1.59473e2 * ( 1.0 + 1.6078 * w ) * ( 1.8 * TDB + 492.0 ) / PB;

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyVFnTdbWPb );
#endif

		//                                      VALIDITY TEST.
		if ( V < 0.0 ) {
#ifdef EP_psych_errors
			if ( V <= -.01 ) {
				if ( ! WarmupFlag ) {
					if ( iPsyErrIndex( iPsyVFnTdbWPb ) == 0 ) {
						String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( w, 3 ) + " Pressure= " + TrimSigDigits( PB, 2 );
						ShowWarningMessage( "Calculated Specific Volume out of range (PsyVFnTdbWPb)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						ShowContinueError( String );
						String = "Calculated Volume= " + TrimSigDigits( V, 3 );
						ShowContinueError( String + " ... Since Calculated Volume < 0.0, it is set to .83" );
					}
					ShowRecurringWarningErrorAtEnd( "Calculated Specific Volume out of range (PsyVFnTdbWPb)", iPsyErrIndex( iPsyVFnTdbWPb ), V, V, _, "m3/kg", "m3/kg" );
				}
			}
			V = 0.83;
#endif
		}

		// V is the result

		return V;
	}

	Real64
	PsyWFnTdpPb(
		Real64 const TDP, // dew-point temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the humidity ratio from dew-point temperature
		// and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

		// USE STATEMENTS:

		// Return value
		Real64 W; // result=> humidity ratio

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 PDEW; // saturation pressure at dew-point temperature {Pascals}

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdpPb );
#endif

		if ( present( CalledFrom ) ) {
			PDEW = PsyPsatFnTemp( TDP, CalledFrom );
		} else {
			PDEW = PsyPsatFnTemp( TDP, "PsyWFnTdpPb" );
		}
		W = PDEW * 0.62198 / ( PB - PDEW );
		//                                      VALIDITY TEST.
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			if ( W <= -.0001 ) {
				if ( ! WarmupFlag ) {
					if ( iPsyErrIndex( iPsyWFnTdpPb ) == 0 ) {
						String = " Dew-Point= " + TrimSigDigits( TDP, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
						ShowWarningMessage( "Calculated Humidity Ratio invalid (PsyWFnTdpPb)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						ShowContinueError( String );
						String = "Calculated Humidity Ratio= " + TrimSigDigits( W, 4 );
						ShowContinueError( String + " ... Humidity Ratio set to .00001" );
					}
					ShowRecurringWarningErrorAtEnd( "Entered Humidity Ratio invalid (PsyWFnTdpPb)", iPsyErrIndex( iPsyWFnTdpPb ), W, W, _, "[]", "[]" );
				}
			}
#endif
			W = 1.e-5;
		}

		// W is the result

		return W;
	}

	Real64
	PsyWFnTdbH(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const H, // enthalpy {J/kg}
		Optional_string_const CalledFrom, // routine this function was called from (error messages)
		Optional_bool_const SuppressWarnings // if calling function is calculating an intermediate state
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the humidity ratio from dry-bulb temperature
		// and enthalpy.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

		// USE STATEMENTS:

		// Return value
		Real64 W; // result=> humidity ratio

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool ReportWarnings;

		ReportWarnings = true;

		if ( present( SuppressWarnings ) ) {
			if ( SuppressWarnings ) ReportWarnings = false;
		}

		//CP-------- here is 1.2, 1200., 1.004, or 1004.  --------
		W = ( H - 1.00484e3 * TDB ) / ( 2.50094e6 + 1.85895e3 * TDB );
#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdbH );
#endif

		//                                      VALIDITY TEST.
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			if ( W < -.0001 ) {
				if ( ! WarmupFlag && ReportWarnings ) {
					if ( iPsyErrIndex( iPsyWFnTdbH ) == 0 ) {
						String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Enthalpy= " + TrimSigDigits( H, 3 );
						ShowWarningMessage( "Calculated Humidity Ratio invalid (PsyWFnTdbH)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						ShowContinueError( String );
						String = "Calculated Humidity Ratio= " + TrimSigDigits( W, 4 );
						ShowContinueError( String + " ... Humidity Ratio set to .00001" );
					}
					ShowRecurringWarningErrorAtEnd( "Calculated Humidity Ratio invalid (PsyWFnTdbH)", iPsyErrIndex( iPsyWFnTdbH ), W, W, _, "[]", "[]" );
				}
			}
#endif
			W = 1.e-5;
		}

		// W is the result

		return W;
	}

	Real64
	PsyWFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWBin, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the humidity ratio from dry-bulb temperature,
		// wet-bulb temperature and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQ 22,35

		// USE STATEMENTS:

		// Return value
		Real64 W; // result=> humidity ratio

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 PWET; // Pressure at wet-bulb temperature {Pascals}
		Real64 WET; // Humidity ratio at wet-bulb temperature
		Real64 TWB; // test wet-bulb temperature

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdbTwbPb );
#endif

		//                                      VALIDITY CHECK.
		TWB = TWBin;
		if ( TWB > TDB ) {
#ifdef EP_psych_errors
			if ( TWB > ( TDB + 0.01 ) ) {
				if ( ReportErrors && ! WarmupFlag ) {
					if ( iPsyErrIndex( iPsyWFnTdbTwbPb ) == 0 ) {
						String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
						ShowWarningMessage( "Given Wet Bulb Temperature invalid (PsyWFnTdbTwbPb)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						ShowContinueError( String );
						String = "Calculated Wet-Bulb= " + TrimSigDigits( TWB, 2 );
						ShowContinueError( String + " ... Since Dry Bulb < Wet Bulb, Wet Bulb set = to Dry Bulb" );
					}
					ShowRecurringWarningErrorAtEnd( "Given Wet Bulb Temperature invalid (PsyWFnTdbTwbPb)", iPsyErrIndex( iPsyWFnTdbTwbPb ), TWB, TWB, _, "C", "C" );
				}
			}
#endif
			TWB = TDB;
		}
		//                                      CALCULATION.
		if ( present( CalledFrom ) ) {
			PWET = PsyPsatFnTemp( TWB, CalledFrom );
		} else {
			PWET = PsyPsatFnTemp( TWB, "PsyWFnTdbTwbPb" );
		}
		WET = 0.62198 * PWET / ( PB - PWET );

		W = ( ( 2501.0 - 2.381 * TWB ) * WET - ( TDB - TWB ) ) / ( 2501.0 + 1.805 * TDB - 4.186 * TWB );
		//                                      VALIDITY CHECK.
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			if ( ReportErrors && ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyWFnTdbTwbPb2 ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Wet-Bulb= " + TrimSigDigits( TWB, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowWarningMessage( "Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					String = "Calculated Humidity Ratio= " + TrimSigDigits( W, 4 ) + ", will recalculate Humidity Ratio";
					ShowContinueError( String + " using Relative Humidity .01% (and Dry-Bulb and Pressure as shown)" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)", iPsyErrIndex( iPsyWFnTdbTwbPb2 ), W, W, _, "[]", "[]" );
			}
#endif
			W = PsyWFnTdbRhPb( TDB, 0.0001, PB, CalledFrom );
		}

		// W is the result

		return W;
	}

	Real64
	PsyWFnTdbRhPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the humidity ratio from dry-bulb temperature,
		// relative humidty (value) and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

		// USE STATEMENTS:

		// Return value
		Real64 W; // result=> humidity ratio

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 PDRY; // Pressure at dry-bulb temperature {Pascals}
		Real64 PDEW; // Pressure at dew-point temperature {Pascals}

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdbRhPb );
#endif

		if ( present( CalledFrom ) ) {
			PDRY = PsyPsatFnTemp( TDB, CalledFrom );
		} else {
			PDRY = PsyPsatFnTemp( TDB, "PsyWFnTdbRhPb" );
		}

		PDEW = RH * PDRY;

		//Numeric error check when the temperature and RH values cause Pdew to equal or exceed
		//barometric pressure which is physically impossible. An approach limit of 1000 pascals
		//was chosen to keep the numerics stable as the denominator approaches 0.
		if ( ( PB - PDEW ) <= 1000.0 ) {
			W = PDEW * 0.62198 / 1000.0;
		} else {
			W = PDEW * 0.62198 / ( PB - PDEW );
		}

		//                                      THIS EQUATION IN SI UNIT IS FROM
		//                                      VALIDITY TEST.
		//                                      ASHRAE HANDBOOK OF FUNDAMENTALS
		//                                      PAGE 99  EQUATION 22
		if ( W < 1.0e-5 ) {
#ifdef EP_psych_errors
			if ( W <= -.0001 ) {
				if ( ! WarmupFlag ) {
					if ( iPsyErrIndex( iPsyWFnTdbRhPb ) == 0 ) {
						String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Relative Humidity [%]= " + TrimSigDigits( RH * 100., 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
						ShowWarningMessage( "Calculated Humidity Ratio is invalid (PsyWFnTdbRhPb)" );
						if ( present( CalledFrom ) ) {
							ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
						} else {
							ShowContinueErrorTimeStamp( " Routine=Unknown," );
						}
						ShowContinueError( String );
						String = "Calculated Humidity Ratio= " + TrimSigDigits( W, 4 );
						ShowContinueError( String + " ... Humidity Ratio set to .00001" );
					}
					ShowRecurringWarningErrorAtEnd( "Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)", iPsyErrIndex( iPsyWFnTdbRhPb ), W, W, _, "[]", "[]" );
				}
			}
#endif
			W = 1.e-5;
		}

		// W is the result

		return W;
	}

#ifdef EP_cache_PsyPsatFnTemp

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         Linda Lawrie
		//       DATE WRITTEN   March 2013
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// Provide a "cache" of results for the given argument (T) and pressure (Pascal) output result.

		// METHODOLOGY EMPLOYED:
		// Use grid shifting and masking to provide hash into the cache. Use Equivalence to
		// make Fortran ignore "types".

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 Pascal; // result=> saturation pressure {Pascals}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Int64 const Grid_Shift( ( 64 - 12 - psatprecision_bits ) );
		//  integer(i64), parameter :: Grid_Mask=NOT(ISHFT(1_i64, Grid_Shift)-1)

		// INTERFACE BLOCK SPECIFICATIONS:
		// na

		// DERIVED TYPE DEFINITIONS:
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Int64 Tdb_tag;
		Int64 hash;
		Real64 Tdb_tag_r;

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyPsatFnTemp_cache );
#endif

		Tdb_tag = TRANSFER( T, Tdb_tag );

		Tdb_tag = bit::bit_shift( Tdb_tag, -Grid_Shift );
		hash = bit::bit_and( Tdb_tag, Int64( psatcache_size - 1 ) );

		if ( cached_Psat( hash ).iTdb != Tdb_tag ) {
			cached_Psat( hash ).iTdb = Tdb_tag;
			Tdb_tag_r = TRANSFER( bit::bit_shift( Tdb_tag, Grid_Shift ), Tdb_tag_r );

			if ( present( CalledFrom ) ) {
				cached_Psat( hash ).Psat = PsyPsatFnTemp_raw( Tdb_tag_r, CalledFrom );
			} else {
				cached_Psat( hash ).Psat = PsyPsatFnTemp_raw( Tdb_tag_r );
			}
		}

		Pascal = cached_Psat( hash ).Psat;

		return Pascal;

	}

	Real64
	PsyPsatFnTemp_raw(
		Real64 const T, // dry-bulb temperature {C}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)

#else

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
#endif
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       NA
		//       RE-ENGINEERED  Nov 2003; Rahul Chillar

		// PURPOSE OF THIS FUNCTION:
		// This function provides the saturation pressure as a function of temperature.

		// METHODOLOGY EMPLOYED:
		// Hyland & Wexler Formulation, range -100C to 200C

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 2005, Chap 6 (Psychrometrics), Eqn 5 & 6.
		// Compared to Table 3 values (August 2007) with average error of 0.00%, max .30%,
		// min -.39%.  (Spreadsheet available on request - Lawrie).

		// USE STATEMENTS:

		// Return value
		Real64 Pascal; // result=> saturation pressure {Pascals}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		Real64 const C1( -5674.5359 ); // Coefficient for TKel < KelvinConvK
		Real64 const C2( 6.3925247 ); // Coefficient for TKel < KelvinConvK
		Real64 const C3( -0.9677843e-2 ); // Coefficient for TKel < KelvinConvK
		Real64 const C4( 0.62215701e-6 ); // Coefficient for TKel < KelvinConvK
		Real64 const C5( 0.20747825e-8 ); // Coefficient for TKel < KelvinConvK
		Real64 const C6( -0.9484024e-12 ); // Coefficient for TKel < KelvinConvK
		Real64 const C7( 4.1635019 ); // Coefficient for TKel < KelvinConvK

		Real64 const C8( -5800.2206 ); // Coefficient for TKel >= KelvinConvK
		Real64 const C9( 1.3914993 ); // Coefficient for TKel >= KelvinConvK
		Real64 const C10( -0.048640239 ); // Coefficient for TKel >= KelvinConvK
		Real64 const C11( 0.41764768e-4 ); // Coefficient for TKel >= KelvinConvK
		Real64 const C12( -0.14452093e-7 ); // Coefficient for TKel >= KelvinConvK
		Real64 const C13( 6.5459673 ); // Coefficient for TKel >= KelvinConvK
#ifdef EP_IF97
		//Table 34 in IF97
		Real64 const N1( 0.11670521452767e04 );
		Real64 const N2( -0.72421316703206e06 );
		Real64 const N3( -0.17073846940092e02 );
		Real64 const N4( 0.12020824702470e05 );
		Real64 const N5( -0.32325550322333e07 );
		Real64 const N6( 0.14915108613530e02 );
		Real64 const N7( -0.48232657361591e04 );
		Real64 const N8( 0.40511340542057e06 );
		Real64 const N9( -0.23855557567849 );
		Real64 const N10( 0.65017534844798e03 );
#endif

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 Tkel; // Dry-bulb in REAL(r64) for function passing
#ifdef EP_IF97
		Real64 phi; // IF97 equation 29b
		Real64 phi2; // phi squared
		Real64 A; // IF97 equation 30
		Real64 B; // IF97 equation 30
		Real64 C; // IF97 equation 30
#endif

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyPsatFnTemp );
#endif

		// CHECK T IN RANGE.
#ifdef EP_psych_errors
		if ( T <= -100.0 || T >= 200.0 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyPsatFnTemp ) == 0 ) {
					ShowWarningMessage( "Temperature out of range [-100. to 200.] (PsyPsatFnTemp)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( " Input Temperature=" + TrimSigDigits( T, 2 ) );
				}
				ShowRecurringWarningErrorAtEnd( "Temperature out of range [-100. to 200.] (PsyPsatFnTemp)", iPsyErrIndex( iPsyPsatFnTemp ), T, T, _, "C", "C" );
			}
		}
#endif

		// Convert temperature from Centigrade to Kelvin.
		Tkel = T + KelvinConv;

		// If below freezing, calculate saturation pressure over ice.
		if ( ( Tkel < KelvinConv ) && ( Tkel >= 173.15 ) ) {
			Pascal = std::exp( C1 / Tkel + C2 + Tkel * ( C3 + Tkel * ( C4 + Tkel * ( C5 + C6 * Tkel ) ) ) + C7 * std::log( Tkel ) );

			// If below -100C,set value of Pressure corresponding to Saturation Temperature of -100C.
		} else if ( ( Tkel < 173.15 ) ) {
			Pascal = 0.0017;

			// If above freezing, calculate saturation pressure over liquid water.
		} else if ( ( Tkel >= KelvinConv ) && ( Tkel <= 473.15 ) ) {
#ifndef EP_IF97
			Pascal = std::exp( C8 / Tkel + C9 + Tkel * ( C10 + Tkel * ( C11 + Tkel * C12 ) ) + C13 * std::log( Tkel ) );
#else
			//         !IF97 equations
			phi = Tkel + N9 / ( Tkel - N10 );
			phi2 = phi * phi;
			A = phi2 + N1 * phi + N2;
			B = N3 * phi2 + N4 * phi + N5;
			C = N6 * phi2 + N7 * phi + N8;
			Pascal = 1000000. * std::pow( ( ( 2. * C ) / ( -B + std::sqrt( std::pow( B, 2 ) - 4. * A * C ) ) ), 4 );
#endif
			// If above 200C, set value of Pressure corresponding to Saturation Temperature of 200C.
		} else if ( ( Tkel > 473.15 ) ) {
			Pascal = 1555000.0;

		} else {
			// bad temperature.  Use 0.0 C
#ifdef EP_psych_errors
			ShowSevereError( "PsyPsatFnTemp -- Bad input temperature=" + TrimSigDigits( T, 2 ) );
			if ( present( CalledFrom ) ) {
				ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
			} else {
				ShowContinueErrorTimeStamp( " Routine=Unknown," );
			}
			ShowFatalError( " Program terminates due to preceding conditions." );
#else
			std::cerr << "Program terminated with Exit Code " << "PsyPsatFnTemp" << std::endl; std::exit( EXIT_FAILURE );
#endif
		}

		return Pascal;
	}

	Real64
	PsyTsatFnHPb(
		Real64 const H, // enthalpy {J/kg}
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       July 2003; LKL -- peg min/max values (outside range of functions)
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides the saturation temperature from the enthalpy
		// and barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

		// USE STATEMENTS:

		// Return value
		Real64 T; // result=> saturation temperature {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 T1; // APPROXIMATE SATURATION TEMPERATURE (C)
		Real64 T2; // APPROXIMATE SATURATION TEMPERATURE (C)
		Real64 TN; // NEW ASSUMED SATURATION TEMPERATURE (C)
		Real64 H1; // APPROXIMATE ENTHALPY (J/KG)
		Real64 H2; // APPROXIMATE ENTHALPY (J/KG)
		Real64 Y1; // ERROR IN ENTHALPY
		Real64 Y2; // ERROR IN ENTHALPY
		int IterCount;
		Real64 HH; // temporary enthalpy (calculation) value
		bool FlagError; // Set when errors should be flagged
		Real64 Hloc; // local value of H

		//                                      CHECK H IN RANGE.
		HH = H + 1.78637e4;

		if ( H >= 0.0 ) {
			Hloc = max( 0.00001, H );
		} else if ( H < 0.0 ) {
			Hloc = min( -.00001, H );
		}

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTsatFnHPb );
#endif

		FlagError = false;
#ifdef EP_psych_errors
		if ( HH <= -4.24E4 || HH >= 4.5866E7 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyTsatFnHPb ) == 0 ) {
					ShowWarningMessage( "Enthalpy out of range (PsyTsatFnHPb)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					String = " Enthalpy=" + TrimSigDigits( HH, 5 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowContinueError( String );
					FlagError = true;
				}
				ShowRecurringWarningErrorAtEnd( "Enthalpy out of range (PsyTsatFnHPb)", iPsyErrIndex( iPsyTsatFnHPb ), HH, HH, _, "J/kg", "J/kg" );
			}
		}
#endif
		if ( HH > 7.5222e4 ) goto Label20;
		if ( HH > 2.7297e4 ) goto Label60;
		if ( HH > -6.7012e2 ) goto Label50;
		if ( HH > -2.2138e4 ) goto Label40;
		if ( HH < -4.24e4 ) HH = -4.24e4; // Peg to minimum
		goto Label30;
Label20: ;
		if ( HH < 1.8379e5 ) goto Label70;
		if ( HH < 4.7577e5 ) goto Label80;
		if ( HH < 1.5445e6 ) goto Label90;
		if ( HH < 3.8353e6 ) goto Label100;
		if ( HH > 4.5866e7 ) HH = 4.5866e7; // Peg to maximum
		goto Label110;
		//                                      TEMP. IS FROM -60 C  TO  -40 C
Label30: ;
		T = F6( HH, -19.44, 8.53675e-4, -5.12637e-9, -9.85546e-14, -1.00102e-18, -4.2705e-24 );
		goto Label120;
		//                                      TEMP. IS FROM -40 C  TO  -20 C
Label40: ;
		T = F6( HH, -1.94224e1, 8.5892e-4, -4.50709e-9, -6.19492e-14, 8.71734e-20, 8.73051e-24 );
		goto Label120;
		//                                      TEMP. IS FROM -20 C  TO    0 C
Label50: ;
		T = F6( HH, -1.94224e1, 8.59061e-4, -4.4875e-9, -5.76696e-14, 7.72217e-19, 3.97894e-24 );
		goto Label120;
		//                                      TEMP. IS FROM   0 C  TO   20 C
Label60: ;
		T = F6( HH, -2.01147e1, 9.04936e-4, -6.83305e-9, 2.3261e-14, 7.27237e-20, -6.31939e-25 );
		goto Label120;
		//                                      TEMP. IS FROM  20 C  TO   40 C
Label70: ;
		T = F6( HH, -1.82124e1, 8.31683e-4, -6.16461e-9, 3.06411e-14, -8.60964e-20, 1.03003e-25 );
		goto Label120;
		//                                      TEMP. IS FROM  40 C  TO   60 C
Label80: ;
		T = F6( HH, -1.29419, 3.88538e-4, -1.30237e-9, 2.78254e-15, -3.27225e-21, 1.60969e-27 );
		goto Label120;
		//                                      TEMP. IS FROM   60 C TO   80 C
Label90: ;
		T = F6( HH, 2.39214e1, 1.27519e-4, -1.52089e-10, 1.1043e-16, -4.33919e-23, 7.05296e-30 );
		goto Label120;
		//                                      TEMP. IS FROM   80 C TO   90 C
Label100: ;
		T = F6( HH, 4.88446e1, 3.85534e-5, -1.78805e-11, 4.87224e-18, -7.15283e-25, 4.36246e-32 );
		goto Label120;
		//                                      TEMP. IS FROM   90 C TO  100C
Label110: ;
		T = F7( HH, 7.60565e11, 5.80534e4, -7.36433e-3, 5.11531e-10, -1.93619e-17, 3.70511e-25, -2.77313e-33 );
		//                                      IF THE BAROMETRIC PRESSURE IS
		//                                      EQUAL TO 1.0133E5 , SATURATION
		//                                      TEMP. IS CALCULATED BY ABOVE EQUA
		//                                      OTHERWISE TEMP. IS COMPUTED BY
		//                                      FOLLOWING ITERATION METHOD
Label120: ;
#ifdef EP_psych_errors
		if ( FlagError ) {
			ShowContinueError( " Initial Resultant Temperature= " + TrimSigDigits( T, 2 ) );
		}
#endif
		if ( std::abs( PB - 1.0133e5 ) / 1.0133e5 <= 0.01 ) goto Label170;
		IterCount = 0;
		T1 = T;
		H1 = PsyHFnTdbW( T1, PsyWFnTdbTwbPb( T1, T1, PB ) );
		Y1 = H1 - Hloc;
		if ( std::abs( Y1 / Hloc ) <= 0.1e-4 ) goto Label140;
		T2 = T1 * 0.9;
Label130: ;
		++IterCount;
		H2 = PsyHFnTdbW( T2, PsyWFnTdbTwbPb( T2, T2, PB ) );
		Y2 = H2 - Hloc;
		if ( std::abs( Y2 / Hloc ) <= 0.1e-4 ) goto Label150;
		if ( Y2 == Y1 ) goto Label150;
		TN = T2 - Y2 / ( Y2 - Y1 ) * ( T2 - T1 );
		if ( IterCount > 30 ) goto Label160;
		T1 = T2;
		T2 = TN;
		Y1 = Y2;
		goto Label130;
Label140: ;
		T = T1;
		goto Label170;
Label150: ;
		T = T2;
		goto Label170;
Label160: ;
#ifdef EP_psych_errors
		if ( FlagError ) {
			ShowSevereError( "Temperature did not converge (PsyTsatFnHPb)" );
			if ( present( CalledFrom ) ) {
				ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
			} else {
				ShowContinueErrorTimeStamp( " Routine=Unknown," );
			}
			String = " Enthalpy=" + TrimSigDigits( HH, 5 ) + " Pressure= " + TrimSigDigits( PB, 2 );
			ShowContinueError( String + " Last T=" + TrimSigDigits( T, 2 ) );
		}
#endif
Label170: ;

		//   result is T

		return T;

	}

	Real64
	F6(
		Real64 const X,
		Real64 const A0,
		Real64 const A1,
		Real64 const A2,
		Real64 const A3,
		Real64 const A4,
		Real64 const A5
	)
	{

		// Return value
		Real64 F6;

		F6 = A0 + X * ( A1 + X * ( A2 + X * ( A3 + X * ( A4 + X * A5 ) ) ) );
		return F6;
	}

	Real64
	F7(
		Real64 const X,
		Real64 const A0,
		Real64 const A1,
		Real64 const A2,
		Real64 const A3,
		Real64 const A4,
		Real64 const A5,
		Real64 const A6
	)
	{

		// Return value
		Real64 F7;

		F7 = ( A0 + X * ( A1 + X * ( A2 + X * ( A3 + X * ( A4 + X * ( A5 + X * A6 ) ) ) ) ) ) / 1.0E10;
		return F7;
	}

	Real64
	PsyTsatFnPb(
		Real64 const Press, // barometric pressure {Pascals}
		Optional_string_const CalledFrom // routine this function was called from (error messages)
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       RE-ENGINEERED  Dec 2003; Rahul Chillar

		// PURPOSE OF THIS FUNCTION:
		// This function provides the saturation temperature from barometric pressure.

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// 1989 ASHRAE Handbook - Fundamentals
		// Checked against 2005 HOF, Chap 6, Table 3 (using pressure in, temperature out) with
		// good correlation from -60C to 160C

		// Using/Aliasing
		using General::Iterate;

		// Return value
		Real64 Temp; // result=> saturation temperature {C}

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const itmax( 50 ); // Maximum number of iterations
		static Real64 convTol( 0.0001 );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool FlagError; // set when errors should be flagged
		static Real64 Press_Save( -99999. );
		static Real64 tSat_Save( -99999. );
		Real64 tSat; // Water temperature guess
		Real64 pSat; // Pressure corresponding to temp. guess
		Real64 error; // Deviation of dependent variable in iteration
		Real64 X1; // Previous value of independent variable in ITERATE
		Real64 Y1; // Previous value of dependent variable in ITERATE
		Real64 ResultX; // ResultX is the final Iteration result passed back to the calling routine
		int iter; // Iteration counter
		int icvg; // Iteration convergence flag

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTsatFnPb );
#endif

		// Check press in range.
		FlagError = false;
#ifdef EP_psych_errors
		if ( Press <= 0.0017 || Press >= 1555000. ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyTsatFnPb ) == 0 ) {
					ShowWarningMessage( "Pressure out of range (PsyTsatFnPb)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( " Input Pressure= " + TrimSigDigits( Press, 2 ) );
					FlagError = true;
				}
				ShowRecurringWarningErrorAtEnd( "Pressure out of range (PsyTsatFnPb)", iPsyErrIndex( iPsyTsatFnPb ), Press, Press, _, "Pa", "Pa" );
			}
		}
#endif
		if ( Press == Press_Save ) {
			Temp = tSat_Save;
			return Temp;
		}
		Press_Save = Press;

		// Uses an iterative process to determine the saturation temperature at a given
		// pressure by correlating saturated water vapor as a function of temperature.

		// Initial guess of boiling temperature
		tSat = 100.0;
		iter = 0;

		// If above 1555000,set value of Temp corresponding to Saturation Pressure of 1555000 Pascal.
		if ( Press >= 1555000. ) {
			tSat = 200.0;
			// If below 0.0017,set value of Temp corresponding to Saturation Pressure of 0.0017 Pascal.
		} else if ( Press <= 0.0017 ) {
			tSat = -100.0;

			// Setting Value of PsyTsatFnPb= 0C, due to non-continuous function for Saturation Pressure at 0C.
		} else if ( ( Press > 611.000 ) && ( Press < 611.25 ) ) {
			tSat = 0.0;

		} else {
			// Iterate to find the saturation temperature
			// of water given the total pressure

			// Set iteration loop parameters
			// make sure these are initialized
			for ( iter = 1; iter <= itmax; ++iter ) {

				// Calculate saturation pressure for estimated boiling temperature
				if ( present( CalledFrom ) ) {
					pSat = PsyPsatFnTemp( tSat, CalledFrom );
				} else {
					pSat = PsyPsatFnTemp( tSat, "PsyTsatFnPb" );
				}

				//Compare with specified pressure and update estimate of temperature
				error = Press - pSat;
				Iterate( ResultX, convTol, tSat, error, X1, Y1, iter, icvg );
				tSat = ResultX;
				//If converged leave loop iteration
				if ( icvg == 1 ) break;

				// Water temperature not converged, repeat calculations with new
				// estimate of water temperature

			}

			// Saturation temperature has not converged after maximum specified
			// iterations. Print error message, set return error flag, and RETURN

		} //End If for the Pressure Range Checking

#ifdef EP_psych_stats
		NumIterations( iPsyTsatFnPb ) += iter;
#endif

#ifdef EP_psych_errors
		if ( iter > itmax ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyTsatFnPb2 ) == 0 ) {
					ShowWarningMessage( "Saturation Temperature not converged after " + TrimSigDigits( iter ) + " iterations (PsyTsatFnPb)" );
					if ( present( CalledFrom ) ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( " Input Pressure= " + TrimSigDigits( Press, 2 ) );
					FlagError = true;
				}
				ShowRecurringWarningErrorAtEnd( "Saturation Temperature not converged after max iterations (PsyTsatFnPb)", iPsyErrIndex( iPsyTsatFnPb2 ), tSat, tSat, _, "C", "C" );
			}
		}
#endif

		// Result is SatTemperature
		Temp = tSat;
		tSat_Save = tSat;

#ifdef EP_psych_errors
		if ( FlagError ) {
			ShowContinueError( " Resultant Temperature= " + TrimSigDigits( Temp, 2 ) );
		}
#endif

		return Temp;

	}

	Real64
	CPCW(
		Real64 const Temperature, // unused1208
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         RUSSELL D. TAYLOR
		//       DATE WRITTEN   April 1992

		// PURPOSE OF THIS FUNCTION:
		// This function provides the specific heat of chilled water. CPCW (J/Kg/k)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SpecHeatCW; // result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		SpecHeatCW = 4180.0;

		return SpecHeatCW;
	}

	Real64
	CPHW(
		Real64 const Temperature, // unused1208
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         RUSSELL D. TAYLOR
		//       DATE WRITTEN   April 1992

		// PURPOSE OF THIS FUNCTION:
		// This function provides the specific heat of hot water. CPHW (J/Kg/k)

		// METHODOLOGY EMPLOYED:
		// na

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 SpecHeatHW; // result

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		SpecHeatHW = 4180.0;

		return SpecHeatHW;
	}

	Real64
	RhoH2O(
		Real64 const TB, // Dry bulb temperature. {C}
		Optional_string_const CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{

		// FUNCTION INFORMATION:
		//       AUTHOR         SIGSTEINN P. GRETARSSON
		//       DATE WRITTEN   April 1992

		// PURPOSE OF THIS FUNCTION:
		// This function provides the density of water at a specific temperature.

		// METHODOLOGY EMPLOYED:
		//     Density of water [kg/m3]
		//     (RANGE: KelvinConv - 423.15 DEG. K) (convert to C first)

		// REFERENCES:
		// na

		// USE STATEMENTS:
		// na

		// Return value
		Real64 RhoResult;

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		// na

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		// na

		RhoResult = 1000.1207 + 8.3215874e-04 * TB - 4.929976e-03 * std::pow( TB, 2 ) + 8.4791863e-06 * std::pow( TB, 3 );

		return RhoResult;
	}

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
	//     and The Regents of the University of California through Ernest Orlando Lawrence
	//     Berkeley National Laboratory.  All rights reserved.

	//     Portions of the EnergyPlus software package have been developed and copyrighted
	//     by other individuals, companies and institutions.  These portions have been
	//     incorporated into the EnergyPlus software package under license.   For a complete
	//     list of contributors, see "Notice" located in EnergyPlus.f90.

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

} // Psychrometrics

} // EnergyPlus
