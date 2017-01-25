// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

// C++ Headers
#include <cstdlib>
#include <iostream>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/string.functions.hh>

// EnergyPlus Headers
#include <CommandLineInterface.hh>
#include <Psychrometrics.hh>
#include <DataEnvironment.hh>
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
	static std::string const BlankString;
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
	std::string const blank_string;
#ifdef EP_psych_stats
	Array1D_string const PsyRoutineNames( NumPsychMonitors, { "PsyTdpFnTdbTwbPb", "PsyRhFnTdbWPb", "PsyTwbFnTdbWPb", "PsyVFnTdbWPb", "PsyWFnTdpPb", "PsyWFnTdbH", "PsyWFnTdbTwbPb", "PsyWFnTdbRhPb", "PsyPsatFnTemp", "PsyTsatFnHPb", "PsyTsatFnPb", "PsyRhFnTdbRhov", "PsyRhFnTdbRhovLBnd0C", "PsyTwbFnTdbWPb", "PsyTwbFnTdbWPb", "PsyWFnTdbTwbPb", "PsyTsatFnPb", "PsyTwbFnTdbWPb_cache", "PsyPsatFnTemp_cache" } ); // 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 - HR | 15 - max iter | 16 - HR | 17 - max iter | 18 - PsyTwbFnTdbWPb_raw (raw calc) | 19 - PsyPsatFnTemp_raw (raw calc)

	Array1D_bool const PsyReportIt( NumPsychMonitors, { true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, false, false, true, true } ); // PsyTdpFnTdbTwbPb     1 | PsyRhFnTdbWPb        2 | PsyTwbFnTdbWPb       3 | PsyVFnTdbWPb         4 | PsyWFnTdpPb          5 | PsyWFnTdbH           6 | PsyWFnTdbTwbPb       7 | PsyWFnTdbRhPb        8 | PsyPsatFnTemp        9 | PsyTsatFnHPb         10 | PsyTsatFnPb          11 | PsyRhFnTdbRhov       12 | PsyRhFnTdbRhovLBnd0C 13 | PsyTwbFnTdbWPb       14 - HR | PsyTwbFnTdbWPb       15 - max iter | PsyWFnTdbTwbPb       16 - HR | PsyTsatFnPb          17 - max iter | PsyTwbFnTdbWPb_cache 18 - PsyTwbFnTdbWPb_raw (raw calc) | PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
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
	Int64 const psatcache_mask( psatcache_size - 1 );
#endif

	// MODULE VARIABLE DECLARATIONS:
	// na

	// MODULE VARIABLE DEFINITIONS:
	std::string String;
	bool ReportErrors( true );
	Array1D_int iPsyErrIndex( NumPsychMonitors, 0 ); // Number of times error occurred
#ifdef EP_psych_stats
	Array1D< Int64 > NumTimesCalled( NumPsychMonitors, 0 );
	Array1D_int NumIterations( NumPsychMonitors, 0 );
#endif

	// Object Data
#ifdef EP_cache_PsyTwbFnTdbWPb
	Array1D< cached_twb_t > cached_Twb; // DIMENSION(0:twbcache_size)
#endif
#ifdef EP_cache_PsyPsatFnTemp
	Array1D< cached_psat_t > cached_Psat; // DIMENSION(0:psatcache_size)
#endif

	// Subroutine Specifications for the Module

	// Functions

	void
	clear_state()
	{
		String = "";
		ReportErrors = true;
		iPsyErrIndex = Array1D_int( NumPsychMonitors, 0 );
#ifdef EP_psych_stats
		NumTimesCalled = Array1D< Int64 >( NumPsychMonitors, 0 );
		NumIterations = Array1D_int( NumPsychMonitors, 0 );
#endif
#ifdef EP_cache_PsyTwbFnTdbWPb
		cached_Twb.deallocate();
#endif
#ifdef EP_cache_PsyPsatFnTemp
		cached_Psat.deallocate();
#endif
	}

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
		static gio::Fmt fmtLD( "*" );
		static gio::Fmt fmtA( "(A)" );

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

		EchoInputFile = FindUnitNumber( outputAuditFile );
		if ( EchoInputFile == 0 ) return;
		if ( any_gt( NumTimesCalled, 0 ) ) {
			gio::write( EchoInputFile, fmtA ) << "RoutineName,#times Called,Avg Iterations";
			for ( Loop = 1; Loop <= NumPsychMonitors; ++Loop ) {
				if ( ! PsyReportIt( Loop ) ) continue;
				gio::write( istring, fmtLD ) << NumTimesCalled( Loop );
				strip( istring );
				if ( NumIterations( Loop ) > 0 ) {
					AverageIterations = double( NumIterations( Loop ) ) / double( NumTimesCalled( Loop ) );
					gio::write( EchoInputFile, fmtA ) << PsyRoutineNames( Loop ) + ',' + istring + ',' + RoundSigDigits( AverageIterations, 2 );
				} else {
					gio::write( EchoInputFile, fmtA ) << PsyRoutineNames( Loop ) + ',' + istring;
				}
			}
		}
#endif

	}

#ifdef EP_psych_errors
	void
	PsyRhoAirFnPbTdbW_error(
		Real64 const pb, // barometric pressure (Pascals)
		Real64 const tdb, // dry bulb temperature (Celsius)
		Real64 const dw, // humidity ratio (kgWater/kgDryAir)
		Real64 const rhoair, // density of air
		std::string const & CalledFrom // routine this function was called from (error messages) !unused1208
	)
	{
		// Using/Aliasing
		using General::RoundSigDigits;

		if ( rhoair < 0.0 ) {
			ShowSevereError( "PsyRhoAirFnPbTdbW: RhoAir (Density of Air) is calculated <= 0 [" + RoundSigDigits( rhoair, 5 ) + "]." );
			ShowContinueError( "pb =[" + RoundSigDigits( pb, 2 ) + "], tdb=[" + RoundSigDigits( tdb, 2 ) + "], w=[" + RoundSigDigits( dw, 7 ) + "]." );
			if ( !CalledFrom.empty() ) {
				ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
			} else {
				ShowContinueErrorTimeStamp( " Routine=Unknown," );
			}
			ShowFatalError( "Program terminates due to preceding condition." );
		}
	}
#endif

#ifdef EP_psych_errors
	void
	PsyRhFnTdbRhovLBnd0C_error(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Real64 const RHValue, // relative humidity value (0.0-1.0)
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( RHValue > 1.01 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100.0, 2 );
					ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C) " );
					if ( !CalledFrom.empty() ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					ShowContinueError( "Relative Humidity being reset to 100.0%" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C)", iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ), RHValue * 100.0, RHValue * 100.0, _, "%", "%" );
			}
		} else if ( RHValue < -0.05 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100.0, 2 );
					ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C) " );
					if ( !CalledFrom.empty() ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					ShowContinueError( "Relative Humidity being reset to 1%" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhovLBnd0C)", iPsyErrIndex( iPsyRhFnTdbRhovLBnd0C ), RHValue * 100.0, RHValue * 100.0, _, "%", "%" );
			}
		}
	}
#endif

#ifdef EP_cache_PsyTwbFnTdbWPb

	Real64
	PsyTwbFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const W, // humidity ratio
		Real64 const Pb, // barometric pressure {Pascals}
		std::string const & CalledFrom // routine this function was called from (error messages)
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

			cached_Twb( hash ).Twb = PsyTwbFnTdbWPb_raw( Tdb_tag_r, W_tag_r, Pb_tag_r, CalledFrom );
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
		std::string const & CalledFrom // routine this function was called from (error messages)
	)

#else

	Real64
	PsyTwbFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		std::string const & CalledFrom // routine this function was called from (error messages)
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
		static std::string const RoutineName( "PsyTwbFnTdbWPb" );
		static gio::Fmt fmtLD( "*" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		Real64 tBoil; // Boiling temperature of water at given pressure
		static Real64 last_Patm( -99999.0 ); // barometric pressure {Pascals}  (last)
		static Real64 last_tBoil( -99999.0 ); // Boiling temperature of water at given pressure (last)
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
					if ( !CalledFrom.empty() ) {
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
			if ( W <= -0.0001 ) {
				if ( ! WarmupFlag ) {
					if ( iPsyErrIndex( iPsyTwbFnTdbWPb2 ) == 0 ) {
						String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( W, 3 ) + " Pressure= " + TrimSigDigits( Patm, 2 );
						ShowWarningMessage( "Entered Humidity Ratio invalid (PsyTwbFnTdbWPb)" );
						if ( !CalledFrom.empty() ) {
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
			W = 1.0e-5;
		}

		// Initial temperature guess at atmospheric pressure
		if ( Patm != last_Patm ) {
			tBoil = PsyTsatFnPb( Patm, ( CalledFrom.empty() ? RoutineName : CalledFrom ) );
			last_Patm = Patm;
			last_tBoil = tBoil;
		} else {
			tBoil = last_tBoil;
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
			PSatstar = PsyPsatFnTemp( WBT, ( CalledFrom.empty() ? RoutineName : CalledFrom ) );

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
					if ( !CalledFrom.empty() ) {
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
		gio::write( OutputFileDebug, fmtLD ) << TDB << dW << Patm << Twb;
#endif

		return TWB;
	}

#ifdef EP_psych_errors
	void
	PsyVFnTdbWPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const w, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const V, // specific volume {m3/kg}
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( V <= -0.01 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyVFnTdbWPb ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( w, 3 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowWarningMessage( "Calculated Specific Volume out of range (PsyVFnTdbWPb)" );
					if ( !CalledFrom.empty() ) {
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
	}
#endif

#ifdef EP_psych_errors
	void
	PsyWFnTdbH_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const H, // enthalpy {J/kg}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( W < -0.0001 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyWFnTdbH ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Enthalpy= " + TrimSigDigits( H, 3 );
					ShowWarningMessage( "Calculated Humidity Ratio invalid (PsyWFnTdbH)" );
					if ( !CalledFrom.empty() ) {
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
	}
#endif

#ifdef EP_cache_PsyPsatFnTemp

	Real64
	PsyPsatFnTemp_raw(
		Real64 const T, // dry-bulb temperature {C}
		std::string const & CalledFrom // routine this function was called from (error messages)
	)

#else

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		std::string const & CalledFrom // routine this function was called from (error messages)
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

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyPsatFnTemp );
#endif

		// CHECK T IN RANGE.
#ifdef EP_psych_errors
		if ( ! WarmupFlag ) {
			if ( T <= -100.0 || T >= 200.0 ) {
				if ( iPsyErrIndex( iPsyPsatFnTemp ) == 0 ) {
					ShowWarningMessage( "Temperature out of range [-100. to 200.] (PsyPsatFnTemp)" );
					if ( !CalledFrom.empty() ) {
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
		Real64 const Tkel( T + KelvinConv ); // Dry-bulb in REAL(r64) for function passing

			// If below -100C,set value of Pressure corresponding to Saturation Temperature of -100C.
		if ( Tkel < 173.15 ) {
			Pascal = 0.0017;

		// If below freezing, calculate saturation pressure over ice.
		} else if ( Tkel < KelvinConv ) { // Tkel >= 173.15
			Real64 const C1( -5674.5359 ); // Coefficient for TKel < KelvinConvK
			Real64 const C2( 6.3925247 ); // Coefficient for TKel < KelvinConvK
			Real64 const C3( -0.9677843e-2 ); // Coefficient for TKel < KelvinConvK
			Real64 const C4( 0.62215701e-6 ); // Coefficient for TKel < KelvinConvK
			Real64 const C5( 0.20747825e-8 ); // Coefficient for TKel < KelvinConvK
			Real64 const C6( -0.9484024e-12 ); // Coefficient for TKel < KelvinConvK
			Real64 const C7( 4.1635019 ); // Coefficient for TKel < KelvinConvK
			Pascal = std::exp( C1 / Tkel + C2 + Tkel * ( C3 + Tkel * ( C4 + Tkel * ( C5 + C6 * Tkel ) ) ) + C7 * std::log( Tkel ) );

			// If above freezing, calculate saturation pressure over liquid water.
		} else if ( Tkel <= 473.15 ) { // Tkel >= 173.15 // Tkel >= KelvinConv
#ifndef EP_IF97
			Real64 const C8( -5800.2206 ); // Coefficient for TKel >= KelvinConvK
			Real64 const C9( 1.3914993 ); // Coefficient for TKel >= KelvinConvK
			Real64 const C10( -0.048640239 ); // Coefficient for TKel >= KelvinConvK
			Real64 const C11( 0.41764768e-4 ); // Coefficient for TKel >= KelvinConvK
			Real64 const C12( -0.14452093e-7 ); // Coefficient for TKel >= KelvinConvK
			Real64 const C13( 6.5459673 ); // Coefficient for TKel >= KelvinConvK
			Pascal = std::exp( C8 / Tkel + C9 + Tkel * ( C10 + Tkel * ( C11 + Tkel * C12 ) ) + C13 * std::log( Tkel ) );
#else
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
			//         !IF97 equations
			Real64 const phi = Tkel + N9 / ( Tkel - N10 ); // IF97 equation 29b
			Real64 const phi2 = phi * phi; // phi squared
			Real64 const A = phi2 + N1 * phi + N2;
			Real64 const B = N3 * phi2 + N4 * phi + N5;
			Real64 const C = N6 * phi2 + N7 * phi + N8;
			Pascal = 1000000.0 * pow_4( ( 2.0 * C ) / ( -B + std::sqrt( ( B * B ) - 4.0 * A * C ) ) );
#endif
			// If above 200C, set value of Pressure corresponding to Saturation Temperature of 200C.
		} else { // Tkel >= 173.15 // Tkel >= KelvinConv // Tkel > 473.15
			Pascal = 1555000.0;
		}

		return Pascal;
	}

#ifdef EP_psych_errors
	void
	PsyWFnTdbTwbPb_temperature_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( TWB > ( TDB + 0.01 ) ) {
			if ( ReportErrors && ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyWFnTdbTwbPb ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowWarningMessage( "Given Wet Bulb Temperature invalid (PsyWFnTdbTwbPb)" );
					if ( !CalledFrom.empty() ) {
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
	}
#endif

#ifdef EP_psych_errors
	void
	PsyWFnTdbTwbPb_humidity_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{

		if ( W < 0.0 ) {
			if ( ReportErrors && ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyWFnTdbTwbPb2 ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Wet-Bulb= " + TrimSigDigits( TWB, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowWarningMessage( "Calculated Humidity Ratio Invalid (PsyWFnTdbTwbPb)" );
					if ( !CalledFrom.empty() ) {
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
		}
	}
#endif

#ifdef EP_psych_errors
	void
	PsyTdpFnTdbTwbPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Real64 const W, // humidity ratio
		Real64 const TDP,  // dew-point temperature {C}
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( TDP > TWB + 0.1 ) {
			if ( ! WarmupFlag ) { // Display error message
				if ( iPsyErrIndex( iPsyTdpFnTdbTwbPb ) == 0 ) {
					ShowWarningMessage( "Calculated Dew Point Temperature being reset (PsyTdpFnTdbTwbPb)" );
					if ( !CalledFrom.empty() ) {
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
		}
	}
#endif

	Real64
	PsyTsatFnHPb(
		Real64 const H, // enthalpy {J/kg}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom // routine this function was called from (error messages)
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
			Hloc = min( -0.00001, H );
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
					if ( !CalledFrom.empty() ) {
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
			if ( !CalledFrom.empty() ) {
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

#ifdef EP_psych_errors
	void
	PsyRhFnTdbRhov_error(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Real64 const RHValue, // relative humidity value (0.0-1.0)
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( RHValue > 1.01 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyRhFnTdbRhov ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100.0, 2 );
					ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov) " );
					if ( !CalledFrom.empty() ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					ShowContinueError( "Relative Humidity being reset to 100.0 %" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov)", iPsyErrIndex( iPsyRhFnTdbRhov ), RHValue * 100.0, RHValue * 100.0, _, "%", "%" );
			}
		} else if ( RHValue < -0.05 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyRhFnTdbRhov ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( Tdb, 2 ) + " Rhovapor= " + TrimSigDigits( Rhovapor, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100.0, 2 );
					ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov) " );
					if ( !CalledFrom.empty() ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					ShowContinueError( "Relative Humidity being reset to 1%" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbRhov)", iPsyErrIndex( iPsyRhFnTdbRhov ), RHValue * 100.0, RHValue * 100.0, _, "%", "%" );
			}
		}
	}
#endif

#ifdef EP_psych_errors
	void
	PsyRhFnTdbWPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const W, // humidity ratio
		Real64 const RHValue, // relative humidity (0.0-1.0)
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( RHValue > 1.01 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyRhFnTdbWPb ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( W, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100.0, 2 );
					ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb) " );
					if ( !CalledFrom.empty() ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					ShowContinueError( "Relative Humidity being reset to 100.0%" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb)", iPsyErrIndex( iPsyRhFnTdbWPb ), RHValue * 100.0, RHValue * 100.0, _, "%", "%" );
			}
		} else if ( RHValue < -0.05 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyRhFnTdbWPb ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Humidity Ratio= " + TrimSigDigits( W, 3 ) + " Calculated Relative Humidity [%]= " + TrimSigDigits( RHValue * 100.0, 2 );
					ShowWarningMessage( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb) " );
					if ( !CalledFrom.empty() ) {
						ShowContinueErrorTimeStamp( " Routine=" + CalledFrom + ',' );
					} else {
						ShowContinueErrorTimeStamp( " Routine=Unknown," );
					}
					ShowContinueError( String );
					ShowContinueError( "Relative Humidity being reset to 1%" );
				}
				ShowRecurringWarningErrorAtEnd( "Calculated Relative Humidity out of range (PsyRhFnTdbWPb)", iPsyErrIndex( iPsyRhFnTdbWPb ), RHValue * 100.0, RHValue * 100.0, _, "%", "%" );
			}
		}
	}
#endif

#ifdef EP_psych_errors
	void
	PsyWFnTdpPb_error(
		Real64 const TDP, // dew-point temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( W <= -0.0001 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyWFnTdpPb ) == 0 ) {
					String = " Dew-Point= " + TrimSigDigits( TDP, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowWarningMessage( "Calculated Humidity Ratio invalid (PsyWFnTdpPb)" );
					if ( !CalledFrom.empty() ) {
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
	}
#endif

#ifdef EP_psych_errors
	void
	PsyWFnTdbRhPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	)
	{
		if ( W <= -0.0001 ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyWFnTdbRhPb ) == 0 ) {
					String = " Dry-Bulb= " + TrimSigDigits( TDB, 2 ) + " Relative Humidity [%]= " + TrimSigDigits( RH * 100.0, 2 ) + " Pressure= " + TrimSigDigits( PB, 2 );
					ShowWarningMessage( "Calculated Humidity Ratio is invalid (PsyWFnTdbRhPb)" );
					if ( !CalledFrom.empty() ) {
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
	}
#endif

	Real64
	PsyTsatFnPb(
		Real64 const Press, // barometric pressure {Pascals}
		std::string const & CalledFrom // routine this function was called from (error messages)
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

		// Locals
		// FUNCTION ARGUMENT DEFINITIONS:

		// FUNCTION PARAMETER DEFINITIONS:
		int const itmax( 50 ); // Maximum number of iterations
		Real64 const convTol( 0.0001 );
		static std::string const RoutineName( "PsyTsatFnPb" );

		// INTERFACE BLOCK SPECIFICATIONS
		// na

		// DERIVED TYPE DEFINITIONS
		// na

		// FUNCTION LOCAL VARIABLE DECLARATIONS:
		bool FlagError; // set when errors should be flagged
		static Real64 Press_Save( -99999.0 );
		static Real64 tSat_Save( -99999.0 );
		Real64 tSat; // Water temperature guess
		int iter; // Iteration counter

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTsatFnPb );
#endif

		// Check press in range.
		FlagError = false;
#ifdef EP_psych_errors
		if ( ! WarmupFlag ) {
			if ( Press <= 0.0017 || Press >= 1555000.0 ) {
				if ( iPsyErrIndex( iPsyTsatFnPb ) == 0 ) {
					ShowWarningMessage( "Pressure out of range (PsyTsatFnPb)" );
					if ( !CalledFrom.empty() ) {
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
			return tSat_Save;
		}
		Press_Save = Press;

		// Uses an iterative process to determine the saturation temperature at a given
		// pressure by correlating saturated water vapor as a function of temperature.

		// Initial guess of boiling temperature
		tSat = 100.0;
		iter = 0;

		// If above 1555000,set value of Temp corresponding to Saturation Pressure of 1555000 Pascal.
		if ( Press >= 1555000.0 ) {
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
			Real64 pSat; // Pressure corresponding to temp. guess
			Real64 error; // Deviation of dependent variable in iteration
			Real64 X1; // Previous value of independent variable in ITERATE
			Real64 Y1; // Previous value of dependent variable in ITERATE
			Real64 ResultX; // ResultX is the final Iteration result passed back to the calling routine
			bool const CalledFrom_empty( CalledFrom.empty() );
			int icvg; // Iteration convergence flag
			for ( iter = 1; iter <= itmax; ++iter ) {

				// Calculate saturation pressure for estimated boiling temperature
				pSat = PsyPsatFnTemp( tSat, ( CalledFrom_empty ? RoutineName : CalledFrom ) );

				// Compare with specified pressure and update estimate of temperature
				error = Press - pSat;
				Iterate( ResultX, convTol, tSat, error, X1, Y1, iter, icvg );
				tSat = ResultX;
				// If converged leave loop iteration
				if ( icvg == 1 ) break;

				// Water temperature not converged, repeat calculations with new
				// estimate of water temperature

			}

			// Saturation temperature has not converged after maximum specified
			// iterations. Print error message, set return error flag, and RETURN

		} // End If for the Pressure Range Checking

#ifdef EP_psych_stats
		NumIterations( iPsyTsatFnPb ) += iter;
#endif

#ifdef EP_psych_errors
		if ( iter > itmax ) {
			if ( ! WarmupFlag ) {
				if ( iPsyErrIndex( iPsyTsatFnPb2 ) == 0 ) {
					ShowWarningMessage( "Saturation Temperature not converged after " + TrimSigDigits( iter ) + " iterations (PsyTsatFnPb)" );
					if ( !CalledFrom.empty() ) {
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
		Real64 const Temp = tSat_Save = tSat; // result=> saturation temperature {C}

#ifdef EP_psych_errors
		if ( FlagError ) {
			ShowContinueError( " Resultant Temperature= " + TrimSigDigits( Temp, 2 ) );
		}
#endif

		return Temp;

	}

} // Psychrometrics

} // EnergyPlus
