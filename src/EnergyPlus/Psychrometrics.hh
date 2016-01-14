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

#ifndef Psychrometrics_hh_INCLUDED
#define Psychrometrics_hh_INCLUDED

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/bit.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
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

#ifdef EP_psych_errors
	using namespace DataGlobals;
#endif

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// call for recurring errors
	extern int const iPsyTdpFnTdbTwbPb;
	extern int const iPsyRhFnTdbWPb;
	extern int const iPsyTwbFnTdbWPb;
	extern int const iPsyTwbFnTdbWPb2;
	extern int const iPsyTwbFnTdbWPb3; // convergence
	extern int const iPsyVFnTdbWPb;
	extern int const iPsyWFnTdpPb;
	extern int const iPsyWFnTdbH;
	extern int const iPsyWFnTdbTwbPb;
	extern int const iPsyWFnTdbTwbPb2;
	extern int const iPsyWFnTdbRhPb;
	extern int const iPsyPsatFnTemp;
	extern int const iPsyTsatFnHPb;
	extern int const iPsyTsatFnPb;
	extern int const iPsyTsatFnPb2; // iterations
	extern int const iPsyRhFnTdbRhov;
	extern int const iPsyRhFnTdbRhovLBnd0C;
	extern int const iPsyTwbFnTdbWPb_cache;
	extern int const iPsyPsatFnTemp_cache;
	extern int const NumPsychMonitors; // Parameterization of Number of psychrometric routines that
	extern std::string const blank_string;
#ifdef EP_psych_stats
	extern Array1D_string const PsyRoutineNames; // 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 - HR | 15 - max iter | 16 - HR | 17 - max iter | 18 - PsyTwbFnTdbWPb_raw (raw calc) | 19 - PsyPsatFnTemp_raw (raw calc)

	extern Array1D_bool const PsyReportIt; // PsyTdpFnTdbTwbPb     1 | PsyRhFnTdbWPb        2 | PsyTwbFnTdbWPb       3 | PsyVFnTdbWPb         4 | PsyWFnTdpPb          5 | PsyWFnTdbH           6 | PsyWFnTdbTwbPb       7 | PsyWFnTdbRhPb        8 | PsyPsatFnTemp        9 | PsyTsatFnHPb         10 | PsyTsatFnPb          11 | PsyRhFnTdbRhov       12 | PsyRhFnTdbRhovLBnd0C 13 | PsyTwbFnTdbWPb       14 - HR | PsyTwbFnTdbWPb       15 - max iter | PsyWFnTdbTwbPb       16 - HR | PsyTsatFnPb          17 - max iter | PsyTwbFnTdbWPb_cache 18 - PsyTwbFnTdbWPb_raw (raw calc) | PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
#endif

#ifndef EP_psych_errors
	extern Real64 const KelvinConv;
#endif

#ifdef EP_cache_PsyTwbFnTdbWPb
	extern int const twbcache_size;
	extern int const twbprecision_bits;
#endif
#ifdef EP_cache_PsyPsatFnTemp
	extern int const psatcache_size;
	extern int const psatprecision_bits; // 28  //24  //32
	extern Int64 const psatcache_mask;
#endif

	// MODULE VARIABLE DECLARATIONS:
	// na

	// MODULE VARIABLE DEFINITIONS:
	extern std::string String;
	extern bool ReportErrors;
	extern Array1D_int iPsyErrIndex; // Number of times error occurred
#ifdef EP_psych_stats
	extern Array1D< Int64 > NumTimesCalled;
	extern Array1D_int NumIterations;
#endif

	// DERIVED TYPE DEFINITIONS

	// Types

#ifdef EP_cache_PsyTwbFnTdbWPb
	struct cached_twb_t
	{
		// Members
		Int64 iTdb;
		Int64 iW;
		Int64 iPb;
		Real64 Twb;

		// Default Constructor
		cached_twb_t() :
			iTdb( 0 ),
			iW( 0 ),
			iPb( 0 ),
			Twb( 0.0 )
		{}

	};
#endif

#ifdef EP_cache_PsyPsatFnTemp
	struct cached_psat_t
	{
		// Members
		Int64 iTdb;
		Real64 Psat;

		// Default Constructor
		cached_psat_t() :
			iTdb( -1000 ),
			Psat( 0.0 )
		{}

	};
#endif

	// Object Data
#ifdef EP_cache_PsyTwbFnTdbWPb
	extern Array1D< cached_twb_t > cached_Twb; // DIMENSION(0:twbcache_size)
#endif
#ifdef EP_cache_PsyPsatFnTemp
	extern Array1D< cached_psat_t > cached_Psat; // DIMENSION(0:psatcache_size)
#endif

	// Subroutine Specifications for the Module

	// Functions

	void
	clear_state();

	void
	InitializePsychRoutines();

	void
	ShowPsychrometricSummary();

#ifdef EP_psych_errors
	void
	PsyRhoAirFnPbTdbW_error(
		Real64 const pb, // barometric pressure (Pascals)
		Real64 const tdb, // dry bulb temperature (Celsius)
		Real64 const dw, // humidity ratio (kgWater/kgDryAir)
		Real64 const rhoair, // density of air
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages) !unused1208
	);
#endif

	inline
	Real64
	PsyRhoAirFnPbTdbW(
		Real64 const pb, // barometric pressure (Pascals)
		Real64 const tdb, // dry bulb temperature (Celsius)
		Real64 const dw, // humidity ratio (kgWater/kgDryAir)
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages) !unused1208
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

		Real64 const rhoair( pb / ( 287.0 * ( tdb + KelvinConv ) * ( 1.0 + 1.6077687 * max( dw, 1.0e-5 ) ) ) );
#ifdef EP_psych_errors
		if ( rhoair < 0.0 ) PsyRhoAirFnPbTdbW_error( pb, tdb, dw, rhoair, CalledFrom );
#endif
		return rhoair;
	}

	inline
	Real64
	PsyRhoAirFnPbTdbW_fast(
		Real64 const pb, // barometric pressure (Pascals)
		Real64 const tdb, // dry bulb temperature (Celsius)
		Real64 const dw // humidity ratio (kgWater/kgDryAir)
	)
	{
		// Faster version with humidity ratio already adjusted
		assert( dw >= 1.0e-5 );
		Real64 const rhoair( pb / ( 287.0 * ( tdb + KelvinConv ) * ( 1.0 + 1.6077687 * dw ) ) );
#ifdef EP_psych_errors
		if ( rhoair < 0.0 ) PsyRhoAirFnPbTdbW_error( pb, tdb, dw, rhoair );
#endif
		return rhoair;
	}

	inline
	Real64
	PsyHfgAirFnWTdb(
		Real64 const EP_UNUSED( w ), // humidity ratio {kgWater/kgDryAir} !unused1208
		Real64 const T // input temperature {Celsius}
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

		// Return value
		// result => heat of vaporization for moist air {J/kg}

		// This formulation currently does not use W since it returns results that are in J/kg and the
		//  amount of energy is on a per unit of moisture basis.

		Real64 const Temperature( max( T, 0.0 ) ); // input temperature {Celsius} - corrected for >= 0C
		return ( 2500940.0 + 1858.95 * Temperature ) - ( 4180.0 * Temperature ); // enthalpy of the gas - enthalpy of the fluid
	}

	inline
	Real64
	PsyHgAirFnWTdb(
		Real64 const EP_UNUSED( w ), // humidity ratio {kgWater/kgDryAir} !unused1208
		Real64 const T // input temperature {Celsius}
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

		// REFERENCES:
		// see ASHRAE Fundamentals Psychrometric Chapter
		// USAGE:  hg = PsyHgAirFnWTdb(w,T)

		// This formulation currently does not use W since it returns results that are in J/kg and the
		//  amount of energy is on a per unit of moisture basis.

		return 2500940.0 + 1858.95 * T; // enthalpy of the gas {units?}
	}

	inline
	Real64
	PsyHFnTdbW(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW // humidity ratio
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the enthalpy {J/kg} from dry-bulb temperature and humidity ratio.

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

		// calculate enthalpy
		return 1.00484e3 * TDB + max( dW, 1.0e-5 ) * ( 2.50094e6 + 1.85895e3 * TDB ); // enthalpy {J/kg}
	}

	inline
	Real64
	PsyHFnTdbW_fast(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW // humidity ratio
	)
	{
		// Faster version with humidity ratio already adjusted
		assert( dW >= 1.0e-5 );

		// calculate enthalpy
		return 1.00484e3 * TDB + dW * ( 2.50094e6 + 1.85895e3 * TDB ); // enthalpy {J/kg}
	}

	inline
	Real64
	PsyCpAirFnWTdb(
		Real64 const dw, // humidity ratio {kgWater/kgDryAir}
		Real64 const T // input temperature {Celsius}
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

		// Static locals
		static Real64 dwSave( -100.0 );
		static Real64 Tsave( -100.0 );
		static Real64 cpaSave( -100.0 );

		// check if last call had the same input and if it did just use the saved output
		if ( ( Tsave == T ) && ( dwSave == dw ) ) return cpaSave;

		// compute heat capacity of air
		Real64 const w( max( dw, 1.0e-5 ) );
		Real64 const cpa( ( PsyHFnTdbW( T + 0.1, w ) - PsyHFnTdbW( T, w ) ) * 10.0 ); // result => heat capacity of air {J/kg-C}

		// save values for next call
		dwSave = dw;
		Tsave = T;
		cpaSave = cpa;

		return cpa;
	}

	inline
	Real64
	PsyCpAirFnWTdb_fast(
		Real64 const dw, // humidity ratio {kgWater/kgDryAir}
		Real64 const T // input temperature {Celsius}
	)
	{
		// Faster version with humidity ratio already adjusted
		assert( dw >= 1.0e-5 );

		// Static locals
		static Real64 dwSave( -100.0 );
		static Real64 Tsave( -100.0 );
		static Real64 cpaSave( -100.0 );

		// check if last call had the same input and if it did just use the saved output
		if ( ( Tsave == T ) && ( dwSave == dw ) ) return cpaSave;

		// compute heat capacity of air
		Real64 const cpa( ( PsyHFnTdbW_fast( T + 0.1, dw ) - PsyHFnTdbW_fast( T, dw ) ) * 10.0 ); // result => heat capacity of air {J/kg-C}

		// save values for next call
		dwSave = dw;
		Tsave = T;
		cpaSave = cpa;

		return cpa;
	}

	inline
	Real64
	PsyTdbFnHW(
		Real64 const H, // enthalpy {J/kg}
		Real64 const dW // humidity ratio
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         J. C. VanderZee
		//       DATE WRITTEN   Feb. 1994
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function provides air temperature from enthalpy and humidity ratio.

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32
		//   by inverting function PsyHFnTdbW

		Real64 const W( max( dW, 1.0e-5 ) ); // humidity ratio
		return ( H - 2.50094e6 * W ) / ( 1.00484e3 + 1.85895e3 * W ); // result=> dry-bulb temperature {C}
	}

	inline
	Real64
	PsyRhovFnTdbRhLBnd0C(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const RH // relative humidity value (0.0-1.0)
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

		return RH / ( 461.52 * ( Tdb + KelvinConv ) ) * std::exp( 23.7093 - 4111.0 / ( ( Tdb + KelvinConv ) - 35.45 ) ); // Vapor density in air
	}

	inline
	Real64
	PsyRhovFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB // Barometric Pressure {Pascals}
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

		Real64 const W( max( dW, 1.0e-5 ) ); // humidity ratio
		return W * PB / ( 461.52 * ( Tdb + KelvinConv ) * ( W + 0.62198 ) );
	}

	inline
	Real64
	PsyRhovFnTdbWPb_fast(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB // Barometric Pressure {Pascals}
	)
	{
		// Faster version with humidity ratio already adjusted
		assert( dW >= 1.0e-5 );
		return dW * PB / ( 461.52 * ( Tdb + KelvinConv ) * ( dW + 0.62198 ) );
	}

#ifdef EP_psych_errors
	void
	PsyRhFnTdbRhovLBnd0C_error(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Real64 const RHValue, // relative humidity value (0.0-1.0)
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyRhFnTdbRhovLBnd0C(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyRhFnTdbRhovLBnd0C );
#endif

		Real64 const RHValue( Rhovapor > 0.0 ? Rhovapor * 461.52 * ( Tdb + KelvinConv ) * std::exp( -23.7093 + 4111.0 / ( ( Tdb + KelvinConv ) - 35.45 ) ) : 0.0 );

		if ( ( RHValue < 0.0 ) || ( RHValue > 1.0 ) ) {
#ifdef EP_psych_errors
			if ( ( RHValue < -0.05 ) || ( RHValue > 1.01 ) ) {
				PsyRhFnTdbRhovLBnd0C_error( Tdb, Rhovapor, RHValue, CalledFrom );
			}
#endif
			return min( max( RHValue, 0.01 ), 1.0 );
		} else {
			return RHValue;
		}
	}

#ifdef EP_cache_PsyTwbFnTdbWPb

	Real64
	PsyTwbFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const W, // humidity ratio
		Real64 const Pb, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

	Real64
	PsyTwbFnTdbWPb_raw(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

#else

	Real64
	PsyTwbFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

#endif

#ifdef EP_psych_errors
	void
	PsyVFnTdbWPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const w, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const V, // specific volume {m3/kg}
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyVFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 28

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyVFnTdbWPb );
#endif

		Real64 const w( max( dW, 1.0e-5 ) ); // humidity ratio
		Real64 const V( 1.59473e2 * ( 1.0 + 1.6078 * w ) * ( 1.8 * TDB + 492.0 ) / PB ); // specific volume {m3/kg}

		// Validity test
		if ( V < 0.0 ) {
#ifdef EP_psych_errors
			if ( V <= -0.01 ) PsyVFnTdbWPb_error( TDB, w, PB, V, CalledFrom );
#endif
			return 0.83; //Fix Was inside the ifdef
		} else {
			return V;
		}
	}

#ifdef EP_psych_errors
	void
	PsyWFnTdbH_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const H, // enthalpy {J/kg}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyWFnTdbH(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const H, // enthalpy {J/kg}
		std::string const & CalledFrom = blank_string, // routine this function was called from (error messages)
		bool const SuppressWarnings = false // if calling function is calculating an intermediate state
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

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P100, EQN 32

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdbH );
#endif

		Real64 const W( ( H - 1.00484e3 * TDB ) / ( 2.50094e6 + 1.85895e3 * TDB ) ); // humidity ratio

		// Validity test
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			if ( ( W <= -0.0001 ) && ( ! SuppressWarnings ) ) PsyWFnTdbH_error( TDB, H, W, CalledFrom );
#endif
			return 1.0e-5;
		} else {
			return W;
		}
	}

#ifdef EP_cache_PsyPsatFnTemp

	Real64
	PsyPsatFnTemp_raw(
		Real64 const T, // dry-bulb temperature {C}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

	inline
	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// FUNCTION PARAMETER DEFINITIONS:
		//  integer(i64), parameter :: Grid_Mask=NOT(ISHFT(1_i64, Grid_Shift)-1)
		Int64 const Grid_Shift( 28 ); //Tuned This is a hot spot
		assert( Grid_Shift == 64 - 12 - psatprecision_bits ); // Force Grid_Shift updates when precision bits changes

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyPsatFnTemp_cache );
#endif

		// FUNCTION LOCAL VARIABLE DECLARATIONS:

		Int64 const Tdb_tag( bit::bit_shift( TRANSFER( T, Grid_Shift ), -Grid_Shift ) ); // Note that 2nd arg to TRANSFER is not used: Only type matters
//		Int64 const hash( bit::bit_and( Tdb_tag, psatcache_mask ) ); //Tuned Replaced by below
		Int64 const hash( Tdb_tag & psatcache_mask );
		auto & cPsat( cached_Psat( hash ) );

		if ( cPsat.iTdb != Tdb_tag ) {
			cPsat.iTdb = Tdb_tag;
			Real64 Tdb_tag_r;
			Tdb_tag_r = TRANSFER( bit::bit_shift( Tdb_tag, Grid_Shift ), Tdb_tag_r );
			cPsat.Psat = PsyPsatFnTemp_raw( Tdb_tag_r, CalledFrom );
		}

		return cPsat.Psat; // saturation pressure {Pascals}
	}

#else

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

#endif

	Real64
	PsyTsatFnHPb(
		Real64 const H, // enthalpy {J/kg}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

	inline
	Real64
	PsyRhovFnTdbRh(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		return ( PsyPsatFnTemp( Tdb, CalledFrom ) * RH ) / ( 461.52 * ( Tdb + KelvinConv ) ); // Vapor density in air
	}

#ifdef EP_psych_errors
	void
	PsyRhFnTdbRhov_error(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Real64 const RHValue, // relative humidity
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyRhFnTdbRhov(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PsyRhFnTdbRhov" );

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyRhFnTdbRhov );
#endif

		Real64 const RHValue( Rhovapor > 0.0 ? Rhovapor * 461.52 * ( Tdb + KelvinConv ) / PsyPsatFnTemp( Tdb, RoutineName ) : 0.0 );

		if ( ( RHValue < 0.0 ) || ( RHValue > 1.0 ) ) {
#ifdef EP_psych_errors
			if ( ( RHValue < -0.05 ) || ( RHValue > 1.01 ) ) {
				PsyRhFnTdbRhov_error( Tdb, Rhovapor, RHValue, CalledFrom );
			}
#endif
			return min( max( RHValue, 0.01 ), 1.0 );
		} else {
			return RHValue;
		}
	}

#ifdef EP_psych_errors
	void
	PsyRhFnTdbWPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const W, // humidity ratio
		Real64 const RHValue, // relative humidity (0.0-1.0)
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyRhFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// REFERENCES:
		// ASHRAE HANDBOOK FUNDAMENTALS 1985, P6.12, EQN 10,21,23

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PsyRhFnTdbWPb" );

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyRhFnTdbWPb );
#endif

		Real64 const PWS( PsyPsatFnTemp( TDB, ( CalledFrom.empty() ? RoutineName : CalledFrom ) ) ); // Pressure -- saturated for pure water

		// Find Degree Of Saturation
		Real64 const W( max( dW, 1.0e-5 ) ); // humidity ratio
		Real64 const U( W / ( 0.62198 * PWS / ( PB - PWS ) ) ); // Degree of Saturation

		// Calculate The Relative Humidity
		Real64 const RHValue( U / ( 1.0 - ( 1.0 - U ) * ( PWS / PB ) ) );

		// Validity test
		if ( ( RHValue < 0.0 ) || ( RHValue > 1.0 ) ) {
#ifdef EP_psych_errors
			if ( ( RHValue < -0.05 ) || ( RHValue > 1.01 ) ) {
				PsyRhFnTdbWPb_error( TDB, W, RHValue, CalledFrom );
			}
#endif
			return min( max( RHValue, 0.01 ), 1.0 );
		} else {
			return RHValue;
		}
	}

#ifdef EP_psych_errors
	void
	PsyWFnTdpPb_error(
		Real64 const TDP, // dew-point temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyWFnTdpPb(
		Real64 const TDP, // dew-point temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PsyWFnTdpPb" );

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdpPb );
#endif

		Real64 const PDEW( PsyPsatFnTemp( TDP, ( CalledFrom.empty() ? RoutineName : CalledFrom ) ) ); // saturation pressure at dew-point temperature {Pascals}
		Real64 const W( PDEW * 0.62198 / ( PB - PDEW ) ); // humidity ratio

		// Validity test
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			if ( W <= -0.0001 ) PsyWFnTdpPb_error( TDP, PB, W, CalledFrom );
#endif
			return 1.0e-5;
		} else {
			return W;
		}
	}

#ifdef EP_psych_errors
	void
	PsyWFnTdbRhPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyWFnTdbRhPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQN 22

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PsyWFnTdbRhPb" );

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdbRhPb );
#endif

		Real64 const PDEW( RH * PsyPsatFnTemp( TDB, ( CalledFrom.empty() ? RoutineName : CalledFrom ) ) ); // Pressure at dew-point temperature {Pascals}

		// Numeric error check when the temperature and RH values cause Pdew to equal or exceed
		// barometric pressure which is physically impossible. An approach limit of 1000 pascals
		// was chosen to keep the numerics stable as the denominator approaches 0.
		Real64 const W( PDEW * 0.62198 / max( PB - PDEW, 1000.0 ) ); // humidity ratio
		// THIS EQUATION IN SI UNIT IS FROM ASHRAE HANDBOOK OF FUNDAMENTALS PAGE 99  EQUATION 22

		// Validity test
		if ( W < 1.0e-5 ) {
#ifdef EP_psych_errors
			if ( W <= -0.0001 ) PsyWFnTdbRhPb_error( TDB, RH, PB, W, CalledFrom );
#endif
			return 1.0e-5;
		} else {
			return W;
		}
	}

#ifdef EP_psych_errors

	void
	PsyWFnTdbTwbPb_temperature_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

	void
	PsyWFnTdbTwbPb_humidity_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Real64 const W, // humidity ratio
		std::string const & CalledFrom // routine this function was called from (error messages)
	);

#endif

	inline
	Real64
	PsyWFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWBin, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		// REFERENCES:
		// ASHRAE HANDBOOK OF FUNDAMENTALS, 1972, P99, EQ 22,35

		// FUNCTION PARAMETER DEFINITIONS:
		static std::string const RoutineName( "PsyWFnTdbTwbPb" );

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyWFnTdbTwbPb );
#endif

		Real64 TWB( TWBin ); // test wet-bulb temperature

		// Validity check
		if ( TWB > TDB ) {
#ifdef EP_psych_errors
			if ( TWB > TDB + 0.01 ) PsyWFnTdbTwbPb_temperature_error( TDB, TWB, PB, CalledFrom );
#endif
			TWB = TDB;
		}

		// Calculation
		Real64 const PWET( PsyPsatFnTemp( TWB, ( CalledFrom.empty() ? RoutineName : CalledFrom ) ) ); // Pressure at wet-bulb temperature {Pascals}
		Real64 const WET( 0.62198 * PWET / ( PB - PWET ) ); // Humidity ratio at wet-bulb temperature
		Real64 const W( ( ( 2501.0 - 2.381 * TWB ) * WET - ( TDB - TWB ) ) / ( 2501.0 + 1.805 * TDB - 4.186 * TWB ) ); // humidity ratio

		// Validity check
		if ( W < 0.0 ) {
#ifdef EP_psych_errors
			PsyWFnTdbTwbPb_humidity_error( TDB, TWB, PB, W, CalledFrom );
#endif
			return PsyWFnTdbRhPb( TDB, 0.0001, PB, CalledFrom );
		} else {
			return W;
		}
	}

	inline
	Real64
	PsyHFnTdbRhPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0 - 1.0)
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		return PsyHFnTdbW( TDB, max( PsyWFnTdbRhPb( TDB, RH, PB, CalledFrom ), 1.0e-5 ) ); // enthalpy {J/kg}
	}

	Real64
	PsyTsatFnPb(
		Real64 const Press, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

	inline
	Real64
	PsyTdpFnWPb(
		Real64 const W, // humidity ratio
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
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

		Real64 const W0( max( W, 1.0e-5 ) ); // limited humidity ratio
		Real64 const PDEW( PB * W0 / ( 0.62198 + W0 ) ); // pressure at dew point temperature
		return PsyTsatFnPb( PDEW, CalledFrom );
	}

#ifdef EP_psych_errors
	void
	PsyTdpFnTdbTwbPb_error(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Real64 const W, // humidity ratio
		Real64 const TDP,  // dew-point temperature {C}
		std::string const & CalledFrom // routine this function was called from (error messages)
	);
#endif

	inline
	Real64
	PsyTdpFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         George Shih
		//       DATE WRITTEN   May 1976
		//       MODIFIED       na
		//       RE-ENGINEERED  na

		// PURPOSE OF THIS FUNCTION:
		// This function calculates the dew-point temperature {C} from dry-bulb, wet-bulb and pressure.

#ifdef EP_psych_stats
		++NumTimesCalled( iPsyTdpFnTdbTwbPb );
#endif

		Real64 const W( max( PsyWFnTdbTwbPb( TDB, TWB, PB, CalledFrom ), 1.0e-5 ) );
		Real64 const TDP( PsyTdpFnWPb( W, PB, CalledFrom ) );

		if ( TDP > TWB ) {
#ifdef EP_psych_errors
			if ( TDP > TWB + 0.1 ) PsyTdpFnTdbTwbPb_error( TDB, TWB, PB, W, TDP, CalledFrom );
#endif
			return TWB;
		} else {
			return TDP;
		}

	}

	inline
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
		return A0 + X * ( A1 + X * ( A2 + X * ( A3 + X * ( A4 + X * A5 ) ) ) );
	}

	inline
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
		return ( A0 + X * ( A1 + X * ( A2 + X * ( A3 + X * ( A4 + X * ( A5 + X * A6 ) ) ) ) ) ) / 1.0E10;
	}

	inline
	Real64
	CPCW(
		Real64 const EP_UNUSED( Temperature ) // unused1208
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         RUSSELL D. TAYLOR
		//       DATE WRITTEN   April 1992

		// PURPOSE OF THIS FUNCTION:
		// This function provides the specific heat of chilled water. CPCW (J/Kg/k)

		return 4180.0;
	}

	inline
	Real64
	CPHW(
		Real64 const EP_UNUSED( Temperature ) // unused1208
	)
	{
		// FUNCTION INFORMATION:
		//       AUTHOR         RUSSELL D. TAYLOR
		//       DATE WRITTEN   April 1992

		// PURPOSE OF THIS FUNCTION:
		// This function provides the specific heat of hot water. CPHW (J/Kg/k)

		return 4180.0;
	}

	inline
	Real64
	RhoH2O(
		Real64 const TB // Dry bulb temperature. {C}
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

		return 1000.1207 + 8.3215874e-04 * TB - 4.929976e-03 * pow_2( TB ) + 8.4791863e-06 * pow_3( TB );
	}

} // Psychrometrics

} // EnergyPlus

#endif
