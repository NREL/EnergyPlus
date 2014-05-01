#ifndef Psychrometrics_hh_INCLUDED
#define Psychrometrics_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>

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
#ifdef EP_psych_stats
	extern FArray1D_string const PsyRoutineNames; // 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 - HR | 15 - max iter | 16 - HR | 17 - max iter | 18 - PsyTwbFnTdbWPb_raw (raw calc) | 19 - PsyPsatFnTemp_raw (raw calc)

	extern FArray1D_bool const PsyReportIt; // PsyTdpFnTdbTwbPb     1 | PsyRhFnTdbWPb        2 | PsyTwbFnTdbWPb       3 | PsyVFnTdbWPb         4 | PsyWFnTdpPb          5 | PsyWFnTdbH           6 | PsyWFnTdbTwbPb       7 | PsyWFnTdbRhPb        8 | PsyPsatFnTemp        9 | PsyTsatFnHPb         10 | PsyTsatFnPb          11 | PsyRhFnTdbRhov       12 | PsyRhFnTdbRhovLBnd0C 13 | PsyTwbFnTdbWPb       14 - HR | PsyTwbFnTdbWPb       15 - max iter | PsyWFnTdbTwbPb       16 - HR | PsyTsatFnPb          17 - max iter | PsyTwbFnTdbWPb_cache 18 - PsyTwbFnTdbWPb_raw (raw calc) | PsyPsatFnTemp_cache  19 - PsyPsatFnTemp_raw (raw calc)
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
#endif

	// MODULE VARIABLE DECLARATIONS:
	// na

	// MODULE VARIABLE DEFINITIONS:
	extern std::string String;
	extern bool ReportErrors;
	extern FArray1D_int iPsyErrIndex; // Number of times error occurred
#ifdef EP_psych_stats
	extern FArray1D< Int64 > NumTimesCalled;
	extern FArray1D_int NumIterations;
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

		// Member Constructor
		cached_twb_t(
			Int64 const iTdb,
			Int64 const iW,
			Int64 const iPb,
			Real64 const Twb
		) :
			iTdb( iTdb ),
			iW( iW ),
			iPb( iPb ),
			Twb( Twb )
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

		// Member Constructor
		cached_psat_t(
			Int64 const iTdb,
			Real64 const Psat
		) :
			iTdb( iTdb ),
			Psat( Psat )
		{}

	};
#endif

	// Object Data
#ifdef EP_cache_PsyTwbFnTdbWPb
	extern FArray1D< cached_twb_t > cached_Twb; // DIMENSION(0:twbcache_size)
#endif
#ifdef EP_cache_PsyPsatFnTemp
	extern FArray1D< cached_psat_t > cached_Psat; // DIMENSION(0:psatcache_size)
#endif

	// Subroutine Specifications for the Module

	// Functions

	void
	InitializePsychRoutines();

	void
	ShowPsychrometricSummary();

	Real64
	PsyRhoAirFnPbTdbW(
		Real64 const pb, // barometric pressure (Pascals)
		Real64 const tdb, // dry bulb temperature (Celsius)
		Real64 const dw, // humidity ratio (kgWater/kgDryAir)
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyCpAirFnWTdb(
		Real64 const dw, // humidity ratio {kgWater/kgDryAir}
		Real64 const T, // input temperature {Celsius}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyHfgAirFnWTdb(
		Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
		Real64 const T, // input temperature {Celsius}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyHgAirFnWTdb(
		Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
		Real64 const T, // input temperature {Celsius}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyTdpFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWB, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyTdpFnWPb(
		Real64 const W, // humidity ratio
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyHFnTdbW(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyHFnTdbRhPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0 - 1.0)
		Real64 const PB, // barometric pressure (N/M**2) {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyTdbFnHW(
		Real64 const H, // enthalpy {J/kg}
		Real64 const dW, // humidity ratio
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyRhovFnTdbRh(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyRhovFnTdbRhLBnd0C(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyRhovFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // Barometric Pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	PsyRhFnTdbRhov(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyRhFnTdbRhovLBnd0C(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const Rhovapor, // vapor density in air {kg/m3}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyRhFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

#ifdef EP_cache_PsyTwbFnTdbWPb

	Real64
	PsyTwbFnTdbWPb(
		Real64 const Tdb, // dry-bulb temperature {C}
		Real64 const W, // humidity ratio
		Real64 const Pb, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyTwbFnTdbWPb_raw(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

#else

	Real64
	PsyTwbFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

#endif

	Real64
	PsyVFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyWFnTdpPb(
		Real64 const TDP, // dew-point temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyWFnTdbH(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const H, // enthalpy {J/kg}
		Optional_string_const CalledFrom = _, // routine this function was called from (error messages)
		Optional_bool_const SuppressWarnings = _ // if calling function is calculating an intermediate state
	);

	Real64
	PsyWFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWBin, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyWFnTdbRhPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const RH, // relative humidity value (0.0-1.0)
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

#ifdef EP_cache_PsyPsatFnTemp

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	PsyPsatFnTemp_raw(
		Real64 const T, // dry-bulb temperature {C}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

#else

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

#endif

	Real64
	PsyTsatFnHPb(
		Real64 const H, // enthalpy {J/kg}
		Real64 const PB, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	F6(
		Real64 const X,
		Real64 const A0,
		Real64 const A1,
		Real64 const A2,
		Real64 const A3,
		Real64 const A4,
		Real64 const A5
	);

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
	);

	Real64
	PsyTsatFnPb(
		Real64 const Press, // barometric pressure {Pascals}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages)
	);

	Real64
	CPCW(
		Real64 const Temperature, // unused1208
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	CPHW(
		Real64 const Temperature, // unused1208
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

	Real64
	RhoH2O(
		Real64 const TB, // Dry bulb temperature. {C}
		Optional_string_const CalledFrom = _ // routine this function was called from (error messages) !unused1208
	);

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

#endif
