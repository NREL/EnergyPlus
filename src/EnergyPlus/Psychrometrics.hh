#ifndef Psychrometrics_hh_INCLUDED
#define Psychrometrics_hh_INCLUDED

// C++ Headers
#include <cassert>
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/Fmath.hh>
#include <ObjexxFCL/Optional.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

#define EP_psych_errors

namespace Psychrometrics {

#ifdef EP_psych_errors
	using namespace DataGlobals;
#endif

	// Data
	// MODULE PARAMETER DEFINITIONS:
	// call for recurring errors
	extern std::string const blank_string;

#ifndef EP_psych_errors
	extern Real64 const KelvinConv;
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

	// Subroutine Specifications for the Module

	// Functions

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
		std::string const & CalledFrom // routine this function was called from (error messages) !unused1208
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
	PsyHfgAirFnWTdb(
		Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
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
		Real64 const w, // humidity ratio {kgWater/kgDryAir} !unused1208
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

	Real64
	PsyTwbFnTdbWPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const dW, // humidity ratio
		Real64 const Patm, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

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

	Real64
	PsyPsatFnTemp(
		Real64 const T, // dry-bulb temperature {C}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);


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

	Real64
	PsyWFnTdbTwbPb(
		Real64 const TDB, // dry-bulb temperature {C}
		Real64 const TWBin, // wet-bulb temperature {C}
		Real64 const PB, // barometric pressure {Pascals}
		std::string const & CalledFrom = blank_string // routine this function was called from (error messages)
	);

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
		Real64 const Temperature // unused1208
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
		Real64 const Temperature // unused1208
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
