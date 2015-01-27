// C++ Headers
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// EnergyPlus Headers
#include <TARCOGCommon.hh>
#include <DataGlobals.hh>
#include <DataPrecisionGlobals.hh>
#include <TARCOGParams.hh>

namespace EnergyPlus {

namespace TARCOGCommon {

	// MODULE INFORMATION:
	//       AUTHOR         Simon Vidanovic
	//       DATE WRITTEN   June/22/2010
	//       MODIFIED       na
	//       RE-ENGINEERED  na
	//  Revision: 6.0.36  (June/22/2010)
	//   - Initial setup, extracted from TARCOG.for

	// PURPOSE OF THIS MODULE:
	// A module which contains common TARCOG functions and subroutines

	// METHODOLOGY EMPLOYED:
	// <description>

	// REFERENCES:
	// na

	// OTHER NOTES:
	// na

	// USE STATEMENTS:

	// Using/Aliasing
	using namespace DataPrecisionGlobals;

	// Functions

	bool
	IsShadingLayer( int const layertype )
	{

		// Using/Aliasing
		using namespace TARCOGParams;

		// Return value
		bool IsShadingLayer;

		if ( ( layertype == VENETBLIND ) || ( layertype == WOVSHADE ) || ( layertype == PERFORATED ) || ( layertype == BSDF ) ) {
			IsShadingLayer = true;
		} else {
			IsShadingLayer = false;
		}
		return IsShadingLayer;

	}

	Real64
	LDSumMax(
		Real64 const Width,
		Real64 const Height
	)
	{
		// LDSumMax function calculates sum part of equation for maximum deflection
		// Width - glazing system width
		// Height - glazing system height

		// Using/Aliasing
		using DataGlobals::PiOvr2;
		using namespace TARCOGParams;
		//use TARCOGGassesParams

		// Return value
		Real64 LDSumMax;

		// Locals
		int i;
		int j;

		LDSumMax = 0.0;
		for ( i = 1; i <= mmax; i += 2 ) {
			Real64 const sin_i( std::sin( i * PiOvr2 ) );
			Real64 const pow_i_W( pow_2( i / Width ) );
			for ( j = 1; j <= nmax; j += 2 ) {
				LDSumMax += ( sin_i * std::sin( j * PiOvr2 ) ) / ( i * j * pow_2( pow_i_W + pow_2( j / Height ) ) );
			} //do j = 1, nmax, 2
		} //do i = 1, mmax, 2

		return LDSumMax;
	}

	Real64
	LDSumMean(
		Real64 const Width,
		Real64 const Height
	)
	{
		// LDSumMean function calculates sum part of equation for mean deflection
		// Width - glazing system width
		// Height - glazing system height

		// Using/Aliasing
		using DataGlobals::Pi;
		using namespace TARCOGParams;
		//use TARCOGGassesParams

		// Return value
		Real64 LDSumMean;

		// Locals
		static Real64 const Pi_squared( Pi * Pi );
		int i;
		int j;

		LDSumMean = 0.0;
		for ( i = 1; i <= mmax; i += 2 ) {
			Real64 const pow_i_Pi_2( i * i * Pi_squared );
			Real64 const pow_i_W( pow_2( i / Width ) );
			for ( j = 1; j <= nmax; j += 2 ) {
				LDSumMean += 4.0 / ( pow_i_Pi_2 * pow_2( j ) * pow_2( pow_i_W + pow_2( j / Height ) ) );
			} //do j = 1, nmax, 2
		} //do i = 1, mmax, 2

		return LDSumMean;
	}

	void
	matrixQBalance(
		int const nlayer,
		FArray2A< Real64 > a,
		FArray1A< Real64 > b,
		FArray1A< Real64 > const scon,
		FArray1A< Real64 > const thick,
		FArray1A< Real64 > const hcgas,
		Real64 const hcout,
		Real64 const hcin,
		FArray1A< Real64 > const asol,
		FArray1A< Real64 > const qv,
		Real64 const Tin,
		Real64 const Tout,
		Real64 const Gin,
		Real64 const Gout,
		FArray1A< Real64 > const theta,
		FArray1A< Real64 > const tir,
		FArray1A< Real64 > const rir,
		FArray1A< Real64 > const emis
	)
	{

		// Using/Aliasing
		using DataGlobals::StefanBoltzmann;
		using namespace TARCOGParams;

		// Argument array dimensioning
		a.dim( 4*nlayer, 4*nlayer );
		b.dim( 4*nlayer );
		scon.dim( maxlay );
		thick.dim( maxlay );
		hcgas.dim( maxlay1 );
		asol.dim( maxlay );
		qv.dim( maxlay1 );
		theta.dim( maxlay2 );
		tir.dim( maxlay2 );
		rir.dim( maxlay2 );
		emis.dim( maxlay2 );

		// Locals
		// local variables
		int i;
		int j;
		int k;
		int front;
		int back;

		for ( i = 1; i <= 4 * nlayer; ++i ) {
			b( i ) = 0.0;
			for ( j = 1; j <= 4 * nlayer; ++j ) {
				a( i, j ) = 0.0;
			}
		}

		//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		//!!!!!!!!!!!!!!!!!!!!  build matrix a   !!!!!!!!!!!!!!!!!!!!!!!!!
		//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		// Because of build matrix optimization all environmental heat transfer
		// coefficients were stored in hgas array.  This means that hgas(1) is actually
		// hout, while hgas(nlayer+1) is actually hin.  Same is valid for hcgas and
		// hrgas arrays
		for ( i = 1; i <= nlayer; ++i ) {
			k = 4 * i - 3;
			front = 2 * i - 1;
			back = 2 * i;
			if ( nlayer != 1 ) {
				if ( i != 1 ) {
					a( k, k - 3 ) = -hcgas( i );
					a( k, k - 1 ) = -1.0;
					a( k + 1, k - 3 ) = -hcgas( i );
					a( k + 1, k - 1 ) = -1.0;
					a( k + 2, k - 1 ) = rir( front );
					a( k + 3, k - 1 ) = tir( front );
				}
				if ( i != nlayer ) {
					a( k, k + 4 ) = -hcgas( i + 1 );
					a( k, k + 6 ) = -1.0;
					a( k + 2, k + 6 ) = tir( back );
					a( k + 3, k + 6 ) = rir( back );
				}
			}
			a( k, k ) = hcgas( i );
			a( k, k + 1 ) = hcgas( i + 1 );
			a( k, k + 2 ) = 1.0;
			a( k, k + 3 ) = 1.0;
			a( k + 1, k ) = scon( i ) / thick( i ) + hcgas( i );
			a( k + 1, k + 1 ) = -scon( i ) / thick( i );
			a( k + 1, k + 2 ) = 1.0;
			a( k + 2, k ) = emis( front ) * StefanBoltzmann * pow_3( theta( front ) );
			a( k + 2, k + 2 ) = -1.0;
			a( k + 3, k + 1 ) = emis( back ) * StefanBoltzmann * pow_3( theta( back ) );
			a( k + 3, k + 3 ) = -1.0;
		}

		//build matrix b
		for ( i = 1; i <= nlayer; ++i ) {
			k = 4 * i - 3;
			front = 2 * i - 1;
			back = 2 * i;
			b( k ) = asol( i ) + 0.5 * qv( i ) + 0.5 * qv( i + 1 );
			b( k + 1 ) = 0.5 * asol( i ) + 0.5 * qv( i );
			if ( i == 1 ) {
				b( k ) += hcout * Tout + Gout;
				b( k + 1 ) += hcout * Tout + Gout;
				b( k + 2 ) -= rir( front ) * Gout;
				b( k + 3 ) -= tir( front ) * Gout;
			}
			if ( i == ( nlayer ) ) {
				b( k ) += hcin * Tin + Gin;
				b( k + 2 ) -= tir( back ) * Gin;
				b( k + 3 ) -= rir( back ) * Gin;
			}
		}

	}

	void
	EquationsSolver(
		FArray2A< Real64 > a,
		FArray1A< Real64 > b,
		int const n,
		int & nperr,
		std::string & ErrorMessage
	)
	{
		//***********************************************************************
		// Purpose: solves the main system of energy balance equations
		//***********************************************************************
		// Input:
		//   a - matrix, radiositied
		//   b - vector of known quantities
		//   n - ???
		// Output:
		//   b - solutions
		//   nperr - error code

		// Using/Aliasing
		using namespace TARCOGParams;

		// Argument array dimensioning
		a.dim( n, n );
		b.dim( n );

		// Locals
		FArray1D_int indx( n );
		Real64 d;

		ludcmp( a, n, indx, d, nperr, ErrorMessage );

		// Exit on error
		if ( ( nperr > 0 ) && ( nperr <= 1000 ) ) return;

		lubksb( a, n, indx, b );

	}

	void
	ludcmp(
		FArray2A< Real64 > a,
		int const n,
		FArray1A_int indx,
		Real64 & d,
		int & nperr,
		std::string & ErrorMessage
	)
	{

		// Argument array dimensioning
		a.dim( n, n );
		indx.dim( n );

		// Locals
		int const NMAX( 500 );
		Real64 const TINY( 1.0e-20 );

		int i;
		int imax;
		int j;
		int k;
		Real64 aamax;
		Real64 dum;
		Real64 sum;
		FArray1D< Real64 > vv( NMAX );

		d = 1.0;
		for ( i = 1; i <= n; ++i ) {
			aamax = 0.0;
			for ( j = 1; j <= n; ++j ) {
				if ( std::abs( a( i, j ) ) > aamax ) aamax = std::abs( a( i, j ) );
			} // j
			if ( aamax == 0.0 ) {
				nperr = 13;
				ErrorMessage = "Singular matrix in ludcmp.";
				return;
			}
			vv( i ) = 1.0 / aamax;
		} // i

		for ( j = 1; j <= n; ++j ) {
			for ( i = 1; i <= j - 1; ++i ) {
				sum = a( i, j );
				for ( k = 1; k <= i - 1; ++k ) {
					sum -= a( i, k ) * a( k, j );
				} // k
				a( i, j ) = sum;
			} // i
			aamax = 0.0;
			for ( i = j; i <= n; ++i ) {
				sum = a( i, j );
				for ( k = 1; k <= j - 1; ++k ) {
					sum -= a( i, k ) * a( k, j );
				} // k
				a( i, j ) = sum;
				dum = vv( i ) * std::abs( sum );
				if ( dum >= aamax ) {
					imax = i;
					aamax = dum;
				}
			} // i
			if ( j != imax ) {
				for ( k = 1; k <= n; ++k ) {
					dum = a( imax, k );
					a( imax, k ) = a( j, k );
					a( j, k ) = dum;
				} // k
				d = -d;
				vv( imax ) = vv( j );
			}
			indx( j ) = imax;
			if ( a( j, j ) == 0.0 ) a( j, j ) = TINY;
			if ( j != n ) {
				dum = 1.0 / a( j, j );
				for ( i = j + 1; i <= n; ++i ) {
					a( i, j ) *= dum;
				} // i
			}
		} // j

	}

	void
	lubksb(
		FArray2A< Real64 > const a,
		int const n,
		FArray1A_int const indx,
		FArray1A< Real64 > b
	)
	{
		//***********************************************************************
		//***********************************************************************

		// Argument array dimensioning
		a.dim( n, n );
		indx.dim( n );
		b.dim( n );

		// Locals
		int i;
		int ii;
		int j;
		int ll;
		Real64 sum;

		ii = 0;
		for ( i = 1; i <= n; ++i ) {
			ll = indx( i );
			sum = b( ll );
			b( ll ) = b( i );
			if ( ii != 0 ) {
				for ( j = ii; j <= i - 1; ++j ) {
					sum -= a( i, j ) * b( j );
				} // j
			} else if ( sum != 0.0 ) {
				ii = i;
			}
			b( i ) = sum;
		} // i

		for ( i = n; i >= 1; --i ) {
			sum = b( i );
			for ( j = i + 1; j <= n; ++j ) {
				sum -= a( i, j ) * b( j );
			} // j
			b( i ) = sum / a( i, i );
		} // i

	}

	Real64
	pos( Real64 const x )
	{
		//***********************************************************************
		//***********************************************************************

		// Return value
		Real64 pos;

		pos = ( x + std::abs( x ) ) / 2.0;

		return pos;
	}

	//     NOTICE

	//     Copyright � 1996-2014 The Board of Trustees of the University of Illinois
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

} // TARCOGCommon

} // EnergyPlus
