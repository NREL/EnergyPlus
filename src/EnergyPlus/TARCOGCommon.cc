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
#include <cmath>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
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
		Array2< Real64 > & a,
		Array1< Real64 > & b,
		Array1< Real64 > const & scon,
		Array1< Real64 > const & thick,
		Array1< Real64 > const & hcgas,
		Real64 const hcout,
		Real64 const hcin,
		Array1< Real64 > const & asol,
		Array1< Real64 > const & qv,
		Real64 const Tin,
		Real64 const Tout,
		Real64 const Gin,
		Real64 const Gout,
		Array1< Real64 > const & theta,
		Array1< Real64 > const & tir,
		Array1< Real64 > const & rir,
		Array1< Real64 > const & emis
	)
	{

		// Using/Aliasing
		using DataGlobals::StefanBoltzmann;
		using namespace TARCOGParams;

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
				a( j, i ) = 0.0;
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
					a( k - 3, k ) = -hcgas( i );
					a( k - 1, k ) = -1.0;
					a( k - 3, k + 1 ) = -hcgas( i );
					a( k - 1, k + 1 ) = -1.0;
					a( k - 1, k + 2 ) = rir( front );
					a( k - 1, k + 3 ) = tir( front );
				}
				if ( i != nlayer ) {
					a( k + 4, k ) = -hcgas( i + 1 );
					a( k + 6, k ) = -1.0;
					a( k + 6, k + 2 ) = tir( back );
					a( k + 6, k + 3 ) = rir( back );
				}
			}
			a( k, k ) = hcgas( i );
			a( k + 1, k ) = hcgas( i + 1 );
			a( k + 2, k ) = 1.0;
			a( k + 3, k ) = 1.0;
			a( k, k + 1 ) = scon( i ) / thick( i ) + hcgas( i );
			a( k + 1, k + 1 ) = -scon( i ) / thick( i );
			a( k + 2, k + 1 ) = 1.0;
			a( k, k + 2 ) = emis( front ) * StefanBoltzmann * pow_3( theta( front ) );
			a( k + 2, k + 2 ) = -1.0;
			a( k + 1, k + 3 ) = emis( back ) * StefanBoltzmann * pow_3( theta( back ) );
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
		Array2< Real64 > & a,
		Array1< Real64 > & b,
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

		// Locals
		Array1D_int indx( n );
		Real64 d;

		ludcmp( a, n, indx, d, nperr, ErrorMessage );

		// Exit on error
		if ( ( nperr > 0 ) && ( nperr <= 1000 ) ) return;

		lubksb( a, n, indx, b );

	}

	void
	ludcmp(
		Array2< Real64 > & a,
		int const n,
		Array1_int & indx,
		Real64 & d,
		int & nperr,
		std::string & ErrorMessage
	)
	{

		// Locals
		static int const NMAX( 500 );
		static Array1D< Real64 > vv( NMAX );

		Real64 const TINY( 1.0e-20 );

		int i;
		int imax;
		int j;
		int k;
		Real64 aamax;
		Real64 dum;
		Real64 sum;

		d = 1.0;
		for ( i = 1; i <= n; ++i ) {
			aamax = 0.0;
			for ( j = 1; j <= n; ++j ) {
				if ( std::abs( a( j, i ) ) > aamax ) aamax = std::abs( a( j, i ) );
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
				sum = a( j, i );
				for ( k = 1; k <= i - 1; ++k ) {
					sum -= a( k, i ) * a( j, k );
				} // k
				a( j, i ) = sum;
			} // i
			aamax = 0.0;
			for ( i = j; i <= n; ++i ) {
				sum = a( j, i );
				for ( k = 1; k <= j - 1; ++k ) {
					sum -= a( k, i ) * a( j, k );
				} // k
				a( j, i ) = sum;
				dum = vv( i ) * std::abs( sum );
				if ( dum >= aamax ) {
					imax = i;
					aamax = dum;
				}
			} // i
			if ( j != imax ) {
				for ( k = 1; k <= n; ++k ) {
					dum = a( k, imax );
					a( k, imax ) = a( k, j );
					a( k, j ) = dum;
				} // k
				d = -d;
				vv( imax ) = vv( j );
			}
			indx( j ) = imax;
			if ( a( j, j ) == 0.0 ) a( j, j ) = TINY;
			if ( j != n ) {
				dum = 1.0 / a( j, j );
				for ( i = j + 1; i <= n; ++i ) {
					a( j, i ) *= dum;
				} // i
			}
		} // j

	}

	void
	lubksb(
		Array2A< Real64 > const a,
		int const n,
		Array1A_int const indx,
		Array1A< Real64 > b
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
					sum -= a( j, i ) * b( j );
				} // j
			} else if ( sum != 0.0 ) {
				ii = i;
			}
			b( i ) = sum;
		} // i

		for ( i = n; i >= 1; --i ) {
			sum = b( i );
			for ( j = i + 1; j <= n; ++j ) {
				sum -= a( j, i ) * b( j );
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

} // TARCOGCommon

} // EnergyPlus
