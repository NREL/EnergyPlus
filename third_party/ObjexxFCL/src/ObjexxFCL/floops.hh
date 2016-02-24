#ifndef ObjexxFCL_floops_hh_INCLUDED
#define ObjexxFCL_floops_hh_INCLUDED

// Fortran-Compatible DO Loop Support
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Semantics:
//  Fortran:   DO i = b, e, s
//  C++ equiv: for ( i = b, i_end = floop_end( b, e, s ); i != i_end; i += s )
//  C++ equiv: for ( std::size_t j = 1, n = floops( i, b, e, s ); j <= n; ++j, i += s )

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cstddef>

namespace ObjexxFCL {

// Fortran DO Loop End Value
template< typename T >
inline
T
floop_end( T const & b, T const & e, T const & s = T( 1 ) ) // DO i = b, e, s
{
	// Returns the termination value of i
	return b + std::max( T( ( e - b + s ) / s ), T( 0 ) ) * s;
}

// Fortran DO Loop Control
template< typename I, typename B, typename E, typename S >
inline
std::size_t
floops( I & i, B const & b, E const & e, S const & s ) // DO i = b, e, s
{
	// Initializes the DO variable i and returns the number of iterations count

	// Convert control expressions to type of loop control variable
	I const n1( b );
	I const n2( e );
	I const n3( s );
	I const ZERO( 0 );

	// Initialize loop variable (Fortran does this even if loop never executes)
	i = n1;

	// Get loop count being careful to avoid negative values in case I is an unsigned type
	if ( n1 < n2 ) {
		if ( n3 < ZERO ) {
			return 0;
		} else { // n3 > 0
			assert( n3 > ZERO );
			return ( ( n2 - n1 ) + n3 ) / n3;
		}
	} else if ( n1 > n2 ) {
		if ( n3 > ZERO ) {
			return 0;
		} else { // n3 < 0
			assert( n3 < ZERO );
			return ( ( n1 - n2 ) - n3 ) / -n3;
		}
	} else { // n1 == n2
		// Allow n3 == 0 in this case but Fortran doesn't
		return 1;
	}
}

// Fortran DO Loop Control with Implicit Step=1
template< typename I, typename B, typename E >
inline
std::size_t
floops( I & i, B const & b, E const & e ) // DO i = b, e
{
	return floops( i, b, e, 1 );
}

} // ObjexxFCL

#endif // ObjexxFCL_floops_hh_INCLUDED
