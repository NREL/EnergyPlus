#ifndef ObjexxFCL_numeric_hh_INCLUDED
#define ObjexxFCL_numeric_hh_INCLUDED

// Numeric Support Functions
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array.hh>

// C++ Headers
#include <cmath>
#include <complex>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <type_traits>

namespace ObjexxFCL {

template< typename T >
inline
int
RADIX( T const & )
{
	return std::numeric_limits< T >::radix;
}

template< typename T >
inline
T
HUGE_( T const & )
{
	return std::numeric_limits< T >::max();
}

template< typename T >
inline
T
TINY( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::min();
}

template< typename T >
inline
int
EXPONENT( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return ( x != T( 0 ) ? int( std::log2( std::abs( x ) ) ) + 1 : 0 );
}

template< typename T >
inline
T
FRACTION( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return x * std::pow( T( RADIX( x ) ), T( -EXPONENT( x ) ) );
}

template< typename T, typename Y >
inline
T
NEAREST( T const x, Y const y )
{
	assert( y != Y( 0 ) );
	if ( y >= Y( 0 ) ) {
		return std::nextafter( x, std::numeric_limits< T >::max() );
	} else {
		return std::nextafter( x, std::numeric_limits< T >::lowest() );
	}
}

} // ObjexxFCL

#endif // ObjexxFCL_numeric_hh_INCLUDED
