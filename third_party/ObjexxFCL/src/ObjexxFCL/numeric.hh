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

inline
int
KIND( bool const & )
{
	return 4; // Assumes bool is standing for LOGICAL
}

inline
int
KIND( std::int8_t const & )
{
	return 1;
}

inline
int
KIND( std::int16_t const & )
{
	return 2;
}

inline
int
KIND( std::int32_t const & )
{
	return 4;
}

inline
int
KIND( std::int64_t const & )
{
	return 8;
}

inline
int
KIND( std::uint8_t const & )
{
	return 1;
}

inline
int
KIND( std::uint16_t const & )
{
	return 2;
}

inline
int
KIND( std::uint32_t const & )
{
	return 4;
}

inline
int
KIND( std::uint64_t const & )
{
	return 8;
}

inline
int
KIND( float const & )
{
	return 4;
}

inline
int
KIND( double const & )
{
	return 8;
}

inline
int
KIND( long double const & )
{
	return 16; // Assumes long double standing in for REAL(16)
}

inline
int
KIND( std::complex< float > const & )
{
	return 4;
}

inline
int
KIND( std::complex< double > const & )
{
	return 8;
}

inline
int
KIND( std::complex< long double > const & )
{
	return 16;
}

inline
int
KIND( char const & )
{
	return 1;
}

inline
int
KIND( std::string const & )
{
	return 1;
}

template< typename T >
inline
int
KIND( T const & )
{
	return static_cast< int >( sizeof( T ) );
}

int
SELECTED_INT_KIND( int const r );

int
SELECTED_REAL_KIND( int const p = 0, int const r = 0, int const radix = 2 );

inline
int
SELECTED_CHAR_KIND( char const & )
{
	return -1;
}

int
SELECTED_CHAR_KIND( std::string const & s );

inline
std::size_t
SIZEOF( std::string const & x )
{
	return x.length();
}

std::size_t
SIZEOF( char const * x );

template< typename T >
inline
std::size_t
SIZEOF( Array< T > const & x )
{
	return x.size() * sizeof( T );
}

template< typename T, class = typename std::enable_if< ! std::is_base_of< BArray, T >::value >::type >
inline
std::size_t
SIZEOF( T const & x )
{
	return sizeof( x );
}

template< typename T >
inline
int
RADIX( T const & )
{
	return std::numeric_limits< T >::radix;
}

template< typename T >
inline
int
DIGITS( T const & )
{
	return std::numeric_limits< T >::digits;
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
T
EPSILON( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::epsilon();
}

template< typename T >
inline
int
PRECISION( T const & x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return int( ( DIGITS( x ) - 1 ) * std::log10( double( RADIX( x ) ) ) ); // + 1 if radix(x) == 10^N but we don't support non-traditional architectures
}

template< typename T >
inline
int
PRECISION( std::complex< T > const & x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return int( ( DIGITS( x.real() ) - 1 ) * std::log10( double( RADIX( x.real() ) ) ) ); // + 1 if radix(x) == 10^N but we don't support non-traditional architectures
}

inline
int
EXPONENT_RANGE( std::int8_t const & )
{
	return 2;
}

inline
int
EXPONENT_RANGE( std::int16_t const & )
{
	return 4;
}

inline
int
EXPONENT_RANGE( std::int32_t const & )
{
	return 9;
}

inline
int
EXPONENT_RANGE( std::int64_t const & )
{
	return 18;
}

inline
int
EXPONENT_RANGE( std::uint8_t const & )
{
	return 2;
}

inline
int
EXPONENT_RANGE( std::uint16_t const & )
{
	return 4;
}

inline
int
EXPONENT_RANGE( std::uint32_t const & )
{
	return 9;
}

inline
int
EXPONENT_RANGE( std::uint64_t const & )
{
	return 18;
}

inline
int
EXPONENT_RANGE( float const & )
{
	return 37;
}

inline
int
EXPONENT_RANGE( double const & )
{
	return 307;
}

inline
int
EXPONENT_RANGE( long double const & )
{
	return 4931; // Assumes long double standing in for REAL(16)
}

inline
int
EXPONENT_RANGE( std::complex< float > const & )
{
	return 37;
}

inline
int
EXPONENT_RANGE( std::complex< double > const & )
{
	return 307;
}

inline
int
EXPONENT_RANGE( std::complex< long double > const & )
{
	return 4931;
}

template< typename T >
inline
int
MINEXPONENT( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::min_exponent;
}

template< typename T >
inline
int
MAXEXPONENT( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::max_exponent;
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
SET_EXPONENT( T const x, int const i )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return ( x != T( 0 ) ? x * std::pow( T( RADIX( x ) ), T( i - EXPONENT( x ) ) ) : T( 0 ) );
}

template< typename T >
inline
T
SCALE( T const x, int const i )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return ( x != T( 0 ) ? x * std::pow( T( RADIX( x ) ), T( i ) ) : T( 0 ) );
}

template< typename T >
inline
T
FRACTION( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return x * std::pow( T( RADIX( x ) ), T( -EXPONENT( x ) ) );
}

template< typename T >
inline
T
SPACING( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	if ( x == T( 0 ) ) return TINY( x );
	try {
		return std::pow( T( RADIX( x ) ), std::max( T( EXPONENT( x ) - DIGITS( x ) ), T( MINEXPONENT( x ) - 1 ) ) );
	} catch (...) {
		return TINY( x );
	}
}

template< typename T >
inline
T
RECIPROCAL_RELATIVE_SPACING( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::abs( FRACTION( x ) ) * std::pow( double( RADIX( x ) ), DIGITS( x ) );
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
