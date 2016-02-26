#ifndef ObjexxFCL_numeric_hh_INCLUDED
#define ObjexxFCL_numeric_hh_INCLUDED

// Numeric Support Functions
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

// C++ Headers
#include <cmath>
#include <complex>
#include <cstdint>
#include <limits>
#include <string>
#include <type_traits>

namespace ObjexxFCL {

inline
int
kind( bool const & )
{
	return 4; // Assumes bool is standing for LOGICAL
}

inline
int
kind( std::int8_t const & )
{
	return 1;
}

inline
int
kind( std::int16_t const & )
{
	return 2;
}

inline
int
kind( std::int32_t const & )
{
	return 4;
}

inline
int
kind( std::int64_t const & )
{
	return 8;
}

inline
int
kind( std::uint8_t const & )
{
	return 1;
}

inline
int
kind( std::uint16_t const & )
{
	return 2;
}

inline
int
kind( std::uint32_t const & )
{
	return 4;
}

inline
int
kind( std::uint64_t const & )
{
	return 8;
}

inline
int
kind( float const & )
{
	return 4;
}

inline
int
kind( double const & )
{
	return 8;
}

inline
int
kind( long double const & )
{
	return 16; // Assumes long double standing in for REAL(16)
}

inline
int
kind( std::complex< float > const & )
{
	return 4;
}

inline
int
kind( std::complex< double > const & )
{
	return 8;
}

inline
int
kind( std::complex< long double > const & )
{
	return 16;
}

inline
int
kind( char const & )
{
	return 1;
}

inline
int
kind( std::string const & )
{
	return 1;
}

template< typename T >
inline
int
KIND( T const & x )
{
	return kind( x );
}

int
selected_int_kind( int const r );

template< typename R >
inline
int
SELECTED_INT_KIND( R const r )
{
	return selected_int_kind( r );
}

int
selected_real_kind( int const p = 0, int const r = 0 ); // Fortran 2008 variant with radix argument not supported

inline
int
SELECTED_REAL_KIND( int const p = 0, int const r = 0 ) // Fortran 2008 variant with radix argument not supported
{
	return selected_real_kind( p, r );
}

inline
int
selected_char_kind( char const & )
{
	return -1;
}

int
selected_char_kind( std::string const & s );

template< typename S >
inline
int
SELECTED_CHAR_KIND( S const & s )
{
	return selected_char_kind( s );
}

template< typename T >
inline
int
radix( T const & )
{
	return std::numeric_limits< T >::radix;
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
digits( T const & )
{
	return std::numeric_limits< T >::digits;
}

template< typename T >
inline
int
DIGITS( T const & )
{
	return std::numeric_limits< T >::digits;
}

inline
int
range( std::int8_t const & )
{
	return 2;
}

inline
int
range( std::int16_t const & )
{
	return 4;
}

inline
int
range( std::int32_t const & )
{
	return 9;
}

inline
int
range( std::int64_t const & )
{
	return 18;
}

inline
int
range( std::uint8_t const & )
{
	return 2;
}

inline
int
range( std::uint16_t const & )
{
	return 4;
}

inline
int
range( std::uint32_t const & )
{
	return 9;
}

inline
int
range( std::uint64_t const & )
{
	return 18;
}

inline
int
range( float const & )
{
	return 37;
}

inline
int
range( double const & )
{
	return 307;
}

inline
int
range( long double const & )
{
	return 4931; // Assumes long double standing in for REAL(16)
}

inline
int
range( std::complex< float > const & )
{
	return 37;
}

inline
int
range( std::complex< double > const & )
{
	return 307;
}

inline
int
range( std::complex< long double > const & )
{
	return 4931;
}

template< typename T >
inline
int
RANGE( T const & x )
{
	return range( x );
}

// Huge: Largest Number of the Type
template< typename T >
inline
T
huge( T const & )
{
	return std::numeric_limits< T >::max();
}

// Tiny: Smallest Number of the Type
template< typename T >
inline
T
tiny( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::min();
}

// Tiny: Smallest Number of the Type
template< typename T >
inline
T
TINY( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::min();
}

// Epsilon: Smallest Number of the Type
template< typename T >
inline
T
epsilon( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::epsilon();
}

// Epsilon: Smallest Number of the Type
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
precision( T const & x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return int( ( digits( x ) - 1 ) * std::log10( double( radix( x ) ) ) ); // + 1 if radix(x) == 10^N but we don't support non-traditional architectures
}

template< typename T >
inline
int
precision( std::complex< T > const & x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return int( ( digits( x.real() ) - 1 ) * std::log10( double( radix( x.real() ) ) ) ); // + 1 if radix(x) == 10^N but we don't support non-traditional architectures
}

template< typename T >
inline
int
PRECISION( T const & x )
{
	return precision( x );
}

template< typename T >
inline
T
minexponent( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::min_exponent;
}

template< typename T >
inline
T
MINEXPONENT( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::min_exponent;
}

template< typename T >
inline
T
maxexponent( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::max_exponent;
}

template< typename T >
inline
T
MAXEXPONENT( T const & )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return std::numeric_limits< T >::max_exponent;
}

template< typename T >
inline
int
exponent( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return ( x != T( 0 ) ? int( std::log2( std::abs( x ) ) ) + 1 : 0 );
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
fraction( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return x * std::pow( T( radix( x ) ), T( -exponent( x ) ) );
}

template< typename T >
inline
T
FRACTION( T const x )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return x * std::pow( T( radix( x ) ), T( -exponent( x ) ) );
}

template< typename T >
inline
T
set_exponent( T const x, int const i )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return ( x != T( 0 ) ? x * std::pow( T( radix( x ) ), T( i - exponent( x ) ) ) : T( 0 ) );
}

template< typename T >
inline
T
SET_EXPONENT( T const x, int const i )
{
	static_assert( std::is_floating_point< T >::value, "Floating point argument required" );
	return ( x != T( 0 ) ? x * std::pow( T( radix( x ) ), T( i - exponent( x ) ) ) : T( 0 ) );
}

} // ObjexxFCL

#endif // ObjexxFCL_numeric_hh_INCLUDED
