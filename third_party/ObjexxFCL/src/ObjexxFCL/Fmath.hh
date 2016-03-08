#ifndef ObjexxFCL_Fmath_hh_INCLUDED
#define ObjexxFCL_Fmath_hh_INCLUDED

// Fortran Intrinsic-Compatible and General Math Functions
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
#include <algorithm>
#include <cassert>
#include <cfloat>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <limits>
#include <type_traits>

namespace ObjexxFCL {

typedef  std::intmax_t  SSize;

// min Functions

// min( short, short )
inline
short int
min( short int const a, short int const b )
{
	return ( a < b ? a : b );
}

// min( int, int )
inline
int
min( int const a, int const b )
{
	return ( a < b ? a : b );
}

// min( long, long )
inline
long int
min( long int const a, long int const b )
{
	return ( a < b ? a : b );
}

// min( unsigned short, unsigned short )
inline
unsigned short int
min( unsigned short int const a, unsigned short int const b )
{
	return ( a < b ? a : b );
}

// min( unsigned, unsigned )
inline
unsigned int
min( unsigned int const a, unsigned int const b )
{
	return ( a < b ? a : b );
}

// min( unsigned long, unsigned long )
inline
unsigned long int
min( unsigned long int const a, unsigned long int const b )
{
	return ( a < b ? a : b );
}

// min( float, float )
inline
float
min( float const a, float const b )
{
	return ( a < b ? a : b );
}

// min( double, double )
inline
double
min( double const a, double const b )
{
	return ( a < b ? a : b );
}

// min( long double, long double )
inline
long double
min( long double const a, long double const b )
{
	return ( a < b ? a : b );
}

// Use std::min for 2 arguments not covered by the above overloads
using std::min;

// min( short, short, short )
inline
short int
min( short int a, short int b, short int c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( short, short, short, short, ... )
template< typename... Ts >
inline
short int
min( short int const a, short int const b, short int const c, short int const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( int, int, int )
inline
int
min( int a, int b, int c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( int, int, int, int, ... )
template< typename... Ts >
inline
int
min( int const a, int const b, int const c, int const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( long, long, long )
inline
long int
min( long int a, long int b, long int c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( long, long, long, long, ... )
template< typename... Ts >
inline
long int
min( long int const a, long int const b, long int const c, long int const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( unsigned short, unsigned short, unsigned short )
inline
unsigned short int
min( unsigned short int a, unsigned short int b, unsigned short int c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( unsigned short, unsigned short, unsigned short, unsigned short, ... )
template< typename... Ts >
inline
unsigned short int
min( unsigned short int const a, unsigned short int const b, unsigned short int const c, unsigned short int const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( unsigned, unsigned, unsigned )
inline
unsigned int
min( unsigned int a, unsigned int b, unsigned int c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( unsigned, unsigned, unsigned, unsigned, ... )
template< typename... Ts >
inline
unsigned int
min( unsigned int const a, unsigned int const b, unsigned int const c, unsigned int const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( unsigned long, unsigned long, unsigned long )
inline
unsigned long int
min( unsigned long int a, unsigned long int b, unsigned long int c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( unsigned long, unsigned long, unsigned long, unsigned long, ... )
template< typename... Ts >
inline
unsigned long int
min( unsigned long int const a, unsigned long int const b, unsigned long int const c, unsigned long int const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( float, float, float )
inline
float
min( float a, float b, float c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( float, float, float, float, ... )
template< typename... Ts >
inline
float
min( float const a, float const b, float const c, float const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( double, double, double )
inline
double
min( double a, double b, double c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( double, double, double, double, ... )
template< typename... Ts >
inline
double
min( double const a, double const b, double const c, double const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( long double, long double, long double )
inline
long double
min( long double a, long double b, long double c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( long double, long double, long double, long double, ... )
template< typename... Ts >
inline
long double
min( long double const a, long double const b, long double const c, long double const d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// min( a, b, c )
template< typename T >
inline
T const &
min( T const & a, T const & b, T const & c )
{
	return ( a < b ? ( a < c ? a : c ) : ( b < c ? b : c ) );
}

// min( a, b, c, d, ... )
template< typename T, typename... Ts >
inline
T const &
min( T const & a, T const & b, T const & c, T const & d, Ts const &... o )
{
	return min( a < b ? a : b, c < d ? c : d, o... );
}

// max Functions

// max( short, short )
inline
short int
max( short int const a, short int const b )
{
	return ( a < b ? b : a );
}

// max( int, int )
inline
int
max( int const a, int const b )
{
	return ( a < b ? b : a );
}

// max( long, long )
inline
long int
max( long int const a, long int const b )
{
	return ( a < b ? b : a );
}

// max( unsigned short, unsigned short )
inline
unsigned short int
max( unsigned short int const a, unsigned short int const b )
{
	return ( a < b ? b : a );
}

// max( unsigned, unsigned )
inline
unsigned int
max( unsigned int const a, unsigned int const b )
{
	return ( a < b ? b : a );
}

// max( unsigned long, unsigned long )
inline
unsigned long int
max( unsigned long int const a, unsigned long int const b )
{
	return ( a < b ? b : a );
}

// max( float, float )
inline
float
max( float const a, float const b )
{
	return ( a < b ? b : a );
}

// max( double, double )
inline
double
max( double const a, double const b )
{
	return ( a < b ? b : a );
}

// max( long double, long double )
inline
long double
max( long double const a, long double const b )
{
	return ( a < b ? b : a );
}

// Use std::max for 2 arguments not covered by the above overloads
using std::max;

// max( short, short, short )
inline
short int
max( short int a, short int b, short int c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( short, short, short, short, ... )
template< typename... Ts >
inline
short int
max( short int const a, short int const b, short int const c, short int const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( int, int, int )
inline
int
max( int a, int b, int c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( int, int, int, int, ... )
template< typename... Ts >
inline
int
max( int const a, int const b, int const c, int const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( long, long, long )
inline
long int
max( long int a, long int b, long int c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( long, long, long, long, ... )
template< typename... Ts >
inline
long int
max( long int const a, long int const b, long int const c, long int const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( unsigned short, unsigned short, unsigned short )
inline
unsigned short int
max( unsigned short int a, unsigned short int b, unsigned short int c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( unsigned short, unsigned short, unsigned short, unsigned short, ... )
template< typename... Ts >
inline
unsigned short int
max( unsigned short int const a, unsigned short int const b, unsigned short int const c, unsigned short int const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( unsigned, unsigned, unsigned )
inline
unsigned int
max( unsigned int a, unsigned int b, unsigned int c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( unsigned, unsigned, unsigned, unsigned, ... )
template< typename... Ts >
inline
unsigned int
max( unsigned int const a, unsigned int const b, unsigned int const c, unsigned int const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( unsigned long, unsigned long, unsigned long )
inline
unsigned long int
max( unsigned long int a, unsigned long int b, unsigned long int c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( unsigned long, unsigned long, unsigned long, unsigned long, ... )
template< typename... Ts >
inline
unsigned long int
max( unsigned long int const a, unsigned long int const b, unsigned long int const c, unsigned long int const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( float, float, float )
inline
float
max( float a, float b, float c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( float, float, float, float, ... )
template< typename... Ts >
inline
float
max( float const a, float const b, float const c, float const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( double, double, double )
inline
double
max( double a, double b, double c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( double, double, double, double, ... )
template< typename... Ts >
inline
double
max( double const a, double const b, double const c, double const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( long double, long double, long double )
inline
long double
max( long double a, long double b, long double c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( long double, long double, long double, long double, ... )
template< typename... Ts >
inline
long double
max( long double const a, long double const b, long double const c, long double const d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// max( a, b, c )
template< typename T >
inline
T const &
max( T const & a, T const & b, T const & c )
{
	return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
}

// max( a, b, c, d, ... )
template< typename T, typename... Ts >
inline
T const &
max( T const & a, T const & b, T const & c, T const & d, Ts const &... o )
{
	return max( a < b ? b : a, c < d ? d : c, o... );
}

// Math Functions

// abs( x ) == | x |
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
abs( T const & x )
{
	return ( x < T( 0 ) ? -x : x );
}

// FLOOR( x )
template< typename T >
inline
int
FLOOR( T const & x )
{
	return int( std::floor( x ) );
}

// CEILING( x )
template< typename T >
inline
int
CEILING( T const & x )
{
	return int( std::ceil( x ) );
}

// square( x ) == x^2
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
square( T const x )
{
	return x * x;
}

// cube( x ) == x^3
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
cube( T const x )
{
	return x * x * x;
}

// quad( x ) == x^4
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
quad( T const x )
{
	T const t( x * x );
	return t * t;
}

// pow_2( x ) == x^2
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_2( T const x )
{
	return x * x;
}

// pow_3( x ) == x^3
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_3( T const x )
{
	return x * x * x;
}

// pow_4( x ) == x^4
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_4( T const x )
{
	T const t( x * x );
	return t * t;
}

// pow_5( x ) == x^5
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_5( T const x )
{
	T const t( x * x );
	return t * t * x;
}

// pow_6( x ) == x^6
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_6( T const x )
{
	T const t( x * x * x );
	return t * t;
}

// pow_7( x ) == x^7
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_7( T const x )
{
	T const t( x * x * x );
	return t * t * x;
}

// pow_8( x ) == x^8
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_8( T const x )
{
	T t( x * x );
	t *= t;
	return t * t;
}

// pow_9( x ) == x^9
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
pow_9( T const x )
{
	T const t( x * x * x );
	return t * t * t;
}

// root_4( x ) == x^(1/4)
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
root_4( T const x )
{
	return std::sqrt( std::sqrt( x ) );
}

// root_8( x ) == x^(1/8)
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
T
root_8( T const x )
{
	return std::sqrt( std::sqrt( std::sqrt( x ) ) );
}

// square( x ) == x^2
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
square( T const & x )
{
	return x * x;
}

// cube( x ) == x^3
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
cube( T const & x )
{
	return x * x * x;
}

// quad( x ) == x^4
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
quad( T const & x )
{
	T const t( x * x );
	return t * t;
}

// pow_2( x ) == x^2
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_2( T const & x )
{
	return x * x;
}

// pow_3( x ) == x^3
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_3( T const & x )
{
	return x * x * x;
}

// pow_4( x ) == x^4
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_4( T const & x )
{
	T const t( x * x );
	return t * t;
}

// pow_5( x ) == x^5
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_5( T const & x )
{
	T const t( x * x );
	return t * t * x;
}

// pow_6( x ) == x^6
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_6( T const & x )
{
	T const t( x * x * x );
	return t * t;
}

// pow_7( x ) == x^7
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_7( T const & x )
{
	T const t( x * x * x );
	return t * t * x;
}

// pow_8( x ) == x^8
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_8( T const & x )
{
	T t( x * x );
	t *= t;
	return t * t;
}

// pow_9( x ) == x^9
template< typename T, class = typename std::enable_if< ! std::is_arithmetic< T >::value >::type >
inline
T
pow_9( T const & x )
{
	T const t( x * x * x );
	return t * t * t;
}

// sign( x )
template< typename T, class = typename std::enable_if< std::is_arithmetic< T >::value >::type >
inline
int
sign( T const & x )
{
	return ( x >= T( 0 ) ? +1 : -1 );
}

// Sign Transfer from Second Argument to First Argument
template< typename X, typename Y, class = typename std::enable_if< std::is_arithmetic< X >::value && std::is_arithmetic< Y >::value, X >::type >
inline
X
sign( X const & x, Y const & y )
{
	return ( y >= Y( 0 ) ? abs( x ) : -abs( x ) );
}

// Positive Difference
template< typename T >
inline
T
dim( T const & x, T const & y )
{
	return max( x - y, T( 0 ) );
}

// Nearest function selector class for R non-integer or T integer
template< typename R, typename T, bool >
struct NearestSelector
{
	static
	R
	nearest( T const & x )
	{
		return R( x );
	}
};

// Nearest function selector class for R integer and T non-integer
template< typename R, typename T >
struct NearestSelector< R, T, true >
{
	static
	R
	nearest( T const & x )
	{
		return R( x + ( sign( x ) * T( 0.5 ) ) );
	}
};

// nearest< R >( x ): Nearest R
template< typename R, typename T >
inline
R
nearest( T const & x )
{
	return NearestSelector< R, T, ( ( std::numeric_limits< R >::is_integer ) && ( ! std::numeric_limits< T >::is_integer ) ) >::nearest( x );
}

// nearest_size( x ): Nearest std::size_t
template< typename T >
inline
std::size_t
nearest_size( T const & x )
{
	return std::size_t( x > T( 0 ) ? x + ( sign( x ) * T( 0.5 ) ) : 0 );
}

// nearest_ssize( x ): Nearest SSize
template< typename T >
inline
SSize
nearest_ssize( T const & x )
{
	return SSize( x + ( sign( x ) * T( 0.5 ) ) );
}

// nearest_int( x ): Nearest int
template< typename T >
inline
int
nearest_int( T const & x )
{
	return static_cast< int >( x + ( sign( x ) * T( 0.5 ) ) );
}

// nint( x ): Nearest int
template< typename T >
inline
int
nint( T const & x )
{
	return static_cast< int >( x + ( sign( x ) * T( 0.5 ) ) );
}

// nsint( x ): Nearest short int
template< typename T >
inline
short int
nsint( T const & x )
{
	return static_cast< short int >( x + ( sign( x ) * T( 0.5 ) ) );
}

// nlint( x ): Nearest long int
template< typename T >
inline
long int
nlint( T const & x )
{
	return static_cast< long int >( x + ( sign( x ) * T( 0.5 ) ) );
}

// nlint( x ): Nearest long int
template< typename T >
inline
int64_t
nint64( T const & x )
{
	return static_cast< int64_t >( x + ( sign( x ) * T( 0.5 ) ) );
}

// Mod function selector class for non-integer types
template< typename T, bool >
struct ModSelector
{
	static
	T
	mod( T const & x, T const & y )
	{
		return ( y != T( 0 ) ? x - ( T( static_cast< SSize >( x / y ) ) * y ) : T( 0 ) );
	}
};

// Mod function selector class for integer types
//  When used with negative integer arguments this assumes integer division
//   rounds towards zero (de facto and future official standard)
template< typename T >
struct ModSelector< T, true >
{
	static
	T
	mod( T const & x, T const & y )
	{
		return ( y != T( 0 ) ? x - ( ( x / y ) * y ) : T( 0 ) );
	}
};

// x(mod y) computational modulo returning magnitude < | y | and sign of x
//  When used with negative integer arguments this assumes integer division
//   rounds towards zero (de facto and future official standard)
template< typename T >
inline
T
mod( T const & x, T const & y )
{
	return ModSelector< T, std::numeric_limits< T >::is_integer >::mod( x, y );
}

// i(mod n) : float Arguments
inline
float
mod( float const & i, float const & n )
{
	assert( n != 0.0f );
	return ( n == 0.0f ? 0.0f : std::fmod( i, n ) );
}

// i(mod n) : double Arguments
inline
double
mod( double const & i, double const & n )
{
	assert( n != 0.0 );
	return ( n == 0.0 ? 0.0 : std::fmod( i, n ) );
}

// i(mod n) : long double Arguments
inline
long double
mod( long double const & i, long double const & n )
{
	assert( n != 0.0l );
	return ( n == 0.0l ? 0.0l : std::fmod( i, n ) );
}

// Modulo function selector class for non-integer types
template< typename T, bool >
struct ModuloSelector
{
	static
	T
	modulo( T const & x, T const & y )
	{
		return ( y != T( 0 ) ? x - ( std::floor( x / y ) * y ) : T( 0 ) );
	}
};

// Modulo function selector class for integer types
template< typename T >
struct ModuloSelector< T, true >
{
	static
	T
	modulo( T const & x, T const & y )
	{
		return ( y != T( 0 ) ? x - ( T( std::floor( static_cast< long double >( x ) / y ) ) * y ) : T( 0 ) );
	}
};

// x(mod y) mathematical modulo returning magnitude < | y | and sign of y
template< typename T >
inline
T
modulo( T const & x, T const & y )
{
	return ModuloSelector< T, std::numeric_limits< T >::is_integer >::modulo( x, y );
}

// Greatest Common Divisor
template< typename T >
inline
T
gcd( T const & m, T const & n )
{
	T lo( min( m, n ) );
	T hi( max( m, n ) );
	while ( lo > T( 0 ) ) {
		T const rem( mod( hi, lo ) );
		hi = lo;
		lo = rem;
	}
	return hi;
}

// Comparison-with-Tolerance Functions

// Equal within specified relative and absolute tolerances?
template< typename T >
inline
bool
eq_tol( T const & x, T const & y, T const & r_tol, T const & a_tol )
{
	using std::abs; // Can use std::abs or user-defined abs
	assert( r_tol >= T( 0 ) );
	assert( a_tol >= T( 0 ) );
	return ( abs( x - y ) <= min( r_tol * max( abs( x ), abs( y ) ), a_tol ) );
}

// Less than within specified relative and absolute tolerances?
template< typename T >
inline
bool
lt_tol( T const & x, T const & y, T const & r_tol, T const & a_tol )
{
	using std::abs; // Can use std::abs or user-defined abs
	assert( r_tol >= T( 0 ) );
	assert( a_tol >= T( 0 ) );
	return ( x < y + min( r_tol * max( abs( x ), abs( y ) ), a_tol ) );
}

// Less than or equal within specified relative and absolute tolerances?
template< typename T >
inline
bool
le_tol( T const & x, T const & y, T const & r_tol, T const & a_tol )
{
	using std::abs; // Can use std::abs or user-defined abs
	assert( r_tol >= T( 0 ) );
	assert( a_tol >= T( 0 ) );
	return ( x <= y + min( r_tol * max( abs( x ), abs( y ) ), a_tol ) );
}

// Greater than or equal within specified relative and absolute tolerances?
template< typename T >
inline
bool
ge_tol( T const & x, T const & y, T const & r_tol, T const & a_tol )
{
	using std::abs; // Can use std::abs or user-defined abs
	assert( r_tol >= T( 0 ) );
	assert( a_tol >= T( 0 ) );
	return ( x >= y - min( r_tol * max( abs( x ), abs( y ) ), a_tol ) );
}

// Greater than within specified relative and absolute tolerances?
template< typename T >
inline
bool
gt_tol( T const & x, T const & y, T const & r_tol, T const & a_tol )
{
	using std::abs; // Can use std::abs or user-defined abs
	assert( r_tol >= T( 0 ) );
	assert( a_tol >= T( 0 ) );
	return ( x > y - min( r_tol * max( abs( x ), abs( y ) ), a_tol ) );
}

} // ObjexxFCL

#endif // ObjexxFCL_Fmath_hh_INCLUDED
