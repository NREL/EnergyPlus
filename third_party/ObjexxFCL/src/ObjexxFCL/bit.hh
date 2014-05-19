#ifndef ObjexxFCL_bit_hh_INCLUDED
#define ObjexxFCL_bit_hh_INCLUDED

// Bit Functions
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/FArray1D.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <type_traits>
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
#include <bitset>
#include <limits>
#endif

namespace ObjexxFCL {

// Value Reinterpreted as Another Type
template< typename T, typename U >
inline
U
TRANSFER( T const & v, U const & )
{
	return *reinterpret_cast< U const * >( &v );
}

// Value Reinterpreted as an Array of Another Type
template< typename T, typename U >
inline
FArray1D< U >
TRANSFER( T const & v, FArray< U > const & )
{
	FArray1D< U > r( typename FArray< U >::size_type( std::ceil( double( sizeof( T ) ) / sizeof( U ) ) ) );
	r.data_copy_from( &v, sizeof( T ) );
	return r;
}

// Array Reinterpreted as Another Type
template< typename T, typename U >
inline
U
TRANSFER( FArray< T > const & a, U const & )
{
	return *reinterpret_cast< U const * >( a.data() );
}

// Array Reinterpreted as an Array of Another Type
template< typename T, typename U >
inline
FArray1D< U >
TRANSFER( FArray< T > const & a, FArray< U > const & )
{
	FArray1D< U > r( typename FArray< U >::size_type( std::ceil( double( a.size() * sizeof( T ) ) / sizeof( U ) ) ) );
	r.data_copy_from( a.data(), a.size() * sizeof( T ) );
	return r;
}

// Value Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
FArray1D< U >
TRANSFER( T const & v, U const &, I const size )
{
	return FArray1D< U >( size, *reinterpret_cast< U const * >( &v ) );
}

// Value Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
FArray1D< U >
TRANSFER( T const & v, FArray< U > const &, I const size )
{
	return FArray1D< U >( size, *reinterpret_cast< U const * >( &v ) );
}

// Array Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
FArray1D< U >
TRANSFER( FArray< T > const & a, U const &, I const size )
{
	FArray1D< U > r( size );
	r.data_copy_from( a.data(), std::min( a.size() * sizeof( T ), size * sizeof( U ) ) );
	return r;
}

// Array Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
FArray1D< U >
TRANSFER( FArray< T > const & a, FArray< U > const &, I const size )
{
	FArray1D< U > r( size );
	r.data_copy_from( a.data(), std::min( a.size() * sizeof( T ), size * sizeof( U ) ) );
	return r;
}

namespace bit { // Protect from collisions with C++11 <functional> functors

// Bit Size
template< typename T >
inline
int
bit_size( T )
{
	return 8 * sizeof( T ); // Assumes 8-bit byte architecture
}

// Bit Value Set to 1
template< typename T >
inline
T
bit_set( T const & x, T const & pos )
{
	assert( pos >= T( 0 ) );
	return ( x | ( T( 1 ) << pos ) );
}

// Bit Value Set to 0
template< typename T >
inline
T
bit_clr( T const & x, T const & pos )
{
	assert( pos >= T( 0 ) );
	return ( x & ~( T( 1 ) << pos ) );
}

// Bit Value Test
template< typename T >
inline
bool
bit_test( T const & x, T const & pos )
{
	assert( pos >= T( 0 ) );
	return ( x & ( T( 1 ) << pos ) );
}

// Bitwise Not
template< typename T >
inline
T
bit_not( T const & x )
{
	return ( ~x );
}

// Bitwise And
template< typename T >
inline
T
bit_and( T const & x, T const & y )
{
	return ( x & y );
}

// Bitwise Inclusive Or
template< typename T >
inline
T
bit_or( T const & x, T const & y )
{
	return ( x | y );
}

// Bitwise Exclusive Or
template< typename T >
inline
T
bit_xor( T const & x, T const & y )
{
	return ( x ^ y );
}

// Bit Shifted
template< typename T, typename S >
inline
T
bit_shift( T const & x, S const & shift, typename std::enable_if< std::is_unsigned< T >::value >::type * = 0 )
{
	assert( std::abs( shift ) <= bit_size( x ) );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	std::bitset< std::numeric_limits< T >::digits > b( x );
	return ( shift >= S( 0 ) ? b <<= shift : b >>= -shift ).to_ullong();
#else
	return ( shift >= S( 0 ) ? x << shift : x >> -shift );
#endif
}

// Bit Shifted
template< typename T, typename S >
inline
T
bit_shift( T const & x, S const & shift, typename std::enable_if< std::is_signed< T >::value >::type * = 0 ) // x<0 behavior varies in Fortran ISHFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( std::abs( shift ) <= bit_size( x ) );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	std::bitset< std::numeric_limits< T >::digits + 1 > b( x );
	return ( shift >= S( 0 ) ? b <<= shift : b >>= -shift ).to_ullong();
#else
	auto const & u( *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) );
	return ( shift >= S( 0 ) ? u << shift : u >> -shift );
#endif
}

// Bit Left Shifted
template< typename T, typename S >
inline
T
bit_lshift( T const & x, S const & shift, typename std::enable_if< std::is_unsigned< T >::value >::type * = 0 )
{
	assert( shift >= S( 0 ) );
	assert( shift <= bit_size( x ) );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	std::bitset< std::numeric_limits< T >::digits > b( x );
	return ( b <<= shift ).to_ullong();
#else
	return x << shift;
#endif
}

// Bit Left Shifted
template< typename T, typename S >
inline
T
bit_lshift( T const & x, S const & shift, typename std::enable_if< std::is_signed< T >::value >::type * = 0 ) // x<0 behavior varies in Fortran LSHFT/LSHIFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( shift >= S( 0 ) );
	assert( shift <= bit_size( x ) );
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around
	std::bitset< std::numeric_limits< T >::digits + 1 > b( x );
	return ( b <<= shift ).to_ullong();
#else
	return *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) << shift;
#endif
}

// Bit Right Shifted
template< typename T, typename S >
inline
T
bit_rshift( T const & x, S const & shift, typename std::enable_if< std::is_unsigned< T >::value >::type * = 0 )
{
	assert( shift >= S( 0 ) );
	assert( shift <= bit_size( x ) );
	return x >> shift;
}

// Bit Right Shifted
template< typename T, typename S >
inline
T
bit_rshift( T const & x, S const & shift, typename std::enable_if< std::is_signed< T >::value >::type * = 0 ) // x<0 behavior varies in Fortran RSHFT/RSHIFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( shift >= S( 0 ) );
	assert( shift <= bit_size( x ) );
	return *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) >> shift;
}

// Bit Circularly Shifted
template< typename T, typename S >
inline
T
bit_cshift( T const & x, S const & shift, typename std::enable_if< std::is_unsigned< T >::value >::type * = 0 )
{
	assert( std::abs( shift ) <= bit_size( x ) );
#if (defined(__clang__) && defined(__APPLE__)) || (defined(_MSC_VER) && !defined(__INTEL_COMPILER)) // VC++2013 bug work-around
	std::bitset< std::numeric_limits< T >::digits > const b( x );
	return ( shift >= S( 0 ) ? b << shift | b >> ( bit_size( x ) - shift ) : b >> -shift | b << ( bit_size( x ) + shift ) ).to_ullong();
#else
	return ( shift >= S( 0 ) ? x << shift | x >> ( bit_size( x ) - shift ) : x >> -shift | x << ( bit_size( x ) + shift ) );
#endif
}

// Bit Circularly Shifted
template< typename T, typename S >
inline
T
bit_cshift( T const & x, S const & shift, typename std::enable_if< std::is_signed< T >::value >::type * = 0 ) // x<0 behavior varies in Fortran ISHFTC: We do a logical shift like Intel Fortran and GFortran
{
	assert( std::abs( shift ) <= bit_size( x ) );
#if (defined(__clang__) && defined(__APPLE__)) || (defined(_MSC_VER) && !defined(__INTEL_COMPILER)) // VC++2013 bug work-around
	std::bitset< std::numeric_limits< T >::digits + 1 > const b( x );
	return ( shift >= S( 0 ) ? b << shift | b >> ( bit_size( x ) - shift ) : b >> -shift | b << ( bit_size( x ) + shift ) ).to_ullong();
#else
	auto const & u( *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) );
	return ( shift >= S( 0 ) ? u << shift | u >> ( bit_size( x ) - shift ) : u >> -shift | u << ( bit_size( x ) + shift ) );
#endif
}

// Bit Arithmetic Shifted
template< typename T, typename S >
inline
T
bit_ashift( T const & x, S const & shift )
{
	assert( std::abs( shift ) <= bit_size( x ) );
	if ( shift >= S( 0 ) ) {
		return x << shift;
	} else { // Negative (right) shift
		if ( x >= T( 0 ) ) {
			return x >> -shift;
		} else {
			return x >> -shift | ~( ( S( 1 ) << ( bit_size( x ) + shift ) ) - 1 );
		}
	}
}

// Bit Right Arithmetic Shifted
template< typename T, typename S >
inline
T
bit_arshift( T const & x, S const & shift )
{
	assert( shift >= S( 0 ) );
	assert( shift <= bit_size( x ) );
	if ( x >= T( 0 ) ) {
		return x >> shift;
	} else {
		return x >> shift | ~( ( S( 1 ) << ( bit_size( x ) - shift ) ) - 1 );
	}
}

// Bits Extracted
template< typename T, typename P, typename L >
inline
T
bit_bits( T const & x, P const & pos, L const & len, typename std::enable_if< std::is_unsigned< T >::value >::type * = 0 )
{
	assert( pos >= T( 0 ) );
	assert( int( pos + len ) <= bit_size( x ) );
	T const siz( bit_size( x ) );
	return x << ( siz - ( pos + len ) ) >> ( siz - len );
}

// Bits Extracted
template< typename T, typename P, typename L >
inline
T
bit_bits( T const & x, P const & pos, L const & len, typename std::enable_if< std::is_signed< T >::value >::type * = 0 )
{
	assert( pos >= T( 0 ) );
	assert( int( pos + len ) <= bit_size( x ) );
	T const siz( bit_size( x ) );
	return *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) << ( siz - ( pos + len ) ) >> ( siz - len );
}

} // bit

} // ObjexxFCL

#endif // ObjexxFCL_bit_hh_INCLUDED
