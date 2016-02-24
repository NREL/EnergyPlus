#ifndef ObjexxFCL_bit_hh_INCLUDED
#define ObjexxFCL_bit_hh_INCLUDED

// Bit Functions
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

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <type_traits>

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
Array1D< U >
TRANSFER( T const & v, Array< U > const & )
{
	Array1D< U > r( typename Array< U >::size_type( std::ceil( double( sizeof( T ) ) / sizeof( U ) ) ) );
	r.data_copy_from( &v, sizeof( T ) );
	return r;
}

// Array Reinterpreted as Another Type
template< typename T, typename U >
inline
U
TRANSFER( Array< T > const & a, U const & )
{
	return *reinterpret_cast< U const * >( a.data() );
}

// Array Reinterpreted as an Array of Another Type
template< typename T, typename U >
inline
Array1D< U >
TRANSFER( Array< T > const & a, Array< U > const & )
{
	Array1D< U > r( typename Array< U >::size_type( std::ceil( double( a.size() * sizeof( T ) ) / sizeof( U ) ) ) );
	r.data_copy_from( a.data(), a.size() * sizeof( T ) );
	return r;
}

// Value Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
Array1D< U >
TRANSFER( T const & v, U const &, I const size )
{
	return Array1D< U >( size, *reinterpret_cast< U const * >( &v ) );
}

// Value Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
Array1D< U >
TRANSFER( T const & v, Array< U > const &, I const size )
{
	return Array1D< U >( size, *reinterpret_cast< U const * >( &v ) );
}

// Array Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
Array1D< U >
TRANSFER( Array< T > const & a, U const &, I const size )
{
	Array1D< U > r( size );
	r.data_copy_from( a.data(), std::min( a.size() * sizeof( T ), size * sizeof( U ) ) );
	return r;
}

// Array Reinterpreted as an Array of Another Type
template< typename T, typename U, typename I >
inline
Array1D< U >
TRANSFER( Array< T > const & a, Array< U > const &, I const size )
{
	Array1D< U > r( size );
	r.data_copy_from( a.data(), std::min( a.size() * sizeof( T ), size * sizeof( U ) ) );
	return r;
}

namespace bit { // Protect from collisions with C++11 <functional> functors

// Bit Size
template< typename T, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
int
bit_size( T )
{
	return std::numeric_limits< T >::digits;
}

// Bit Size
template< typename T, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
int
bit_size( T )
{
	return std::numeric_limits< T >::digits + 1;
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
	return ( ( x & ( T( 1 ) << pos ) ) != T( 0 ) );
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
template< typename T, typename S, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
T
bit_shift( T const & x, S const & shift )
{
	auto const x_bits( std::numeric_limits< T >::digits );
	return ( shift >= S( 0 ) ? ( shift < x_bits ? x << shift : T( 0 ) ) : ( -shift < x_bits ? x >> -shift : T( 0 ) ) );
}

// Bit Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
T
bit_shift( T const & x, S const & shift ) // x<0 behavior varies in Fortran ISHFT: We do a logical shift like Intel Fortran and GFortran
{
	auto const x_bits( std::numeric_limits< T >::digits + 1 );
	auto const & u( *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) );
	return ( shift >= S( 0 ) ? ( shift < x_bits ? u << shift : T( 0 ) ) : ( -shift < x_bits ? u >> -shift : T( 0 ) ) );
}

// Bit Left Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
T
bit_lshift( T const & x, S const & shift )
{
	assert( shift >= S( 0 ) );
	auto const x_bits( std::numeric_limits< T >::digits );
	return ( shift < x_bits ? x << shift : T( 0 ) );
}

// Bit Left Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
T
bit_lshift( T const & x, S const & shift ) // x<0 behavior varies in Fortran LSHFT/LSHIFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( shift >= S( 0 ) );
	auto const x_bits( std::numeric_limits< T >::digits + 1 );
	return ( shift < x_bits ? *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) << shift : T( 0 ) );
}

// Bit Right Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
T
bit_rshift( T const & x, S const & shift )
{
	assert( shift >= S( 0 ) );
	auto const x_bits( std::numeric_limits< T >::digits );
	return ( shift < x_bits ? x >> shift : T( 0 ) );
}

// Bit Right Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
T
bit_rshift( T const & x, S const & shift ) // x<0 behavior varies in Fortran RSHFT/RSHIFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( shift >= S( 0 ) );
	auto const x_bits( std::numeric_limits< T >::digits + 1 );
	return ( shift < x_bits ? *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) >> shift : T( 0 ) );
}

// Bit Circularly Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
T
bit_cshift( T const & x, S const & shift )
{
	auto const x_bits( std::numeric_limits< T >::digits );
	if ( shift >= S( 0 ) ) {
		S const s( shift % x_bits );
		return ( s > S( 0 ) ? x << s | x >> ( x_bits - s ) : x );
	} else { // Negative (right) shift
		S const s( -shift % x_bits );
		return ( s > S( 0 ) ? x >> s | x << ( x_bits - s ) : x );
	}
}

// Bit Circularly Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
T
bit_cshift( T const & x, S const & shift ) // x<0 behavior varies in Fortran ISHFTC: We do a logical shift like Intel Fortran and GFortran
{
	auto const x_bits( std::numeric_limits< T >::digits + 1 );
	auto const & u( *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) );
	if ( shift >= S( 0 ) ) {
		S const s( shift % x_bits );
		return ( s > S( 0 ) ? u << s | u >> ( x_bits - s ) : x );
	} else { // Negative (right) shift
		S const s( -shift % x_bits );
		return ( s > S( 0 ) ? u >> s | u << ( x_bits - s ) : x );
	}
}

// Bit Arithmetic Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
T
bit_ashift( T const & x, S const & shift )
{
	auto const x_bits( std::numeric_limits< T >::digits );
	if ( shift >= S( 0 ) ) {
		return ( shift < x_bits ? x << shift : T( 0 ) );
	} else { // Negative (right) shift
		if ( x >= T( 0 ) ) {
			return ( -shift < x_bits ? x >> -shift : T( 0 ) );
		} else {
			return ( -shift < x_bits ? x >> -shift | ~( ( S( 1 ) << ( x_bits + shift ) ) - 1 ) : T( 0 ) );
		}
	}
}

// Bit Arithmetic Shifted
template< typename T, typename S, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
T
bit_ashift( T const & x, S const & shift )
{
	auto const x_bits( std::numeric_limits< T >::digits + 1 );
	if ( shift >= S( 0 ) ) {
		return ( shift < x_bits ? *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) << shift : T( 0 ) );
	} else { // Negative (right) shift
		if ( x >= T( 0 ) ) {
			return ( -shift < x_bits ? x >> -shift : T( 0 ) );
		} else {
			return ( -shift < x_bits ? x >> -shift | ~( ( S( 1 ) << ( x_bits + shift ) ) - 1 ) : T( 0 ) );
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
	static auto const x_bits( bit_size( x ) );
	if ( x >= T( 0 ) ) {
		return ( shift < x_bits ? x >> shift : T( 0 ) );
	} else {
		return ( shift < x_bits ? x >> shift | ~( ( S( 1 ) << ( bit_size( x ) - shift ) ) - 1 ) : T( 0 ) );
	}
}

// Bits Extracted
template< typename T, typename P, typename L, class = typename std::enable_if< std::is_unsigned< T >::value >::type >
inline
T
bit_bits( T const & x, P const & pos, L const & len )
{
	assert( pos >= T( 0 ) );
	assert( int( pos + len ) <= bit_size( x ) );
	T const siz( bit_size( x ) );
	return x << ( siz - ( pos + len ) ) >> ( siz - len );
}

// Bits Extracted
template< typename T, typename P, typename L, class = typename std::enable_if< std::is_signed< T >::value >::type, typename = void >
inline
T
bit_bits( T const & x, P const & pos, L const & len )
{
	assert( pos >= T( 0 ) );
	assert( int( pos + len ) <= bit_size( x ) );
	T const siz( bit_size( x ) );
	return *reinterpret_cast< typename std::make_unsigned< T const >::type * >( &x ) << ( siz - ( pos + len ) ) >> ( siz - len );
}

} // bit

} // ObjexxFCL

#endif // ObjexxFCL_bit_hh_INCLUDED
