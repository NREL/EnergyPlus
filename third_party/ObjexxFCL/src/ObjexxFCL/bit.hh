#ifndef ObjexxFCL_bit_hh_INCLUDED
#define ObjexxFCL_bit_hh_INCLUDED

// Bit Functions
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
#include <ObjexxFCL/Array1D.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <type_traits>

namespace ObjexxFCL {

namespace bit { // Protect from collisions with C++11 <functional> functors

// Bit Size
template< typename I, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
int
bit_size( I )
{
	return std::numeric_limits< I >::digits;
}

// Bit Size
template< typename I, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
int
bit_size( I )
{
	return std::numeric_limits< I >::digits + 1;
}

// Bit Number of Leading Zeros
template< typename I >
inline
int
bit_leading_zeros( I i )
{
	int const i_bits( bit_size( i ) );
	if ( i == I( 0 ) ) return i_bits;
	if ( i_bits == 0 ) return 0;
	int n( 0 );
	while ( ( i & ( 1 << ( i_bits - 1 - n ) ) ) == I( 0 ) ) n += 1;
	return n;
}

// Bit Number of Trailing Zeros
template< typename I >
inline
int
bit_trailing_zeros( I const & i )
{
	int const i_bits( bit_size( i ) );
	if ( i == I( 0 ) ) return i_bits;
	if ( i_bits == 0 ) return 0;
	int n( 0 );
	while ( i % ( 2 << n ) == 0 ) n += 1;
	return n;
}

// Bit Number of Ones
template< typename I >
inline
int
bit_ones( I const & i )
{
	if ( i == I( 0 ) ) return 0;
	int const i_bits( bit_size( i ) );
	if ( i_bits == 0 ) return 0;
	int n( 0 );
	for ( int k = 0; k < i_bits; ++k ) n += ( i >> k ) & 1;
	return n;
}

// Bit Parity of Ones
template< typename I >
inline
int
bit_parity( I const & i )
{
	if ( i == I( 0 ) ) return 0;
	int const i_bits( bit_size( i ) );
	if ( i_bits == 0 ) return 0;
	int n( 0 );
	for ( int k = 0; k < i_bits; ++k ) n += ( i >> k ) & 1;
	return n % 2;
}

// Bit Mask with Leftmost n Bits set to One
template< typename I = int >
inline
I
bit_maskl( I const n )
{
	assert( n >= I( 0 ) );
	if ( n == I( 0 ) ) return I( 0 );
	int const n_bits( bit_size( n ) );
	if ( n_bits == 0 ) return 0;
	I r( 0 );
	for ( int k = n_bits - n; k < n_bits; ++k ) r |= ( 1 << k );
	return r;
}

// Bit Mask with Rightmost n Bits set to One
template< typename I = int >
inline
I
bit_maskr( I const n )
{
	assert( n >= I( 0 ) );
	if ( n == I( 0 ) ) return I( 0 );
	int const n_bits( bit_size( n ) );
	if ( n_bits == 0 ) return 0;
	I r( 0 );
	for ( int k = 0; k < n; ++k ) r |= ( 1 << k );
	return r;
}

// Bit Value Test
template< typename I >
inline
bool
bit_test( I const & i, I const & pos )
{
	assert( pos >= I( 0 ) );
	return ( ( i & ( I( 1 ) << pos ) ) != I( 0 ) );
}

// sign( i )
template< typename I >
inline
int
_bit_sign( I const & i )
{
	return ( i >= I( 0 ) ? +1 : -1 );
}

// Bit Less Than
template< typename I, typename J >
inline
bool
bit_lt( I const & i, J const & j )
{
	if ( _bit_sign( i ) == _bit_sign( j ) ) {
		return i < j;
	} else {
		return i >= j;
	}
}

// Bit Less Than or Equal
template< typename I, typename J >
inline
bool
bit_le( I const & i, J const & j )
{
	if ( _bit_sign( i ) == _bit_sign( j ) ) {
		return i <= j;
	} else {
		return i > j;
	}
}

// Bit Greater Than or Equal
template< typename I, typename J >
inline
bool
bit_ge( I const & i, J const & j )
{
	if ( _bit_sign( i ) == _bit_sign( j ) ) {
		return i >= j;
	} else {
		return i < j;
	}
}

// Bit Greater Than
template< typename I, typename J >
inline
bool
bit_gt( I const & i, J const & j )
{
	if ( _bit_sign( i ) == _bit_sign( j ) ) {
		return i > j;
	} else {
		return i <= j;
	}
}

// Bit Value Set to 1
template< typename I >
inline
I
bit_set( I const & i, I const & pos )
{
	assert( pos >= I( 0 ) );
	return ( i | ( I( 1 ) << pos ) );
}

// Bit Value Set to 0
template< typename I >
inline
I
bit_clr( I const & i, I const & pos )
{
	assert( pos >= I( 0 ) );
	return ( i & ~( I( 1 ) << pos ) );
}

// Bitwise Not
template< typename I >
inline
I
bit_not( I const & i )
{
	return ( ~i );
}

// Bitwise And
template< typename I, typename J >
inline
I
bit_and( I const & i, J const & j )
{
	return ( i & j );
}

// Bitwise Inclusive Or
template< typename I, typename J >
inline
I
bit_or( I const & i, J const & j )
{
	return ( i | j );
}

// Bitwise Exclusive Or
template< typename I, typename J >
inline
I
bit_xor( I const & i, J const & j )
{
	return ( i ^ j );
}

// Bit Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
I
bit_shift( I const & i, S const & shift )
{
	auto const i_bits( std::numeric_limits< I >::digits );
	return ( shift >= S( 0 ) ? ( shift < i_bits ? i << shift : I( 0 ) ) : ( -shift < i_bits ? i >> -shift : I( 0 ) ) );
}

// Bit Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
I
bit_shift( I const & i, S const & shift ) // i<0 behavior varies in Fortran ISHFT: We do a logical shift like Intel Fortran and GFortran
{
	auto const i_bits( std::numeric_limits< I >::digits + 1 );
	auto const & u( *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) );
	return ( shift >= S( 0 ) ? ( shift < i_bits ? u << shift : I( 0 ) ) : ( -shift < i_bits ? u >> -shift : I( 0 ) ) );
}

// Bit Left Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
I
bit_shiftl( I const & i, S const & shift )
{
	assert( shift >= S( 0 ) );
	auto const i_bits( std::numeric_limits< I >::digits );
	return ( shift < i_bits ? i << shift : I( 0 ) );
}

// Bit Left Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
I
bit_shiftl( I const & i, S const & shift ) // i<0 behavior varies in Fortran LSHFT/LSHIFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( shift >= S( 0 ) );
	auto const i_bits( std::numeric_limits< I >::digits + 1 );
	return ( shift < i_bits ? *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) << shift : I( 0 ) );
}

// Bit Right Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
I
bit_shiftr( I const & i, S const & shift )
{
	assert( shift >= S( 0 ) );
	auto const i_bits( std::numeric_limits< I >::digits );
	return ( shift < i_bits ? i >> shift : I( 0 ) );
}

// Bit Right Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
I
bit_shiftr( I const & i, S const & shift ) // i<0 behavior varies in Fortran RSHFT/RSHIFT: We do a logical shift like Intel Fortran and GFortran
{
	assert( shift >= S( 0 ) );
	auto const i_bits( std::numeric_limits< I >::digits + 1 );
	return ( shift < i_bits ? *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) >> shift : I( 0 ) );
}

// Bit Circularly Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
I
bit_cshift( I const & i, S const & shift )
{
	auto const i_bits( std::numeric_limits< I >::digits );
	if ( shift >= S( 0 ) ) {
		S const s( shift % i_bits );
		return ( s > S( 0 ) ? i << s | i >> ( i_bits - s ) : i );
	} else { // Negative (right) shift
		S const s( -shift % i_bits );
		return ( s > S( 0 ) ? i >> s | i << ( i_bits - s ) : i );
	}
}

// Bit Circularly Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
I
bit_cshift( I const & i, S const & shift ) // i<0 behavior varies in Fortran ISHFTC: We do a logical shift like Intel Fortran and GFortran
{
	auto const i_bits( std::numeric_limits< I >::digits + 1 );
	auto const & u( *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) );
	if ( shift >= S( 0 ) ) {
		S const s( shift % i_bits );
		return ( s > S( 0 ) ? u << s | u >> ( i_bits - s ) : i );
	} else { // Negative (right) shift
		S const s( -shift % i_bits );
		return ( s > S( 0 ) ? u >> s | u << ( i_bits - s ) : i );
	}
}

// Bit Arithmetic Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
I
bit_ashift( I const & i, S const & shift )
{
	auto const i_bits( std::numeric_limits< I >::digits );
	if ( shift >= S( 0 ) ) {
		return ( shift < i_bits ? i << shift : I( 0 ) );
	} else { // Negative (right) shift
		if ( i >= I( 0 ) ) {
			return ( -shift < i_bits ? i >> -shift : I( 0 ) );
		} else {
			return ( -shift < i_bits ? i >> -shift | ~( ( S( 1 ) << ( i_bits + shift ) ) - 1 ) : I( 0 ) );
		}
	}
}

// Bit Arithmetic Shifted
template< typename I, typename S, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
I
bit_ashift( I const & i, S const & shift )
{
	auto const i_bits( std::numeric_limits< I >::digits + 1 );
	if ( shift >= S( 0 ) ) {
		return ( shift < i_bits ? *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) << shift : I( 0 ) );
	} else { // Negative (right) shift
		if ( i >= I( 0 ) ) {
			return ( -shift < i_bits ? i >> -shift : I( 0 ) );
		} else {
			return ( -shift < i_bits ? i >> -shift | ~( ( S( 1 ) << ( i_bits + shift ) ) - 1 ) : I( 0 ) );
		}
	}
}

// Bit Left Arithmetic Shifted
template< typename I, typename S >
inline
I
bit_ashiftl( I const & i, S const & shift )
{
	assert( shift >= S( 0 ) );
	auto const i_bits( bit_size( i ) );
	return ( shift < i_bits ? *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) << shift : I( 0 ) );
}

// Bit Right Arithmetic Shifted
template< typename I, typename S >
inline
I
bit_ashiftr( I const & i, S const & shift )
{
	assert( shift >= S( 0 ) );
	auto const i_bits( bit_size( i ) );
	if ( i >= I( 0 ) ) {
		return ( shift < i_bits ? i >> shift : I( 0 ) );
	} else {
		return ( shift < i_bits ? i >> shift | ~( ( S( 1 ) << ( i_bits - shift ) ) - 1 ) : I( 0 ) );
	}
}

// Bit Combined Left Shifted
template< typename I, typename J, typename S >
inline
I
bit_dshiftl( I const & i, J const & j, S const & shift )
{
	assert( shift >= S( 0 ) );
	if ( shift == S( 0 ) ) return i;
	return bit_or( bit_shiftl( i, shift ), bit_shiftr( j, bit_size( j ) - shift ) );
}

// Bit Combined Right Shifted
template< typename I, typename J, typename S >
inline
I
bit_dshiftr( I const & i, J const & j, S const & shift )
{
	assert( shift >= S( 0 ) );
	if ( shift == S( 0 ) ) return I( j );
	auto const i_bits( bit_size( i ) );
	I const ones( -1 );
	return bit_or( ones & bit_shiftl( i, i_bits - shift ), bit_shiftr( j, shift ) );
}

// Bits Extracted
template< typename I, typename P, typename L, class = typename std::enable_if< std::is_unsigned< I >::value >::type >
inline
I
bit_bits( I const & i, P const & pos, L const & len )
{
	assert( pos >= I( 0 ) );
	auto const i_bits( bit_size( i ) );
	assert( int( pos + len ) <= i_bits );
	return i << ( i_bits - ( pos + len ) ) >> ( i_bits - len );
}

// Bits Extracted
template< typename I, typename P, typename L, class = typename std::enable_if< std::is_signed< I >::value >::type, typename = void >
inline
I
bit_bits( I const & i, P const & pos, L const & len )
{
	assert( pos >= I( 0 ) );
	assert( len >= L( 0 ) );
	auto const i_bits( bit_size( i ) );
	assert( int( pos + len ) <= i_bits );
	return *reinterpret_cast< typename std::make_unsigned< I const >::type * >( &i ) << ( i_bits - ( pos + len ) ) >> ( i_bits - len );
}

// Bit Merge
template< typename I, typename J, typename M >
inline
I
bit_merge( I const & i, J const & j, M const & mask )
{
	return bit_or( bit_and( i, mask ), bit_and( j, ~mask ) );
}

// Bit Move
template< typename I, typename P, typename L >
inline
I
bit_move( I const & from, P const & frompos, L const & len, I to, P const & topos )
{
	assert( frompos >= 0 );
	assert( len >= 0 );
	assert( topos >= 0 );
	assert( topos + len <= bit_size( to ) );
	if ( from == I( 0 ) ) return to;
	auto const from_bits( bit_size( from ) );
	if ( from_bits == 0 ) return I( 0 );
	auto m( topos );
	for ( P k = frompos; k < frompos + len; ++k ) {
		I const mask( 1u << m );
		to &= ~mask; // Zero the bit
		if ( ( from >> k ) & 1 ) to |= mask; // Set the bit
		m += 1;
	}
	return to;
}

// Value Reinterpreted as Another Type
template< typename I, typename U >
inline
U
bit_transfer( I const & v, U const & )
{
	return *reinterpret_cast< U const * >( &v );
}

// Value Reinterpreted as an Array of Another Type
template< typename I, typename U >
inline
Array1D< U >
bit_transfer( I const & v, Array< U > const & )
{
	Array1D< U > r( BArray::size_type( std::ceil( double( sizeof( I ) ) / sizeof( U ) ) ) );
	r.data_copy_from( &v, sizeof( I ) );
	return r;
}

// Array Reinterpreted as Another Type
template< typename I, typename U >
inline
U
bit_transfer( Array< I > const & a, U const & )
{
	return *reinterpret_cast< U const * >( a.data() );
}

// Array Reinterpreted as an Array of Another Type
template< typename I, typename U >
inline
Array1D< U >
bit_transfer( Array< I > const & a, Array< U > const & )
{
	Array1D< U > r( BArray::size_type( std::ceil( double( a.size() * sizeof( I ) ) / sizeof( U ) ) ) );
	r.data_copy_from( a.data(), a.size() * sizeof( I ) );
	return r;
}

// Value Reinterpreted as an Array of Another Type
template< typename I, typename U, typename S >
inline
Array1D< U >
bit_transfer( I const & v, U const &, S const size )
{
	return Array1D< U >( size, *reinterpret_cast< U const * >( &v ) );
}

// Value Reinterpreted as an Array of Another Type
template< typename I, typename U, typename S >
inline
Array1D< U >
bit_transfer( I const & v, Array< U > const &, S const size )
{
	return Array1D< U >( size, *reinterpret_cast< U const * >( &v ) );
}

// Array Reinterpreted as an Array of Another Type
template< typename I, typename U, typename S >
inline
Array1D< U >
bit_transfer( Array< I > const & a, U const &, S const size )
{
	Array1D< U > r( size );
	r.data_copy_from( a.data(), std::min( a.size() * sizeof( I ), size * sizeof( U ) ) );
	return r;
}

// Array Reinterpreted as an Array of Another Type
template< typename I, typename U, typename S >
inline
Array1D< U >
bit_transfer( Array< I > const & a, Array< U > const &, S const size )
{
	Array1D< U > r( size );
	r.data_copy_from( a.data(), std::min( a.size() * sizeof( I ), size * sizeof( U ) ) );
	return r;
}

} // bit

} // ObjexxFCL

#endif // ObjexxFCL_bit_hh_INCLUDED
