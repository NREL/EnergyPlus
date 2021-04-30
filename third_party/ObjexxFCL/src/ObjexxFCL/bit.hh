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
// note that this is Undefined Behavior and it is being used
// in E+
template< typename I, typename U >
inline
U
bit_transfer( I const & v, U const & )
{
	return *reinterpret_cast< U const * >( &v );
}

} // bit

} // ObjexxFCL

#endif // ObjexxFCL_bit_hh_INCLUDED
