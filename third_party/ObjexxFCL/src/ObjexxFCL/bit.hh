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

// Bitwise And
template< typename I, typename J >
inline
I
bit_and( I const & i, J const & j )
{
	return ( i & j );
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
