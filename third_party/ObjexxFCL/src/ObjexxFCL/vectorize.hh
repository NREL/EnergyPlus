#ifndef ObjexxFCL_vectorize_hh_INCLUDED
#define ObjexxFCL_vectorize_hh_INCLUDED

// Vectorization Support
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cassert>

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#if ( defined(__GNUC__) && ( ( __GNUC__ > 4 ) || ( ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ >= 7 ) ) ) ) || ( defined(__clang__) && __has_builtin(__builtin_assume_aligned) )
#define ASSUME(b)
#define ASSUME_ALIGNED(p,b) p=(decltype(p))__builtin_assume_aligned(p,b)
#define RESTRICT __restrict__
#elif defined(_WIN32) && ( defined(_MSC_VER) || defined(__INTEL_COMPILER) )
#define ASSUME(b) __assume(b)
#define ASSUME_ALIGNED(p,b) __assume_aligned(p,b)
#define RESTRICT __restrict
#else
#define ASSUME(b)
#define ASSUME_ALIGNED(p,b)
#define RESTRICT
#endif

namespace ObjexxFCL {

// Round up to Multiple of 2
template< typename T >
T
round_up_even( T const t )
{
	static T const one( 1 );
	return ( ( t + one ) >> one ) << one;
}

// Round up to Satisfy Alignment
template< typename T >
T
round_up_aligned( T const t, T const alignment )
{
	static T const zero( 0 );
	static T const one( 1 );
	assert( ( alignment > zero ) && ( ( alignment & ( alignment - one ) ) == zero ) );
	return ( ( t == zero ) || ( alignment <= zero ) ? t : ( t + ( alignment - one ) ) & ~( alignment - one ) );
}

// Round up to Power of 2
template< typename T >
T
round_up_pow_2( T const t )
{
	static T const one( 1 );
	T power( 1 );
	while ( power < t ) power <<= one;
	return power;
}

} // ObjexxFCL

#endif // ObjexxFCL_vectorize_hh_INCLUDED
