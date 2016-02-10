#ifndef ObjexxFCL_vectorize_hh_INCLUDED
#define ObjexxFCL_vectorize_hh_INCLUDED

// Vectorization Support
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
#include <ObjexxFCL/align.hh>

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#if ( defined(__GNUC__) && ( ( __GNUC__ > 4 ) || ( ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ >= 7 ) ) ) ) || ( defined(__clang__) && __has_builtin(__builtin_assume_aligned) )
#define ASSUME(b)
#define ASSUME_ALIGNED(p,b) p=(decltype(p))__builtin_assume_aligned(p,b)
#define ASSUME_ALIGNED_OBJEXXFCL(p) p=(decltype(p))__builtin_assume_aligned(p,OBJEXXFCL_ALIGN)
#define RESTRICT __restrict__

#elif defined(_WIN64) && ( defined(_MSC_VER) || defined(__INTEL_COMPILER) )
#define ASSUME(b) __assume(b)
#define ASSUME_ALIGNED(p,b) __assume_aligned(p,b)
#define ASSUME_ALIGNED_OBJEXXFCL(p) __assume_aligned(p,OBJEXXFCL_ALIGN)
#define RESTRICT __restrict

// Disable alignment hints for 32-bit windows builds
#elif defined(_WIN32) && ( defined(_MSC_VER) || defined(__INTEL_COMPILER) )
#define ASSUME(b) __assume(b)
#define ASSUME_ALIGNED(p,b) 
#define RESTRICT __restrict

#else
#define ASSUME(b)
#define ASSUME_ALIGNED(p,b)
#define ASSUME_ALIGNED_OBJEXXFCL(p,b)
#define RESTRICT
#endif

// Alignment for Compiler Options
#if defined (__AVX2__)
int const VEC_ALIGN( 64 ); // size of __m512* types
#elif defined (__AVX__)
int const VEC_ALIGN( 32 ); // size of __m256* types
#else
int const VEC_ALIGN( 16 ); // size of __m128* types
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

// Byte Address Rounded Up to Satisfy Alignment
template< typename T >
T *
aligned( T * const mem ) // Can add a variant taking a void * if needed
{
	static T const ALIGN( VEC_ALIGN );
	static T const zero( 0 );
	static T const one( 1 );
	static_assert( ( ALIGN > zero ) && ( ( ALIGN & ( ALIGN - one ) ) == zero ), "Byte alignment must be a positive power of 2" ); // Positive power of 2 check
	return ( ( mem == nullptr ) || ( ALIGN <= T( 0 ) ) ? mem : reinterpret_cast< T * >( ( reinterpret_cast< std::uintptr_t >( mem ) + static_cast< std::uintptr_t >( ALIGN - 1 ) ) & ~static_cast< std::uintptr_t >( ALIGN - 1 ) ) );
}

// Can Row Length be Padded to Satisfy Alignment?
template< typename T >
bool
paddable() // Call as paddable< Type >()
{
	static int const REM( VEC_ALIGN % static_cast< int >( sizeof( T ) ) );
	return REM == 0;
}

// Row Length Rounded up to Satisfy Alignment
template< typename T >
T
padded( int const row ) // Call as padded< Type >( row )
{
	static_assert( VEC_ALIGN % static_cast< int >( sizeof( T ) ) == 0, "Type is not row-paddable for the byte alignment" ); // Paddable check
	assert( row >= 0 );
	static int const MUL( VEC_ALIGN / static_cast< int >( sizeof( T ) ) ); // Aligned multiple of T
	int const rem( row % MUL );
	return ( rem > 0 ? row + MUL - rem : row );
}

} // ObjexxFCL

#endif // ObjexxFCL_vectorize_hh_INCLUDED
