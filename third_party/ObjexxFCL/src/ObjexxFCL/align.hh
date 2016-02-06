#ifndef ObjexxFCL_align_hh_INCLUDED
#define ObjexxFCL_align_hh_INCLUDED

// Alignment Support
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
#include <cassert>
#include <cstddef>
#include <cstdint>

// Auto-Selected Alignment
#if defined(__MIC__)
#define OBJEXXFCL_ALIGN_AUTO 64
#elif defined(__AVX__)
#define OBJEXXFCL_ALIGN_AUTO 32
#elif defined(__SSE__)
#define OBJEXXFCL_ALIGN_AUTO 16
#else
#define OBJEXXFCL_ALIGN_AUTO 16
#endif

// Specified Alignment
#ifdef OBJEXXFCL_ALIGN
#if OBJEXXFCL_ALIGN < OBJEXXFCL_ALIGN_AUTO
#undef OBJEXXFCL_ALIGN
#define OBJEXXFCL_ALIGN OBJEXXFCL_ALIGN_AUTO
#endif
static_assert( ( OBJEXXFCL_ALIGN & ( OBJEXXFCL_ALIGN - 1 ) ) == 0, "OBJEXXFCL_ALIGN must be a power of 2" );
#endif

#ifdef OBJEXXFCL_ALIGN

namespace ObjexxFCL {

// Byte Address Rounded Up to Satisfy Alignment
template< typename T >
T *
aligned( T * const mem ) // Can add a variant taking a void * if needed
{
	static T const ALIGN( OBJEXXFCL_ALIGN );
	static T const zero( 0 );
	static T const one( 1 );
	static_assert( ( ALIGN > zero ) && ( ( ALIGN & ( ALIGN - one ) ) == zero ), "Byte alignment must be a positive power of 2" ); // Positive power of 2 check
	return ( ( mem == nullptr ) || ( ALIGN <= T( 0 ) ) ? mem : reinterpret_cast< T * >( ( reinterpret_cast< std::uintptr_t >( mem ) + static_cast< std::uintptr_t >( ALIGN - 1 ) ) & ~static_cast< std::uintptr_t >( ALIGN - 1 ) ) );
}

// Can Row Length be Padded to Satisfy Alignment?
template< typename T >
bool
paddable()
{
	static int const REM( OBJEXXFCL_ALIGN % static_cast< int >( sizeof( T ) ) );
	return REM == 0;
}

// Row Length Rounded up to Satisfy Alignment
template< typename T >
T
padded( int const row )
{
	static_assert( OBJEXXFCL_ALIGN % static_cast< int >( sizeof( T ) ) == 0, "Type is not row-paddable for the byte alignment" ); // Paddable check
	assert( row >= 0 );
	static int const MUL( OBJEXXFCL_ALIGN / static_cast< int >( sizeof( T ) ) ); // Aligned multiple of T
	int const rem( row % MUL );
	return ( rem > 0 ? row + MUL - rem : row );
}

} // ObjexxFCL

#endif

#endif // ObjexxFCL_align_hh_INCLUDED
