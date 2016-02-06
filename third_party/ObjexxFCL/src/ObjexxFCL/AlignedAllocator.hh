#ifndef ObjexxFCL_AlignedAllocator_hh_INCLUDED
#define ObjexxFCL_AlignedAllocator_hh_INCLUDED

// Aligned Allocator
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

// C++ Headers
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace ObjexxFCL {

// Aligned Allocator Class Template
template< typename T >
struct AlignedAllocator
{

public: // Types

	// STL Style
	using size_type = std::size_t;

	// C++ Style
	using Size = std::size_t;

public: // Static Methods

	// Allocate Raw Array Memory with ::operator new
	static
	void *
	allocate( size_type const n )
	{
#ifdef OBJEXXFCL_ALIGN
		void * mem( n > 0u ? ::operator new( ( n * sizeof( T ) ) + ( OBJEXXFCL_ALIGN - 1 ) ) : nullptr );
#else
		void * mem( n > 0u ? ::operator new( n * sizeof( T ) ) : nullptr );
#endif
		assert( ( n == 0u ) || ( mem != nullptr ) );
		return mem;
	}

	// Allocate Raw Array Memory with ::operator new Even if Size is Zero
	static
	void *
	allocate_zero( size_type const n )
	{
#ifdef OBJEXXFCL_ALIGN
		void * mem( ::operator new( ( n * sizeof( T ) ) + ( OBJEXXFCL_ALIGN - 1 ) ) );
#else
		void * mem( ::operator new( n * sizeof( T ) ) );
#endif
		assert( ( n == 0u ) || ( mem != nullptr ) );
		return mem;
	}

	// Aligned Data Pointer for a Given Memory Pointer
	static
	T *
	data( void * const mem )
	{
#ifdef OBJEXXFCL_ALIGN
		T * p( static_cast< T * >( ( mem == nullptr ) || ( OBJEXXFCL_ALIGN == 0u ) ? mem : reinterpret_cast< void * >( ( reinterpret_cast< std::uintptr_t >( mem ) + static_cast< std::uintptr_t >( OBJEXXFCL_ALIGN - 1 ) ) & ~static_cast< std::uintptr_t >( OBJEXXFCL_ALIGN - 1 ) ) ) );
		assert( p >= mem );
		assert( reinterpret_cast< std::uintptr_t >( p ) % OBJEXXFCL_ALIGN == 0 );
#else
		T * p( static_cast< T * >( mem ) );
#endif
		return p;
	}

}; // AlignedAllocator

} // ObjexxFCL

#endif // ObjexxFCL_AlignedAllocator_hh_INCLUDED
