#ifndef ObjexxFCL_ArrayTail_hh_INCLUDED
#define ObjexxFCL_ArrayTail_hh_INCLUDED

// Contiguous Array Tail Proxy
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/ArrayTail.fwd.hh>

namespace ObjexxFCL {

// Forward
template< typename > class Array;

// Contiguous Array Tail Proxy
template< typename T >
class ArrayTail
{

private: // Friend

	friend class Array< T >;

public: // Types

	// STL style
	using value_type = T;
	using reference = T &;
	using const_reference = T const &;
	using pointer = T *;
	using const_pointer = T const *;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	// C++ style
	using Value = T;
	using Reference = T &;
	using ConstReference = T const &;
	using Pointer = T *;
	using ConstPointer = T const *;
	using Size = std::size_t;
	using Difference = std::ptrdiff_t;

public: // Creation

	// Pointer + Size Constructor
	ArrayTail( T const * a, size_type const size ) :
	 data_( const_cast< T * >( a ) ),
	 size_( size )
	{}

public: // Inspector

	// Size
	size_type
	size() const
	{
		return size_;
	}

	// Size
	int
	isize() const
	{
		return static_cast< int >( size_ );
	}

private: // Data

	T * data_{ nullptr }; // Pointer (non-owning) to data array
	size_type size_{ 0u }; // Size of data array

}; // ArrayTail

} // ObjexxFCL

#endif // ObjexxFCL_ArrayTail_hh_INCLUDED
