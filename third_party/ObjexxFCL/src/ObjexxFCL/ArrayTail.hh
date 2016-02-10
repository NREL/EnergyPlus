#ifndef ObjexxFCL_ArrayTail_hh_INCLUDED
#define ObjexxFCL_ArrayTail_hh_INCLUDED

// ArrayTail: Contiguous Array Tail Proxy
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
#include <ObjexxFCL/ArrayTail.fwd.hh>

namespace ObjexxFCL {

// Forward
template< typename > class Array;

// ArrayTail: Contiguous Array Tail Proxy
template< typename T >
class ArrayTail
{

private: // Friend

	friend class Array< T >;

public: // Types

	// STL style
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;

	// C++ style
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

public: // Creation

	// Copy Constructor
	ArrayTail( ArrayTail const & s ) :
	 data_( s.data_ ),
	 size_( s.size_ )
	{}

	// Pointer + Size Constructor
	ArrayTail( T const * a, size_type const size ) :
	 data_( const_cast< T * >( a ) ),
	 size_( size )
	{}

	// Destructor
	~ArrayTail()
	{}

public: // Assignment

	// Copy Assignment
	ArrayTail &
	operator =( ArrayTail const & s )
	{
		if ( this != &s ) {
			data_ = s.data_;
			size_ = s.size_;
		}
		return *this;
	}

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

	T * data_; // Pointer (non-owning) to data array
	size_type size_; // Size of data array

}; // ArrayTail

} // ObjexxFCL

#endif // ObjexxFCL_ArrayTail_hh_INCLUDED
