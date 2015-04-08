#ifndef ObjexxFCL_MArray_hh_INCLUDED
#define ObjexxFCL_MArray_hh_INCLUDED

// MArray: Member Array Proxy Abstract Base Class Template
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

// ObjexxFCL Headers
#include <ObjexxFCL/BArray.hh>
#include <ObjexxFCL/fmt.hh>
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cassert>
#include <cstddef>
#include <initializer_list>
#include <iomanip>
#include <istream>
#include <ostream>
#include <type_traits>

namespace ObjexxFCL {

// MArray: Member Array Proxy Abstract Base Class Template
template< class A, typename T >
class MArray : public BArray
{

public: // Types

	typedef  A  ArrayType;
	typedef  typename A::value_type  Class;
	typedef  T Class::*  MPtr;
	typedef  TypeTraits< T >  Traits;
	typedef  IndexRange  IR;

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

protected: // Creation

	// Copy Constructor
	inline
	MArray( MArray const & a ) :
	 BArray( a ),
	 array_( a.array_ ),
	 pmem_( a.pmem_ )
	{}

	// Constructor
	inline
	MArray( A & a, T Class::* pmem ) :
	 array_( a ),
	 pmem_( pmem )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~MArray()
	{}

protected: // Assignment

	// Copy Assignment
	inline
	MArray &
	operator =( MArray const & a ); // Disallow

public: // Predicate

	// Allocated
	inline
	bool
	allocated() const
	{
		return array_.allocated();
	}

	// Active Array Empty?
	inline
	bool
	empty() const
	{
		return ( array_.size() == 0u );
	}

	// Active Array Size Bounded?
	inline
	bool
	size_bounded() const
	{
		return array_.size_bounded();
	}

	// Conformable?
	template< class Ar >
	inline
	bool
	conformable( Ar const & a ) const
	{
		return array_.conformable( a );
	}

public: // Inspector

	// Rank
	virtual
	int
	rank() const = 0;

	// Size
	inline
	size_type
	size() const
	{
		return array_.size();
	}

	// Size
	inline
	int
	isize() const
	{
		return array_.isize();
	}

	// IndexRange of a Dimension
	inline
	IR
	I( int const d ) const
	{
		return IR( 1, u( d ) );
	}

	// Lower Index of a Dimension
	inline
	int
	l( int const d ) const
	{
		assert( ( 1 <= d ) && ( d <= rank() ) );
#ifdef NDEBUG
		static_cast< void >( d ); // Suppress unused warning
#endif
		return 1;
	}

	// Upper Index of a Dimension
	inline
	int
	u( int const d ) const
	{
		return array_.isize( d );
	}

	// Size of a Dimension
	inline
	size_type
	size( int const d ) const
	{
		return array_.size( d );
	}

	// Size of a Dimension
	inline
	int
	isize( int const d ) const
	{
		return array_.isize( d );
	}

	// Proxied Array
	inline
	A const &
	array() const
	{
		return array_;
	}

	// Proxied Array
	inline
	A &
	array()
	{
		return array_;
	}

protected: // Methods

	// Array Index of an Index of Dimension
	inline
	int
	j( int const d, int const i ) const
	{
		return array_.l( d ) + i - 1;
	}

	// Array Index of Dimension 1
	inline
	int
	j1( int const i ) const
	{
		return array_.l1() + i - 1;
	}

	// Array Index of Dimension 2
	inline
	int
	j2( int const i ) const
	{
		return array_.l2() + i - 1;
	}

	// Array Index of Dimension 3
	inline
	int
	j3( int const i ) const
	{
		return array_.l3() + i - 1;
	}

	// Array Index of Dimension 4
	inline
	int
	j4( int const i ) const
	{
		return array_.l4() + i - 1;
	}

	// Array Index of Dimension 5
	inline
	int
	j5( int const i ) const
	{
		return array_.l5() + i - 1;
	}

	// Array Index of Dimension 6
	inline
	int
	j6( int const i ) const
	{
		return array_.l6() + i - 1;
	}

protected: // Static Methods

	// Is Last Index in [1,u] Range?
	inline
	static
	bool
	in_range( int const u, int const i )
	{
		assert( u > 0 );
		return ( ( 1 <= i ) && ( i <= u ) );
	}

	// Are Last Two Indexes in [1,u] Range?
	inline
	static
	bool
	in_range( int const u, int const i, int const j )
	{
		assert( u > 0 );
		return ( ( 1 <= i ) && ( i <= u ) && ( 1 <= j ) && ( j <= u ) );
	}

protected: // Data

	A & array_; // A

	T Class::* pmem_; // Pointer to member

}; // MArray

// Conformable?
template< class A, typename T, class Ar >
inline
bool
conformable( MArray< A, T > const & a, Ar const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename T, class Ar >
inline
bool
conformable( Ar const & a, MArray< A, T > const & b )
{
	return b.conformable( a );
}

} // ObjexxFCL

#endif // ObjexxFCL_MArray_hh_INCLUDED
