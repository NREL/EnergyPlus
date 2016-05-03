#ifndef ObjexxFCL_ArrayS_hh_INCLUDED
#define ObjexxFCL_ArrayS_hh_INCLUDED

// ArrayS: Slice Array Proxy Abstract Base Class Template
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
#include <ObjexxFCL/ArrayS.fwd.hh>
#include <ObjexxFCL/BArray.hh>
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/fmt.hh>
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/IndexSlice.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <initializer_list>
#include <iomanip>
#include <istream>
#include <limits>
#include <ostream>
#include <type_traits>

namespace ObjexxFCL {

// Forward
template< typename > class Array;

// ArrayS: Slice Array Proxy Abstract Base Class Template
template< typename T >
class ArrayS : public BArray
{

private: // Friend

	template< typename > friend class ArrayS;
	template< typename > friend class Array;

public: // Types

	typedef  ArrayS< T >  Base;
	typedef  TypeTraits< T >  Traits;
	typedef  IndexRange  IR;
	typedef  IndexSlice  IS;
	typedef  DimensionSlice  DS;

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

	// Default Constructor
	ArrayS() :
	 data_( nullptr ),
	 data_beg_( nullptr ),
	 data_end_( nullptr ),
	 size_( 0u ),
	 contiguous_( false )
	{}

	// Copy Constructor
	ArrayS( ArrayS const & a ) :
	 BArray( a ),
	 data_( a.data_ ),
	 data_beg_( a.data_beg_ ),
	 data_end_( a.data_end_ ),
	 size_( a.size_ ),
	 contiguous_( a.contiguous_ )
	{}

	// Data Constructor
	ArrayS( T const * data, size_type const size ) :
	 data_( const_cast< T * >( data ) ),
	 data_beg_( nullptr ),
	 data_end_( nullptr ),
	 size_( size ),
	 contiguous_( false )
	{}

	// Non-Const Data Constructor
	ArrayS( T * data, size_type const size ) :
	 data_( data ),
	 data_beg_( nullptr ),
	 data_end_( nullptr ),
	 size_( size ),
	 contiguous_( false )
	{}

public: // Creation

	// Destructor
	virtual
	~ArrayS()
	{}

public: // Predicate

	// Active Array Empty?
	bool
	empty() const
	{
		return ( size_ == 0u );
	}

	// Active Array Size Bounded?
	bool
	size_bounded() const
	{
		return true;
	}

	// Contiguous?
	bool
	contiguous() const
	{
		return contiguous_;
	}

	// Conformable?
	template< class A >
	bool
	conformable( A const & a ) const
	{
		if ( rank() != a.rank() ) return false;
		for ( int i = 1; i <= rank(); ++i ) if ( size( i ) != a.size( i ) ) return false;
		return true;
	}

	// Memory Can Overlap a Range?
	bool
	overlap( T const * const b, T const * const e ) const
	{
		if ( ( data_beg_ == nullptr ) || ( data_end_ == nullptr ) || ( b == nullptr ) || ( e == nullptr ) ) { // No active memory range(s)
			return false;
		} else if ( size_ == 0u ) { // No finite memory range
			return false;
		} else { // Bounded ranges
			assert( data_beg_ <= data_end_ );
			assert( b <= e );
			return ( ( data_beg_ >= b ? data_beg_ : b ) <= ( data_end_ <= e ? data_end_ : e ) );
		}
	}

	// Memory Can Overlap an Array?
	template< template< typename > class A >
	bool
	overlap( A< T > const & a ) const
	{
		if ( data_ == nullptr ) { // No active memory range
			return false;
		} else { // Bounded ranges
			return overlap( a.data_beg(), a.data_end() );
		}
	}

public: // Inspector

	// Rank
	virtual
	int
	rank() const = 0;

	// Active Array Size
	size_type
	size() const
	{
		return size_;
	}

	// Active Array Size
	int
	isize() const
	{
		return static_cast< int >( size_ );
	}

	// Lower Index of a Dimension
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
	virtual
	int
	u( int const d ) const = 0;

	// Size of a Dimension
	virtual
	size_type
	size( int const d ) const = 0;

	// Size of a Dimension
	virtual
	int
	isize( int const d ) const = 0;

	// Array Data Pointer
	T const *
	data() const
	{
		return data_;
	}

	// Array Data Pointer
	T *
	data()
	{
		return data_;
	}

	// Array Data Begin Pointer
	T const *
	data_beg() const
	{
		return data_beg_;
	}

	// Array Data Begin Pointer
	T *
	data_beg()
	{
		return data_beg_;
	}

	// Array Data End Pointer
	T const *
	data_end() const
	{
		return data_end_;
	}

	// Array Data End Pointer
	T *
	data_end()
	{
		return data_end_;
	}

protected: // Static Methods

	// Is Last Index in [1,u] Range?
	static
	bool
	in_range( int const u, int const i )
	{
		assert( u > 0 );
		return ( ( 1 <= i ) && ( i <= u ) );
	}

	// Are Last Two Indexes in [1,u] Range?
	static
	bool
	in_range( int const u, int const i, int const j )
	{
		assert( u > 0 );
		return ( ( 1 <= i ) && ( i <= u ) && ( 1 <= j ) && ( j <= u ) );
	}

	// Slice Constant for a Scalar Index
	static
	std::int64_t
	slice_k( IR const & range, int const i, std::int64_t const multiplier = 1 )
	{
		assert( range.contains( i ) );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( i ) );
		(void)range; // Suppress unused warning in release builds
		return i * multiplier;
	}

	// Slice Constant for a Scalar Index
	static
	std::int64_t
	slice_k( int const u, int const i, std::int64_t const multiplier = 1 )
	{
		assert( ( 1 <= i ) && ( i <= u ) );
		assert( multiplier <= std::numeric_limits< std::int64_t >::max() / std::abs( i ) );
		(void)u; // Suppress unused warning in release builds
		return i * multiplier;
	}

protected: // Data

	T * data_; // Pointer to data array
	T * data_beg_; // Pointer to data begin
	T * data_end_; // Pointer to data end

	size_type const size_; // Size of visible array

	bool contiguous_; // Contiguous?

}; // ArrayS

// Conformable?
template< typename T, class A >
inline
bool
conformable( ArrayS< T > const & s, A const & a )
{
	return s.conformable( a );
}

// Conformable?
template< class A, typename T >
inline
bool
conformable( A const & a, ArrayS< T > const & s )
{
	return s.conformable( a );
}

} // ObjexxFCL

#endif // ObjexxFCL_ArrayS_hh_INCLUDED
