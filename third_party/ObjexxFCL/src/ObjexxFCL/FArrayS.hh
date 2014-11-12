#ifndef ObjexxFCL_FArrayS_hh_INCLUDED
#define ObjexxFCL_FArrayS_hh_INCLUDED

// FArrayS: Slice Array Proxy Abstract Base Class Template
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/FArrayS.fwd.hh>
#include <ObjexxFCL/BArray.hh>
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/IndexSlice.hh>
#include <ObjexxFCL/StaticIndexRange.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <initializer_list>
#include <limits>
#include <type_traits>

namespace ObjexxFCL {

// FArrayS: Slice Array Proxy Abstract Base Class Template
template< typename T >
class FArrayS : public BArray
{

private: // Friend

	template< typename > friend class FArrayS;

public: // Types

	typedef  FArrayS< T >  Base;
	typedef  TypeTraits< T >  Traits;
	typedef  StaticIndexRange  IR;
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
	inline
	FArrayS() :
	 data_( nullptr ),
	 data_beg_( nullptr ),
	 data_end_( nullptr ),
	 size_( 0 )
	{}

	// Copy Constructor
	inline
	FArrayS( FArrayS const & a ) :
	 BArray( a ),
	 data_( a.data_ ),
	 data_beg_( a.data_beg_ ),
	 data_end_( a.data_end_ ),
	 size_( a.size_ )
	{}

	// Data Constructor
	inline
	FArrayS( T const * data, size_type const size ) :
	 data_( const_cast< T * >( data ) ),
	 data_beg_( nullptr ),
	 data_end_( nullptr ),
	 size_( size )
	{}

	// Non-Const Data Constructor
	inline
	FArrayS( T * data, size_type const size ) :
	 data_( data ),
	 data_beg_( nullptr ),
	 data_end_( nullptr ),
	 size_( size )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~FArrayS()
	{}

public: // Predicate

	// Dimensions Initialized?
	inline
	bool
	dimensions_initialized() const
	{
		return true;
	}

	// Active Array Empty?
	inline
	bool
	empty() const
	{
		return ( size_ == 0u );
	}

	// Active Array Size Bounded?
	inline
	bool
	size_bounded() const
	{
		return true;
	}

	// Conformable?
	template< class ArrayType >
	inline
	bool
	conformable( ArrayType const & a ) const
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
	template< template< typename > class ArrayType >
	inline
	bool
	overlap( ArrayType< T > const & a ) const
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
	inline
	size_type
	size() const
	{
		return size_;
	}

	// Active Array Size
	inline
	int
	isize() const
	{
		return static_cast< int >( size_ );
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
	inline
	T const *
	data() const
	{
		return data_;
	}

	// Array Data Pointer
	inline
	T *
	data()
	{
		return data_;
	}

	// Array Data Begin Pointer
	inline
	T const *
	data_beg() const
	{
		return data_beg_;
	}

	// Array Data Begin Pointer
	inline
	T *
	data_beg()
	{
		return data_beg_;
	}

	// Array Data End Pointer
	inline
	T const *
	data_end() const
	{
		return data_end_;
	}

	// Array Data End Pointer
	inline
	T *
	data_end()
	{
		return data_end_;
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

	// Slice Constant for a Scalar Index
	inline
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
	inline
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

	size_type size_; // Size of visible array

}; // FArrayS

// Conformable?
template< typename T, class ArrayType >
inline
bool
conformable( FArrayS< T > const & s, ArrayType const & a )
{
	return s.conformable( a );
}

// Conformable?
template< class ArrayType, typename T >
inline
bool
conformable( ArrayType const & a, FArrayS< T > const & s )
{
	return s.conformable( a );
}

} // ObjexxFCL

#endif // ObjexxFCL_FArrayS_hh_INCLUDED
