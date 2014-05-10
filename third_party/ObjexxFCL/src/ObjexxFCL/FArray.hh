#ifndef ObjexxFCL_FArray_hh_INCLUDED
#define ObjexxFCL_FArray_hh_INCLUDED

// FArray: Fortran-Compatible Array Abstract Base Class
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
#include <ObjexxFCL/FArray.fwd.hh>
#include <ObjexxFCL/BArray.hh>
#include <ObjexxFCL/CArray.hh>
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/FArrayTail.hh>
#include <ObjexxFCL/FArrayS.hh>
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/IndexSlice.hh>
#include <ObjexxFCL/InitializerSentinel.hh>
#include <ObjexxFCL/MArray.hh>
#include <ObjexxFCL/ProxySentinel.hh>
#include <ObjexxFCL/proxy_const_assert.hh>
#include <ObjexxFCL/Sticky.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <limits>
#include <type_traits>
#include <typeinfo>
#include <utility>
#include <vector>
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
#include <iostream>
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT

namespace ObjexxFCL {

// FArray: Fortran-Compatible Array Abstract Base Class
//
// Note:
//  Can hold numeric or non-numeric values: Numeric operations on non-numeric values won't compile
//  Any meaningful array index ranges can be specified as in Fortran
//  Column-major storage order used as in Fortran
//  Zero-sized arrays are supported but have no valid indices
//  Argument/proxy arrays can have unbounded/unknown size
//  For efficiency constructors without initializer function or value do not initialize the array
//  For efficiency bounds checking is only active via asserts in debug builds
template< typename T >
class FArray : public BArray
{

private: // Friend

	template< typename > friend class FArray;

protected: // Types

	typedef  internal::InitializerSentinel  InitializerSentinel;
	typedef  internal::ProxySentinel  ProxySentinel;

public: // Types

	typedef  FArray< T >  Base;
	typedef  FArrayTail< T >  Tail;
	typedef  TypeTraits< T >  Traits;
	typedef  IndexRange  IR;
	typedef  DimensionSlice  DS;
	typedef  IndexSlice  IS;

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
	FArray() :
		data_size_( 0 ),
		data_( nullptr ),
		size_( 0 ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Copy Constructor
	inline
	FArray( FArray const & a ) :
		BArray( a ),
		data_size_( size_of( a.size_ ) ),
		data_( a.data_ ? new T[ data_size_ ] : nullptr ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( a.shift_ ),
		sdata_( data_ - shift_ )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			reassign( i, a[ i ] );
		}
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Copy Constructor Template
	template< typename U >
	inline
	explicit
	FArray( FArray< U > const & a ) :
		data_size_( size_of( a.size() ) ),
		data_( a.data_ ? new T[ data_size_ ] : nullptr ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( a.shift_ ),
		sdata_( data_ - shift_ )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			reassign( i, a[ i ] );
		}
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Slice Constructor Template
	template< typename U >
	inline
	explicit
	FArray( FArrayS< U > const & a ) :
		data_size_( size_of( a.size() ) ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray( MArray< A, M > const & a ) :
		data_size_( size_of( a.size() ) ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Size Constructor
	inline
	explicit
	FArray( size_type const size ) :
		data_size_( size_of( size ) ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
#if defined(OBJEXXFCL_FARRAY_INIT) || defined(OBJEXXFCL_FARRAY_INIT_DEBUG)
		std::fill_n( data_, size_, Traits::initial_array_value() );
#endif // OBJEXXFCL_FARRAY_INIT || OBJEXXFCL_FARRAY_INIT_DEBUG
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Size + InitializerSentinel Constructor
	inline
	FArray( size_type const size, InitializerSentinel const & ) :
		data_size_( size_of( size ) ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Initializer List Constructor Template
	template< typename U >
	inline
	FArray( std::initializer_list< U > const l ) :
		data_size_( l.size() ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
		auto il( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++il ) {
			reassign( i, *il );
		}
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// std::array Constructor Template
	template< typename U, Size s >
	inline
	FArray( std::array< U, s > const & a ) :
		data_size_( s ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
		auto ia( a.begin() );
		for ( size_type i = 0; i < size_; ++i, ++ia ) {
			reassign( i, *ia );
		}
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// std::vector Constructor Template
	template< typename U >
	inline
	FArray( std::vector< U > const & v ) :
		data_size_( v.size() ),
		data_( new T[ data_size_ ] ),
		size_( data_size_ ),
		owner_( true ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{
		auto iv( v.begin() );
		for ( size_type i = 0; i < size_; ++i, ++iv ) {
			reassign( i, *iv );
		}
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Default Proxy Constructor
	inline
	FArray( ProxySentinel const & ) :
		data_size_( 0 ),
		data_( nullptr ),
		size_( 0 ),
		owner_( false ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( false ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{}

	// Array Proxy Constructor
	inline
	FArray( FArray const & a, ProxySentinel const & ) :
		data_size_( a.data_size_ ),
		data_( a.data_ ),
		size_( a.size_ ),
		owner_( false ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( true ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	// Non-Const Array Proxy Constructor
	inline
	FArray( FArray & a, ProxySentinel const & ) :
		data_size_( a.data_size_ ),
		data_( a.data_ ),
		size_( a.size_ ),
		owner_( false ),
		const_proxy_( a.const_proxy_ ),
		shift_( 0 ),
		sdata_( nullptr )
	{}
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Tail Proxy Constructor
	inline
	FArray( Tail const & s, ProxySentinel const & ) :
		data_size_( s.size() ),
		data_( s.data_ ),
		size_( data_size_ ),
		owner_( false ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( true ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	// Non-Const Tail Proxy Constructor
	inline
	FArray( Tail & s, ProxySentinel const & ) :
		data_size_( s.size() ),
		data_( s.data_ ),
		size_( data_size_ ),
		owner_( false ),
		const_proxy_( s.const_proxy_ ),
		shift_( 0 ),
		sdata_( nullptr )
	{}
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Value Proxy Constructor
	inline
	FArray( T const & t, ProxySentinel const & ) :
		data_size_( npos ), // Unknown
		data_( const_cast< T * >( &t ) ),
		size_( npos ), // Unbounded
		owner_( false ),
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_( true ),
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_( 0 ),
		sdata_( nullptr )
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	// Non-Const Value Proxy Constructor
	inline
	FArray( T & t, ProxySentinel const & ) :
		data_size_( npos ), // Unknown
		data_( &t ),
		size_( npos ), // Unbounded
		owner_( false ),
		const_proxy_( false ),
		shift_( 0 ),
		sdata_( nullptr )
	{}
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Switch to Size Construction
	void
	reconstruct_by_size( size_type const size )
	{
		delete[] data_;
		data_size_ = size;
		size_ = data_size_;
		data_ = new T[ data_size_ ];
#if defined(OBJEXXFCL_FARRAY_INIT) || defined(OBJEXXFCL_FARRAY_INIT_DEBUG)
		reassign( Traits::initial_array_value() );
#endif // OBJEXXFCL_FARRAY_INIT || OBJEXXFCL_FARRAY_INIT_DEBUG
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

public: // Creation

	// Destructor
	inline
	virtual
	~FArray()
	{
		if ( owner_ ) delete[] data_;
	}

protected: // Assignment

	// Copy Assignment
	inline
	void
	operator =( FArray const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size_ );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = a[ i ];
			}
		}
	}

	// Copy Assignment Template
	template< typename U >
	inline
	void
	operator =( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = a[ i ];
		}
	}

	// Initializer List Assignment Template
	template< typename U >
	inline
	void
	operator =( std::initializer_list< U > const l )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == l.size() );
		std::copy( l.begin(), l.end(), data_ );
	}

	// += Array
	inline
	void
	operator +=( FArray const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] += c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] += a[ i ];
			}
		}
	}

	// -= Array
	inline
	void
	operator -=( FArray const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] -= c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] -= a[ i ];
			}
		}
	}

	// *= Array
	inline
	void
	operator *=( FArray const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] *= c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] *= a[ i ];
			}
		}
	}

	// /= Array
	inline
	void
	operator /=( FArray const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				assert( a[ i ] != T( 0 ) );
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] /= c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				assert( a[ i ] != T( 0 ) );
				data_[ i ] /= a[ i ];
			}
		}
	}

	// &&= Array
	inline
	void
	and_equals( FArray< T > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = data_[ i ] && c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = data_[ i ] && a[ i ];
			}
		}
	}

	// ||= Array
	inline
	void
	or_equals( FArray< T > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( size_type i = 0; i < size_; ++i ) {
				c[ i ] = a[ i ];
			}
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = data_[ i ] || c[ i ];
			}
		} else { // Not overlap-safe
			for ( size_type i = 0; i < size_; ++i ) {
				data_[ i ] = data_[ i ] || a[ i ];
			}
		}
	}

	// += Array Template
	template< typename U >
	inline
	void
	operator +=( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += a[ i ];
		}
	}

	// -= Array Template
	template< typename U >
	inline
	void
	operator -=( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= a[ i ];
		}
	}

	// *= Array Template
	template< typename U >
	inline
	void
	operator *=( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= a[ i ];
		}
	}

	// /= Array Template
	template< typename U >
	inline
	void
	operator /=( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			assert( a[ i ] != T( 0 ) );
			data_[ i ] /= a[ i ];
		}
	}

	// &&= Array Template
	template< typename U >
	inline
	void
	and_equals( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] && a[ i ];
		}
	}

	// ||= Array Template
	template< typename U >
	inline
	void
	or_equals( FArray< U > const & a )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] || a[ i ];
		}
	}

public: // Assignment

	// = Value
	inline
	FArray &
	operator =( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		if ( data_ ) std::fill_n( data_, size_, t );
		return *this;
	}

	// += Value
	inline
	FArray &
	operator +=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += t;
		}
		return *this;
	}

	// -= Value
	inline
	FArray &
	operator -=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= t;
		}
		return *this;
	}

	// *= Value
	inline
	FArray &
	operator *=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= t;
		}
		return *this;
	}

	// /= Value
	inline
	FArray &
	operator /=( T const & t )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		assert( t != T( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] /= t;
		}
		return *this;
	}

public: // Subscript

	// array[ i ] const: Linear Subscript
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( ( i < size_ ) || ( size_ == npos ) );
		return data_[ i ];
	}

	// array[ i ]: Linear Subscript
	inline
	T &
	operator []( size_type const i )
	{
		proxy_const_assert( not_const_proxy() );
		assert( ( i < size_ ) || ( size_ == npos ) );
		return data_[ i ];
	}

public: // Predicate

	// Dimensions Initialized?
	virtual
	bool
	dimensions_initialized() const = 0;

	// Initializer Active?
	virtual
	bool
	initializer_active() const = 0;

	// Active?
	inline
	bool
	active() const
	{
		return ( data_ != nullptr );
	}

	// Allocated
	inline
	bool
	allocated() const
	{
		return ( active() && dimensions_initialized() );
	}

	// Contiguous?
	inline
	bool
	is_contiguous() const
	{
		return true;
	}

	// Data Size Bounded?
	inline
	bool
	data_size_bounded() const
	{
		return ( data_size_ != npos );
	}

	// Data Size Unbounded?
	inline
	bool
	data_size_unbounded() const
	{
		return ( data_size_ == npos );
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
		return ( size_ != npos );
	}

	// Active Array Size Unbounded?
	inline
	bool
	size_unbounded() const
	{
		return ( size_ == npos );
	}

	// Owner?
	inline
	bool
	owner() const
	{
		return owner_;
	}

	// Proxy?
	inline
	bool
	proxy() const
	{
		return ! owner_;
	}

	// All Elements Default Valued?
	inline
	bool
	is_default() const
	{
		for ( size_type i = 0; i < size_; ++i ) {
			if ( data_[ i ] != Traits::initial_array_value() ) return false;
		}
		return true;
	}

	// All Elements Zero?
	inline
	bool
	is_zero() const
	{
		for ( size_type i = 0; i < size_; ++i ) {
			if ( data_[ i ] != T( 0 ) ) return false;
		}
		return true;
	}

	// Uniform Valued?
	inline
	bool
	is_uniform() const
	{
		if ( size_ <= 1 ) return true;
		T const & t( data_[ 0 ] );
		for ( size_type i = 1; i < size_; ++i ) {
			if ( data_[ i ] != t ) return false;
		}
		return true;
	}

	// Uniform Valued with Specified Value?
	inline
	bool
	is_uniform( T const & t ) const
	{
		for ( size_type i = 0; i < size_; ++i ) {
			if ( data_[ i ] != t ) return false;
		}
		return true;
	}

	// Memory Can Overlap a Range?
	bool
	overlap( T const * const b, T const * const e ) const
	{
		if ( ( data_ == nullptr ) || ( b == nullptr ) || ( e == nullptr ) ) { // No active memory range(s)
			return false;
		} else if ( size_ == 0u ) { // No finite memory range
			return false;
		} else if ( size_ == npos ) { // No memory upper bound
			return ( e >= data_ );
		} else { // Bounded ranges
			assert( b <= e );
			T const * const de( data_ + size_ - 1 ); // Data end pointer
			return ( ( data_ >= b ? data_ : b ) <= ( de <= e ? de : e ) );
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

	// Data Size
	inline
	size_type
	data_size() const
	{
		return data_size_;
	}

	// Active Array Size
	inline
	size_type
	size() const
	{
		return size_;
	}

	// IndexRange of a Dimension
	virtual
	IR const &
	I( int const d ) const = 0;

	// Lower Index of a Dimension
	virtual
	int
	l( int const d ) const = 0;

	// Upper Index of Dimension
	virtual
	int
	u( int const d ) const = 0;

	// Size of a Dimension
	virtual
	size_type
	size( int const d ) const = 0;

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
		return data_;
	}

	// Array Data Begin Pointer
	inline
	T *
	data_beg()
	{
		return data_;
	}

	// Array Data End Pointer
	inline
	T const *
	data_end() const
	{
		return ( ( data_ != nullptr ) && ( size_ > 0u ) && ( size_ != npos ) ? data_ + size_ - 1 : nullptr );
	}

	// Array Data End Pointer
	inline
	T *
	data_end()
	{
		return ( ( data_ != nullptr ) && ( size_ > 0u ) && ( size_ != npos ) ? data_ + size_ - 1 : nullptr );
	}

public: // Modifier

	// Clear
	inline
	virtual
	FArray &
	clear()
	{
		data_size_ = 0;
		if ( owner_ ) delete[] data_; data_ = nullptr;
		size_ = 0u;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = false;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = 0;
		sdata_ = data_;
		return *this;
	}

	// Assign Default Value to all Elements
	inline
	virtual
	FArray &
	to_default()
	{
		proxy_const_assert( not_const_proxy() );
		if ( data_ ) std::fill_n( data_, size_, Traits::initial_array_value() );
		return *this;
	}

	// Assign Zero to all Elements
	//  Can't be virtual (for covariant return) or will try to instantiate for all value types
	inline
	void
	zero()
	{
		proxy_const_assert( not_const_proxy() );
		if ( data_ ) std::fill_n( data_, size_, T( 0 ) );
	}

	// Assign Zero to all Elements
	//  Can't be virtual (for covariant return) or will try to instantiate for all value types
	inline
	void
	to_zero()
	{
		proxy_const_assert( not_const_proxy() );
		if ( data_ ) std::fill_n( data_, size_, T( 0 ) );
	}

	// Invert (Elemental)
	inline
	void
	invert()
	{
		T const one( 1 );
		for ( size_type i = 0; i < size_; ++i ) {
			assert( data_[ i ] != T( 0 ) );
			data_[ i ] = one / data_[ i ];
		}
	}

	// Copy Array Data from Source
	template< typename U >
	inline
	void
	data_copy_from( U const * source, size_type const size )
	{
		if ( data_ ) std::memcpy( data_, source, std::min( size, size_ ) );
	}

public: // Comparison: Predicate

	// FArray == Value
	inline
	friend
	bool
	eq( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] == t ) ) return false;
		}
		return true;
	}

	// Value == FArray
	inline
	friend
	bool
	eq( T const & t, FArray const & a )
	{
		return eq( a, t );
	}

	// FArray != Value
	inline
	friend
	bool
	ne( FArray const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Value != FArray
	inline
	friend
	bool
	ne( T const & t, FArray const & a )
	{
		return ! eq( t, a );
	}

	// FArray < Value
	inline
	friend
	bool
	lt( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] < t ) ) return false;
		}
		return true;
	}

	// Value < FArray
	inline
	friend
	bool
	lt( T const & t, FArray const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( t < a[ i ] ) ) return false;
		}
		return true;
	}

	// FArray <= Value
	inline
	friend
	bool
	le( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] <= t ) ) return false;
		}
		return true;
	}

	// Value <= FArray
	inline
	friend
	bool
	le( T const & t, FArray const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( t <= a[ i ] ) ) return false;
		}
		return true;
	}

	// FArray > Value
	inline
	friend
	bool
	gt( FArray const & a, T const & t )
	{
		return lt( t, a );
	}

	// Value > FArray
	inline
	friend
	bool
	gt( T const & t, FArray const & a )
	{
		return lt( a, t );
	}

	// FArray >= Value
	inline
	friend
	bool
	ge( FArray const & a, T const & t )
	{
		return le( t, a );
	}

	// Value >= FArray
	inline
	friend
	bool
	ge( T const & t, FArray const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any FArray == Value
	inline
	friend
	bool
	any_eq( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] == t ) return true;
		}
		return false;
	}

	// Any Value == FArray
	inline
	friend
	bool
	any_eq( T const & t, FArray const & a )
	{
		return any_eq( a, t );
	}

	// Any FArray != Value
	inline
	friend
	bool
	any_ne( FArray const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any Value != FArray
	inline
	friend
	bool
	any_ne( T const & t, FArray const & a )
	{
		return ! eq( a, t );
	}

	// Any FArray < Value
	inline
	friend
	bool
	any_lt( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < t ) return true;
		}
		return false;
	}

	// Any Value < FArray
	inline
	friend
	bool
	any_lt( T const & t, FArray const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( t < a[ i ] ) return true;
		}
		return false;
	}

	// Any FArray <= Value
	inline
	friend
	bool
	any_le( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= t ) return true;
		}
		return false;
	}

	// Any Value <= FArray
	inline
	friend
	bool
	any_le( T const & t, FArray const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( t <= a[ i ] ) return true;
		}
		return false;
	}

	// Any FArray > Value
	inline
	friend
	bool
	any_gt( FArray const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any Value > FArray
	inline
	friend
	bool
	any_gt( T const & t, FArray const & a )
	{
		return any_lt( a, t );
	}

	// Any FArray >= Value
	inline
	friend
	bool
	any_ge( FArray const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value >= FArray
	inline
	friend
	bool
	any_ge( T const & t, FArray const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All FArray == Value
	inline
	friend
	bool
	all_eq( FArray const & a, T const & t )
	{
		return eq( a, t );
	}

	// All Value == FArray
	inline
	friend
	bool
	all_eq( T const & t, FArray const & a )
	{
		return eq( a, t );
	}

	// All FArray != Value
	inline
	friend
	bool
	all_ne( FArray const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All Value != FArray
	inline
	friend
	bool
	all_ne( T const & t, FArray const & a )
	{
		return ! any_eq( a, t );
	}

	// All FArray < Value
	inline
	friend
	bool
	all_lt( FArray const & a, T const & t )
	{
		return lt( a, t );
	}

	// All Value < FArray
	inline
	friend
	bool
	all_lt( T const & t, FArray const & a )
	{
		return lt( t, a );
	}

	// All FArray <= Value
	inline
	friend
	bool
	all_le( FArray const & a, T const & t )
	{
		return le( a, t );
	}

	// All Value <= FArray
	inline
	friend
	bool
	all_le( T const & t, FArray const & a )
	{
		return le( t, a );
	}

	// All FArray > Value
	inline
	friend
	bool
	all_gt( FArray const & a, T const & t )
	{
		return gt( a, t );
	}

	// All Value > FArray
	inline
	friend
	bool
	all_gt( T const & t, FArray const & a )
	{
		return gt( t, a );
	}

	// All FArray >= Value
	inline
	friend
	bool
	all_ge( FArray const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value >= FArray
	inline
	friend
	bool
	all_ge( T const & t, FArray const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count FArray == Value
	inline
	friend
	size_type
	count_eq( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] == t ) ++n;
		}
		return n;
	}

	// Count Value == FArray
	inline
	friend
	size_type
	count_eq( T const & t, FArray const & a )
	{
		return count_eq( a, t );
	}

	// Count FArray != Value
	inline
	friend
	size_type
	count_ne( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] != t ) ++n;
		}
		return n;
	}

	// Count Value != FArray
	inline
	friend
	size_type
	count_ne( T const & t, FArray const & a )
	{
		return count_ne( a, t );
	}

	// Count FArray < Value
	inline
	friend
	size_type
	count_lt( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < t ) ++n;
		}
		return n;
	}

	// Count Value < FArray
	inline
	friend
	size_type
	count_lt( T const & t, FArray const & a )
	{
		return count_gt( a, t );
	}

	// Count FArray <= Value
	inline
	friend
	size_type
	count_le( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= t ) ++n;
		}
		return n;
	}

	// Count Value <= FArray
	inline
	friend
	size_type
	count_le( T const & t, FArray const & a )
	{
		return count_ge( a, t );
	}

	// Count FArray > Value
	inline
	friend
	size_type
	count_gt( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] > t ) ++n;
		}
		return n;
	}

	// Count Value > FArray
	inline
	friend
	size_type
	count_gt( T const & t, FArray const & a )
	{
		return count_lt( a, t );
	}

	// Count FArray >= Value
	inline
	friend
	size_type
	count_ge( FArray const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] >= t ) ++n;
		}
		return n;
	}

	// Count Value >= FArray
	inline
	friend
	size_type
	count_ge( T const & t, FArray const & a )
	{
		return count_le( a, t );
	}

protected: // Comparison: Predicate

	// FArray == FArray
	inline
	friend
	bool
	eq( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] == b[ i ] ) ) return false;
		}
		return true;
	}

	// FArray != FArray
	inline
	friend
	bool
	ne( FArray const & a, FArray const & b )
	{
		return ! eq( a, b );
	}

	// FArray < FArray
	inline
	friend
	bool
	lt( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}

	// FArray <= FArray
	inline
	friend
	bool
	le( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}

	// FArray > FArray
	inline
	friend
	bool
	gt( FArray const & a, FArray const & b )
	{
		return lt( b, a );
	}

	// FArray >= FArray
	inline
	friend
	bool
	ge( FArray const & a, FArray const & b )
	{
		return le( b, a );
	}

protected: // Comparison: Predicate: Any

	// Any FArray == FArray
	inline
	friend
	bool
	any_eq( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] == b[ i ] ) ) return false;
		}
		return true;
	}

	// Any FArray != FArray
	inline
	friend
	bool
	any_ne( FArray const & a, FArray const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray < FArray
	inline
	friend
	bool
	any_lt( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < b[ i ] ) return true;
		}
		return false;
	}

	// Any FArray <= FArray
	inline
	friend
	bool
	any_le( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= b[ i ] ) return true;
		}
		return false;
	}

	// Any FArray > FArray
	inline
	friend
	bool
	any_gt( FArray const & a, FArray const & b )
	{
		return any_lt( b, a );
	}

	// Any FArray >= FArray
	inline
	friend
	bool
	any_ge( FArray const & a, FArray const & b )
	{
		return any_le( b, a );
	}

protected: // Comparison: Predicate: All

	// All FArray == FArray
	inline
	friend
	bool
	all_eq( FArray const & a, FArray const & b )
	{
		return eq( a, b );
	}

	// All FArray != FArray
	inline
	friend
	bool
	all_ne( FArray const & a, FArray const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray < FArray
	inline
	friend
	bool
	all_lt( FArray const & a, FArray const & b )
	{
		return lt( a, b );
	}

	// All FArray <= FArray
	inline
	friend
	bool
	all_le( FArray const & a, FArray const & b )
	{
		return le( a, b );
	}

	// All FArray > FArray
	inline
	friend
	bool
	all_gt( FArray const & a, FArray const & b )
	{
		return gt( a, b );
	}

	// All FArray >= FArray
	inline
	friend
	bool
	all_ge( FArray const & a, FArray const & b )
	{
		return ge( a, b );
	}

protected: // Comparison: Count

	// Count FArray == FArray
	inline
	friend
	size_type
	count_eq( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] == b[ i ] ) ++n;
		}
		return n;
	}

	// Count FArray != FArray
	inline
	friend
	size_type
	count_ne( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] != b[ i ] ) ++n;
		}
		return n;
	}

	// Count FArray < FArray
	inline
	friend
	size_type
	count_lt( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < b[ i ] ) ++n;
		}
		return n;
	}

	// Count FArray <= FArray
	inline
	friend
	size_type
	count_le( FArray const & a, FArray const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= b[ i ] ) ++n;
		}
		return n;
	}

	// Count FArray > FArray
	inline
	friend
	size_type
	count_gt( FArray const & a, FArray const & b )
	{
		return lt( b, a );
	}

	// Count FArray >= FArray
	inline
	friend
	size_type
	count_ge( FArray const & a, FArray const & b )
	{
		return le( b, a );
	}

protected: // Comparison: Elemental

	// FArray == FArray
	inline
	friend
	void
	eq_elemental( FArray const & a, FArray const & b, FArray< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] == b[ i ] );
		}
	}

	// FArray != FArray
	inline
	friend
	void
	ne_elemental( FArray const & a, FArray const & b, FArray< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] != b[ i ] );
		}
	}

	// FArray < FArray
	inline
	friend
	void
	lt_elemental( FArray const & a, FArray const & b, FArray< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] < b[ i ] );
		}
	}

	// FArray <= FArray
	inline
	friend
	void
	le_elemental( FArray const & a, FArray const & b, FArray< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] <= b[ i ] );
		}
	}

	// FArray > FArray
	inline
	friend
	void
	gt_elemental( FArray const & a, FArray const & b, FArray< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] > b[ i ] );
		}
	}

	// FArray >= FArray
	inline
	friend
	void
	ge_elemental( FArray const & a, FArray const & b, FArray< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] >= b[ i ] );
		}
	}

	// FArray == Value
	inline
	friend
	void
	eq_elemental( FArray const & a, T const & t, FArray< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] == t );
		}
	}

	// FArray != Value
	inline
	friend
	void
	ne_elemental( FArray const & a, T const & t, FArray< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] != t );
		}
	}

	// FArray < Value
	inline
	friend
	void
	lt_elemental( FArray const & a, T const & t, FArray< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] < t );
		}
	}

	// FArray <= Value
	inline
	friend
	void
	le_elemental( FArray const & a, T const & t, FArray< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] <= t );
		}
	}

	// FArray > Value
	inline
	friend
	void
	gt_elemental( FArray const & a, T const & t, FArray< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] > t );
		}
	}

	// FArray >= Value
	inline
	friend
	void
	ge_elemental( FArray const & a, T const & t, FArray< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] >= t );
		}
	}

	// Value == FArray
	inline
	friend
	void
	eq_elemental( T const & t, FArray const & b, FArray< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t == b[ i ] );
		}
	}

	// Value != FArray
	inline
	friend
	void
	ne_elemental( T const & t, FArray const & b, FArray< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t != b[ i ] );
		}
	}

	// Value < FArray
	inline
	friend
	void
	lt_elemental( T const & t, FArray const & b, FArray< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t < b[ i ] );
		}
	}

	// Value <= FArray
	inline
	friend
	void
	le_elemental( T const & t, FArray const & b, FArray< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t <= b[ i ] );
		}
	}

	// Value > FArray
	inline
	friend
	void
	gt_elemental( T const & t, FArray const & b, FArray< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t > b[ i ] );
		}
	}

	// Value >= FArray
	inline
	friend
	void
	ge_elemental( T const & t, FArray const & b, FArray< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t >= b[ i ] );
		}
	}

protected: // Methods

	// Shift Setup
	inline
	void
	shift_set( difference_type const shift )
	{
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Active Array Size Setup
	inline
	void
	size_set( size_type const size )
	{
		assert( size <= data_size_ );
		size_ = size;
	}

	// Resize a Real Array
	inline
	FArray &
	resize( size_type const size )
	{
		assert( owner_ );
		assert( size != npos );
		if ( ( data_size_ != size ) || ( ! data_ ) ) {
			data_size_ = size;
			delete[] data_; data_ = new T[ data_size_ ]; // Allocate even if size==0 for consistency with Fortran
			size_ = size;
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
			size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
		}
#if defined(OBJEXXFCL_FARRAY_INIT) || defined(OBJEXXFCL_FARRAY_INIT_DEBUG)
		if ( ! initializer_active() ) std::fill_n( data_, size_, Traits::initial_array_value() );
#endif // OBJEXXFCL_FARRAY_INIT || OBJEXXFCL_FARRAY_INIT_DEBUG
		return *this;
	}

	// Attach Proxy/Argument Array to Const Array of Same Rank
	inline
	void
	attach( FArray const & a )
	{
		assert( ! owner_ );
		data_size_ = a.data_size_;
		data_ = a.data_;
		size_ = a.size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = a.shift_;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Array of Same Rank
	inline
	void
	attach( FArray & a )
	{
		assert( ! owner_ );
		data_size_ = a.data_size_;
		data_ = a.data_;
		size_ = a.size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = a.const_proxy_;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = a.shift_;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Const Array
	inline
	void
	attach( FArray const & a, int const shift )
	{
		assert( ! owner_ );
		data_size_ = a.data_size_;
		data_ = a.data_;
		size_ = a.size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Array
	inline
	void
	attach( FArray & a, int const shift )
	{
		assert( ! owner_ );
		data_size_ = a.data_size_;
		data_ = a.data_;
		size_ = a.size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = a.const_proxy_;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Const Tail
	inline
	void
	attach( Tail const & s, int const shift )
	{
		assert( ! owner_ );
		data_size_ = s.size();
		data_ = s.data_;
		size_ = data_size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Tail
	inline
	void
	attach( Tail & s, int const shift )
	{
		assert( ! owner_ );
		data_size_ = s.size();
		data_ = s.data_;
		size_ = data_size_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = s.const_proxy_;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Const Value
	inline
	void
	attach( T const & t, int const shift )
	{
		assert( ! owner_ );
		data_size_ = npos; // Unknown
		data_ = const_cast< T * >( &t );
		size_ = npos; // Unbounded
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Value
	inline
	void
	attach( T & t, int const shift )
	{
		assert( ! owner_ );
		data_size_ = npos; // Unknown
		data_ = &t;
		size_ = npos; // Unbounded
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = false;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Detach Proxy/Argument Array
	inline
	void
	detach()
	{
		assert( ! owner_ );
		data_size_ = 0;
		data_ = nullptr;
		size_ = 0u;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = false;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		shift_ = 0;
		sdata_ = nullptr;
	}

	// Update Proxy Array Attachment to Const Array
	inline
	void
	update_to( FArray const & a )
	{
		assert( ! owner_ );
		data_size_ = a.data_size_;
		data_ = a.data_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = true;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	}

	// Update Proxy Array Attachment to Array
	inline
	void
	update_to( FArray & a )
	{
		assert( ! owner_ );
		data_size_ = a.data_size_;
		data_ = a.data_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		const_proxy_ = a.const_proxy_;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
	}

	// Swap
	inline
	void
	swapB( FArray & v )
	{
		assert( owner_ );
		assert( v.owner_ );
		std::swap( data_size_, v.data_size_ );
		std::swap( data_, v.data_ );
		std::swap( size_, v.size_ );
		std::swap( shift_, v.shift_ );
		std::swap( sdata_, v.sdata_ );
	}

	// Method Discriminator Helper Class
	template< typename U >
	class Has_reassign
	{
		template< typename V, void (V::*)( V const & ) > struct Check;
		template< typename V > static char test( Check< V, &V::reassign > * );
		template< typename V > static int test( ... );
	public:
		enum { value = sizeof( test< T >( 0 ) ) == sizeof( char ) };
	}; // Has_reassign

	// Uniform Reassignment
	template< typename U >
	inline
	void
	reassign( U const & u, typename std::enable_if< Has_reassign< U >::value >::type * = 0 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ].reassign( u );
		}
	}

	// Uniform Reassignment
	template< typename U >
	inline
	void
	reassign( U const & u, typename std::enable_if< ! Has_reassign< U >::value >::type * = 0 )
	{
		operator =( u );
	}

	// Element Reassignment
	template< typename U >
	inline
	void
	reassign( size_type const i, U const & u, typename std::enable_if< Has_reassign< U >::value >::type * = 0 )
	{
		operator []( i ).reassign( u );
	}

	// Element Reassignment
	template< typename U >
	inline
	void
	reassign( size_type const i, U const & u, typename std::enable_if< ! Has_reassign< U >::value >::type * = 0 )
	{
		operator []( i ) = u;
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Const Proxy?
	inline
	bool
	const_proxy() const
	{
		return const_proxy_;
	}

	// Not a Const Proxy Under Strict Const-Correctness?
	inline
	bool
	not_const_proxy() const
	{
		return ! const_proxy_;
	}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

protected: // Static Functions

	// Array Size Product of Specified Bounded Dimensional Sizes
	inline
	static
	size_type
	size_of( size_type const s1 )
	{
		assert( s1 != npos );
		return s1;
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	inline
	static
	size_type
	size_of( size_type const s1, size_type const s2 )
	{
		assert( s1 != npos );
		assert( s2 != npos );
		assert( ( s2 == 0 ) || ( s1 <= max_size / s2 ) );
		return s1 * s2;
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	inline
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3 )
	{
		return size_of( size_of( s1, s2 ), s3 );
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	inline
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3, size_type const s4 )
	{
		return size_of( size_of( s1, s2 ), size_of( s3, s4 ) );
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	inline
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3, size_type const s4, size_type const s5 )
	{
		return size_of( size_of( size_of( s1, s2 ), size_of( s3, s4 ) ), s5 );
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	inline
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3, size_type const s4, size_type const s5, size_type const s6 )
	{
		return size_of( size_of( size_of( s1, s2 ), size_of( s3, s4 ) ), size_of( s5, s6 ) );
	}

	// Array Size Product of Specified Bounded IndexRanges
	inline
	static
	size_type
	size_of( IR const & I1 )
	{
		return size_of( I1.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	inline
	static
	size_type
	size_of( IR const & I1, IR const & I2 )
	{
		return size_of( I1.size(), I2.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	inline
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3 )
	{
		return size_of( I1.size(), I2.size(), I3.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	inline
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		return size_of( I1.size(), I2.size(), I3.size(), I4.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	inline
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		return size_of( I1.size(), I2.size(), I3.size(), I4.size(), I5.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	inline
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 )
	{
		return size_of( I1.size(), I2.size(), I3.size(), I4.size(), I5.size(), I6.size() );
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
	slice_k( IR const & range, int const i, size_type const multiplier )
	{
		assert( range.contains( i ) );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( i ) ) );
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

	// Slice Constant for a Scalar Index
	inline
	static
	std::int64_t
	slice_k( int const u, int const i, size_type const multiplier )
	{
		assert( ( 1 <= i ) && ( i <= u ) );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( i ) ) );
		(void)u; // Suppress unused warning in release builds
		return i * multiplier;
	}

private: // Properties

#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT

	// Report size if at least value defined for OBJEXXFCL_FARRAY_SIZE_REPORT
	//  Size is based on sizeof( T ) so T-controlled heap memory is not counted
	inline
	void
	size_report() const
	{
		if ( size_ * sizeof( T ) >= OBJEXXFCL_FARRAY_SIZE_REPORT ) {
			std::cout << "FArray< " << typeid( T ).name() << " >"
			 << "  Size: " << size_ * sizeof( T ) << "  Elements: " << size_;
		}
	}

#endif // OBJEXXFCL_FARRAY_SIZE_REPORT

public: // Data

	static size_type const npos = static_cast< size_type >( -1 ); // Unbounded "size"

	static size_type const max_size = npos - static_cast< size_type >( 1 ); // Max array size

protected: // Data

	size_type data_size_; // Size of data array

	T * data_; // Pointer to data array

	size_type size_; // Size of active array

	bool const owner_; // Owner of data array?

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	bool const_proxy_; // Proxy for const data array?
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	difference_type shift_; // Array shift

	T * sdata_; // Shifted pointer to data array

}; // FArray

// Static Data Member Template Definitions

template< typename T > typename FArray< T >::size_type const FArray< T >::npos;

template< typename T > typename FArray< T >::size_type const FArray< T >::max_size;

} // ObjexxFCL

#endif // ObjexxFCL_FArray_hh_INCLUDED
