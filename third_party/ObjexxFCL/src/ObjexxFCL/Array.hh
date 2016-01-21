#ifndef ObjexxFCL_Array_hh_INCLUDED
#define ObjexxFCL_Array_hh_INCLUDED

// Array: Array Abstract Base Class
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
#include <ObjexxFCL/Array.fwd.hh>
#include <ObjexxFCL/BArray.hh>
#include <ObjexxFCL/ArrayS.hh>
#include <ObjexxFCL/ArrayTail.hh>
#include <ObjexxFCL/CArray.hh>
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/fmt.hh>
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/IndexSlice.hh>
#include <ObjexxFCL/InitializerSentinel.hh>
#include <ObjexxFCL/MArray.hh>
#include <ObjexxFCL/ProxySentinel.hh>
#include <ObjexxFCL/Sticky.hh>
#include <ObjexxFCL/TypeTraits.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/Vector4.hh>

// C++ Headers
#include <algorithm>
#include <array>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <iomanip>
#include <istream>
#include <iterator>
#include <limits>
#include <ostream>
#include <type_traits>
#include <typeinfo>
#include <utility>
#include <vector>

namespace ObjexxFCL {

// Array: Array Abstract Base Class
//
// Note:
//  Can hold numeric or non-numeric values: Numeric operations on non-numeric values won't compile
//  Any meaningful array index ranges can be specified as in Fortran
//  Zero-sized arrays are supported but have no valid indices
//  Argument/proxy arrays can have unbounded/unknown size
//  For efficiency constructors without initializer function or value do not initialize the array
//  For efficiency bounds checking is only active via asserts in debug builds
template< typename T >
class Array : public BArray
{

private: // Friend

	template< typename > friend class Array;

protected: // Types

	typedef  internal::InitializerSentinel  InitializerSentinel;
	typedef  internal::ProxySentinel  ProxySentinel;

public: // Types

	typedef  Array< T >  Base;
	typedef  ArrayTail< T >  Tail;
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
	typedef  T *  iterator;
	typedef  T const *  const_iterator;
	typedef  std::reverse_iterator< T * >  reverse_iterator;
	typedef  std::reverse_iterator< T const * >  const_reverse_iterator;
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;

	// C++ style
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  T *  Iterator;
	typedef  T const *  ConstIterator;
	typedef  std::reverse_iterator< T * >  ReverseIterator;
	typedef  std::reverse_iterator< T const * >  ConstReverseIterator;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

protected: // Creation

	// Default Constructor
	Array() :
	 capacity_( 0u ),
	 data_( nullptr ),
	 size_( 0u ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// Copy Constructor
	Array( Array const & a ) :
	 BArray( a ),
	 capacity_( size_of( a.size_ ) ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( a.data_ ? new T[ capacity_ ] : nullptr ),
#else
	 data_( a.data_ ? new_array< T >() : nullptr ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( a.shift_ ),
	 sdata_( data_ - shift_ )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// Move Constructor
	Array( Array && a ) NOEXCEPT :
	 BArray( std::move( a ) ),
	 capacity_( a.capacity_ ),
	 data_( a.data_ ),
	 size_( a.size_ ),
	 owner_( a.owner_ ),
	 shift_( a.shift_ ),
	 sdata_( a.sdata_ )
	{
		a.capacity_ = 0u;
		a.data_ = nullptr;
		a.size_ = 0u;
		a.shift_ = 0;
		a.sdata_ = nullptr;
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array( Array< U > const & a ) :
	 capacity_( size_of( a.size() ) ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( a.data_ ? new T[ capacity_ ] : nullptr ),
#else
	 data_( a.data_ ? new_array< T >() : nullptr ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( a.shift_ ),
	 sdata_( data_ - shift_ )
	{
		for ( size_type i = 0; i < size_; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array( ArrayS< U > const & a ) :
	 capacity_( size_of( a.size() ) ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	explicit
	Array( MArray< A, M > const & a ) :
	 capacity_( size_of( a.size() ) ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// Size Constructor
	explicit
	Array( size_type const size ) :
	 capacity_( size_of( size ) ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
		std::fill_n( data_, size_, Traits::initial_array_value() );
#endif // OBJEXXFCL_ARRAY_INIT || OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	// Size + InitializerSentinel Constructor
	Array( size_type const size, InitializerSentinel const & ) :
	 capacity_( size_of( size ) ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array( std::initializer_list< U > const l ) :
	 capacity_( l.size() ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			initialize( i, *r );
		}
	}

	// std::array Constructor Template
	template< typename U, Size s, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array( std::array< U, s > const & a ) :
	 capacity_( s ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		auto ia( a.begin() );
		for ( size_type i = 0; i < size_; ++i, ++ia ) {
			initialize( i, *ia );
		}
	}

	// std::vector Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array( std::vector< U > const & v ) :
	 capacity_( v.size() ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		auto iv( v.begin() );
		for ( size_type i = 0; i < size_; ++i, ++iv ) {
			initialize( i, *iv );
		}
	}

	// Vector2 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array( Vector2< U > const & v ) :
	 capacity_( 2 ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		operator []( 0 ) = v.x;
		operator []( 1 ) = v.y;
	}

	// Vector3 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array( Vector3< U > const & v ) :
	 capacity_( 3 ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		operator []( 0 ) = v.x;
		operator []( 1 ) = v.y;
		operator []( 2 ) = v.z;
	}

	// Vector4 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array( Vector4< U > const & v ) :
	 capacity_( 4 ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		operator []( 0 ) = v.x;
		operator []( 1 ) = v.y;
		operator []( 2 ) = v.z;
		operator []( 3 ) = v.w;
	}

	// Iterator Range Constructor Template
	template< class Iterator, typename = decltype( *std::declval< Iterator & >(), void(), ++std::declval< Iterator & >(), void() ) >
	Array( Iterator const beg, Iterator const end ) :
	 capacity_( end - beg ),
#ifndef OBJEXXFCL_ARRAY_ALIGN
	 data_( new T[ capacity_ ] ),
#else
	 data_( new_array< T >() ),
#endif
	 size_( capacity_ ),
	 owner_( true ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		size_type j( 0u );
		for ( Iterator i = beg; i != end; ++i, ++j ) {
			operator []( j ) = *i;
		}
	}

	// Default Proxy Constructor
	Array( ProxySentinel const & ) :
	 capacity_( 0u ),
	 data_( nullptr ),
	 size_( 0u ),
	 owner_( false ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// Array Proxy Constructor
	Array( Array const & a, ProxySentinel const & ) :
	 capacity_( a.capacity_ ),
	 data_( a.data_ ),
	 size_( a.size_ ),
	 owner_( false ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// Slice Proxy Constructor
	Array( ArrayS< T > const & a, ProxySentinel const & ) :
	 capacity_( a.size() ),
	 data_( a.data_beg_ ),
	 size_( a.size() ),
	 owner_( false ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{
		assert( a.contiguous() );
	}

	// Tail Proxy Constructor
	Array( Tail const & s, ProxySentinel const & ) :
	 capacity_( s.size() ),
	 data_( s.data_ ),
	 size_( capacity_ ),
	 owner_( false ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

	// Value Proxy Constructor
	Array( T const & t, ProxySentinel const & ) :
	 capacity_( npos ), // Unknown
	 data_( const_cast< T * >( &t ) ),
	 size_( npos ), // Unbounded
	 owner_( false ),
	 shift_( 0 ),
	 sdata_( nullptr )
	{}

public: // Creation

	// Destructor
	virtual
	~Array()
	{
#ifndef OBJEXXFCL_ARRAY_ALIGN
		if ( owner_ ) delete[] data_;
#else
		if ( owner_ ) del_array();
#endif
	}

protected: // Assignment: Array

	// Copy Assignment
	void
	operator =( Array const & a )
	{
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

	// Move Assignment
	void
	operator =( Array && a ) NOEXCEPT
	{
		assert( this != &a );
		assert( owner_ == a.owner_ );
#ifndef OBJEXXFCL_ARRAY_ALIGN
		if ( owner_ ) delete[] data_;
#else
		if ( owner_ ) del_array();
#endif
		capacity_ = a.capacity_;
		data_ = a.data_;
		size_ = a.size_;
		shift_ = a.shift_;
		sdata_ = a.sdata_;
		a.capacity_ = 0u;
		a.data_ = nullptr;
		a.size_ = 0u;
		a.shift_ = 0;
		a.sdata_ = nullptr;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = a[ i ];
		}
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		std::copy( l.begin(), l.end(), data_ );
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( std::array< U, s > const & a )
	{
		assert( size_ == s );
		std::copy( a.begin(), a.end(), data_ );
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		std::copy( v.begin(), v.end(), data_ );
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator []( 0 ) = v.x;
		operator []( 1 ) = v.y;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator []( 0 ) = v.x;
		operator []( 1 ) = v.y;
		operator []( 2 ) = v.z;
	}

	// Vector4 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator =( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		operator []( 0 ) = v.x;
		operator []( 1 ) = v.y;
		operator []( 2 ) = v.z;
		operator []( 3 ) = v.w;
	}

	// += Array
	void
	operator +=( Array const & a )
	{
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
	void
	operator -=( Array const & a )
	{
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
	void
	operator *=( Array const & a )
	{
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
	void
	operator /=( Array const & a )
	{
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

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += a[ i ];
		}
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= a[ i ];
		}
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= a[ i ];
		}
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			assert( a[ i ] != T( 0 ) );
			data_[ i ] /= a[ i ];
		}
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			data_[ i ] += *r;
		}
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			data_[ i ] -= *r;
		}
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			data_[ i ] *= *r;
		}
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			assert( *r != T( 0 ) );
			data_[ i ] /= *r;
		}
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += a[ i ];
		}
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= a[ i ];
		}
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= a[ i ];
		}
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		for ( size_type i = 0; i < size_; ++i ) {
			assert( a[ i ] != T( 0 ) );
			data_[ i ] /= a[ i ];
		}
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += v[ i ];
		}
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= v[ i ];
		}
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= v[ i ];
		}
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			assert( v[ i ] != T( 0 ) );
			data_[ i ] /= v[ i ];
		}
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		data_[ 0 ] += v.x;
		data_[ 1 ] += v.y;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		data_[ 0 ] -= v.x;
		data_[ 1 ] -= v.y;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		data_[ 0 ] *= v.x;
		data_[ 1 ] *= v.y;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		data_[ 0 ] /= v.x;
		data_[ 1 ] /= v.y;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		data_[ 0 ] += v.x;
		data_[ 1 ] += v.y;
		data_[ 2 ] += v.z;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		data_[ 0 ] -= v.x;
		data_[ 1 ] -= v.y;
		data_[ 2 ] -= v.z;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		data_[ 0 ] *= v.x;
		data_[ 1 ] *= v.y;
		data_[ 2 ] *= v.z;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		data_[ 0 ] /= v.x;
		data_[ 1 ] /= v.y;
		data_[ 2 ] /= v.z;
	}

	// += Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator +=( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		data_[ 0 ] += v.x;
		data_[ 1 ] += v.y;
		data_[ 2 ] += v.z;
		data_[ 3 ] += v.w;
	}

	// -= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator -=( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		data_[ 0 ] -= v.x;
		data_[ 1 ] -= v.y;
		data_[ 2 ] -= v.z;
		data_[ 3 ] -= v.w;
	}

	// *= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator *=( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		data_[ 0 ] *= v.x;
		data_[ 1 ] *= v.y;
		data_[ 2 ] *= v.z;
		data_[ 3 ] *= v.w;
	}

	// /= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	operator /=( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		assert( v.w != T( 0 ) );
		data_[ 0 ] /= v.x;
		data_[ 1 ] /= v.y;
		data_[ 2 ] /= v.z;
		data_[ 3 ] /= v.w;
	}

public: // Assignment: Value

	// = Value
	Array &
	operator =( T const & t )
	{
		assert( size_bounded() );
		if ( data_ ) std::fill_n( data_, size_, t );
		return *this;
	}

	// += Value
	Array &
	operator +=( T const & t )
	{
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] += t;
		}
		return *this;
	}

	// -= Value
	Array &
	operator -=( T const & t )
	{
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] -= t;
		}
		return *this;
	}

	// *= Value
	Array &
	operator *=( T const & t )
	{
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= t;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	Array &
	operator /=( U const & u )
	{
		assert( size_bounded() );
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] *= inv_u;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	Array &
	operator /=( U const & u )
	{
		assert( size_bounded() );
		assert( u != U( 0 ) );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] /= u;
		}
		return *this;
	}

protected: // Assignment: Logical

	// &&= Array
	void
	and_equals( Array< T > const & a )
	{
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
	void
	or_equals( Array< T > const & a )
	{
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

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] && a[ i ];
		}
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( Array< U > const & a )
	{
		assert( size_bounded() );
		assert( size_ == a.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] || a[ i ];
		}
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			data_[ i ] = data_[ i ] && *r;
		}
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			data_[ i ] = data_[ i ] || *r;
		}
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( std::array< U, s > const & a )
	{
		assert( size_ == s );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] && a[ i ];
		}
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( std::array< U, s > const & a )
	{
		assert( size_ == s );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] || a[ i ];
		}
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] && v[ i ];
		}
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ] = data_[ i ] || v[ i ];
		}
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		data_[ 0 ] = data_[ 0 ] && v.x;
		data_[ 1 ] = data_[ 1 ] && v.y;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		data_[ 0 ] = data_[ 0 ] || v.x;
		data_[ 1 ] = data_[ 1 ] || v.y;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		data_[ 0 ] = data_[ 0 ] && v.x;
		data_[ 1 ] = data_[ 1 ] && v.y;
		data_[ 2 ] = data_[ 2 ] && v.z;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		data_[ 0 ] = data_[ 0 ] || v.x;
		data_[ 1 ] = data_[ 1 ] || v.y;
		data_[ 2 ] = data_[ 2 ] || v.z;
	}

	// &&= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	and_equals( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		data_[ 0 ] = data_[ 0 ] && v.x;
		data_[ 1 ] = data_[ 1 ] && v.y;
		data_[ 2 ] = data_[ 2 ] && v.z;
		data_[ 3 ] = data_[ 3 ] && v.w;
	}

	// ||= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	void
	or_equals( Vector4< U > const & v )
	{
		assert( size_ == 4u );
		data_[ 0 ] = data_[ 0 ] || v.x;
		data_[ 1 ] = data_[ 1 ] || v.y;
		data_[ 2 ] = data_[ 2 ] || v.z;
		data_[ 3 ] = data_[ 3 ] || v.w;
	}

public: // Subscript

	// array[ i ] const: Linear Subscript
	T const &
	operator []( size_type const i ) const
	{
		assert( ( i < size_ ) || ( size_ == npos ) );
		return data_[ i ];
	}

	// array[ i ]: Linear Subscript
	T &
	operator []( size_type const i )
	{
		assert( ( i < size_ ) || ( size_ == npos ) );
		return data_[ i ];
	}

public: // Predicate

	// Initializer Active?
	virtual
	bool
	initializer_active() const = 0;

	// Active?
	bool
	active() const
	{
		return ( data_ != nullptr );
	}

	// Allocated
	bool
	allocated() const
	{
		return ( data_ != nullptr );
	}

	// Contiguous?
	bool
	contiguous() const
	{
		return true;
	}

	// Data Size Bounded?
	bool
	capacity_bounded() const
	{
		return ( capacity_ != npos );
	}

	// Data Size Unbounded?
	bool
	capacity_unbounded() const
	{
		return ( capacity_ == npos );
	}

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
		return ( size_ != npos );
	}

	// Active Array Size Unbounded?
	bool
	size_unbounded() const
	{
		return ( size_ == npos );
	}

	// Owner?
	bool
	owner() const
	{
		return owner_;
	}

	// Proxy?
	bool
	proxy() const
	{
		return ! owner_;
	}

	// All Elements Default Valued?
	bool
	is_default() const
	{
		for ( size_type i = 0; i < size_; ++i ) {
			if ( data_[ i ] != Traits::initial_array_value() ) return false;
		}
		return true;
	}

	// All Elements Zero?
	bool
	is_zero() const
	{
		for ( size_type i = 0; i < size_; ++i ) {
			if ( data_[ i ] != T( 0 ) ) return false;
		}
		return true;
	}

	// Uniform Valued?
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

	// Data Size
	size_type
	capacity() const
	{
		return capacity_;
	}

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
		assert( size_ != npos );
		return static_cast< int >( size_ );
	}

	// IndexRange of a Dimension
	virtual
	IR const &
	I( int const d ) const = 0;

	// Lower Index of a Dimension
	virtual
	int
	l( int const d ) const = 0;

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

	// Begin Iterator
	const_iterator
	begin() const
	{
		return data_;
	}

	// Begin Iterator
	iterator
	begin()
	{
		return data_;
	}

	// End Iterator
	const_iterator
	end() const
	{
		return ( ( data_ != nullptr ) && ( size_ != npos ) ? data_ + size_ : nullptr );
	}

	// End Iterator
	iterator
	end()
	{
		return ( ( data_ != nullptr ) && ( size_ != npos ) ? data_ + size_ : nullptr );
	}

	// Reverse Begin Iterator
	const_reverse_iterator
	rbegin() const
	{
		return const_reverse_iterator( ( data_ != nullptr ) && ( size_ != npos ) ? data_ + size_ : nullptr );
	}

	// Reverse Begin Iterator
	reverse_iterator
	rbegin()
	{
		return reverse_iterator( ( data_ != nullptr ) && ( size_ != npos ) ? data_ + size_ : nullptr );
	}

	// Reverse End Iterator
	const_reverse_iterator
	rend() const
	{
		return const_reverse_iterator( data_ );
	}

	// Reverse End Iterator
	reverse_iterator
	rend()
	{
		return reverse_iterator( data_ );
	}

	// Data Pointer
	T const *
	data() const
	{
		return data_;
	}

	// Data Pointer
	T *
	data()
	{
		return data_;
	}

	// Data Begin Pointer
	T const *
	data_beg() const
	{
		return data_;
	}

	// Data Begin Pointer
	T *
	data_beg()
	{
		return data_;
	}

	// Data End Pointer
	T const *
	data_end() const
	{
		return ( ( data_ != nullptr ) && ( size_ > 0u ) && ( size_ != npos ) ? data_ + size_ - 1 : nullptr );
	}

	// Data End Pointer
	T *
	data_end()
	{
		return ( ( data_ != nullptr ) && ( size_ > 0u ) && ( size_ != npos ) ? data_ + size_ - 1 : nullptr );
	}

public: // Modifier

	// Clear
	virtual
	Array &
	clear()
	{
#ifndef OBJEXXFCL_ARRAY_ALIGN
		if ( owner_ ) delete[] data_;
#else
		if ( owner_ ) del_array();
#endif
		data_ = nullptr;
		capacity_ = 0u;
		size_ = 0u;
		shift_ = 0;
		sdata_ = nullptr;
		return *this;
	}

	// Assign Zero to all Elements
	//  Can't be virtual (for covariant return) or will try to instantiate for all value types
	void
	zero()
	{
		if ( data_ ) std::fill_n( data_, size_, T( 0 ) );
	}

	// Assign Zero to all Elements
	//  Can't be virtual (for covariant return) or will try to instantiate for all value types
	void
	to_zero()
	{
		if ( data_ ) std::fill_n( data_, size_, T( 0 ) );
	}

	// Invert (Elemental)
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
	void
	data_copy_from( U const * source, size_type const size )
	{
		if ( data_ ) std::memcpy( data_, source, std::min( size, size_ ) );
	}

	// Swap Data of Same Size Arrays
	void
	data_swap( Array & v )
	{
		using std::swap;
		assert( owner_ );
		assert( v.owner_ );
		assert( size_ == v.size_ );
		swap( capacity_, v.capacity_ );
		swap( data_, v.data_ );
		swap( shift_, v.shift_ );
		swap( sdata_, v.sdata_ );
	}

public: // Comparison: Predicate

	// Array == Value
	friend
	bool
	eq( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] == t ) ) return false;
		}
		return true;
	}

	// Value == Array
	friend
	bool
	eq( T const & t, Array const & a )
	{
		return eq( a, t );
	}

	// Array != Value
	friend
	bool
	ne( Array const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Value != Array
	friend
	bool
	ne( T const & t, Array const & a )
	{
		return ! eq( t, a );
	}

	// Array < Value
	friend
	bool
	lt( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] < t ) ) return false;
		}
		return true;
	}

	// Value < Array
	friend
	bool
	lt( T const & t, Array const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( t < a[ i ] ) ) return false;
		}
		return true;
	}

	// Array <= Value
	friend
	bool
	le( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] <= t ) ) return false;
		}
		return true;
	}

	// Value <= Array
	friend
	bool
	le( T const & t, Array const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( t <= a[ i ] ) ) return false;
		}
		return true;
	}

	// Array > Value
	friend
	bool
	gt( Array const & a, T const & t )
	{
		return lt( t, a );
	}

	// Value > Array
	friend
	bool
	gt( T const & t, Array const & a )
	{
		return lt( a, t );
	}

	// Array >= Value
	friend
	bool
	ge( Array const & a, T const & t )
	{
		return le( t, a );
	}

	// Value >= Array
	friend
	bool
	ge( T const & t, Array const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any Array == Value
	friend
	bool
	any_eq( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] == t ) return true;
		}
		return false;
	}

	// Any Value == Array
	friend
	bool
	any_eq( T const & t, Array const & a )
	{
		return any_eq( a, t );
	}

	// Any Array != Value
	friend
	bool
	any_ne( Array const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any Value != Array
	friend
	bool
	any_ne( T const & t, Array const & a )
	{
		return ! eq( a, t );
	}

	// Any Array < Value
	friend
	bool
	any_lt( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < t ) return true;
		}
		return false;
	}

	// Any Value < Array
	friend
	bool
	any_lt( T const & t, Array const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( t < a[ i ] ) return true;
		}
		return false;
	}

	// Any Array <= Value
	friend
	bool
	any_le( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= t ) return true;
		}
		return false;
	}

	// Any Value <= Array
	friend
	bool
	any_le( T const & t, Array const & a )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( t <= a[ i ] ) return true;
		}
		return false;
	}

	// Any Array > Value
	friend
	bool
	any_gt( Array const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any Value > Array
	friend
	bool
	any_gt( T const & t, Array const & a )
	{
		return any_lt( a, t );
	}

	// Any Array >= Value
	friend
	bool
	any_ge( Array const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value >= Array
	friend
	bool
	any_ge( T const & t, Array const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All Array == Value
	friend
	bool
	all_eq( Array const & a, T const & t )
	{
		return eq( a, t );
	}

	// All Value == Array
	friend
	bool
	all_eq( T const & t, Array const & a )
	{
		return eq( a, t );
	}

	// All Array != Value
	friend
	bool
	all_ne( Array const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All Value != Array
	friend
	bool
	all_ne( T const & t, Array const & a )
	{
		return ! any_eq( a, t );
	}

	// All Array < Value
	friend
	bool
	all_lt( Array const & a, T const & t )
	{
		return lt( a, t );
	}

	// All Value < Array
	friend
	bool
	all_lt( T const & t, Array const & a )
	{
		return lt( t, a );
	}

	// All Array <= Value
	friend
	bool
	all_le( Array const & a, T const & t )
	{
		return le( a, t );
	}

	// All Value <= Array
	friend
	bool
	all_le( T const & t, Array const & a )
	{
		return le( t, a );
	}

	// All Array > Value
	friend
	bool
	all_gt( Array const & a, T const & t )
	{
		return gt( a, t );
	}

	// All Value > Array
	friend
	bool
	all_gt( T const & t, Array const & a )
	{
		return gt( t, a );
	}

	// All Array >= Value
	friend
	bool
	all_ge( Array const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value >= Array
	friend
	bool
	all_ge( T const & t, Array const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count Array == Value
	friend
	size_type
	count_eq( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] == t ) ++n;
		}
		return n;
	}

	// Count Value == Array
	friend
	size_type
	count_eq( T const & t, Array const & a )
	{
		return count_eq( a, t );
	}

	// Count Array != Value
	friend
	size_type
	count_ne( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] != t ) ++n;
		}
		return n;
	}

	// Count Value != Array
	friend
	size_type
	count_ne( T const & t, Array const & a )
	{
		return count_ne( a, t );
	}

	// Count Array < Value
	friend
	size_type
	count_lt( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < t ) ++n;
		}
		return n;
	}

	// Count Value < Array
	friend
	size_type
	count_lt( T const & t, Array const & a )
	{
		return count_gt( a, t );
	}

	// Count Array <= Value
	friend
	size_type
	count_le( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= t ) ++n;
		}
		return n;
	}

	// Count Value <= Array
	friend
	size_type
	count_le( T const & t, Array const & a )
	{
		return count_ge( a, t );
	}

	// Count Array > Value
	friend
	size_type
	count_gt( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] > t ) ++n;
		}
		return n;
	}

	// Count Value > Array
	friend
	size_type
	count_gt( T const & t, Array const & a )
	{
		return count_lt( a, t );
	}

	// Count Array >= Value
	friend
	size_type
	count_ge( Array const & a, T const & t )
	{
		assert( a.size_bounded() );
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] >= t ) ++n;
		}
		return n;
	}

	// Count Value >= Array
	friend
	size_type
	count_ge( T const & t, Array const & a )
	{
		return count_le( a, t );
	}

protected: // Comparison: Predicate

	// Array == Array
	friend
	bool
	eq( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] == b[ i ] ) ) return false;
		}
		return true;
	}

	// Array != Array
	friend
	bool
	ne( Array const & a, Array const & b )
	{
		return ! eq( a, b );
	}

	// Array < Array
	friend
	bool
	lt( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] < b[ i ] ) ) return false;
		}
		return true;
	}

	// Array <= Array
	friend
	bool
	le( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( ! ( a[ i ] <= b[ i ] ) ) return false;
		}
		return true;
	}

	// Array > Array
	friend
	bool
	gt( Array const & a, Array const & b )
	{
		return lt( b, a );
	}

	// Array >= Array
	friend
	bool
	ge( Array const & a, Array const & b )
	{
		return le( b, a );
	}

protected: // Comparison: Predicate: Any

	// Any Array == Array
	friend
	bool
	any_eq( Array const & a, Array const & b )
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

	// Any Array != Array
	friend
	bool
	any_ne( Array const & a, Array const & b )
	{
		return ! eq( a, b );
	}

	// Any Array < Array
	friend
	bool
	any_lt( Array const & a, Array const & b )
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

	// Any Array <= Array
	friend
	bool
	any_le( Array const & a, Array const & b )
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

	// Any Array > Array
	friend
	bool
	any_gt( Array const & a, Array const & b )
	{
		return any_lt( b, a );
	}

	// Any Array >= Array
	friend
	bool
	any_ge( Array const & a, Array const & b )
	{
		return any_le( b, a );
	}

protected: // Comparison: Predicate: All

	// All Array == Array
	friend
	bool
	all_eq( Array const & a, Array const & b )
	{
		return eq( a, b );
	}

	// All Array != Array
	friend
	bool
	all_ne( Array const & a, Array const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array < Array
	friend
	bool
	all_lt( Array const & a, Array const & b )
	{
		return lt( a, b );
	}

	// All Array <= Array
	friend
	bool
	all_le( Array const & a, Array const & b )
	{
		return le( a, b );
	}

	// All Array > Array
	friend
	bool
	all_gt( Array const & a, Array const & b )
	{
		return gt( a, b );
	}

	// All Array >= Array
	friend
	bool
	all_ge( Array const & a, Array const & b )
	{
		return ge( a, b );
	}

protected: // Comparison: Count

	// Count Array == Array
	friend
	size_type
	count_eq( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] == b[ i ] ) ++n;
		}
		return n;
	}

	// Count Array != Array
	friend
	size_type
	count_ne( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] != b[ i ] ) ++n;
		}
		return n;
	}

	// Count Array < Array
	friend
	size_type
	count_lt( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] < b[ i ] ) ++n;
		}
		return n;
	}

	// Count Array <= Array
	friend
	size_type
	count_le( Array const & a, Array const & b )
	{
		assert( a.size_bounded() );
		assert( a.size_ == b.size_ );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0u );
		for ( size_type i = 0, e = a.size_; i < e; ++i ) {
			if ( a[ i ] <= b[ i ] ) ++n;
		}
		return n;
	}

	// Count Array > Array
	friend
	size_type
	count_gt( Array const & a, Array const & b )
	{
		return lt( b, a );
	}

	// Count Array >= Array
	friend
	size_type
	count_ge( Array const & a, Array const & b )
	{
		return le( b, a );
	}

protected: // Comparison: Elemental

	// Array == Array
	friend
	void
	eq_elemental( Array const & a, Array const & b, Array< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] == b[ i ] );
		}
	}

	// Array != Array
	friend
	void
	ne_elemental( Array const & a, Array const & b, Array< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] != b[ i ] );
		}
	}

	// Array < Array
	friend
	void
	lt_elemental( Array const & a, Array const & b, Array< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] < b[ i ] );
		}
	}

	// Array <= Array
	friend
	void
	le_elemental( Array const & a, Array const & b, Array< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] <= b[ i ] );
		}
	}

	// Array > Array
	friend
	void
	gt_elemental( Array const & a, Array const & b, Array< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] > b[ i ] );
		}
	}

	// Array >= Array
	friend
	void
	ge_elemental( Array const & a, Array const & b, Array< bool > & r )
	{
		assert( a.size() == b.size() );
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] >= b[ i ] );
		}
	}

	// Array == Value
	friend
	void
	eq_elemental( Array const & a, T const & t, Array< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] == t );
		}
	}

	// Array != Value
	friend
	void
	ne_elemental( Array const & a, T const & t, Array< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] != t );
		}
	}

	// Array < Value
	friend
	void
	lt_elemental( Array const & a, T const & t, Array< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] < t );
		}
	}

	// Array <= Value
	friend
	void
	le_elemental( Array const & a, T const & t, Array< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] <= t );
		}
	}

	// Array > Value
	friend
	void
	gt_elemental( Array const & a, T const & t, Array< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] > t );
		}
	}

	// Array >= Value
	friend
	void
	ge_elemental( Array const & a, T const & t, Array< bool > & r )
	{
		assert( a.size() == r.size() );
		for ( size_type i = 0, e = a.size(); i < e; ++i ) {
			r[ i ] = ( a[ i ] >= t );
		}
	}

	// Value == Array
	friend
	void
	eq_elemental( T const & t, Array const & b, Array< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t == b[ i ] );
		}
	}

	// Value != Array
	friend
	void
	ne_elemental( T const & t, Array const & b, Array< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t != b[ i ] );
		}
	}

	// Value < Array
	friend
	void
	lt_elemental( T const & t, Array const & b, Array< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t < b[ i ] );
		}
	}

	// Value <= Array
	friend
	void
	le_elemental( T const & t, Array const & b, Array< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t <= b[ i ] );
		}
	}

	// Value > Array
	friend
	void
	gt_elemental( T const & t, Array const & b, Array< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t > b[ i ] );
		}
	}

	// Value >= Array
	friend
	void
	ge_elemental( T const & t, Array const & b, Array< bool > & r )
	{
		assert( b.size() == r.size() );
		for ( size_type i = 0, e = b.size(); i < e; ++i ) {
			r[ i ] = ( t >= b[ i ] );
		}
	}

protected: // Methods

	// Shift Setup
	void
	shift_set( difference_type const shift )
	{
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Active Array Size Setup
	void
	size_set( size_type const size )
	{
		assert( size <= capacity_ );
		size_ = size;
	}

	// Resize a Real Array
	void
	resize( size_type const size )
	{
		assert( owner_ );
		assert( size != npos );
		if ( ( data_ == nullptr ) || ( capacity_ < size ) || ( ( capacity_ == size_ ) && ( size != size_ ) ) ) {
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
			size_ = capacity_ = size;
			data_ = new T[ capacity_ ]; // Allocate even if size==0 for consistency with Fortran
#else
			del_array();
			size_ = capacity_ = size;
			data_ = new_array< T >(); // Allocate even if size==0 for consistency with Fortran
#endif
		} else {
			size_ = size;
		}
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
		if ( ! initializer_active() ) std::fill_n( data_, size_, Traits::initial_array_value() );
#endif // OBJEXXFCL_ARRAY_INIT || OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	// Reserve Capacity in a Real Array
	void
	reserve_capacity( size_type const n )
	{
		assert( owner_ );
		assert( n != npos );
		if ( capacity_ < n ) {
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ n ];
#else
			T * new_data = new_array< T >( n );
#endif
			if ( size_ > 0u ) move_or_copy( data_, data_ + size_, new_data );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = n;
			data_ = new_data;
			sdata_ = data_ - shift_;
		}
	}

	// Grow Capacity of a Real Array
	void
	grow_capacity( size_type const n = 1u )
	{
		assert( owner_ );
		assert( size_ < npos - n );
		size_type const new_size( size_ + n );
		assert( new_size <= max_size );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			if ( size_ > 0u ) move_or_copy( data_, data_ + size_, new_data );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
		}
		size_ = new_size;
	}

	// Shrink Capacity to Size in a Real Array
	void
	shrink_capacity()
	{
		assert( owner_ );
		if ( capacity_ > size_ ) {
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ size_ ];
#else
			T * new_data = new_array< T >( size_ );
#endif
			if ( size_ > 0u ) move_or_copy( data_, data_ + size_, new_data );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = size_;
			data_ = new_data;
			sdata_ = data_ - shift_;
		}
	}

	// Append Value by Copy in a Real Array
	void
	do_push_back_copy( T const & t )
	{
		assert( owner_ );
		assert( size_ < npos - 1 );
		size_type const new_size( size_ + 1 );
		assert( new_size <= max_size );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			new_data[ size_ ] = t;
			if ( size_ > 0u ) move_or_copy( data_, data_ + size_, new_data );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
		} else {
			data_[ size_ ] = t;
		}
		size_ = new_size;
	}

	// Append Value by Move in a Real Array
	void
	do_push_back_move( T && t )
	{
		assert( owner_ );
		assert( size_ < npos - 1 );
		size_type const new_size( size_ + 1 );
		assert( new_size <= max_size );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			new_data[ size_ ] = std::move( t );
			if ( size_ > 0u ) move_or_copy( data_, data_ + size_, new_data );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
		} else {
			data_[ size_ ] = std::move( t );
		}
		size_ = new_size;
	}

	// Insert Value by Copy in a Real Array
	iterator
	do_insert_copy( const_iterator pos, T const & t )
	{
		assert( owner_ );
		assert( size_ < npos - 1 );
		assert( data_ <= pos );
		assert( pos <= end() );
		size_type const new_size( size_ + 1 );
		assert( new_size <= max_size );
		iterator const old_pos( data_ + ( pos - data_ ) );
		iterator const old_end( end() );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			iterator const new_pos( new_data + ( pos - data_ ) );
			*new_pos = t;
			if ( data_ < pos ) move_or_copy( data_, old_pos, new_data );
			if ( pos < old_end ) move_or_copy( old_pos, old_end, new_pos + 1 );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
			size_ = new_size;
			return new_pos;
		} else {
			if ( pos == old_end ) {
				*old_pos = t;
			} else {
				T const tt( t );
				move_or_copy_backward( old_pos, old_end, old_end + 1 );
				*old_pos = tt;
			}
			size_ = new_size;
			return old_pos;
		}
	}

	// Insert Value by Move in a Real Array
	iterator
	do_insert_move( const_iterator pos, T && t )
	{
		assert( owner_ );
		assert( size_ < npos - 1 );
		assert( data_ <= pos );
		assert( pos <= end() );
		size_type const new_size( size_ + 1 );
		assert( new_size <= max_size );
		iterator const old_pos( data_ + ( pos - data_ ) );
		iterator const old_end( end() );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			iterator const new_pos( new_data + ( pos - data_ ) );
			*new_pos = std::move( t );
			if ( data_ < pos ) move_or_copy( data_, old_pos, new_data );
			if ( pos < old_end ) move_or_copy( old_pos, old_end, new_pos + 1 );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
			size_ = new_size;
			return new_pos;
		} else {
			if ( pos != old_end ) move_or_copy_backward( old_pos, old_end, old_end + 1 );
			*old_pos = std::move( t );
			size_ = new_size;
			return old_pos;
		}
	}

	// Insert Multiples of a Value by Copy in a Real Array
	iterator
	do_insert_n( const_iterator pos, size_type n, T const & t )
	{
		assert( owner_ );
		assert( size_ < npos - n );
		assert( data_ <= pos );
		assert( pos <= end() );
		size_type const new_size( size_ + n );
		assert( new_size <= max_size );
		iterator const old_pos( data_ + ( pos - data_ ) );
		iterator const old_end( end() );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			iterator const new_pos( new_data + ( pos - data_ ) );
			std::fill_n( new_pos, n, t );
			if ( data_ < pos ) move_or_copy( data_, old_pos, new_data );
			if ( pos < old_end ) move_or_copy( old_pos, old_end, new_pos + n );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
			size_ = new_size;
			return new_pos;
		} else {
			if ( pos == old_end ) {
				std::fill_n( old_pos, n, t );
			} else {
				T const tt( t );
				move_or_copy_backward( old_pos, old_end, old_end + n );
				std::fill_n( old_pos, n, tt );
			}
			size_ = new_size;
			return old_pos;
		}
	}

	// Insert Iterator Range in a Real Array
	template< typename Iterator, class = typename std::enable_if<
	 std::is_same< typename std::iterator_traits< Iterator >::iterator_category, std::input_iterator_tag >::value ||
	 std::is_same< typename std::iterator_traits< Iterator >::iterator_category, std::forward_iterator_tag >::value ||
	 std::is_same< typename std::iterator_traits< Iterator >::iterator_category, std::bidirectional_iterator_tag >::value ||
	 std::is_same< typename std::iterator_traits< Iterator >::iterator_category, std::random_access_iterator_tag >::value
	 >::type >
	iterator
	do_insert_iterator( const_iterator pos, Iterator first, Iterator last ) // Like std containers first and last can't be iterators to this Array
	{
		assert( owner_ );
		size_type const n( std::distance( first, last ) );
		assert( size_ < npos - n );
		assert( data_ <= pos );
		assert( pos <= end() );
		size_type const new_size( size_ + n );
		assert( new_size <= max_size );
		iterator const old_pos( data_ + ( pos - data_ ) );
		iterator const old_end( end() );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			iterator const new_pos( new_data + ( pos - data_ ) );
			std::copy( first, last, new_pos );
			if ( data_ < pos ) move_or_copy( data_, old_pos, new_data );
			if ( pos < old_end ) move_or_copy( old_pos, old_end, new_pos + n );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
			size_ = new_size;
			return new_pos;
		} else {
			if ( pos != old_end ) move_or_copy_backward( old_pos, old_end, old_end + n );
			std::copy( first, last, old_pos );
			size_ = new_size;
			return old_pos;
		}
	}

	// Insert Initializer List in a Real Array
	iterator
	do_insert_initializer_list( const_iterator pos, std::initializer_list< T > il )
	{
		assert( owner_ );
		size_type const n( il.size() );
		assert( size_ < npos - n );
		assert( data_ <= pos );
		assert( pos <= end() );
		size_type const new_size( size_ + n );
		assert( new_size <= max_size );
		iterator const old_pos( data_ + ( pos - data_ ) );
		iterator const old_end( end() );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			iterator const new_pos( new_data + ( pos - data_ ) );
			std::copy( il.begin(), il.end(), new_pos );
			if ( data_ < pos ) move_or_copy( data_, old_pos, new_data );
			if ( pos < old_end ) move_or_copy( old_pos, old_end, new_pos + n );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
			size_ = new_size;
			return new_pos;
		} else {
			if ( pos != old_end ) move_or_copy_backward( old_pos, old_end, old_end + n );
			std::copy( il.begin(), il.end(), old_pos );
			size_ = new_size;
			return old_pos;
		}
	}

	// Insert Value Constructed in Place in a Real Array
	template< typename... Args >
	iterator
	do_emplace( const_iterator pos, Args &&... args )
	{
		assert( owner_ );
		assert( size_ < npos - 1 );
		assert( data_ <= pos );
		assert( pos <= end() );
		size_type const new_size( size_ + 1 );
		assert( new_size <= max_size );
		iterator const old_pos( data_ + ( pos - data_ ) );
		iterator const old_end( end() );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			iterator const new_pos( new_data + ( pos - data_ ) );
			*new_pos = T( std::forward< Args >( args )... );
			if ( data_ < pos ) move_or_copy( data_, old_pos, new_data );
			if ( pos < old_end ) move_or_copy( old_pos, old_end, new_pos + 1 );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
			size_ = new_size;
			return new_pos;
		} else {
			if ( pos != old_end ) move_or_copy_backward( old_pos, old_end, old_end + 1 );
			*pos = T( std::forward< Args >( args )... );
			size_ = new_size;
			return old_pos;
		}
	}

	// Append Value Constructed in Place in a Real Array
	template< typename... Args >
	void
	do_emplace_back( Args &&... args )
	{
		assert( owner_ );
		assert( size_ < npos - 1 );
		size_type const new_size( size_ + 1 );
		assert( new_size <= max_size );
		if ( capacity_ < new_size ) {
			size_type const new_capacity( std::min( std::max( capacity_ << 1, new_size ), max_size ) );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			T * new_data = new T[ new_capacity ];
#else
			T * new_data = new_array< T >( new_capacity );
#endif
			new_data[ size_ ] = T( std::forward< Args >( args )... );
			if ( size_ > 0u ) move_or_copy( data_, data_ + size_, new_data );
#ifndef OBJEXXFCL_ARRAY_ALIGN
			delete[] data_;
#else
			del_array();
#endif
			capacity_ = new_capacity;
			data_ = new_data;
			sdata_ = data_ - shift_;
		} else {
			data_[ size_ ] = T( std::forward< Args >( args )... );
		}
		size_ = new_size;
	}

	// Erase Iterator in a Real Array
	iterator
	do_erase( const_iterator pos )
	{
		assert( owner_ );
		assert( data_ <= pos );
		assert( pos < end() );
		iterator const old_pos( data_ + ( pos - data_ ) );
		move_or_copy( old_pos + 1, end(), old_pos );
		--size_;
		return old_pos;
	}

	// Erase Iterator Range in a Real Array
	iterator
	do_erase_iterator( const_iterator first, const_iterator last )
	{
		assert( owner_ );
		assert( ( data_ <= first ) || ( first == last ) );
		assert( ( first <= last ) || ( first == last ) );
		assert( ( last <= end() ) || ( first == last ) );
		size_type const n( std::distance( first, last ) );
		iterator const start( data_ + ( first - data_ ) );
		if ( n > 0u ) {
			iterator const stop( data_ + ( last - data_ ) );
			move_or_copy( stop, end(), start );
			size_ -= n;
		}
		return start;
	}

	// Attach Proxy/Argument Array to Const Array of Same Rank
	void
	attach( Array const & a )
	{
		assert( ! owner_ );
		capacity_ = a.capacity_;
		data_ = a.data_;
		size_ = a.size_;
		shift_ = a.shift_;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Array of Same Rank
	void
	attach( Array & a )
	{
		assert( ! owner_ );
		capacity_ = a.capacity_;
		data_ = a.data_;
		size_ = a.size_;
		shift_ = a.shift_;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Const Array
	template< int shift >
	void
	attach( Array const & a )
	{
		assert( ! owner_ );
		capacity_ = a.capacity_;
		data_ = a.data_;
		size_ = a.size_;
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Array
	template< int shift >
	void
	attach( Array & a )
	{
		assert( ! owner_ );
		capacity_ = a.capacity_;
		data_ = a.data_;
		size_ = a.size_;
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Const Tail
	template< int shift >
	void
	attach( Tail const & s )
	{
		assert( ! owner_ );
		capacity_ = s.size();
		data_ = s.data_;
		size_ = capacity_;
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Tail
	template< int shift >
	void
	attach( Tail & s )
	{
		assert( ! owner_ );
		capacity_ = s.size();
		data_ = s.data_;
		size_ = capacity_;
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Const Value
	template< int shift >
	void
	attach( T const & t )
	{
		assert( ! owner_ );
		capacity_ = npos; // Unknown
		data_ = const_cast< T * >( &t );
		size_ = npos; // Unbounded
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Attach Proxy/Argument Array to Value
	template< int shift >
	void
	attach( T & t )
	{
		assert( ! owner_ );
		capacity_ = npos; // Unknown
		data_ = &t;
		size_ = npos; // Unbounded
		shift_ = shift;
		sdata_ = data_ - shift_;
	}

	// Detach Proxy/Argument Array
	void
	detach()
	{
		assert( ! owner_ );
		capacity_ = 0u;
		data_ = nullptr;
		size_ = 0u;
		shift_ = 0;
		sdata_ = nullptr;
	}

	// Update Proxy Array Attachment to Const Array
	void
	update_to( Array const & a )
	{
		assert( ! owner_ );
		capacity_ = a.capacity_;
		data_ = a.data_;
	}

	// Update Proxy Array Attachment to Array
	void
	update_to( Array & a )
	{
		assert( ! owner_ );
		capacity_ = a.capacity_;
		data_ = a.data_;
	}

	// Swap
	void
	swapB( Array & v )
	{
		assert( owner_ );
		assert( v.owner_ );
		std::swap( capacity_, v.capacity_ );
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
	};

	// Initialize
	template< typename U, class = typename std::enable_if< Has_reassign< U >::value >::type >
	void
	initialize( U const & u )
	{
		assert( size_bounded() );
		for ( size_type i = 0; i < size_; ++i ) {
			data_[ i ].reassign( u );
		}
	}

	// Initialize
	template< typename U, class = typename std::enable_if< ! Has_reassign< U >::value >::type, typename = void >
	void
	initialize( U const & u )
	{
		operator =( u );
	}

	// Initialize an Element
	template< typename U, class = typename std::enable_if< Has_reassign< U >::value >::type >
	void
	initialize( size_type const i, U const & u )
	{
		operator []( i ).reassign( u );
	}

	// Initialize an Element
	template< typename U, class = typename std::enable_if< ! Has_reassign< U >::value >::type, typename = void >
	void
	initialize( size_type const i, U const & u )
	{
		operator []( i ) = u;
	}

	// Switch to Size Construction
	void
	reconstruct_by_size( size_type const size )
	{
		assert( owner_ );
#ifndef OBJEXXFCL_ARRAY_ALIGN
		delete[] data_;
		size_ = capacity_ = size;
		data_ = new T[ capacity_ ];
#else
		del_array();
		size_ = capacity_ = size;
		data_ = new_array< T >();
#endif
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
		initialize( Traits::initial_array_value() );
#endif // OBJEXXFCL_ARRAY_INIT || OBJEXXFCL_ARRAY_INIT_DEBUG
	}

	// Conformable Move
	void
	conformable_move( Array & a )
	{
		assert( this != &a );
		assert( owner_ == a.owner_ );
#ifndef OBJEXXFCL_ARRAY_ALIGN
		if ( owner_ ) delete[] data_;
#else
		if ( owner_ ) del_array();
#endif
		capacity_ = a.capacity_;
		data_ = a.data_;
		sdata_ = data_ - shift_;
		a.capacity_ = 0u;
		a.data_ = nullptr;
		a.size_ = 0u;
		a.shift_ = 0;
		a.sdata_ = nullptr;
	}

protected: // Static Methods

	// Array Size Product of Specified Bounded Dimensional Sizes
	static
	size_type
	size_of( size_type const s1 )
	{
		assert( s1 != npos );
		return s1;
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
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
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3 )
	{
		return size_of( size_of( s1, s2 ), s3 );
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3, size_type const s4 )
	{
		return size_of( size_of( s1, s2 ), size_of( s3, s4 ) );
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3, size_type const s4, size_type const s5 )
	{
		return size_of( size_of( size_of( s1, s2 ), size_of( s3, s4 ) ), s5 );
	}

	// Array Size Product of Specified Bounded Dimensional Sizes
	static
	size_type
	size_of( size_type const s1, size_type const s2, size_type const s3, size_type const s4, size_type const s5, size_type const s6 )
	{
		return size_of( size_of( size_of( s1, s2 ), size_of( s3, s4 ) ), size_of( s5, s6 ) );
	}

	// Array Size Product of Specified Bounded IndexRanges
	static
	size_type
	size_of( IR const & I1 )
	{
		return size_of( I1.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	static
	size_type
	size_of( IR const & I1, IR const & I2 )
	{
		return size_of( I1.size(), I2.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3 )
	{
		return size_of( I1.size(), I2.size(), I3.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		return size_of( I1.size(), I2.size(), I3.size(), I4.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		return size_of( I1.size(), I2.size(), I3.size(), I4.size(), I5.size() );
	}

	// Array Size Product of Specified Bounded IndexRanges
	static
	size_type
	size_of( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, IR const & I6 )
	{
		return size_of( I1.size(), I2.size(), I3.size(), I4.size(), I5.size(), I6.size() );
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
	slice_k( IR const & range, int const i, size_type const multiplier )
	{
		assert( range.contains( i ) );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( i ) ) );
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

	// Slice Constant for a Scalar Index
	static
	std::int64_t
	slice_k( int const u, int const i, size_type const multiplier )
	{
		assert( ( 1 <= i ) && ( i <= u ) );
		assert( multiplier <= size_type( std::numeric_limits< std::int64_t >::max() / std::abs( i ) ) );
		(void)u; // Suppress unused warning in release builds
		return i * multiplier;
	}

	// Move if Movable: Movable Overload
	template< typename U, class = typename std::enable_if< std::is_move_assignable< U >::value >::type >
	static
	U &&
	move_if( U & u )
	{
		return std::move( u );
	}

	// Move if Movable: Non-Movable Overload
	template< typename U, class = typename std::enable_if< ! std::is_move_assignable< U >::value >::type, typename = void >
	static
	U &
	move_if( U & u )
	{
		return u;
	}

	// Move or Copy: Move Overload
	template< typename InputIterator, typename OutputIterator, typename U = T, class = typename std::enable_if< std::is_move_assignable< U >::value >::type >
	static
	OutputIterator
	move_or_copy( InputIterator beg, InputIterator end, OutputIterator out )
	{
		return std::move( beg, end, out );
	}

	// Move or Copy: Copy Overload
	template< typename InputIterator, typename OutputIterator, typename U = T, class = typename std::enable_if< ! std::is_move_assignable< U >::value >::type, typename = void >
	static
	OutputIterator
	move_or_copy( InputIterator beg, InputIterator end, OutputIterator out )
	{
		return std::copy( beg, end, out );
	}

	// Move or Copy Backward: Move Overload
	template< typename InputIterator, typename OutputIterator, typename U = T, class = typename std::enable_if< std::is_move_assignable< U >::value >::type >
	static
	OutputIterator
	move_or_copy_backward( InputIterator beg, InputIterator end, OutputIterator out )
	{
		return std::move_backward( beg, end, out );
	}

	// Move or Copy Backward: Copy Overload
	template< typename InputIterator, typename OutputIterator, typename U = T, class = typename std::enable_if< ! std::is_move_assignable< U >::value >::type, typename = void >
	static
	OutputIterator
	move_or_copy_backward( InputIterator beg, InputIterator end, OutputIterator out )
	{
		return std::copy_backward( beg, end, out );
	}

private: // Methods

	// Array Heap Allocator for POD Types
	template< typename U, class = typename std::enable_if< std::is_fundamental< U >::value >::type >
	T *
	new_array()
	{
#ifndef OBJEXXFCL_ARRAY_ALIGN
		return new T[ capacity_ ];
#else
#if defined(_WIN32)
		return static_cast< T * >( _aligned_malloc( capacity_ * sizeof( T ), OBJEXXFCL_ARRAY_ALIGN ) );
#elif defined(__linux__)
		void * p;
		return ( posix_memalign( &p, OBJEXXFCL_ARRAY_ALIGN, capacity_ * sizeof( T ) ) == 0 ? static_cast< T * >( p ) : nullptr );
#else
		return new T[ capacity_ ];
#endif
#endif
	}

	// Array Heap Allocator for Class Types
	template< typename U, class = typename std::enable_if< ! std::is_fundamental< U >::value >::type, typename = void >
	T *
	new_array()
	{
#ifndef OBJEXXFCL_ARRAY_ALIGN
		return new T[ capacity_ ];
#else
#if defined(_WIN32)
		T * pT( static_cast< T * >( _aligned_malloc( capacity_ * sizeof( T ), OBJEXXFCL_ARRAY_ALIGN ) ) );
		for ( size_t i = 0; i < capacity_; ++i ) {
			pT[ i ] = *( new( pT + i ) T() );
		}
		return pT;
#elif defined(__linux__)
		void * p;
		int const status( posix_memalign( &p, OBJEXXFCL_ARRAY_ALIGN, capacity_ * sizeof( T ) ) );
		if ( status == 0 ) { // Success
			T * pT( static_cast< T * >( p ) );
			for ( size_t i = 0; i < capacity_; ++i ) {
				pT[ i ] = *( new( pT + i ) T() );
			}
			return pT;
		} else {
			return nullptr;
		}
#else
		return new T[ capacity_ ];
#endif
#endif
	}

	// Array Heap Allocator for POD Types
	template< typename U, class = typename std::enable_if< std::is_fundamental< U >::value >::type >
	T *
	new_array( size_type const n )
	{
#ifndef OBJEXXFCL_ARRAY_ALIGN
		return new T[ n ];
#else
#if defined(_WIN32)
		return static_cast< T * >( _aligned_malloc( n * sizeof( T ), OBJEXXFCL_ARRAY_ALIGN ) );
#elif defined(__linux__)
		void * p;
		return ( posix_memalign( &p, OBJEXXFCL_ARRAY_ALIGN, n * sizeof( T ) ) == 0 ? static_cast< T * >( p ) : nullptr );
#else
		return new T[ n ];
#endif
#endif
	}

	// Array Heap Allocator for Class Types
	template< typename U, class = typename std::enable_if< ! std::is_fundamental< U >::value >::type, typename = void >
	T *
	new_array( size_type const n )
	{
#ifndef OBJEXXFCL_ARRAY_ALIGN
		return new T[ n ];
#else
#if defined(_WIN32)
		T * pT( static_cast< T * >( _aligned_malloc( n * sizeof( T ), OBJEXXFCL_ARRAY_ALIGN ) ) );
		for ( size_t i = 0; i < n; ++i ) {
			pT[ i ] = *( new( pT + i ) T() );
		}
		return pT;
#elif defined(__linux__)
		void * p;
		int const status( posix_memalign( &p, OBJEXXFCL_ARRAY_ALIGN, n * sizeof( T ) ) );
		if ( status == 0 ) { // Success
			T * pT( static_cast< T * >( p ) );
			for ( size_t i = 0; i < n; ++i ) {
				pT[ i ] = *( new( pT + i ) T() );
			}
			return pT;
		} else {
			return nullptr;
		}
#else
		return new T[ n ];
#endif
#endif
	}

	// Array Heap Destructor
	void
	del_array()
	{
		assert( owner_ );
#ifndef OBJEXXFCL_ARRAY_ALIGN
		delete[] data_;
#else
#if defined(_WIN32)
		size_type i( capacity_ );
		while ( i ) data_[ --i ].~T();
		_aligned_free( data_ );
#elif defined(__linux__)
		size_type i( capacity_ );
		while ( i ) data_[ --i ].~T();
		free( data_ );
#else
		delete[] data_;
#endif
#endif
	}

public: // Static Data

	static size_type const npos; // Unbounded "size"
	static size_type const max_size; // Max array size

protected: // Data

	size_type capacity_; // Size of data array
	T * data_; // Pointer to data array
	size_type size_; // Size of active array
	bool const owner_; // Owner of data array?
	difference_type shift_; // Array shift
	T * sdata_; // Shifted pointer to data array

}; // Array

// Static Data Member Template Definitions
template< typename T > typename Array< T >::size_type const Array< T >::npos = static_cast< size_type >( -1 );
template< typename T > typename Array< T >::size_type const Array< T >::max_size = npos - static_cast< size_type >( 1 );

// Stream >> Array
template< typename T >
inline
std::istream &
operator >>( std::istream & stream, Array< T > & a )
{
	if ( stream && ( a.size() > 0u ) ) {
		for ( typename Array< T >::size_type i = 0, e = a.size(); i < e; ++i ) {
			stream >> a[ i ];
			if ( ! stream ) break;
		}
	}
	return stream;
}

// Stream << Array
template< typename T >
inline
std::ostream &
operator <<( std::ostream & stream, Array< T > const & a )
{
	typedef  TypeTraits< T >  Traits;
	if ( stream && ( a.size() > 0u ) ) {
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision ) );
		stream << std::right << std::showpoint << std::uppercase;
		int const w( Traits::iwidth );
		for ( typename Array< T >::size_type i = 0, e = a.size() - 1; i < e; ++i ) {
			stream << std::setw( w ) << a[ i ] << ' ';
			if ( ! stream ) break;
		} if ( stream ) stream << std::setw( w ) << a[ a.size() - 1 ];
		stream.precision( old_precision );
		stream.flags( old_flags );
	}
	return stream;
}

// Read an Array from a Binary File
template< typename T >
inline
std::istream &
read_binary( std::istream & stream, Array< T > & a )
{
	std::size_t const n( a.size() );
	if ( stream && ( n > 0u ) ) {
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( std::size_t i = 0; i < n; ++i ) {
			stream.read( ( std::istream::char_type * )&a[ i ], type_size );
			if ( ! stream ) break;
		}
	}
	return stream;
}

// Write an Array to a Binary File
template< typename T >
inline
std::ostream &
write_binary( std::ostream & stream, Array< T > const & a )
{
	std::size_t const n( a.size() );
	if ( stream && ( n > 0u ) ) {
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( std::size_t i = 0; i < n; ++i ) {
			stream.write( ( std::ostream::char_type const * )&a[ i ], type_size );
			if ( ! stream ) break;
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: Array
template< typename T >
inline
std::string
LD( Array< T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width );
		for ( std::size_t i = 0; i < n; ++i ) {
			s.append( fmt::LD( a[ i ] ) );
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_Array_hh_INCLUDED
