#ifndef ObjexxFCL_Array1_hh_INCLUDED
#define ObjexxFCL_Array1_hh_INCLUDED

// Array1: 1D Array Abstract Base Class
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
#include <ObjexxFCL/Array1.fwd.hh>
#include <ObjexxFCL/Array.hh>
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/MArray1.hh>

// C++ Headers
#include <cmath>

namespace ObjexxFCL {

// Forward
template< typename > class Array1D;
template< typename > class Array1A;

// Array1: 1D Array Abstract Base Class
template< typename T >
class Array1 : public Array< T >
{

private: // Types

	typedef  Array< T >  Super;

private: // Friend

	template< typename > friend class Array1;
	template< typename > friend class Array1D;
	template< typename > friend class Array1A;

protected: // Types

	typedef  internal::InitializerSentinel  InitializerSentinel;
	typedef  internal::ProxySentinel  ProxySentinel;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Tail  Tail;
	typedef  typename Super::IR  IR;
	typedef  typename Super::IS  IS;
	typedef  typename Super::DS  DS;

	// STL Style
	typedef  typename Super::value_type  value_type;
	typedef  typename Super::reference  reference;
	typedef  typename Super::const_reference  const_reference;
	typedef  typename Super::pointer  pointer;
	typedef  typename Super::const_pointer  const_pointer;
	typedef  typename Super::iterator  iterator;
	typedef  typename Super::const_iterator  const_iterator;
	typedef  typename Super::reverse_iterator  reverse_iterator;
	typedef  typename Super::const_reverse_iterator  const_reverse_iterator;
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Iterator  Iterator;
	typedef  typename Super::ConstIterator  ConstIterator;
	typedef  typename Super::ReverseIterator  ReverseIterator;
	typedef  typename Super::ConstReverseIterator  ConstReverseIterator;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	using Super::isize;
	using Super::npos;
	using Super::overlap;
	using Super::size;
	using Super::size_of;
	using Super::slice_k;
	using Super::swapB;
	using Super::capacity_;
	using Super::data_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;

	// Types to prevent compile failure when std::distance is in scope
	typedef  void  iterator_category;

protected: // Creation

	// Default Constructor
	inline
	Array1()
	{}

	// Copy Constructor
	inline
	Array1( Array1 const & a ) :
	 Super( a ),
	 I_( a.I_ )
	{}

	// Move Constructor
	inline
	Array1( Array1 && a ) NOEXCEPT :
	 Super( std::move( a ) ),
	 I_( a.I_ )
	{
		a.clear_move();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array1( Array1< U > const & a ) :
	 Super( a ),
	 I_( a.I_ )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array1( Array1S< U > const & a ) :
	 Super( a ),
	 I_( a.u() )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	Array1( MArray1< A, M > const & a ) :
	 Super( a ),
	 I_( a.u() )
	{}

	// IndexRange Constructor
	inline
	explicit
	Array1( IR const & I ) :
	 Super( size_of( I ) ),
	 I_( I )
	{}

	// IndexRange + InitializerSentinel Constructor
	inline
	Array1( IR const & I, InitializerSentinel const & initialized ) :
	 Super( size_of( I ), initialized ),
	 I_( I )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1( std::initializer_list< U > const l ) :
	 Super( l ),
	 I_( static_cast< int >( l.size() ) )
	{}

	// Index Range + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1( IR const & I, std::initializer_list< U > const l ) :
	 Super( l ),
	 I_( I )
	{
		assert( size_of( I ) == l.size() );
	}

	// std::array Constructor Template
	template< typename U, Size s, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1( std::array< U, s > const & a ) :
	 Super( a ),
	 I_( static_cast< int >( s ) )
	{}

	// std::vector Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1( std::vector< U > const & v ) :
	 Super( v ),
	 I_( static_cast< int >( v.size() ) )
	{}

	// Vector2 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1( Vector2< U > const & v ) :
	 Super( v ),
	 I_( 2 )
	{}

	// Vector3 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1( Vector3< U > const & v ) :
	 Super( v ),
	 I_( 3 )
	{}

	// Default Proxy Constructor
	inline
	Array1( ProxySentinel const & proxy ) :
	 Super( proxy )
	{}

	// Copy Proxy Constructor
	inline
	Array1( Array1 const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I_( a.I_ )
	{}

	// Slice Proxy Constructor
	inline
	Array1( Array1S< T > const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I_( a.u() )
	{}

	// Base Proxy Constructor
	inline
	Array1( Base const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I_( a.isize() )
	{}

	// Tail Proxy Constructor
	inline
	Array1( Tail const & s, ProxySentinel const & proxy ) :
	 Super( s, proxy ),
	 I_( s.isize() )
	{}

	// Value Proxy Constructor
	inline
	Array1( T const & t, ProxySentinel const & proxy ) :
	 Super( t, proxy ),
	 I_( _ )
	{}

	// Copy + IndexRange Proxy Constructor
	inline
	Array1( Array1 const & a, IR const & I, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I_( I )
	{}

	// Slice + IndexRange Proxy Constructor
	inline
	Array1( Array1S< T > const & a, IR const & I, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I_( I )
	{}

	// Base + IndexRange Proxy Constructor
	inline
	Array1( Base const & a, IR const & I, ProxySentinel const & proxy ) :
	 Super( a, proxy ),
	 I_( I )
	{}

	// Tail + IndexRange Proxy Constructor
	inline
	Array1( Tail const & s, IR const & I, ProxySentinel const & proxy ) :
	 Super( s, proxy ),
	 I_( I )
	{}

	// Value + IndexRange Proxy Constructor
	inline
	Array1( T const & t, IR const & I, ProxySentinel const & proxy ) :
	 Super( t, proxy ),
	 I_( I )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~Array1()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	Array1 &
	operator =( Array1 const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension_assign( a.I_ );
			Super::operator =( a );
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( Array1< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I_ );
		Super::operator =( a );
		return *this;
	}

	// Slice Assignment
	inline
	Array1 &
	operator =( Array1S< T > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I() );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] = a( i );
			}
		}
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( Array1S< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I() );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] = a( i );
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	Array1 &
	operator =( MArray1< A, M > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I() );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] = a( i );
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( std::array< U, s > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( std::vector< U > const & v )
	{
		Super::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( Vector2< U > const & v )
	{
		Super::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator =( Vector3< U > const & v )
	{
		Super::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( Array1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( Array1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( Array1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( Array1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator /=( a );
		return *this;
	}

	// += Slice
	inline
	Array1 &
	operator +=( Array1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] += c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] += a( i );
			}
		}
		return *this;
	}

	// -= Slice
	inline
	Array1 &
	operator -=( Array1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] -= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] -= a( i );
			}
		}
		return *this;
	}

	// *= Slice
	inline
	Array1 &
	operator *=( Array1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] *= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] *= a( i );
			}
		}
		return *this;
	}

	// /= Slice
	inline
	Array1 &
	operator /=( Array1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				assert( a( i ) != T( 0 ) );
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] /= c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				assert( a( i ) != T( 0 ) );
				data_[ l ] /= a( i );
			}
		}
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( Array1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] += a( i );
		}
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( Array1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] -= a( i );
		}
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( Array1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] *= a( i );
		}
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( Array1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			assert( a( i ) != T( 0 ) );
			data_[ l ] /= a( i );
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	Array1 &
	operator +=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] += a( i );
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	Array1 &
	operator -=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] -= a( i );
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	Array1 &
	operator *=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] *= a( i );
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	Array1 &
	operator /=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			assert( a( i ) != T( 0 ) );
			data_[ l ] /= a( i );
		}
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( std::initializer_list< U > const l )
	{
		Super::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( std::initializer_list< U > const l )
	{
		Super::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( std::initializer_list< U > const l )
	{
		Super::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( std::initializer_list< U > const l )
	{
		Super::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( std::array< U, s > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( std::array< U, s > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( std::array< U, s > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( std::array< U, s > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( std::vector< U > const & v )
	{
		Super::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( std::vector< U > const & v )
	{
		Super::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( std::vector< U > const & v )
	{
		Super::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( std::vector< U > const & v )
	{
		Super::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( Vector2< U > const & v )
	{
		Super::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( Vector2< U > const & v )
	{
		Super::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( Vector2< U > const & v )
	{
		Super::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( Vector2< U > const & v )
	{
		Super::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator +=( Vector3< U > const & v )
	{
		Super::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator -=( Vector3< U > const & v )
	{
		Super::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator *=( Vector3< U > const & v )
	{
		Super::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	operator /=( Vector3< U > const & v )
	{
		Super::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( Array1< U > const & a )
	{
		assert( conformable( a ) );
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( Array1< U > const & a )
	{
		assert( conformable( a ) );
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice
	inline
	Array1 &
	and_equals( Array1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] && c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] = data_[ l ] && a( i );
			}
		}
		return *this;
	}

	// ||= Slice
	inline
	Array1 &
	or_equals( Array1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				c[ l ] = a( i );
			}
			for ( size_type i = 0; i < c.size(); ++i ) {
				data_[ i ] = data_[ i ] || c[ i ];
			}
		} else { // Not overlap-safe
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] = data_[ l ] || a( i );
			}
		}
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( Array1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] = data_[ l ] && a( i );
		}
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( Array1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] = data_[ l ] || a( i );
		}
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	Array1 &
	and_equals( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] = data_[ l ] && a( i );
		}
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	Array1 &
	or_equals( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			data_[ l ] = data_[ l ] || a( i );
		}
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1 &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	Array1 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}

	// += Value
	inline
	Array1 &
	operator +=( T const & t )
	{
		Super::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	Array1 &
	operator -=( T const & t )
	{
		Super::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	Array1 &
	operator *=( T const & t )
	{
		Super::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	Array1 &
	operator /=( T const & t )
	{
		Super::operator /=( t );
		return *this;
	}

public: // Subscript

	// array( i ) const
	inline
	T const &
	operator ()( int const i ) const
	{
		assert( contains( i ) );
		return sdata_[ i ];
	}

	// array( i )
	inline
	T &
	operator ()( int const i )
	{
		assert( contains( i ) );
		return sdata_[ i ];
	}

	// Linear Index
	inline
	size_type
	index( int const i ) const
	{
		return i - shift_;
	}

	// Const Tail Starting at array( i )
	inline
	Tail const
	a( int const i ) const
	{
		assert( contains( i ) );
		return Tail( static_cast< T const * >( sdata_ + i ), ( size_ != npos ? size_ - ( i - shift_ ) : npos ) );
	}

	// Tail Starting at array( i )
	inline
	Tail
	a( int const i )
	{
		assert( contains( i ) );
		return Tail( sdata_ + i, ( size_ != npos ? size_ - ( i - shift_ ) : npos ) );
	}

public: // Slice Proxy Generators

	// array( s ) const
	inline
	Array1S< T >
	operator ()( IS const & s ) const
	{
		DS const d( I_, s );
		return Array1S< T >( data_, -shift_, d );
	}

	// array( s )
	inline
	Array1S< T >
	operator ()( IS const & s )
	{
		DS const d( I_, s );
		return Array1S< T >( data_, -shift_, d );
	}

#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around

	// array( {s} ) const
	inline
	Array1S< T >
	operator ()( std::initializer_list< int > const l ) const
	{
		IS const s( l );
		DS const d( I_, s );
		return Array1S< T >( data_, -shift_, d );
	}

	// array( {s} )
	inline
	Array1S< T >
	operator ()( std::initializer_list< int > const l )
	{
		IS const s( l );
		DS const d( I_, s );
		return Array1S< T >( data_, -shift_, d );
	}

#else

	// array( {s} ) const
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	Array1S< T >
	operator ()( std::initializer_list< U > const l ) const
	{
		IS const s( l );
		DS const d( I_, s );
		return Array1S< T >( data_, -shift_, d );
	}

	// array( {s} )
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	Array1S< T >
	operator ()( std::initializer_list< U > const l )
	{
		IS const s( l );
		DS const d( I_, s );
		return Array1S< T >( data_, -shift_, d );
	}

#endif

public: // Predicate

	// Contains Indexed Element?
	inline
	bool
	contains( int const i ) const
	{
		return I_.contains( i );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( Array1< U > const & a ) const
	{
		return ( size_ == a.size() );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( Array1S< U > const & a ) const
	{
		return ( size_ == a.size() );
	}

	// Conformable?
	template< class A, typename M >
	inline
	bool
	conformable( MArray1< A, M > const & a ) const
	{
		return ( size_ == a.size() );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( Array1< U > const & a ) const
	{
		return ( I_ == a.I_ );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( Array1S< U > const & a ) const
	{
		return ( ( l() == 1 ) && ( u() == a.u() ) );
	}

	// Equal Dimensions?
	template< class A, typename M >
	inline
	bool
	equal_dimensions( MArray1< A, M > const & a ) const
	{
		return ( ( l() == 1 ) && ( u() == a.u() ) );
	}

public: // Inspector

	// Rank
	inline
	int
	rank() const
	{
		return 1;
	}

	// IndexRange of a Dimension
	inline
	IR const &
	I( int const d ) const
	{
		switch ( d ) {
		case 1:
			return I_;
		default:
			assert( false );
			return I_;
		}
	}

	// Lower Index of a Dimension
	inline
	int
	l( int const d ) const
	{
		switch ( d ) {
		case 1:
			return l1();
		default:
			assert( false );
			return l1();
		}
	}

	// Upper Index of a Dimension
	inline
	int
	u( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u1();
		default:
			assert( false );
			return u1();
		}
	}

	// Size of a Dimension
	inline
	size_type
	size( int const d ) const
	{
		switch ( d ) {
		case 1:
			return size1();
		default:
			assert( false );
			return size1();
		}
	}

	// Size of a Dimension
	inline
	int
	isize( int const d ) const
	{
		switch ( d ) {
		case 1:
			return isize1();
		default:
			assert( false );
			return isize1();
		}
	}

	// IndexRange
	inline
	IR const &
	I() const
	{
		return I_;
	}

	// Lower Index
	inline
	int
	l() const
	{
		return I_.l();
	}

	// Upper Index
	inline
	int
	u() const
	{
		return I_.u();
	}

	// IndexRange of Dimension 1
	inline
	IR const &
	I1() const
	{
		return I_;
	}

	// Lower Index of Dimension 1
	inline
	int
	l1() const
	{
		return I_.l();
	}

	// Upper Index of Dimension 1
	inline
	int
	u1() const
	{
		return I_.u();
	}

	// Size of Dimension 1
	inline
	size_type
	size1() const
	{
		return I_.size();
	}

	// Size of Dimension 1
	inline
	int
	isize1() const
	{
		return I_.isize();
	}

	// Length
	inline
	T
	length() const
	{
		T length_sq( T( 0 ) );
		for ( int i = l(), e = u(); i <= e; ++i ) {
			T const length_i( sdata_[ i ] );
			length_sq += length_i * length_i;
		}
		return std::sqrt( length_sq );
	}

	// Length Squared
	inline
	T
	length_squared() const
	{
		T length_sq( T( 0 ) );
		for ( int i = l(), e = u(); i <= e; ++i ) {
			T const length_i( sdata_[ i ] );
			length_sq += length_i * length_i;
		}
		return length_sq;
	}

public: // Modifier

	// Clear
	inline
	Array1 &
	clear()
	{
		Super::clear();
		I_.clear();
		return *this;
	}

	// Assign Default Value to all Elements
	inline
	Array1 &
	to_default()
	{
		Super::to_default();
		return *this;
	}

	// Normalize to Unit Length
	inline
	Array1 &
	normalize()
	{
		T const length_( length() );
		assert( length_ > T( 0 ) );
		operator /=( length_ );
		return *this;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray1< Array1 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray1< Array1 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray1< Array1, M >
	ma( M ClassT::* pmem )
	{
		return MArray1< Array1, M >( *this, pmem );
	}

#include <ObjexxFCL/Array1.Project.MArray.hh> // Inject project-specific MArray generators

public: // Comparison: Predicate

	// Array1 == Array1
	friend
	inline
	bool
	eq( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 != Array1
	friend
	inline
	bool
	ne( Array1 const & a, Array1 const & b )
	{
		return ! eq( a, b );
	}

	// Array1 < Array1
	friend
	inline
	bool
	lt( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 <= Array1
	friend
	inline
	bool
	le( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 > Array1
	friend
	inline
	bool
	gt( Array1 const & a, Array1 const & b )
	{
		return lt( b, a );
	}

	// Array1 >= Array1
	friend
	inline
	bool
	ge( Array1 const & a, Array1 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any

	// Array1 == Array1
	friend
	inline
	bool
	any_eq( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 != Array1
	friend
	inline
	bool
	any_ne( Array1 const & a, Array1 const & b )
	{
		return ! eq( a, b );
	}

	// Array1 < Array1
	friend
	inline
	bool
	any_lt( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 <= Array1
	friend
	inline
	bool
	any_le( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 > Array1
	friend
	inline
	bool
	any_gt( Array1 const & a, Array1 const & b )
	{
		return any_lt( b, a );
	}

	// Array1 >= Array1
	friend
	inline
	bool
	any_ge( Array1 const & a, Array1 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All

	// Array1 == Array1
	friend
	inline
	bool
	all_eq( Array1 const & a, Array1 const & b )
	{
		return eq( a, b );
	}

	// Array1 != Array1
	friend
	inline
	bool
	all_ne( Array1 const & a, Array1 const & b )
	{
		return ! any_eq( a, b );
	}

	// Array1 < Array1
	friend
	inline
	bool
	all_lt( Array1 const & a, Array1 const & b )
	{
		return lt( a, b );
	}

	// Array1 <= Array1
	friend
	inline
	bool
	all_le( Array1 const & a, Array1 const & b )
	{
		return le( a, b );
	}

	// Array1 > Array1
	friend
	inline
	bool
	all_gt( Array1 const & a, Array1 const & b )
	{
		return gt( a, b );
	}

	// Array1 >= Array1
	friend
	inline
	bool
	all_ge( Array1 const & a, Array1 const & b )
	{
		return ge( a, b );
	}

public: // Comparison: Count

	// Array1 == Array1
	friend
	inline
	bool
	count_eq( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 != Array1
	friend
	inline
	bool
	count_ne( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ne( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 < Array1
	friend
	inline
	bool
	count_lt( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 <= Array1
	friend
	inline
	bool
	count_le( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 > Array1
	friend
	inline
	bool
	count_gt( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_gt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// Array1 >= Array1
	friend
	inline
	bool
	count_ge( Array1 const & a, Array1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ge( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

public: // Comparison: Predicate: Slice

	// Array1 == Array1S
	friend
	inline
	bool
	eq( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] == b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 != Array1S
	friend
	inline
	bool
	ne( Array1 const & a, Array1S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Array1 < Array1S
	friend
	inline
	bool
	lt( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] < b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 <= Array1S
	friend
	inline
	bool
	le( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] <= b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 > Array1S
	friend
	inline
	bool
	gt( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] > b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 >= Array1S
	friend
	inline
	bool
	ge( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] >= b( i ) ) ) return false;
		}
		return true;
	}

	// Array1S == Array1
	friend
	inline
	bool
	eq( Array1S< T > const & a, Array1 const & b )
	{
		return eq( b, a );
	}

	// Array1S != Array1
	friend
	inline
	bool
	ne( Array1S< T > const & a, Array1 const & b )
	{
		return ne( b, a );
	}

	// Array1S < Array1
	friend
	inline
	bool
	lt( Array1S< T > const & a, Array1 const & b )
	{
		return gt( b, a );
	}

	// Array1S <= Array1
	friend
	inline
	bool
	le( Array1S< T > const & a, Array1 const & b )
	{
		return ge( b, a );
	}

	// Array1S > Array1
	friend
	inline
	bool
	gt( Array1S< T > const & a, Array1 const & b )
	{
		return lt( b, a );
	}

	// Array1S >= Array1
	friend
	inline
	bool
	ge( Array1S< T > const & a, Array1 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: Slice

	// Any Array1 == Array1S
	friend
	inline
	bool
	any_eq( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] == b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 != Array1S
	friend
	inline
	bool
	any_ne( Array1 const & a, Array1S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Any Array1 < Array1S
	friend
	inline
	bool
	any_lt( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] < b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 <= Array1S
	friend
	inline
	bool
	any_le( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] <= b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 > Array1S
	friend
	inline
	bool
	any_gt( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] > b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 >= Array1S
	friend
	inline
	bool
	any_ge( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] >= b( i ) ) return true;
		}
		return false;
	}

	// Any Array1S == Array1
	friend
	inline
	bool
	any_eq( Array1S< T > const & a, Array1 const & b )
	{
		return any_eq( b, a );
	}

	// Any Array1S != Array1
	friend
	inline
	bool
	any_ne( Array1S< T > const & a, Array1 const & b )
	{
		return any_ne( b, a );
	}

	// Any Array1S < Array1
	friend
	inline
	bool
	any_lt( Array1S< T > const & a, Array1 const & b )
	{
		return any_gt( b, a );
	}

	// Any Array1S <= Array1
	friend
	inline
	bool
	any_le( Array1S< T > const & a, Array1 const & b )
	{
		return any_ge( b, a );
	}

	// Any Array1S > Array1
	friend
	inline
	bool
	any_gt( Array1S< T > const & a, Array1 const & b )
	{
		return any_lt( b, a );
	}

	// Any Array1S >= Array1
	friend
	inline
	bool
	any_ge( Array1S< T > const & a, Array1 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: Slice

	// All Array1 == Array1S
	friend
	inline
	bool
	all_eq( Array1 const & a, Array1S< T > const & b )
	{
		return eq( a, b );
	}

	// All Array1 != Array1S
	friend
	inline
	bool
	all_ne( Array1 const & a, Array1S< T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array1 < Array1S
	friend
	inline
	bool
	all_lt( Array1 const & a, Array1S< T > const & b )
	{
		return lt( a, b );
	}

	// All Array1 <= Array1S
	friend
	inline
	bool
	all_le( Array1 const & a, Array1S< T > const & b )
	{
		return le( a, b );
	}

	// All Array1 > Array1S
	friend
	inline
	bool
	all_gt( Array1 const & a, Array1S< T > const & b )
	{
		return gt( a, b );
	}

	// All Array1 >= Array1S
	friend
	inline
	bool
	all_ge( Array1 const & a, Array1S< T > const & b )
	{
		return ge( a, b );
	}

	// All Array1S == Array1
	friend
	inline
	bool
	all_eq( Array1S< T > const & a, Array1 const & b )
	{
		return all_eq( b, a );
	}

	// All Array1S != Array1
	friend
	inline
	bool
	all_ne( Array1S< T > const & a, Array1 const & b )
	{
		return all_ne( b, a );
	}

	// All Array1S < Array1
	friend
	inline
	bool
	all_lt( Array1S< T > const & a, Array1 const & b )
	{
		return all_gt( b, a );
	}

	// All Array1S <= Array1
	friend
	inline
	bool
	all_le( Array1S< T > const & a, Array1 const & b )
	{
		return all_ge( b, a );
	}

	// All Array1S > Array1
	friend
	inline
	bool
	all_gt( Array1S< T > const & a, Array1 const & b )
	{
		return all_lt( b, a );
	}

	// All Array1S >= Array1
	friend
	inline
	bool
	all_ge( Array1S< T > const & a, Array1 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: Slice

	// Count Array1 == Array1S
	friend
	inline
	size_type
	count_eq( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] == b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 != Array1S
	friend
	inline
	size_type
	count_ne( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] != b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 < Array1S
	friend
	inline
	size_type
	count_lt( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] < b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 <= Array1S
	friend
	inline
	size_type
	count_le( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] <= b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 > Array1S
	friend
	inline
	size_type
	count_gt( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] > b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 >= Array1S
	friend
	inline
	size_type
	count_ge( Array1 const & a, Array1S< T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] >= b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1S == Array1
	friend
	inline
	size_type
	count_eq( Array1S< T > const & a, Array1 const & b )
	{
		return count_eq( b, a );
	}

	// Count Array1S != Array1
	friend
	inline
	size_type
	count_ne( Array1S< T > const & a, Array1 const & b )
	{
		return count_ne( b, a );
	}

	// Count Array1S < Array1
	friend
	inline
	size_type
	count_lt( Array1S< T > const & a, Array1 const & b )
	{
		return count_gt( b, a );
	}

	// Count Array1S <= Array1
	friend
	inline
	size_type
	count_le( Array1S< T > const & a, Array1 const & b )
	{
		return count_ge( b, a );
	}

	// Count Array1S > Array1
	friend
	inline
	size_type
	count_gt( Array1S< T > const & a, Array1 const & b )
	{
		return count_lt( b, a );
	}

	// Count Array1S >= Array1
	friend
	inline
	size_type
	count_ge( Array1S< T > const & a, Array1 const & b )
	{
		return count_le( b, a );
	}

public: // Comparison: Predicate: MArray

	// Array1 == MArray1
	template< class A >
	friend
	inline
	bool
	eq( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] == b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 != MArray1
	template< class A >
	friend
	inline
	bool
	ne( Array1 const & a, MArray1< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Array1 < MArray1
	template< class A >
	friend
	inline
	bool
	lt( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] < b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 <= MArray1
	template< class A >
	friend
	inline
	bool
	le( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] <= b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 > MArray1
	template< class A >
	friend
	inline
	bool
	gt( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] > b( i ) ) ) return false;
		}
		return true;
	}

	// Array1 >= MArray1
	template< class A >
	friend
	inline
	bool
	ge( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( ! ( a[ l ] >= b( i ) ) ) return false;
		}
		return true;
	}

	// MArray1 == Array1
	template< class A >
	friend
	inline
	bool
	eq( MArray1< A, T > const & a, Array1 const & b )
	{
		return eq( b, a );
	}

	// MArray1 != Array1
	template< class A >
	friend
	inline
	bool
	ne( MArray1< A, T > const & a, Array1 const & b )
	{
		return ne( b, a );
	}

	// MArray1 < Array1
	template< class A >
	friend
	inline
	bool
	lt( MArray1< A, T > const & a, Array1 const & b )
	{
		return gt( b, a );
	}

	// MArray1 <= Array1
	template< class A >
	friend
	inline
	bool
	le( MArray1< A, T > const & a, Array1 const & b )
	{
		return ge( b, a );
	}

	// MArray1 > Array1
	template< class A >
	friend
	inline
	bool
	gt( MArray1< A, T > const & a, Array1 const & b )
	{
		return lt( b, a );
	}

	// MArray1 >= Array1
	template< class A >
	friend
	inline
	bool
	ge( MArray1< A, T > const & a, Array1 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any Array1 == MArray1
	template< class A >
	friend
	inline
	bool
	any_eq( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] == b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 != MArray1
	template< class A >
	friend
	inline
	bool
	any_ne( Array1 const & a, MArray1< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any Array1 < MArray1
	template< class A >
	friend
	inline
	bool
	any_lt( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] < b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 <= MArray1
	template< class A >
	friend
	inline
	bool
	any_le( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] <= b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 > MArray1
	template< class A >
	friend
	inline
	bool
	any_gt( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] > b( i ) ) return true;
		}
		return false;
	}

	// Any Array1 >= MArray1
	template< class A >
	friend
	inline
	bool
	any_ge( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		size_type l( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] >= b( i ) ) return true;
		}
		return false;
	}

	// Any MArray1 == Array1
	template< class A >
	friend
	inline
	bool
	any_eq( MArray1< A, T > const & a, Array1 const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray1 != Array1
	template< class A >
	friend
	inline
	bool
	any_ne( MArray1< A, T > const & a, Array1 const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray1 < Array1
	template< class A >
	friend
	inline
	bool
	any_lt( MArray1< A, T > const & a, Array1 const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray1 <= Array1
	template< class A >
	friend
	inline
	bool
	any_le( MArray1< A, T > const & a, Array1 const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray1 > Array1
	template< class A >
	friend
	inline
	bool
	any_gt( MArray1< A, T > const & a, Array1 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray1 >= Array1
	template< class A >
	friend
	inline
	bool
	any_ge( MArray1< A, T > const & a, Array1 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All Array1 == MArray1
	template< class A >
	friend
	inline
	bool
	all_eq( Array1 const & a, MArray1< A, T > const & b )
	{
		return eq( a, b );
	}

	// All Array1 != MArray1
	template< class A >
	friend
	inline
	bool
	all_ne( Array1 const & a, MArray1< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All Array1 < MArray1
	template< class A >
	friend
	inline
	bool
	all_lt( Array1 const & a, MArray1< A, T > const & b )
	{
		return lt( a, b );
	}

	// All Array1 <= MArray1
	template< class A >
	friend
	inline
	bool
	all_le( Array1 const & a, MArray1< A, T > const & b )
	{
		return le( a, b );
	}

	// All Array1 > MArray1
	template< class A >
	friend
	inline
	bool
	all_gt( Array1 const & a, MArray1< A, T > const & b )
	{
		return gt( a, b );
	}

	// All Array1 >= MArray1
	template< class A >
	friend
	inline
	bool
	all_ge( Array1 const & a, MArray1< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray1 == Array1
	template< class A >
	friend
	inline
	bool
	all_eq( MArray1< A, T > const & a, Array1 const & b )
	{
		return all_eq( b, a );
	}

	// All MArray1 != Array1
	template< class A >
	friend
	inline
	bool
	all_ne( MArray1< A, T > const & a, Array1 const & b )
	{
		return all_ne( b, a );
	}

	// All MArray1 < Array1
	template< class A >
	friend
	inline
	bool
	all_lt( MArray1< A, T > const & a, Array1 const & b )
	{
		return all_gt( b, a );
	}

	// All MArray1 <= Array1
	template< class A >
	friend
	inline
	bool
	all_le( MArray1< A, T > const & a, Array1 const & b )
	{
		return all_ge( b, a );
	}

	// All MArray1 > Array1
	template< class A >
	friend
	inline
	bool
	all_gt( MArray1< A, T > const & a, Array1 const & b )
	{
		return all_lt( b, a );
	}

	// All MArray1 >= Array1
	template< class A >
	friend
	inline
	bool
	all_ge( MArray1< A, T > const & a, Array1 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count Array1 == MArray1
	template< class A >
	friend
	inline
	size_type
	count_eq( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] == b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 != MArray1
	template< class A >
	friend
	inline
	size_type
	count_ne( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] != b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 < MArray1
	template< class A >
	friend
	inline
	size_type
	count_lt( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] < b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 <= MArray1
	template< class A >
	friend
	inline
	size_type
	count_le( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] <= b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 > MArray1
	template< class A >
	friend
	inline
	size_type
	count_gt( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] > b( i ) ) ++n;
		}
		return n;
	}

	// Count Array1 >= MArray1
	template< class A >
	friend
	inline
	size_type
	count_ge( Array1 const & a, MArray1< A, T > const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type l( 0 ), n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i, ++l ) {
			if ( a[ l ] >= b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 == Array1
	template< class A >
	friend
	inline
	size_type
	count_eq( MArray1< A, T > const & a, Array1 const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray1 != Array1
	template< class A >
	friend
	inline
	size_type
	count_ne( MArray1< A, T > const & a, Array1 const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray1 < Array1
	template< class A >
	friend
	inline
	size_type
	count_lt( MArray1< A, T > const & a, Array1 const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray1 <= Array1
	template< class A >
	friend
	inline
	size_type
	count_le( MArray1< A, T > const & a, Array1 const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray1 > Array1
	template< class A >
	friend
	inline
	size_type
	count_gt( MArray1< A, T > const & a, Array1 const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray1 >= Array1
	template< class A >
	friend
	inline
	size_type
	count_ge( MArray1< A, T > const & a, Array1 const & b )
	{
		return count_le( b, a );
	}

protected: // Functions

	// Dimension by IndexRange
	virtual
	void
	dimension_assign( IR const & I ) = 0;

	// Clear on Move
	inline
	void
	clear_move()
	{
		I_.clear();
	}

	// Swap
	inline
	void
	swap1( Array1 & v )
	{
		swapB( v );
		I_.swap( v.I_ );
	}

protected: // Data

	IR I_; // Index range

}; // Array1

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array1< U > const & a, Array1< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array1< U > const & a, Array1S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( Array1S< U > const & a, Array1< V > const & b )
{
	return b.conformable( a );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( Array1< U > const & a, MArray1< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray1< A, M > const & a, Array1< V > const & b )
{
	return b.conformable( a );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( Array1< U > const & a, Array1< V > const & b )
{
	return a.equal_dimensions( b );
}

// Magnitude
template< typename T >
inline
T
magnitude( Array1< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( int i = a.l(), e = a.u(); i <= e; ++i ) {
		T const mag_i( a( i ) );
		mag_sq += mag_i * mag_i;
	}
	return std::sqrt( mag_sq );
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( Array1< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( int i = a.l(), e = a.u(); i <= e; ++i ) {
		T const mag_i( a( i ) );
		mag_sq += mag_i * mag_i;
	}
	return mag_sq;
}

// Distance
template< typename T >
inline
T
distance( Array1< T > const & a, Array1< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance Squared
template< typename T >
inline
T
distance_squared( Array1< T > const & a, Array1< T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Dot Product
template< typename T >
inline
T
dot( Array1< T > const & a, Array1< T > const & b )
{
	assert( a.size() == b.size() );
	T result( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		result += a( i ) * b( j );
	}
	return result;
}

// Dot Product
template< typename T >
inline
T
dot( Array1< T > const & a, Array1S< T > const & b )
{
	assert( a.size() == b.size() );
	T result( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		result += a( i ) * b( j );
	}
	return result;
}

// Dot Product
template< typename T >
inline
T
dot( Array1S< T > const & a, Array1< T > const & b )
{
	return dot( b, a );
}

// Dot Product
template< class A, typename T >
inline
T
dot( Array1< T > const & a, MArray1< A, T > const & b )
{
	assert( a.size() == b.size() );
	T result( T( 0 ) );
	for ( int i = a.l(), j = 1, e = a.u(); i <= e; ++i, ++j ) {
		result += a( i ) * b( j );
	}
	return result;
}

// Dot Product
template< class A, typename T >
inline
T
dot( MArray1< A, T > const & a, Array1< T > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays
inline
bool
dot( Array1< bool > const & a, Array1< bool > const & b )
{
	assert( a.size() == b.size() );
	bool result( false );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		if ( a( i ) && b( j ) ) {
			result = true;
			break;
		}
	}
	return result;
}

// Dot Product of Boolean Arrays
inline
bool
dot( Array1< bool > const & a, Array1S< bool > const & b )
{
	assert( a.size() == b.size() );
	bool result( false );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		if ( a( i ) && b( j ) ) {
			result = true;
			break;
		}
	}
	return result;
}

// Dot Product of Boolean Arrays
inline
bool
dot( Array1S< bool > const & a, Array1< bool > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays
template< class A >
inline
bool
dot( Array1< bool > const & a, MArray1< A, bool > const & b )
{
	assert( a.size() == b.size() );
	bool result( false );
	for ( int i = a.l(), j = 1, e = a.u(); i <= e; ++i, ++j ) {
		if ( a( i ) && b( j ) ) {
			result = true;
			break;
		}
	}
	return result;
}

// Dot Product of Boolean Arrays
template< class A >
inline
bool
dot( MArray1< A, bool > const & a, Array1< bool > const & b )
{
	return dot( b, a );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( Array1< T > const & a, Array1< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( Array1< T > const & a, Array1S< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( Array1S< T > const & a, Array1< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( Array1< T > const & a, MArray1< A, T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( MArray1< A, T > const & a, Array1< T > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( Array1< bool > const & a, Array1< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( Array1< bool > const & a, Array1S< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( Array1S< bool > const & a, Array1< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( Array1< bool > const & a, MArray1< A, bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( MArray1< A, bool > const & a, Array1< bool > const & b )
{
	return dot( b, a );
}

} // ObjexxFCL

#endif // ObjexxFCL_Array1_hh_INCLUDED
