#ifndef ObjexxFCL_FArray1_hh_INCLUDED
#define ObjexxFCL_FArray1_hh_INCLUDED

// FArray1: Fortran-Compatible 1D Array Abstract Base Class
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
#include <ObjexxFCL/FArray1.fwd.hh>
#include <ObjexxFCL/FArray.hh>
#include <ObjexxFCL/FArray1S.hh>
#include <ObjexxFCL/MArray1.hh>

// C++ Headers
#include <cmath>

namespace ObjexxFCL {

// Forward
template< typename > class FArray1D;
template< typename > class FArray1P;
template< typename > class FArray1A;

// FArray1: Fortran-Compatible 1D Array Abstract Base Class
template< typename T >
class FArray1 : public FArray< T >
{

private: // Types

	typedef  FArray< T >  Super;
	typedef  FArray1D< T >  real_FArray;
	typedef  FArray1P< T >  proxy_FArray;
	typedef  FArray1A< T >  arg_FArray;

private: // Friend

	template< typename > friend class FArray1;
	template< typename > friend class FArray1D;
	template< typename > friend class FArray1P;
	template< typename > friend class FArray1A;

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
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	using Super::dimensions_initialized;
	using Super::isize;
	using Super::npos;
	using Super::overlap;
	using Super::size;
	using Super::slice_k;
	using Super::swapB;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	using Super::not_const_proxy;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Types to prevent compile failure when std::distance is in scope
	typedef  void  iterator_category;

protected: // Creation

	// Default Constructor
	inline
	FArray1()
	{}

	// Copy Constructor
	inline
	FArray1( FArray1 const & a ) :
	 Super( a )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray1( FArray1< U > const & a ) :
	 Super( a )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray1( FArray1S< U > const & a ) :
	 Super( a )
	{}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray1( MArray1< A, M > const & a ) :
	 Super( a )
	{}

	// Size Constructor
	inline
	explicit
	FArray1( size_type const size ) :
	 Super( size )
	{}

	// Size + InitializerSentinel Constructor
	inline
	FArray1( size_type const size, InitializerSentinel const & initialized ) :
	 Super( size, initialized )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1( std::initializer_list< U > const l ) :
	 Super( l )
	{}

	// std::array Constructor Template
	template< typename U, Size s, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1( std::array< U, s > const & a ) :
	 Super( a )
	{}

	// std::vector Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1( std::vector< U > const & v ) :
	 Super( v )
	{}

	// Vector2 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1( Vector2< U > const & v ) :
	 Super( v )
	{}

	// Vector3 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1( Vector3< U > const & v ) :
	 Super( v )
	{}

	// Default Proxy Constructor
	inline
	FArray1( ProxySentinel const & proxy ) :
	 Super( proxy )
	{}

	// Copy Proxy Constructor
	inline
	FArray1( FArray1 const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Base Proxy Constructor
	inline
	FArray1( Base const & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Tail Proxy Constructor
	inline
	FArray1( Tail const & s, ProxySentinel const & proxy ) :
	 Super( s, proxy )
	{}

	// Value Proxy Constructor
	inline
	FArray1( T const & t, ProxySentinel const & proxy ) :
	 Super( t, proxy )
	{}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Proxy Constructor
	inline
	FArray1( FArray1 & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Non-Const Base Proxy Constructor
	inline
	FArray1( Base & a, ProxySentinel const & proxy ) :
	 Super( a, proxy )
	{}

	// Non-Const Tail Proxy Constructor
	inline
	FArray1( Tail & s, ProxySentinel const & proxy ) :
	 Super( s, proxy )
	{}

	// Non-Const Value Proxy Constructor
	inline
	FArray1( T & t, ProxySentinel const & proxy ) :
	 Super( t, proxy )
	{}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Destructor
	inline
	virtual
	~FArray1()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray1 &
	operator =( FArray1 const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension_assign( a.I() );
			Super::operator =( a );
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator =( FArray1< U > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I() );
		Super::operator =( a );
		return *this;
	}

	// Slice Assignment
	inline
	FArray1 &
	operator =( FArray1S< T > const & a )
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
	FArray1 &
	operator =( FArray1S< U > const & a )
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
	FArray1 &
	operator =( MArray1< A, M > const & a )
	{
		if ( ! conformable( a ) ) dimension_assign( a.I() );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] = a( i );
			}
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator =( std::initializer_list< U > const l )
	{
		Super::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator =( std::array< U, s > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator =( std::vector< U > const & v )
	{
		Super::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator =( Vector2< U > const & v )
	{
		Super::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator =( Vector3< U > const & v )
	{
		Super::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator +=( FArray1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator -=( FArray1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator *=( FArray1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator /=( FArray1< U > const & a )
	{
		assert( conformable( a ) );
		Super::operator /=( a );
		return *this;
	}

	// += Slice
	inline
	FArray1 &
	operator +=( FArray1S< T > const & a )
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
	FArray1 &
	operator -=( FArray1S< T > const & a )
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
	FArray1 &
	operator *=( FArray1S< T > const & a )
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
	FArray1 &
	operator /=( FArray1S< T > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( a.size() );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				assert( T( a( i ) ) != T( 0 ) );
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
	FArray1 &
	operator +=( FArray1S< U > const & a )
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
	FArray1 &
	operator -=( FArray1S< U > const & a )
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
	FArray1 &
	operator *=( FArray1S< U > const & a )
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
	FArray1 &
	operator /=( FArray1S< U > const & a )
	{
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			assert( T( a( i ) ) != T( 0 ) );
			data_[ l ] /= a( i );
		}
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray1 &
	operator +=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] += a( i );
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray1 &
	operator -=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] -= a( i );
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray1 &
	operator *=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] *= a( i );
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray1 &
	operator /=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				assert( T( a( i ) ) != T( 0 ) );
				data_[ l ] /= a( i );
			}
		}
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator +=( std::initializer_list< U > const l )
	{
		Super::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator -=( std::initializer_list< U > const l )
	{
		Super::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator *=( std::initializer_list< U > const l )
	{
		Super::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator /=( std::initializer_list< U > const l )
	{
		Super::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator +=( std::array< U, s > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator -=( std::array< U, s > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator *=( std::array< U, s > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator /=( std::array< U, s > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator +=( std::vector< U > const & v )
	{
		Super::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator -=( std::vector< U > const & v )
	{
		Super::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator *=( std::vector< U > const & v )
	{
		Super::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator /=( std::vector< U > const & v )
	{
		Super::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator +=( Vector2< U > const & v )
	{
		Super::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator -=( Vector2< U > const & v )
	{
		Super::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator *=( Vector2< U > const & v )
	{
		Super::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator /=( Vector2< U > const & v )
	{
		Super::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator +=( Vector3< U > const & v )
	{
		Super::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator -=( Vector3< U > const & v )
	{
		Super::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator *=( Vector3< U > const & v )
	{
		Super::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	operator /=( Vector3< U > const & v )
	{
		Super::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	and_equals( FArray1< U > const & a )
	{
		assert( conformable( a ) );
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	or_equals( FArray1< U > const & a )
	{
		assert( conformable( a ) );
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice
	inline
	FArray1 &
	and_equals( FArray1S< T > const & a )
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
	FArray1 &
	or_equals( FArray1S< T > const & a )
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
	FArray1 &
	and_equals( FArray1S< U > const & a )
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
	FArray1 &
	or_equals( FArray1S< U > const & a )
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
	FArray1 &
	and_equals( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] = data_[ l ] && a( i );
			}
		}
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray1 &
	or_equals( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				data_[ l ] = data_[ l ] || a( i );
			}
		}
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1 &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray1 &
	operator =( T const & t )
	{
		Super::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray1 &
	operator +=( T const & t )
	{
		Super::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray1 &
	operator -=( T const & t )
	{
		Super::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray1 &
	operator *=( T const & t )
	{
		Super::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray1 &
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
		proxy_const_assert( not_const_proxy() );
		assert( contains( i ) );
		return sdata_[ i ];
	}

	// Const Tail Starting at array( i )
	inline
	Tail const
	a( int const i ) const
	{
		assert( contains( i ) );
		return Tail( static_cast< T const * >( sdata_ + i ), ( data_size_ != npos ? data_size_ - ( i - shift_ ) : npos ) );
	}

	// Tail Starting at array( i )
	inline
	Tail
	a( int const i )
	{
		proxy_const_assert( not_const_proxy() );
		assert( contains( i ) );
		return Tail( sdata_ + i, ( data_size_ != npos ? data_size_ - ( i - shift_ ) : npos ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i ) const
	{
		assert( dimensions_initialized() );
		return i - shift_;
	}

public: // Slice Proxy Generators

	// array( s ) const
	inline
	FArray1S< T >
	operator ()( IS const & s ) const
	{
		DS const d( I(), s );
		return FArray1S< T >( data_, -shift_, d );
	}

	// array( s )
	inline
	FArray1S< T >
	operator ()( IS const & s )
	{
		DS const d( I(), s );
		return FArray1S< T >( data_, -shift_, d );
	}

#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 bug work-around

	// array( {s} ) const
	inline
	FArray1S< T >
	operator ()( std::initializer_list< int > const l ) const
	{
		IS const s( l );
		DS const d( I(), s );
		return FArray1S< T >( data_, -shift_, d );
	}

	// array( {s} )
	inline
	FArray1S< T >
	operator ()( std::initializer_list< int > const l )
	{
		IS const s( l );
		DS const d( I(), s );
		return FArray1S< T >( data_, -shift_, d );
	}

#else

	// array( {s} ) const
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	FArray1S< T >
	operator ()( std::initializer_list< U > const l ) const
	{
		IS const s( l );
		DS const d( I(), s );
		return FArray1S< T >( data_, -shift_, d );
	}

	// array( {s} )
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	FArray1S< T >
	operator ()( std::initializer_list< U > const l )
	{
		IS const s( l );
		DS const d( I(), s );
		return FArray1S< T >( data_, -shift_, d );
	}

#endif

public: // Predicate

	// contains( i )
	inline
	bool
	contains( int const i ) const
	{
		return ( I().contains( i ) );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray1< U > const & a ) const
	{
		return ( size() == a.size() );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray1S< U > const & a ) const
	{
		return ( size() == a.size() );
	}

	// Conformable?
	template< class A, typename M >
	inline
	bool
	conformable( MArray1< A, M > const & a ) const
	{
		return ( size() == a.size() );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray1< U > const & a ) const
	{
		return ( I() == a.I() );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray1S< U > const & a ) const
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
			return I1();
		default:
			assert( false );
			return I1();
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
	virtual
	IR const &
	I() const = 0;

	// Lower Index
	virtual
	int
	l() const = 0;

	// Upper Index
	virtual
	int
	u() const = 0;

	// IndexRange of Dimension 1
	virtual
	IR const &
	I1() const = 0;

	// Lower Index of Dimension 1
	virtual
	int
	l1() const = 0;

	// Upper Index of Dimension 1
	virtual
	int
	u1() const = 0;

	// Size of Dimension 1
	virtual
	size_type
	size1() const = 0;

	// Size of Dimension 1
	virtual
	int
	isize1() const = 0;

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
	FArray1 &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Assign Default Value to all Elements
	inline
	FArray1 &
	to_default()
	{
		Super::to_default();
		return *this;
	}

	// Normalize to Unit Length
	inline
	FArray1 &
	normalize()
	{
		proxy_const_assert( not_const_proxy() );
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
	MArray1< FArray1 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray1< FArray1 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray1< FArray1, M >
	ma( M ClassT::* pmem )
	{
		return MArray1< FArray1, M >( *this, pmem );
	}

#include <ObjexxFCL/FArray1.Project.MArray.hh> // Inject project-specific MArray generators

public: // Comparison: Predicate

	// FArray1 == FArray1
	inline
	friend
	bool
	eq( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 != FArray1
	inline
	friend
	bool
	ne( FArray1 const & a, FArray1 const & b )
	{
		return ! eq( a, b );
	}

	// FArray1 < FArray1
	inline
	friend
	bool
	lt( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 <= FArray1
	inline
	friend
	bool
	le( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 > FArray1
	inline
	friend
	bool
	gt( FArray1 const & a, FArray1 const & b )
	{
		return lt( b, a );
	}

	// FArray1 >= FArray1
	inline
	friend
	bool
	ge( FArray1 const & a, FArray1 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any

	// FArray1 == FArray1
	inline
	friend
	bool
	any_eq( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 != FArray1
	inline
	friend
	bool
	any_ne( FArray1 const & a, FArray1 const & b )
	{
		return ! eq( a, b );
	}

	// FArray1 < FArray1
	inline
	friend
	bool
	any_lt( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 <= FArray1
	inline
	friend
	bool
	any_le( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return any_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 > FArray1
	inline
	friend
	bool
	any_gt( FArray1 const & a, FArray1 const & b )
	{
		return any_lt( b, a );
	}

	// FArray1 >= FArray1
	inline
	friend
	bool
	any_ge( FArray1 const & a, FArray1 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All

	// FArray1 == FArray1
	inline
	friend
	bool
	all_eq( FArray1 const & a, FArray1 const & b )
	{
		return eq( a, b );
	}

	// FArray1 != FArray1
	inline
	friend
	bool
	all_ne( FArray1 const & a, FArray1 const & b )
	{
		return ! any_eq( a, b );
	}

	// FArray1 < FArray1
	inline
	friend
	bool
	all_lt( FArray1 const & a, FArray1 const & b )
	{
		return lt( a, b );
	}

	// FArray1 <= FArray1
	inline
	friend
	bool
	all_le( FArray1 const & a, FArray1 const & b )
	{
		return le( a, b );
	}

	// FArray1 > FArray1
	inline
	friend
	bool
	all_gt( FArray1 const & a, FArray1 const & b )
	{
		return gt( a, b );
	}

	// FArray1 >= FArray1
	inline
	friend
	bool
	all_ge( FArray1 const & a, FArray1 const & b )
	{
		return ge( a, b );
	}

public: // Comparison: Count

	// FArray1 == FArray1
	inline
	friend
	bool
	count_eq( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_eq( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 != FArray1
	inline
	friend
	bool
	count_ne( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ne( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 < FArray1
	inline
	friend
	bool
	count_lt( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_lt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 <= FArray1
	inline
	friend
	bool
	count_le( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_le( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 > FArray1
	inline
	friend
	bool
	count_gt( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_gt( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

	// FArray1 >= FArray1
	inline
	friend
	bool
	count_ge( FArray1 const & a, FArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		return count_ge( static_cast< Super const & >( a ), static_cast< Super const & >( b ) );
	}

public: // Comparison: Predicate: Slice

	// FArray1 == FArray1S
	inline
	friend
	bool
	eq( FArray1 const & a, FArray1S< T > const & b )
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

	// FArray1 != FArray1S
	inline
	friend
	bool
	ne( FArray1 const & a, FArray1S< T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray1 < FArray1S
	inline
	friend
	bool
	lt( FArray1 const & a, FArray1S< T > const & b )
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

	// FArray1 <= FArray1S
	inline
	friend
	bool
	le( FArray1 const & a, FArray1S< T > const & b )
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

	// FArray1 > FArray1S
	inline
	friend
	bool
	gt( FArray1 const & a, FArray1S< T > const & b )
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

	// FArray1 >= FArray1S
	inline
	friend
	bool
	ge( FArray1 const & a, FArray1S< T > const & b )
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

	// FArray1S == FArray1
	inline
	friend
	bool
	eq( FArray1S< T > const & a, FArray1 const & b )
	{
		return eq( b, a );
	}

	// FArray1S != FArray1
	inline
	friend
	bool
	ne( FArray1S< T > const & a, FArray1 const & b )
	{
		return ne( b, a );
	}

	// FArray1S < FArray1
	inline
	friend
	bool
	lt( FArray1S< T > const & a, FArray1 const & b )
	{
		return gt( b, a );
	}

	// FArray1S <= FArray1
	inline
	friend
	bool
	le( FArray1S< T > const & a, FArray1 const & b )
	{
		return ge( b, a );
	}

	// FArray1S > FArray1
	inline
	friend
	bool
	gt( FArray1S< T > const & a, FArray1 const & b )
	{
		return lt( b, a );
	}

	// FArray1S >= FArray1
	inline
	friend
	bool
	ge( FArray1S< T > const & a, FArray1 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: Slice

	// Any FArray1 == FArray1S
	inline
	friend
	bool
	any_eq( FArray1 const & a, FArray1S< T > const & b )
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

	// Any FArray1 != FArray1S
	inline
	friend
	bool
	any_ne( FArray1 const & a, FArray1S< T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray1 < FArray1S
	inline
	friend
	bool
	any_lt( FArray1 const & a, FArray1S< T > const & b )
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

	// Any FArray1 <= FArray1S
	inline
	friend
	bool
	any_le( FArray1 const & a, FArray1S< T > const & b )
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

	// Any FArray1 > FArray1S
	inline
	friend
	bool
	any_gt( FArray1 const & a, FArray1S< T > const & b )
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

	// Any FArray1 >= FArray1S
	inline
	friend
	bool
	any_ge( FArray1 const & a, FArray1S< T > const & b )
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

	// Any FArray1S == FArray1
	inline
	friend
	bool
	any_eq( FArray1S< T > const & a, FArray1 const & b )
	{
		return any_eq( b, a );
	}

	// Any FArray1S != FArray1
	inline
	friend
	bool
	any_ne( FArray1S< T > const & a, FArray1 const & b )
	{
		return any_ne( b, a );
	}

	// Any FArray1S < FArray1
	inline
	friend
	bool
	any_lt( FArray1S< T > const & a, FArray1 const & b )
	{
		return any_gt( b, a );
	}

	// Any FArray1S <= FArray1
	inline
	friend
	bool
	any_le( FArray1S< T > const & a, FArray1 const & b )
	{
		return any_ge( b, a );
	}

	// Any FArray1S > FArray1
	inline
	friend
	bool
	any_gt( FArray1S< T > const & a, FArray1 const & b )
	{
		return any_lt( b, a );
	}

	// Any FArray1S >= FArray1
	inline
	friend
	bool
	any_ge( FArray1S< T > const & a, FArray1 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: Slice

	// All FArray1 == FArray1S
	inline
	friend
	bool
	all_eq( FArray1 const & a, FArray1S< T > const & b )
	{
		return eq( a, b );
	}

	// All FArray1 != FArray1S
	inline
	friend
	bool
	all_ne( FArray1 const & a, FArray1S< T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray1 < FArray1S
	inline
	friend
	bool
	all_lt( FArray1 const & a, FArray1S< T > const & b )
	{
		return lt( a, b );
	}

	// All FArray1 <= FArray1S
	inline
	friend
	bool
	all_le( FArray1 const & a, FArray1S< T > const & b )
	{
		return le( a, b );
	}

	// All FArray1 > FArray1S
	inline
	friend
	bool
	all_gt( FArray1 const & a, FArray1S< T > const & b )
	{
		return gt( a, b );
	}

	// All FArray1 >= FArray1S
	inline
	friend
	bool
	all_ge( FArray1 const & a, FArray1S< T > const & b )
	{
		return ge( a, b );
	}

	// All FArray1S == FArray1
	inline
	friend
	bool
	all_eq( FArray1S< T > const & a, FArray1 const & b )
	{
		return all_eq( b, a );
	}

	// All FArray1S != FArray1
	inline
	friend
	bool
	all_ne( FArray1S< T > const & a, FArray1 const & b )
	{
		return all_ne( b, a );
	}

	// All FArray1S < FArray1
	inline
	friend
	bool
	all_lt( FArray1S< T > const & a, FArray1 const & b )
	{
		return all_gt( b, a );
	}

	// All FArray1S <= FArray1
	inline
	friend
	bool
	all_le( FArray1S< T > const & a, FArray1 const & b )
	{
		return all_ge( b, a );
	}

	// All FArray1S > FArray1
	inline
	friend
	bool
	all_gt( FArray1S< T > const & a, FArray1 const & b )
	{
		return all_lt( b, a );
	}

	// All FArray1S >= FArray1
	inline
	friend
	bool
	all_ge( FArray1S< T > const & a, FArray1 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: Slice

	// Count FArray1 == FArray1S
	inline
	friend
	size_type
	count_eq( FArray1 const & a, FArray1S< T > const & b )
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

	// Count FArray1 != FArray1S
	inline
	friend
	size_type
	count_ne( FArray1 const & a, FArray1S< T > const & b )
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

	// Count FArray1 < FArray1S
	inline
	friend
	size_type
	count_lt( FArray1 const & a, FArray1S< T > const & b )
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

	// Count FArray1 <= FArray1S
	inline
	friend
	size_type
	count_le( FArray1 const & a, FArray1S< T > const & b )
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

	// Count FArray1 > FArray1S
	inline
	friend
	size_type
	count_gt( FArray1 const & a, FArray1S< T > const & b )
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

	// Count FArray1 >= FArray1S
	inline
	friend
	size_type
	count_ge( FArray1 const & a, FArray1S< T > const & b )
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

	// Count FArray1S == FArray1
	inline
	friend
	size_type
	count_eq( FArray1S< T > const & a, FArray1 const & b )
	{
		return count_eq( b, a );
	}

	// Count FArray1S != FArray1
	inline
	friend
	size_type
	count_ne( FArray1S< T > const & a, FArray1 const & b )
	{
		return count_ne( b, a );
	}

	// Count FArray1S < FArray1
	inline
	friend
	size_type
	count_lt( FArray1S< T > const & a, FArray1 const & b )
	{
		return count_gt( b, a );
	}

	// Count FArray1S <= FArray1
	inline
	friend
	size_type
	count_le( FArray1S< T > const & a, FArray1 const & b )
	{
		return count_ge( b, a );
	}

	// Count FArray1S > FArray1
	inline
	friend
	size_type
	count_gt( FArray1S< T > const & a, FArray1 const & b )
	{
		return count_lt( b, a );
	}

	// Count FArray1S >= FArray1
	inline
	friend
	size_type
	count_ge( FArray1S< T > const & a, FArray1 const & b )
	{
		return count_le( b, a );
	}

public: // Comparison: Predicate: MArray

	// FArray1 == MArray1
	template< class A >
	inline
	friend
	bool
	eq( FArray1 const & a, MArray1< A, T > const & b )
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

	// FArray1 != MArray1
	template< class A >
	inline
	friend
	bool
	ne( FArray1 const & a, MArray1< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray1 < MArray1
	template< class A >
	inline
	friend
	bool
	lt( FArray1 const & a, MArray1< A, T > const & b )
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

	// FArray1 <= MArray1
	template< class A >
	inline
	friend
	bool
	le( FArray1 const & a, MArray1< A, T > const & b )
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

	// FArray1 > MArray1
	template< class A >
	inline
	friend
	bool
	gt( FArray1 const & a, MArray1< A, T > const & b )
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

	// FArray1 >= MArray1
	template< class A >
	inline
	friend
	bool
	ge( FArray1 const & a, MArray1< A, T > const & b )
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

	// MArray1 == FArray1
	template< class A >
	inline
	friend
	bool
	eq( MArray1< A, T > const & a, FArray1 const & b )
	{
		return eq( b, a );
	}

	// MArray1 != FArray1
	template< class A >
	inline
	friend
	bool
	ne( MArray1< A, T > const & a, FArray1 const & b )
	{
		return ne( b, a );
	}

	// MArray1 < FArray1
	template< class A >
	inline
	friend
	bool
	lt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return gt( b, a );
	}

	// MArray1 <= FArray1
	template< class A >
	inline
	friend
	bool
	le( MArray1< A, T > const & a, FArray1 const & b )
	{
		return ge( b, a );
	}

	// MArray1 > FArray1
	template< class A >
	inline
	friend
	bool
	gt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return lt( b, a );
	}

	// MArray1 >= FArray1
	template< class A >
	inline
	friend
	bool
	ge( MArray1< A, T > const & a, FArray1 const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any FArray1 == MArray1
	template< class A >
	inline
	friend
	bool
	any_eq( FArray1 const & a, MArray1< A, T > const & b )
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

	// Any FArray1 != MArray1
	template< class A >
	inline
	friend
	bool
	any_ne( FArray1 const & a, MArray1< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray1 < MArray1
	template< class A >
	inline
	friend
	bool
	any_lt( FArray1 const & a, MArray1< A, T > const & b )
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

	// Any FArray1 <= MArray1
	template< class A >
	inline
	friend
	bool
	any_le( FArray1 const & a, MArray1< A, T > const & b )
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

	// Any FArray1 > MArray1
	template< class A >
	inline
	friend
	bool
	any_gt( FArray1 const & a, MArray1< A, T > const & b )
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

	// Any FArray1 >= MArray1
	template< class A >
	inline
	friend
	bool
	any_ge( FArray1 const & a, MArray1< A, T > const & b )
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

	// Any MArray1 == FArray1
	template< class A >
	inline
	friend
	bool
	any_eq( MArray1< A, T > const & a, FArray1 const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray1 != FArray1
	template< class A >
	inline
	friend
	bool
	any_ne( MArray1< A, T > const & a, FArray1 const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray1 < FArray1
	template< class A >
	inline
	friend
	bool
	any_lt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray1 <= FArray1
	template< class A >
	inline
	friend
	bool
	any_le( MArray1< A, T > const & a, FArray1 const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray1 > FArray1
	template< class A >
	inline
	friend
	bool
	any_gt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray1 >= FArray1
	template< class A >
	inline
	friend
	bool
	any_ge( MArray1< A, T > const & a, FArray1 const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All FArray1 == MArray1
	template< class A >
	inline
	friend
	bool
	all_eq( FArray1 const & a, MArray1< A, T > const & b )
	{
		return eq( a, b );
	}

	// All FArray1 != MArray1
	template< class A >
	inline
	friend
	bool
	all_ne( FArray1 const & a, MArray1< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray1 < MArray1
	template< class A >
	inline
	friend
	bool
	all_lt( FArray1 const & a, MArray1< A, T > const & b )
	{
		return lt( a, b );
	}

	// All FArray1 <= MArray1
	template< class A >
	inline
	friend
	bool
	all_le( FArray1 const & a, MArray1< A, T > const & b )
	{
		return le( a, b );
	}

	// All FArray1 > MArray1
	template< class A >
	inline
	friend
	bool
	all_gt( FArray1 const & a, MArray1< A, T > const & b )
	{
		return gt( a, b );
	}

	// All FArray1 >= MArray1
	template< class A >
	inline
	friend
	bool
	all_ge( FArray1 const & a, MArray1< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray1 == FArray1
	template< class A >
	inline
	friend
	bool
	all_eq( MArray1< A, T > const & a, FArray1 const & b )
	{
		return all_eq( b, a );
	}

	// All MArray1 != FArray1
	template< class A >
	inline
	friend
	bool
	all_ne( MArray1< A, T > const & a, FArray1 const & b )
	{
		return all_ne( b, a );
	}

	// All MArray1 < FArray1
	template< class A >
	inline
	friend
	bool
	all_lt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return all_gt( b, a );
	}

	// All MArray1 <= FArray1
	template< class A >
	inline
	friend
	bool
	all_le( MArray1< A, T > const & a, FArray1 const & b )
	{
		return all_ge( b, a );
	}

	// All MArray1 > FArray1
	template< class A >
	inline
	friend
	bool
	all_gt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return all_lt( b, a );
	}

	// All MArray1 >= FArray1
	template< class A >
	inline
	friend
	bool
	all_ge( MArray1< A, T > const & a, FArray1 const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count FArray1 == MArray1
	template< class A >
	inline
	friend
	size_type
	count_eq( FArray1 const & a, MArray1< A, T > const & b )
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

	// Count FArray1 != MArray1
	template< class A >
	inline
	friend
	size_type
	count_ne( FArray1 const & a, MArray1< A, T > const & b )
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

	// Count FArray1 < MArray1
	template< class A >
	inline
	friend
	size_type
	count_lt( FArray1 const & a, MArray1< A, T > const & b )
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

	// Count FArray1 <= MArray1
	template< class A >
	inline
	friend
	size_type
	count_le( FArray1 const & a, MArray1< A, T > const & b )
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

	// Count FArray1 > MArray1
	template< class A >
	inline
	friend
	size_type
	count_gt( FArray1 const & a, MArray1< A, T > const & b )
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

	// Count FArray1 >= MArray1
	template< class A >
	inline
	friend
	size_type
	count_ge( FArray1 const & a, MArray1< A, T > const & b )
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

	// Count MArray1 == FArray1
	template< class A >
	inline
	friend
	size_type
	count_eq( MArray1< A, T > const & a, FArray1 const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray1 != FArray1
	template< class A >
	inline
	friend
	size_type
	count_ne( MArray1< A, T > const & a, FArray1 const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray1 < FArray1
	template< class A >
	inline
	friend
	size_type
	count_lt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray1 <= FArray1
	template< class A >
	inline
	friend
	size_type
	count_le( MArray1< A, T > const & a, FArray1 const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray1 > FArray1
	template< class A >
	inline
	friend
	size_type
	count_gt( MArray1< A, T > const & a, FArray1 const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray1 >= FArray1
	template< class A >
	inline
	friend
	size_type
	count_ge( MArray1< A, T > const & a, FArray1 const & b )
	{
		return count_le( b, a );
	}

protected: // Functions

	// Dimension by IndexRange
	virtual
	void
	dimension_assign( IR const & I ) = 0;

	// Swap
	inline
	void
	swap1DB( FArray1 & v )
	{
		swapB( v );
	}

}; // FArray1

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray1< U > const & a, FArray1< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray1< U > const & a, FArray1S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray1S< U > const & a, FArray1< V > const & b )
{
	return b.conformable( a );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( FArray1< U > const & a, MArray1< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray1< A, M > const & a, FArray1< V > const & b )
{
	return b.conformable( a );
}

// Equal Dimensions?
template< typename U, typename V >
inline
bool
equal_dimensions( FArray1< U > const & a, FArray1< V > const & b )
{
	return a.equal_dimensions( b );
}

// Magnitude
template< typename T >
inline
T
magnitude( FArray1< T > const & a )
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
magnitude_squared( FArray1< T > const & a )
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
distance( FArray1< T > const & a, FArray1< T > const & b )
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
distance_squared( FArray1< T > const & a, FArray1< T > const & b )
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
dot( FArray1< T > const & a, FArray1< T > const & b )
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
dot( FArray1< T > const & a, FArray1S< T > const & b )
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
dot( FArray1S< T > const & a, FArray1< T > const & b )
{
	return dot( b, a );
}

// Dot Product
template< class A, typename T >
inline
T
dot( FArray1< T > const & a, MArray1< A, T > const & b )
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
dot( MArray1< A, T > const & a, FArray1< T > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays
inline
bool
dot( FArray1< bool > const & a, FArray1< bool > const & b )
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
dot( FArray1< bool > const & a, FArray1S< bool > const & b )
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
dot( FArray1S< bool > const & a, FArray1< bool > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays
template< class A >
inline
bool
dot( FArray1< bool > const & a, MArray1< A, bool > const & b )
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
dot( MArray1< A, bool > const & a, FArray1< bool > const & b )
{
	return dot( b, a );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( FArray1< T > const & a, FArray1< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( FArray1< T > const & a, FArray1S< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( FArray1S< T > const & a, FArray1< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( MArray1< A, T > const & a, FArray1< T > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( FArray1< bool > const & a, FArray1< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( FArray1< bool > const & a, FArray1S< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( FArray1S< bool > const & a, FArray1< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( FArray1< bool > const & a, MArray1< A, bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( MArray1< A, bool > const & a, FArray1< bool > const & b )
{
	return dot( b, a );
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray1_hh_INCLUDED
