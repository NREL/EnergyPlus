#ifndef ObjexxFCL_FArray1A_hh_INCLUDED
#define ObjexxFCL_FArray1A_hh_INCLUDED

// FArray1A: Fortran-Compatible 1D Argument Array
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
#include <ObjexxFCL/FArray1A.fwd.hh>
#include <ObjexxFCL/FArray1P.hh>

namespace ObjexxFCL {

// FArray1A: Fortran-Compatible 1D Argument Array
template< typename T >
class FArray1A : public FArray1< T >
{

private: // Types

	typedef  FArray1< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  typename Super::arg_FArray  arg_FArray;
	typedef  internal::ProxySentinel  ProxySentinel;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Base::Tail  Tail;
	typedef  typename Super::IR  SIR;
	typedef  StaticIndexRange  IR;

	// STL Style
	typedef  typename Base::value_type  value_type;
	typedef  typename Base::reference  reference;
	typedef  typename Base::const_reference  const_reference;
	typedef  typename Base::pointer  pointer;
	typedef  typename Base::const_pointer  const_pointer;
	typedef  typename Base::size_type  size_type;
	typedef  typename Base::difference_type  difference_type;

	// C++ Style
	typedef  typename Base::Value  Value;
	typedef  typename Base::Reference  Reference;
	typedef  typename Base::ConstReference  ConstReference;
	typedef  typename Base::Pointer  Pointer;
	typedef  typename Base::ConstPointer  ConstPointer;
	typedef  typename Base::Size  Size;
	typedef  typename Base::Difference  Difference;

	using Super::conformable;
	using Super::npos;
	using Super::operator ();
	using Super::shift_set;
	using Super::size_set;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	using Super::not_const_proxy;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Default Constructor
	inline
	FArray1A() :
	 Super( ProxySentinel() )
	{}

	// Copy Constructor
	inline
	FArray1A( FArray1A const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ )
	{
		shift_set( a.shift_ );
	}

	// Real Constructor
	inline
	FArray1A( real_FArray const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ )
	{
		shift_set( a.shift_ );
	}

	// Proxy Constructor
	inline
	FArray1A( proxy_FArray const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ )
	{
		shift_set( a.shift_ );
	}

	// Super Constructor
	inline
	FArray1A( Super const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I() )
	{
		shift_set( a.shift_ );
	}

	// Base Constructor
	inline
	FArray1A( Base const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.isize() )
	{
		shift_set( 1 );
	}

	// Tail Constructor
	inline
	FArray1A( Tail const & s ) :
	 Super( s, ProxySentinel() ),
	 I_( s.isize() )
	{
		shift_set( 1 );
	}

	// Value Constructor
	inline
	FArray1A( T const & t ) :
	 Super( t, ProxySentinel() ),
	 I_( star ) // Unbounded
	{
		shift_set( 1 );
	}

	// Copy + IndexRange Constructor
	inline
	FArray1A( FArray1A const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Real + IndexRange Constructor
	inline
	FArray1A( real_FArray const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Proxy + IndexRange Constructor
	inline
	FArray1A( proxy_FArray const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Super + IndexRange Constructor
	inline
	FArray1A( Super const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Base + IndexRange Constructor
	inline
	FArray1A( Base const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Tail + IndexRange Constructor
	inline
	FArray1A( Tail const & s, IR const & I ) :
	 Super( s, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Value + IndexRange Constructor
	inline
	FArray1A( T const & t, IR const & I ) :
	 Super( t, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Constructor
	inline
	FArray1A( FArray1A & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ )
	{
		shift_set( a.shift_ );
	}

	// Non-Const Real Constructor
	inline
	FArray1A( real_FArray & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ )
	{
		shift_set( a.shift_ );
	}

	// Non-Const Proxy Constructor
	inline
	FArray1A( proxy_FArray & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ )
	{
		shift_set( a.shift_ );
	}

	// Non-Const Super Constructor
	inline
	FArray1A( Super & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I() )
	{
		shift_set( a.shift_ );
	}

	// Non-Const Base Constructor
	inline
	FArray1A( Base & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.isize() )
	{
		shift_set( 1 );
	}

	// Non-Const Tail Constructor
	inline
	FArray1A( Tail & s ) :
	 Super( s, ProxySentinel() ),
	 I_( s.isize() )
	{
		shift_set( 1 );
	}

	// Non-Const Value Constructor
	inline
	FArray1A( T & t ) :
	 Super( t, ProxySentinel() ),
	 I_( star ) // Unbounded
	{
		shift_set( 1 );
	}

	// Non-Const Copy + IndexRange Constructor
	inline
	FArray1A( FArray1A & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Non-Const Real + IndexRange Constructor
	inline
	FArray1A( real_FArray & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Non-Const Proxy + IndexRange Constructor
	inline
	FArray1A( proxy_FArray & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Non-Const Super + IndexRange Constructor
	inline
	FArray1A( Super & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Non-Const Base + IndexRange Constructor
	inline
	FArray1A( Base & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Non-Const Tail + IndexRange Constructor
	inline
	FArray1A( Tail & s, IR const & I ) :
	 Super( s, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

	// Non-Const Value + IndexRange Constructor
	inline
	FArray1A( T & t, IR const & I ) :
	 Super( t, ProxySentinel() ),
	 I_( I )
	{
		dimension_argument();
	}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Destructor
	inline
	virtual
	~FArray1A()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray1A &
	operator =( FArray1A const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray1A &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( FArray1< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( FArray1S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray1A &
	operator =( MArray1< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( std::array< U, s > const & a )
	{
		Base::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( std::vector< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( Vector2< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator =( Vector3< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( FArray1< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( FArray1< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( FArray1< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( FArray1< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( FArray1S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( FArray1S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( FArray1S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( FArray1S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray1A &
	operator +=( MArray1< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray1A &
	operator -=( MArray1< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray1A &
	operator *=( MArray1< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray1A &
	operator /=( MArray1< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( std::initializer_list< U > const l )
	{
		Base::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( std::initializer_list< U > const l )
	{
		Base::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( std::initializer_list< U > const l )
	{
		Base::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( std::initializer_list< U > const l )
	{
		Base::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( std::array< U, s > const & a )
	{
		Base::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( std::array< U, s > const & a )
	{
		Base::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( std::array< U, s > const & a )
	{
		Base::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( std::array< U, s > const & a )
	{
		Base::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( std::vector< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( std::vector< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( std::vector< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( std::vector< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( Vector2< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( Vector2< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( Vector2< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( Vector2< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator +=( Vector3< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator -=( Vector3< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator *=( Vector3< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	operator /=( Vector3< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( FArray1< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( FArray1< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( FArray1S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( FArray1S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray1A &
	and_equals( MArray1< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray1A &
	or_equals( MArray1< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1A &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray1A &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray1A &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray1A &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray1A &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray1A &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Subscript

	// Const Tail Starting at array( i )
	inline
	Tail const
	a( int const i ) const
	{
		assert( I_.contains( i ) );
		return Tail( static_cast< T const * >( sdata_ + i ), ( data_size_ != npos ? data_size_ - ( i - shift_ ) : npos ) );
	}

	// Tail Starting at array( i )
	inline
	Tail
	a( int const i )
	{
		proxy_const_assert( not_const_proxy() );
		assert( I_.contains( i ) );
		return Tail( sdata_ + i, ( data_size_ != npos ? data_size_ - ( i - shift_ ) : npos ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i ) const
	{
		return ( i - shift_ );
	}

public: // Predicate

	// Dimensions Initialized?
	inline
	bool
	dimensions_initialized() const
	{
		return true;
	}

	// Contains Indexed Element?
	inline
	bool
	contains( int const i ) const
	{
		return I_.contains( i );
	}

	// Initializer Active?
	inline
	bool
	initializer_active() const
	{
		return false;
	}

public: // Inspector

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

public: // Modifier

	// Clear
	inline
	FArray1A &
	clear()
	{
		Super::clear();
		I_.clear();
		return *this;
	}

	// Dimension by IndexRange Even if Const
	inline
	FArray1A const &
	dim( IR const & I ) const
	{
		const_cast< FArray1A & >( *this ).dimension( I );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	inline
	FArray1A const &
	dim( FArray1< U > const & a ) const
	{
		const_cast< FArray1A & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray1A &
	dimension( IR const & I )
	{
		I_.assign_value_of( I );
		dimension_argument();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	inline
	FArray1A &
	dimension( FArray1< U > const & a )
	{
		I_.assign_value_of( a.I() );
		dimension_argument();
		return *this;
	}

	// Attach to Argument Array
	inline
	FArray1A &
	attach( FArray1A const & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I_ );
		return *this;
	}

	// Attach to Non-Const Argument Array
	inline
	FArray1A &
	attach( FArray1A & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I_ );
		return *this;
	}

	// Attach to Real Array
	inline
	FArray1A &
	attach( real_FArray const & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I_ );
		return *this;
	}

	// Attach to Non-Const Real Array
	inline
	FArray1A &
	attach( real_FArray & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I_ );
		return *this;
	}

	// Attach to Proxy Array
	inline
	FArray1A &
	attach( proxy_FArray const & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I_ );
		return *this;
	}

	// Attach to Non-Const Proxy Array
	inline
	FArray1A &
	attach( proxy_FArray & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I_ );
		return *this;
	}

	// Attach to Super Array
	inline
	FArray1A &
	attach( Super const & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I() );
		return *this;
	}

	// Attach to Non-Const Super Array
	inline
	FArray1A &
	attach( Super & a )
	{
		Base::attach( a );
		I_.assign_value_of( a.I() );
		return *this;
	}

	// Attach to Base Array
	inline
	FArray1A &
	attach( Base const & a )
	{
		Base::attach( a, 1 );
		I_ = a.size();
		return *this;
	}

	// Attach to Non-Const Base Array
	inline
	FArray1A &
	attach( Base & a )
	{
		Base::attach( a, 1 );
		I_ = a.size();
		return *this;
	}

	// Attach to Tail
	inline
	FArray1A &
	attach( Tail const & s )
	{
		Base::attach( s, 1 );
		I_ = s.size();
		return *this;
	}

	// Attach to Non-Const Tail
	inline
	FArray1A &
	attach( Tail & s )
	{
		Base::attach( s, 1 );
		I_ = s.size();
		return *this;
	}

	// Attach to Value
	inline
	FArray1A &
	attach( T const & t )
	{
		Base::attach( t, 1 );
		I_ = star; // Unbounded
		return *this;
	}

	// Attach to Non-Const Value
	inline
	FArray1A &
	attach( T & t )
	{
		Base::attach( t, 1 );
		I_ = star; // Unbounded
		return *this;
	}

	// Detach from Source Array
	inline
	FArray1A &
	detach()
	{
		Base::detach();
		I_.clear();
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( SIR const & I )
	{
		I_.assign_value_of( I );
		dimension_argument();
	}

private: // Functions

	// Dimension by Current IndexRange
	inline
	void
	dimension_argument()
	{
		if ( I_.bounded() ) { // Bounded
			size_set( I_.size() );
		} else if ( data_size_ != npos ) { // Unbounded with bounded data array
			// Infer upper index and size
			I_.u( I_.lz() + static_cast< int >( data_size_ ) - 1 );
			size_set( I_.size() );
		} else { // Unbounded with unbounded data array
			size_set( npos );
		}
		shift_set( I_.lz() );
	}

private: // Data

	IR I_; // Index range

}; // FArray1A

} // ObjexxFCL

#endif // ObjexxFCL_FArray1A_hh_INCLUDED
