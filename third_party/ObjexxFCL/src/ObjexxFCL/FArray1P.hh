#ifndef ObjexxFCL_FArray1P_hh_INCLUDED
#define ObjexxFCL_FArray1P_hh_INCLUDED

// FArray1P: Fortran-Compatible 1D Proxy Array
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
#include <ObjexxFCL/FArray1P.fwd.hh>
#include <ObjexxFCL/FArray1D.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>

namespace ObjexxFCL {

// FArray1P: Fortran-Compatible 1D Proxy Array
template< typename T >
class FArray1P : public FArray1< T >, public ObserverMulti
{

private: // Types

	typedef  FArray1< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  internal::ProxySentinel  ProxySentinel;

private: // Friend

	friend class FArray1A< T >;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Base::Tail  Tail;
	typedef  typename Super::IR  SIR;
	typedef  DynamicIndexRange  IR;

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
	using Super::update_to;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	using Super::const_proxy;
	using Super::not_const_proxy;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Default Constructor
	inline
	FArray1P() :
	 Super( ProxySentinel() ),
	 source_( 0 )
	{}

	// Copy Constructor
	inline
	FArray1P( FArray1P const & a ) :
	 Super( a, ProxySentinel() ),
	 ObserverMulti(),
	 I_( a.I_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		insert_as_observer();
	}

	// Real Constructor
	inline
	FArray1P( real_FArray const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		insert_as_observer();
	}

	// Super Constructor
	inline
	FArray1P( Super const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( a.shift_ );
		insert_as_observer();
	}

	// Base Constructor
	inline
	FArray1P( Base const & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.isize() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( 1 );
		insert_as_observer();
	}

	// Tail Constructor
	inline
	FArray1P( Tail const & s ) :
	 Super( s, ProxySentinel() ),
	 I_( s.isize() ),
	 source_( 0 )
	{
		shift_set( 1 );
		insert_as_observer();
	}

	// Value Constructor
	inline
	FArray1P( T const & t ) :
	 Super( t, ProxySentinel() ),
	 I_( star ), // Unbounded
	 source_( 0 )
	{
		shift_set( 1 );
		insert_as_observer();
	}

	// Copy + IndexRange Constructor
	inline
	FArray1P( FArray1P const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Real + IndexRange Constructor
	inline
	FArray1P( real_FArray const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Super + IndexRange Constructor
	inline
	FArray1P( Super const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Base + IndexRange Constructor
	inline
	FArray1P( Base const & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Tail + IndexRange Constructor
	inline
	FArray1P( Tail const & s, IR const & I ) :
	 Super( s, ProxySentinel() ),
	 I_( I ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Value + IndexRange Constructor
	inline
	FArray1P( T const & t, IR const & I ) :
	 Super( t, ProxySentinel() ),
	 I_( I ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Constructor
	inline
	FArray1P( FArray1P & a ) :
	 Super( a, ProxySentinel() ),
	 ObserverMulti(),
	 I_( a.I_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		insert_as_observer();
	}

	// Non-Const Real Constructor
	inline
	FArray1P( real_FArray & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		insert_as_observer();
	}

	// Non-Const Super Constructor
	inline
	FArray1P( Super & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.I() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( a.shift_ );
		insert_as_observer();
	}

	// Non-Const Base Constructor
	inline
	FArray1P( Base & a ) :
	 Super( a, ProxySentinel() ),
	 I_( a.isize() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( 1 );
		insert_as_observer();
	}

	// Non-Const Tail Constructor
	inline
	FArray1P( Tail & s ) :
	 Super( s, ProxySentinel() ),
	 I_( s.isize() ),
	 source_( 0 )
	{
		shift_set( 1 );
		insert_as_observer();
	}

	// Non-Const Value Constructor
	inline
	FArray1P( T & t ) :
	 Super( t, ProxySentinel() ),
	 I_( star ), // Unbounded
	 source_( 0 )
	{
		shift_set( 1 );
		insert_as_observer();
	}

	// Non-Const Copy + IndexRange Constructor
	inline
	FArray1P( FArray1P & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Real + IndexRange Constructor
	inline
	FArray1P( real_FArray & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Super + IndexRange Constructor
	inline
	FArray1P( Super & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Base + IndexRange Constructor
	inline
	FArray1P( Base & a, IR const & I ) :
	 Super( a, ProxySentinel() ),
	 I_( I ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Tail + IndexRange Constructor
	inline
	FArray1P( Tail & s, IR const & I ) :
	 Super( s, ProxySentinel() ),
	 I_( I ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Value + IndexRange Constructor
	inline
	FArray1P( T & t, IR const & I ) :
	 Super( t, ProxySentinel() ),
	 I_( I ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Destructor
	inline
	virtual
	~FArray1P()
	{
		if ( source_ ) source_->remove_observer( *this );
	}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray1P &
	operator =( FArray1P const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray1P &
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
	FArray1P &
	operator =( FArray1< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator =( FArray1S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray1P &
	operator =( MArray1< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator =( std::array< U, s > const & a )
	{
		Base::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator =( std::vector< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator =( Vector2< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator =( Vector3< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( FArray1< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( FArray1< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( FArray1< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( FArray1< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( FArray1S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( FArray1S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( FArray1S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( FArray1S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray1P &
	operator +=( MArray1< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray1P &
	operator -=( MArray1< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray1P &
	operator *=( MArray1< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray1P &
	operator /=( MArray1< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( std::initializer_list< U > const l )
	{
		Base::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( std::initializer_list< U > const l )
	{
		Base::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( std::initializer_list< U > const l )
	{
		Base::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( std::initializer_list< U > const l )
	{
		Base::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( std::array< U, s > const & a )
	{
		Base::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( std::array< U, s > const & a )
	{
		Base::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( std::array< U, s > const & a )
	{
		Base::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( std::array< U, s > const & a )
	{
		Base::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( std::vector< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( std::vector< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( std::vector< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( std::vector< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( Vector2< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( Vector2< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( Vector2< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( Vector2< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator +=( Vector3< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator -=( Vector3< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator *=( Vector3< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	operator /=( Vector3< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( FArray1< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( FArray1< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( FArray1S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( FArray1S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray1P &
	and_equals( MArray1< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray1P &
	or_equals( MArray1< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1P &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray1P &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray1P &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray1P &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray1P &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray1P &
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
		assert( I_.initialized() );
		return ( i - shift_ );
	}

public: // Predicate

	// Dimensions Initialized?
	inline
	bool
	dimensions_initialized() const
	{
		return I_.initialized();
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
	FArray1P &
	clear()
	{
		Super::clear();
		I_.clear_no_notify();
		source_ = 0;
		return *this;
	}

	// Dimension by IndexRange Even if Const
	inline
	FArray1P const &
	dim( IR const & I ) const
	{
		const_cast< FArray1P & >( *this ).dimension( I );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	inline
	FArray1P const &
	dim( FArray1< U > const & a ) const
	{
		const_cast< FArray1P & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray1P &
	dimension( IR const & I )
	{
		I_.assign_no_notify( I );
		dimension_proxy();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	inline
	FArray1P &
	dimension( FArray1< U > const & a )
	{
		I_.assign_no_notify( a.I() );
		dimension_proxy();
		return *this;
	}

	// Attach to Proxy Array
	inline
	FArray1P &
	attach( FArray1P const & a )
	{
		Base::attach( a );
		I_ = a.I_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Proxy Array
	inline
	FArray1P &
	attach( FArray1P & a )
	{
		Base::attach( a );
		I_ = a.I_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Real Array
	inline
	FArray1P &
	attach( real_FArray const & a )
	{
		Base::attach( a );
		I_ = a.I_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Real Array
	inline
	FArray1P &
	attach( real_FArray & a )
	{
		Base::attach( a );
		I_ = a.I_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Super Array
	inline
	FArray1P &
	attach( Super const & a )
	{
		Base::attach( a );
		I_ = a.I();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Super Array
	inline
	FArray1P &
	attach( Super & a )
	{
		Base::attach( a );
		I_ = a.I();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Base Array
	inline
	FArray1P &
	attach( Base const & a )
	{
		Base::attach( a, 1 );
		I_ = a.size();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Base Array
	inline
	FArray1P &
	attach( Base & a )
	{
		Base::attach( a, 1 );
		I_ = a.size();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Tail
	inline
	FArray1P &
	attach( Tail const & s )
	{
		Base::attach( s, 1 );
		I_ = s.size();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Non-Const Tail
	inline
	FArray1P &
	attach( Tail & s )
	{
		Base::attach( s, 1 );
		I_ = s.size();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Value
	inline
	FArray1P &
	attach( T const & t )
	{
		Base::attach( t, 1 );
		I_ = star; // Unbounded
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Non-Const Value
	inline
	FArray1P &
	attach( T & t )
	{
		Base::attach( t, 1 );
		I_ = star; // Unbounded
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Detach from Source Array
	inline
	FArray1P &
	detach()
	{
		Base::detach();
		I_.clear();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

public: // Observer Modifier

	// Update
	inline
	void
	update()
	{
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
		if ( source_ ) {
			if ( const_proxy() ) {
				update_to( *dynamic_cast< Base const * >( source_ ) );
			} else {
				update_to( *dynamic_cast< Base * >( const_cast< SubjectMulti * >( source_ ) ) );
			}
		}
#else
		if ( source_ ) update_to( *dynamic_cast< Base const * >( source_ ) );
#endif // OBJEXXFCL_PROXY_CONST_CHECKS
		dimension_proxy();
	}

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & subject )
	{
		if ( ( source_ ) && ( &subject == source_ ) ) { // Source array is being destructed
			Base::detach();
			I_.clear();
			source_ = 0;
		}
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( SIR const & I )
	{
		I_.assign_no_notify( I );
		dimension_proxy();
	}

private: // Functions

	// Dimension by Current IndexRange
	inline
	void
	dimension_proxy()
	{
		if ( dimensions_initialized() ) {
			if ( I_.bounded() ) { // Bounded
				size_set( I_.size() );
			} else if ( data_size_ != npos ) { // Unbounded with bounded data array
				// Infer upper index and size
				I_.u_no_notify( I_.lz() + static_cast< int >( data_size_ ) - 1 );
				size_set( I_.size() );
			} else { // Unbounded with unbounded data array
				size_set( npos );
			}
			shift_set( I_.lz() );
		} else {
			size_set( 0 );
			shift_set( 0 );
		}
	}

	// Insert as Observer of the IndexRange and Source Array
	inline
	void
	insert_as_observer()
	{
		I_.insert_observer( *this );
		if ( source_ ) source_->insert_observer( *this );
	}

	// Remove as Observer of the IndexRange and Source Array
	inline
	void
	remove_as_observer()
	{
		I_.remove_observer( *this );
		if ( source_ ) source_->remove_observer( *this );
	}

private: // Data

	IR I_; // Index range

	SubjectMulti const * source_; // Pointer (non-owning) to source array (0 if unknown)

}; // FArray1P

} // ObjexxFCL

#endif // ObjexxFCL_FArray1P_hh_INCLUDED
