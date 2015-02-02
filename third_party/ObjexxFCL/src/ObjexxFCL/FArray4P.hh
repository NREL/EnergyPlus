#ifndef ObjexxFCL_FArray4P_hh_INCLUDED
#define ObjexxFCL_FArray4P_hh_INCLUDED

// FArray4P: Fortran-Compatible 4D Proxy Array
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
#include <ObjexxFCL/FArray4P.fwd.hh>
#include <ObjexxFCL/FArray4D.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>

namespace ObjexxFCL {

// FArray4P: Fortran-Compatible 4D Proxy Array
template< typename T >
class FArray4P : public FArray4< T >, public ObserverMulti
{

private: // Types

	typedef  FArray4< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  internal::ProxySentinel  ProxySentinel;

private: // Friend

	friend class FArray4A< T >;

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
	using Super::size_of;
	using Super::size_set;
	using Super::update_to;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
	using Super::z1_;
	using Super::z2_;
	using Super::z3_;
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	using Super::const_proxy;
	using Super::not_const_proxy;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Default Constructor
	inline
	FArray4P() :
	 Super( ProxySentinel() ),
	 source_( 0 )
	{}

	// Copy Constructor
	inline
	FArray4P( FArray4P const & a ) :
	 Super( a, ProxySentinel() ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		insert_as_observer();
	}

	// Real Constructor
	inline
	FArray4P( real_FArray const & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		insert_as_observer();
	}

	// Super Constructor
	inline
	FArray4P( Super const & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1() ),
	 I2_( a.I2() ),
	 I3_( a.I3() ),
	 I4_( a.I4() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		insert_as_observer();
	}

	// Base Constructor
	inline
	FArray4P( Base const & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( a.isize() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( 4 );
		z1_ = z2_ = z3_ = 1;
		insert_as_observer();
	}

	// Tail Constructor
	inline
	FArray4P( Tail const & s ) :
	 Super( s, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( s.isize() ),
	 source_( 0 )
	{
		shift_set( 4 );
		z1_ = z2_ = z3_ = 1;
		insert_as_observer();
	}

	// Value Constructor
	inline
	FArray4P( T const & t ) :
	 Super( t, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( star ), // Unbounded
	 source_( 0 )
	{
		shift_set( 4 );
		z1_ = z2_ = z3_ = 1;
		insert_as_observer();
	}

	// Copy + IndexRange Constructor
	inline
	FArray4P( FArray4P const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Real + IndexRange Constructor
	inline
	FArray4P( real_FArray const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Super + IndexRange Constructor
	inline
	FArray4P( Super const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Base + IndexRange Constructor
	inline
	FArray4P( Base const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Tail + IndexRange Constructor
	inline
	FArray4P( Tail const & s, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( s, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Value + IndexRange Constructor
	inline
	FArray4P( T const & t, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( t, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Constructor
	inline
	FArray4P( FArray4P & a ) :
	 Super( a, ProxySentinel() ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		insert_as_observer();
	}

	// Non-Const Real Constructor
	inline
	FArray4P( real_FArray & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 I3_( a.I3_ ),
	 I4_( a.I4_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		insert_as_observer();
	}

	// Non-Const Super Constructor
	inline
	FArray4P( Super & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1() ),
	 I2_( a.I2() ),
	 I3_( a.I3() ),
	 I4_( a.I4() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		insert_as_observer();
	}

	// Non-Const Base Constructor
	inline
	FArray4P( Base & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( a.isize() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( 4 );
		z1_ = z2_ = z3_ = 1;
		insert_as_observer();
	}

	// Non-Const Tail Constructor
	inline
	FArray4P( Tail & s ) :
	 Super( s, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( s.isize() ),
	 source_( 0 )
	{
		shift_set( 4 );
		z1_ = z2_ = z3_ = 1;
		insert_as_observer();
	}

	// Non-Const Value Constructor
	inline
	FArray4P( T & t ) :
	 Super( t, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( 1 ),
	 I3_( 1 ),
	 I4_( star ), // Unbounded
	 source_( 0 )
	{
		shift_set( 4 );
		z1_ = z2_ = z3_ = 1;
		insert_as_observer();
	}

	// Non-Const Copy + IndexRange Constructor
	inline
	FArray4P( FArray4P & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Real + IndexRange Constructor
	inline
	FArray4P( real_FArray & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Super + IndexRange Constructor
	inline
	FArray4P( Super & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Base + IndexRange Constructor
	inline
	FArray4P( Base & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Tail + IndexRange Constructor
	inline
	FArray4P( Tail & s, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( s, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Value + IndexRange Constructor
	inline
	FArray4P( T & t, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( t, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 I3_( I3 ),
	 I4_( I4 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Destructor
	inline
	virtual
	~FArray4P()
	{
		if ( source_ ) source_->remove_observer( *this );
	}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray4P &
	operator =( FArray4P const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray4P &
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
	FArray4P &
	operator =( FArray4< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator =( FArray4S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray4P &
	operator =( MArray4< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator +=( FArray4< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator -=( FArray4< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator *=( FArray4< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator /=( FArray4< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator +=( FArray4S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator -=( FArray4S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator *=( FArray4S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	operator /=( FArray4S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray4P &
	operator +=( MArray4< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray4P &
	operator -=( MArray4< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray4P &
	operator *=( MArray4< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray4P &
	operator /=( MArray4< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	and_equals( FArray4< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	or_equals( FArray4< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	and_equals( FArray4S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray4P &
	or_equals( FArray4S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray4P &
	and_equals( MArray4< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray4P &
	or_equals( MArray4< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray4P &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray4P &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray4P &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray4P &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray4P &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Subscript

	// Const Tail Starting at array( i1, i2, i3, i4 )
	inline
	Tail const
	a( int const i1, int const i2, int const i3, int const i4 ) const
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) && ( I4_.contains( i4 ) ) );
		size_type const offset( ( ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Tail Starting at array( i1, i2, i3, i4 )
	inline
	Tail
	a( int const i1, int const i2, int const i3, int const i4 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) && ( I4_.contains( i4 ) ) );
		size_type const offset( ( ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2, int const i3, int const i4 ) const
	{
		assert( ( I1_.initialized() ) && ( I2_.initialized() ) && ( I3_.initialized() ) && ( I4_.initialized() ) );
		return ( ( ( ( ( ( ( i4 * z3_ ) + i3 ) * z2_ ) + i2 ) * z1_ ) + i1 ) - shift_ );
	}

public: // Predicate

	// Dimensions Initialized?
	inline
	bool
	dimensions_initialized() const
	{
		return ( ( I1_.initialized() ) && ( I2_.initialized() ) && ( I3_.initialized() ) && ( I4_.initialized() ) );
	}

	// Contains Indexed Element?
	inline
	bool
	contains( int const i1, int const i2, int const i3, int const i4 ) const
	{
		return ( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) && ( I3_.contains( i3 ) ) && ( I4_.contains( i4 ) ) );
	}

	// Initializer Active?
	inline
	bool
	initializer_active() const
	{
		return false;
	}

public: // Inspector

	// IndexRange of Dimension 1
	inline
	IR const &
	I1() const
	{
		return I1_;
	}

	// Lower Index of Dimension 1
	inline
	int
	l1() const
	{
		return I1_.l();
	}

	// Upper Index of Dimension 1
	inline
	int
	u1() const
	{
		return I1_.u();
	}

	// IndexRange of Dimension 2
	inline
	IR const &
	I2() const
	{
		return I2_;
	}

	// Lower Index of Dimension 2
	inline
	int
	l2() const
	{
		return I2_.l();
	}

	// Upper Index of Dimension 2
	inline
	int
	u2() const
	{
		return I2_.u();
	}

	// IndexRange of Dimension 3
	inline
	IR const &
	I3() const
	{
		return I3_;
	}

	// Lower Index of Dimension 3
	inline
	int
	l3() const
	{
		return I3_.l();
	}

	// Upper Index of Dimension 3
	inline
	int
	u3() const
	{
		return I3_.u();
	}

	// IndexRange of Dimension 4
	inline
	IR const &
	I4() const
	{
		return I4_;
	}

	// Lower Index of Dimension 4
	inline
	int
	l4() const
	{
		return I4_.l();
	}

	// Upper Index of Dimension 4
	inline
	int
	u4() const
	{
		return I4_.u();
	}

	// Size of Dimension 4
	inline
	size_type
	size4() const
	{
		return I4_.size();
	}

	// Size of Dimension 4
	inline
	int
	isize4() const
	{
		return I4_.isize();
	}

public: // Modifier

	// Clear
	inline
	FArray4P &
	clear()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		I3_.clear_no_notify();
		I4_.clear_no_notify();
		source_ = 0;
		return *this;
	}

	// Dimension by IndexRange Even if Const
	inline
	FArray4P const &
	dim( IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) const
	{
		const_cast< FArray4P & >( *this ).dimension( I1, I2, I3, I4 );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	inline
	FArray4P const &
	dim( FArray4< U > const & a ) const
	{
		const_cast< FArray4P & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray4P &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		dimension_proxy();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	inline
	FArray4P &
	dimension( FArray4< U > const & a )
	{
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		I3_.assign_no_notify( a.I3() );
		I4_.assign_no_notify( a.I4() );
		dimension_proxy();
		return *this;
	}

	// Attach to Proxy Array
	inline
	FArray4P &
	attach( FArray4P const & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		I3_ = a.I3_;
		I4_ = a.I4_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Proxy Array
	inline
	FArray4P &
	attach( FArray4P & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		I3_ = a.I3_;
		I4_ = a.I4_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Real Array
	inline
	FArray4P &
	attach( real_FArray const & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		I3_ = a.I3_;
		I4_ = a.I4_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Real Array
	inline
	FArray4P &
	attach( real_FArray & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		I3_ = a.I3_;
		I4_ = a.I4_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Super Array
	inline
	FArray4P &
	attach( Super const & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		I1_ = a.I1();
		I2_ = a.I2();
		I3_ = a.I3();
		I4_ = a.I4();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Super Array
	inline
	FArray4P &
	attach( Super & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		I1_ = a.I1();
		I2_ = a.I2();
		I3_ = a.I3();
		I4_ = a.I4();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Base Array
	inline
	FArray4P &
	attach( Base const & a )
	{
		Base::attach( a, 4 );
		z1_ = z2_ = z3_ = 1;
		I1_ = 1;
		I2_ = 1;
		I3_ = 1;
		I4_ = a.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Base Array
	inline
	FArray4P &
	attach( Base & a )
	{
		Base::attach( a, 4 );
		z1_ = z2_ = z3_ = 1;
		I1_ = 1;
		I2_ = 1;
		I3_ = 1;
		I4_ = a.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Tail
	inline
	FArray4P &
	attach( Tail const & s )
	{
		Base::attach( s, 4 );
		z1_ = z2_ = z3_ = 1;
		I1_ = 1;
		I2_ = 1;
		I3_ = 1;
		I4_ = s.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Non-Const Tail
	inline
	FArray4P &
	attach( Tail & s )
	{
		Base::attach( s, 4 );
		z1_ = z2_ = z3_ = 1;
		I1_ = 1;
		I2_ = 1;
		I3_ = 1;
		I4_ = s.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Value
	inline
	FArray4P &
	attach( T const & t )
	{
		Base::attach( t, 4 );
		z1_ = z2_ = z3_ = 1;
		I1_ = 1;
		I2_ = 1;
		I3_ = 1;
		I4_ = star; // Unbounded
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Non-Const Value
	inline
	FArray4P &
	attach( T & t )
	{
		Base::attach( t, 4 );
		z1_ = z2_ = z3_ = 1;
		I1_ = 1;
		I2_ = 1;
		I3_ = 1;
		I4_ = star; // Unbounded
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Detach from Source Array
	inline
	FArray4P &
	detach()
	{
		Base::detach();
		z1_ = z2_ = z3_ = 0;
		I1_.clear();
		I2_.clear();
		I3_.clear();
		I4_.clear();
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
			z1_ = z2_ = z3_ = 0;
			I1_.clear();
			I2_.clear();
			I3_.clear();
			I4_.clear();
			source_ = 0;
		}
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( SIR const & I1, SIR const & I2, SIR const & I3, SIR const & I4 )
	{
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		I3_.assign_no_notify( I3 );
		I4_.assign_no_notify( I4 );
		dimension_proxy();
	}

private: // Functions

	// Dimension by Current IndexRanges
	inline
	void
	dimension_proxy()
	{
		assert( I1_.not_unbounded() );
		assert( I2_.not_unbounded() );
		assert( I3_.not_unbounded() );
		z1_ = I1_.size();
		z2_ = I2_.size();
		z3_ = I3_.size();
		if ( dimensions_initialized() ) {
			if ( I4_.bounded() ) { // Bounded
				size_set( size_of( z1_, z2_, z3_, I4_.size() ) );
			} else if ( data_size_ != npos ) { // Unbounded with bounded data array
				size_type const slice_size( size_of( z1_, z2_, z3_ ) );
				if ( slice_size > 0u ) { // Infer upper index and size
					I4_.u_no_notify( I4_.lz() + static_cast< int >( data_size_ / slice_size ) - 1 );
					size_set( size_of( slice_size, I4_.size() ) );
				} else {
					size_set( data_size_ );
				}
			} else { // Unbounded with unbounded data array
				size_set( npos );
			}
			shift_set( ( ( ( ( ( I4_.lz() * z3_ ) + I3_.lz() ) * z2_ ) + I2_.lz() ) * z1_ ) + I1_.lz() );
		} else {
			size_set( 0 );
			shift_set( 0 );
		}
	}

	// Insert as Observer of the IndexRanges and Source Array
	inline
	void
	insert_as_observer()
	{
		I1_.insert_observer( *this );
		I2_.insert_observer( *this );
		I3_.insert_observer( *this );
		I4_.insert_observer( *this );
		if ( source_ ) source_->insert_observer( *this );
	}

	// Remove as Observer of the IndexRanges and Source Array
	inline
	void
	remove_as_observer()
	{
		I1_.remove_observer( *this );
		I2_.remove_observer( *this );
		I3_.remove_observer( *this );
		I4_.remove_observer( *this );
		if ( source_ ) source_->remove_observer( *this );
	}

private: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2
	IR I3_; // Index range of dim 3
	IR I4_; // Index range of dim 4

	SubjectMulti const * source_; // Pointer (non-owning) to source array (0 if unknown)

}; // FArray4P

} // ObjexxFCL

#endif // ObjexxFCL_FArray4P_hh_INCLUDED
