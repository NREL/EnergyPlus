#ifndef ObjexxFCL_FArray2P_hh_INCLUDED
#define ObjexxFCL_FArray2P_hh_INCLUDED

// FArray2P: Fortran-Compatible 2D Proxy Array
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
#include <ObjexxFCL/FArray2P.fwd.hh>
#include <ObjexxFCL/FArray2D.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>

namespace ObjexxFCL {

// FArray2P: Fortran-Compatible 2D Proxy Array
template< typename T >
class FArray2P : public FArray2< T >, public ObserverMulti
{

private: // Types

	typedef  FArray2< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  internal::ProxySentinel  ProxySentinel;

private: // Friend

	friend class FArray2A< T >;

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
#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
	using Super::const_proxy;
	using Super::not_const_proxy;
#endif // OBJEXXFCL_PROXY_CONST_CHECKS

public: // Creation

	// Default Constructor
	inline
	FArray2P() :
	 Super( ProxySentinel() ),
	 source_( 0 )
	{}

	// Copy Constructor
	inline
	FArray2P( FArray2P const & a ) :
	 Super( a, ProxySentinel() ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		insert_as_observer();
	}

	// Real Constructor
	inline
	FArray2P( real_FArray const & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		insert_as_observer();
	}

	// Super Constructor
	inline
	FArray2P( Super const & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1() ),
	 I2_( a.I2() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		insert_as_observer();
	}

	// Base Constructor
	inline
	FArray2P( Base const & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( a.isize() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( 2 );
		z1_ = 1;
		insert_as_observer();
	}

	// Tail Constructor
	inline
	FArray2P( Tail const & s ) :
	 Super( s, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( s.isize() ),
	 source_( 0 )
	{
		shift_set( 2 );
		z1_ = 1;
		insert_as_observer();
	}

	// Value Constructor
	inline
	FArray2P( T const & t ) :
	 Super( t, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( star ), // Unbounded
	 source_( 0 )
	{
		shift_set( 2 );
		z1_ = 1;
		insert_as_observer();
	}

	// Copy + IndexRange Constructor
	inline
	FArray2P( FArray2P const & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Real + IndexRange Constructor
	inline
	FArray2P( real_FArray const & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Super + IndexRange Constructor
	inline
	FArray2P( Super const & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Base + IndexRange Constructor
	inline
	FArray2P( Base const & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Tail + IndexRange Constructor
	inline
	FArray2P( Tail const & s, IR const & I1, IR const & I2 ) :
	 Super( s, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Value + IndexRange Constructor
	inline
	FArray2P( T const & t, IR const & I1, IR const & I2 ) :
	 Super( t, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS

	// Non-Const Copy Constructor
	inline
	FArray2P( FArray2P & a ) :
	 Super( a, ProxySentinel() ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		insert_as_observer();
	}

	// Non-Const Real Constructor
	inline
	FArray2P( real_FArray & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ ),
	 source_( &a )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		insert_as_observer();
	}

	// Non-Const Super Constructor
	inline
	FArray2P( Super & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( a.I1() ),
	 I2_( a.I2() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( a.shift_ );
		z1_ = a.z1_;
		insert_as_observer();
	}

	// Non-Const Base Constructor
	inline
	FArray2P( Base & a ) :
	 Super( a, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( a.isize() ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		shift_set( 2 );
		z1_ = 1;
		insert_as_observer();
	}

	// Non-Const Tail Constructor
	inline
	FArray2P( Tail & s ) :
	 Super( s, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( s.isize() ),
	 source_( 0 )
	{
		shift_set( 2 );
		z1_ = 1;
		insert_as_observer();
	}

	// Non-Const Value Constructor
	inline
	FArray2P( T & t ) :
	 Super( t, ProxySentinel() ),
	 I1_( 1 ),
	 I2_( star ), // Unbounded
	 source_( 0 )
	{
		shift_set( 2 );
		z1_ = 1;
		insert_as_observer();
	}

	// Non-Const Copy + IndexRange Constructor
	inline
	FArray2P( FArray2P & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Real + IndexRange Constructor
	inline
	FArray2P( real_FArray & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( &a )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Super + IndexRange Constructor
	inline
	FArray2P( Super & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Base + IndexRange Constructor
	inline
	FArray2P( Base & a, IR const & I1, IR const & I2 ) :
	 Super( a, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( dynamic_cast< SubjectMulti const * >( &a ) )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Tail + IndexRange Constructor
	inline
	FArray2P( Tail & s, IR const & I1, IR const & I2 ) :
	 Super( s, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

	// Non-Const Value + IndexRange Constructor
	inline
	FArray2P( T & t, IR const & I1, IR const & I2 ) :
	 Super( t, ProxySentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 source_( 0 )
	{
		dimension_proxy();
		insert_as_observer();
	}

#endif // OBJEXXFCL_PROXY_CONST_CHECKS

	// Destructor
	inline
	virtual
	~FArray2P()
	{
		if ( source_ ) source_->remove_observer( *this );
	}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray2P &
	operator =( FArray2P const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray2P &
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
	FArray2P &
	operator =( FArray2< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator =( FArray2S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray2P &
	operator =( MArray2< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator +=( FArray2< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator -=( FArray2< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator *=( FArray2< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator /=( FArray2< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator +=( FArray2S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator -=( FArray2S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator *=( FArray2S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	operator /=( FArray2S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray2P &
	operator +=( MArray2< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray2P &
	operator -=( MArray2< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray2P &
	operator *=( MArray2< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray2P &
	operator /=( MArray2< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	and_equals( FArray2< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	or_equals( FArray2< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	and_equals( FArray2S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2P &
	or_equals( FArray2S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray2P &
	and_equals( MArray2< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray2P &
	or_equals( MArray2< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray2P &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray2P &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray2P &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray2P &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray2P &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Subscript

	// Const Tail Starting at array( i1, i2 )
	inline
	Tail const
	a( int const i1, int const i2 ) const
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) );
		size_type const offset( ( ( i2 * z1_ ) + i1 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Tail Starting at array( i1, i2 )
	inline
	Tail
	a( int const i1, int const i2 )
	{
		proxy_const_assert( not_const_proxy() );
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) );
		size_type const offset( ( ( i2 * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, ( data_size_ != npos ? data_size_ - offset : npos ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2 ) const
	{
		assert( ( I1_.initialized() ) && ( I2_.initialized() ) );
		return ( ( ( i2 * z1_ ) + i1 ) - shift_ );
	}

public: // Predicate

	// Dimensions Initialized?
	inline
	bool
	dimensions_initialized() const
	{
		return ( ( I1_.initialized() ) && ( I2_.initialized() ) );
	}

	// Contains Indexed Element?
	inline
	bool
	contains( int const i1, int const i2 ) const
	{
		return ( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) );
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

	// Size of Dimension 2
	inline
	size_type
	size2() const
	{
		return I2_.size();
	}

	// Size of Dimension 2
	inline
	int
	isize2() const
	{
		return I2_.isize();
	}

public: // Modifier

	// Clear
	inline
	FArray2P &
	clear()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		source_ = 0;
		return *this;
	}

	// Dimension by IndexRange Even if Const
	inline
	FArray2P const &
	dim( IR const & I1, IR const & I2 ) const
	{
		const_cast< FArray2P & >( *this ).dimension( I1, I2 );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	inline
	FArray2P const &
	dim( FArray2< U > const & a ) const
	{
		const_cast< FArray2P & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray2P &
	dimension( IR const & I1, IR const & I2 )
	{
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_proxy();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	inline
	FArray2P &
	dimension( FArray2< U > const & a )
	{
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		dimension_proxy();
		return *this;
	}

	// Attach to Proxy Array
	inline
	FArray2P &
	attach( FArray2P const & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Proxy Array
	inline
	FArray2P &
	attach( FArray2P & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Real Array
	inline
	FArray2P &
	attach( real_FArray const & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Real Array
	inline
	FArray2P &
	attach( real_FArray & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		I1_ = a.I1_;
		I2_ = a.I2_;
		if ( source_ ) source_->remove_observer( *this );
		source_ = &a;
		a.insert_observer( *this );
		return *this;
	}

	// Attach to Super Array
	inline
	FArray2P &
	attach( Super const & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		I1_ = a.I1();
		I2_ = a.I2();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Super Array
	inline
	FArray2P &
	attach( Super & a )
	{
		Base::attach( a );
		z1_ = a.z1_;
		I1_ = a.I1();
		I2_ = a.I2();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Base Array
	inline
	FArray2P &
	attach( Base const & a )
	{
		Base::attach( a, 2 );
		z1_ = 1;
		I1_ = 1;
		I2_ = a.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Non-Const Base Array
	inline
	FArray2P &
	attach( Base & a )
	{
		Base::attach( a, 2 );
		z1_ = 1;
		I1_ = 1;
		I2_ = a.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = dynamic_cast< SubjectMulti const * >( &a );
		if ( source_ ) source_->insert_observer( *this );
		return *this;
	}

	// Attach to Tail
	inline
	FArray2P &
	attach( Tail const & s )
	{
		Base::attach( s, 2 );
		z1_ = 1;
		I1_ = 1;
		I2_ = s.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Non-Const Tail
	inline
	FArray2P &
	attach( Tail & s )
	{
		Base::attach( s, 2 );
		z1_ = 1;
		I1_ = 1;
		I2_ = s.isize();
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Value
	inline
	FArray2P &
	attach( T const & t )
	{
		Base::attach( t, 2 );
		z1_ = 1;
		I1_ = 1;
		I2_ = star; // Unbounded
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Attach to Non-Const Value
	inline
	FArray2P &
	attach( T & t )
	{
		Base::attach( t, 2 );
		z1_ = 1;
		I1_ = 1;
		I2_ = star; // Unbounded
		if ( source_ ) source_->remove_observer( *this );
		source_ = 0;
		return *this;
	}

	// Detach from Source Array
	inline
	FArray2P &
	detach()
	{
		Base::detach();
		z1_ = 0;
		I1_.clear();
		I2_.clear();
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
			z1_ = 0;
			I1_.clear();
			I2_.clear();
			source_ = 0;
		}
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( SIR const & I1, SIR const & I2 )
	{
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_proxy();
	}

private: // Functions

	// Dimension by Current IndexRanges
	inline
	void
	dimension_proxy()
	{
		assert( I1_.not_unbounded() );
		z1_ = I1_.size();
		if ( dimensions_initialized() ) {
			if ( I2_.bounded() ) { // Bounded
				size_set( size_of( z1_, I2_.size() ) );
			} else if ( data_size_ != npos ) { // Unbounded with bounded data array
				if ( z1_ > 0u ) { // Infer upper index and size
					I2_.u_no_notify( I2_.lz() + static_cast< int >( data_size_ / z1_ ) - 1 );
					size_set( size_of( z1_, I2_.size() ) );
				} else {
					size_set( data_size_ );
				}
			} else { // Unbounded with unbounded data array
				size_set( npos );
			}
			shift_set( ( I2_.lz() * z1_ ) + I1_.lz() );
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
		if ( source_ ) source_->insert_observer( *this );
	}

	// Remove as Observer of the IndexRanges and Source Array
	inline
	void
	remove_as_observer()
	{
		I1_.remove_observer( *this );
		I2_.remove_observer( *this );
		if ( source_ ) source_->remove_observer( *this );
	}

private: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2

	SubjectMulti const * source_; // Pointer (non-owning) to source array (0 if unknown)

}; // FArray2P

} // ObjexxFCL

#endif // ObjexxFCL_FArray2P_hh_INCLUDED
