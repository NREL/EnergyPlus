#ifndef ObjexxFCL_FArray2D_hh_INCLUDED
#define ObjexxFCL_FArray2D_hh_INCLUDED

// FArray2D: Fortran-Compatible 2D Array
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
#include <ObjexxFCL/FArray2D.fwd.hh>
#include <ObjexxFCL/FArray2.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>
#include <ObjexxFCL/FArrayInitializer.hh>

namespace ObjexxFCL {

// FArray2D: Fortran-Compatible 2D Array
template< typename T >
class FArray2D : public FArray2< T >, public ObserverMulti
{

private: // Types

	typedef  FArray2< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  typename Super::arg_FArray  arg_FArray;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class FArray2D;
	friend class FArray2P< T >;
	friend class FArray2A< T >;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Tail  Tail;
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

	typedef  FArrayInitializer< T, ObjexxFCL::FArray2D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::conformable;
	using Super::isize1;
	using Super::isize2;
	using Super::l;
	using Super::operator ();
	using Super::reassign;
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size2;
	using Super::size_of;
	using Super::swap2DB;
	using Super::u;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;

public: // Creation

	// Default Constructor
	inline
	FArray2D()
	{
		insert_as_observer();
	}

	// Copy Constructor
	inline
	FArray2D( FArray2D const & a ) :
	 Super( a ),
	 ObserverMulti(),
	 I1_( a.I1_ ),
	 I2_( a.I2_ )
	{
		insert_as_observer();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray2D( FArray2D< U > const & a ) :
	 Super( a ),
	 I1_( a.I1_ ),
	 I2_( a.I2_ )
	{
		insert_as_observer();
	}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray2D( FArray2< U > const & a ) :
	 Super( a ),
	 I1_( a.I1() ),
	 I2_( a.I2() )
	{
		insert_as_observer();
	}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray2D( FArray2S< U > const & a ) :
	 Super( a ),
	 I1_( 1, a.u1() ),
	 I2_( 1, a.u2() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					reassign( l, a( i1, i2 ) );
				}
			}
		}
		insert_as_observer();
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray2D( MArray2< A, M > const & a ) :
	 Super( a ),
	 I1_( 1, a.u1() ),
	 I2_( 1, a.u2() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				size_type l( 0 );
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						reassign( l, a( i1, i2 ) );
					}
				}
			}
		}
		insert_as_observer();
	}

	// Sticky Initializer Value Constructor
	inline
	explicit
	FArray2D( Sticky< T > const & t ) :
	 initializer_( t )
	{
		insert_as_observer();
	}

	// IndexRange Constructor
	inline
	FArray2D( IR const & I1, IR const & I2 ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Initializer Value Constructor
	inline
	FArray2D( IR const & I1, IR const & I2, T const & t ) :
	 Super( size_of( I1, I2 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	FArray2D( IR const & I1, IR const & I2, Sticky< T > const & t ) :
	 Super( size_of( I1, I2 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	FArray2D( IR const & I1, IR const & I2, Sticky< T > const & t, T const & u ) :
	 Super( size_of( I1, I2 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
		insert_as_observer();
	}

	// IndexRange + Initializer Function Constructor
	inline
	FArray2D( IR const & I1, IR const & I2, InitializerFunction const & fxn ) :
	 Super( size_of( I1, I2 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( IR const & I1, IR const & I2, std::initializer_list< U > const l ) :
	 Super( l ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		assert( size_ == l.size() );
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( IR const & I1, IR const & I2, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( size_of( I1, I2 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 initializer_( t )
	{
		assert( size_ == l.size() );
		setup_real();
		initialize();
		std::copy( l.begin(), l.end(), data_ );
		insert_as_observer();
	}

	// IndexRange + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( IR const & I1, IR const & I2, FArray2< U > const & a ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( IR const & I1, IR const & I2, Sticky< T > const & t, FArray2< U > const & a ) :
	 Super( size_of( I1, I2 ), InitializerSentinel() ),
	 I1_( I1 ),
	 I2_( I2 ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					data_[ i ] = a[ i ];
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( IR const & I1, IR const & I2, FArray2S< U > const & a ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			assert( conformable( a ) );
			size_type l( 0 );
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
					reassign( l, a( i1, i2 ) );
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	FArray2D( IR const & I1, IR const & I2, MArray2< A, M > const & a ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				size_type l( 0 );
				for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
					for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1, ++l ) {
						reassign( l, a( i1, i2 ) );
					}
				}
			}
		}
		insert_as_observer();
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( FArray2< U > const & a, IR const & I1, IR const & I2 ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// IndexRange + Base Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( IR const & I1, IR const & I2, FArray< U > const & a ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( size_ == a.size() );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// Base + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray2D( FArray< U > const & a, IR const & I1, IR const & I2 ) :
	 Super( size_of( I1, I2 ) ),
	 I1_( I1 ),
	 I2_( I2 )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( size_ == a.size() );
				for ( size_type i = 0, e = size_; i < e; ++i ) {
					reassign( i, a[ i ] );
				}
			}
		}
		insert_as_observer();
	}

	// Range Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	range( FArray2< U > const & a )
	{
		return FArray2D( a.I1(), a.I2() );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	range( FArray2< U > const & a, T const & t )
	{
		return FArray2D( a.I1(), a.I2(), t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	shape( FArray2< U > const & a )
	{
		return FArray2D( a.isize1(), a.isize2() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	shape( FArray2< U > const & a, T const & t )
	{
		return FArray2D( a.isize1(), a.isize2(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	shape( FArray2S< U > const & a )
	{
		return FArray2D( a.isize1(), a.isize2() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	shape( FArray2S< U > const & a, T const & t )
	{
		return FArray2D( a.isize1(), a.isize2(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray2D
	shape( MArray2< A, M > const & a )
	{
		return FArray2D( a.isize1(), a.isize2() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray2D
	shape( MArray2< A, M > const & a, T const & t )
	{
		return FArray2D( a.isize1(), a.isize2(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	one_based( FArray2< U > const & a )
	{
		return FArray2D( a, a.isize1(), a.isize2() );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	FArray2D
	one_based( FArray2S< U > const & a )
	{
		return FArray2D( a.isize1(), a.isize2(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray2D
	one_based( MArray2< A, M > const & a )
	{
		return FArray2D( a.isize1(), a.isize2(), a );
	}

	// Diagonal Matrix Named Constructor
	inline
	static
	FArray2D
	diag( IR const & I, T const & d )
	{
		FArray2D D( I, I );
		D.to_diag( d );
		return D;
	}

	// Identity Matrix Named Constructor
	inline
	static
	FArray2D
	identity( IR const & I )
	{
		FArray2D D( I, I );
		D.to_diag( T( 1 ) );
		return D;
	}

	// Destructor
	inline
	virtual
	~FArray2D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray2D &
	operator =( FArray2D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray2D &
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
	FArray2D &
	operator =( FArray2< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator =( FArray2S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray2D &
	operator =( MArray2< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator +=( FArray2< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator -=( FArray2< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator *=( FArray2< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator /=( FArray2< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator +=( FArray2S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator -=( FArray2S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator *=( FArray2S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	operator /=( FArray2S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray2D &
	operator +=( MArray2< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray2D &
	operator -=( MArray2< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray2D &
	operator *=( MArray2< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray2D &
	operator /=( MArray2< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	and_equals( FArray2< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	or_equals( FArray2< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	and_equals( FArray2S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray2D &
	or_equals( FArray2S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray2D &
	and_equals( MArray2< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray2D &
	or_equals( MArray2< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray2D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray2D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray2D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray2D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray2D &
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
		return Tail( static_cast< T const * >( data_ + offset ), data_size_ - offset );
	}

	// Tail Starting at array( i1, i2 )
	inline
	Tail
	a( int const i1, int const i2 )
	{
		assert( ( I1_.contains( i1 ) ) && ( I2_.contains( i2 ) ) );
		size_type const offset( ( ( i2 * z1_ ) + i1 ) - shift_ );
		return Tail( data_ + offset, data_size_ - offset );
	}

	// Linear Index
	inline
	size_type
	index( int const i1, int const i2 ) const
	{
		assert( ( I1_.initialized() ) && ( I2_.initialized() ) );
		return ( ( ( i2 * z1_ ) + i1 ) - shift_ );
	}

	// array[ i ] const: Linear Subscript
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( i < size_ );
		return data_[ i ];
	}

	// array[ i ]: Linear Subscript
	inline
	T &
	operator []( size_type const i )
	{
		assert( i < size_ );
		return data_[ i ];
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
		return initializer_.is_active();
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
	FArray2D &
	clear()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		initializer_.clear();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray2D &
	allocate( IR const & I1, IR const & I2 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray2D &
	allocate( FArray2< U > const & a )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Deallocate
	inline
	FArray2D &
	deallocate()
	{
		Super::clear();
		I1_.clear_no_notify();
		I2_.clear_no_notify();
		initializer_.clear_nonsticky();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray2D &
	dimension( IR const & I1, IR const & I2 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	FArray2D &
	dimension( IR const & I1, IR const & I2, T const & t )
	{
		initializer_ = t;
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	FArray2D &
	dimension( IR const & I1, IR const & I2, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray2D &
	dimension( FArray2< U > const & a )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	FArray2D &
	dimension( FArray2< U > const & a, T const & t )
	{
		initializer_ = t;
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	FArray2D &
	dimension( FArray2< U > const & a, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I1_.assign_no_notify( a.I1() );
		I2_.assign_no_notify( a.I2() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	FArray2D &
	redimension( IR const & I1, IR const & I2 )
	{
		FArray2D o( I1, I2 );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
				int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
				for ( int i2 = b2; i2 <= e2; ++i2 ) {
					for ( int i1 = b1; i1 <= e1; ++i1 ) {
						o( i1, i2 ) = operator ()( i1, i2 );
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	FArray2D &
	redimension( IR const & I1, IR const & I2, T const & t )
	{
		FArray2D o( I1, I2, t );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
				int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
				for ( int i2 = b2; i2 <= e2; ++i2 ) {
					for ( int i1 = b1; i1 <= e1; ++i1 ) {
						o( i1, i2 ) = operator ()( i1, i2 );
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array Template
	template< typename U >
	inline
	FArray2D &
	redimension( FArray2< U > const & a )
	{
		FArray2D o( a.I1(), a.I2() );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
				int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
				for ( int i2 = b2; i2 <= e2; ++i2 ) {
					for ( int i1 = b1; i1 <= e1; ++i1 ) {
						o( i1, i2 ) = operator ()( i1, i2 );
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array + Fill Value Template
	template< typename U >
	inline
	FArray2D &
	redimension( FArray2< U > const & a, T const & t )
	{
		FArray2D o( a.I1(), a.I2(), t );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
				int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
				for ( int i2 = b2; i2 <= e2; ++i2 ) {
					for ( int i1 = b1; i1 <= e1; ++i1 ) {
						o( i1, i2 ) = operator ()( i1, i2 );
					}
				}
			}
		}
		return swap( o );
	}

	// Set Initializer Value
	inline
	FArray2D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	FArray2D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	FArray2D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	FArray2D &
	initialize()
	{
		if ( ( initializer_.is_active() ) && ( dimensions_initialized() ) ) {
			if ( initializer_.is_value() ) {
				reassign( initializer_.value() );
			} else if ( initializer_.is_function() ) {
				initializer_.function()( *this );
			}
		}
		return *this;
	}

	// Swap
	inline
	FArray2D &
	swap( FArray2D & v )
	{
		swap2DB( v );
		I1_.swap_no_notify( v.I1_ );
		I2_.swap_no_notify( v.I2_ );
		std::swap( initializer_, v.initializer_ );
		notify(); // So proxy FArrays can reattach
		v.notify(); // So proxy FArrays can reattach
		return *this;
	}

public: // Observer Modifier

	// Update
	inline
	void
	update()
	{
		dimension_real();
		initialize();
	}

	// Update for Destruction of a Subject
	inline
	void
	destructed( Subject const & )
	{}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( SIR const & I1, SIR const & I2 )
	{
		initializer_.clear_nonsticky();
		I1_.assign_no_notify( I1 );
		I2_.assign_no_notify( I2 );
		dimension_real();
		initialize();
		notify();
	}

private: // Functions

	// Setup for IndexRange Constructor
	inline
	void
	setup_real()
	{
		z1_ = I1_.size();
		if ( dimensions_initialized() ) {
			shift_set( ( I2_.lz() * z1_ ) + I1_.lz() );
		} else {
			shift_set( 0 );
		}
	}

	// Dimension by Current IndexRanges
	inline
	void
	dimension_real()
	{
		z1_ = I1_.size();
		if ( dimensions_initialized() ) {
			resize( size_of( z1_, I2_.size() ) );
			shift_set( ( I2_.lz() * z1_ ) + I1_.lz() );
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
			size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
		} else {
			Base::clear();
		}
	}

	// Insert as Observer of the IndexRanges
	inline
	void
	insert_as_observer()
	{
		I1_.insert_observer( *this );
		I2_.insert_observer( *this );
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Remove as Observer of the IndexRanges
	inline
	void
	remove_as_observer()
	{
		I1_.remove_observer( *this );
		I2_.remove_observer( *this );
	}

#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
	// Report size if at least value defined for OBJEXXFCL_FARRAY_SIZE_REPORT
	//  Size is based on sizeof( T ) so T-controlled heap memory is not counted
	inline
	void
	size_report() const
	{
		if ( size_ * sizeof( T ) >= OBJEXXFCL_FARRAY_SIZE_REPORT ) {
			std::cout << "  Index ranges: " << I1_ << ' ' << I2_ << std::endl;
		}
	}
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT

private: // Data

	IR I1_; // Index range of dim 1
	IR I2_; // Index range of dim 2

	Initializer initializer_; // Array initializer

}; // FArray2D

// Swap
template< typename T >
inline
void
swap( FArray2D< T > & a, FArray2D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
FArray2D< bool >
operator ==( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
FArray2D< bool >
operator !=( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
FArray2D< bool >
operator <( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
FArray2D< bool >
operator <=( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
FArray2D< bool >
operator >( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
FArray2D< bool >
operator >=( FArray2< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
FArray2D< bool >
operator ==( FArray2< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
FArray2D< bool >
operator !=( FArray2< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
FArray2D< bool >
operator <( FArray2< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
FArray2D< bool >
operator <=( FArray2< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
FArray2D< bool >
operator >( FArray2< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
FArray2D< bool >
operator >=( FArray2< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
FArray2D< bool >
operator ==( T const & t, FArray2< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
FArray2D< bool >
operator !=( T const & t, FArray2< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
FArray2D< bool >
operator <( T const & t, FArray2< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
FArray2D< bool >
operator <=( T const & t, FArray2< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
FArray2D< bool >
operator >( T const & t, FArray2< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
FArray2D< bool >
operator >=( T const & t, FArray2< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
FArray2D< bool >
operator ==( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) == b( i1, i2 ) );
		}
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
FArray2D< bool >
operator !=( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) != b( i1, i2 ) );
		}
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
FArray2D< bool >
operator <( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) < b( i1, i2 ) );
		}
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
FArray2D< bool >
operator <=( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= b( i1, i2 ) );
		}
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
FArray2D< bool >
operator >( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) > b( i1, i2 ) );
		}
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
FArray2D< bool >
operator >=( FArray2S< T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= b( i1, i2 ) );
		}
	}
	return r;
}

// Slice == Array
template< typename T >
inline
FArray2D< bool >
operator ==( FArray2S< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) == b[ l ] );
		}
	}
	return r;
}

// Slice != Array
template< typename T >
inline
FArray2D< bool >
operator !=( FArray2S< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) != b[ l ] );
		}
	}
	return r;
}

// Slice < Array
template< typename T >
inline
FArray2D< bool >
operator <( FArray2S< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) < b[ l ] );
		}
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
FArray2D< bool >
operator <=( FArray2S< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= b[ l ] );
		}
	}
	return r;
}

// Slice > Array
template< typename T >
inline
FArray2D< bool >
operator >( FArray2S< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) > b[ l ] );
		}
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
FArray2D< bool >
operator >=( FArray2S< T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= b[ l ] );
		}
	}
	return r;
}

// Array == Slice
template< typename T >
inline
FArray2D< bool >
operator ==( FArray2< T > const & a, FArray2S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
FArray2D< bool >
operator !=( FArray2< T > const & a, FArray2S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
FArray2D< bool >
operator <( FArray2< T > const & a, FArray2S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
FArray2D< bool >
operator <=( FArray2< T > const & a, FArray2S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
FArray2D< bool >
operator >( FArray2< T > const & a, FArray2S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
FArray2D< bool >
operator >=( FArray2< T > const & a, FArray2S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
FArray2D< bool >
operator ==( FArray2S< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) == t );
		}
	}
	return r;
}

// Slice != Value
template< typename T >
inline
FArray2D< bool >
operator !=( FArray2S< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) != t );
		}
	}
	return r;
}

// Slice < Value
template< typename T >
inline
FArray2D< bool >
operator <( FArray2S< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) < t );
		}
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
FArray2D< bool >
operator <=( FArray2S< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= t );
		}
	}
	return r;
}

// Slice > Value
template< typename T >
inline
FArray2D< bool >
operator >( FArray2S< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) > t );
		}
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
FArray2D< bool >
operator >=( FArray2S< T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= t );
		}
	}
	return r;
}

// Value == Slice
template< typename T >
inline
FArray2D< bool >
operator ==( T const & t, FArray2S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
FArray2D< bool >
operator !=( T const & t, FArray2S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
FArray2D< bool >
operator <( T const & t, FArray2S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
FArray2D< bool >
operator <=( T const & t, FArray2S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
FArray2D< bool >
operator >( T const & t, FArray2S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
FArray2D< bool >
operator >=( T const & t, FArray2S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
FArray2D< bool >
operator ==( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) == b( i1, i2 ) );
		}
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
FArray2D< bool >
operator !=( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) != b( i1, i2 ) );
		}
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) < b( i1, i2 ) );
		}
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <=( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= b( i1, i2 ) );
		}
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) > b( i1, i2 ) );
		}
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >=( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= b( i1, i2 ) );
		}
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
FArray2D< bool >
operator ==( MArray2< A, T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) == b[ l ] );
		}
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
FArray2D< bool >
operator !=( MArray2< A, T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) != b[ l ] );
		}
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
FArray2D< bool >
operator <( MArray2< A, T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) < b[ l ] );
		}
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
FArray2D< bool >
operator <=( MArray2< A, T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= b[ l ] );
		}
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
FArray2D< bool >
operator >( MArray2< A, T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) > b[ l ] );
		}
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
FArray2D< bool >
operator >=( MArray2< A, T > const & a, FArray2< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	typename FArray2< T >::size_type l( 0 );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1, ++l ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= b[ l ] );
		}
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
FArray2D< bool >
operator ==( FArray2< T > const & a, MArray2< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
FArray2D< bool >
operator !=( FArray2< T > const & a, MArray2< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <( FArray2< T > const & a, MArray2< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <=( FArray2< T > const & a, MArray2< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >( FArray2< T > const & a, MArray2< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >=( FArray2< T > const & a, MArray2< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
FArray2D< bool >
operator ==( MArray2< A, T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) == b( i1, i2 ) );
		}
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
FArray2D< bool >
operator !=( MArray2< A, T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) != b( i1, i2 ) );
		}
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
FArray2D< bool >
operator <( MArray2< A, T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) < b( i1, i2 ) );
		}
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
FArray2D< bool >
operator <=( MArray2< A, T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= b( i1, i2 ) );
		}
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
FArray2D< bool >
operator >( MArray2< A, T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) > b( i1, i2 ) );
		}
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
FArray2D< bool >
operator >=( MArray2< A, T > const & a, FArray2S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= b( i1, i2 ) );
		}
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
FArray2D< bool >
operator ==( FArray2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
FArray2D< bool >
operator !=( FArray2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <( FArray2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <=( FArray2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >( FArray2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >=( FArray2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
FArray2D< bool >
operator ==( MArray2< A, T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) == t );
		}
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
FArray2D< bool >
operator !=( MArray2< A, T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) != t );
		}
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
FArray2D< bool >
operator <( MArray2< A, T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) < t );
		}
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
FArray2D< bool >
operator <=( MArray2< A, T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) <= t );
		}
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
FArray2D< bool >
operator >( MArray2< A, T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) > t );
		}
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
FArray2D< bool >
operator >=( MArray2< A, T > const & a, T const & t )
{
	FArray2D< bool > r( FArray2D< bool >::shape( a ) );
	for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
			r( i1, i2 ) = ( a( i1, i2 ) >= t );
		}
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
FArray2D< bool >
operator ==( T const & t, MArray2< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
FArray2D< bool >
operator !=( T const & t, MArray2< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <( T const & t, MArray2< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator <=( T const & t, MArray2< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >( T const & t, MArray2< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
FArray2D< bool >
operator >=( T const & t, MArray2< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
FArray2D< T >
operator -( FArray2< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
FArray2D< T >
operator +( FArray2< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
FArray2D< T >
operator -( FArray2< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
FArray2D< T >
operator *( FArray2< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
FArray2D< T >
operator /( FArray2< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
FArray2D< T >
operator +( FArray2< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
FArray2D< T >
operator +( T const & t, FArray2< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
FArray2D< T >
operator -( FArray2< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
FArray2D< T >
operator -( T const & t, FArray2< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
FArray2D< T >
operator *( FArray2< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
FArray2D< T >
operator *( T const & t, FArray2< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
FArray2D< T >
operator /( FArray2< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
FArray2D< T >
operator /( T const & t, FArray2< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
FArray2D< T >
operator &&( FArray2< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
FArray2D< T >
operator ||( FArray2< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Transpose: Fortran Compatible 1-Based Indexing
template< typename T >
inline
FArray2D< T >
transpose( FArray2< T > const & a )
{
	typedef  typename FArray2D< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	FArray2D< T > aT( a.I2(), a.I1() );
	for ( size_type i2 = 0, l = 0; i2 < as2; ++i2 ) {
		for ( size_type i1 = 0, lT = i2; i1 < as1; ++i1, ++l, lT += as2 ) {
			aT[ lT ] = a[ l ];
		}
	}
	return aT;
}

// Transposed: Preserved Indexing
template< typename T >
inline
FArray2D< T >
transposed( FArray2< T > const & a )
{
	typedef  typename FArray2D< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	FArray2D< T > aT( a.I2(), a.I1() );
	for ( size_type i2 = 0, l = 0; i2 < as2; ++i2 ) {
		for ( size_type i1 = 0, lT = i2; i1 < as1; ++i1, ++l, lT += as2 ) {
			aT[ lT ] = a[ l ];
		}
	}
	return aT;
}

// Generator: Slice

// -Slice
template< typename T >
inline
FArray2D< T >
operator -( FArray2S< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
FArray2D< T >
operator +( FArray2S< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
FArray2D< T >
operator -( FArray2S< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
FArray2D< T >
operator *( FArray2S< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
FArray2D< T >
operator /( FArray2S< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
FArray2D< T >
operator +( FArray2S< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
FArray2D< T >
operator -( FArray2S< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
FArray2D< T >
operator *( FArray2S< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
FArray2D< T >
operator /( FArray2S< T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
FArray2D< T >
operator +( FArray2< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
FArray2D< T >
operator -( FArray2< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
FArray2D< T >
operator *( FArray2< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
FArray2D< T >
operator /( FArray2< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
FArray2D< T >
operator +( FArray2S< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
FArray2D< T >
operator +( T const & t, FArray2S< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
FArray2D< T >
operator -( FArray2S< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
FArray2D< T >
operator -( T const & t, FArray2S< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
FArray2D< T >
operator *( FArray2S< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
FArray2D< T >
operator *( T const & t, FArray2S< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
FArray2D< T >
operator /( FArray2S< T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
FArray2D< T >
operator /( T const & t, FArray2S< T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
FArray2D< T >
operator &&( FArray2S< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
FArray2D< T >
operator ||( FArray2S< T > const & a, FArray2S< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Transpose: Fortran Compatible 1-Based Indexing
template< typename T >
inline
FArray2D< T >
transpose( FArray2S< T > const & a )
{
	typedef  typename FArray2S< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	FArray2D< T > aT( as2, as1 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			aT( i2, i1 ) = a( i1, i2 );
		}
	}
	return aT;
}

// Transposed: Preserved Indexing
template< typename T >
inline
FArray2D< T >
transposed( FArray2S< T > const & a )
{
	return transpose( a ); // Slice indexing is 1-based
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
FArray2D< T >
operator -( MArray2< A, T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
FArray2D< T >
operator +( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
FArray2D< T >
operator -( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
FArray2D< T >
operator *( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
FArray2D< T >
operator /( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
FArray2D< T >
operator +( MArray2< A, T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
FArray2D< T >
operator -( MArray2< A, T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
FArray2D< T >
operator *( MArray2< A, T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
FArray2D< T >
operator /( MArray2< A, T > const & a, FArray2< T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
FArray2D< T >
operator +( FArray2< T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
FArray2D< T >
operator -( FArray2< T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
FArray2D< T >
operator *( FArray2< T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
FArray2D< T >
operator /( FArray2< T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
FArray2D< T >
operator +( MArray2< A, T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
FArray2D< T >
operator +( T const & t, MArray2< A, T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
FArray2D< T >
operator -( MArray2< A, T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
FArray2D< T >
operator -( T const & t, MArray2< A, T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
FArray2D< T >
operator *( MArray2< A, T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
FArray2D< T >
operator *( T const & t, MArray2< A, T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
FArray2D< T >
operator /( MArray2< A, T > const & a, T const & t )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
FArray2D< T >
operator /( T const & t, MArray2< A, T > const & a )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
FArray2D< T >
operator &&( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
FArray2D< T >
operator ||( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	FArray2D< T > r( FArray2D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Transpose: Fortran Compatible 1-Based Indexing
template< class A, typename T >
inline
FArray2D< T >
transpose( MArray2< A, T > const & a )
{
	typedef  typename MArray2< A, T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	FArray2D< T > aT( as2, as1 );
	for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			aT( i2, i1 ) = a( i1, i2 );
		}
	}
	return aT;
}

// Transposed: Preserved Indexing
template< class A, typename T >
inline
FArray2D< T >
transposed( MArray2< A, T > const & a )
{
	return transpose( a ); // MArray indexing is 1-based
}

} // ObjexxFCL

#ifndef NO_STD_SWAP_OVERLOADS

// std::swap Overloads for Efficiency
//
// Technically you cannot add template functions overloads to namespace std
// but this works with most compilers and makes it much faster if someone uses
// std::swap instead of swap or ObjexxFCL::swap.  The legal alternative would be
// to add specializations of swap for each anticipated instantiation.

namespace std {

// std::swap( FArray2D, FArray2D )
template< typename T >
inline
void
swap( ObjexxFCL::FArray2D< T > & a, ObjexxFCL::FArray2D< T > & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_FArray2D_hh_INCLUDED
