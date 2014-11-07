#ifndef ObjexxFCL_FArray1D_hh_INCLUDED
#define ObjexxFCL_FArray1D_hh_INCLUDED

// FArray1D: Fortran-Compatible 1D Array
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
#include <ObjexxFCL/FArray1D.fwd.hh>
#include <ObjexxFCL/FArray1.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/DynamicIndexRange.hh>
#include <ObjexxFCL/FArrayInitializer.hh>

namespace ObjexxFCL {

// FArray1D: Fortran-Compatible 1D Array
template< typename T >
class FArray1D : public FArray1< T >, public ObserverMulti
{

private: // Types

	typedef  FArray1< T >  Super;
	typedef  typename Super::real_FArray  real_FArray;
	typedef  typename Super::proxy_FArray  proxy_FArray;
	typedef  typename Super::arg_FArray  arg_FArray;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class FArray1D;
	friend class FArray1P< T >;
	friend class FArray1A< T >;

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

	typedef  FArrayInitializer< T, ObjexxFCL::FArray1D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::conformable;
	using Super::isize1;
	using Super::l;
	using Super::operator ();
	using Super::reassign;
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size_of;
	using Super::swap1DB;
	using Super::u;
	using Super::data_;
	using Super::data_size_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;

public: // Creation

	// Default Constructor
	inline
	FArray1D()
	{
		insert_as_observer();
	}

	// Copy Constructor
	inline
	FArray1D( FArray1D const & a ) :
	 Super( a ),
	 ObserverMulti(),
	 I_( a.I_ )
	{
		insert_as_observer();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray1D( FArray1D< U > const & a ) :
	 Super( a ),
	 I_( a.I_ )
	{
		insert_as_observer();
	}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray1D( FArray1< U > const & a ) :
	 Super( a ),
	 I_( a.I() )
	{
		insert_as_observer();
	}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray1D( FArray1S< U > const & a ) :
	 Super( a ),
	 I_( 1, a.u() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				reassign( l, a( i ) );
			}
		}
		insert_as_observer();
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	FArray1D( MArray1< A, M > const & a ) :
	 Super( a ),
	 I_( 1, a.u() )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				size_type l( 0 );
				for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
					reassign( l, a( i ) );
				}
			}
		}
		insert_as_observer();
	}

	// Sticky Initializer Value Constructor
	inline
	explicit
	FArray1D( Sticky< T > const & t ) :
	 initializer_( t )
	{
		insert_as_observer();
	}

	// Initializer List Index Range Constructor Template
	template< typename U, class = typename std::enable_if< ! std::is_constructible< T, U >::value >::type >
	inline
	explicit
	FArray1D( std::initializer_list< U > const l ) :
	 Super( IR( l ).size() ),
	 I_( l )
	{
		assert( l.size() == 2 );
		setup_real();
		insert_as_observer();
	}

	// Initializer List of Values Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value && ! ( std::is_same< U, int >::value || std::is_same< U, Index >::value ) >::type, typename = void >
	inline
	FArray1D( std::initializer_list< U > const l ) :
	 Super( l ),
	 I_( static_cast< int >( l.size() ) )
	{
		setup_real();
		insert_as_observer();
	}

	// Initializer List of Values or Index Range Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value && ( std::is_same< U, int >::value || std::is_same< U, Index >::value ) >::type, typename = void, typename = void >
	inline
	FArray1D( std::initializer_list< U > const l ) :
	 Super( l ),
	 I_( static_cast< int >( l.size() ) )
	{ // Note: 2 item lists treated as index range: Others treated as values: See ObjexxFCL.Users.FArray.html
#ifdef OBJEXXFCL_DISALLOW_AMBIGUOUS_INITIALIZER_LIST_CONSTRUCTORS
		assert( l.size() != 2 ); // Avoid ambiguity with IndexRange {l,u} usage
#endif
		if ( l.size() == 2 ) { // Treat as an IndexRange
			I_ = l;
			Base::reconstruct_by_size( size_of( I_ ) );
		}
		setup_real();
		insert_as_observer();
	}

	// Initializer List Index Range + Initializer Value Constructor Template
	template< typename U >
	inline
	FArray1D( std::initializer_list< U > const l, T const & t ) :
	 Super( IR( l ).size(), InitializerSentinel() ),
	 I_( l ),
	 initializer_( t )
	{
		assert( l.size() == 2 );
		setup_real();
		initialize();
		insert_as_observer();
	}

	// Initializer List Index Range + Sticky Initializer Value Constructor Template
	template< typename U >
	inline
	FArray1D( std::initializer_list< U > const l, Sticky< T > const & t ) :
	 Super( IR( l ).size(), InitializerSentinel() ),
	 I_( l ),
	 initializer_( t )
	{
		assert( l.size() == 2 );
		setup_real();
		initialize();
		insert_as_observer();
	}

	// Initializer List Index Range + Initializer Function Constructor Template
	template< typename U >
	inline
	FArray1D( std::initializer_list< U > const l, InitializerFunction const & fxn ) :
	 Super( IR( l ).size(), InitializerSentinel() ),
	 I_( l ),
	 initializer_( fxn )
	{
		assert( l.size() == 2 );
		setup_real();
		initialize();
		insert_as_observer();
	}

	// Initializer List Index Range + Super Constructor Template
	template< typename U, typename V, class = typename std::enable_if< std::is_constructible< T, V >::value >::type >
	inline
	FArray1D( std::initializer_list< U > const l, FArray1< V > const & a ) :
	 Super( IR( l ).size() ),
	 I_( l )
	{
		assert( l.size() == 2 );
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

	// Initializer List Index Range + Base Constructor Template
	template< typename U, typename V >
	inline
	FArray1D( std::initializer_list< U > const l, FArray< V > const & a ) :
	 Super( IR( l ).size() ),
	 I_( l )
	{
		assert( l.size() == 2 );
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

	// std::array Constructor Template
	template< typename U, Size s >
	inline
	FArray1D( std::array< U, s > const & a ) :
	 Super( a ),
	 I_( static_cast< int >( s ) )
	{
		setup_real();
		insert_as_observer();
	}

	// std::vector Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1D( std::vector< U > const & v ) :
	 Super( v ),
	 I_( static_cast< int >( v.size() ) )
	{
		setup_real();
		insert_as_observer();
	}

	// Vector2 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1D( Vector2< U > const & v ) :
	 Super( v ),
	 I_( 2 )
	{
		setup_real();
		insert_as_observer();
	}

	// Vector3 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1D( Vector3< U > const & v ) :
	 Super( v ),
	 I_( 3 )
	{
		setup_real();
		insert_as_observer();
	}

	// IndexRange Constructor
	inline
	explicit
	FArray1D( IR const & I ) :
	 Super( size_of( I ) ),
	 I_( I )
	{
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Initializer Value Constructor
	inline
	FArray1D( IR const & I, T const & t ) :
	 Super( size_of( I ), InitializerSentinel() ),
	 I_( I ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	FArray1D( IR const & I, Sticky< T > const & t ) :
	 Super( size_of( I ), InitializerSentinel() ),
	 I_( I ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	FArray1D( IR const & I, Sticky< T > const & t, T const & u ) :
	 Super( size_of( I ), InitializerSentinel() ),
	 I_( I ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
		insert_as_observer();
	}

	// IndexRange + Initializer Function Constructor
	inline
	FArray1D( IR const & I, InitializerFunction const & fxn ) :
	 Super( size_of( I ), InitializerSentinel() ),
	 I_( I ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
		insert_as_observer();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1D( IR const & I, std::initializer_list< U > const l ) :
	 Super( l ),
	 I_( I )
	{
		assert( size_ == l.size() );
		setup_real();
		insert_as_observer();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1D( IR const & I, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( size_of( I ), InitializerSentinel() ),
	 I_( I ),
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
	FArray1D( IR const & I, FArray1< U > const & a ) :
	 Super( size_of( I ) ),
	 I_( I )
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
	FArray1D( IR const & I, Sticky< T > const & t, FArray1< U > const & a ) :
	 Super( size_of( I ), InitializerSentinel() ),
	 I_( I ),
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
	FArray1D( IR const & I, FArray1S< U > const & a ) :
	 Super( size_of( I ) ),
	 I_( I )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			assert( conformable( a ) );
			size_type l( 0 );
			for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
				reassign( l, a( i ) );
			}
		}
		insert_as_observer();
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	FArray1D( IR const & I, MArray1< A, M > const & a ) :
	 Super( size_of( I ) ),
	 I_( I )
	{
		setup_real();
		if ( dimensions_initialized() ) {
			if ( a.dimensions_initialized() ) {
				assert( conformable( a ) );
				size_type l( 0 );
				for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
					reassign( l, a( i ) );
				}
			}
		}
		insert_as_observer();
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	FArray1D( FArray1< U > const & a, IR const & I ) :
	 Super( size_of( I ) ),
	 I_( I )
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
	FArray1D( IR const & I, FArray< U > const & a ) :
	 Super( size_of( I ) ),
	 I_( I )
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
	FArray1D( FArray< U > const & a, IR const & I ) :
	 Super( size_of( I ) ),
	 I_( I )
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
	FArray1D
	range( FArray1< U > const & a )
	{
		return FArray1D( a.I() );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	range( FArray1< U > const & a, T const & t )
	{
		return FArray1D( a.I(), t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	shape( FArray1< U > const & a )
	{
		return FArray1D( a.isize() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	shape( FArray1< U > const & a, T const & t )
	{
		return FArray1D( a.isize(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	shape( FArray1S< U > const & a )
	{
		return FArray1D( a.isize() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	shape( FArray1S< U > const & a, T const & t )
	{
		return FArray1D( a.isize(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray1D
	shape( MArray1< A, M > const & a )
	{
		return FArray1D( a.isize() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray1D
	shape( MArray1< A, M > const & a, T const & t )
	{
		return FArray1D( a.isize(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	one_based( FArray1< U > const & a )
	{
		return FArray1D( a.isize(), a );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	one_based( FArray1S< U > const & a )
	{
		return FArray1D( a.isize(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	FArray1D
	one_based( MArray1< A, M > const & a )
	{
		return FArray1D( a.isize(), a );
	}

	// Initializer List One-Based Named Constructor Template
	template< typename U >
	inline
	static
	FArray1D
	one_based( std::initializer_list< U > const l )
	{
		return FArray1D( static_cast< int >( l.size() ), l );
	}

	// Destructor
	inline
	virtual
	~FArray1D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray1D &
	operator =( FArray1D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	inline
	FArray1D &
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
	FArray1D &
	operator =( FArray1< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator =( FArray1S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray1D &
	operator =( MArray1< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator =( std::array< U, s > const & a )
	{
		Base::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator =( std::vector< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator =( Vector2< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator =( Vector3< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( FArray1< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( FArray1< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( FArray1< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( FArray1< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( FArray1S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( FArray1S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( FArray1S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( FArray1S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray1D &
	operator +=( MArray1< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray1D &
	operator -=( MArray1< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray1D &
	operator *=( MArray1< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray1D &
	operator /=( MArray1< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( std::initializer_list< U > const l )
	{
		Base::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( std::initializer_list< U > const l )
	{
		Base::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( std::initializer_list< U > const l )
	{
		Base::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( std::initializer_list< U > const l )
	{
		Base::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( std::array< U, s > const & a )
	{
		Base::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( std::array< U, s > const & a )
	{
		Base::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( std::array< U, s > const & a )
	{
		Base::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( std::array< U, s > const & a )
	{
		Base::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( std::vector< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( std::vector< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( std::vector< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( std::vector< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( Vector2< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( Vector2< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( Vector2< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( Vector2< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator +=( Vector3< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator -=( Vector3< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator *=( Vector3< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	operator /=( Vector3< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( FArray1< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( FArray1< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( FArray1S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( FArray1S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	FArray1D &
	and_equals( MArray1< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	FArray1D &
	or_equals( MArray1< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1D &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray1D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	FArray1D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	FArray1D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	FArray1D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	FArray1D &
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
		return Tail( static_cast< T const * >( sdata_ + i ), data_size_ - ( i - shift_ ) );
	}

	// Tail Starting at array( i )
	inline
	Tail
	a( int const i )
	{
		assert( I_.contains( i ) );
		return Tail( sdata_ + i, data_size_ - ( i - shift_ ) );
	}

	// Linear Index
	inline
	size_type
	index( int const i ) const
	{
		assert( I_.initialized() );
		return ( i - shift_ );
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
		return initializer_.is_active();
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
	FArray1D &
	clear()
	{
		Super::clear();
		I_.clear_no_notify();
		initializer_.clear();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray1D &
	allocate( IR const & I )
	{
		initializer_.clear_nonsticky();
		I_.assign_no_notify( I );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray1D &
	allocate( FArray1< U > const & a )
	{
		initializer_.clear_nonsticky();
		I_.assign_no_notify( a.I() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Deallocate
	inline
	FArray1D &
	deallocate()
	{
		Super::clear();
		I_.clear_no_notify();
		initializer_.clear_nonsticky();
		notify();
		return *this;
	}

	// Dimension by IndexRange
	inline
	FArray1D &
	dimension( IR const & I )
	{
		initializer_.clear_nonsticky();
		I_.assign_no_notify( I );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	FArray1D &
	dimension( IR const & I, T const & t )
	{
		initializer_ = t;
		I_.assign_no_notify( I );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	FArray1D &
	dimension( IR const & I, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I_.assign_no_notify( I );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	FArray1D &
	dimension( FArray1< U > const & a )
	{
		initializer_.clear_nonsticky();
		I_.assign_no_notify( a.I() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	FArray1D &
	dimension( FArray1< U > const & a, T const & t )
	{
		initializer_ = t;
		I_.assign_no_notify( a.I() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	FArray1D &
	dimension( FArray1< U > const & a, InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		I_.assign_no_notify( a.I() );
		dimension_real();
		initialize();
		notify();
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	FArray1D &
	redimension( IR const & I )
	{
		FArray1D o( I );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b( std::max( I.l(), l() ) ), e( std::min( I.u(), u() ) );
				for ( int i = b; i <= e; ++i ) {
					o( i ) = operator ()( i );
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	FArray1D &
	redimension( IR const & I, T const & t )
	{
		if ( dimensions_initialized() ) {
			if ( I.initialized() ) {
				FArray1D o( I );
				auto const l_( l() );
				auto const I_l_( I.l() );
				auto const l_max_( std::max( l_, I_l_ ) );
				auto const u_( u() );
				auto const I_u_( I.u() );
				auto const u_min_( std::min( u_, I_u_ ) );
				if ( I_l_ < l_ ) {
					for ( int i = I_l_, e = std::min( l_ - 1, I_u_ ); i <= e; ++i ) { // Fill new lower elements
						o( i ) = t;
					}
				}
				if ( l_max_ <= u_min_ ) { // Ranges overlap
					for ( int i = l_max_; i <= u_min_; ++i ) { // Copy array data in overlap
						o( i ) = operator ()( i );
					}
				}
				if ( u_ < I_u_ ) {
					for ( int i = std::max( u_ + 1, I_l_ ); i <= I_u_; ++i ) { // Fill new upper elements
						o( i ) = t;
					}
				}
				return swap( o );
			}
		}
		FArray1D o( I, t );
		return swap( o );
	}

	// Data-Preserving Redimension by Array Template
	template< typename U >
	inline
	FArray1D &
	redimension( FArray1< U > const & a )
	{
		FArray1D o( a.I() );
		if ( dimensions_initialized() ) {
			if ( o.dimensions_initialized() ) { // Copy array data where overlap
				int const b( std::max( a.l(), l() ) ), e( std::min( a.u(), u() ) );
				for ( int i = b; i <= e; ++i ) {
					o( i ) = operator ()( i );
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array + Fill Value Template
	template< typename U >
	inline
	FArray1D &
	redimension( FArray1< U > const & a, T const & t )
	{
		auto const & I( a.I() );
		if ( dimensions_initialized() ) {
			if ( I.initialized() ) {
				FArray1D o( I );
				auto const l_( l() );
				auto const I_l_( I.l() );
				auto const l_max_( std::max( l_, I_l_ ) );
				auto const u_( u() );
				auto const I_u_( I.u() );
				auto const u_min_( std::min( u_, I_u_ ) );
				if ( I_l_ < l_ ) {
					for ( int i = I_l_, e = std::min( l_ - 1, I_u_ ); i <= e; ++i ) { // Fill new lower elements
						o( i ) = t;
					}
				}
				if ( l_max_ <= u_min_ ) { // Ranges overlap
					for ( int i = l_max_; i <= u_min_; ++i ) { // Copy array data in overlap
						o( i ) = operator ()( i );
					}
				}
				if ( u_ < I_u_ ) {
					for ( int i = std::max( u_ + 1, I_l_ ); i <= I_u_; ++i ) { // Fill new upper elements
						o( i ) = t;
					}
				}
				return swap( o );
			}
		}
		FArray1D o( I, t );
		return swap( o );
	}

	// Set Initializer Value
	inline
	FArray1D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	FArray1D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	FArray1D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	FArray1D &
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
	FArray1D &
	swap( FArray1D & v )
	{
		swap1DB( v );
		I_.swap_no_notify( v.I_ );
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
	dimension_assign( SIR const & I )
	{
		initializer_.clear_nonsticky();
		I_.assign_no_notify( I );
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
		if ( dimensions_initialized() ) {
			shift_set( I_.lz() );
		} else {
			shift_set( 0 );
		}
	}

	// Dimension by Current IndexRange
	inline
	void
	dimension_real()
	{
		if ( dimensions_initialized() ) {
			resize( size_of( I_ ) );
			shift_set( I_.lz() );
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
			size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
		} else {
			Base::clear();
		}
	}

	// Insert as Observer of the IndexRange
	inline
	void
	insert_as_observer()
	{
		I_.insert_observer( *this );
#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
		size_report();
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT
	}

	// Remove as Observer of the IndexRange
	inline
	void
	remove_as_observer()
	{
		I_.remove_observer( *this );
	}

#ifdef OBJEXXFCL_FARRAY_SIZE_REPORT
	// Report size if at least value defined for OBJEXXFCL_FARRAY_SIZE_REPORT
	//  Size is based on sizeof( T ) so T-controlled heap memory is not counted
	inline
	void
	size_report() const
	{
		if ( size_ * sizeof( T ) >= OBJEXXFCL_FARRAY_SIZE_REPORT ) {
			std::cout << "  Index ranges: " << I_ << std::endl;
		}
	}
#endif // OBJEXXFCL_FARRAY_SIZE_REPORT

private: // Data

	IR I_; // Index range

	Initializer initializer_; // Array initializer

}; // FArray1D

// Swap
template< typename T >
inline
void
swap( FArray1D< T > & a, FArray1D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
FArray1D< bool >
operator ==( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
FArray1D< bool >
operator !=( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
FArray1D< bool >
operator <( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
FArray1D< bool >
operator <=( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
FArray1D< bool >
operator >( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
FArray1D< bool >
operator >=( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
FArray1D< bool >
operator ==( FArray1< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
FArray1D< bool >
operator !=( FArray1< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
FArray1D< bool >
operator <( FArray1< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
FArray1D< bool >
operator <=( FArray1< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
FArray1D< bool >
operator >( FArray1< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
FArray1D< bool >
operator >=( FArray1< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
FArray1D< bool >
operator ==( T const & t, FArray1< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
FArray1D< bool >
operator !=( T const & t, FArray1< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
FArray1D< bool >
operator <( T const & t, FArray1< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
FArray1D< bool >
operator <=( T const & t, FArray1< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
FArray1D< bool >
operator >( T const & t, FArray1< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
FArray1D< bool >
operator >=( T const & t, FArray1< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
FArray1D< bool >
operator ==( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == b( i ) );
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
FArray1D< bool >
operator !=( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != b( i ) );
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
FArray1D< bool >
operator <( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < b( i ) );
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
FArray1D< bool >
operator <=( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= b( i ) );
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
FArray1D< bool >
operator >( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > b( i ) );
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
FArray1D< bool >
operator >=( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= b( i ) );
	}
	return r;
}

// Slice == Array
template< typename T >
inline
FArray1D< bool >
operator ==( FArray1S< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) == b[ l ] );
	}
	return r;
}

// Slice != Array
template< typename T >
inline
FArray1D< bool >
operator !=( FArray1S< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) != b[ l ] );
	}
	return r;
}

// Slice < Array
template< typename T >
inline
FArray1D< bool >
operator <( FArray1S< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) < b[ l ] );
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
FArray1D< bool >
operator <=( FArray1S< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) <= b[ l ] );
	}
	return r;
}

// Slice > Array
template< typename T >
inline
FArray1D< bool >
operator >( FArray1S< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) > b[ l ] );
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
FArray1D< bool >
operator >=( FArray1S< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) >= b[ l ] );
	}
	return r;
}

// Array == Slice
template< typename T >
inline
FArray1D< bool >
operator ==( FArray1< T > const & a, FArray1S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
FArray1D< bool >
operator !=( FArray1< T > const & a, FArray1S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
FArray1D< bool >
operator <( FArray1< T > const & a, FArray1S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
FArray1D< bool >
operator <=( FArray1< T > const & a, FArray1S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
FArray1D< bool >
operator >( FArray1< T > const & a, FArray1S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
FArray1D< bool >
operator >=( FArray1< T > const & a, FArray1S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
FArray1D< bool >
operator ==( FArray1S< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == t );
	}
	return r;
}

// Slice != Value
template< typename T >
inline
FArray1D< bool >
operator !=( FArray1S< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != t );
	}
	return r;
}

// Slice < Value
template< typename T >
inline
FArray1D< bool >
operator <( FArray1S< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < t );
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
FArray1D< bool >
operator <=( FArray1S< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= t );
	}
	return r;
}

// Slice > Value
template< typename T >
inline
FArray1D< bool >
operator >( FArray1S< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > t );
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
FArray1D< bool >
operator >=( FArray1S< T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= t );
	}
	return r;
}

// Value == Slice
template< typename T >
inline
FArray1D< bool >
operator ==( T const & t, FArray1S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
FArray1D< bool >
operator !=( T const & t, FArray1S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
FArray1D< bool >
operator <( T const & t, FArray1S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
FArray1D< bool >
operator <=( T const & t, FArray1S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
FArray1D< bool >
operator >( T const & t, FArray1S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
FArray1D< bool >
operator >=( T const & t, FArray1S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
FArray1D< bool >
operator ==( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == b( i ) );
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
FArray1D< bool >
operator !=( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != b( i ) );
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < b( i ) );
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <=( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= b( i ) );
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > b( i ) );
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >=( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= b( i ) );
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
FArray1D< bool >
operator ==( MArray1< A, T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) == b[ l ] );
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
FArray1D< bool >
operator !=( MArray1< A, T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) != b[ l ] );
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
FArray1D< bool >
operator <( MArray1< A, T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) < b[ l ] );
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
FArray1D< bool >
operator <=( MArray1< A, T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) <= b[ l ] );
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
FArray1D< bool >
operator >( MArray1< A, T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) > b[ l ] );
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
FArray1D< bool >
operator >=( MArray1< A, T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	typename FArray1< T >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) >= b[ l ] );
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
FArray1D< bool >
operator ==( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
FArray1D< bool >
operator !=( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <=( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >=( FArray1< T > const & a, MArray1< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
FArray1D< bool >
operator ==( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == b( i ) );
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
FArray1D< bool >
operator !=( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != b( i ) );
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
FArray1D< bool >
operator <( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < b( i ) );
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
FArray1D< bool >
operator <=( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= b( i ) );
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
FArray1D< bool >
operator >( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > b( i ) );
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
FArray1D< bool >
operator >=( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	assert( conformable( a, b ) );
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= b( i ) );
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
FArray1D< bool >
operator ==( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
FArray1D< bool >
operator !=( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <=( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >=( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
FArray1D< bool >
operator ==( MArray1< A, T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == t );
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
FArray1D< bool >
operator !=( MArray1< A, T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != t );
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
FArray1D< bool >
operator <( MArray1< A, T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < t );
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
FArray1D< bool >
operator <=( MArray1< A, T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= t );
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
FArray1D< bool >
operator >( MArray1< A, T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > t );
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
FArray1D< bool >
operator >=( MArray1< A, T > const & a, T const & t )
{
	FArray1D< bool > r( FArray1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= t );
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
FArray1D< bool >
operator ==( T const & t, MArray1< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
FArray1D< bool >
operator !=( T const & t, MArray1< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <( T const & t, MArray1< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator <=( T const & t, MArray1< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >( T const & t, MArray1< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
FArray1D< bool >
operator >=( T const & t, MArray1< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
FArray1D< T >
operator -( FArray1< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
FArray1D< T >
operator +( FArray1< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
FArray1D< T >
operator -( FArray1< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
FArray1D< T >
operator *( FArray1< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
FArray1D< T >
operator /( FArray1< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
FArray1D< T >
operator +( FArray1< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
FArray1D< T >
operator +( T const & t, FArray1< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
FArray1D< T >
operator -( FArray1< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
FArray1D< T >
operator -( T const & t, FArray1< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
FArray1D< T >
operator *( FArray1< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
FArray1D< T >
operator *( T const & t, FArray1< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
FArray1D< T >
operator /( FArray1< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
FArray1D< T >
operator /( T const & t, FArray1< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
FArray1D< T >
operator &&( FArray1< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
FArray1D< T >
operator ||( FArray1< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: Slice

// -Slice
template< typename T >
inline
FArray1D< T >
operator -( FArray1S< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
FArray1D< T >
operator +( FArray1S< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
FArray1D< T >
operator -( FArray1S< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
FArray1D< T >
operator *( FArray1S< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
FArray1D< T >
operator /( FArray1S< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
FArray1D< T >
operator +( FArray1S< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
FArray1D< T >
operator -( FArray1S< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
FArray1D< T >
operator *( FArray1S< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
FArray1D< T >
operator /( FArray1S< T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
FArray1D< T >
operator +( FArray1< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
FArray1D< T >
operator -( FArray1< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
FArray1D< T >
operator *( FArray1< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
FArray1D< T >
operator /( FArray1< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
FArray1D< T >
operator +( FArray1S< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
FArray1D< T >
operator +( T const & t, FArray1S< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
FArray1D< T >
operator -( FArray1S< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
FArray1D< T >
operator -( T const & t, FArray1S< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
FArray1D< T >
operator *( FArray1S< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
FArray1D< T >
operator *( T const & t, FArray1S< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
FArray1D< T >
operator /( FArray1S< T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
FArray1D< T >
operator /( T const & t, FArray1S< T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
FArray1D< T >
operator &&( FArray1S< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
FArray1D< T >
operator ||( FArray1S< T > const & a, FArray1S< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
FArray1D< T >
operator -( MArray1< A, T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
FArray1D< T >
operator +( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
FArray1D< T >
operator -( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
FArray1D< T >
operator *( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
FArray1D< T >
operator /( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
FArray1D< T >
operator +( MArray1< A, T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
FArray1D< T >
operator -( MArray1< A, T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
FArray1D< T >
operator *( MArray1< A, T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
FArray1D< T >
operator /( MArray1< A, T > const & a, FArray1< T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
FArray1D< T >
operator +( FArray1< T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
FArray1D< T >
operator -( FArray1< T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
FArray1D< T >
operator *( FArray1< T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
FArray1D< T >
operator /( FArray1< T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
FArray1D< T >
operator +( MArray1< A, T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
FArray1D< T >
operator +( T const & t, MArray1< A, T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
FArray1D< T >
operator -( MArray1< A, T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
FArray1D< T >
operator -( T const & t, MArray1< A, T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
FArray1D< T >
operator *( MArray1< A, T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
FArray1D< T >
operator *( T const & t, MArray1< A, T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
FArray1D< T >
operator /( MArray1< A, T > const & a, T const & t )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
FArray1D< T >
operator /( T const & t, MArray1< A, T > const & a )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
FArray1D< T >
operator &&( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
FArray1D< T >
operator ||( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	FArray1D< T > r( FArray1D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Cross Product of Two 3-Tuple Vectors
template< typename T >
inline
FArray1D< T >
cross( FArray1< T > const & a, FArray1< T > const & b )
{
	assert( conformable( a, b ) );
	assert( a.size() == 3 );
	FArray1D< T > c( FArray1D< T >::one_based( a ) );
	typename FArray1D< T >::size_type const x( 0 ), y( 1 ), z( 2 );
	c[ x ] = ( a[ y ] * b[ z ] ) - ( a[ z ] * b[ y ] );
	c[ y ] = ( a[ z ] * b[ x ] ) - ( a[ x ] * b[ z ] );
	c[ z ] = ( a[ x ] * b[ y ] ) - ( a[ y ] * b[ x ] );
	return c;
}

// Cross Product of Two 3-Tuple Vectors
template< typename T >
inline
FArray1D< T >
cross_product( FArray1< T > const & a, FArray1< T > const & b )
{
	return cross( a, b );
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

// std::swap( FArray1D, FArray1D )
template< typename T >
inline
void
swap( ObjexxFCL::FArray1D< T > & a, ObjexxFCL::FArray1D< T > & b )
{
	a.swap( b );
}

} // std

#endif // NO_STD_SWAP_OVERLOADS

#endif // ObjexxFCL_FArray1D_hh_INCLUDED
