#ifndef ObjexxFCL_Array1D_hh_INCLUDED
#define ObjexxFCL_Array1D_hh_INCLUDED

// Array1D: 1D Array
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
#include <ObjexxFCL/Array1D.fwd.hh>
#include <ObjexxFCL/Array1.hh>
#include <ObjexxFCL/ArrayInitializer.hh>

namespace ObjexxFCL {

// Array1D: 1D Array
template< typename T >
class Array1D : public Array1< T >
{

private: // Types

	typedef  Array1< T >  Super;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class Array1D;
	friend class Array1A< T >;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Tail  Tail;
	typedef  typename Super::IR  IR;

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

	typedef  ArrayInitializer< T, ObjexxFCL::Array1D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::clear_move;
	using Super::conformable;
	using Super::contains;
	using Super::index;
	using Super::initialize;
	using Super::isize1;
	using Super::l;
	using Super::operator ();
	using Super::operator [];
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size_of;
	using Super::swap1;
	using Super::u;
	using Super::capacity_;
	using Super::data_;
	using Super::I_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;

public: // Creation

	// Default Constructor
	inline
	Array1D()
	{}

	// Copy Constructor
	inline
	Array1D( Array1D const & a ) :
	 Super( a ),
	 initializer_( a.initializer_ )
	{}

	// Move Constructor
	inline
	Array1D( Array1D && a ) NOEXCEPT :
	 Super( std::move( a ) ),
	 initializer_( a.initializer_ )
	{
		a.initializer_.clear();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array1D( Array1D< U > const & a ) :
	 Super( a ),
	 initializer_( a.initializer_ )
	{}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array1D( Array1< U > const & a ) :
	 Super( a )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array1D( Array1S< U > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			initialize( l, a( i ) );
		}
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	Array1D( MArray1< A, M > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			initialize( l, a( i ) );
		}
	}

	// Sticky Initializer Value Constructor
	inline
	explicit
	Array1D( Sticky< T > const & t ) :
	 initializer_( t )
	{}

	// IndexRange Constructor
	inline
	explicit
	Array1D( IR const & I ) :
	 Super( I )
	{
		setup_real();
	}

	// IndexRange + Initializer Value Constructor
	inline
	Array1D( IR const & I, T const & t ) :
	 Super( I, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	Array1D( IR const & I, Sticky< T > const & t ) :
	 Super( I, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	Array1D( IR const & I, Sticky< T > const & t, T const & u ) :
	 Super( I, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
	}

	// IndexRange + Initializer Function Constructor
	inline
	Array1D( IR const & I, InitializerFunction const & fxn ) :
	 Super( I, InitializerSentinel() ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( IR const & I, std::initializer_list< U > const l ) :
	 Super( I, l )
	{
		setup_real();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( IR const & I, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( I, InitializerSentinel() ),
	 initializer_( t )
	{
		assert( size_ == l.size() );
		setup_real();
		initialize();
		std::copy( l.begin(), l.end(), data_ );
	}

	// IndexRange + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( IR const & I, Array1< U > const & a ) :
	 Super( I )
	{
		setup_real();
		assert( conformable( a ) );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// IndexRange + Sticky Initializer + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( IR const & I, Sticky< T > const & t, Array1< U > const & a ) :
	 Super( I, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		assert( conformable( a ) );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			data_[ i ] = a[ i ];
		}
	}

	// IndexRange + Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( IR const & I, Array1S< U > const & a ) :
	 Super( I )
	{
		setup_real();
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			initialize( l, a( i ) );
		}
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	Array1D( IR const & I, MArray1< A, M > const & a ) :
	 Super( I )
	{
		setup_real();
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i, ++l ) {
			initialize( l, a( i ) );
		}
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( Array1< U > const & a, IR const & I ) :
	 Super( I )
	{
		setup_real();
		assert( conformable( a ) );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// IndexRange + Base Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( IR const & I, Array< U > const & a ) :
	 Super( I )
	{
		setup_real();
		assert( size_ == a.size() );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// Base + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( Array< U > const & a, IR const & I ) :
	 Super( I )
	{
		setup_real();
		assert( size_ == a.size() );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// Initializer List Index Range Constructor Template
	template< typename U, class = typename std::enable_if< ! std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array1D( std::initializer_list< U > const l ) :
	 Super( IR( l ) )
	{
		assert( l.size() == 2 );
		setup_real();
	}

	// Initializer List of Values Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value && ! ( std::is_same< U, int >::value || std::is_same< U, Index >::value ) >::type, typename = void >
	inline
	Array1D( std::initializer_list< U > const l ) :
	 Super( l )
	{
		setup_real();
	}

	// Initializer List of Values or Index Range Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value && ( std::is_same< U, int >::value || std::is_same< U, Index >::value ) >::type, typename = void, typename = void >
	inline
	Array1D( std::initializer_list< U > const l ) :
	 Super( l )
	{ // Note: 2 item lists treated as index range: Others treated as values: See ObjexxFCL.Users.Array.html
#ifdef OBJEXXFCL_DISALLOW_AMBIGUOUS_INITIALIZER_LIST_CONSTRUCTORS
		assert( l.size() != 2 ); // Avoid ambiguity with IndexRange {l,u} usage
#endif
		if ( l.size() == 2 ) { // Treat as an IndexRange
			I_ = l;
			Base::reconstruct_by_size( size_of( I_ ) );
		}
		setup_real();
	}

	// Initializer List Index Range + Initializer Value Constructor Template
	template< typename U >
	inline
	Array1D( std::initializer_list< U > const l, T const & t ) :
	 Super( IR( l ), InitializerSentinel() ),
	 initializer_( t )
	{
		assert( l.size() == 2 );
		setup_real();
		initialize();
	}

	// Initializer List Index Range + Sticky Initializer Value Constructor Template
	template< typename U >
	inline
	Array1D( std::initializer_list< U > const l, Sticky< T > const & t ) :
	 Super( IR( l ), InitializerSentinel() ),
	 initializer_( t )
	{
		assert( l.size() == 2 );
		setup_real();
		initialize();
	}

	// Initializer List Index Range + Initializer Function Constructor Template
	template< typename U >
	inline
	Array1D( std::initializer_list< U > const l, InitializerFunction const & fxn ) :
	 Super( IR( l ), InitializerSentinel() ),
	 initializer_( fxn )
	{
		assert( l.size() == 2 );
		setup_real();
		initialize();
	}

	// Initializer List Index Range + Super Constructor Template
	template< typename U, typename V, class = typename std::enable_if< std::is_constructible< T, V >::value >::type >
	inline
	Array1D( std::initializer_list< U > const l, Array1< V > const & a ) :
	 Super( IR( l ) )
	{
		assert( l.size() == 2 );
		setup_real();
		assert( conformable( a ) );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// Initializer List Index Range + Base Constructor Template
	template< typename U, typename V >
	inline
	Array1D( std::initializer_list< U > const l, Array< V > const & a ) :
	 Super( IR( l ) )
	{
		assert( l.size() == 2 );
		setup_real();
		assert( size_ == a.size() );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// std::array Constructor Template
	template< typename U, Size s >
	inline
	Array1D( std::array< U, s > const & a ) :
	 Super( a )
	{
		setup_real();
	}

	// std::vector Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( std::vector< U > const & v ) :
	 Super( v )
	{
		setup_real();
	}

	// Vector2 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( Vector2< U > const & v ) :
	 Super( v )
	{
		setup_real();
	}

	// Vector3 Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array1D( Vector3< U > const & v ) :
	 Super( v )
	{
		setup_real();
	}

	// Range Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	range( Array1< U > const & a )
	{
		return Array1D( a.I() );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	range( Array1< U > const & a, T const & t )
	{
		return Array1D( a.I(), t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	shape( Array1< U > const & a )
	{
		return Array1D( a.isize() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	shape( Array1< U > const & a, T const & t )
	{
		return Array1D( a.isize(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	shape( Array1S< U > const & a )
	{
		return Array1D( a.isize() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	shape( Array1S< U > const & a, T const & t )
	{
		return Array1D( a.isize(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array1D
	shape( MArray1< A, M > const & a )
	{
		return Array1D( a.isize() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array1D
	shape( MArray1< A, M > const & a, T const & t )
	{
		return Array1D( a.isize(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	one_based( Array1< U > const & a )
	{
		return Array1D( a.isize(), a );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	one_based( Array1S< U > const & a )
	{
		return Array1D( a.isize(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array1D
	one_based( MArray1< A, M > const & a )
	{
		return Array1D( a.isize(), a );
	}

	// Initializer List One-Based Named Constructor Template
	template< typename U >
	inline
	static
	Array1D
	one_based( std::initializer_list< U > const l )
	{
		return Array1D( static_cast< int >( l.size() ), l );
	}

	// Destructor
	inline
	virtual
	~Array1D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	Array1D &
	operator =( Array1D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) size_real( a.I_ );
			Base::operator =( a );
		}
		return *this;
	}

	// Move Assignment
	inline
	Array1D &
	operator =( Array1D && a ) NOEXCEPT
	{
		if ( conformable( a ) ) {
			Base::conformable_move( a );
		} else {
			Base::operator =( std::move( a ) );
			I_ = a.I_;
		}
		a.clear_move();
		return *this;
	}

	// Super Assignment
	inline
	Array1D &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) size_real( a.I_ );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( Array1< U > const & a )
	{
		if ( ! conformable( a ) ) size_real( a.I_ );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( Array1S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	Array1D &
	operator =( MArray1< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( std::array< U, s > const & a )
	{
		Base::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( std::vector< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( Vector2< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator =( Vector3< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( Array1< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( Array1< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( Array1< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( Array1< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( Array1S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( Array1S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( Array1S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( Array1S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	Array1D &
	operator +=( MArray1< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	Array1D &
	operator -=( MArray1< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	Array1D &
	operator *=( MArray1< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	Array1D &
	operator /=( MArray1< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( std::initializer_list< U > const l )
	{
		Base::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( std::initializer_list< U > const l )
	{
		Base::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( std::initializer_list< U > const l )
	{
		Base::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( std::initializer_list< U > const l )
	{
		Base::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( std::array< U, s > const & a )
	{
		Base::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( std::array< U, s > const & a )
	{
		Base::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( std::array< U, s > const & a )
	{
		Base::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( std::array< U, s > const & a )
	{
		Base::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( std::vector< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( std::vector< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( std::vector< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( std::vector< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( Vector2< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( Vector2< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( Vector2< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( Vector2< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator +=( Vector3< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator -=( Vector3< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator *=( Vector3< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	operator /=( Vector3< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( Array1< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( Array1< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( Array1S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( Array1S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	Array1D &
	and_equals( MArray1< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	Array1D &
	or_equals( MArray1< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array1D &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	Array1D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	Array1D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	Array1D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	Array1D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	Array1D &
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
		assert( contains( i ) );
		return Tail( static_cast< T const * >( sdata_ + i ), size_ - ( i - shift_ ) );
	}

	// Tail Starting at array( i )
	inline
	Tail
	a( int const i )
	{
		assert( contains( i ) );
		return Tail( sdata_ + i, size_ - ( i - shift_ ) );
	}

public: // Predicate

	// Initializer Active?
	inline
	bool
	initializer_active() const
	{
		return initializer_.is_active();
	}

public: // Modifier

	// Clear
	inline
	Array1D &
	clear()
	{
		Super::clear();
		initializer_.clear();
		return *this;
	}

	// Dimension by IndexRange
	inline
	Array1D &
	allocate( IR const & I )
	{
		dimension_real( I );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	Array1D &
	allocate( Array1< U > const & a )
	{
		dimension_real( a.I_ );
		return *this;
	}

	// Deallocate
	inline
	Array1D &
	deallocate()
	{
		Super::clear();
		initializer_.clear_nonsticky();
		return *this;
	}

	// Dimension by IndexRange
	inline
	Array1D &
	dimension( IR const & I )
	{
		dimension_real( I );
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	Array1D &
	dimension( IR const & I, T const & t )
	{
		dimension_real( I, t );
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	Array1D &
	dimension( IR const & I, InitializerFunction const & fxn )
	{
		dimension_real( I, fxn );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	Array1D &
	dimension( Array1< U > const & a )
	{
		dimension_real( a.I_ );
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	Array1D &
	dimension( Array1< U > const & a, T const & t )
	{
		dimension_real( a.I_, t );
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	Array1D &
	dimension( Array1< U > const & a, InitializerFunction const & fxn )
	{
		dimension_real( a.I_, fxn );
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	Array1D &
	redimension( IR const & I )
	{
		Array1D o( I );
		int const b( std::max( I.l(), l() ) ), e( std::min( I.u(), u() ) );
		for ( int i = b; i <= e; ++i ) {
			o( i ) = operator ()( i );
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	Array1D &
	redimension( IR const & I, T const & t )
	{
		Array1D o( I );
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

	// Data-Preserving Redimension by Array Template
	template< typename U >
	inline
	Array1D &
	redimension( Array1< U > const & a )
	{
		Array1D o( a.I_ );
		int const b( std::max( a.l(), l() ) ), e( std::min( a.u(), u() ) );
		for ( int i = b; i <= e; ++i ) {
			o( i ) = operator ()( i );
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array + Fill Value Template
	template< typename U >
	inline
	Array1D &
	redimension( Array1< U > const & a, T const & t )
	{
		auto const & I( a.I_ );
		Array1D o( I );
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

	// Append Value: Grow by 1
	inline
	Array1D &
	append( T const & t )
	{
		if ( capacity_ == size_ ) { // Grow by 1
			Array1D o( IndexRange( l(), u() + 1 ) );
			for ( int i = l(), e = u(); i <= e; ++i ) {
				o( i ) = operator ()( i );
			}
			swap( o );
		} else {
			I_.u( u() + 1 );
		}
		operator ()( u() ) = t;
		return *this;
	}

	// Append Value: Grow Capacity
	inline
	Array1D &
	push_back( T const & t )
	{
		Base::grow_capacity();
		I_.grow();
		setup_real();
		operator ()( I_.u() ) = t;
		return *this;
	}

	// Append Value: Grow Capacity
	inline
	Array1D &
	push_back( T && t )
	{
		Base::grow_capacity();
		I_.grow();
		setup_real();
		operator ()( I_.u() ) = std::move( t );
		return *this;
	}

	// Construct and Append Value: Grow Capacity
	template< class... Args >
	Array1D &
	emplace_back( Args&&... args )
	{
		Base::grow_capacity();
		operator ()( I_.grow().u() ) = T( std::forward< Args >( args )... );
		return *this;
	}

	// Remove Last Value
	inline
	Array1D &
	pop_back()
	{
		if ( size_ > 0u ) --size_;
		return *this;
	}

	// First Value
	inline
	T const &
	front() const
	{
		assert( size_ > 0u );
		return operator []( 0u );
	}

	// First Value
	inline
	T &
	front()
	{
		assert( size_ > 0u );
		return operator []( 0u );
	}

	// Last Value
	inline
	T const &
	back() const
	{
		assert( size_ > 0u );
		return operator []( size_ - 1 );
	}

	// Last Value
	inline
	T &
	back()
	{
		assert( size_ > 0u );
		return operator []( size_ - 1 );
	}

	// Reserve Capacity
	inline
	Array1D &
	reserve( size_type const n )
	{
		Base::reserve_capacity( n );
		setup_real();
		return *this;
	}

	// Shrink Capacity to Size
	inline
	Array1D &
	shrink_to_fit()
	{
		Base::shrink_capacity();
		return *this;
	}

	// Set Initializer Value
	inline
	Array1D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	Array1D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	Array1D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	Array1D &
	initialize()
	{
		if ( initializer_.is_active() ) {
			if ( initializer_.is_value() ) {
				initialize( initializer_.value() );
			} else if ( initializer_.is_function() ) {
				initializer_.function()( *this );
			}
		}
		return *this;
	}

	// Swap
	inline
	Array1D &
	swap( Array1D & v )
	{
		using std::swap;
		swap1( v );
		swap( initializer_, v.initializer_ );
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( IR const & I )
	{
		size_real( I );
	}

private: // Functions

	// Set Up for IndexRange Constructor
	inline
	void
	setup_real()
	{
		shift_set( I_.l() );
	}

	// Size by IndexRange
	inline
	void
	size_real( IR const & I )
	{
		I_.assign( I );
		resize( size_of( I_ ) );
		setup_real();
	}

	// Dimension by IndexRange
	inline
	void
	dimension_real( IR const & I )
	{
		size_real( I );
		initializer_.clear_nonsticky();
		initialize();
	}

	// Dimension by IndexRange + Initializer Value
	inline
	void
	dimension_real( IR const & I, T const & t )
	{
		size_real( I );
		initializer_ = t;
		initialize();
	}

	// Dimension by IndexRange + Initializer Function
	inline
	void
	dimension_real( IR const & I, InitializerFunction const & fxn )
	{
		size_real( I );
		initializer_ = fxn;
		initialize();
	}

private: // Data

	Initializer initializer_; // Array initializer

}; // Array1D

// Swap
template< typename T >
inline
void
swap( Array1D< T > & a, Array1D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
Array1D< bool >
operator ==( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
Array1D< bool >
operator !=( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
Array1D< bool >
operator <( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
Array1D< bool >
operator <=( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
Array1D< bool >
operator >( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
Array1D< bool >
operator >=( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
Array1D< bool >
operator ==( Array1< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
Array1D< bool >
operator !=( Array1< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
Array1D< bool >
operator <( Array1< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
Array1D< bool >
operator <=( Array1< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
Array1D< bool >
operator >( Array1< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
Array1D< bool >
operator >=( Array1< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
Array1D< bool >
operator ==( T const & t, Array1< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
Array1D< bool >
operator !=( T const & t, Array1< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
Array1D< bool >
operator <( T const & t, Array1< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
Array1D< bool >
operator <=( T const & t, Array1< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
Array1D< bool >
operator >( T const & t, Array1< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
Array1D< bool >
operator >=( T const & t, Array1< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
Array1D< bool >
operator ==( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == b( i ) );
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
Array1D< bool >
operator !=( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != b( i ) );
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
Array1D< bool >
operator <( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < b( i ) );
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
Array1D< bool >
operator <=( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= b( i ) );
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
Array1D< bool >
operator >( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > b( i ) );
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
Array1D< bool >
operator >=( Array1S< T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= b( i ) );
	}
	return r;
}

// Slice == Array
template< typename T >
inline
Array1D< bool >
operator ==( Array1S< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) == b[ l ] );
	}
	return r;
}

// Slice != Array
template< typename T >
inline
Array1D< bool >
operator !=( Array1S< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) != b[ l ] );
	}
	return r;
}

// Slice < Array
template< typename T >
inline
Array1D< bool >
operator <( Array1S< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) < b[ l ] );
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
Array1D< bool >
operator <=( Array1S< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) <= b[ l ] );
	}
	return r;
}

// Slice > Array
template< typename T >
inline
Array1D< bool >
operator >( Array1S< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) > b[ l ] );
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
Array1D< bool >
operator >=( Array1S< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) >= b[ l ] );
	}
	return r;
}

// Array == Slice
template< typename T >
inline
Array1D< bool >
operator ==( Array1< T > const & a, Array1S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
Array1D< bool >
operator !=( Array1< T > const & a, Array1S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
Array1D< bool >
operator <( Array1< T > const & a, Array1S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
Array1D< bool >
operator <=( Array1< T > const & a, Array1S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
Array1D< bool >
operator >( Array1< T > const & a, Array1S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
Array1D< bool >
operator >=( Array1< T > const & a, Array1S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
Array1D< bool >
operator ==( Array1S< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == t );
	}
	return r;
}

// Slice != Value
template< typename T >
inline
Array1D< bool >
operator !=( Array1S< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != t );
	}
	return r;
}

// Slice < Value
template< typename T >
inline
Array1D< bool >
operator <( Array1S< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < t );
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
Array1D< bool >
operator <=( Array1S< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= t );
	}
	return r;
}

// Slice > Value
template< typename T >
inline
Array1D< bool >
operator >( Array1S< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > t );
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
Array1D< bool >
operator >=( Array1S< T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= t );
	}
	return r;
}

// Value == Slice
template< typename T >
inline
Array1D< bool >
operator ==( T const & t, Array1S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
Array1D< bool >
operator !=( T const & t, Array1S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
Array1D< bool >
operator <( T const & t, Array1S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
Array1D< bool >
operator <=( T const & t, Array1S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
Array1D< bool >
operator >( T const & t, Array1S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
Array1D< bool >
operator >=( T const & t, Array1S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
Array1D< bool >
operator ==( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == b( i ) );
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
Array1D< bool >
operator !=( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != b( i ) );
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
Array1D< bool >
operator <( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < b( i ) );
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
Array1D< bool >
operator <=( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= b( i ) );
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
Array1D< bool >
operator >( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > b( i ) );
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
Array1D< bool >
operator >=( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= b( i ) );
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
Array1D< bool >
operator ==( MArray1< A, T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) == b[ l ] );
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
Array1D< bool >
operator !=( MArray1< A, T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) != b[ l ] );
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
Array1D< bool >
operator <( MArray1< A, T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) < b[ l ] );
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
Array1D< bool >
operator <=( MArray1< A, T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) <= b[ l ] );
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
Array1D< bool >
operator >( MArray1< A, T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) > b[ l ] );
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
Array1D< bool >
operator >=( MArray1< A, T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	Array1D< bool >::size_type l( 0 );
	for ( int i = 1, e = r.u(); i <= e; ++i, ++l ) {
		r( i ) = ( a( i ) >= b[ l ] );
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
Array1D< bool >
operator ==( Array1< T > const & a, MArray1< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
Array1D< bool >
operator !=( Array1< T > const & a, MArray1< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
Array1D< bool >
operator <( Array1< T > const & a, MArray1< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
Array1D< bool >
operator <=( Array1< T > const & a, MArray1< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
Array1D< bool >
operator >( Array1< T > const & a, MArray1< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
Array1D< bool >
operator >=( Array1< T > const & a, MArray1< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
Array1D< bool >
operator ==( MArray1< A, T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == b( i ) );
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
Array1D< bool >
operator !=( MArray1< A, T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != b( i ) );
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
Array1D< bool >
operator <( MArray1< A, T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < b( i ) );
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
Array1D< bool >
operator <=( MArray1< A, T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= b( i ) );
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
Array1D< bool >
operator >( MArray1< A, T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > b( i ) );
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
Array1D< bool >
operator >=( MArray1< A, T > const & a, Array1S< T > const & b )
{
	assert( conformable( a, b ) );
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= b( i ) );
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
Array1D< bool >
operator ==( Array1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
Array1D< bool >
operator !=( Array1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
Array1D< bool >
operator <( Array1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
Array1D< bool >
operator <=( Array1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
Array1D< bool >
operator >( Array1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
Array1D< bool >
operator >=( Array1S< T > const & a, MArray1< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
Array1D< bool >
operator ==( MArray1< A, T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) == t );
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
Array1D< bool >
operator !=( MArray1< A, T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) != t );
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
Array1D< bool >
operator <( MArray1< A, T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) < t );
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
Array1D< bool >
operator <=( MArray1< A, T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) <= t );
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
Array1D< bool >
operator >( MArray1< A, T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) > t );
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
Array1D< bool >
operator >=( MArray1< A, T > const & a, T const & t )
{
	Array1D< bool > r( Array1D< bool >::shape( a ) );
	for ( int i = 1, e = r.u(); i <= e; ++i ) {
		r( i ) = ( a( i ) >= t );
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
Array1D< bool >
operator ==( T const & t, MArray1< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
Array1D< bool >
operator !=( T const & t, MArray1< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
Array1D< bool >
operator <( T const & t, MArray1< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
Array1D< bool >
operator <=( T const & t, MArray1< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
Array1D< bool >
operator >( T const & t, MArray1< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
Array1D< bool >
operator >=( T const & t, MArray1< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
Array1D< T >
operator -( Array1< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
Array1D< T >
operator +( Array1< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
Array1D< T >
operator -( Array1< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
Array1D< T >
operator *( Array1< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
Array1D< T >
operator /( Array1< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
Array1D< T >
operator +( Array1< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
Array1D< T >
operator +( T const & t, Array1< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
Array1D< T >
operator -( Array1< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
Array1D< T >
operator -( T const & t, Array1< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
Array1D< T >
operator *( Array1< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
Array1D< T >
operator *( T const & t, Array1< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
Array1D< T >
operator /( Array1< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
Array1D< T >
operator /( T const & t, Array1< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
Array1D< T >
operator &&( Array1< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
Array1D< T >
operator ||( Array1< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: Slice

// -Slice
template< typename T >
inline
Array1D< T >
operator -( Array1S< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
Array1D< T >
operator +( Array1S< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
Array1D< T >
operator -( Array1S< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
Array1D< T >
operator *( Array1S< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
Array1D< T >
operator /( Array1S< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
Array1D< T >
operator +( Array1S< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
Array1D< T >
operator -( Array1S< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
Array1D< T >
operator *( Array1S< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
Array1D< T >
operator /( Array1S< T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
Array1D< T >
operator +( Array1< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
Array1D< T >
operator -( Array1< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
Array1D< T >
operator *( Array1< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
Array1D< T >
operator /( Array1< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
Array1D< T >
operator +( Array1S< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
Array1D< T >
operator +( T const & t, Array1S< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
Array1D< T >
operator -( Array1S< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
Array1D< T >
operator -( T const & t, Array1S< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
Array1D< T >
operator *( Array1S< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
Array1D< T >
operator *( T const & t, Array1S< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
Array1D< T >
operator /( Array1S< T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
Array1D< T >
operator /( T const & t, Array1S< T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
Array1D< T >
operator &&( Array1S< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
Array1D< T >
operator ||( Array1S< T > const & a, Array1S< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
Array1D< T >
operator -( MArray1< A, T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
Array1D< T >
operator +( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
Array1D< T >
operator -( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
Array1D< T >
operator *( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
Array1D< T >
operator /( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
Array1D< T >
operator +( MArray1< A, T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
Array1D< T >
operator -( MArray1< A, T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
Array1D< T >
operator *( MArray1< A, T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
Array1D< T >
operator /( MArray1< A, T > const & a, Array1< T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
Array1D< T >
operator +( Array1< T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
Array1D< T >
operator -( Array1< T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
Array1D< T >
operator *( Array1< T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
Array1D< T >
operator /( Array1< T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
Array1D< T >
operator +( MArray1< A, T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
Array1D< T >
operator +( T const & t, MArray1< A, T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
Array1D< T >
operator -( MArray1< A, T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
Array1D< T >
operator -( T const & t, MArray1< A, T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
Array1D< T >
operator *( MArray1< A, T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
Array1D< T >
operator *( T const & t, MArray1< A, T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
Array1D< T >
operator /( MArray1< A, T > const & a, T const & t )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
Array1D< T >
operator /( T const & t, MArray1< A, T > const & a )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
Array1D< T >
operator &&( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
Array1D< T >
operator ||( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	Array1D< T > r( Array1D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Cross Product of Two 3-Tuple Vectors
template< typename T >
inline
Array1D< T >
cross( Array1< T > const & a, Array1< T > const & b )
{
	assert( conformable( a, b ) );
	assert( a.size() == 3 );
	Array1D< T > c( Array1D< T >::one_based( a ) );
	typename Array1D< T >::size_type const x( 0 ), y( 1 ), z( 2 );
	c[ x ] = ( a[ y ] * b[ z ] ) - ( a[ z ] * b[ y ] );
	c[ y ] = ( a[ z ] * b[ x ] ) - ( a[ x ] * b[ z ] );
	c[ z ] = ( a[ x ] * b[ y ] ) - ( a[ y ] * b[ x ] );
	return c;
}

// Cross Product of Two 3-Tuple Vectors
template< typename T >
inline
Array1D< T >
cross_product( Array1< T > const & a, Array1< T > const & b )
{
	return cross( a, b );
}

} // ObjexxFCL

#endif // ObjexxFCL_Array1D_hh_INCLUDED
