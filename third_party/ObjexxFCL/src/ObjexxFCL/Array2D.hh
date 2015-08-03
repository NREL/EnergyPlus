#ifndef ObjexxFCL_Array2D_hh_INCLUDED
#define ObjexxFCL_Array2D_hh_INCLUDED

// Array2D: Row-Major 2D Array
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
#include <ObjexxFCL/Array2D.fwd.hh>
#include <ObjexxFCL/Array2.hh>
#include <ObjexxFCL/ArrayInitializer.hh>

namespace ObjexxFCL {

// Array2D: Row-Major 2D Array
template< typename T >
class Array2D : public Array2< T >
{

private: // Types

	typedef  Array2< T >  Super;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class Array2D;
	friend class Array2A< T >;

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

	typedef  ArrayInitializer< T, ObjexxFCL::Array2D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::clear_move;
	using Super::conformable;
	using Super::contains;
	using Super::index;
	using Super::initialize;
	using Super::isize1;
	using Super::isize2;
	using Super::l1;
	using Super::l2;
	using Super::operator ();
	using Super::operator [];
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size2;
	using Super::size_of;
	using Super::swap2;
	using Super::u1;
	using Super::u2;
	using Super::data_;
	using Super::I1_;
	using Super::I2_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;
	using Super::z2_;

public: // Creation

	// Default Constructor
	inline
	Array2D()
	{}

	// Copy Constructor
	inline
	Array2D( Array2D const & a ) :
	 Super( a ),
	 initializer_( a.initializer_ )
	{}

	// Move Constructor
	inline
	Array2D( Array2D && a ) NOEXCEPT :
	 Super( std::move( a ) ),
	 initializer_( a.initializer_ )
	{
		a.initializer_.clear();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array2D( Array2D< U > const & a ) :
	 Super( a ),
	 initializer_( a.initializer_ )
	{}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array2D( Array2< U > const & a ) :
	 Super( a )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array2D( Array2S< U > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
				initialize( l, a( i1, i2 ) );
			}
		}
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	Array2D( MArray2< A, M > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
				initialize( l, a( i1, i2 ) );
			}
		}
	}

	// Sticky Initializer Value Constructor
	inline
	explicit
	Array2D( Sticky< T > const & t ) :
	 initializer_( t )
	{}

	// IndexRange Constructor
	inline
	Array2D( IR const & I1, IR const & I2 ) :
	 Super( I1, I2 )
	{
		setup_real();
	}

	// IndexRange + Initializer Value Constructor
	inline
	Array2D( IR const & I1, IR const & I2, T const & t ) :
	 Super( I1, I2, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	Array2D( IR const & I1, IR const & I2, Sticky< T > const & t ) :
	 Super( I1, I2, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	Array2D( IR const & I1, IR const & I2, Sticky< T > const & t, T const & u ) :
	 Super( I1, I2, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
	}

	// IndexRange + Initializer Function Constructor
	inline
	Array2D( IR const & I1, IR const & I2, InitializerFunction const & fxn ) :
	 Super( I1, I2, InitializerSentinel() ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array2D( IR const & I1, IR const & I2, std::initializer_list< U > const l ) :
	 Super( I1, I2, l )
	{
		setup_real();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array2D( IR const & I1, IR const & I2, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( I1, I2, InitializerSentinel() ),
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
	Array2D( IR const & I1, IR const & I2, Array2< U > const & a ) :
	 Super( I1, I2 )
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
	Array2D( IR const & I1, IR const & I2, Sticky< T > const & t, Array2< U > const & a ) :
	 Super( I1, I2, InitializerSentinel() ),
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
	Array2D( IR const & I1, IR const & I2, Array2S< U > const & a ) :
	 Super( I1, I2 )
	{
		setup_real();
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
				initialize( l, a( i1, i2 ) );
			}
		}
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	Array2D( IR const & I1, IR const & I2, MArray2< A, M > const & a ) :
	 Super( I1, I2 )
	{
		setup_real();
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2, ++l ) {
				initialize( l, a( i1, i2 ) );
			}
		}
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array2D( Array2< U > const & a, IR const & I1, IR const & I2 ) :
	 Super( I1, I2 )
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
	Array2D( IR const & I1, IR const & I2, Array< U > const & a ) :
	 Super( I1, I2 )
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
	Array2D( Array< U > const & a, IR const & I1, IR const & I2 ) :
	 Super( I1, I2 )
	{
		setup_real();
		assert( size_ == a.size() );
		for ( size_type i = 0, e = size_; i < e; ++i ) {
			initialize( i, a[ i ] );
		}
	}

	// Range Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	range( Array2< U > const & a )
	{
		return Array2D( a.I1_, a.I2_ );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	range( Array2< U > const & a, T const & t )
	{
		return Array2D( a.I1_, a.I2_, t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	shape( Array2< U > const & a )
	{
		return Array2D( a.isize1(), a.isize2() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	shape( Array2< U > const & a, T const & t )
	{
		return Array2D( a.isize1(), a.isize2(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	shape( Array2S< U > const & a )
	{
		return Array2D( a.isize1(), a.isize2() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	shape( Array2S< U > const & a, T const & t )
	{
		return Array2D( a.isize1(), a.isize2(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array2D
	shape( MArray2< A, M > const & a )
	{
		return Array2D( a.isize1(), a.isize2() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array2D
	shape( MArray2< A, M > const & a, T const & t )
	{
		return Array2D( a.isize1(), a.isize2(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	one_based( Array2< U > const & a )
	{
		return Array2D( a, a.isize1(), a.isize2() );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	Array2D
	one_based( Array2S< U > const & a )
	{
		return Array2D( a.isize1(), a.isize2(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array2D
	one_based( MArray2< A, M > const & a )
	{
		return Array2D( a.isize1(), a.isize2(), a );
	}

	// Diagonal Matrix Named Constructor
	inline
	static
	Array2D
	diag( IR const & I, T const & d )
	{
		Array2D D( I, I );
		D.to_diag( d );
		return D;
	}

	// Identity Matrix Named Constructor
	inline
	static
	Array2D
	identity( IR const & I )
	{
		Array2D D( I, I );
		D.to_diag( T( 1 ) );
		return D;
	}

	// Destructor
	inline
	virtual
	~Array2D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	Array2D &
	operator =( Array2D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) size_real( a.I1_, a.I2_ );
			Base::operator =( a );
		}
		return *this;
	}

	// Move Assignment
	inline
	Array2D &
	operator =( Array2D && a ) NOEXCEPT
	{
		if ( conformable( a ) ) {
			Base::conformable_move( a );
		} else {
			Base::operator =( std::move( a ) );
			I1_ = a.I1_;
			I2_ = a.I2_;
			z1_ = a.z1_;
			z2_ = a.z2_;
		}
		a.clear_move();
		return *this;
	}

	// Super Assignment
	inline
	Array2D &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) size_real( a.I1_, a.I2_ );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator =( Array2< U > const & a )
	{
		if ( ! conformable( a ) ) size_real( a.I1_, a.I2_ );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator =( Array2S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	Array2D &
	operator =( MArray2< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator +=( Array2< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator -=( Array2< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator *=( Array2< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator /=( Array2< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator +=( Array2S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator -=( Array2S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator *=( Array2S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	operator /=( Array2S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	Array2D &
	operator +=( MArray2< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	Array2D &
	operator -=( MArray2< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	Array2D &
	operator *=( MArray2< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	Array2D &
	operator /=( MArray2< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	and_equals( Array2< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	or_equals( Array2< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	and_equals( Array2S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array2D &
	or_equals( Array2S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	Array2D &
	and_equals( MArray2< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	Array2D &
	or_equals( MArray2< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	Array2D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	Array2D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	Array2D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	Array2D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	Array2D &
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
		assert( contains( i1, i2 ) );
		size_type const offset( ( ( i1 * z2_ ) + i2 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), size_ - offset );
	}

	// Tail Starting at array( i1, i2 )
	inline
	Tail
	a( int const i1, int const i2 )
	{
		assert( contains( i1, i2 ) );
		size_type const offset( ( ( i1 * z2_ ) + i2 ) - shift_ );
		return Tail( data_ + offset, size_ - offset );
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
	Array2D &
	clear()
	{
		Super::clear();
		initializer_.clear();
		return *this;
	}

	// Dimension by IndexRange
	inline
	Array2D &
	allocate( IR const & I1, IR const & I2 )
	{
		dimension_real( I1, I2 );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	Array2D &
	allocate( Array2< U > const & a )
	{
		dimension_real( a.I1_, a.I2_ );
		return *this;
	}

	// Deallocate
	inline
	Array2D &
	deallocate()
	{
		Super::clear();
		initializer_.clear_nonsticky();
		return *this;
	}

	// Dimension by IndexRange
	inline
	Array2D &
	dimension( IR const & I1, IR const & I2 )
	{
		dimension_real( I1, I2 );
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	Array2D &
	dimension( IR const & I1, IR const & I2, T const & t )
	{
		dimension_real( I1, I2, t );
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	Array2D &
	dimension( IR const & I1, IR const & I2, InitializerFunction const & fxn )
	{
		dimension_real( I1, I2, fxn );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	Array2D &
	dimension( Array2< U > const & a )
	{
		dimension_real( a.I1_, a.I2_ );
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	Array2D &
	dimension( Array2< U > const & a, T const & t )
	{
		dimension_real( a.I1_, a.I2_, t );
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	Array2D &
	dimension( Array2< U > const & a, InitializerFunction const & fxn )
	{
		dimension_real( a.I1_, a.I2_, fxn );
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	Array2D &
	redimension( IR const & I1, IR const & I2 )
	{
		Array2D o( I1, I2 );
		int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
		int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
		size_type const s2( o.z2_ );
		size_type l_beg( index( b1, b2 ) );
		size_type m_beg( o.index( b1, b2 ) );
		size_type l, m;
		for ( int i1 = b1; i1 <= e1; ++i1, l_beg += z2_, m_beg += s2 ) {
			l = l_beg; m = m_beg;
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = operator []( l );
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	Array2D &
	redimension( IR const & I1, IR const & I2, T const & t )
	{
		Array2D o( I1, I2, t );
		int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
		int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
		size_type const s2( o.z2_ );
		size_type l_beg( index( b1, b2 ) );
		size_type m_beg( o.index( b1, b2 ) );
		size_type l, m;
		for ( int i1 = b1; i1 <= e1; ++i1, l_beg += z2_, m_beg += s2 ) {
			l = l_beg; m = m_beg;
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = operator []( l );
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array Template
	template< typename U >
	inline
	Array2D &
	redimension( Array2< U > const & a )
	{
		Array2D o( a.I1_, a.I2_ );
		int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
		int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
		size_type const s2( o.z2_ );
		size_type l_beg( index( b1, b2 ) );
		size_type m_beg( o.index( b1, b2 ) );
		size_type l, m;
		for ( int i1 = b1; i1 <= e1; ++i1, l_beg += z2_, m_beg += s2 ) {
			l = l_beg; m = m_beg;
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = operator []( l );
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array + Fill Value Template
	template< typename U >
	inline
	Array2D &
	redimension( Array2< U > const & a, T const & t )
	{
		Array2D o( a.I1_, a.I2_, t );
		int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
		int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
		size_type const s2( o.z2_ );
		size_type l_beg( index( b1, b2 ) );
		size_type m_beg( o.index( b1, b2 ) );
		size_type l, m;
		for ( int i1 = b1; i1 <= e1; ++i1, l_beg += z2_, m_beg += s2 ) {
			l = l_beg; m = m_beg;
			for ( int i2 = b2; i2 <= e2; ++i2, ++l, ++m ) {
				o[ m ] = operator []( l );
			}
		}
		return swap( o );
	}

	// Set Initializer Value
	inline
	Array2D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	Array2D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	Array2D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	Array2D &
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
	Array2D &
	swap( Array2D & v )
	{
		using std::swap;
		swap2( v );
		swap( initializer_, v.initializer_ );
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( IR const & I1, IR const & I2 )
	{
		size_real( I1, I2 );
	}

private: // Functions

	// Set Up for IndexRange Constructor
	inline
	void
	setup_real()
	{
		shift_set( ( I1_.l() * z2_ ) + I2_.l() );
	}

	// Size by IndexRange
	inline
	void
	size_real( IR const & I1, IR const & I2 )
	{
		I1_.assign( I1 );
		I2_.assign( I2 );
		z1_ = I1_.size();
		z2_ = I2_.size();
		resize( size_of( z1_, z2_ ) );
		setup_real();
	}

	// Dimension by IndexRange
	inline
	void
	dimension_real( IR const & I1, IR const & I2 )
	{
		size_real( I1, I2 );
		initializer_.clear_nonsticky();
		initialize();
	}

	// Dimension by IndexRange + Initializer Value
	inline
	void
	dimension_real( IR const & I1, IR const & I2, T const & t )
	{
		size_real( I1, I2 );
		initializer_ = t;
		initialize();
	}

	// Dimension by IndexRange + Initializer Function
	inline
	void
	dimension_real( IR const & I1, IR const & I2, InitializerFunction const & fxn )
	{
		size_real( I1, I2 );
		initializer_ = fxn;
		initialize();
	}

private: // Data

	Initializer initializer_; // Array initializer

}; // Array2D

// Swap
template< typename T >
inline
void
swap( Array2D< T > & a, Array2D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
Array2D< bool >
operator ==( Array2< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
Array2D< bool >
operator !=( Array2< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
Array2D< bool >
operator <( Array2< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
Array2D< bool >
operator <=( Array2< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
Array2D< bool >
operator >( Array2< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
Array2D< bool >
operator >=( Array2< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
Array2D< bool >
operator ==( Array2< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
Array2D< bool >
operator !=( Array2< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
Array2D< bool >
operator <( Array2< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
Array2D< bool >
operator <=( Array2< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
Array2D< bool >
operator >( Array2< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
Array2D< bool >
operator >=( Array2< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
Array2D< bool >
operator ==( T const & t, Array2< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
Array2D< bool >
operator !=( T const & t, Array2< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
Array2D< bool >
operator <( T const & t, Array2< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
Array2D< bool >
operator <=( T const & t, Array2< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
Array2D< bool >
operator >( T const & t, Array2< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
Array2D< bool >
operator >=( T const & t, Array2< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
Array2D< bool >
operator ==( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == b( i1, i2 ) );
		}
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
Array2D< bool >
operator !=( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != b( i1, i2 ) );
		}
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
Array2D< bool >
operator <( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < b( i1, i2 ) );
		}
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
Array2D< bool >
operator <=( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= b( i1, i2 ) );
		}
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
Array2D< bool >
operator >( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > b( i1, i2 ) );
		}
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
Array2D< bool >
operator >=( Array2S< T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= b( i1, i2 ) );
		}
	}
	return r;
}

// Slice == Array
template< typename T >
inline
Array2D< bool >
operator ==( Array2S< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == b[ l ] );
		}
	}
	return r;
}

// Slice != Array
template< typename T >
inline
Array2D< bool >
operator !=( Array2S< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != b[ l ] );
		}
	}
	return r;
}

// Slice < Array
template< typename T >
inline
Array2D< bool >
operator <( Array2S< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < b[ l ] );
		}
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
Array2D< bool >
operator <=( Array2S< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= b[ l ] );
		}
	}
	return r;
}

// Slice > Array
template< typename T >
inline
Array2D< bool >
operator >( Array2S< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > b[ l ] );
		}
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
Array2D< bool >
operator >=( Array2S< T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= b[ l ] );
		}
	}
	return r;
}

// Array == Slice
template< typename T >
inline
Array2D< bool >
operator ==( Array2< T > const & a, Array2S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
Array2D< bool >
operator !=( Array2< T > const & a, Array2S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
Array2D< bool >
operator <( Array2< T > const & a, Array2S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
Array2D< bool >
operator <=( Array2< T > const & a, Array2S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
Array2D< bool >
operator >( Array2< T > const & a, Array2S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
Array2D< bool >
operator >=( Array2< T > const & a, Array2S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
Array2D< bool >
operator ==( Array2S< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == t );
		}
	}
	return r;
}

// Slice != Value
template< typename T >
inline
Array2D< bool >
operator !=( Array2S< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != t );
		}
	}
	return r;
}

// Slice < Value
template< typename T >
inline
Array2D< bool >
operator <( Array2S< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < t );
		}
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
Array2D< bool >
operator <=( Array2S< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= t );
		}
	}
	return r;
}

// Slice > Value
template< typename T >
inline
Array2D< bool >
operator >( Array2S< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > t );
		}
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
Array2D< bool >
operator >=( Array2S< T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= t );
		}
	}
	return r;
}

// Value == Slice
template< typename T >
inline
Array2D< bool >
operator ==( T const & t, Array2S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
Array2D< bool >
operator !=( T const & t, Array2S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
Array2D< bool >
operator <( T const & t, Array2S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
Array2D< bool >
operator <=( T const & t, Array2S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
Array2D< bool >
operator >( T const & t, Array2S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
Array2D< bool >
operator >=( T const & t, Array2S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
Array2D< bool >
operator ==( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == b( i1, i2 ) );
		}
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
Array2D< bool >
operator !=( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != b( i1, i2 ) );
		}
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
Array2D< bool >
operator <( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < b( i1, i2 ) );
		}
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
Array2D< bool >
operator <=( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= b( i1, i2 ) );
		}
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
Array2D< bool >
operator >( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > b( i1, i2 ) );
		}
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
Array2D< bool >
operator >=( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= b( i1, i2 ) );
		}
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
Array2D< bool >
operator ==( MArray2< A, T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == b[ l ] );
		}
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
Array2D< bool >
operator !=( MArray2< A, T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != b[ l ] );
		}
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
Array2D< bool >
operator <( MArray2< A, T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < b[ l ] );
		}
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
Array2D< bool >
operator <=( MArray2< A, T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= b[ l ] );
		}
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
Array2D< bool >
operator >( MArray2< A, T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > b[ l ] );
		}
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
Array2D< bool >
operator >=( MArray2< A, T > const & a, Array2< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= b[ l ] );
		}
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
Array2D< bool >
operator ==( Array2< T > const & a, MArray2< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
Array2D< bool >
operator !=( Array2< T > const & a, MArray2< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
Array2D< bool >
operator <( Array2< T > const & a, MArray2< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
Array2D< bool >
operator <=( Array2< T > const & a, MArray2< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
Array2D< bool >
operator >( Array2< T > const & a, MArray2< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
Array2D< bool >
operator >=( Array2< T > const & a, MArray2< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
Array2D< bool >
operator ==( MArray2< A, T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == b( i1, i2 ) );
		}
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
Array2D< bool >
operator !=( MArray2< A, T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != b( i1, i2 ) );
		}
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
Array2D< bool >
operator <( MArray2< A, T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < b( i1, i2 ) );
		}
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
Array2D< bool >
operator <=( MArray2< A, T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= b( i1, i2 ) );
		}
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
Array2D< bool >
operator >( MArray2< A, T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > b( i1, i2 ) );
		}
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
Array2D< bool >
operator >=( MArray2< A, T > const & a, Array2S< T > const & b )
{
	assert( conformable( a, b ) );
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= b( i1, i2 ) );
		}
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
Array2D< bool >
operator ==( Array2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
Array2D< bool >
operator !=( Array2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
Array2D< bool >
operator <( Array2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
Array2D< bool >
operator <=( Array2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
Array2D< bool >
operator >( Array2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
Array2D< bool >
operator >=( Array2S< T > const & a, MArray2< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
Array2D< bool >
operator ==( MArray2< A, T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) == t );
		}
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
Array2D< bool >
operator !=( MArray2< A, T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) != t );
		}
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
Array2D< bool >
operator <( MArray2< A, T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) < t );
		}
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
Array2D< bool >
operator <=( MArray2< A, T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) <= t );
		}
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
Array2D< bool >
operator >( MArray2< A, T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) > t );
		}
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
Array2D< bool >
operator >=( MArray2< A, T > const & a, T const & t )
{
	Array2D< bool > r( Array2D< bool >::shape( a ) );
	Array2D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2, ++l ) {
			r[ l ] = ( a( i1, i2 ) >= t );
		}
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
Array2D< bool >
operator ==( T const & t, MArray2< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
Array2D< bool >
operator !=( T const & t, MArray2< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
Array2D< bool >
operator <( T const & t, MArray2< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
Array2D< bool >
operator <=( T const & t, MArray2< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
Array2D< bool >
operator >( T const & t, MArray2< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
Array2D< bool >
operator >=( T const & t, MArray2< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
Array2D< T >
operator -( Array2< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
Array2D< T >
operator +( Array2< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
Array2D< T >
operator -( Array2< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
Array2D< T >
operator *( Array2< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
Array2D< T >
operator /( Array2< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
Array2D< T >
operator +( Array2< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
Array2D< T >
operator +( T const & t, Array2< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
Array2D< T >
operator -( Array2< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
Array2D< T >
operator -( T const & t, Array2< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
Array2D< T >
operator *( Array2< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
Array2D< T >
operator *( T const & t, Array2< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
Array2D< T >
operator /( Array2< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
Array2D< T >
operator /( T const & t, Array2< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
Array2D< T >
operator &&( Array2< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
Array2D< T >
operator ||( Array2< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Array Transpose: Fortran-Compatible 1-Based Indexing
template< typename T >
inline
Array2D< T >
transpose( Array2< T > const & a )
{
	typedef  typename Array2D< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	Array2D< T > aT( static_cast< int >( as2 ), static_cast< int >( as1 ) );
	for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
		for ( size_type i2 = 0, lT = i1; i2 < as2; ++i2, ++l, lT += as1 ) {
			aT[ lT ] = a[ l ];
		}
	}
	return aT;
}

// Array Transposed: Preserved Indexing
template< typename T >
inline
Array2D< T >
transposed( Array2< T > const & a )
{
	typedef  typename Array2D< T >::size_type  size_type;
	size_type const as1( a.size1() );
	size_type const as2( a.size2() );
	Array2D< T > aT( a.I2(), a.I1() );
	for ( size_type i1 = 0, l = 0; i1 < as1; ++i1 ) {
		for ( size_type i2 = 0, lT = i1; i2 < as2; ++i2, ++l, lT += as1 ) {
			aT[ lT ] = a[ l ];
		}
	}
	return aT;
}

// Generator: Slice

// -Slice
template< typename T >
inline
Array2D< T >
operator -( Array2S< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
Array2D< T >
operator +( Array2S< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
Array2D< T >
operator -( Array2S< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
Array2D< T >
operator *( Array2S< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
Array2D< T >
operator /( Array2S< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
Array2D< T >
operator +( Array2S< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
Array2D< T >
operator -( Array2S< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
Array2D< T >
operator *( Array2S< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
Array2D< T >
operator /( Array2S< T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
Array2D< T >
operator +( Array2< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
Array2D< T >
operator -( Array2< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
Array2D< T >
operator *( Array2< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
Array2D< T >
operator /( Array2< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
Array2D< T >
operator +( Array2S< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
Array2D< T >
operator +( T const & t, Array2S< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
Array2D< T >
operator -( Array2S< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
Array2D< T >
operator -( T const & t, Array2S< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
Array2D< T >
operator *( Array2S< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
Array2D< T >
operator *( T const & t, Array2S< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
Array2D< T >
operator /( Array2S< T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
Array2D< T >
operator /( T const & t, Array2S< T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
Array2D< T >
operator &&( Array2S< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
Array2D< T >
operator ||( Array2S< T > const & a, Array2S< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Slice Transpose: Fortran-Compatible 1-Based Indexing
template< typename T >
inline
Array2D< T >
transpose( Array2S< T > const & a )
{
	typedef  typename Array2S< T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	Array2D< T > aT( as2, as1 );
	size_type l( 0 );
	for ( int i1 = 1; i1 <= as2; ++i1 ) {
		for ( int i2 = 1; i2 <= as1; ++i2, ++l ) {
			aT[ l ] = a( i2, i1 );
		}
	}
	return aT;
}

// Slice Transposed: Preserved Indexing
template< typename T >
inline
Array2D< T >
transposed( Array2S< T > const & a )
{
	return transpose( a ); // Slice indexing is 1-based
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
Array2D< T >
operator -( MArray2< A, T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
Array2D< T >
operator +( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
Array2D< T >
operator -( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
Array2D< T >
operator *( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
Array2D< T >
operator /( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
Array2D< T >
operator +( MArray2< A, T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
Array2D< T >
operator -( MArray2< A, T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
Array2D< T >
operator *( MArray2< A, T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
Array2D< T >
operator /( MArray2< A, T > const & a, Array2< T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
Array2D< T >
operator +( Array2< T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
Array2D< T >
operator -( Array2< T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
Array2D< T >
operator *( Array2< T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
Array2D< T >
operator /( Array2< T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
Array2D< T >
operator +( MArray2< A, T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
Array2D< T >
operator +( T const & t, MArray2< A, T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
Array2D< T >
operator -( MArray2< A, T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
Array2D< T >
operator -( T const & t, MArray2< A, T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
Array2D< T >
operator *( MArray2< A, T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
Array2D< T >
operator *( T const & t, MArray2< A, T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
Array2D< T >
operator /( MArray2< A, T > const & a, T const & t )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
Array2D< T >
operator /( T const & t, MArray2< A, T > const & a )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
Array2D< T >
operator &&( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
Array2D< T >
operator ||( MArray2< A, T > const & a, MArray2< A, T > const & b )
{
	Array2D< T > r( Array2D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// MArray Transpose: Fortran-Compatible 1-Based Indexing
template< class A, typename T >
inline
Array2D< T >
transpose( MArray2< A, T > const & a )
{
	typedef  typename MArray2< A, T >::size_type  size_type;
	int const as1( a.isize1() );
	int const as2( a.isize2() );
	Array2D< T > aT( as2, as1 );
	size_type l( 0 );
	for ( int i1 = 1; i1 <= as2; ++i1 ) {
		for ( int i2 = 1; i2 <= as1; ++i2, ++l ) {
			aT[ l ] = a( i2, i1 );
		}
	}
	return aT;
}

// MArray Transposed: Preserved Indexing
template< class A, typename T >
inline
Array2D< T >
transposed( MArray2< A, T > const & a )
{
	return transpose( a ); // MArray indexing is 1-based
}

} // ObjexxFCL

#endif // ObjexxFCL_Array2D_hh_INCLUDED
