#ifndef ObjexxFCL_Array5D_hh_INCLUDED
#define ObjexxFCL_Array5D_hh_INCLUDED

// Array5D: Row-Major 5D Array
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
#include <ObjexxFCL/Array5D.fwd.hh>
#include <ObjexxFCL/Array5.hh>
#include <ObjexxFCL/ArrayInitializer.hh>

namespace ObjexxFCL {

// Array5D: Row-Major 5D Array
template< typename T >
class Array5D : public Array5< T >
{

private: // Types

	typedef  Array5< T >  Super;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class Array5D;
	friend class Array5A< T >;

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

	typedef  ArrayInitializer< T, ObjexxFCL::Array5D >  Initializer;
	typedef  typename Initializer::Function  InitializerFunction;

	using Super::clear_move;
	using Super::conformable;
	using Super::contains;
	using Super::index;
	using Super::initialize;
	using Super::isize1;
	using Super::isize2;
	using Super::isize3;
	using Super::isize4;
	using Super::isize5;
	using Super::l1;
	using Super::l2;
	using Super::l3;
	using Super::l4;
	using Super::l5;
	using Super::move_if;
	using Super::operator ();
	using Super::operator [];
	using Super::resize;
	using Super::shift_set;
	using Super::size1;
	using Super::size2;
	using Super::size3;
	using Super::size4;
	using Super::size5;
	using Super::size_of;
	using Super::swap5;
	using Super::u1;
	using Super::u2;
	using Super::u3;
	using Super::u4;
	using Super::u5;
	using Super::data_;
	using Super::I1_;
	using Super::I2_;
	using Super::I3_;
	using Super::I4_;
	using Super::I5_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;
	using Super::z2_;
	using Super::z3_;
	using Super::z4_;
	using Super::z5_;

public: // Creation

	// Default Constructor
	inline
	Array5D()
	{}

	// Copy Constructor
	inline
	Array5D( Array5D const & a ) :
	 Super( a ),
	 initializer_( a.initializer_ )
	{}

	// Move Constructor
	inline
	Array5D( Array5D && a ) NOEXCEPT :
	 Super( std::move( a ) ),
	 initializer_( a.initializer_ )
	{
		a.initializer_.clear();
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array5D( Array5D< U > const & a ) :
	 Super( a ),
	 initializer_( a.initializer_ )
	{}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array5D( Array5< U > const & a ) :
	 Super( a )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	explicit
	Array5D( Array5S< U > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
							initialize( l, a( i1, i2, i3, i4, i5 ) );
						}
					}
				}
			}
		}
	}

	// MArray Constructor Template
	template< class A, typename M >
	inline
	explicit
	Array5D( MArray5< A, M > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
							initialize( l, a( i1, i2, i3, i4, i5 ) );
						}
					}
				}
			}
		}
	}

	// Sticky Initializer Value Constructor
	inline
	explicit
	Array5D( Sticky< T > const & t ) :
	 initializer_( t )
	{}

	// IndexRange Constructor
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( I1, I2, I3, I4, I5 )
	{
		setup_real();
	}

	// IndexRange + Initializer Value Constructor
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Sticky Initializer Value Constructor
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Sticky< T > const & t ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Sticky Initializer Value + Initializer Value Constructor
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Sticky< T > const & t, T const & u ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel() ),
	 initializer_( t )
	{
		setup_real();
		initialize();
		operator =( u );
	}

	// IndexRange + Initializer Function Constructor
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, InitializerFunction const & fxn ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel() ),
	 initializer_( fxn )
	{
		setup_real();
		initialize();
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, std::initializer_list< U > const l ) :
	 Super( I1, I2, I3, I4, I5, l )
	{
		setup_real();
	}

	// IndexRange + Sticky Initializer + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Sticky< T > const & t, std::initializer_list< U > const l ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel() ),
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
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Array5< U > const & a ) :
	 Super( I1, I2, I3, I4, I5 )
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
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Sticky< T > const & t, Array5< U > const & a ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel() ),
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
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Array5S< U > const & a ) :
	 Super( I1, I2, I3, I4, I5 )
	{
		setup_real();
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
							initialize( l, a( i1, i2, i3, i4, i5 ) );
						}
					}
				}
			}
		}
	}

	// IndexRange + MArray Constructor Template
	template< class A, typename M >
	inline
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, MArray5< A, M > const & a ) :
	 Super( I1, I2, I3, I4, I5 )
	{
		setup_real();
		assert( conformable( a ) );
		size_type l( 0 );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4 ) {
						for ( int i5 = 1, e5 = a.u5(); i5 <= e5; ++i5, ++l ) {
							initialize( l, a( i1, i2, i3, i4, i5 ) );
						}
					}
				}
			}
		}
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Array5D( Array5< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( I1, I2, I3, I4, I5 )
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
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Array< U > const & a ) :
	 Super( I1, I2, I3, I4, I5 )
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
	Array5D( Array< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( I1, I2, I3, I4, I5 )
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
	Array5D
	range( Array5< U > const & a )
	{
		return Array5D( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
	}

	// Range + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	range( Array5< U > const & a, T const & t )
	{
		return Array5D( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, t );
	}

	// Array Shape Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	shape( Array5< U > const & a )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	}

	// Array Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	shape( Array5< U > const & a, T const & t )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), t );
	}

	// Slice Shape Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	shape( Array5S< U > const & a )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	}

	// Slice Shape + Initializer Value Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	shape( Array5S< U > const & a, T const & t )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), t );
	}

	// MArray Shape Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array5D
	shape( MArray5< A, M > const & a )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	}

	// MArray Shape + Initializer Value Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array5D
	shape( MArray5< A, M > const & a, T const & t )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	one_based( Array5< U > const & a )
	{
		return Array5D( a, a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	inline
	static
	Array5D
	one_based( Array5S< U > const & a )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a );
	}

	// One-Based MArray Named Constructor Template
	template< class A, typename M >
	inline
	static
	Array5D
	one_based( MArray5< A, M > const & a )
	{
		return Array5D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5(), a );
	}

	// Destructor
	inline
	virtual
	~Array5D()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	Array5D &
	operator =( Array5D const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) size_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
			Base::operator =( a );
		}
		return *this;
	}

	// Move Assignment
	inline
	Array5D &
	operator =( Array5D && a ) NOEXCEPT
	{
		if ( conformable( a ) ) {
			Base::conformable_move( a );
		} else {
			Base::operator =( std::move( a ) );
			I1_ = a.I1_;
			I2_ = a.I2_;
			I3_ = a.I3_;
			I4_ = a.I4_;
			I5_ = a.I5_;
			z1_ = a.z1_;
			z2_ = a.z2_;
			z3_ = a.z3_;
			z4_ = a.z4_;
			z5_ = a.z5_;
		}
		a.clear_move();
		return *this;
	}

	// Super Assignment
	inline
	Array5D &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) size_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator =( Array5< U > const & a )
	{
		if ( ! conformable( a ) ) size_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator =( Array5S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	Array5D &
	operator =( MArray5< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator +=( Array5< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator -=( Array5< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator *=( Array5< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator /=( Array5< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator +=( Array5S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator -=( Array5S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator *=( Array5S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	operator /=( Array5S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	Array5D &
	operator +=( MArray5< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	Array5D &
	operator -=( MArray5< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	Array5D &
	operator *=( MArray5< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	Array5D &
	operator /=( MArray5< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	and_equals( Array5< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	or_equals( Array5< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	and_equals( Array5S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Array5D &
	or_equals( Array5S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	inline
	Array5D &
	and_equals( MArray5< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	inline
	Array5D &
	or_equals( MArray5< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	Array5D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	inline
	Array5D &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	inline
	Array5D &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	inline
	Array5D &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	inline
	Array5D &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Subscript

	// Const Tail Starting at array( i1, i2, i3, i4, i5 )
	inline
	Tail const
	a( int const i1, int const i2, int const i3, int const i4, int const i5 ) const
	{
		assert( contains( i1, i2, i3, i4, i5 ) );
		size_type const offset( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) - shift_ );
		return Tail( static_cast< T const * >( data_ + offset ), size_ - offset );
	}

	// Tail Starting at array( i1, i2, i3, i4, i5 )
	inline
	Tail
	a( int const i1, int const i2, int const i3, int const i4, int const i5 )
	{
		assert( contains( i1, i2, i3, i4, i5 ) );
		size_type const offset( ( ( ( ( ( ( ( ( i1 * z2_ ) + i2 ) * z3_ ) + i3 ) * z4_ ) + i4 ) * z5_ ) + i5 ) - shift_ );
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
	Array5D &
	clear()
	{
		Super::clear();
		initializer_.clear();
		return *this;
	}

	// Dimension by IndexRange
	inline
	Array5D &
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		dimension_real( I1, I2, I3, I4, I5 );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	Array5D &
	allocate( Array5< U > const & a )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
		return *this;
	}

	// Deallocate
	inline
	Array5D &
	deallocate()
	{
		Super::clear();
		initializer_.clear_nonsticky();
		return *this;
	}

	// Dimension by IndexRange
	inline
	Array5D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		dimension_real( I1, I2, I3, I4, I5 );
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	inline
	Array5D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t )
	{
		dimension_real( I1, I2, I3, I4, I5, t );
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	inline
	Array5D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, InitializerFunction const & fxn )
	{
		dimension_real( I1, I2, I3, I4, I5, fxn );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	inline
	Array5D &
	dimension( Array5< U > const & a )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	inline
	Array5D &
	dimension( Array5< U > const & a, T const & t )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, t );
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	inline
	Array5D &
	dimension( Array5< U > const & a, InitializerFunction const & fxn )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, fxn );
		return *this;
	}

	// Data-Preserving Redimension by IndexRange
	inline
	Array5D &
	redimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		Array5D o( I1, I2, I3, I4, I5 );
		int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
		int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
		int const b3( std::max( I3.l(), l3() ) ), e3( std::min( I3.u(), u3() ) );
		int const b4( std::max( I4.l(), l4() ) ), e4( std::min( I4.u(), u4() ) );
		int const b5( std::max( I5.l(), l5() ) ), e5( std::min( I5.u(), u5() ) );
		for ( int i1 = b1; i1 <= e1; ++i1 ) {
			for ( int i2 = b2; i2 <= e2; ++i2 ) {
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i4 = b4; i4 <= e4; ++i4 ) {
						size_type l( index( i1, i2, i3, i4, b5 ) );
						size_type m( o.index( i1, i2, i3, i4, b5 ) );
						for ( int i5 = b5; i5 <= e5; ++i5, ++l, ++m ) {
							o[ m ] = move_if( operator []( l ) );
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by IndexRange + Fill Value
	inline
	Array5D &
	redimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t )
	{
		Array5D o( I1, I2, I3, I4, I5, t );
		int const b1( std::max( I1.l(), l1() ) ), e1( std::min( I1.u(), u1() ) );
		int const b2( std::max( I2.l(), l2() ) ), e2( std::min( I2.u(), u2() ) );
		int const b3( std::max( I3.l(), l3() ) ), e3( std::min( I3.u(), u3() ) );
		int const b4( std::max( I4.l(), l4() ) ), e4( std::min( I4.u(), u4() ) );
		int const b5( std::max( I5.l(), l5() ) ), e5( std::min( I5.u(), u5() ) );
		for ( int i1 = b1; i1 <= e1; ++i1 ) {
			for ( int i2 = b2; i2 <= e2; ++i2 ) {
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i4 = b4; i4 <= e4; ++i4 ) {
						size_type l( index( i1, i2, i3, i4, b5 ) );
						size_type m( o.index( i1, i2, i3, i4, b5 ) );
						for ( int i5 = b5; i5 <= e5; ++i5, ++l, ++m ) {
							o[ m ] = move_if( operator []( l ) );
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array Template
	template< typename U >
	inline
	Array5D &
	redimension( Array5< U > const & a )
	{
		Array5D o( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
		int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
		int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
		int const b3( std::max( a.l3(), l3() ) ), e3( std::min( a.u3(), u3() ) );
		int const b4( std::max( a.l4(), l4() ) ), e4( std::min( a.u4(), u4() ) );
		int const b5( std::max( a.l5(), l5() ) ), e5( std::min( a.u5(), u5() ) );
		for ( int i1 = b1; i1 <= e1; ++i1 ) {
			for ( int i2 = b2; i2 <= e2; ++i2 ) {
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i4 = b4; i4 <= e4; ++i4 ) {
						size_type l( index( i1, i2, i3, i4, b5 ) );
						size_type m( o.index( i1, i2, i3, i4, b5 ) );
						for ( int i5 = b5; i5 <= e5; ++i5, ++l, ++m ) {
							o[ m ] = move_if( operator []( l ) );
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Data-Preserving Redimension by Array + Fill Value Template
	template< typename U >
	inline
	Array5D &
	redimension( Array5< U > const & a, T const & t )
	{
		Array5D o( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, t );
		int const b1( std::max( a.l1(), l1() ) ), e1( std::min( a.u1(), u1() ) );
		int const b2( std::max( a.l2(), l2() ) ), e2( std::min( a.u2(), u2() ) );
		int const b3( std::max( a.l3(), l3() ) ), e3( std::min( a.u3(), u3() ) );
		int const b4( std::max( a.l4(), l4() ) ), e4( std::min( a.u4(), u4() ) );
		int const b5( std::max( a.l5(), l5() ) ), e5( std::min( a.u5(), u5() ) );
		for ( int i1 = b1; i1 <= e1; ++i1 ) {
			for ( int i2 = b2; i2 <= e2; ++i2 ) {
				for ( int i3 = b3; i3 <= e3; ++i3 ) {
					for ( int i4 = b4; i4 <= e4; ++i4 ) {
						size_type l( index( i1, i2, i3, i4, b5 ) );
						size_type m( o.index( i1, i2, i3, i4, b5 ) );
						for ( int i5 = b5; i5 <= e5; ++i5, ++l, ++m ) {
							o[ m ] = move_if( operator []( l ) );
						}
					}
				}
			}
		}
		return swap( o );
	}

	// Set Initializer Value
	inline
	Array5D &
	initializer( T const & t )
	{
		initializer_ = t;
		return *this;
	}

	// Set Initializer Function
	inline
	Array5D &
	initializer( InitializerFunction const & fxn )
	{
		initializer_ = fxn;
		return *this;
	}

	// Clear Initializer
	inline
	Array5D &
	initializer_clear()
	{
		initializer_.clear();
		return *this;
	}

	// Initialize
	inline
	Array5D &
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
	Array5D &
	swap( Array5D & v )
	{
		using std::swap;
		swap5( v );
		swap( initializer_, v.initializer_ );
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	inline
	void
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		size_real( I1, I2, I3, I4, I5 );
	}

private: // Functions

	// Set Up for IndexRange Constructor
	inline
	void
	setup_real()
	{
		shift_set( ( ( ( ( ( ( ( I1_.l() * z2_ ) + I2_.l() ) * z3_ ) + I3_.l() ) * z4_ ) + I4_.l() ) * z5_ ) + I5_.l() );
	}

	// Size by IndexRange
	inline
	void
	size_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		I1_.assign( I1 );
		I2_.assign( I2 );
		I3_.assign( I3 );
		I4_.assign( I4 );
		I5_.assign( I5 );
		z1_ = I1_.size();
		z2_ = I2_.size();
		z3_ = I3_.size();
		z4_ = I4_.size();
		z5_ = I5_.size();
		resize( size_of( z1_, z2_, z3_, z4_, z5_ ) );
		setup_real();
	}

	// Dimension by IndexRange
	inline
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		size_real( I1, I2, I3, I4, I5 );
		initializer_.clear_nonsticky();
		initialize();
	}

	// Dimension by IndexRange + Initializer Value
	inline
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t )
	{
		size_real( I1, I2, I3, I4, I5 );
		initializer_ = t;
		initialize();
	}

	// Dimension by IndexRange + Initializer Function
	inline
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, InitializerFunction const & fxn )
	{
		size_real( I1, I2, I3, I4, I5 );
		initializer_ = fxn;
		initialize();
	}

private: // Data

	Initializer initializer_; // Array initializer

}; // Array5D

// Swap
template< typename T >
inline
void
swap( Array5D< T > & a, Array5D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental

// Array == Array
template< typename T >
inline
Array5D< bool >
operator ==( Array5< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	eq_elemental( a, b, r );
	return r;
}

// Array != Array
template< typename T >
inline
Array5D< bool >
operator !=( Array5< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	ne_elemental( a, b, r );
	return r;
}

// Array < Array
template< typename T >
inline
Array5D< bool >
operator <( Array5< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	lt_elemental( a, b, r );
	return r;
}

// Array <= Array
template< typename T >
inline
Array5D< bool >
operator <=( Array5< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	le_elemental( a, b, r );
	return r;
}

// Array > Array
template< typename T >
inline
Array5D< bool >
operator >( Array5< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	gt_elemental( a, b, r );
	return r;
}

// Array >= Array
template< typename T >
inline
Array5D< bool >
operator >=( Array5< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	ge_elemental( a, b, r );
	return r;
}

// Array == Value
template< typename T >
inline
Array5D< bool >
operator ==( Array5< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	eq_elemental( a, t, r );
	return r;
}

// Array != Value
template< typename T >
inline
Array5D< bool >
operator !=( Array5< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	ne_elemental( a, t, r );
	return r;
}

// Array < Value
template< typename T >
inline
Array5D< bool >
operator <( Array5< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	lt_elemental( a, t, r );
	return r;
}

// Array <= Value
template< typename T >
inline
Array5D< bool >
operator <=( Array5< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	le_elemental( a, t, r );
	return r;
}

// Array > Value
template< typename T >
inline
Array5D< bool >
operator >( Array5< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	gt_elemental( a, t, r );
	return r;
}

// Array >= Value
template< typename T >
inline
Array5D< bool >
operator >=( Array5< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	ge_elemental( a, t, r );
	return r;
}

// Value == Array
template< typename T >
inline
Array5D< bool >
operator ==( T const & t, Array5< T > const & b )
{
	return ( b == t );
}

// Value != Array
template< typename T >
inline
Array5D< bool >
operator !=( T const & t, Array5< T > const & b )
{
	return ( b != t );
}

// Value < Array
template< typename T >
inline
Array5D< bool >
operator <( T const & t, Array5< T > const & b )
{
	return ( b > t );
}

// Value <= Array
template< typename T >
inline
Array5D< bool >
operator <=( T const & t, Array5< T > const & b )
{
	return ( b >= t );
}

// Value > Array
template< typename T >
inline
Array5D< bool >
operator >( T const & t, Array5< T > const & b )
{
	return ( b < t );
}

// Value >= Array
template< typename T >
inline
Array5D< bool >
operator >=( T const & t, Array5< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: Slice

// Slice == Slice
template< typename T >
inline
Array5D< bool >
operator ==( Array5S< T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice != Slice
template< typename T >
inline
Array5D< bool >
operator !=( Array5S< T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice < Slice
template< typename T >
inline
Array5D< bool >
operator <( Array5S< T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice <= Slice
template< typename T >
inline
Array5D< bool >
operator <=( Array5S< T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice > Slice
template< typename T >
inline
Array5D< bool >
operator >( Array5S< T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice >= Slice
template< typename T >
inline
Array5D< bool >
operator >=( Array5S< T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice == Array
template< typename T >
inline
Array5D< bool >
operator ==( Array5S< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Slice != Array
template< typename T >
inline
Array5D< bool >
operator !=( Array5S< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Slice < Array
template< typename T >
inline
Array5D< bool >
operator <( Array5S< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Slice <= Array
template< typename T >
inline
Array5D< bool >
operator <=( Array5S< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Slice > Array
template< typename T >
inline
Array5D< bool >
operator >( Array5S< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Slice >= Array
template< typename T >
inline
Array5D< bool >
operator >=( Array5S< T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Array == Slice
template< typename T >
inline
Array5D< bool >
operator ==( Array5< T > const & a, Array5S< T > const & b )
{
	return ( b == a );
}

// Array != Slice
template< typename T >
inline
Array5D< bool >
operator !=( Array5< T > const & a, Array5S< T > const & b )
{
	return ( b != a );
}

// Array < Slice
template< typename T >
inline
Array5D< bool >
operator <( Array5< T > const & a, Array5S< T > const & b )
{
	return ( b > a );
}

// Array <= Slice
template< typename T >
inline
Array5D< bool >
operator <=( Array5< T > const & a, Array5S< T > const & b )
{
	return ( b >= a );
}

// Array > Slice
template< typename T >
inline
Array5D< bool >
operator >( Array5< T > const & a, Array5S< T > const & b )
{
	return ( b < a );
}

// Array >= Slice
template< typename T >
inline
Array5D< bool >
operator >=( Array5< T > const & a, Array5S< T > const & b )
{
	return ( b <= a );
}

// Slice == Value
template< typename T >
inline
Array5D< bool >
operator ==( Array5S< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == t );
					}
				}
			}
		}
	}
	return r;
}

// Slice != Value
template< typename T >
inline
Array5D< bool >
operator !=( Array5S< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != t );
					}
				}
			}
		}
	}
	return r;
}

// Slice < Value
template< typename T >
inline
Array5D< bool >
operator <( Array5S< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < t );
					}
				}
			}
		}
	}
	return r;
}

// Slice <= Value
template< typename T >
inline
Array5D< bool >
operator <=( Array5S< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= t );
					}
				}
			}
		}
	}
	return r;
}

// Slice > Value
template< typename T >
inline
Array5D< bool >
operator >( Array5S< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > t );
					}
				}
			}
		}
	}
	return r;
}

// Slice >= Value
template< typename T >
inline
Array5D< bool >
operator >=( Array5S< T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= t );
					}
				}
			}
		}
	}
	return r;
}

// Value == Slice
template< typename T >
inline
Array5D< bool >
operator ==( T const & t, Array5S< T > const & b )
{
	return ( b == t );
}

// Value != Slice
template< typename T >
inline
Array5D< bool >
operator !=( T const & t, Array5S< T > const & b )
{
	return ( b != t );
}

// Value < Slice
template< typename T >
inline
Array5D< bool >
operator <( T const & t, Array5S< T > const & b )
{
	return ( b > t );
}

// Value <= Slice
template< typename T >
inline
Array5D< bool >
operator <=( T const & t, Array5S< T > const & b )
{
	return ( b >= t );
}

// Value > Slice
template< typename T >
inline
Array5D< bool >
operator >( T const & t, Array5S< T > const & b )
{
	return ( b < t );
}

// Value >= Slice
template< typename T >
inline
Array5D< bool >
operator >=( T const & t, Array5S< T > const & b )
{
	return ( b <= t );
}

// Comparison: Elemental: MArray

// MArray == MArray
template< class A, typename T >
inline
Array5D< bool >
operator ==( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray != MArray
template< class A, typename T >
inline
Array5D< bool >
operator !=( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray < MArray
template< class A, typename T >
inline
Array5D< bool >
operator <( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray <= MArray
template< class A, typename T >
inline
Array5D< bool >
operator <=( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray > MArray
template< class A, typename T >
inline
Array5D< bool >
operator >( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray >= MArray
template< class A, typename T >
inline
Array5D< bool >
operator >=( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray == Array
template< class A, typename T >
inline
Array5D< bool >
operator ==( MArray5< A, T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// MArray != Array
template< class A, typename T >
inline
Array5D< bool >
operator !=( MArray5< A, T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// MArray < Array
template< class A, typename T >
inline
Array5D< bool >
operator <( MArray5< A, T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// MArray <= Array
template< class A, typename T >
inline
Array5D< bool >
operator <=( MArray5< A, T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// MArray > Array
template< class A, typename T >
inline
Array5D< bool >
operator >( MArray5< A, T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// MArray >= Array
template< class A, typename T >
inline
Array5D< bool >
operator >=( MArray5< A, T > const & a, Array5< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= b[ l ] );
					}
				}
			}
		}
	}
	return r;
}

// Array == MArray
template< class A, typename T >
inline
Array5D< bool >
operator ==( Array5< T > const & a, MArray5< A, T > const & b )
{
	return ( b == a );
}

// Array != MArray
template< class A, typename T >
inline
Array5D< bool >
operator !=( Array5< T > const & a, MArray5< A, T > const & b )
{
	return ( b != a );
}

// Array < MArray
template< class A, typename T >
inline
Array5D< bool >
operator <( Array5< T > const & a, MArray5< A, T > const & b )
{
	return ( b > a );
}

// Array <= MArray
template< class A, typename T >
inline
Array5D< bool >
operator <=( Array5< T > const & a, MArray5< A, T > const & b )
{
	return ( b >= a );
}

// Array > MArray
template< class A, typename T >
inline
Array5D< bool >
operator >( Array5< T > const & a, MArray5< A, T > const & b )
{
	return ( b < a );
}

// Array >= MArray
template< class A, typename T >
inline
Array5D< bool >
operator >=( Array5< T > const & a, MArray5< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Slice
template< class A, typename T >
inline
Array5D< bool >
operator ==( MArray5< A, T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray != Slice
template< class A, typename T >
inline
Array5D< bool >
operator !=( MArray5< A, T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray < Slice
template< class A, typename T >
inline
Array5D< bool >
operator <( MArray5< A, T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray <= Slice
template< class A, typename T >
inline
Array5D< bool >
operator <=( MArray5< A, T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray > Slice
template< class A, typename T >
inline
Array5D< bool >
operator >( MArray5< A, T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// MArray >= Slice
template< class A, typename T >
inline
Array5D< bool >
operator >=( MArray5< A, T > const & a, Array5S< T > const & b )
{
	assert( conformable( a, b ) );
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= b( i1, i2, i3, i4, i5 ) );
					}
				}
			}
		}
	}
	return r;
}

// Slice == MArray
template< class A, typename T >
inline
Array5D< bool >
operator ==( Array5S< T > const & a, MArray5< A, T > const & b )
{
	return ( b == a );
}

// Slice != MArray
template< class A, typename T >
inline
Array5D< bool >
operator !=( Array5S< T > const & a, MArray5< A, T > const & b )
{
	return ( b != a );
}

// Slice < MArray
template< class A, typename T >
inline
Array5D< bool >
operator <( Array5S< T > const & a, MArray5< A, T > const & b )
{
	return ( b > a );
}

// Slice <= MArray
template< class A, typename T >
inline
Array5D< bool >
operator <=( Array5S< T > const & a, MArray5< A, T > const & b )
{
	return ( b >= a );
}

// Slice > MArray
template< class A, typename T >
inline
Array5D< bool >
operator >( Array5S< T > const & a, MArray5< A, T > const & b )
{
	return ( b < a );
}

// Slice >= MArray
template< class A, typename T >
inline
Array5D< bool >
operator >=( Array5S< T > const & a, MArray5< A, T > const & b )
{
	return ( b <= a );
}

// MArray == Value
template< class A, typename T >
inline
Array5D< bool >
operator ==( MArray5< A, T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) == t );
					}
				}
			}
		}
	}
	return r;
}

// MArray != Value
template< class A, typename T >
inline
Array5D< bool >
operator !=( MArray5< A, T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) != t );
					}
				}
			}
		}
	}
	return r;
}

// MArray < Value
template< class A, typename T >
inline
Array5D< bool >
operator <( MArray5< A, T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) < t );
					}
				}
			}
		}
	}
	return r;
}

// MArray <= Value
template< class A, typename T >
inline
Array5D< bool >
operator <=( MArray5< A, T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) <= t );
					}
				}
			}
		}
	}
	return r;
}

// MArray > Value
template< class A, typename T >
inline
Array5D< bool >
operator >( MArray5< A, T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) > t );
					}
				}
			}
		}
	}
	return r;
}

// MArray >= Value
template< class A, typename T >
inline
Array5D< bool >
operator >=( MArray5< A, T > const & a, T const & t )
{
	Array5D< bool > r( Array5D< bool >::shape( a ) );
	Array5D< bool >::size_type l( 0 );
	for ( int i1 = 1, e1 = r.u1(); i1 <= e1; ++i1 ) {
		for ( int i2 = 1, e2 = r.u2(); i2 <= e2; ++i2 ) {
			for ( int i3 = 1, e3 = r.u3(); i3 <= e3; ++i3 ) {
				for ( int i4 = 1, e4 = r.u4(); i4 <= e4; ++i4 ) {
					for ( int i5 = 1, e5 = r.u5(); i5 <= e5; ++i5, ++l ) {
						r[ l ] = ( a( i1, i2, i3, i4, i5 ) >= t );
					}
				}
			}
		}
	}
	return r;
}

// Value == MArray
template< class A, typename T >
inline
Array5D< bool >
operator ==( T const & t, MArray5< A, T > const & b )
{
	return ( b == t );
}

// Value != MArray
template< class A, typename T >
inline
Array5D< bool >
operator !=( T const & t, MArray5< A, T > const & b )
{
	return ( b != t );
}

// Value < MArray
template< class A, typename T >
inline
Array5D< bool >
operator <( T const & t, MArray5< A, T > const & b )
{
	return ( b > t );
}

// Value <= MArray
template< class A, typename T >
inline
Array5D< bool >
operator <=( T const & t, MArray5< A, T > const & b )
{
	return ( b >= t );
}

// Value > MArray
template< class A, typename T >
inline
Array5D< bool >
operator >( T const & t, MArray5< A, T > const & b )
{
	return ( b < t );
}

// Value >= MArray
template< class A, typename T >
inline
Array5D< bool >
operator >=( T const & t, MArray5< A, T > const & b )
{
	return ( b <= t );
}

// Generator

// -Array
template< typename T >
inline
Array5D< T >
operator -( Array5< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Array + Array
template< typename T >
inline
Array5D< T >
operator +( Array5< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Array
template< typename T >
inline
Array5D< T >
operator -( Array5< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Array
template< typename T >
inline
Array5D< T >
operator *( Array5< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Array
template< typename T >
inline
Array5D< T >
operator /( Array5< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Value
template< typename T >
inline
Array5D< T >
operator +( Array5< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Array
template< typename T >
inline
Array5D< T >
operator +( T const & t, Array5< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += t;
	return r;
}

// Array - Value
template< typename T >
inline
Array5D< T >
operator -( Array5< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Array
template< typename T >
inline
Array5D< T >
operator -( T const & t, Array5< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Array * Value
template< typename T >
inline
Array5D< T >
operator *( Array5< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Array
template< typename T >
inline
Array5D< T >
operator *( T const & t, Array5< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Array / Value
template< typename T >
inline
Array5D< T >
operator /( Array5< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Array
template< typename T >
inline
Array5D< T >
operator /( T const & t, Array5< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Array && Array
template< typename T >
inline
Array5D< T >
operator &&( Array5< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Array || Array
template< typename T >
inline
Array5D< T >
operator ||( Array5< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: Slice

// -Slice
template< typename T >
inline
Array5D< T >
operator -( Array5S< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// Slice + Slice
template< typename T >
inline
Array5D< T >
operator +( Array5S< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Slice
template< typename T >
inline
Array5D< T >
operator -( Array5S< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Slice
template< typename T >
inline
Array5D< T >
operator *( Array5S< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Slice
template< typename T >
inline
Array5D< T >
operator /( Array5S< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Array
template< typename T >
inline
Array5D< T >
operator +( Array5S< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// Slice - Array
template< typename T >
inline
Array5D< T >
operator -( Array5S< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Slice * Array
template< typename T >
inline
Array5D< T >
operator *( Array5S< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Slice / Array
template< typename T >
inline
Array5D< T >
operator /( Array5S< T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + Slice
template< typename T >
inline
Array5D< T >
operator +( Array5< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - Slice
template< typename T >
inline
Array5D< T >
operator -( Array5< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * Slice
template< typename T >
inline
Array5D< T >
operator *( Array5< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / Slice
template< typename T >
inline
Array5D< T >
operator /( Array5< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Slice + Value
template< typename T >
inline
Array5D< T >
operator +( Array5S< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + Slice
template< typename T >
inline
Array5D< T >
operator +( T const & t, Array5S< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += t;
	return r;
}

// Slice - Value
template< typename T >
inline
Array5D< T >
operator -( Array5S< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - Slice
template< typename T >
inline
Array5D< T >
operator -( T const & t, Array5S< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// Slice * Value
template< typename T >
inline
Array5D< T >
operator *( Array5S< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * Slice
template< typename T >
inline
Array5D< T >
operator *( T const & t, Array5S< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Slice / Value
template< typename T >
inline
Array5D< T >
operator /( Array5S< T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / Slice
template< typename T >
inline
Array5D< T >
operator /( T const & t, Array5S< T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// Slice && Slice
template< typename T >
inline
Array5D< T >
operator &&( Array5S< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// Slice || Slice
template< typename T >
inline
Array5D< T >
operator ||( Array5S< T > const & a, Array5S< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

// Generator: MArray

// -MArray
template< class A, typename T >
inline
Array5D< T >
operator -( MArray5< A, T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= T( -1 );
	return r;
}

// MArray + MArray
template< class A, typename T >
inline
Array5D< T >
operator +( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - MArray
template< class A, typename T >
inline
Array5D< T >
operator -( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * MArray
template< class A, typename T >
inline
Array5D< T >
operator *( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / MArray
template< class A, typename T >
inline
Array5D< T >
operator /( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Array
template< class A, typename T >
inline
Array5D< T >
operator +( MArray5< A, T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// MArray - Array
template< class A, typename T >
inline
Array5D< T >
operator -( MArray5< A, T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// MArray * Array
template< class A, typename T >
inline
Array5D< T >
operator *( MArray5< A, T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// MArray / Array
template< class A, typename T >
inline
Array5D< T >
operator /( MArray5< A, T > const & a, Array5< T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// Array + MArray
template< class A, typename T >
inline
Array5D< T >
operator +( Array5< T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += b;
	return r;
}

// Array - MArray
template< class A, typename T >
inline
Array5D< T >
operator -( Array5< T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= b;
	return r;
}

// Array * MArray
template< class A, typename T >
inline
Array5D< T >
operator *( Array5< T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= b;
	return r;
}

// Array / MArray
template< class A, typename T >
inline
Array5D< T >
operator /( Array5< T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= b;
	return r;
}

// MArray + Value
template< class A, typename T >
inline
Array5D< T >
operator +( MArray5< A, T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += t;
	return r;
}

// Value + MArray
template< class A, typename T >
inline
Array5D< T >
operator +( T const & t, MArray5< A, T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r += t;
	return r;
}

// MArray - Value
template< class A, typename T >
inline
Array5D< T >
operator -( MArray5< A, T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r -= t;
	return r;
}

// Value - MArray
template< class A, typename T >
inline
Array5D< T >
operator -( T const & t, MArray5< A, T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= T( -1 );
	r += t;
	return r;
}

// MArray * Value
template< class A, typename T >
inline
Array5D< T >
operator *( MArray5< A, T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= t;
	return r;
}

// Value * MArray
template< class A, typename T >
inline
Array5D< T >
operator *( T const & t, MArray5< A, T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r *= t;
	return r;
}

// MArray / Value
template< class A, typename T >
inline
Array5D< T >
operator /( MArray5< A, T > const & a, T const & t )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r /= t;
	return r;
}

// Value / MArray
template< class A, typename T >
inline
Array5D< T >
operator /( T const & t, MArray5< A, T > const & a )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.invert();
	r *= t;
	return r;
}

// MArray && MArray
template< class A, typename T >
inline
Array5D< T >
operator &&( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.and_equals( b );
	return r;
}

// MArray || MArray
template< class A, typename T >
inline
Array5D< T >
operator ||( MArray5< A, T > const & a, MArray5< A, T > const & b )
{
	Array5D< T > r( Array5D< T >::one_based( a ) );
	r.or_equals( b );
	return r;
}

} // ObjexxFCL

#endif // ObjexxFCL_Array5D_hh_INCLUDED
