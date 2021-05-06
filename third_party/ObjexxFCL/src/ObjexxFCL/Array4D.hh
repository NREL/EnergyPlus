#ifndef ObjexxFCL_Array4D_hh_INCLUDED
#define ObjexxFCL_Array4D_hh_INCLUDED

// Array4D: Row-Major 4D Array
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array4D.fwd.hh>
#include <ObjexxFCL/Array4.hh>

// C++ Headers
#include <functional>

namespace ObjexxFCL {

// Array4D: Row-Major 4D Array
template< typename T >
class Array4D : public Array4< T >
{

private: // Types

	typedef  Array4< T >  Super;
	typedef  internal::InitializerSentinel  InitializerSentinel;

private: // Friend

	template< typename > friend class Array4D;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Traits  Traits;
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

	typedef  std::function< void( Array4D< T > & ) >  InitializerFunction;

	using Super::conformable;
	using Super::contains;
	using Super::index;
	using Super::isize1;
	using Super::isize2;
	using Super::isize3;
	using Super::isize4;
	using Super::l1;
	using Super::l2;
	using Super::l3;
	using Super::l4;
	using Super::operator ();
	using Super::operator [];
	using Super::size1;
	using Super::size2;
	using Super::size3;
	using Super::size4;
	using Super::u1;
	using Super::u2;
	using Super::u3;
	using Super::u4;

protected: // Types

	using Super::assign;
	using Super::clear_move;
	using Super::initialize;
	using Super::move_if;
	using Super::resize;
	using Super::shift_set;
	using Super::shift_only_set;
	using Super::size_of;
	using Super::swap4;

	using Super::data_;
	using Super::I1_;
	using Super::I2_;
	using Super::I3_;
	using Super::I4_;
	using Super::sdata_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;
	using Super::z2_;
	using Super::z3_;
	using Super::z4_;

public: // Creation

	// Default Constructor
	Array4D()
	{}

	// Copy Constructor
	Array4D( Array4D const & a ) :
	 Super( a )
	{}

	// Move Constructor
	Array4D( Array4D && a ) noexcept :
	 Super( std::move( a ) )
	{
	}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array4D( Array4D< U > const & a ) :
	 Super( a )
	{}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array4D( Array4< U > const & a ) :
	 Super( a )
	{}

	// Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array4D( Array4S< U > const & a ) :
	 Super( a )
	{
		setup_real();
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
						initialize( l, a( i1, i2, i3, i4 ) );
					}
				}
			}
		}
	}


	// IndexRange Constructor
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( I1, I2, I3, I4 )
	{
		setup_real();
	}

	// IndexRange + Initializer Value Constructor
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, T const & t ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		setup_real();
		initialize( t );
	}


	// IndexRange + Initializer Function Constructor
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, InitializerFunction const & fxn ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		setup_real();
		initialize( fxn );
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, std::initializer_list< U > const l ) :
	 Super( I1, I2, I3, I4, l )
	{
		setup_real();
	}

	// IndexRange + Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, Array4< U > const & a ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		assert( conformable( a ) );
		setup_real();
		initialize( a );
	}

	// IndexRange + Slice Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, Array4S< U > const & a ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		assert( conformable( a ) );
		setup_real();
		size_type l( 0u );
		for ( int i1 = 1, e1 = a.u1(); i1 <= e1; ++i1 ) {
			for ( int i2 = 1, e2 = a.u2(); i2 <= e2; ++i2 ) {
				for ( int i3 = 1, e3 = a.u3(); i3 <= e3; ++i3 ) {
					for ( int i4 = 1, e4 = a.u4(); i4 <= e4; ++i4, ++l ) {
						initialize( l, a( i1, i2, i3, i4 ) );
					}
				}
			}
		}
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array4D( Array4< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		assert( conformable( a ) );
		setup_real();
		initialize( a );
	}

	// IndexRange + Base Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, Array< U > const & a ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		assert( size_ == a.size() );
		setup_real();
		initialize( a );
	}

	// Base + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array4D( Array< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4 ) :
	 Super( I1, I2, I3, I4, InitializerSentinel{} )
	{
		assert( size_ == a.size() );
		setup_real();
		initialize( a );
	}

	// Array Range Named Constructor Template
	template< typename U >
	static
	Array4D
	range( Array4< U > const & a )
	{
		return Array4D( a.I1_, a.I2_, a.I3_, a.I4_ );
	}

	// Array Range + Initializer Value Named Constructor Template
	template< typename U >
	static
	Array4D
	range( Array4< U > const & a, T const & t )
	{
		return Array4D( a.I1_, a.I2_, a.I3_, a.I4_, t );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	static
	Array4D
	one_based( Array4< U > const & a )
	{
		return Array4D( a, a.isize1(), a.isize2(), a.isize3(), a.isize4() );
	}

	// One-Based Slice Named Constructor Template
	template< typename U >
	static
	Array4D
	one_based( Array4S< U > const & a )
	{
		return Array4D( a.isize1(), a.isize2(), a.isize3(), a.isize4(), a );
	}

	// Destructor
	virtual
	~Array4D() = default;

private: // Creation

	// IndexRange Raw Constructor
	explicit
	Array4D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, InitializerSentinel initialized ) :
	 Super( I1, I2, I3, I4, initialized )
	{
		setup_real();
	}

public: // Assignment: Array

	// Copy Assignment
	Array4D &
	operator =( Array4D const & a )
	{
		if ( this != &a ) {
			if ( ( conformable( a ) ) || ( ! size_real( a.I1_, a.I2_, a.I3_, a.I4_ ) ) ) {
				Base::operator =( a );
			} else {
				Base::initialize( a );
			}
		}
		return *this;
	}

	// Move Assignment
	Array4D &
	operator =( Array4D && a ) noexcept
	{
		if ( conformable( a ) ) {
			Base::conformable_move( a );
		} else {
			Base::operator =( std::move( a ) );
			I1_ = a.I1_;
			I2_ = a.I2_;
			I3_ = a.I3_;
			I4_ = a.I4_;
			z1_ = a.z1_;
			z2_ = a.z2_;
			z3_ = a.z3_;
			z4_ = a.z4_;
		}
		a.clear_move();
		return *this;
	}

	// Super Assignment
	Array4D &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ( conformable( a ) ) || ( ! size_real( a.I1_, a.I2_, a.I3_, a.I4_ ) ) ) {
				Base::operator =( a );
			} else {
				Base::initialize( a );
			}
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array4D &
	operator =( Array4< U > const & a )
	{
		if ( ( conformable( a ) ) || ( ! size_real( a.I1_, a.I2_, a.I3_, a.I4_ ) ) ) {
			Base::operator =( a );
		} else {
			Base::initialize( a );
		}
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array4D &
	operator =( Array4S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array4D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}


public: // Assignment: Value

	// = Value
	Array4D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}


public: // Modifier

	// Clear
	Array4D &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange
	Array4D &
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		dimension_real( I1, I2, I3, I4 );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	Array4D &
	allocate( Array4< U > const & a )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_ );
		return *this;
	}

	// Deallocate
	Array4D &
	deallocate()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange
	Array4D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		dimension_real( I1, I2, I3, I4 );
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	Array4D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, T const & t )
	{
		dimension_real( I1, I2, I3, I4, t );
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	Array4D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, InitializerFunction const & fxn )
	{
		dimension_real( I1, I2, I3, I4, fxn );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	Array4D &
	dimension( Array4< U > const & a )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_ );
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	Array4D &
	dimension( Array4< U > const & a, T const & t )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, t );
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	Array4D &
	dimension( Array4< U > const & a, InitializerFunction const & fxn )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, fxn );
		return *this;
	}

	// Swap
	Array4D &
	swap( Array4D & v )
	{
		using std::swap;
		swap4( v );
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	bool
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		return size_real( I1, I2, I3, I4 );
	}

	// Initialize to Default State
	void
	initialize()
	{
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
		std::uninitialized_fill_n( data_, size_, Traits::initial_array_value() );
#else
		for ( size_type i = 0; i < size_; ++i ) {
				new ( data_ + i ) T;
			}
#endif
	}

	// Initialize by Function
	void
	initialize( InitializerFunction const & fxn )
	{
		initialize();
		fxn( *this );
	}

	// Assignment to Default State
	void
	assign()
	{
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
		std::fill_n( data_, size_, Traits::initial_array_value() );
#endif
	}

private: // Functions

	// Set Up for IndexRange Constructor
	void
	setup_real()
	{
		shift_set( ( ( ( ( ( I1_.l() * z2_ ) + I2_.l() ) * z3_ ) + I3_.l() ) * z4_ ) + I4_.l() );
	}

	// Size by IndexRange
	bool
	size_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		I1_.assign( I1 );
		I2_.assign( I2 );
		I3_.assign( I3 );
		I4_.assign( I4 );
		z1_ = I1_.size();
		z2_ = I2_.size();
		z3_ = I3_.size();
		z4_ = I4_.size();
		shift_only_set( ( ( ( ( ( I1_.l() * z2_ ) + I2_.l() ) * z3_ ) + I3_.l() ) * z4_ ) + I4_.l() );
		return resize( size_of( z1_, z2_, z3_, z4_ ) );
	}

	// Dimension by IndexRange
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4 )
	{
		if ( size_real( I1, I2, I3, I4 ) ) {
			initialize();
		} else {
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
			assign();
#endif
		}
	}

	// Dimension by IndexRange + Initializer Value
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, T const & t )
	{
		if ( size_real( I1, I2, I3, I4 ) ) {
			initialize( t );
		} else {
			assign( t );
		}
	}

	// Dimension by IndexRange + Initializer Function
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, InitializerFunction const & fxn )
	{
		if ( size_real( I1, I2, I3, I4 ) ) initialize();
		fxn( *this );
	}

}; // Array4D

// Swap
template< typename T >
inline
void
swap( Array4D< T > & a, Array4D< T > & b )
{
	a.swap( b );
}

// Comparison: Elemental


} // ObjexxFCL

#endif // ObjexxFCL_Array4D_hh_INCLUDED
