#ifndef ObjexxFCL_Array5D_hh_INCLUDED
#define ObjexxFCL_Array5D_hh_INCLUDED

// Array5D: Row-Major 5D Array
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
#include <ObjexxFCL/Array5D.fwd.hh>
#include <ObjexxFCL/Array5.hh>

// C++ Headers
#include <functional>

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

	typedef  std::function< void( Array5D< T > & ) >  InitializerFunction;

	using Super::conformable;
	using Super::contains;
	using Super::index;
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
	using Super::operator ();
	using Super::operator [];
	using Super::size1;
	using Super::size2;
	using Super::size3;
	using Super::size4;
	using Super::size5;
	using Super::u1;
	using Super::u2;
	using Super::u3;
	using Super::u4;
	using Super::u5;

protected: // Types

	using Super::assign;
	using Super::clear_move;
	using Super::initialize;
	using Super::resize;
	using Super::shift_set;
	using Super::shift_only_set;
	using Super::size_of;
	using Super::swap5;

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
	Array5D()
	{}

	// Copy Constructor
	Array5D( Array5D const & a ) :
	 Super( a )
	{}

	// Move Constructor
	Array5D( Array5D && a ) noexcept :
	 Super( std::move( a ) )
	{
	}

	// Super Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	explicit
	Array5D( Array5< U > const & a ) :
	 Super( a )
	{}

	// IndexRange Constructor
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( I1, I2, I3, I4, I5 )
	{
		setup_real();
	}

	// IndexRange + Initializer Value Constructor
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel{} )
	{
		setup_real();
		initialize( t );
	}

	// IndexRange + Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, std::initializer_list< U > const l ) :
	 Super( I1, I2, I3, I4, I5, l )
	{
		setup_real();
	}

	// Super + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array5D( Array5< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel{} )
	{
		assert( conformable( a ) );
		setup_real();
		initialize( a );
	}

	// IndexRange + Base Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array5D( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, Array< U > const & a ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel{} )
	{
		assert( size_ == a.size() );
		setup_real();
		initialize( a );
	}

	// Base + IndexRange Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Array5D( Array< U > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( I1, I2, I3, I4, I5, InitializerSentinel{} )
	{
		assert( size_ == a.size() );
		setup_real();
		initialize( a );
	}

	// One-Based Copy Named Constructor Template
	template< typename U >
	static
	Array5D
	one_based( Array5< U > const & a )
	{
		return Array5D( a, a.isize1(), a.isize2(), a.isize3(), a.isize4(), a.isize5() );
	}

	// Destructor
	virtual
	~Array5D() = default;


public: // Assignment: Array

	// Copy Assignment
	Array5D &
	operator =( Array5D const & a )
	{
		if ( this != &a ) {
			if ( ( conformable( a ) ) || ( ! size_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ ) ) ) {
				Base::operator =( a );
			} else {
				Base::initialize( a );
			}
		}
		return *this;
	}

	// Move Assignment
	Array5D &
	operator =( Array5D && a ) noexcept
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
	Array5D &
	operator =( Super const & a )
	{
		if ( this != &a ) {
			if ( ( conformable( a ) ) || ( ! size_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ ) ) ) {
				Base::operator =( a );
			} else {
				Base::initialize( a );
			}
		}
		return *this;
	}

	// Super Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5D &
	operator =( Array5< U > const & a )
	{
		if ( ( conformable( a ) ) || ( ! size_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ ) ) ) {
			Base::operator =( a );
		} else {
			Base::initialize( a );
		}
		Base::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5D &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}


public: // Assignment: Value

	// = Value
	Array5D &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

public: // Modifier

	// Clear
	Array5D &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange
	Array5D &
	allocate( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		dimension_real( I1, I2, I3, I4, I5 );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	Array5D &
	allocate( Array5< U > const & a )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
		return *this;
	}

	// Deallocate
	Array5D &
	deallocate()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange
	Array5D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		dimension_real( I1, I2, I3, I4, I5 );
		return *this;
	}

	// Dimension by IndexRange + Initializer Value
	Array5D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t )
	{
		dimension_real( I1, I2, I3, I4, I5, t );
		return *this;
	}

	// Dimension by IndexRange + Initializer Function
	Array5D &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, InitializerFunction const & fxn )
	{
		dimension_real( I1, I2, I3, I4, I5, fxn );
		return *this;
	}

	// Dimension by Array Template
	template< typename U >
	Array5D &
	dimension( Array5< U > const & a )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_ );
		return *this;
	}

	// Dimension by Array + Initializer Value Template
	template< typename U >
	Array5D &
	dimension( Array5< U > const & a, T const & t )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, t );
		return *this;
	}

	// Dimension by Array + Initializer Function Template
	template< typename U >
	Array5D &
	dimension( Array5< U > const & a, InitializerFunction const & fxn )
	{
		dimension_real( a.I1_, a.I2_, a.I3_, a.I4_, a.I5_, fxn );
		return *this;
	}

	// Swap
	Array5D &
	swap( Array5D & v )
	{
		using std::swap;
		swap5( v );
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	bool
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		return size_real( I1, I2, I3, I4, I5 );
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
		shift_set( ( ( ( ( ( ( ( I1_.l() * z2_ ) + I2_.l() ) * z3_ ) + I3_.l() ) * z4_ ) + I4_.l() ) * z5_ ) + I5_.l() );
	}

	// Size by IndexRange
	bool
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
		shift_only_set( ( ( ( ( ( ( ( I1_.l() * z2_ ) + I2_.l() ) * z3_ ) + I3_.l() ) * z4_ ) + I4_.l() ) * z5_ ) + I5_.l() );
		return resize( size_of( z1_, z2_, z3_, z4_, z5_ ) );
	}

	// Dimension by IndexRange
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
	{
		if ( size_real( I1, I2, I3, I4, I5 ) ) {
			initialize();
		} else {
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
			assign();
#endif
		}
	}

	// Dimension by IndexRange + Initializer Value
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, T const & t )
	{
		if ( size_real( I1, I2, I3, I4, I5 ) ) {
			initialize( t );
		} else {
			assign( t );
		}
	}

	// Dimension by IndexRange + Initializer Function
	void
	dimension_real( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5, InitializerFunction const & fxn )
	{
		if ( size_real( I1, I2, I3, I4, I5 ) ) initialize();
		fxn( *this );
	}

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


} // ObjexxFCL

#endif // ObjexxFCL_Array5D_hh_INCLUDED
