#ifndef ObjexxFCL_Array5A_hh_INCLUDED
#define ObjexxFCL_Array5A_hh_INCLUDED

// Array5A: Row-Major 5D Argument Array
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.1.0
//
// Language: C++
//
// Copyright (c) 2000-2016 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array5A.fwd.hh>
#include <ObjexxFCL/Array5.hh>

namespace ObjexxFCL {

// Array5A: Row-Major 5D Argument Array
template< typename T >
class Array5A : public Array5< T >
{

private: // Types

	typedef  Array5< T >  Super;
	typedef  internal::ProxySentinel  ProxySentinel;

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

	using Super::conformable;
	using Super::npos;
	using Super::operator ();
	using Super::shift_set;
	using Super::size_of;
	using Super::size_set;
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
	Array5A() :
	 Super( ProxySentinel() )
	{}

	// Copy Constructor
	Array5A( Array5A const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift_ );
	}

	// Super Constructor
	Array5A( Super const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift_ );
	}

	// Slice Constructor
	Array5A( Array5S< T > const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift() );
	}

	// Base Constructor
	Array5A( Base const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( 5 );
	}

	// Tail Constructor
	Array5A( Tail const & s ) :
	 Super( s, ProxySentinel() )
	{
		shift_set( 5 );
	}

	// Value Constructor
	Array5A( T const & t ) :
	 Super( t, ProxySentinel() )
	{
		shift_set( 5 );
	}

	// Copy + IndexRange Constructor
	Array5A( Array5A const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( a, I1, I2, I3, I4, I5, ProxySentinel() )
	{
		dimension_argument();
	}

	// Super + IndexRange Constructor
	Array5A( Super const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( a, I1, I2, I3, I4, I5, ProxySentinel() )
	{
		dimension_argument();
	}

	// Slice + IndexRange Constructor
	Array5A( Array5S< T > const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( a, I1, I2, I3, I4, I5, ProxySentinel() )
	{
		dimension_argument();
	}

	// Base + IndexRange Constructor
	Array5A( Base const & a, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( a, I1, I2, I3, I4, I5, ProxySentinel() )
	{
		dimension_argument();
	}

	// Tail + IndexRange Constructor
	Array5A( Tail const & s, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( s, I1, I2, I3, I4, I5, ProxySentinel() )
	{
		dimension_argument();
	}

	// Value + IndexRange Constructor
	Array5A( T const & t, IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) :
	 Super( t, I1, I2, I3, I4, I5, ProxySentinel() )
	{
		dimension_argument();
	}

	// Destructor
	virtual
	~Array5A()
	{}

public: // Assignment: Array

	// Copy Assignment
	Array5A &
	operator =( Array5A const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	Array5A &
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
	Array5A &
	operator =( Array5< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator =( Array5S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	Array5A &
	operator =( MArray5< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator +=( Array5< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator -=( Array5< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator *=( Array5< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator /=( Array5< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator +=( Array5S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator -=( Array5S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator *=( Array5S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	operator /=( Array5S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	Array5A &
	operator +=( MArray5< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	Array5A &
	operator -=( MArray5< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	Array5A &
	operator *=( MArray5< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	Array5A &
	operator /=( MArray5< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	and_equals( Array5< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	or_equals( Array5< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	and_equals( Array5S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array5A &
	or_equals( Array5S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	Array5A &
	and_equals( MArray5< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	Array5A &
	or_equals( MArray5< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	Array5A &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	Array5A &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	Array5A &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	Array5A &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	Array5A &
	operator /=( T const & t )
	{
		Base::operator /=( t );
		return *this;
	}

public: // Predicate

	// Initializer Active?
	bool
	initializer_active() const
	{
		return false;
	}

public: // Modifier

	// Clear
	Array5A &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange Even if Const
	Array5A const &
	dim( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 ) const
	{
		const_cast< Array5A & >( *this ).dimension( I1, I2, I3, I4, I5 );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	Array5A const &
	dim( Array5< U > const & a ) const
	{
		const_cast< Array5A & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	Array5A &
	dimension( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
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
		dimension_argument();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	Array5A &
	dimension( Array5< U > const & a )
	{
		I1_.assign( a.I1() );
		I2_.assign( a.I2() );
		I3_.assign( a.I3() );
		I4_.assign( a.I4() );
		I5_.assign( a.I5() );
		z1_ = I1_.size();
		z2_ = I2_.size();
		z3_ = I3_.size();
		z4_ = I4_.size();
		z5_ = I5_.size();
		dimension_argument();
		return *this;
	}

	// Attach to Super Array
	Array5A &
	attach( Super const & a )
	{
		Base::attach( a );
		I1_.assign( a.I1_ );
		I2_.assign( a.I2_ );
		I3_.assign( a.I3_ );
		I4_.assign( a.I4_ );
		I5_.assign( a.I5_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		z4_ = a.z4_;
		z5_ = a.z5_;
		return *this;
	}

	// Attach to Non-Const Super Array
	Array5A &
	attach( Super & a )
	{
		Base::attach( a );
		I1_.assign( a.I1_ );
		I2_.assign( a.I2_ );
		I3_.assign( a.I3_ );
		I4_.assign( a.I4_ );
		I5_.assign( a.I5_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		z3_ = a.z3_;
		z4_ = a.z4_;
		z5_ = a.z5_;
		return *this;
	}

	// Attach to Base Array
	Array5A &
	attach( Base const & a )
	{
		Base::attach< 5 >( a );
		I1_ = a.isize();
		I2_ = 1;
		I3_ = 1;
		I4_ = 1;
		I5_ = 1;
		z1_ = I1_.size();
		z2_ = z3_ = z4_ = z5_ = 1u;
		return *this;
	}

	// Attach to Non-Const Base Array
	Array5A &
	attach( Base & a )
	{
		Base::attach< 5 >( a );
		I1_ = a.isize();
		I2_ = 1;
		I3_ = 1;
		I4_ = 1;
		I5_ = 1;
		z1_ = I1_.size();
		z2_ = z3_ = z4_ = z5_ = 1u;
		return *this;
	}

	// Attach to Tail
	Array5A &
	attach( Tail const & s )
	{
		Base::attach< 5 >( s );
		I1_ = s.isize();
		I2_ = 1;
		I3_ = 1;
		I4_ = 1;
		I5_ = 1;
		z1_ = I1_.size();
		z2_ = z3_ = z4_ = z5_ = 1u;
		return *this;
	}

	// Attach to Non-Const Tail
	Array5A &
	attach( Tail & s )
	{
		Base::attach< 5 >( s );
		I1_ = s.isize();
		I2_ = 1;
		I3_ = 1;
		I4_ = 1;
		I5_ = 1;
		z1_ = I1_.size();
		z2_ = z3_ = z4_ = z5_ = 1u;
		return *this;
	}

	// Attach to Value
	Array5A &
	attach( T const & t )
	{
		Base::attach< 5 >( t );
		I1_ = _;
		I2_ = 1;
		I3_ = 1;
		I4_ = 1;
		I5_ = 1;
		z1_ = I1_.size();
		z2_ = z3_ = z4_ = z5_ = 1u;
		return *this;
	}

	// Attach to Non-Const Value
	Array5A &
	attach( T & t )
	{
		Base::attach< 5 >( t );
		I1_ = _;
		I2_ = 1;
		I3_ = 1;
		I4_ = 1;
		I5_ = 1;
		z1_ = I1_.size();
		z2_ = z3_ = z4_ = z5_ = 1u;
		return *this;
	}

	// Detach from Source Array
	Array5A &
	detach()
	{
		Base::detach();
		I1_.clear();
		I2_.clear();
		I3_.clear();
		I4_.clear();
		I5_.clear();
		z1_ = z2_ = z3_ = z4_ = z5_ = 0u;
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	bool
	dimension_assign( IR const & I1, IR const & I2, IR const & I3, IR const & I4, IR const & I5 )
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
		dimension_argument();
		return false;
	}

private: // Functions

	// Dimension by Current IndexRanges
	void
	dimension_argument()
	{
		assert( I2_.bounded() );
		assert( I3_.bounded() );
		assert( I4_.bounded() );
		assert( I5_.bounded() );
		if ( I1_.bounded() ) {
			size_set( size_of( z1_, z2_, z3_, z4_, z5_ ) );
		} else if ( size_ == npos ) {
			size_set( npos );
		} else {
			size_type const slice_size( size_of( z2_, z3_, z4_, z5_ ) );
			if ( slice_size > 0u ) { // Infer size
				z1_ = size_ / slice_size;
				I1_.u( I1_.l() + static_cast< int >( z1_ ) - 1 );
				size_set( size_of( z1_, slice_size ) );
			} else {
				size_set( size_ );
			}
		}
		shift_set( ( ( ( ( ( ( ( I1_.l() * z2_ ) + I2_.l() ) * z3_ ) + I3_.l() ) * z4_ ) + I4_.l() ) * z5_ ) + I5_.l() );
	}

}; // Array5A

} // ObjexxFCL

#endif // ObjexxFCL_Array5A_hh_INCLUDED
