#ifndef ObjexxFCL_Array2A_hh_INCLUDED
#define ObjexxFCL_Array2A_hh_INCLUDED

// Row-Major 2D Argument Array
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array2A.fwd.hh>
#include <ObjexxFCL/Array2.hh>

namespace ObjexxFCL {

// Row-Major 2D Argument Array
template< typename T >
class Array2A : public Array2< T >
{

private: // Types

	using Super = Array2< T >;
	using ProxySentinel = internal::ProxySentinel;

public: // Types

	using Base = typename Super::Base;
	using Tail = typename Super::Tail;
	using IR = typename Super::IR;

	// STL Style
	using value_type = typename Super::value_type;
	using reference = typename Super::reference;
	using const_reference = typename Super::const_reference;
	using pointer = typename Super::pointer;
	using const_pointer = typename Super::const_pointer;
	using iterator = typename Super::iterator;
	using const_iterator = typename Super::const_iterator;
	using reverse_iterator = typename Super::reverse_iterator;
	using const_reverse_iterator = typename Super::const_reverse_iterator;
	using size_type = typename Super::size_type;
	using difference_type = typename Super::difference_type;

	// C++ Style
	using Value = typename Super::Value;
	using Reference = typename Super::Reference;
	using ConstReference = typename Super::ConstReference;
	using Pointer = typename Super::Pointer;
	using ConstPointer = typename Super::ConstPointer;
	using Iterator = typename Super::Iterator;
	using ConstIterator = typename Super::ConstIterator;
	using ReverseIterator = typename Super::ReverseIterator;
	using ConstReverseIterator = typename Super::ConstReverseIterator;
	using Size = typename Super::Size;
	using Difference = typename Super::Difference;

	using Super::conformable;
	using Super::npos;
	using Super::operator ();

protected: // Types

	using Super::shift_set;
	using Super::size_of;
	using Super::size_set;

	using Super::data_;
	using Super::I1_;
	using Super::I2_;
	using Super::shift_;
	using Super::size_;
	using Super::z1_;
	using Super::z2_;

public: // Creation

	// Default Constructor
	Array2A() :
	 Super( ProxySentinel() )
	{}

	// Copy Constructor
	Array2A( Array2A const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift_ );
	}

	// Super Constructor
	Array2A( Super const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift_ );
	}

	// Slice Constructor
	Array2A( Array2S< T > const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift() );
	}

	// Base Constructor
	Array2A( Base const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( 2 );
	}

	// Tail Constructor
	Array2A( Tail const & s ) :
	 Super( s, ProxySentinel() )
	{
		shift_set( 2 );
	}

	// Value Constructor
	Array2A( T const & t ) :
	 Super( t, ProxySentinel() )
	{
		shift_set( 2 );
	}

	// Copy + IndexRange Constructor
	Array2A( Array2A const & a, IR const & I1, IR const & I2 ) :
	 Super( a, I1, I2, ProxySentinel() )
	{
		dimension_argument();
	}

	// Super + IndexRange Constructor
	Array2A( Super const & a, IR const & I1, IR const & I2 ) :
	 Super( a, I1, I2, ProxySentinel() )
	{
		dimension_argument();
	}

	// Slice + IndexRange Constructor
	Array2A( Array2S< T > const & a, IR const & I1, IR const & I2 ) :
	 Super( a, I1, I2, ProxySentinel() )
	{
		dimension_argument();
	}

	// Base + IndexRange Constructor
	Array2A( Base const & a, IR const & I1, IR const & I2 ) :
	 Super( a, I1, I2, ProxySentinel() )
	{
		dimension_argument();
	}

	// Tail + IndexRange Constructor
	Array2A( Tail const & s, IR const & I1, IR const & I2 ) :
	 Super( s, I1, I2, ProxySentinel() )
	{
		dimension_argument();
	}

	// Value + IndexRange Constructor
	Array2A( T const & t, IR const & I1, IR const & I2 ) :
	 Super( t, I1, I2, ProxySentinel() )
	{
		dimension_argument();
	}

public: // Assignment: Array

	// Copy Assignment
	Array2A &
	operator =( Array2A const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	Array2A &
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
	Array2A &
	operator =( Array2< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator =( Array2S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	Array2A &
	operator =( MArray2< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator +=( Array2< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator -=( Array2< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator *=( Array2< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator /=( Array2< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator +=( Array2S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator -=( Array2S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator *=( Array2S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	operator /=( Array2S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	Array2A &
	operator +=( MArray2< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	Array2A &
	operator -=( MArray2< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	Array2A &
	operator *=( MArray2< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	Array2A &
	operator /=( MArray2< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	and_equals( Array2< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	or_equals( Array2< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	and_equals( Array2S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array2A &
	or_equals( Array2S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	Array2A &
	and_equals( MArray2< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	Array2A &
	or_equals( MArray2< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

public: // Assignment: Value

	// = Value
	Array2A &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	Array2A &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	Array2A &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	Array2A &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	Array2A &
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
	Array2A &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange Even if Const
	Array2A const &
	dim( IR const & I1, IR const & I2 ) const
	{
		const_cast< Array2A & >( *this ).dimension( I1, I2 );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	Array2A const &
	dim( Array2< U > const & a ) const
	{
		const_cast< Array2A & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	Array2A &
	dimension( IR const & I1, IR const & I2 )
	{
		I1_.assign( I1 );
		I2_.assign( I2 );
		z1_ = I1_.size();
		z2_ = I2_.size();
		dimension_argument();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	Array2A &
	dimension( Array2< U > const & a )
	{
		I1_.assign( a.I1() );
		I2_.assign( a.I2() );
		z1_ = I1_.size();
		z2_ = I2_.size();
		dimension_argument();
		return *this;
	}

	// Attach to Super Array
	Array2A &
	attach( Super const & a )
	{
		Base::attach( a );
		I1_.assign( a.I1_ );
		I2_.assign( a.I2_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		return *this;
	}

	// Attach to Non-Const Super Array
	Array2A &
	attach( Super & a )
	{
		Base::attach( a );
		I1_.assign( a.I1_ );
		I2_.assign( a.I2_ );
		z1_ = a.z1_;
		z2_ = a.z2_;
		return *this;
	}

	// Attach to Base Array
	Array2A &
	attach( Base const & a )
	{
		Base::attach< 2 >( a );
		I1_ = a.isize();
		I2_ = 1;
		z1_ = I1_.size();
		z2_ = 1u;
		return *this;
	}

	// Attach to Non-Const Base Array
	Array2A &
	attach( Base & a )
	{
		Base::attach< 2 >( a );
		I1_ = a.isize();
		I2_ = 1;
		z1_ = I1_.size();
		z2_ = 1u;
		return *this;
	}

	// Attach to Tail
	Array2A &
	attach( Tail const & s )
	{
		Base::attach< 2 >( s );
		I1_ = s.isize();
		I2_ = 1;
		z1_ = I1_.size();
		z2_ = 1u;
		return *this;
	}

	// Attach to Non-Const Tail
	Array2A &
	attach( Tail & s )
	{
		Base::attach< 2 >( s );
		I1_ = s.isize();
		I2_ = 1;
		z1_ = I1_.size();
		z2_ = 1u;
		return *this;
	}

	// Attach to Value
	Array2A &
	attach( T const & t )
	{
		Base::attach< 2 >( t );
		I1_ = _;
		I2_ = 1;
		z1_ = I1_.size();
		z2_ = 1u;
		return *this;
	}

	// Attach to Non-Const Value
	Array2A &
	attach( T & t )
	{
		Base::attach< 2 >( t );
		I1_ = _;
		I2_ = 1;
		z1_ = I1_.size();
		z2_ = 1u;
		return *this;
	}

	// Detach from Source Array
	Array2A &
	detach()
	{
		Base::detach();
		I1_.clear();
		I2_.clear();
		z1_ = z2_ = 0u;
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	bool
	dimension_assign( IR const & I1, IR const & I2 )
	{
		I1_.assign( I1 );
		I2_.assign( I2 );
		z1_ = I1_.size();
		z2_ = I2_.size();
		dimension_argument();
		return false;
	}

private: // Functions

	// Dimension by Current IndexRanges
	void
	dimension_argument()
	{
		assert( I2_.bounded() );
		if ( I1_.bounded() ) {
			size_set( size_of( z1_, z2_ ) );
		} else if ( size_ == npos ) {
			size_set( npos );
		} else {
			if ( z2_ > 0u ) { // Infer size
				z1_ = size_ / z2_;
				I1_.u( I1_.l() + static_cast< int >( z1_ ) - 1 );
				size_set( size_of( z1_, z2_ ) );
			} else {
				size_set( size_ );
			}
		}
		shift_set( ( I1_.l() * z2_ ) + I2_.l() );
	}

}; // Array2A

} // ObjexxFCL

#endif // ObjexxFCL_Array2A_hh_INCLUDED
