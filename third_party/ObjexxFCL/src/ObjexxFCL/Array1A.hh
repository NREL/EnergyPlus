#ifndef ObjexxFCL_Array1A_hh_INCLUDED
#define ObjexxFCL_Array1A_hh_INCLUDED

// 1D Argument Array
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.fwd.hh>
#include <ObjexxFCL/Array1.hh>

namespace ObjexxFCL {

// 1D Argument Array
template< typename T >
class Array1A : public Array1< T >
{

private: // Types

	using Super = Array1< T >;
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
	using Super::size_set;

	using Super::data_;
	using Super::I_;
	using Super::shift_;
	using Super::size_;

public: // Creation

	// Default Constructor
	Array1A() :
	 Super( ProxySentinel() )
	{}

	// Copy Constructor
	Array1A( Array1A const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift_ );
	}

	// Super Constructor
	Array1A( Super const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift_ );
	}

	// Slice Constructor
	Array1A( Array1S< T > const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( a.shift() );
	}

	// Base Constructor
	Array1A( Base const & a ) :
	 Super( a, ProxySentinel() )
	{
		shift_set( 1 );
	}

	// Tail Constructor
	Array1A( Tail const & s ) :
	 Super( s, ProxySentinel() )
	{
		shift_set( 1 );
	}

	// Value Constructor
	Array1A( T const & t ) :
	 Super( t, ProxySentinel() )
	{
		shift_set( 1 );
	}

	// Copy + IndexRange Constructor
	Array1A( Array1A const & a, IR const & I ) :
	 Super( a, I, ProxySentinel() )
	{
		dimension_argument();
	}

	// Super + IndexRange Constructor
	Array1A( Super const & a, IR const & I ) :
	 Super( a, I, ProxySentinel() )
	{
		dimension_argument();
	}

	// Slice + IndexRange Constructor
	Array1A( Array1S< T > const & a, IR const & I ) :
	 Super( a, I, ProxySentinel() )
	{
		dimension_argument();
	}

	// Base + IndexRange Constructor
	Array1A( Base const & a, IR const & I ) :
	 Super( a, I, ProxySentinel() )
	{
		dimension_argument();
	}

	// Tail + IndexRange Constructor
	Array1A( Tail const & s, IR const & I ) :
	 Super( s, I, ProxySentinel() )
	{
		dimension_argument();
	}

	// Value + IndexRange Constructor
	Array1A( T const & t, IR const & I ) :
	 Super( t, I, ProxySentinel() )
	{
		dimension_argument();
	}

public: // Assignment: Array

	// Copy Assignment
	Array1A &
	operator =( Array1A const & a )
	{
		if ( this != &a ) {
			if ( ! conformable( a ) ) dimension( a );
			Base::operator =( a );
		}
		return *this;
	}

	// Super Assignment
	Array1A &
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
	Array1A &
	operator =( Array1< U > const & a )
	{
		if ( ! conformable( a ) ) dimension( a );
		Base::operator =( a );
		return *this;
	}

	// Slice Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( Array1S< U > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	Array1A &
	operator =( MArray1< A, M > const & a )
	{
		Super::operator =( a );
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( std::initializer_list< U > const l )
	{
		Base::operator =( l );
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( std::array< U, s > const & a )
	{
		Base::operator =( a );
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( std::vector< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( Vector2< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( Vector3< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// Vector4 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator =( Vector4< U > const & v )
	{
		Base::operator =( v );
		return *this;
	}

	// += Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( Array1< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( Array1< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( Array1< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( Array1< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( Array1S< U > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( Array1S< U > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( Array1S< U > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( Array1S< U > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	Array1A &
	operator +=( MArray1< A, M > const & a )
	{
		Super::operator +=( a );
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	Array1A &
	operator -=( MArray1< A, M > const & a )
	{
		Super::operator -=( a );
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	Array1A &
	operator *=( MArray1< A, M > const & a )
	{
		Super::operator *=( a );
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	Array1A &
	operator /=( MArray1< A, M > const & a )
	{
		Super::operator /=( a );
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( std::initializer_list< U > const l )
	{
		Base::operator +=( l );
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( std::initializer_list< U > const l )
	{
		Base::operator -=( l );
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( std::initializer_list< U > const l )
	{
		Base::operator *=( l );
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( std::initializer_list< U > const l )
	{
		Base::operator /=( l );
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( std::array< U, s > const & a )
	{
		Base::operator +=( a );
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( std::array< U, s > const & a )
	{
		Base::operator -=( a );
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( std::array< U, s > const & a )
	{
		Base::operator *=( a );
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( std::array< U, s > const & a )
	{
		Base::operator /=( a );
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( std::vector< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( std::vector< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( std::vector< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( std::vector< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( Vector2< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( Vector2< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( Vector2< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( Vector2< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( Vector3< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( Vector3< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( Vector3< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( Vector3< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

	// += Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator +=( Vector4< U > const & v )
	{
		Base::operator +=( v );
		return *this;
	}

	// -= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator -=( Vector4< U > const & v )
	{
		Base::operator -=( v );
		return *this;
	}

	// *= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator *=( Vector4< U > const & v )
	{
		Base::operator *=( v );
		return *this;
	}

	// /= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	operator /=( Vector4< U > const & v )
	{
		Base::operator /=( v );
		return *this;
	}

public: // Assignment: Array: Logical

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( Array1< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( Array1< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( Array1S< U > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= Slice Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( Array1S< U > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= MArray Template
	template< class A, typename M >
	Array1A &
	and_equals( MArray1< A, M > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= MArray Template
	template< class A, typename M >
	Array1A &
	or_equals( MArray1< A, M > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( std::initializer_list< U > const l )
	{
		Super::and_equals( l );
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( std::initializer_list< U > const l )
	{
		Super::or_equals( l );
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( std::array< U, s > const & a )
	{
		Super::and_equals( a );
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( std::array< U, s > const & a )
	{
		Super::or_equals( a );
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( std::vector< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( std::vector< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( Vector2< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( Vector2< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( Vector3< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( Vector3< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

	// &&= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	and_equals( Vector4< U > const & v )
	{
		Super::and_equals( v );
		return *this;
	}

	// ||= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Array1A &
	or_equals( Vector4< U > const & v )
	{
		Super::or_equals( v );
		return *this;
	}

public: // Assignment: Value

	// = Value
	Array1A &
	operator =( T const & t )
	{
		Base::operator =( t );
		return *this;
	}

	// += Value
	Array1A &
	operator +=( T const & t )
	{
		Base::operator +=( t );
		return *this;
	}

	// -= Value
	Array1A &
	operator -=( T const & t )
	{
		Base::operator -=( t );
		return *this;
	}

	// *= Value
	Array1A &
	operator *=( T const & t )
	{
		Base::operator *=( t );
		return *this;
	}

	// /= Value
	Array1A &
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
	Array1A &
	clear()
	{
		Super::clear();
		return *this;
	}

	// Dimension by IndexRange Even if Const
	Array1A const &
	dim( IR const & I ) const
	{
		const_cast< Array1A & >( *this ).dimension( I );
		return *this;
	}

	// Dimension by Array Even if Const
	template< typename U >
	Array1A const &
	dim( Array1< U > const & a ) const
	{
		const_cast< Array1A & >( *this ).dimension( a );
		return *this;
	}

	// Dimension by IndexRange
	Array1A &
	dimension( IR const & I )
	{
		I_.assign( I );
		dimension_argument();
		return *this;
	}

	// Dimension by Array
	template< typename U >
	Array1A &
	dimension( Array1< U > const & a )
	{
		I_.assign( a.I() );
		dimension_argument();
		return *this;
	}

	// Attach to Super Array
	Array1A &
	attach( Super const & a )
	{
		Base::attach( a );
		I_.assign( a.I_ );
		return *this;
	}

	// Attach to Non-Const Super Array
	Array1A &
	attach( Super & a )
	{
		Base::attach( a );
		I_.assign( a.I_ );
		return *this;
	}

	// Attach to Base Array
	Array1A &
	attach( Base const & a )
	{
		Base::attach< 1 >( a );
		I_ = a.isize();
		return *this;
	}

	// Attach to Non-Const Base Array
	Array1A &
	attach( Base & a )
	{
		Base::attach< 1 >( a );
		I_ = a.isize();
		return *this;
	}

	// Attach to Tail
	Array1A &
	attach( Tail const & s )
	{
		Base::attach< 1 >( s );
		I_ = s.isize();
		return *this;
	}

	// Attach to Non-Const Tail
	Array1A &
	attach( Tail & s )
	{
		Base::attach< 1 >( s );
		I_ = s.isize();
		return *this;
	}

	// Attach to Value
	Array1A &
	attach( T const & t )
	{
		Base::attach< 1 >( t );
		I_ = _;
		return *this;
	}

	// Attach to Non-Const Value
	Array1A &
	attach( T & t )
	{
		Base::attach< 1 >( t );
		I_ = _;
		return *this;
	}

	// Detach from Source Array
	Array1A &
	detach()
	{
		Base::detach();
		I_.clear();
		return *this;
	}

protected: // Functions

	// Dimension by IndexRange
	bool
	dimension_assign( IR const & I )
	{
		I_.assign( I );
		dimension_argument();
		return false;
	}

private: // Functions

	// Dimension by Current IndexRange
	void
	dimension_argument()
	{
		if ( I_.bounded() ) {
			size_set( I_.size() );
		} else if ( size_ == npos ) {
			size_set( npos );
		} else { // Infer size
			I_.u( I_.l() + static_cast< int >( size_ ) - 1 );
			size_set( I_.size() );
		}
		shift_set( I_.l() );
	}

}; // Array1A

} // ObjexxFCL

#endif // ObjexxFCL_Array1A_hh_INCLUDED
