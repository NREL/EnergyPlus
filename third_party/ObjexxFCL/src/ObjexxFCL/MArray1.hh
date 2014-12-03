#ifndef ObjexxFCL_MArray1_hh_INCLUDED
#define ObjexxFCL_MArray1_hh_INCLUDED

// MArray1: 1D Member Array Proxy
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
#include <ObjexxFCL/MArrayR.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>

// C++ Headers
#include <array>
#include <cmath>
#include <vector>

namespace ObjexxFCL {

// MArray1: 1D Member Array Proxy
template< class A, typename T >
class MArray1 : public MArrayR< A, T, 1 >
{

private: // Types

	typedef  MArrayR< A, T, 1 >  Super;

private: // Friend

	template< typename, typename > friend class MArray1;

public: // Types

	typedef  typename Super::Array  Array;
	typedef  typename Super::Class  Class;
	typedef  typename Super::MPtr  MPtr;
	typedef  typename Super::Traits  Traits;
	typedef  typename Super::IR  IR;

	// STL Style
	typedef  typename Super::value_type  value_type;
	typedef  typename Super::reference  reference;
	typedef  typename Super::const_reference  const_reference;
	typedef  typename Super::pointer  pointer;
	typedef  typename Super::const_pointer  const_pointer;
	typedef  typename Super::size_type  size_type;
	typedef  typename Super::difference_type  difference_type;

	// C++ Style
	typedef  typename Super::Value  Value;
	typedef  typename Super::Reference  Reference;
	typedef  typename Super::ConstReference  ConstReference;
	typedef  typename Super::Pointer  Pointer;
	typedef  typename Super::ConstPointer  ConstPointer;
	typedef  typename Super::Size  Size;
	typedef  typename Super::Difference  Difference;

	// Using
	using Super::in_range;
	using Super::isize;
	using Super::l;
	using Super::u;
	using Super::size;
	using Super::j1;
	using Super::array_;
	using Super::pmem_;

public: // Creation

	// Copy Constructor
	inline
	MArray1( MArray1 const & a ) :
	 Super( a )
	{}

	// Constructor
	inline
	MArray1( A & a, T Class::* pmem ) :
	 Super( a, pmem )
	{}

	// Destructor
	inline
	virtual
	~MArray1()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	MArray1 &
	operator =( MArray1 const & a )
	{
		if ( this != &a ) {
			assert( conformable( a ) );
			for ( int i = 1, e = u(); i <= e; ++i ) {
				operator ()( i ) = a( i ); // Not overlap-safe
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename Aa, typename Ta >
	inline
	MArray1 &
	operator =( MArray1< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = a( i ); // Not overlap-safe
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) = a( j ); // Not overlap-safe
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = *r;
		}
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = *r;
		}
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = *r;
		}
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) = v[ 0 ];
		operator ()( 2 ) = v[ 1 ];
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) = v[ 0 ];
		operator ()( 2 ) = v[ 1 ];
		operator ()( 3 ) = v[ 2 ];
		return *this;
	}

	// += MArray1 Template
	template< typename Aa, typename Ta >
	inline
	MArray1 &
	operator +=( MArray1< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) += a( i ); // Not overlap-safe
		}
		return *this;
	}

	// -= MArray1 Template
	template< typename Aa, typename Ta >
	inline
	MArray1 &
	operator -=( MArray1< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) -= a( i ); // Not overlap-safe
		}
		return *this;
	}

	// *= MArray1 Template
	template< typename Aa, typename Ta >
	inline
	MArray1 &
	operator *=( MArray1< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) *= a( i ); // Not overlap-safe
		}
		return *this;
	}

	// /= MArray1 Template
	template< typename Aa, typename Ta >
	inline
	MArray1 &
	operator /=( MArray1< Aa, Ta > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			assert( a( i ) != T( 0 ) );
			operator ()( i ) /= a( i ); // Not overlap-safe
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator +=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) += a( j ); // Not overlap-safe
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator -=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) -= a( j ); // Not overlap-safe
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator *=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) *= a( j ); // Not overlap-safe
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			assert( a( j ) != T( 0 ) );
			operator ()( i ) /= a( j ); // Not overlap-safe
		}
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator +=( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) += *r;
		}
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator -=( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) -= *r;
		}
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator *=( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) *= *r;
		}
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			assert( *r != T( 0 ) );
			operator ()( i ) /= *r;
		}
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator +=( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) += *r;
		}
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator -=( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) -= *r;
		}
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator *=( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) *= *r;
		}
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			assert( *r != T( 0 ) );
			operator ()( i ) /= *r;
		}
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator +=( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) += *r;
		}
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator -=( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) -= *r;
		}
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator *=( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) *= *r;
		}
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			assert( *r != T( 0 ) );
			operator ()( i ) /= *r;
		}
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator +=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) += v[ 0 ];
		operator ()( 2 ) += v[ 1 ];
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator -=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) -= v[ 0 ];
		operator ()( 2 ) -= v[ 1 ];
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator *=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) *= v[ 0 ];
		operator ()( 2 ) *= v[ 1 ];
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		assert( v[ 0 ] != T( 0 ) );
		assert( v[ 1 ] != T( 0 ) );
		operator ()( 1 ) /= v[ 0 ];
		operator ()( 2 ) /= v[ 1 ];
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator +=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) += v[ 0 ];
		operator ()( 2 ) += v[ 1 ];
		operator ()( 3 ) += v[ 2 ];
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator -=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) -= v[ 0 ];
		operator ()( 2 ) -= v[ 1 ];
		operator ()( 3 ) -= v[ 2 ];
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator *=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) *= v[ 0 ];
		operator ()( 2 ) *= v[ 1 ];
		operator ()( 3 ) *= v[ 2 ];
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		assert( v[ 0 ] != T( 0 ) );
		assert( v[ 1 ] != T( 0 ) );
		assert( v[ 2 ] != T( 0 ) );
		operator ()( 1 ) /= v[ 0 ];
		operator ()( 2 ) /= v[ 1 ];
		operator ()( 3 ) /= v[ 2 ];
		return *this;
	}

public: // Assignment: Logical

	// &&= MArray1 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	and_equals( MArray1 const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = operator ()( i ) && a( i ); // Not overlap-safe
		}
		return *this;
	}

	// ||= MArray1 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	or_equals( MArray1 const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = operator ()( i ) || a( i ); // Not overlap-safe
		}
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	and_equals( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) && *r;
		}
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	or_equals( std::initializer_list< U > const l )
	{
		assert( size() == l.size() );
		auto r( l.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) || *r;
		}
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	and_equals( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) && *r;
		}
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	or_equals( std::array< U, s > const & a )
	{
		assert( size() == s );
		auto r( a.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) || *r;
		}
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	and_equals( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) && *r;
		}
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	or_equals( std::vector< U > const & v )
	{
		assert( size() == v.size() );
		auto r( v.begin() );
		for ( int i = 1, e = u(); i <= e; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) || *r;
		}
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	and_equals( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) = operator ()( 1 ) && v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) && v[ 1 ];
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	or_equals( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) = operator ()( 1 ) || v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) || v[ 1 ];
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	and_equals( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) = operator ()( 1 ) && v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) && v[ 1 ];
		operator ()( 3 ) = operator ()( 3 ) && v[ 2 ];
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	or_equals( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) = operator ()( 1 ) || v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) || v[ 1 ];
		operator ()( 3 ) = operator ()( 3 ) || v[ 2 ];
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	MArray1 &
	operator =( T const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = t;
		}
		return *this;
	}

	// = Value Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator =( U const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = t;
		}
		return *this;
	}

	// += Value
	inline
	MArray1 &
	operator +=( T const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) += t;
		}
		return *this;
	}

	// -= Value
	inline
	MArray1 &
	operator -=( T const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) -= t;
		}
		return *this;
	}

	// *= Value
	inline
	MArray1 &
	operator *=( T const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) *= t;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	inline
	MArray1 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) *= inv_u;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	inline
	MArray1 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) /= u;
		}
		return *this;
	}

public: // Subscript

	// array( i ) const
	inline
	T const &
	operator ()( int const i ) const
	{
		assert( contains( i ) );
		return array_( j1( i ) ).*pmem_;
	}

	// array( i )
	inline
	T &
	operator ()( int const i )
	{
		assert( contains( i ) );
		return array_( j1( i ) ).*pmem_;
	}

	// array[ i ] const: 0-Based Subscript
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( contains( i + 1 ) );
		return array_( j1( i + 1 ) ).*pmem_;
	}

	// array[ i ]: 0-Based Subscript
	inline
	T &
	operator []( size_type const i )
	{
		assert( contains( i + 1 ) );
		return array_( j1( i + 1 ) ).*pmem_;
	}

public: // Predicate

	// Contains Indexed Element?
	inline
	bool
	contains( int const i ) const
	{
		return in_range( u(), i );
	}

	// Conformable?
	template< typename Aa, typename Ta >
	inline
	bool
	conformable( MArray1< Aa, Ta > const & a ) const
	{
		return ( size() == a.size() );
	}

	// Conformable?
	template< class ArrayType >
	inline
	bool
	conformable( ArrayType const & a ) const
	{
		return ( ( a.rank() == 1 ) && ( size() == a.size() ) );
	}

	// Equal Dimensions?
	template< typename Aa, typename Ta >
	inline
	bool
	equal_dimensions( MArray1< Aa, Ta > const & a ) const
	{
		return conformable( a );
	}

	// Equal Dimensions?
	template< class ArrayType >
	inline
	bool
	equal_dimensions( ArrayType const & a ) const
	{
		return conformable( a );
	}

public: // Inspector

	// IndexRange
	inline
	IR
	I() const
	{
		return IR( 1, u() );
	}

	// Lower Index
	inline
	int
	l() const
	{
		return 1;
	}

	// Upper Index
	inline
	int
	u() const
	{
		return array_.isize1();
	}

	// IndexRange of Dimension 1
	inline
	IR
	I1() const
	{
		return IR( 1, u1() );
	}

	// Lower Index of Dimension 1
	inline
	int
	l1() const
	{
		return 1;
	}

	// Upper Index of Dimension 1
	inline
	int
	u1() const
	{
		return array_.isize1();
	}

	// Size of Dimension 1
	inline
	size_type
	size1() const
	{
		return array_.size1();
	}

	// Size of Dimension 1
	inline
	int
	isize1() const
	{
		return array_.isize1();
	}

	// Length
	inline
	T
	length() const
	{
		T length_sq( T( 0 ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			T const length_i( operator ()( i ) );
			length_sq += length_i * length_i;
		}
		return std::sqrt( length_sq );
	}

	// Length Squared
	inline
	T
	length_squared() const
	{
		T length_sq( T( 0 ) );
		for ( int i = 1, e = u(); i <= e; ++i ) {
			T const length_i( operator ()( i ) );
			length_sq += length_i * length_i;
		}
		return length_sq;
	}

public: // Modifier

	// Assign Default Value to all Elements
	inline
	MArray1 &
	to_default()
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = Traits::initial_value();
		}
		return *this;
	}

	// Normalize to Unit Length
	inline
	MArray1 &
	normalize()
	{
		T const length_( length() );
		assert( length_ > T( 0 ) );
		operator /=( length_ );
		return *this;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray1< MArray1 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray1< MArray1 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray1< MArray1, M >
	ma( M ClassT::* pmem )
	{
		return MArray1< MArray1, M >( *this, pmem );
	}

#include <ObjexxFCL/MArray1.Project.MArray.hh> // Inject project-specific MArray generators

public: // Comparison: Predicate

	// MArray1 == MArray1
	inline
	friend
	bool
	eq( MArray1 const & a, MArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) == b( i ) ) ) return false;
		}
		return true;
	}

	// MArray1 != MArray1
	inline
	friend
	bool
	ne( MArray1 const & a, MArray1 const & b )
	{
		return ! eq( a, b );
	}

	// MArray1 < MArray1
	inline
	friend
	bool
	lt( MArray1 const & a, MArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) < b( i ) ) ) return false;
		}
		return true;
	}

	// MArray1 <= MArray1
	inline
	friend
	bool
	le( MArray1 const & a, MArray1 const & b )
	{
		assert( a.size_bounded() );
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) <= b( i ) ) ) return false;
		}
		return true;
	}

	// MArray1 > MArray1
	inline
	friend
	bool
	gt( MArray1 const & a, MArray1 const & b )
	{
		return lt( b, a );
	}

	// MArray1 >= MArray1
	inline
	friend
	bool
	ge( MArray1 const & a, MArray1 const & b )
	{
		return le( b, a );
	}

	// MArray1 == Value
	inline
	friend
	bool
	eq( MArray1 const & a, T const & t )
	{
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) == t ) ) return false;
		}
		return true;
	}

	// MArray1 != Value
	inline
	friend
	bool
	ne( MArray1 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// MArray1 < Value
	inline
	friend
	bool
	lt( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) < t ) ) return false;
		}
		return true;
	}

	// MArray1 <= Value
	inline
	friend
	bool
	le( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) <= t ) ) return false;
		}
		return true;
	}

	// MArray1 > Value
	inline
	friend
	bool
	gt( MArray1 const & a, T const & t )
	{
		return lt( t, a );
	}

	// MArray1 >= Value
	inline
	friend
	bool
	ge( MArray1 const & a, T const & t )
	{
		return le( t, a );
	}

	// Value == MArray1
	inline
	friend
	bool
	eq( T const & t, MArray1 const & a )
	{
		return eq( a, t );
	}

	// Value != MArray1
	inline
	friend
	bool
	ne( T const & t, MArray1 const & a )
	{
		return ! eq( a, t );
	}

	// Value < MArray1
	inline
	friend
	bool
	lt( T const & t, MArray1 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( t < a( i ) ) ) return false;
		}
		return true;
	}

	// Value <= MArray1
	inline
	friend
	bool
	le( T const & t, MArray1 const & a )
	{
		if ( a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( t <= a( i ) ) ) return false;
		}
		return true;
	}

	// Value > MArray1
	inline
	friend
	bool
	gt( T const & t, MArray1 const & a )
	{
		return lt( a, t );
	}

	// Value >= MArray1
	inline
	friend
	bool
	ge( T const & t, MArray1 const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any MArray1 == MArray1
	inline
	friend
	bool
	any_eq( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) return true;
		}
		return false;
	}

	// Any MArray1 != MArray1
	inline
	friend
	bool
	any_ne( MArray1 const & a, MArray1 const & b )
	{
		return ! eq( a, b );
	}

	// Any MArray1 < MArray1
	inline
	friend
	bool
	any_lt( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < b( i ) ) return true;
		}
		return false;
	}

	// Any MArray1 <= MArray1
	inline
	friend
	bool
	any_le( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) return true;
		}
		return false;
	}

	// Any MArray1 > MArray1
	inline
	friend
	bool
	any_gt( MArray1 const & a, MArray1 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray1 >= MArray1
	inline
	friend
	bool
	any_ge( MArray1 const & a, MArray1 const & b )
	{
		return any_le( b, a );
	}

	// Any MArray1 == Value
	inline
	friend
	bool
	any_eq( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == t ) return true;
		}
		return false;
	}

	// Any MArray1 != Value
	inline
	friend
	bool
	any_ne( MArray1 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any MArray1 < Value
	inline
	friend
	bool
	any_lt( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < t ) return true;
		}
		return false;
	}

	// Any MArray1 <= Value
	inline
	friend
	bool
	any_le( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= t ) return true;
		}
		return false;
	}

	// Any MArray1 > Value
	inline
	friend
	bool
	any_gt( MArray1 const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any MArray1 >= Value
	inline
	friend
	bool
	any_ge( MArray1 const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value == MArray1
	inline
	friend
	bool
	any_eq( T const & t, MArray1 const & a )
	{
		return any_eq( a, t );
	}

	// Any Value != MArray1
	inline
	friend
	bool
	any_ne( T const & t, MArray1 const & a )
	{
		return ! eq( a, t );
	}

	// Any Value < MArray1
	inline
	friend
	bool
	any_lt( T const & t, MArray1 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( t < a( i ) ) return true;
		}
		return false;
	}

	// Any Value <= MArray1
	inline
	friend
	bool
	any_le( T const & t, MArray1 const & a )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( t <= a( i ) ) return true;
		}
		return false;
	}

	// Any Value > MArray1
	inline
	friend
	bool
	any_gt( T const & t, MArray1 const & a )
	{
		return any_lt( a, t );
	}

	// Any Value >= MArray1
	inline
	friend
	bool
	any_ge( T const & t, MArray1 const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All MArray1 == MArray1
	inline
	friend
	bool
	all_eq( MArray1 const & a, MArray1 const & b )
	{
		return eq( a, b );
	}

	// All MArray1 != MArray1
	inline
	friend
	bool
	all_ne( MArray1 const & a, MArray1 const & b )
	{
		return ! any_eq( a, b );
	}

	// All MArray1 < MArray1
	inline
	friend
	bool
	all_lt( MArray1 const & a, MArray1 const & b )
	{
		return lt( a, b );
	}

	// All MArray1 <= MArray1
	inline
	friend
	bool
	all_le( MArray1 const & a, MArray1 const & b )
	{
		return le( a, b );
	}

	// All MArray1 > MArray1
	inline
	friend
	bool
	all_gt( MArray1 const & a, MArray1 const & b )
	{
		return gt( a, b );
	}

	// All MArray1 >= MArray1
	inline
	friend
	bool
	all_ge( MArray1 const & a, MArray1 const & b )
	{
		return ge( a, b );
	}

	// All MArray1 == Value
	inline
	friend
	bool
	all_eq( MArray1 const & a, T const & t )
	{
		return eq( a, t );
	}

	// All MArray1 != Value
	inline
	friend
	bool
	all_ne( MArray1 const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All MArray1 < Value
	inline
	friend
	bool
	all_lt( MArray1 const & a, T const & t )
	{
		return lt( a, t );
	}

	// All MArray1 <= Value
	inline
	friend
	bool
	all_le( MArray1 const & a, T const & t )
	{
		return le( a, t );
	}

	// All MArray1 > Value
	inline
	friend
	bool
	all_gt( MArray1 const & a, T const & t )
	{
		return gt( a, t );
	}

	// All MArray1 >= Value
	inline
	friend
	bool
	all_ge( MArray1 const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value == MArray1
	inline
	friend
	bool
	all_eq( T const & t, MArray1 const & a )
	{
		return eq( t, a );
	}

	// All Value != MArray1
	inline
	friend
	bool
	all_ne( T const & t, MArray1 const & a )
	{
		return ! any_eq( t, a );
	}

	// All Value < MArray1
	inline
	friend
	bool
	all_lt( T const & t, MArray1 const & a )
	{
		return lt( t, a );
	}

	// All Value <= MArray1
	inline
	friend
	bool
	all_le( T const & t, MArray1 const & a )
	{
		return le( t, a );
	}

	// All Value > MArray1
	inline
	friend
	bool
	all_gt( T const & t, MArray1 const & a )
	{
		return gt( t, a );
	}

	// All Value >= MArray1
	inline
	friend
	bool
	all_ge( T const & t, MArray1 const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count MArray1 == MArray1
	inline
	friend
	size_type
	count_eq( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 != MArray1
	inline
	friend
	size_type
	count_ne( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) != b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 < MArray1
	inline
	friend
	size_type
	count_lt( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 <= MArray1
	inline
	friend
	size_type
	count_le( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 > MArray1
	inline
	friend
	size_type
	count_gt( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) > b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 >= MArray1
	inline
	friend
	size_type
	count_ge( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 == Value
	inline
	friend
	size_type
	count_eq( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == t ) ++n;
		}
		return n;
	}

	// Count Value == MArray1
	inline
	friend
	size_type
	count_eq( T const & t, MArray1 const & a )
	{
		return count_eq( a, t );
	}

	// Count MArray1 != Value
	inline
	friend
	size_type
	count_ne( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) != t ) ++n;
		}
		return n;
	}

	// Count Value != MArray1
	inline
	friend
	size_type
	count_ne( T const & t, MArray1 const & a )
	{
		return count_ne( a, t );
	}

	// Count MArray1 < Value
	inline
	friend
	size_type
	count_lt( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < t ) ++n;
		}
		return n;
	}

	// Count Value < MArray1
	inline
	friend
	size_type
	count_lt( T const & t, MArray1 const & a )
	{
		return count_gt( a, t );
	}

	// Count MArray1 <= Value
	inline
	friend
	size_type
	count_le( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= t ) ++n;
		}
		return n;
	}

	// Count Value <= MArray1
	inline
	friend
	size_type
	count_le( T const & t, MArray1 const & a )
	{
		return count_ge( a, t );
	}

	// Count MArray1 > Value
	inline
	friend
	size_type
	count_gt( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) > t ) ++n;
		}
		return n;
	}

	// Count Value > MArray1
	inline
	friend
	size_type
	count_gt( T const & t, MArray1 const & a )
	{
		return count_lt( a, t );
	}

	// Count MArray1 >= Value
	inline
	friend
	size_type
	count_ge( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= t ) ++n;
		}
		return n;
	}

	// Count Value >= MArray1
	inline
	friend
	size_type
	count_ge( T const & t, MArray1 const & a )
	{
		return count_le( a, t );
	}

}; // MArray1

// Functions

// Make a MArray1
template< class A, typename T >
inline
MArray1< A, T >
make_MArray1( A & array, T A::value_type::* pmem )
{
	return MArray1< A, T >( array, pmem );
}

// Make a MArray1
template< class A, typename T >
inline
MArray1< A, T >
MA1( A & array, T A::value_type::* pmem )
{
	return MArray1< A, T >( array, pmem );
}

// Conformable?
template< typename Aa, typename Ta, typename Ab, typename Tb >
inline
bool
conformable( MArray1< Aa, Ta > const & a, MArray1< Ab, Tb > const & b )
{
	return a.conformable( b );
}

// Equal Dimensions?
template< typename Aa, typename Ta, typename Ab, typename Tb >
inline
bool
equal_dimensions( MArray1< Aa, Ta > const & a, MArray1< Ab, Tb > const & b )
{
	return a.equal_dimensions( b );
}

// Magnitude
template< class A, typename T >
inline
T
magnitude( MArray1< A, T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		T const mag_i( a( i ) );
		mag_sq += mag_i * mag_i;
	}
	return std::sqrt( mag_sq );
}

// Magnitude Squared
template< class A, typename T >
inline
T
magnitude_squared( MArray1< A, T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		T const mag_i( a( i ) );
		mag_sq += mag_i * mag_i;
	}
	return mag_sq;
}

// Distance
template< class A, typename T >
inline
T
distance( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		T const distance_i( a( i ) - b( i ) );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size() == b.size() );
	T distance_sq( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		T const distance_i( a( i ) - b( i ) );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Dot Product
template< class A, typename T >
inline
T
dot( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( a.size() == b.size() );
	T result( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		result += a( i ) * b( i );
	}
	return result;
}

// Dot Product of Boolean Arrays
template< class A >
inline
bool
dot( MArray1< A, bool > const & a, MArray1< A, bool > const & b )
{
	assert( a.size() == b.size() );
	bool result( false );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		if ( a( i ) && b( i ) ) {
			result = true;
			break;
		}
	}
	return result;
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( MArray1< A, bool > const & a, MArray1< A, bool > const & b )
{
	return dot( a, b );
}

} // ObjexxFCL

#endif // ObjexxFCL_MArray1_hh_INCLUDED
