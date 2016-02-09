#ifndef ObjexxFCL_MArray1_hh_INCLUDED
#define ObjexxFCL_MArray1_hh_INCLUDED

// MArray1: 1D Member Array Proxy
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
#include <ObjexxFCL/MArrayR.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/Vector4.hh>

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

	typedef  typename Super::ArrayType  ArrayType;
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
	MArray1( MArray1 const & a ) :
	 Super( a )
	{}

	// Constructor
	MArray1( A & a, T Class::* pmem ) :
	 Super( a, pmem )
	{}

	// Destructor
	virtual
	~MArray1()
	{}

public: // Assignment: Array

	// Copy Assignment
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
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator =( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) = a( j ); // Not overlap-safe
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
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
	MArray1 &
	operator =( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) = v.x;
		operator ()( 2 ) = v.y;
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator =( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) = v.x;
		operator ()( 2 ) = v.y;
		operator ()( 3 ) = v.z;
		return *this;
	}

	// Vector4 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator =( Vector4< U > const & v )
	{
		assert( size() == 4u );
		operator ()( 1 ) = v.x;
		operator ()( 2 ) = v.y;
		operator ()( 3 ) = v.z;
		operator ()( 3 ) = v.w;
		return *this;
	}

	// += MArray1 Template
	template< typename Aa, typename Ta >
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
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator +=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) += a( j ); // Not overlap-safe
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator -=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) -= a( j ); // Not overlap-safe
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator *=( Ar< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(), e = u(); i <= e; ++i, ++j ) {
			operator ()( i ) *= a( j ); // Not overlap-safe
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class Ar, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator /=( Ar< U > const & a )
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
	MArray1 &
	operator +=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) += v.x;
		operator ()( 2 ) += v.y;
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator -=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) -= v.x;
		operator ()( 2 ) -= v.y;
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator *=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) *= v.x;
		operator ()( 2 ) *= v.y;
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator /=( Vector2< U > const & v )
	{
		assert( size() == 2u );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		operator ()( 1 ) /= v.x;
		operator ()( 2 ) /= v.y;
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator +=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) += v.x;
		operator ()( 2 ) += v.y;
		operator ()( 3 ) += v.z;
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator -=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) -= v.x;
		operator ()( 2 ) -= v.y;
		operator ()( 3 ) -= v.z;
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator *=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) *= v.x;
		operator ()( 2 ) *= v.y;
		operator ()( 3 ) *= v.z;
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator /=( Vector3< U > const & v )
	{
		assert( size() == 3u );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		operator ()( 1 ) /= v.x;
		operator ()( 2 ) /= v.y;
		operator ()( 3 ) /= v.z;
		return *this;
	}

	// += Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator +=( Vector4< U > const & v )
	{
		assert( size() == 4u );
		operator ()( 1 ) += v.x;
		operator ()( 2 ) += v.y;
		operator ()( 3 ) += v.z;
		operator ()( 4 ) += v.w;
		return *this;
	}

	// -= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator -=( Vector4< U > const & v )
	{
		assert( size() == 4u );
		operator ()( 1 ) -= v.x;
		operator ()( 2 ) -= v.y;
		operator ()( 3 ) -= v.z;
		operator ()( 4 ) -= v.w;
		return *this;
	}

	// *= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator *=( Vector4< U > const & v )
	{
		assert( size() == 4u );
		operator ()( 1 ) *= v.x;
		operator ()( 2 ) *= v.y;
		operator ()( 3 ) *= v.z;
		operator ()( 4 ) *= v.w;
		return *this;
	}

	// /= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	operator /=( Vector4< U > const & v )
	{
		assert( size() == 4u );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		assert( v.w != T( 0 ) );
		operator ()( 1 ) /= v.x;
		operator ()( 2 ) /= v.y;
		operator ()( 3 ) /= v.z;
		operator ()( 4 ) /= v.w;
		return *this;
	}

public: // Assignment: Logical

	// &&= MArray1 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
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
	MArray1 &
	and_equals( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) = operator ()( 1 ) && v.x;
		operator ()( 2 ) = operator ()( 2 ) && v.y;
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	or_equals( Vector2< U > const & v )
	{
		assert( size() == 2u );
		operator ()( 1 ) = operator ()( 1 ) || v.x;
		operator ()( 2 ) = operator ()( 2 ) || v.y;
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	and_equals( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) = operator ()( 1 ) && v.x;
		operator ()( 2 ) = operator ()( 2 ) && v.y;
		operator ()( 3 ) = operator ()( 3 ) && v.z;
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	or_equals( Vector3< U > const & v )
	{
		assert( size() == 3u );
		operator ()( 1 ) = operator ()( 1 ) || v.x;
		operator ()( 2 ) = operator ()( 2 ) || v.y;
		operator ()( 3 ) = operator ()( 3 ) || v.z;
		return *this;
	}

	// &&= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	and_equals( Vector4< U > const & v )
	{
		assert( size() == 4u );
		operator ()( 1 ) = operator ()( 1 ) && v.x;
		operator ()( 2 ) = operator ()( 2 ) && v.y;
		operator ()( 3 ) = operator ()( 3 ) && v.z;
		operator ()( 4 ) = operator ()( 4 ) && v.w;
		return *this;
	}

	// ||= Vector4 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	MArray1 &
	or_equals( Vector4< U > const & v )
	{
		assert( size() == 4u );
		operator ()( 1 ) = operator ()( 1 ) || v.x;
		operator ()( 2 ) = operator ()( 2 ) || v.y;
		operator ()( 3 ) = operator ()( 3 ) || v.z;
		operator ()( 4 ) = operator ()( 4 ) || v.w;
		return *this;
	}

public: // Assignment: Value

	// = Value
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
	MArray1 &
	operator =( U const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) = t;
		}
		return *this;
	}

	// += Value
	MArray1 &
	operator +=( T const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) += t;
		}
		return *this;
	}

	// -= Value
	MArray1 &
	operator -=( T const & t )
	{
		for ( int i = 1, e = u(); i <= e; ++i ) {
			operator ()( i ) -= t;
		}
		return *this;
	}

	// *= Value
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
	template< typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
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
	T const &
	operator ()( int const i ) const
	{
		assert( contains( i ) );
		return array_( j1( i ) ).*pmem_;
	}

	// array( i )
	T &
	operator ()( int const i )
	{
		assert( contains( i ) );
		return array_( j1( i ) ).*pmem_;
	}

	// array[ i ] const: 0-Based Subscript
	T const &
	operator []( size_type const i ) const
	{
		assert( contains( i + 1 ) );
		return array_( j1( i + 1 ) ).*pmem_;
	}

	// array[ i ]: 0-Based Subscript
	T &
	operator []( size_type const i )
	{
		assert( contains( i + 1 ) );
		return array_( j1( i + 1 ) ).*pmem_;
	}

public: // Predicate

	// Contains Indexed Element?
	bool
	contains( int const i ) const
	{
		return in_range( u(), i );
	}

	// Conformable?
	template< typename Aa, typename Ta >
	bool
	conformable( MArray1< Aa, Ta > const & a ) const
	{
		return ( size() == a.size() );
	}

	// Conformable?
	template< class Ar >
	bool
	conformable( Ar const & a ) const
	{
		return ( ( a.rank() == 1 ) && ( size() == a.size() ) );
	}

	// Equal Dimensions?
	template< typename Aa, typename Ta >
	bool
	equal_dimensions( MArray1< Aa, Ta > const & a ) const
	{
		return conformable( a );
	}

	// Equal Dimensions?
	template< class Ar >
	bool
	equal_dimensions( Ar const & a ) const
	{
		return conformable( a );
	}

public: // Inspector

	// IndexRange
	IR
	I() const
	{
		return IR( 1, u() );
	}

	// Lower Index
	int
	l() const
	{
		return 1;
	}

	// Upper Index
	int
	u() const
	{
		return array_.isize1();
	}

	// IndexRange of Dimension 1
	IR
	I1() const
	{
		return IR( 1, u1() );
	}

	// Lower Index of Dimension 1
	int
	l1() const
	{
		return 1;
	}

	// Upper Index of Dimension 1
	int
	u1() const
	{
		return array_.isize1();
	}

	// Size of Dimension 1
	size_type
	size1() const
	{
		return array_.size1();
	}

	// Size of Dimension 1
	int
	isize1() const
	{
		return array_.isize1();
	}

	// Length
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

	// Normalize to Unit Length
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
	MArray1< MArray1 const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray1< MArray1 const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	MArray1< MArray1, M >
	ma( M ClassT::* pmem )
	{
		return MArray1< MArray1, M >( *this, pmem );
	}

public: // Comparison: Predicate

	// MArray1 == MArray1
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
	friend
	bool
	ne( MArray1 const & a, MArray1 const & b )
	{
		return ! eq( a, b );
	}

	// MArray1 < MArray1
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
	friend
	bool
	gt( MArray1 const & a, MArray1 const & b )
	{
		return lt( b, a );
	}

	// MArray1 >= MArray1
	friend
	bool
	ge( MArray1 const & a, MArray1 const & b )
	{
		return le( b, a );
	}

	// MArray1 == Value
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
	friend
	bool
	ne( MArray1 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// MArray1 < Value
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
	friend
	bool
	gt( MArray1 const & a, T const & t )
	{
		return lt( t, a );
	}

	// MArray1 >= Value
	friend
	bool
	ge( MArray1 const & a, T const & t )
	{
		return le( t, a );
	}

	// Value == MArray1
	friend
	bool
	eq( T const & t, MArray1 const & a )
	{
		return eq( a, t );
	}

	// Value != MArray1
	friend
	bool
	ne( T const & t, MArray1 const & a )
	{
		return ! eq( a, t );
	}

	// Value < MArray1
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
	friend
	bool
	gt( T const & t, MArray1 const & a )
	{
		return lt( a, t );
	}

	// Value >= MArray1
	friend
	bool
	ge( T const & t, MArray1 const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any MArray1 == MArray1
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
	friend
	bool
	any_ne( MArray1 const & a, MArray1 const & b )
	{
		return ! eq( a, b );
	}

	// Any MArray1 < MArray1
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
	friend
	bool
	any_gt( MArray1 const & a, MArray1 const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray1 >= MArray1
	friend
	bool
	any_ge( MArray1 const & a, MArray1 const & b )
	{
		return any_le( b, a );
	}

	// Any MArray1 == Value
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
	friend
	bool
	any_ne( MArray1 const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any MArray1 < Value
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
	friend
	bool
	any_gt( MArray1 const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any MArray1 >= Value
	friend
	bool
	any_ge( MArray1 const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value == MArray1
	friend
	bool
	any_eq( T const & t, MArray1 const & a )
	{
		return any_eq( a, t );
	}

	// Any Value != MArray1
	friend
	bool
	any_ne( T const & t, MArray1 const & a )
	{
		return ! eq( a, t );
	}

	// Any Value < MArray1
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
	friend
	bool
	any_gt( T const & t, MArray1 const & a )
	{
		return any_lt( a, t );
	}

	// Any Value >= MArray1
	friend
	bool
	any_ge( T const & t, MArray1 const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All MArray1 == MArray1
	friend
	bool
	all_eq( MArray1 const & a, MArray1 const & b )
	{
		return eq( a, b );
	}

	// All MArray1 != MArray1
	friend
	bool
	all_ne( MArray1 const & a, MArray1 const & b )
	{
		return ! any_eq( a, b );
	}

	// All MArray1 < MArray1
	friend
	bool
	all_lt( MArray1 const & a, MArray1 const & b )
	{
		return lt( a, b );
	}

	// All MArray1 <= MArray1
	friend
	bool
	all_le( MArray1 const & a, MArray1 const & b )
	{
		return le( a, b );
	}

	// All MArray1 > MArray1
	friend
	bool
	all_gt( MArray1 const & a, MArray1 const & b )
	{
		return gt( a, b );
	}

	// All MArray1 >= MArray1
	friend
	bool
	all_ge( MArray1 const & a, MArray1 const & b )
	{
		return ge( a, b );
	}

	// All MArray1 == Value
	friend
	bool
	all_eq( MArray1 const & a, T const & t )
	{
		return eq( a, t );
	}

	// All MArray1 != Value
	friend
	bool
	all_ne( MArray1 const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All MArray1 < Value
	friend
	bool
	all_lt( MArray1 const & a, T const & t )
	{
		return lt( a, t );
	}

	// All MArray1 <= Value
	friend
	bool
	all_le( MArray1 const & a, T const & t )
	{
		return le( a, t );
	}

	// All MArray1 > Value
	friend
	bool
	all_gt( MArray1 const & a, T const & t )
	{
		return gt( a, t );
	}

	// All MArray1 >= Value
	friend
	bool
	all_ge( MArray1 const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value == MArray1
	friend
	bool
	all_eq( T const & t, MArray1 const & a )
	{
		return eq( t, a );
	}

	// All Value != MArray1
	friend
	bool
	all_ne( T const & t, MArray1 const & a )
	{
		return ! any_eq( t, a );
	}

	// All Value < MArray1
	friend
	bool
	all_lt( T const & t, MArray1 const & a )
	{
		return lt( t, a );
	}

	// All Value <= MArray1
	friend
	bool
	all_le( T const & t, MArray1 const & a )
	{
		return le( t, a );
	}

	// All Value > MArray1
	friend
	bool
	all_gt( T const & t, MArray1 const & a )
	{
		return gt( t, a );
	}

	// All Value >= MArray1
	friend
	bool
	all_ge( T const & t, MArray1 const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count MArray1 == MArray1
	friend
	size_type
	count_eq( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 != MArray1
	friend
	size_type
	count_ne( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) != b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 < MArray1
	friend
	size_type
	count_lt( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 <= MArray1
	friend
	size_type
	count_le( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 > MArray1
	friend
	size_type
	count_gt( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) > b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 >= MArray1
	friend
	size_type
	count_ge( MArray1 const & a, MArray1 const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size();
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 == Value
	friend
	size_type
	count_eq( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == t ) ++n;
		}
		return n;
	}

	// Count Value == MArray1
	friend
	size_type
	count_eq( T const & t, MArray1 const & a )
	{
		return count_eq( a, t );
	}

	// Count MArray1 != Value
	friend
	size_type
	count_ne( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) != t ) ++n;
		}
		return n;
	}

	// Count Value != MArray1
	friend
	size_type
	count_ne( T const & t, MArray1 const & a )
	{
		return count_ne( a, t );
	}

	// Count MArray1 < Value
	friend
	size_type
	count_lt( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < t ) ++n;
		}
		return n;
	}

	// Count Value < MArray1
	friend
	size_type
	count_lt( T const & t, MArray1 const & a )
	{
		return count_gt( a, t );
	}

	// Count MArray1 <= Value
	friend
	size_type
	count_le( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= t ) ++n;
		}
		return n;
	}

	// Count Value <= MArray1
	friend
	size_type
	count_le( T const & t, MArray1 const & a )
	{
		return count_ge( a, t );
	}

	// Count MArray1 > Value
	friend
	size_type
	count_gt( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) > t ) ++n;
		}
		return n;
	}

	// Count Value > MArray1
	friend
	size_type
	count_gt( T const & t, MArray1 const & a )
	{
		return count_lt( a, t );
	}

	// Count MArray1 >= Value
	friend
	size_type
	count_ge( MArray1 const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0u );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= t ) ++n;
		}
		return n;
	}

	// Count Value >= MArray1
	friend
	size_type
	count_ge( T const & t, MArray1 const & a )
	{
		return count_le( a, t );
	}

}; // MArray1

// Functions

// Make an MArray1
template< class A, typename T >
inline
MArray1< A, T >
make_MArray1( A & a, T A::value_type::* pmem )
{
	return MArray1< A, T >( a, pmem );
}

// Make an MArray1
template< class A, typename T >
inline
MArray1< A, T >
MA1( A & a, T A::value_type::* pmem )
{
	return MArray1< A, T >( a, pmem );
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

// Distance
template< class A, typename T >
inline
T
distance( MArray1< A, T > const & a, Vector2< T > const & b )
{
	assert( a.size() == 2u );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance
template< class A, typename T >
inline
T
distance( Vector2< T > const & a, MArray1< A, T > const & b )
{
	return distance( b, a );
}

// Distance
template< class A, typename T >
inline
T
distance( MArray1< A, T > const & a, Vector3< T > const & b )
{
	assert( a.size() == 3u );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance
template< class A, typename T >
inline
T
distance( Vector3< T > const & a, MArray1< A, T > const & b )
{
	return distance( b, a );
}

// Distance
template< class A, typename T >
inline
T
distance( MArray1< A, T > const & a, Vector4< T > const & b )
{
	assert( a.size() == 4u );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return std::sqrt( distance_sq );
}

// Distance
template< class A, typename T >
inline
T
distance( Vector4< T > const & a, MArray1< A, T > const & b )
{
	return distance( b, a );
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

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( MArray1< A, T > const & a, Vector2< T > const & b )
{
	assert( a.size() == 2u );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( Vector2< T > const & a, MArray1< A, T > const & b )
{
	return distance_squared( b, a );
}

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( MArray1< A, T > const & a, Vector3< T > const & b )
{
	assert( a.size() == 3u );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( Vector3< T > const & a, MArray1< A, T > const & b )
{
	return distance_squared( b, a );
}

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( MArray1< A, T > const & a, Vector4< T > const & b )
{
	assert( a.size() == 4u );
	T distance_sq( T( 0 ) );
	for ( int i = a.l(), j = b.l(), e = a.u(); i <= e; ++i, ++j ) {
		T const distance_i( a( i ) - b( j ) );
		distance_sq += distance_i * distance_i;
	}
	return distance_sq;
}

// Distance Squared
template< class A, typename T >
inline
T
distance_squared( Vector4< T > const & a, MArray1< A, T > const & b )
{
	return distance_squared( b, a );
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

// Dot Product with Vector2
template< class A, typename T >
inline
T
dot( MArray1< A, T > const & a, Vector2< T > const & b )
{
	assert( a.size() == 2u );
	T result( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		result += a( i ) * b( i );
	}
	return result;
}

// Dot Product with Vector2
template< class A, typename T >
inline
T
dot( Vector2< T > const & a, MArray1< A, T > const & b )
{
	return dot( b, a );
}

// Dot Product with Vector3
template< class A, typename T >
inline
T
dot( MArray1< A, T > const & a, Vector3< T > const & b )
{
	assert( a.size() == 3u );
	T result( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		result += a( i ) * b( i );
	}
	return result;
}

// Dot Product with Vector3
template< class A, typename T >
inline
T
dot( Vector3< T > const & a, MArray1< A, T > const & b )
{
	return dot( b, a );
}

// Dot Product with Vector4
template< class A, typename T >
inline
T
dot( MArray1< A, T > const & a, Vector4< T > const & b )
{
	assert( a.size() == 4u );
	T result( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		result += a( i ) * b( i );
	}
	return result;
}

// Dot Product with Vector4
template< class A, typename T >
inline
T
dot( Vector4< T > const & a, MArray1< A, T > const & b )
{
	return dot( b, a );
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

// Cross Product of 2-Tuples
template< class A, typename T >
inline
T
cross2( MArray1< A, T > const & a, MArray1< A, T > const & b )
{
	assert( conformable( a, b ) );
	assert( a.size() == 2u );
	return ( a[ 0 ] * b[ 1 ] ) - ( a[ 1 ] * b[ 0 ] );
}

// Cross Product of 2-Tuples
template< class A, typename T >
inline
T
cross2( MArray1< A, T > const & a, Vector2< T > const & b )
{
	assert( a.size() == 2u );
	return ( a[ 0 ] * b.y ) - ( a[ 1 ] * b.x );
}

// Cross Product of 2-Tuples
template< class A, typename T >
inline
T
cross2( Vector2< T > const & a, MArray1< A, T > const & b )
{
	return cross2( b, a );
}

// Stream >> MArray1
template< class A, typename T >
inline
std::istream &
operator >>( std::istream & stream, MArray1< A, T > & a )
{
	if ( stream && ( a.size() > 0u ) ) {
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			stream >> a( i );
			if ( ! stream ) break;
		}
	}
	return stream;
}

// Stream << MArray1
template< class A, typename T >
inline
std::ostream &
operator <<( std::ostream & stream, MArray1< A, T > const & a )
{
	typedef  TypeTraits< T >  Traits;
	if ( stream && ( a.size() > 0u ) ) {
		std::ios_base::fmtflags const old_flags( stream.flags() );
		std::streamsize const old_precision( stream.precision( Traits::precision ) );
		stream << std::right << std::showpoint << std::uppercase;
		int const w( Traits::iwidth );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			stream << std::setw( w ) << a( i ) << ' ';
			if ( ! stream ) break;
		}
		stream.precision( old_precision );
		stream.flags( old_flags );
	}
	return stream;
}

// Read an MArray1 from a Binary File
template< class A, typename T >
inline
std::istream &
read_binary( std::istream & stream, MArray1< A, T > & a )
{
	std::size_t const n( a.size() );
	if ( stream && ( n > 0u ) ) {
		std::size_t const type_size( sizeof( T ) / sizeof( std::istream::char_type ) );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			stream.read( ( std::istream::char_type * )&a( i ), type_size );
			if ( ! stream ) break;
		}
	}
	return stream;
}

// Write an MArray1 to a Binary File
template< class A, typename T >
inline
std::ostream &
write_binary( std::ostream & stream, MArray1< A, T > const & a )
{
	std::size_t const n( a.size() );
	if ( stream && ( n > 0u ) ) {
		std::size_t const type_size( sizeof( T ) / sizeof( std::ostream::char_type ) );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			stream.write( ( std::ostream::char_type const * )&a( i ), type_size );
			if ( ! stream ) break;
		}
	}
	return stream;
}

namespace fmt {

// List-Directed Format: MArray1
template< class A, typename T >
inline
std::string
LD( MArray1< A, T > const & a )
{
	std::string s;
	std::size_t const n( a.size() );
	if ( n > 0u ) {
		s.reserve( n * TypeTraits< T >::width );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			s.append( fmt::LD( a( i ) ) );
		}
	}
	return s;
}

} // fmt

} // ObjexxFCL

#endif // ObjexxFCL_MArray1_hh_INCLUDED
