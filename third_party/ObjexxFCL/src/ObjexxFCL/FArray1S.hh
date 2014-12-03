#ifndef ObjexxFCL_FArray1S_hh_INCLUDED
#define ObjexxFCL_FArray1S_hh_INCLUDED

// FArray1S: 1D Slice Array Proxy
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
#include <ObjexxFCL/FArray1S.fwd.hh>
#include <ObjexxFCL/FArrayRS.hh>
#include <ObjexxFCL/MArray1.hh>
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>

// C++ Headers
#include <array>
#include <vector>

namespace ObjexxFCL {

// FArray1S: 1D Slice Array Proxy
template< typename T >
class FArray1S : public FArrayRS< T, 1 >
{

private: // Types

	typedef  FArrayRS< T, 1 >  Super;

private: // Friend

	template< typename > friend class FArray1S;

public: // Types

	typedef  typename Super::Base  Base;
	typedef  typename Super::Traits  Traits;
	typedef  typename Super::IR  IR;
	typedef  typename Super::IS  IS;
	typedef  typename Super::DS  DS;

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
	using Super::overlap;
	using Super::size;
	using Super::slice_k;
	using Super::data_;
	using Super::data_beg_;
	using Super::data_end_;
	using Super::size_;

public: // Creation

	// Default Constructor
	inline
	FArray1S() :
	 m_( 1 ),
	 k_( 0 ),
	 u_( 0 )
	{}

	// Copy Constructor
	inline
	FArray1S( FArray1S const & a ) :
	 Super( a ),
	 m_( a.m_ ),
	 k_( a.k_ ),
	 u_( a.u_ )
	{
		data_set();
	}

	// Data Constructor
	inline
	FArray1S( T const * data, std::int64_t const k, DS const & d ) :
	 Super( data, d.z() ),
	 m_( d.m() ),
	 k_( k + d.k() ),
	 u_( d.u() )
	{
		data_set();
	}

	// Non-Const Data Constructor
	inline
	FArray1S( T * data, std::int64_t const k, DS const & d ) :
	 Super( data, d.z() ),
	 m_( d.m() ),
	 k_( k + d.k() ),
	 u_( d.u() )
	{
		data_set();
	}

	// Array Constructor
	template< template< typename > class Array >
	inline
	FArray1S( Array< T > const & a ) :
	 Super( a.data(), a.size() ),
	 m_( 1 ),
	 k_( -m_ ),
	 u_( a.isize() )
	{
		data_set();
	}

	// Destructor
	inline
	virtual
	~FArray1S()
	{}

public: // Assignment: Array

	// Copy Assignment
	inline
	FArray1S &
	operator =( FArray1S const & a )
	{
		if ( this != &a ) {
			assert( conformable( a ) );
			if ( overlap( a ) ) { // Overlap-safe
				CArray< T > c( size_ );
				for ( int i = 1; i <= u_; ++i ) {
					c( i ) = a( i );
				}
				for ( int i = 1; i <= u_; ++i ) {
					operator ()( i ) = c( i );
				}
			} else { // Not overlap-safe
				for ( int i = 1; i <= u_; ++i ) {
					operator ()( i ) = a( i );
				}
			}
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( FArray1S< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) = a( i );
		}
		return *this;
	}

	// MArray Assignment Template
	template< class A, typename M >
	inline
	FArray1S &
	operator =( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) = a( i );
			}
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class ArrayType >
	inline
	FArray1S &
	operator =( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				c( i ) = a( j );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) = c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				operator ()( i ) = a( j );
			}
		}
		return *this;
	}

	// Array Assignment Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
			operator ()( i ) = a( j );
		}
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = *r;
		}
		return *this;
	}

	// std::array Assignment Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = *r;
		}
		return *this;
	}

	// std::vector Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = *r;
		}
		return *this;
	}

	// Vector2 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator ()( 1 ) = v[ 0 ];
		operator ()( 2 ) = v[ 1 ];
		return *this;
	}

	// Vector3 Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator =( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator ()( 1 ) = v[ 0 ];
		operator ()( 2 ) = v[ 1 ];
		operator ()( 3 ) = v[ 2 ];
		return *this;
	}

	// += MArray Template
	template< class A, typename M >
	inline
	FArray1S &
	operator +=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) += a( i );
			}
		}
		return *this;
	}

	// -= MArray Template
	template< class A, typename M >
	inline
	FArray1S &
	operator -=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) -= a( i );
			}
		}
		return *this;
	}

	// *= MArray Template
	template< class A, typename M >
	inline
	FArray1S &
	operator *=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) *= a( i );
			}
		}
		return *this;
	}

	// /= MArray Template
	template< class A, typename M >
	inline
	FArray1S &
	operator /=( MArray1< A, M > const & a )
	{
		assert( conformable( a ) );
		if ( a.dimensions_initialized() ) {
			for ( int i = 1; i <= u_; ++i ) {
				assert( T( a( i ) ) != T( 0 ) );
				operator ()( i ) /= a( i );
			}
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class ArrayType >
	inline
	FArray1S &
	operator +=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				c( i ) = a( j );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) += c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				operator ()( i ) += a( j );
			}
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class ArrayType >
	inline
	FArray1S &
	operator -=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				c( i ) = a( j );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) -= c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				operator ()( i ) -= a( j );
			}
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class ArrayType >
	inline
	FArray1S &
	operator *=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				c( i ) = a( j );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) *= c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				operator ()( i ) *= a( j );
			}
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class ArrayType >
	inline
	FArray1S &
	operator /=( ArrayType< T > const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				assert( a( j ) != T( 0 ) );
				c( i ) = a( j );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) /= c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
				assert( a( j ) != T( 0 ) );
				operator ()( i ) /= a( j );
			}
		}
		return *this;
	}

	// += Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator +=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
			operator ()( i ) += a( j );
		}
		return *this;
	}

	// -= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator -=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
			operator ()( i ) -= a( j );
		}
		return *this;
	}

	// *= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator *=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
			operator ()( i ) *= a( j );
		}
		return *this;
	}

	// /= Array Template
	template< template< typename > class ArrayType, typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( ArrayType< U > const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1, j = a.l(); i <= u_; ++i, ++j ) {
			assert( T( a( j ) ) != T( 0 ) );
			operator ()( i ) /= a( j );
		}
		return *this;
	}

	// += Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator +=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) += *r;
		}
		return *this;
	}

	// -= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator -=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) -= *r;
		}
		return *this;
	}

	// *= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator *=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) *= *r;
		}
		return *this;
	}

	// /= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			assert( *r != T( 0 ) );
			operator ()( i ) /= *r;
		}
		return *this;
	}

	// += std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator +=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) += *r;
		}
		return *this;
	}

	// -= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator -=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) -= *r;
		}
		return *this;
	}

	// *= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator *=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) *= *r;
		}
		return *this;
	}

	// /= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			assert( *r != T( 0 ) );
			operator ()( i ) /= *r;
		}
		return *this;
	}

	// += std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator +=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) += *r;
		}
		return *this;
	}

	// -= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator -=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) -= *r;
		}
		return *this;
	}

	// *= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator *=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) *= *r;
		}
		return *this;
	}

	// /= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			assert( *r != T( 0 ) );
			operator ()( i ) /= *r;
		}
		return *this;
	}

	// += Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator +=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator ()( 1 ) += v[ 0 ];
		operator ()( 2 ) += v[ 1 ];
		return *this;
	}

	// -= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator -=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator ()( 1 ) -= v[ 0 ];
		operator ()( 2 ) -= v[ 1 ];
		return *this;
	}

	// *= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator *=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator ()( 1 ) *= v[ 0 ];
		operator ()( 2 ) *= v[ 1 ];
		return *this;
	}

	// /= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		assert( v[ 0 ] != T( 0 ) );
		assert( v[ 1 ] != T( 0 ) );
		operator ()( 1 ) /= v[ 0 ];
		operator ()( 2 ) /= v[ 1 ];
		return *this;
	}

	// += Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator +=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator ()( 1 ) += v[ 0 ];
		operator ()( 2 ) += v[ 1 ];
		operator ()( 3 ) += v[ 2 ];
		return *this;
	}

	// -= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator -=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator ()( 1 ) -= v[ 0 ];
		operator ()( 2 ) -= v[ 1 ];
		operator ()( 3 ) -= v[ 2 ];
		return *this;
	}

	// *= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator *=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator ()( 1 ) *= v[ 0 ];
		operator ()( 2 ) *= v[ 1 ];
		operator ()( 3 ) *= v[ 2 ];
		return *this;
	}

	// /= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		assert( v[ 0 ] != T( 0 ) );
		assert( v[ 1 ] != T( 0 ) );
		assert( v[ 2 ] != T( 0 ) );
		operator ()( 1 ) /= v[ 0 ];
		operator ()( 2 ) /= v[ 1 ];
		operator ()( 3 ) /= v[ 2 ];
		return *this;
	}

public: // Assignment: Logical

	// &&= Array
	inline
	FArray1S &
	and_equals( FArray1S const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1; i <= u_; ++i ) {
				c( i ) = a( i );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) = operator ()( i ) && c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) = operator ()( i ) && a( i );
			}
		}
		return *this;
	}

	// ||= Array
	inline
	FArray1S &
	or_equals( FArray1S const & a )
	{
		assert( conformable( a ) );
		if ( overlap( a ) ) { // Overlap-safe
			CArray< T > c( size_ );
			for ( int i = 1; i <= u_; ++i ) {
				c( i ) = a( i );
			}
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) = operator ()( i ) || c( i );
			}
		} else { // Not overlap-safe
			for ( int i = 1; i <= u_; ++i ) {
				operator ()( i ) = operator ()( i ) || a( i );
			}
		}
		return *this;
	}

	// &&= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	and_equals( FArray1S const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) = operator ()( i ) && a( i );
		}
		return *this;
	}

	// ||= Array Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	or_equals( FArray1S const & a )
	{
		assert( conformable( a ) );
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) = operator ()( i ) || a( i );
		}
		return *this;
	}

	// &&= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	and_equals( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) && *r;
		}
		return *this;
	}

	// ||= Initializer List Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	or_equals( std::initializer_list< U > const l )
	{
		assert( size_ == l.size() );
		auto r( l.begin() );
		for ( size_type i = 0; i < size_; ++i, ++r ) {
			data_[ i ] = data_[ i ] || *r;
		}
		return *this;
	}

	// &&= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	and_equals( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) && *r;
		}
		return *this;
	}

	// ||= std::array Template
	template< typename U, Size s, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	or_equals( std::array< U, s > const & a )
	{
		assert( size_ == s );
		auto r( a.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) || *r;
		}
		return *this;
	}

	// &&= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	and_equals( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) && *r;
		}
		return *this;
	}

	// ||= std::vector Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	or_equals( std::vector< U > const & v )
	{
		assert( size_ == v.size() );
		auto r( v.begin() );
		for ( int i = 1; i <= u_; ++i, ++r ) {
			operator ()( i ) = operator ()( i ) || *r;
		}
		return *this;
	}

	// &&= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	and_equals( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator ()( 1 ) = operator ()( 1 ) && v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) && v[ 1 ];
		return *this;
	}

	// ||= Vector2 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	or_equals( Vector2< U > const & v )
	{
		assert( size_ == 2u );
		operator ()( 1 ) = operator ()( 1 ) || v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) || v[ 1 ];
		return *this;
	}

	// &&= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	and_equals( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator ()( 1 ) = operator ()( 1 ) && v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) && v[ 1 ];
		operator ()( 3 ) = operator ()( 3 ) && v[ 2 ];
		return *this;
	}

	// ||= Vector3 Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	or_equals( Vector3< U > const & v )
	{
		assert( size_ == 3u );
		operator ()( 1 ) = operator ()( 1 ) || v[ 0 ];
		operator ()( 2 ) = operator ()( 2 ) || v[ 1 ];
		operator ()( 3 ) = operator ()( 3 ) || v[ 2 ];
		return *this;
	}

public: // Assignment: Value

	// = Value
	inline
	FArray1S &
	operator =( T const & t )
	{
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) = t;
		}
		return *this;
	}

	// += Value
	inline
	FArray1S &
	operator +=( T const & t )
	{
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) += t;
		}
		return *this;
	}

	// -= Value
	inline
	FArray1S &
	operator -=( T const & t )
	{
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) -= t;
		}
		return *this;
	}

	// *= Value
	inline
	FArray1S &
	operator *=( T const & t )
	{
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) *= t;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	inline
	FArray1S &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		for ( int i = 1; i <= u_; ++i ) {
			operator ()( i ) *= inv_u;
		}
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	inline
	FArray1S &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		for ( int i = 1; i <= u_; ++i ) {
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
		return data_[ k_ + ( m_ * i ) ];
	}

	// array( i )
	inline
	T &
	operator ()( int const i )
	{
		assert( contains( i ) );
		return data_[ k_ + ( m_ * i ) ];
	}

	// Linear Index
	inline
	size_type
	index( int const i ) const
	{
		return k_ + ( m_ * i );
	}

	// array[ i ] const: 0-Based Subscript
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( contains( i + 1 ) );
		return data_[ k_ + ( m_ * ( i + 1 ) ) ];
	}

	// array[ i ]: 0-Based Subscript
	inline
	T &
	operator []( size_type const i )
	{
		assert( contains( i + 1 ) );
		return data_[ k_ + ( m_ * ( i + 1 ) ) ];
	}

public: // Slice Proxy Generators

	// array( s ) const
	inline
	FArray1S
	operator ()( IS const & s ) const
	{
		DS const d( u_, s, m_ );
		return FArray1S( data_, k_, d );
	}

	// array( s )
	inline
	FArray1S
	operator ()( IS const & s )
	{
		DS const d( u_, s, m_ );
		return FArray1S( data_, k_, d );
	}

	// array( {s} ) const
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	FArray1S
	operator ()( std::initializer_list< U > const l ) const
	{
		IS const s( l );
		DS const d( u_, s, m_ );
		return FArray1S( data_, k_, d );
	}

	// array( {s} )
	template< typename U, class = typename std::enable_if< std::is_constructible< int, U >::value >::type >
	inline
	FArray1S
	operator ()( std::initializer_list< U > const l )
	{
		IS const s( l );
		DS const d( u_, s, m_ );
		return FArray1S( data_, k_, d );
	}

public: // Predicate

	// Contains Indexed Element?
	inline
	bool
	contains( int const i ) const
	{
		return in_range( u_, i );
	}

	// Conformable?
	template< typename U >
	inline
	bool
	conformable( FArray1S< U > const & a ) const
	{
		return ( u_ == a.u_ );
	}

	// Conformable?
	template< class A, typename M >
	inline
	bool
	conformable( MArray1< A, M > const & a ) const
	{
		return ( size_ == a.size() );
	}

	// Conformable?
	template< class ArrayType >
	inline
	bool
	conformable( ArrayType const & a ) const
	{
		return ( ( a.rank() == 1 ) && ( size_ == a.size() ) );
	}

	// Equal Dimensions?
	template< typename U >
	inline
	bool
	equal_dimensions( FArray1S< U > const & a ) const
	{
		return conformable( a );
	}

	// Equal Dimensions?
	template< class A, typename M >
	inline
	bool
	equal_dimensions( MArray1< A, M > const & a ) const
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

	// IndexRange of a Dimension
	inline
	IR
	I( int const d ) const
	{
		switch ( d ) {
		case 1:
			return I1();
		default:
			assert( false );
			return I1();
		}
	}

	// Upper Index of a Dimension
	inline
	int
	u( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u_;
		default:
			assert( false );
			return u_;
		}
	}

	// Size of a Dimension
	inline
	size_type
	size( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u_;
		default:
			assert( false );
			return u_;
		}
	}

	// Size of a Dimension
	inline
	int
	isize( int const d ) const
	{
		switch ( d ) {
		case 1:
			return u_;
		default:
			assert( false );
			return u_;
		}
	}

	// IndexRange
	inline
	IR
	I() const
	{
		return IR( 1, u_ );
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
		return u_;
	}

	// IndexRange of Dimension 1
	inline
	IR
	I1() const
	{
		return IR( 1, u_ );
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
		return u_;
	}

	// Size of Dimension 1
	inline
	size_type
	size1() const
	{
		return u_;
	}

	// Size of Dimension 1
	inline
	int
	isize1() const
	{
		return u_;
	}

public: // MArray Generators

	// Template Helpers
	template< typename U > class Wrapper {};
	typedef  typename std::conditional< std::is_class< T >::value, T, Wrapper< T > >::type  ClassT;

	// MArray Generator
	template< typename M >
	inline
	MArray1< FArray1S const, M >
	ma( M ClassT::* pmem ) const
	{
		return MArray1< FArray1S const, M >( *this, pmem );
	}

	// MArray Generator
	template< typename M >
	inline
	MArray1< FArray1S, M >
	ma( M ClassT::* pmem )
	{
		return MArray1< FArray1S, M >( *this, pmem );
	}

#include <ObjexxFCL/FArray1S.Project.MArray.hh> // Inject project-specific MArray generators

public: // Comparison: Predicate

	// Slice == Slice
	inline
	friend
	bool
	eq( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) == b( i ) ) ) return false;
		}
		return true;
	}

	// Slice != Slice
	inline
	friend
	bool
	ne( FArray1S const & a, FArray1S const & b )
	{
		return ! eq( a, b );
	}

	// Slice < Slice
	inline
	friend
	bool
	lt( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) < b( i ) ) ) return false;
		}
		return true;
	}

	// Slice <= Slice
	inline
	friend
	bool
	le( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( ( &a == &b ) || a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) <= b( i ) ) ) return false;
		}
		return true;
	}

	// Slice > Slice
	inline
	friend
	bool
	gt( FArray1S const & a, FArray1S const & b )
	{
		return lt( b, a );
	}

	// Slice >= Slice
	inline
	friend
	bool
	ge( FArray1S const & a, FArray1S const & b )
	{
		return le( b, a );
	}

	// Slice == Value
	inline
	friend
	bool
	eq( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) == t ) ) return false;
		}
		return true;
	}

	// Slice != Value
	inline
	friend
	bool
	ne( FArray1S const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Slice < Value
	inline
	friend
	bool
	lt( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) < t ) ) return false;
		}
		return true;
	}

	// Slice <= Value
	inline
	friend
	bool
	le( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( a( i ) <= t ) ) return false;
		}
		return true;
	}

	// Slice > Value
	inline
	friend
	bool
	gt( FArray1S const & a, T const & t )
	{
		return lt( t, a );
	}

	// Slice >= Value
	inline
	friend
	bool
	ge( FArray1S const & a, T const & t )
	{
		return le( t, a );
	}

	// Value == Slice
	inline
	friend
	bool
	eq( T const & t, FArray1S const & a )
	{
		return eq( a, t );
	}

	// Value != Slice
	inline
	friend
	bool
	ne( T const & t, FArray1S const & a )
	{
		return ! eq( a, t );
	}

	// Value < Slice
	inline
	friend
	bool
	lt( T const & t, FArray1S const & a )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( t < a( i ) ) ) return false;
		}
		return true;
	}

	// Value <= Slice
	inline
	friend
	bool
	le( T const & t, FArray1S const & a )
	{
		if ( a.empty() ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( ! ( t <= a( i ) ) ) return false;
		}
		return true;
	}

	// Value > Slice
	inline
	friend
	bool
	gt( T const & t, FArray1S const & a )
	{
		return lt( a, t );
	}

	// Value >= Slice
	inline
	friend
	bool
	ge( T const & t, FArray1S const & a )
	{
		return le( a, t );
	}

public: // Comparison: Predicate: Any

	// Any Slice == Slice
	inline
	friend
	bool
	any_eq( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) return true;
		}
		return false;
	}

	// Any Slice != Slice
	inline
	friend
	bool
	any_ne( FArray1S const & a, FArray1S const & b )
	{
		return ! eq( a, b );
	}

	// Any Slice < Slice
	inline
	friend
	bool
	any_lt( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < b( i ) ) return true;
		}
		return false;
	}

	// Any Slice <= Slice
	inline
	friend
	bool
	any_le( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		if ( &a == &b ) return true;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) return true;
		}
		return false;
	}

	// Any Slice > Slice
	inline
	friend
	bool
	any_gt( FArray1S const & a, FArray1S const & b )
	{
		return any_lt( b, a );
	}

	// Any Slice >= Slice
	inline
	friend
	bool
	any_ge( FArray1S const & a, FArray1S const & b )
	{
		return any_le( b, a );
	}

	// Any Slice == Value
	inline
	friend
	bool
	any_eq( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == t ) return true;
		}
		return false;
	}

	// Any Slice != Value
	inline
	friend
	bool
	any_ne( FArray1S const & a, T const & t )
	{
		return ! eq( a, t );
	}

	// Any Slice < Value
	inline
	friend
	bool
	any_lt( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < t ) return true;
		}
		return false;
	}

	// Any Slice <= Value
	inline
	friend
	bool
	any_le( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= t ) return true;
		}
		return false;
	}

	// Any Slice > Value
	inline
	friend
	bool
	any_gt( FArray1S const & a, T const & t )
	{
		return any_lt( t, a );
	}

	// Any Slice >= Value
	inline
	friend
	bool
	any_ge( FArray1S const & a, T const & t )
	{
		return any_le( t, a );
	}

	// Any Value == Slice
	inline
	friend
	bool
	any_eq( T const & t, FArray1S const & a )
	{
		return any_eq( a, t );
	}

	// Any Value != Slice
	inline
	friend
	bool
	any_ne( T const & t, FArray1S const & a )
	{
		return ! eq( a, t );
	}

	// Any Value < Slice
	inline
	friend
	bool
	any_lt( T const & t, FArray1S const & a )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( t < a( i ) ) return true;
		}
		return false;
	}

	// Any Value <= Slice
	inline
	friend
	bool
	any_le( T const & t, FArray1S const & a )
	{
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( t <= a( i ) ) return true;
		}
		return false;
	}

	// Any Value > Slice
	inline
	friend
	bool
	any_gt( T const & t, FArray1S const & a )
	{
		return any_lt( a, t );
	}

	// Any Value >= Slice
	inline
	friend
	bool
	any_ge( T const & t, FArray1S const & a )
	{
		return any_le( a, t );
	}

public: // Comparison: Predicate: All

	// All Slice == Slice
	inline
	friend
	bool
	all_eq( FArray1S const & a, FArray1S const & b )
	{
		return eq( a, b );
	}

	// All Slice != Slice
	inline
	friend
	bool
	all_ne( FArray1S const & a, FArray1S const & b )
	{
		return ! any_eq( a, b );
	}

	// All Slice < Slice
	inline
	friend
	bool
	all_lt( FArray1S const & a, FArray1S const & b )
	{
		return lt( a, b );
	}

	// All Slice <= Slice
	inline
	friend
	bool
	all_le( FArray1S const & a, FArray1S const & b )
	{
		return le( a, b );
	}

	// All Slice > Slice
	inline
	friend
	bool
	all_gt( FArray1S const & a, FArray1S const & b )
	{
		return gt( a, b );
	}

	// All Slice >= Slice
	inline
	friend
	bool
	all_ge( FArray1S const & a, FArray1S const & b )
	{
		return ge( a, b );
	}

	// All Slice == Value
	inline
	friend
	bool
	all_eq( FArray1S const & a, T const & t )
	{
		return eq( a, t );
	}

	// All Slice != Value
	inline
	friend
	bool
	all_ne( FArray1S const & a, T const & t )
	{
		return ! any_eq( a, t );
	}

	// All Slice < Value
	inline
	friend
	bool
	all_lt( FArray1S const & a, T const & t )
	{
		return lt( a, t );
	}

	// All Slice <= Value
	inline
	friend
	bool
	all_le( FArray1S const & a, T const & t )
	{
		return le( a, t );
	}

	// All Slice > Value
	inline
	friend
	bool
	all_gt( FArray1S const & a, T const & t )
	{
		return gt( a, t );
	}

	// All Slice >= Value
	inline
	friend
	bool
	all_ge( FArray1S const & a, T const & t )
	{
		return ge( a, t );
	}

	// All Value == Slice
	inline
	friend
	bool
	all_eq( T const & t, FArray1S const & a )
	{
		return eq( t, a );
	}

	// All Value != Slice
	inline
	friend
	bool
	all_ne( T const & t, FArray1S const & a )
	{
		return ! any_eq( t, a );
	}

	// All Value < Slice
	inline
	friend
	bool
	all_lt( T const & t, FArray1S const & a )
	{
		return lt( t, a );
	}

	// All Value <= Slice
	inline
	friend
	bool
	all_le( T const & t, FArray1S const & a )
	{
		return le( t, a );
	}

	// All Value > Slice
	inline
	friend
	bool
	all_gt( T const & t, FArray1S const & a )
	{
		return gt( t, a );
	}

	// All Value >= Slice
	inline
	friend
	bool
	all_ge( T const & t, FArray1S const & a )
	{
		return ge( t, a );
	}

public: // Comparison: Count

	// Count Slice == Slice
	inline
	friend
	size_type
	count_eq( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) ++n;
		}
		return n;
	}

	// Count Slice != Slice
	inline
	friend
	size_type
	count_ne( FArray1S const & a, FArray1S const & b )
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

	// Count Slice < Slice
	inline
	friend
	size_type
	count_lt( FArray1S const & a, FArray1S const & b )
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

	// Count Slice <= Slice
	inline
	friend
	size_type
	count_le( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) ++n;
		}
		return n;
	}

	// Count Slice > Slice
	inline
	friend
	size_type
	count_gt( FArray1S const & a, FArray1S const & b )
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

	// Count Slice >= Slice
	inline
	friend
	size_type
	count_ge( FArray1S const & a, FArray1S const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		if ( &a == &b ) return a.size_;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= b( i ) ) ++n;
		}
		return n;
	}

	// Count Slice == Value
	inline
	friend
	size_type
	count_eq( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == t ) ++n;
		}
		return n;
	}

	// Count Value == Slice
	inline
	friend
	size_type
	count_eq( T const & t, FArray1S const & a )
	{
		return count_eq( a, t );
	}

	// Count Slice != Value
	inline
	friend
	size_type
	count_ne( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) != t ) ++n;
		}
		return n;
	}

	// Count Value != Slice
	inline
	friend
	size_type
	count_ne( T const & t, FArray1S const & a )
	{
		return count_ne( a, t );
	}

	// Count Slice < Value
	inline
	friend
	size_type
	count_lt( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < t ) ++n;
		}
		return n;
	}

	// Count Value < Slice
	inline
	friend
	size_type
	count_lt( T const & t, FArray1S const & a )
	{
		return count_gt( a, t );
	}

	// Count Slice <= Value
	inline
	friend
	size_type
	count_le( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= t ) ++n;
		}
		return n;
	}

	// Count Value <= Slice
	inline
	friend
	size_type
	count_le( T const & t, FArray1S const & a )
	{
		return count_ge( a, t );
	}

	// Count Slice > Value
	inline
	friend
	size_type
	count_gt( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) > t ) ++n;
		}
		return n;
	}

	// Count Value > Slice
	inline
	friend
	size_type
	count_gt( T const & t, FArray1S const & a )
	{
		return count_lt( a, t );
	}

	// Count Slice >= Value
	inline
	friend
	size_type
	count_ge( FArray1S const & a, T const & t )
	{
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= t ) ++n;
		}
		return n;
	}

	// Count Value >= Slice
	inline
	friend
	size_type
	count_ge( T const & t, FArray1S const & a )
	{
		return count_le( a, t );
	}

public: // Comparison: Predicate: MArray

	// FArray1S == MArray1
	template< class A >
	inline
	friend
	bool
	eq( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( ! ( a( i ) == b( i ) ) ) return false;
		}
		return true;
	}

	// FArray1S != MArray1
	template< class A >
	inline
	friend
	bool
	ne( FArray1S const & a, MArray1< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// FArray1S < MArray1
	template< class A >
	inline
	friend
	bool
	lt( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( ! ( a( i ) < b( i ) ) ) return false;
		}
		return true;
	}

	// FArray1S <= MArray1
	template< class A >
	inline
	friend
	bool
	le( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( ! ( a( i ) <= b( i ) ) ) return false;
		}
		return true;
	}

	// FArray1S > MArray1
	template< class A >
	inline
	friend
	bool
	gt( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( ! ( a( i ) > b( i ) ) ) return false;
		}
		return true;
	}

	// FArray1S >= MArray1
	template< class A >
	inline
	friend
	bool
	ge( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return true;
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( ! ( a( i ) >= b( i ) ) ) return false;
		}
		return true;
	}

	// MArray1 == FArray1S
	template< class A >
	inline
	friend
	bool
	eq( MArray1< A, T > const & a, FArray1S const & b )
	{
		return eq( b, a );
	}

	// MArray1 != FArray1S
	template< class A >
	inline
	friend
	bool
	ne( MArray1< A, T > const & a, FArray1S const & b )
	{
		return ne( b, a );
	}

	// MArray1 < FArray1S
	template< class A >
	inline
	friend
	bool
	lt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return gt( b, a );
	}

	// MArray1 <= FArray1S
	template< class A >
	inline
	friend
	bool
	le( MArray1< A, T > const & a, FArray1S const & b )
	{
		return ge( b, a );
	}

	// MArray1 > FArray1S
	template< class A >
	inline
	friend
	bool
	gt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return lt( b, a );
	}

	// MArray1 >= FArray1S
	template< class A >
	inline
	friend
	bool
	ge( MArray1< A, T > const & a, FArray1S const & b )
	{
		return le( b, a );
	}

public: // Comparison: Predicate: Any: MArray

	// Any FArray1S == MArray1
	template< class A >
	inline
	friend
	bool
	any_eq( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) return true;
		}
		return false;
	}

	// Any FArray1S != MArray1
	template< class A >
	inline
	friend
	bool
	any_ne( FArray1S const & a, MArray1< A, T > const & b )
	{
		return ! eq( a, b );
	}

	// Any FArray1S < MArray1
	template< class A >
	inline
	friend
	bool
	any_lt( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) < b( i ) ) return true;
		}
		return false;
	}

	// Any FArray1S <= MArray1
	template< class A >
	inline
	friend
	bool
	any_le( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) return true;
		}
		return false;
	}

	// Any FArray1S > MArray1
	template< class A >
	inline
	friend
	bool
	any_gt( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) > b( i ) ) return true;
		}
		return false;
	}

	// Any FArray1S >= MArray1
	template< class A >
	inline
	friend
	bool
	any_ge( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return false;
		for ( int i = 1, e = a.u(); i <= e; ++i ) {
			if ( a( i ) >= b( i ) ) return true;
		}
		return false;
	}

	// Any MArray1 == FArray1S
	template< class A >
	inline
	friend
	bool
	any_eq( MArray1< A, T > const & a, FArray1S const & b )
	{
		return any_eq( b, a );
	}

	// Any MArray1 != FArray1S
	template< class A >
	inline
	friend
	bool
	any_ne( MArray1< A, T > const & a, FArray1S const & b )
	{
		return any_ne( b, a );
	}

	// Any MArray1 < FArray1S
	template< class A >
	inline
	friend
	bool
	any_lt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return any_gt( b, a );
	}

	// Any MArray1 <= FArray1S
	template< class A >
	inline
	friend
	bool
	any_le( MArray1< A, T > const & a, FArray1S const & b )
	{
		return any_ge( b, a );
	}

	// Any MArray1 > FArray1S
	template< class A >
	inline
	friend
	bool
	any_gt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return any_lt( b, a );
	}

	// Any MArray1 >= FArray1S
	template< class A >
	inline
	friend
	bool
	any_ge( MArray1< A, T > const & a, FArray1S const & b )
	{
		return any_le( b, a );
	}

public: // Comparison: Predicate: All: MArray

	// All FArray1S == MArray1
	template< class A >
	inline
	friend
	bool
	all_eq( FArray1S const & a, MArray1< A, T > const & b )
	{
		return eq( a, b );
	}

	// All FArray1S != MArray1
	template< class A >
	inline
	friend
	bool
	all_ne( FArray1S const & a, MArray1< A, T > const & b )
	{
		return ! any_eq( a, b );
	}

	// All FArray1S < MArray1
	template< class A >
	inline
	friend
	bool
	all_lt( FArray1S const & a, MArray1< A, T > const & b )
	{
		return lt( a, b );
	}

	// All FArray1S <= MArray1
	template< class A >
	inline
	friend
	bool
	all_le( FArray1S const & a, MArray1< A, T > const & b )
	{
		return le( a, b );
	}

	// All FArray1S > MArray1
	template< class A >
	inline
	friend
	bool
	all_gt( FArray1S const & a, MArray1< A, T > const & b )
	{
		return gt( a, b );
	}

	// All FArray1S >= MArray1
	template< class A >
	inline
	friend
	bool
	all_ge( FArray1S const & a, MArray1< A, T > const & b )
	{
		return ge( a, b );
	}

	// All MArray1 == FArray1S
	template< class A >
	inline
	friend
	bool
	all_eq( MArray1< A, T > const & a, FArray1S const & b )
	{
		return all_eq( b, a );
	}

	// All MArray1 != FArray1S
	template< class A >
	inline
	friend
	bool
	all_ne( MArray1< A, T > const & a, FArray1S const & b )
	{
		return all_ne( b, a );
	}

	// All MArray1 < FArray1S
	template< class A >
	inline
	friend
	bool
	all_lt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return all_gt( b, a );
	}

	// All MArray1 <= FArray1S
	template< class A >
	inline
	friend
	bool
	all_le( MArray1< A, T > const & a, FArray1S const & b )
	{
		return all_ge( b, a );
	}

	// All MArray1 > FArray1S
	template< class A >
	inline
	friend
	bool
	all_gt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return all_lt( b, a );
	}

	// All MArray1 >= FArray1S
	template< class A >
	inline
	friend
	bool
	all_ge( MArray1< A, T > const & a, FArray1S const & b )
	{
		return all_le( b, a );
	}

public: // Comparison: Count: MArray

	// Count FArray1S == MArray1
	template< class A >
	inline
	friend
	size_type
	count_eq( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( a( i ) == b( i ) ) ++n;
		}
		return n;
	}

	// Count FArray1S != MArray1
	template< class A >
	inline
	friend
	size_type
	count_ne( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( a( i ) != b( i ) ) ++n;
		}
		return n;
	}

	// Count FArray1S < MArray1
	template< class A >
	inline
	friend
	size_type
	count_lt( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( a( i ) < b( i ) ) ++n;
		}
		return n;
	}

	// Count FArray1S <= MArray1
	template< class A >
	inline
	friend
	size_type
	count_le( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( a( i ) <= b( i ) ) ++n;
		}
		return n;
	}

	// Count FArray1S > MArray1
	template< class A >
	inline
	friend
	size_type
	count_gt( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( a( i ) > b( i ) ) ++n;
		}
		return n;
	}

	// Count FArray1S >= MArray1
	template< class A >
	inline
	friend
	size_type
	count_ge( FArray1S const & a, MArray1< A, T > const & b )
	{
		assert( a.conformable( b ) );
		if ( a.empty() ) return 0;
		size_type n( 0 );
		for ( int i = 1, e = b.u(); i <= e; ++i ) {
			if ( a( i ) >= b( i ) ) ++n;
		}
		return n;
	}

	// Count MArray1 == FArray1S
	template< class A >
	inline
	friend
	size_type
	count_eq( MArray1< A, T > const & a, FArray1S const & b )
	{
		return count_eq( b, a );
	}

	// Count MArray1 != FArray1S
	template< class A >
	inline
	friend
	size_type
	count_ne( MArray1< A, T > const & a, FArray1S const & b )
	{
		return count_ne( b, a );
	}

	// Count MArray1 < FArray1S
	template< class A >
	inline
	friend
	size_type
	count_lt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return count_gt( b, a );
	}

	// Count MArray1 <= FArray1S
	template< class A >
	inline
	friend
	size_type
	count_le( MArray1< A, T > const & a, FArray1S const & b )
	{
		return count_ge( b, a );
	}

	// Count MArray1 > FArray1S
	template< class A >
	inline
	friend
	size_type
	count_gt( MArray1< A, T > const & a, FArray1S const & b )
	{
		return count_lt( b, a );
	}

	// Count MArray1 >= FArray1S
	template< class A >
	inline
	friend
	size_type
	count_ge( MArray1< A, T > const & a, FArray1S const & b )
	{
		return count_le( b, a );
	}

private: // Methods

	// Memory Range Set
	inline
	void
	data_set()
	{
		if ( size_ > 0u ) { // Non-empty slice
			data_beg_ = data_end_ = data_ + k_;
			data_beg_ += m_ * ( m_ >= 0 ? 1 : u_ );
			data_end_ += m_ * ( m_ <= 0 ? 1 : u_ );
		} else {
			data_ = data_beg_ = data_end_ = nullptr;
		}
	}

private: // Data

	std::int64_t m_; // Multiplier
	std::int64_t k_; // Constant
	int u_; // Upper index

}; // FArray1S

// Conformable?
template< typename U, typename V >
inline
bool
conformable( FArray1S< U > const & a, FArray1S< V > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< typename U, class A, typename M >
inline
bool
conformable( FArray1S< U > const & a, MArray1< A, M > const & b )
{
	return a.conformable( b );
}

// Conformable?
template< class A, typename M, typename V >
inline
bool
conformable( MArray1< A, M > const & a, FArray1S< V > const & b )
{
	return b.conformable( a );
}

// Magnitude
template< typename T >
inline
T
magnitude( FArray1S< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		T const mag_i( a( i ) );
		mag_sq += mag_i * mag_i;
	}
	return std::sqrt( mag_sq );
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( FArray1S< T > const & a )
{
	T mag_sq( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		T const mag_i( a( i ) );
		mag_sq += mag_i * mag_i;
	}
	return mag_sq;
}

// Distance
template< typename T >
inline
T
distance( FArray1S< T > const & a, FArray1S< T > const & b )
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
template< typename T >
inline
T
distance_squared( FArray1S< T > const & a, FArray1S< T > const & b )
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
template< typename T >
inline
T
dot( FArray1S< T > const & a, FArray1S< T > const & b )
{
	assert( a.size() == b.size() );
	T result( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		result += a( i ) * b( i );
	}
	return result;
}

// Dot Product
template< class A, typename T >
inline
T
dot( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	assert( a.size() == b.size() );
	T result( T( 0 ) );
	for ( int i = 1, e = a.u(); i <= e; ++i ) {
		result += a( i ) * b( i );
	}
	return result;
}

// Dot Product
template< class A, typename T >
inline
T
dot( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays
inline
bool
dot( FArray1S< bool > const & a, FArray1S< bool > const & b )
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

// Dot Product of Boolean Arrays
template< class A >
inline
bool
dot( FArray1S< bool > const & a, MArray1< A, bool > const & b )
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

// Dot Product of Boolean Arrays
template< class A >
inline
bool
dot( MArray1< A, bool > const & a, FArray1S< bool > const & b )
{
	return dot( b, a );
}

// Dot Product (Fortran Intrinsic Name)
template< typename T >
inline
T
dot_product( FArray1S< T > const & a, FArray1S< T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( FArray1S< T > const & a, MArray1< A, T > const & b )
{
	return dot( a, b );
}

// Dot Product (Fortran Intrinsic Name)
template< class A, typename T >
inline
T
dot_product( MArray1< A, T > const & a, FArray1S< T > const & b )
{
	return dot( b, a );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
inline
bool
dot_product( FArray1S< bool > const & a, FArray1S< bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( FArray1S< bool > const & a, MArray1< A, bool > const & b )
{
	return dot( a, b );
}

// Dot Product of Boolean Arrays (Fortran Intrinsic Name)
template< class A >
inline
bool
dot_product( MArray1< A, bool > const & a, FArray1S< bool > const & b )
{
	return dot( b, a );
}

} // ObjexxFCL

#endif // ObjexxFCL_FArray1S_hh_INCLUDED
