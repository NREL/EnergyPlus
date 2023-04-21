#ifndef ObjexxFCL_MArray1_hh_INCLUDED
#define ObjexxFCL_MArray1_hh_INCLUDED

// MArray1: 1D Member Array Proxy
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

	using Super::isize;
	using Super::l;
	using Super::u;
	using Super::size;

protected: // Types

	using Super::in_range;
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
	~MArray1() = default;

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
		assert( i < std::numeric_limits< int >::max() );
		assert( contains( static_cast< int >( i + 1 ) ) );
		return array_( j1( static_cast< int >( i + 1 ) ) ).*pmem_;
	}

	// array[ i ]: 0-Based Subscript
	T &
	operator []( size_type const i )
	{
		assert( i < std::numeric_limits< int >::max() );
		assert( contains( static_cast< int >( i + 1 ) ) );
		return array_( j1( static_cast< int >( i + 1 ) ) ).*pmem_;
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

}; // MArray1

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
