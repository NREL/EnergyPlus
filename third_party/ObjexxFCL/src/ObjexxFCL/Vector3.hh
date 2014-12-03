#ifndef ObjexxFCL_Vector3_hh_INCLUDED
#define ObjexxFCL_Vector3_hh_INCLUDED

// Vector3: Fast Three-Element Vector
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
#include <ObjexxFCL/Vector3.fwd.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdlib>
#include <initializer_list>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <type_traits>

namespace ObjexxFCL {

// Vector3: Fast Three-Element Vector
template< typename T >
class Vector3
{

private: // Friends

	template< typename > friend class Vector3;

public: // Types

	// STL Style
	typedef  T  value_type;
	typedef  T &  reference;
	typedef  T const &  const_reference;
	typedef  T *  pointer;
	typedef  T const *  const_pointer;
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;

	// C++ Style
	typedef  T  Value;
	typedef  T &  Reference;
	typedef  T const &  ConstReference;
	typedef  T *  Pointer;
	typedef  T const *  ConstPointer;
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

public: // Creation

	// Default Constructor
	inline
	Vector3() :
	 x( T() ),
	 y( T() ),
	 z( T() )
	{}

	// Copy Constructor
	inline
	Vector3( Vector3 const & v ) :
	 x( v.x ),
	 y( v.y ),
	 z( v.z )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Vector3( Vector3< U > const & v ) :
	 x( v.x ),
	 y( v.y ),
	 z( v.z )
	{}

	// Uniform Value Constructor
	inline
	explicit
	Vector3( T const & t ) :
	 x( t ),
	 y( t ),
	 z( t )
	{}

	// Value Constructor
	inline
	Vector3(
	 T const & x_,
	 T const & y_,
	 T const & z_
	) :
	 x( x_ ),
	 y( y_ ),
	 z( z_ )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	inline
	Vector3( std::initializer_list< U > const l ) :
	 x( *l.begin() ),
	 y( *( l.begin() + 1 ) ),
	 z( *( l.begin() + 2 ) )
	{
		assert( l.size() == 3 );
	}

	// Array Constructor Template
	template< typename A, class = typename std::enable_if< std::is_constructible< T, typename A::value_type >::value >::type >
	inline
	Vector3( A const & a ) :
	 x( a[ 0 ] ),
	 y( a[ 1 ] ),
	 z( a[ 2 ] )
	{
		assert( a.size() == 3 );
	}

	// Default Vector Named Constructor
	inline
	static
	Vector3
	default_vector()
	{
		return Vector3( T() );
	}

	// Zero Vector Named Constructor
	inline
	static
	Vector3
	zero_vector()
	{
		return Vector3( T( 0 ) );
	}

	// x Vector of Specified Length Named Constructor
	inline
	static
	Vector3
	x_vector( T const & tar_length = T( 1 ) )
	{
		return Vector3( tar_length, T( 0 ), T( 0 ) );
	}

	// y Vector of Specified Length Named Constructor
	inline
	static
	Vector3
	y_vector( T const & tar_length = T( 1 ) )
	{
		return Vector3( T( 0 ), tar_length, T( 0 ) );
	}

	// z Vector of Specified Length Named Constructor
	inline
	static
	Vector3
	z_vector( T const & tar_length = T( 1 ) )
	{
		return Vector3( T( 0 ), T( 0 ), tar_length );
	}

	// Uniform Vector of Specified Length Named Constructor
	inline
	static
	Vector3
	uniform_vector( T const & tar_length = T( 1 ) )
	{
		return Vector3( std::sqrt( ( tar_length * tar_length ) / T( 3 ) ) );
	}

	// Destructor
	inline
	~Vector3()
	{}

public: // Assignment

	// Copy Assignment
	inline
	Vector3 &
	operator =( Vector3 const & v )
	{
		if ( this != &v ) {
			x = v.x;
			y = v.y;
			z = v.z;
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator =( Vector3< U > const & v )
	{
		x = v.x;
		y = v.y;
		z = v.z;
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator =( std::initializer_list< U > const l )
	{
		assert( l.size() == 3 );
		auto i( l.begin() );
		x = *i;
		y = *(++i);
		z = *(++i);
		return *this;
	}

	// Array Assignment Template
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	Vector3 &
	operator =( A const & a )
	{
		assert( a.size() == 3 );
		x = a[ 0 ];
		y = a[ 1 ];
		z = a[ 2 ];
		return *this;
	}

	// += Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator +=( Vector3< U > const & v )
	{
		x += v.x;
		y += v.y;
		z += v.z;
		return *this;
	}

	// -= Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator -=( Vector3< U > const & v )
	{
		x -= v.x;
		y -= v.y;
		z -= v.z;
		return *this;
	}

	// *= Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator *=( Vector3< U > const & v )
	{
		x *= v.x;
		y *= v.y;
		z *= v.z;
		return *this;
	}

	// /= Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator /=( Vector3< U > const & v )
	{
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		x /= v.x;
		y /= v.y;
		z /= v.z;
		return *this;
	}

	// += Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator +=( std::initializer_list< U > const l )
	{
		assert( l.size() == 3 );
		auto i( l.begin() );
		x += *i;
		y += *(++i);
		z += *(++i);
		return *this;
	}

	// -= Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator -=( std::initializer_list< U > const l )
	{
		assert( l.size() == 3 );
		auto i( l.begin() );
		x -= *i;
		y -= *(++i);
		z -= *(++i);
		return *this;
	}

	// *= Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator *=( std::initializer_list< U > const l )
	{
		assert( l.size() == 3 );
		auto i( l.begin() );
		x *= *i;
		y *= *(++i);
		z *= *(++i);
		return *this;
	}

	// /= Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	operator /=( std::initializer_list< U > const l )
	{
		assert( l.size() == 3 );
		auto i( l.begin() );
		assert( *i != T( 0 ) );
		assert( *(i+1) != T( 0 ) );
		assert( *(i+2) != T( 0 ) );
		x /= *i;
		y /= *(++i);
		z /= *(++i);
		return *this;
	}

	// += Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	Vector3 &
	operator +=( A const & a )
	{
		assert( a.size() == 3 );
		x += a[ 0 ];
		y += a[ 1 ];
		z += a[ 2 ];
		return *this;
	}

	// -= Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	Vector3 &
	operator -=( A const & a )
	{
		assert( a.size() == 3 );
		x -= a[ 0 ];
		y -= a[ 1 ];
		z -= a[ 2 ];
		return *this;
	}

	// *= Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	Vector3 &
	operator *=( A const & a )
	{
		assert( a.size() == 3 );
		x *= a[ 0 ];
		y *= a[ 1 ];
		z *= a[ 2 ];
		return *this;
	}

	// /= Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	Vector3 &
	operator /=( A const & a )
	{
		assert( a.size() == 3 );
		assert( a[ 0 ] != T( 0 ) );
		assert( a[ 1 ] != T( 0 ) );
		assert( a[ 2 ] != T( 0 ) );
		x /= a[ 0 ];
		y /= a[ 1 ];
		z /= a[ 2 ];
		return *this;
	}

	// = Value
	inline
	Vector3 &
	operator =( T const & t )
	{
		x = y = z = t;
		return *this;
	}

	// += Value
	inline
	Vector3 &
	operator +=( T const & t )
	{
		x += t;
		y += t;
		z += t;
		return *this;
	}

	// -= Value
	inline
	Vector3 &
	operator -=( T const & t )
	{
		x -= t;
		y -= t;
		z -= t;
		return *this;
	}

	// *= Value
	inline
	Vector3 &
	operator *=( T const & t )
	{
		x *= t;
		y *= t;
		z *= t;
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	inline
	Vector3 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		x *= inv_u;
		y *= inv_u;
		z *= inv_u;
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void, typename = void >
	inline
	Vector3 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		x /= u;
		y /= u;
		z /= u;
		return *this;
	}

	// Value Assignment
	inline
	Vector3 &
	assign(
	 T const & x_,
	 T const & y_,
	 T const & z_
	)
	{
		x = x_;
		y = y_;
		z = z_;
		return *this;
	}

public: // Assignment: Scaled

	// Assign Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	scaled_assign( T const & t, Vector3< U > const & v )
	{
		x = t * v.x;
		y = t * v.y;
		z = t * v.z;
		return *this;
	}

	// Add Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	scaled_add( T const & t, Vector3< U > const & v )
	{
		x += t * v.x;
		y += t * v.y;
		z += t * v.z;
		return *this;
	}

	// Subtract Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	scaled_sub( T const & t, Vector3< U > const & v )
	{
		x -= t * v.x;
		y -= t * v.y;
		z -= t * v.z;
		return *this;
	}

	// Multiply by Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	scaled_mul( T const & t, Vector3< U > const & v )
	{
		x *= t * v.x;
		y *= t * v.y;
		z *= t * v.z;
		return *this;
	}

	// Divide by Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	inline
	Vector3 &
	scaled_div( T const & t, Vector3< U > const & v )
	{
		assert( t != T( 0 ) );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		x /= t * v.x;
		y /= t * v.y;
		z /= t * v.z;
		return *this;
	}

public: // Subscript

	// Vector3[ i ] const: 0-Based Index
	inline
	T const &
	operator []( size_type const i ) const
	{
		assert( i <= 2 );
		return ( i == 0 ? x : ( i == 1 ? y : z ) );
	}

	// Vector3[ i ]: 0-Based Index
	inline
	T &
	operator []( size_type const i )
	{
		assert( i <= 2 );
		return ( i == 0 ? x : ( i == 1 ? y : z ) );
	}

	// Vector3( i ) const: 1-Based Index
	inline
	T const &
	operator ()( size_type const i ) const
	{
		assert( ( 1 <= i ) && ( i <= 3 ) );
		return ( i == 1 ? x : ( i == 2 ? y : z ) );
	}

	// Vector3( i ): 1-Based Index
	inline
	T &
	operator ()( size_type const i )
	{
		assert( ( 1 <= i ) && ( i <= 3 ) );
		return ( i == 1 ? x : ( i == 2 ? y : z ) );
	}

public: // Properties: Predicates

	// Is Zero Vector?
	inline
	bool
	is_zero() const
	{
		static T const ZERO( 0 );
		return ( x == ZERO ) && ( y == ZERO ) && ( z == ZERO );
	}

	// Is Unit Vector?
	inline
	bool
	is_unit() const
	{
		return ( length_squared() == T( 1 ) );
	}

public: // Properties: Comparison

	// Equal Length?
	inline
	bool
	equal_length( Vector3 const & v )
	{
		return ( length_squared() == v.length_squared() );
	}

	// Longer?
	inline
	bool
	longer( Vector3 const & v )
	{
		return ( length_squared() > v.length_squared() );
	}

	// Longer or Equal Length?
	inline
	bool
	longer_or_equal( Vector3 const & v )
	{
		return ( length_squared() >= v.length_squared() );
	}

	// Shorter?
	inline
	bool
	shorter( Vector3 const & v )
	{
		return ( length_squared() < v.length_squared() );
	}

	// Shorter or Equal Length?
	inline
	bool
	shorter_or_equal( Vector3 const & v )
	{
		return ( length_squared() <= v.length_squared() );
	}

public: // Properties: General

	// Size
	inline
	Size
	size() const
	{
		return 3u;
	}

	// Length
	inline
	T
	length() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// Length Squared
	inline
	T
	length_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z );
	}

	// Magnitude
	inline
	T
	magnitude() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// Magnitude Squared
	inline
	T
	magnitude_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z );
	}

	// L1 Norm
	inline
	T
	norm_L1() const
	{
		return std::abs( x ) + std::abs( y ) + std::abs( z );
	}

	// L2 Norm
	inline
	T
	norm_L2() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// L-infinity Norm
	inline
	T
	norm_Linf() const
	{
		return max3( std::abs( x ), std::abs( y ), std::abs( z ) );
	}

	// Distance
	inline
	T
	distance( Vector3 const & v ) const
	{
		return std::sqrt( square( x - v.x ) + square( y - v.y ) + square( z - v.z ) );
	}

	// Distance Squared
	inline
	T
	distance_squared( Vector3 const & v ) const
	{
		return square( x - v.x ) + square( y - v.y ) + square( z - v.z );
	}

	// Dot Product
	inline
	T
	dot( Vector3 const & v ) const
	{
		return ( x * v.x ) + ( y * v.y ) + ( z * v.z );
	}

	// Dot Product
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	T
	dot( A const & a ) const
	{
		assert( a.size() == 3 );
		return ( x * a[ 0 ] ) + ( y * a[ 1 ] ) + ( z * a[ 2 ] );
	}

	// Cross Product
	inline
	Vector3
	cross( Vector3 const & v ) const
	{
		return Vector3(
		 ( y * v.z ) - ( z * v.y ),
		 ( z * v.x ) - ( x * v.z ),
		 ( x * v.y ) - ( y * v.x )
		);
	}

	// Cross Product
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	inline
	Vector3
	cross( A const & a ) const
	{
		assert( a.size() == 3 );
		return Vector3(
		 ( y * a[ 2 ] ) - ( z * a[ 1 ] ),
		 ( z * a[ 0 ] ) - ( x * a[ 2 ] ),
		 ( x * a[ 1 ] ) - ( y * a[ 0 ] )
		);
	}

	// Alias for Element 1
	inline
	T const &
	x1() const
	{
		return x;
	}

	// Alias for Element 1
	inline
	T &
	x1()
	{
		return x;
	}

	// Alias for Element 2
	inline
	T const &
	x2() const
	{
		return y;
	}

	// Alias for Element 2
	inline
	T &
	x2()
	{
		return y;
	}

	// Alias for Element 3
	inline
	T const &
	x3() const
	{
		return z;
	}

	// Alias for Element 3
	inline
	T &
	x3()
	{
		return z;
	}

public: // Modifiers

	// Zero
	inline
	Vector3 &
	zero()
	{
		x = y = z = T( 0 );
		return *this;
	}

	// Negate
	inline
	Vector3 &
	negate()
	{
		x = -x;
		y = -y;
		z = -z;
		return *this;
	}

	// Normalize to a Length
	inline
	Vector3 &
	normalize( T const & tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		assert( cur_length != T ( 0 ) );
		T const dilation( tar_length / cur_length );
		x *= dilation;
		y *= dilation;
		z *= dilation;
		return *this;
	}

	// Normalize to a Length: Zero Vector3 if Length is Zero
	inline
	Vector3 &
	normalize_zero( T const & tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
		} else { // Set zero vector
			x = y = z = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: x Vector3 if Length is Zero
	inline
	Vector3 &
	normalize_x( T const & tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
		} else { // Set x vector
			x = tar_length;
			y = z = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: y Vector3 if Length is Zero
	inline
	Vector3 &
	normalize_y( T const & tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
		} else { // Set y vector
			y = tar_length;
			x = z = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: z Vector3 if Length is Zero
	inline
	Vector3 &
	normalize_z( T const & tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
		} else { // Set z vector
			z = tar_length;
			x = y = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: Uniform Vector3 if Length is Zero
	inline
	Vector3 &
	normalize_uniform( T const & tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
		} else { // Set uniform vector
			operator =( uniform_vector( tar_length ) );
		}
		return *this;
	}

	// Set Minimum Coordinates wrt a Vector3
	inline
	Vector3 &
	min( Vector3 const & v )
	{
		x = ( x <= v.x ? x : v.x );
		y = ( y <= v.y ? y : v.y );
		z = ( z <= v.z ? z : v.z );
		return *this;
	}

	// Set Maximum Coordinates wrt a Vector3
	inline
	Vector3 &
	max( Vector3 const & v )
	{
		x = ( x >= v.x ? x : v.x );
		y = ( y >= v.y ? y : v.y );
		z = ( z >= v.z ? z : v.z );
		return *this;
	}

	// Sum of Vector3s
	inline
	Vector3 &
	sum( Vector3 const & a, Vector3 const & b )
	{
		x = a.x + b.x;
		y = a.y + b.y;
		z = a.z + b.z;
		return *this;
	}

	// Difference of Vector3s
	inline
	Vector3 &
	diff( Vector3 const & a, Vector3 const & b )
	{
		x = a.x - b.x;
		y = a.y - b.y;
		z = a.z - b.z;
		return *this;
	}

	// Cross Product of Vector3s
	inline
	Vector3 &
	cross( Vector3 const & a, Vector3 const & b )
	{
		x = ( a.y * b.z ) - ( a.z * b.y );
		y = ( a.z * b.x ) - ( a.x * b.z );
		z = ( a.x * b.y ) - ( a.y * b.x );
		return *this;
	}

	// Midpoint of Two Vector3s
	inline
	Vector3 &
	mid( Vector3 const & a, Vector3 const & b )
	{
		x = T( 0.5 * ( a.x + b.x ) );
		y = T( 0.5 * ( a.y + b.y ) );
		z = T( 0.5 * ( a.z + b.z ) );
		return *this;
	}

	// Center of Two Vector3s
	inline
	Vector3 &
	cen( Vector3 const & a, Vector3 const & b )
	{
		x = T( 0.5 * ( a.x + b.x ) );
		y = T( 0.5 * ( a.y + b.y ) );
		z = T( 0.5 * ( a.z + b.z ) );
		return *this;
	}

	// Center of Three Vector3s
	inline
	Vector3 &
	cen( Vector3 const & a, Vector3 const & b, Vector3 const & c )
	{
		static long double const third( 1.0 / 3.0 );
		x = T( third * ( a.x + b.x + c.x ) );
		y = T( third * ( a.y + b.y + c.y ) );
		z = T( third * ( a.z + b.z + c.z ) );
		return *this;
	}

	// Center of Four Vector3s
	inline
	Vector3 &
	cen( Vector3 const & a, Vector3 const & b, Vector3 const & c, Vector3 const & d )
	{
		x = T( 0.25 * ( a.x + b.x + c.x + d.x ) );
		y = T( 0.25 * ( a.y + b.y + c.y + d.y ) );
		z = T( 0.25 * ( a.z + b.z + c.z + d.z ) );
		return *this;
	}

	// Project Normally onto a Vector3
	inline
	Vector3 &
	project_normal( Vector3 const & v )
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		x -= c * v.x;
		y -= c * v.y;
		z -= c * v.z;
		return *this;
	}

	// Project in Direction of a Vector3
	inline
	Vector3 &
	project_parallel( Vector3 const & v )
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		x = c * v.x;
		y = c * v.y;
		z = c * v.z;
		return *this;
	}

public: // Generators

	// -Vector3 (Negated)
	inline
	Vector3
	operator -() const
	{
		return Vector3( -x, -y, -z );
	}

	// Negated
	inline
	Vector3
	negated() const
	{
		return Vector3( -x, -y, -z );
	}

	// Normalized to a Length
	inline
	Vector3
	normalized( T const & tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		assert( cur_length != T ( 0 ) );
		T const dilation( tar_length / cur_length );
		return Vector3(
		 x * dilation,
		 y * dilation,
		 z * dilation
		);
	}

	// Normalized to a Length: Zero Vector3 if Length is Zero
	inline
	Vector3
	normalized_zero( T const & tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector3(
			 x * dilation,
			 y * dilation,
			 z * dilation
			);
		} else { // Return zero vector
			return Vector3( T( 0 ) );
		}
	}

	// Normalized to a Length: x Vector3 if Length is Zero
	inline
	Vector3
	normalized_x( T const & tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector3(
			 x * dilation,
			 y * dilation,
			 z * dilation
			);
		} else { // Return x vector
			return Vector3( tar_length, T( 0 ), T( 0 ) );
		}
	}

	// Normalized to a Length: y Vector3 if Length is Zero
	inline
	Vector3
	normalized_y( T const & tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector3(
			 x * dilation,
			 y * dilation,
			 z * dilation
			);
		} else { // Return y vector
			return Vector3( T( 0 ), tar_length, T( 0 ) );
		}
	}

	// Normalized to a Length: z Vector3 if Length is Zero
	inline
	Vector3
	normalized_z( T const & tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector3(
			 x * dilation,
			 y * dilation,
			 z * dilation
			);
		} else { // Return z vector
			return Vector3( tar_length, T( 0 ), T( 0 ), tar_length );
		}
	}

	// Normalized to a Length: Uniform Vector3 if Length is Zero
	inline
	Vector3
	normalized_uniform( T const & tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector3(
			 x * dilation,
			 y * dilation,
			 z * dilation
			);
		} else { // Return uniform vector
			return uniform_vector( tar_length );
		}
	}

	// Vector3 + Vector3
	friend
	inline
	Vector3
	operator +( Vector3 const & a, Vector3 const & b )
	{
		return Vector3( a.x + b.x, a.y + b.y, a.z + b.z );
	}

	// Vector3 + Value
	friend
	inline
	Vector3
	operator +( Vector3 const & v, T const & t )
	{
		return Vector3( v.x + t, v.y + t, v.z + t );
	}

	// Value + Vector3
	friend
	inline
	Vector3
	operator +( T const & t, Vector3 const & v )
	{
		return Vector3( t + v.x, t + v.y, t + v.z );
	}

	// Vector3 - Vector3
	friend
	inline
	Vector3
	operator -( Vector3 const & a, Vector3 const & b )
	{
		return Vector3( a.x - b.x, a.y - b.y, a.z - b.z );
	}

	// Vector3 - Value
	friend
	inline
	Vector3
	operator -( Vector3 const & v, T const & t )
	{
		return Vector3( v.x - t, v.y - t, v.z - t );
	}

	// Value - Vector3
	friend
	inline
	Vector3
	operator -( T const & t, Vector3 const & v )
	{
		return Vector3( t - v.x, t - v.y, t - v.z );
	}

	// Vector3 * Vector3
	friend
	inline
	Vector3
	operator *( Vector3 const & a, Vector3 const & b )
	{
		return Vector3( a.x * b.x, a.y * b.y, a.z * b.z );
	}

	// Vector3 * Value
	friend
	inline
	Vector3
	operator *( Vector3 const & v, T const & t )
	{
		return Vector3( v.x * t, v.y * t, v.z * t );
	}

	// Value * Vector3
	friend
	inline
	Vector3
	operator *( T const & t, Vector3 const & v )
	{
		return Vector3( t * v.x, t * v.y, t * v.z );
	}

	// Vector3 / Vector3
	friend
	inline
	Vector3
	operator /( Vector3 const & a, Vector3 const & b )
	{
		assert( b.x != T( 0 ) );
		assert( b.y != T( 0 ) );
		assert( b.z != T( 0 ) );
		return Vector3( a.x / b.x, a.y / b.y, a.z / b.z );
	}

	// Vector3 / Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
	friend
	inline
	Vector3
	operator /( Vector3 const & v, U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U ( 1 ) / u );
		return Vector3( v.x * inv_u, v.y * inv_u, v.z * inv_u );
	}

	// Vector3 / Value
	template< typename U, class = typename std::enable_if< !std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	friend
	inline
	Vector3
	operator /( Vector3 const & v, U const & u )
	{
		assert( u != U( 0 ) );
		return Vector3( v.x / u, v.y / u, v.z / u );
	}

	// Value / Vector3
	friend
	inline
	Vector3
	operator /( T const & t, Vector3 const & v )
	{
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		return Vector3( t / v.x, t / v.y, t / v.z );
	}

	// Vector3 with Min Coordinates of Two Vector3s
	friend
	inline
	Vector3
	min( Vector3 const & a, Vector3 const & b )
	{
		return Vector3(
		 ( a.x <= b.x ? a.x : b.x ),
		 ( a.y <= b.y ? a.y : b.y ),
		 ( a.z <= b.z ? a.z : b.z )
		);
	}

	// Vector3 with Max Coordinates of Two Vector3s
	friend
	inline
	Vector3
	max( Vector3 const & a, Vector3 const & b )
	{
		return Vector3(
		 ( a.x >= b.x ? a.x : b.x ),
		 ( a.y >= b.y ? a.y : b.y ),
		 ( a.z >= b.z ? a.z : b.z )
		);
	}

	// Projected Normally onto a Vector3
	inline
	Vector3
	projected_normal( Vector3 const & v ) const
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		return Vector3( x - ( c * v.x ), y - ( c * v.y ), z - ( c * v.z ) );
	}

	// Projected in Direction of a Vector3
	inline
	Vector3
	projected_parallel( Vector3 const & v ) const
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		return Vector3( c * v.x, c * v.y, c * v.z );
	}

public: // Friends: Comparison

	// Vector3 == Vector3
	friend
	inline
	bool
	operator ==( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x == b.x ) && ( a.y == b.y ) && ( a.z == b.z );
	}

	// Vector3 != Vector3
	friend
	inline
	bool
	operator !=( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x != b.x ) || ( a.y != b.y ) || ( a.z != b.z );
	}

	// Vector3 < Vector3: Lexicographic
	friend
	inline
	bool
	operator <( Vector3 const & a, Vector3 const & b )
	{
		return (
		 ( a.x < b.x ? true :
		 ( b.x < a.x ? false : // a.x == b.x
		 ( a.y < b.y ? true :
		 ( b.y < a.y ? false : // a.y == b.y
		 ( a.z < b.z ) ) ) ) )
		);
	}

	// Vector3 <= Vector3: Lexicographic
	friend
	inline
	bool
	operator <=( Vector3 const & a, Vector3 const & b )
	{
		return (
		 ( a.x < b.x ? true :
		 ( b.x < a.x ? false : // a.x == b.x
		 ( a.y < b.y ? true :
		 ( b.y < a.y ? false : // a.y == b.y
		 ( a.z <= b.z ) ) ) ) )
		);
	}

	// Vector3 >= Vector3: Lexicographic
	friend
	inline
	bool
	operator >=( Vector3 const & a, Vector3 const & b )
	{
		return (
		 ( a.x > b.x ? true :
		 ( b.x > a.x ? false : // a.x == b.x
		 ( a.y > b.y ? true :
		 ( b.y > a.y ? false : // a.y == b.y
		 ( a.z >= b.z ) ) ) ) )
		);
	}

	// Vector3 > Vector3: Lexicographic
	friend
	inline
	bool
	operator >( Vector3 const & a, Vector3 const & b )
	{
		return (
		 ( a.x > b.x ? true :
		 ( b.x > a.x ? false : // a.x == b.x
		 ( a.y > b.y ? true :
		 ( b.y > a.y ? false : // a.y == b.y
		 ( a.z > b.z ) ) ) ) )
		);
	}

	// Vector3 < Vector3: Element-wise
	friend
	inline
	bool
	lt( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x < b.x ) && ( a.y < b.y ) && ( a.z < b.z );
	}

	// Vector3 <= Vector3: Element-wise
	friend
	inline
	bool
	le( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x <= b.x ) && ( a.y <= b.y ) && ( a.z <= b.z );
	}

	// Vector3 >= Vector3: Element-wise
	friend
	inline
	bool
	ge( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x >= b.x ) && ( a.y >= b.y ) && ( a.z >= b.z );
	}

	// Vector3 > Vector3: Element-wise
	friend
	inline
	bool
	gt( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x > b.x ) && ( a.y > b.y ) && ( a.z > b.z );
	}

	// Vector3 == Value
	friend
	inline
	bool
	operator ==( Vector3 const & v, T const & t )
	{
		return ( v.x == t ) && ( v.y == t ) && ( v.z == t );
	}

	// Vector3 != Value
	friend
	inline
	bool
	operator !=( Vector3 const & v, T const & t )
	{
		return ( v.x != t ) || ( v.y != t ) || ( v.z != t );
	}

	// Vector3 < Value
	friend
	inline
	bool
	operator <( Vector3 const & v, T const & t )
	{
		return ( v.x < t ) && ( v.y < t ) && ( v.z < t );
	}

	// Vector3 <= Value
	friend
	inline
	bool
	operator <=( Vector3 const & v, T const & t )
	{
		return ( v.x <= t ) && ( v.y <= t ) && ( v.z <= t );
	}

	// Vector3 >= Value
	friend
	inline
	bool
	operator >=( Vector3 const & v, T const & t )
	{
		return ( v.x >= t ) && ( v.y >= t ) && ( v.z >= t );
	}

	// Vector3 > Value
	friend
	inline
	bool
	operator >( Vector3 const & v, T const & t )
	{
		return ( v.x > t ) && ( v.y > t ) && ( v.z > t );
	}

	// Value == Vector3
	friend
	inline
	bool
	operator ==( T const & t, Vector3 const & v )
	{
		return ( t == v.x ) && ( t == v.y ) && ( t == v.z );
	}

	// Value != Vector3
	friend
	inline
	bool
	operator !=( T const & t, Vector3 const & v )
	{
		return ( t != v.x ) || ( t != v.y ) || ( t != v.z );
	}

	// Value < Vector3
	friend
	inline
	bool
	operator <( T const & t, Vector3 const & v )
	{
		return ( t < v.x ) && ( t < v.y ) && ( t < v.z );
	}

	// Value <= Vector3
	friend
	inline
	bool
	operator <=( T const & t, Vector3 const & v )
	{
		return ( t <= v.x ) && ( t <= v.y ) && ( t <= v.z );
	}

	// Value >= Vector3
	friend
	inline
	bool
	operator >=( T const & t, Vector3 const & v )
	{
		return ( t >= v.x ) && ( t >= v.y ) && ( t >= v.z );
	}

	// Value > Vector3
	friend
	inline
	bool
	operator >( T const & t, Vector3 const & v )
	{
		return ( t > v.x ) && ( t > v.y ) && ( t > v.z );
	}

	// Equal Length?
	friend
	inline
	bool
	equal_length( Vector3 const & a, Vector3 const & b )
	{
		return ( a.length_squared() == b.length_squared() );
	}

	// Not Equal Length?
	inline
	bool
	not_equal_length( Vector3 const & v )
	{
		return ( length_squared() != v.length_squared() );
	}

	// Not Equal Length?
	friend
	inline
	bool
	not_equal_length( Vector3 const & a, Vector3 const & b )
	{
		return ( a.length_squared() != b.length_squared() );
	}

public: // Friends

	// Distance
	friend
	inline
	T
	distance( Vector3 const & a, Vector3 const & b )
	{
		return std::sqrt( square( a.x - b.x ) + square( a.y - b.y ) + square( a.z - b.z ) );
	}

	// Distance Squared
	friend
	inline
	T
	distance_squared( Vector3 const & a, Vector3 const & b )
	{
		return square( a.x - b.x ) + square( a.y - b.y ) + square( a.z - b.z );
	}

	// Dot Product
	friend
	inline
	T
	dot( Vector3 const & a, Vector3 const & b )
	{
		return ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z );
	}

	// Cross Product
	friend
	inline
	Vector3
	cross( Vector3 const & a, Vector3 const & b )
	{
		return Vector3(
		 ( a.y * b.z ) - ( a.z * b.y ),
		 ( a.z * b.x ) - ( a.x * b.z ),
		 ( a.x * b.y ) - ( a.y * b.x )
		);
	}

	// Midpoint of Two Vector3s
	friend
	inline
	Vector3
	mid( Vector3 const & a, Vector3 const & b )
	{
		return Vector3(
		 T( 0.5 * ( a.x + b.x ) ),
		 T( 0.5 * ( a.y + b.y ) ),
		 T( 0.5 * ( a.z + b.z ) )
		);
	}

	// Center of Two Vector3s
	friend
	inline
	Vector3
	cen( Vector3 const & a, Vector3 const & b )
	{
		return Vector3(
		 T( 0.5 * ( a.x + b.x ) ),
		 T( 0.5 * ( a.y + b.y ) ),
		 T( 0.5 * ( a.z + b.z ) )
		);
	}

	// Center of Three Vector3s
	friend
	inline
	Vector3
	cen( Vector3 const & a, Vector3 const & b, Vector3 const & c )
	{
		static long double const third( 1.0 / 3.0 );
		return Vector3(
		 T( third * ( a.x + b.x + c.x ) ),
		 T( third * ( a.y + b.y + c.y ) ),
		 T( third * ( a.z + b.z + c.z ) )
		);
	}

	// Center of Four Vector3s
	friend
	inline
	Vector3
	cen( Vector3 const & a, Vector3 const & b, Vector3 const & c, Vector3 const & d )
	{
		return Vector3(
		 T( 0.25 * ( a.x + b.x + c.x + d.x ) ),
		 T( 0.25 * ( a.y + b.y + c.y + d.y ) ),
		 T( 0.25 * ( a.z + b.z + c.z + d.z ) )
		);
	}

	// Angle Between Two Vector3s (in Radians on [0,pi])
	friend
	inline
	T
	angle( Vector3 const & a, Vector3 const & b )
	{
		T const axb( a.cross( b ).magnitude() );
		T const adb( a.dot( b ) );
		return ( ( axb != T( 0 ) ) || ( adb != T( 0 ) ) ? bump_up_angle( std::atan2( axb, adb ) ) : T( 0 ) ); // More accurate than dot-based for angles near 0 and Pi
	}

	// Angle abc Formed by Three Vector3s (in Radians on [0,pi])
	friend
	inline
	T
	angle( Vector3 const & a, Vector3 const & b, Vector3 const & c )
	{
		return angle( a - b, c - b );
	}

	// Cosine of Angle Between Two Vector3s
	friend
	inline
	T
	cos( Vector3 const & a, Vector3 const & b )
	{
		T const mag( std::sqrt( a.length_squared() * b.length_squared() ) );
		return ( mag > T( 0 ) ? sin_cos_range( a.dot( b ) / mag ) : T( 1 ) );
	}

	// Cosine of Angle abc Formed by Three Vector3s
	friend
	inline
	T
	cos( Vector3 const & a, Vector3 const & b, Vector3 const & c )
	{
		return cos( a - b, c - b );
	}

	// Sine of Angle Between Two Vector3s
	friend
	inline
	T
	sin( Vector3 const & a, Vector3 const & b )
	{
		T const mag( std::sqrt( a.length_squared() * b.length_squared() ) );
		return ( mag > T( 0 ) ? std::abs( sin_cos_range( a.cross( b ).magnitude() / mag ) ) : T( 0 ) );
	}

	// Sine of Angle abc Formed by Three Vector3s
	friend
	inline
	T
	sin( Vector3 const & a, Vector3 const & b, Vector3 const & c )
	{
		return sin( a - b, c - b );
	}

private: // Methods

	// Square of a value
	inline
	static
	T
	square( T const & t )
	{
		return t * t;
	}

	// Value Clipped to [-1,1]
	inline
	static
	T
	sin_cos_range( T const & t )
	{
		return std::min( std::max( t, T( -1 ) ), T( 1 ) );
	}

	// Add 2*Pi to a Negative Value
	inline
	static
	T
	bump_up_angle( T const & t )
	{
		static T const Two_Pi( T( 2 ) * std::acos( -1.0 ) );
		return ( t >= T( 0 ) ? t : Two_Pi + t );
	}

	// Max of Three Values
	inline
	static
	T const &
	max3( T const & a, T const & b, T const & c )
	{
		return ( a < b ? ( b < c ? c : b ) : ( a < c ? c : a ) );
	}

public: // Data

	T x, y, z; // Elements

}; // Vector3

// stream << Vector3 output operator
template< typename T >
std::ostream &
operator <<( std::ostream & stream, Vector3< T > const & v )
{
	// Types
	typedef  TypeTraits< T >  Traits;

	// Save current stream state and set persistent state
	std::ios_base::fmtflags const old_flags( stream.flags() );
	std::streamsize const old_precision( stream.precision( Traits::precision() ) );
	stream << std::right << std::showpoint << std::uppercase;

	// Output Vector3
	std::size_t const w( Traits::width() );
	stream << std::setw( w ) << v.x << ' ' << std::setw( w ) << v.y << ' ' << std::setw( w ) << v.z;

	// Restore previous stream state
	stream.precision( old_precision );
	stream.flags( old_flags );

	return stream;
}

// stream >> Vector3 input operator
//  Supports whitespace-separated values with optional commas between values as long as whitespace is also present
//  String or char values containing whitespace or commas or enclosed in quotes are not supported
//  Vector can optionally be enclosed in parentheses () or square brackets []
template< typename T >
std::istream &
operator >>( std::istream & stream, Vector3< T > & v )
{
	bool parens( false ); // Opening ( present?
	bool brackets( false ); // Opening [ present?

	{ // x
		std::string input_string;
		stream >> input_string;
		if ( input_string == "(" ) { // Skip opening (
			stream >> input_string;
			parens = true;
		} else if ( input_string[ 0 ] == '(' ) { // Skip opening (
			input_string.erase( 0, 1 );
			brackets = true;
		} else if ( input_string == "[" ) { // Skip opening [
			stream >> input_string;
			brackets = true;
		} else if ( input_string[ 0 ] == '[' ) { // Skip opening [
			input_string.erase( 0, 1 );
			brackets = true;
		}
		std::string::size_type const input_size( input_string.size() );
		if ( ( input_size > 0 ) && ( input_string[ input_size - 1 ] == ',' ) ) {
			input_string.erase( input_size - 1 ); // Remove trailing ,
		}
		std::istringstream num_stream( input_string );
		num_stream >> v.x;
	}

	{ // y
		std::string input_string;
		stream >> input_string;
		if ( input_string == "," ) { // Skip ,
			stream >> input_string;
		} else if ( input_string[ 0 ] == ',' ) { // Skip leading ,
			input_string.erase( 0, 1 );
		}
		std::string::size_type const input_size( input_string.size() );
		if ( ( input_size > 0 ) && ( input_string[ input_size - 1 ] == ',' ) ) {
			input_string.erase( input_size - 1 ); // Remove trailing ,
		}
		std::istringstream num_stream( input_string );
		num_stream >> v.y;
	}

	{ // z
		std::string input_string;
		stream >> input_string;
		if ( input_string == "," ) { // Skip ,
			stream >> input_string;
		} else if ( input_string[ 0 ] == ',' ) { // Skip leading ,
			input_string.erase( 0, 1 );
		}
		std::string::size_type input_size( input_string.size() );
		if ( parens || brackets ) { // Remove closing ) or ]
			if ( input_size > 0 ) {
				if ( parens ) {
					if ( input_string[ input_size - 1 ] == ')' ) { // Remove closing )
						input_string.erase( input_size - 1 );
						--input_size;
					}
				} else if ( brackets ) {
					if ( input_string[ input_size - 1 ] == ']' ) { // Remove closing ]
						input_string.erase( input_size - 1 );
						--input_size;
					}
				}
			}
		}
		if ( ( input_size > 0 ) && ( input_string[ input_size - 1 ] == ',' ) ) {
			input_string.erase( input_size - 1 ); // Remove trailing ,
		}
		std::istringstream num_stream( input_string );
		num_stream >> v.z;
	}

	// Remove closing ) or ] if opening ( or [ present
	if ( parens || brackets ) { // Remove closing ) or ]
		while ( ( stream.peek() == ' ' ) || ( stream.peek() == '\t' ) ) {
			stream.ignore();
		}
		if ( parens ) { // Remove closing ) if present
			if ( stream.peek() == ')' ) stream.ignore();
		} else if ( brackets ) { // Remove closing ] if present
			if ( stream.peek() == ']' ) stream.ignore();
		}
	}

	return stream;
}

} // ObjexxFCL

#endif // ObjexxFCL_Vector3_hh_INCLUDED
