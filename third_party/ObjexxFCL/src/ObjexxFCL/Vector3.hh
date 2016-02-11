#ifndef ObjexxFCL_Vector3_hh_INCLUDED
#define ObjexxFCL_Vector3_hh_INCLUDED

// Vector3: Fast 3-Element Vector
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
#include <ObjexxFCL/Vector3.fwd.hh>
#include <ObjexxFCL/Fmath.hh>
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

// Vector3: Fast 3-Element Vector
// . Heap-free and loop-free for speed
// . Provides direct element access via .x style lookup
// . Use std::array< T, 3 > instead in array/vectorization context
template< typename T >
class Vector3
{

private: // Friends

	template< typename > friend class Vector3;

public: // Types

	typedef  TypeTraits< T >  Traits;
	typedef  typename std::conditional< std::is_scalar< T >::value, T const, T const & >::type  Tc;
	typedef  typename std::conditional< std::is_scalar< T >::value, typename std::remove_const< T >::type, T const & >::type  Tr;

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
	Vector3()
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
	 :
	 x( Traits::initial_array_value() ),
	 y( Traits::initial_array_value() ),
	 z( Traits::initial_array_value() )
#endif
	{}

	// Copy Constructor
	Vector3( Vector3 const & v ) :
	 x( v.x ),
	 y( v.y ),
	 z( v.z )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Vector3( Vector3< U > const & v ) :
	 x( v.x ),
	 y( v.y ),
	 z( v.z )
	{}

	// Uniform Value Constructor
	explicit
	Vector3( Tc t ) :
	 x( t ),
	 y( t ),
	 z( t )
	{}

	// Value Constructor
	Vector3(
	 Tc x_,
	 Tc y_,
	 Tc z_
	) :
	 x( x_ ),
	 y( y_ ),
	 z( z_ )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Vector3( std::initializer_list< U > const l ) :
	 x( *l.begin() ),
	 y( *( l.begin() + 1 ) ),
	 z( *( l.begin() + 2 ) )
	{
		assert( l.size() == 3 );
	}

	// Array Constructor Template
	template< typename A, class = typename std::enable_if< std::is_constructible< T, typename A::value_type >::value >::type >
	Vector3( A const & a ) :
	 x( a[ 0 ] ),
	 y( a[ 1 ] ),
	 z( a[ 2 ] )
	{
		assert( a.size() == 3 );
	}

	// Default Vector Named Constructor
	static
	Vector3
	default_vector()
	{
		return Vector3( T() );
	}

	// Zero Vector Named Constructor
	static
	Vector3
	zero_vector()
	{
		return Vector3( T( 0 ) );
	}

	// x Vector of Specified Length Named Constructor
	static
	Vector3
	x_vector( Tc tar_length = T( 1 ) )
	{
		return Vector3( tar_length, T( 0 ), T( 0 ) );
	}

	// y Vector of Specified Length Named Constructor
	static
	Vector3
	y_vector( Tc tar_length = T( 1 ) )
	{
		return Vector3( T( 0 ), tar_length, T( 0 ) );
	}

	// z Vector of Specified Length Named Constructor
	static
	Vector3
	z_vector( Tc tar_length = T( 1 ) )
	{
		return Vector3( T( 0 ), T( 0 ), tar_length );
	}

	// Uniform Vector of Specified Length Named Constructor
	static
	Vector3
	uniform_vector( Tc tar_length = T( 1 ) )
	{
		return Vector3( tar_length / std::sqrt( T( 3 ) ) );
	}

	// Destructor
	~Vector3()
	{}

public: // Assignment

	// Copy Assignment
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
	Vector3 &
	operator =( Tc t )
	{
		x = y = z = t;
		return *this;
	}

	// += Value
	Vector3 &
	operator +=( Tc t )
	{
		x += t;
		y += t;
		z += t;
		return *this;
	}

	// -= Value
	Vector3 &
	operator -=( Tc t )
	{
		x -= t;
		y -= t;
		z -= t;
		return *this;
	}

	// *= Value
	Vector3 &
	operator *=( Tc t )
	{
		x *= t;
		y *= t;
		z *= t;
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
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
	template< typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void, typename = void >
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
	Vector3 &
	assign(
	 Tc x_,
	 Tc y_,
	 Tc z_
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
	Vector3 &
	scaled_assign( Tc t, Vector3< U > const & v )
	{
		x = t * v.x;
		y = t * v.y;
		z = t * v.z;
		return *this;
	}

	// Add Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector3 &
	scaled_add( Tc t, Vector3< U > const & v )
	{
		x += t * v.x;
		y += t * v.y;
		z += t * v.z;
		return *this;
	}

	// Subtract Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector3 &
	scaled_sub( Tc t, Vector3< U > const & v )
	{
		x -= t * v.x;
		y -= t * v.y;
		z -= t * v.z;
		return *this;
	}

	// Multiply by Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector3 &
	scaled_mul( Tc t, Vector3< U > const & v )
	{
		x *= t * v.x;
		y *= t * v.y;
		z *= t * v.z;
		return *this;
	}

	// Divide by Value * Vector3
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector3 &
	scaled_div( Tc t, Vector3< U > const & v )
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
	Tr
	operator []( size_type const i ) const
	{
		assert( i <= 2 );
		return ( i == 0 ? x : ( i == 1 ? y : z ) );
	}

	// Vector3[ i ]: 0-Based Index
	T &
	operator []( size_type const i )
	{
		assert( i <= 2 );
		return ( i == 0 ? x : ( i == 1 ? y : z ) );
	}

	// Vector3( i ) const: 1-Based Index
	Tr
	operator ()( size_type const i ) const
	{
		assert( ( 1 <= i ) && ( i <= 3 ) );
		return ( i == 1 ? x : ( i == 2 ? y : z ) );
	}

	// Vector3( i ): 1-Based Index
	T &
	operator ()( size_type const i )
	{
		assert( ( 1 <= i ) && ( i <= 3 ) );
		return ( i == 1 ? x : ( i == 2 ? y : z ) );
	}

public: // Properties: Predicates

	// Is Zero Vector?
	bool
	is_zero() const
	{
		static T const ZERO( 0 );
		return ( x == ZERO ) && ( y == ZERO ) && ( z == ZERO );
	}

	// Is Unit Vector?
	bool
	is_unit() const
	{
		return ( length_squared() == T( 1 ) );
	}

public: // Properties: General

	// Size
	Size
	size() const
	{
		return 3u;
	}

	// Length
	T
	length() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// Length Squared
	T
	length_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z );
	}

	// Magnitude
	T
	magnitude() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// Magnitude
	T
	mag() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// Magnitude Squared
	T
	magnitude_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z );
	}

	// Magnitude Squared
	T
	mag_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z );
	}

	// L1 Norm
	T
	norm_L1() const
	{
		return std::abs( x ) + std::abs( y ) + std::abs( z );
	}

	// L2 Norm
	T
	norm_L2() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) );
	}

	// L-infinity Norm
	T
	norm_Linf() const
	{
		return ObjexxFCL::max( std::abs( x ), std::abs( y ), std::abs( z ) );
	}

	// Distance to a Vector3
	T
	distance( Vector3 const & v ) const
	{
		return std::sqrt( square( x - v.x ) + square( y - v.y ) + square( z - v.z ) );
	}

	// Distance Squared to a Vector3
	T
	distance_squared( Vector3 const & v ) const
	{
		return square( x - v.x ) + square( y - v.y ) + square( z - v.z );
	}

	// Dot Product with a Vector3
	T
	dot( Vector3 const & v ) const
	{
		return ( x * v.x ) + ( y * v.y ) + ( z * v.z );
	}

	// Dot Product with an Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	T
	dot( A const & a ) const
	{
		assert( a.size() == 3 );
		return ( x * a[ 0 ] ) + ( y * a[ 1 ] ) + ( z * a[ 2 ] );
	}

	// Cross Product with a Vector3
	Vector3
	cross( Vector3 const & v ) const
	{
		return Vector3(
		 ( y * v.z ) - ( z * v.y ),
		 ( z * v.x ) - ( x * v.z ),
		 ( x * v.y ) - ( y * v.x )
		);
	}

	// Cross Product with an Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
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
	Tr
	x1() const
	{
		return x;
	}

	// Alias for Element 1
	T &
	x1()
	{
		return x;
	}

	// Alias for Element 2
	Tr
	x2() const
	{
		return y;
	}

	// Alias for Element 2
	T &
	x2()
	{
		return y;
	}

	// Alias for Element 3
	Tr
	x3() const
	{
		return z;
	}

	// Alias for Element 3
	T &
	x3()
	{
		return z;
	}

public: // Modifiers

	// Zero
	Vector3 &
	zero()
	{
		x = y = z = T( 0 );
		return *this;
	}

	// Negate
	Vector3 &
	negate()
	{
		x = -x;
		y = -y;
		z = -z;
		return *this;
	}

	// Normalize to a Length
	Vector3 &
	normalize( Tc tar_length = T( 1 ) )
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
	Vector3 &
	normalize_zero( Tc tar_length = T( 1 ) )
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

	// Normalize to a Length: Uniform Vector3 if Length is Zero
	Vector3 &
	normalize_uniform( Tc tar_length = T( 1 ) )
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

	// Normalize to a Length: x Vector3 if Length is Zero
	Vector3 &
	normalize_x( Tc tar_length = T( 1 ) )
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
	Vector3 &
	normalize_y( Tc tar_length = T( 1 ) )
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
	Vector3 &
	normalize_z( Tc tar_length = T( 1 ) )
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

	// Minimum Coordinates with a Vector3
	Vector3 &
	min( Vector3 const & v )
	{
		x = ( x <= v.x ? x : v.x );
		y = ( y <= v.y ? y : v.y );
		z = ( z <= v.z ? z : v.z );
		return *this;
	}

	// Maximum Coordinates with a Vector3
	Vector3 &
	max( Vector3 const & v )
	{
		x = ( x >= v.x ? x : v.x );
		y = ( y >= v.y ? y : v.y );
		z = ( z >= v.z ? z : v.z );
		return *this;
	}

	// Add a Vector3
	Vector3 &
	add( Vector3 const & v )
	{
		x += v.x;
		y += v.y;
		z += v.z;
		return *this;
	}

	// Sum a Vector3
	Vector3 &
	sum( Vector3 const & v )
	{
		x += v.x;
		y += v.y;
		z += v.z;
		return *this;
	}

	// Subtract a Vector3
	Vector3 &
	sub( Vector3 const & v )
	{
		x -= v.x;
		y -= v.y;
		z -= v.z;
		return *this;
	}

	// Subtract a Vector3
	Vector3 &
	subtract( Vector3 const & v )
	{
		x -= v.x;
		y -= v.y;
		z -= v.z;
		return *this;
	}

	// Project Normal to a Vector3
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

	// Project onto a Vector3
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
	Vector3
	operator -() const
	{
		return Vector3( -x, -y, -z );
	}

	// Negated
	Vector3
	negated() const
	{
		return Vector3( -x, -y, -z );
	}

	// Normalized to a Length
	Vector3
	normalized( Tc tar_length = T( 1 ) ) const
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
	Vector3
	normalized_zero( Tc tar_length = T( 1 ) ) const
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

	// Normalized to a Length: Uniform Vector3 if Length is Zero
	Vector3
	normalized_uniform( Tc tar_length = T( 1 ) ) const
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

	// Normalized to a Length: x Vector3 if Length is Zero
	Vector3
	normalized_x( Tc tar_length = T( 1 ) ) const
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
	Vector3
	normalized_y( Tc tar_length = T( 1 ) ) const
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
	Vector3
	normalized_z( Tc tar_length = T( 1 ) ) const
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

	// Projected Normal to a Vector3
	Vector3
	projected_normal( Vector3 const & v ) const
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		return Vector3( x - ( c * v.x ), y - ( c * v.y ), z - ( c * v.z ) );
	}

	// Projected onto a Vector3
	Vector3
	projected_parallel( Vector3 const & v ) const
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		return Vector3( c * v.x, c * v.y, c * v.z );
	}

public: // Static Methods

	// Square of a value
	static
	T
	square( Tc t )
	{
		return t * t;
	}

	// Value Clipped to [-1,1]
	static
	T
	sin_cos_range( Tc t )
	{
		return std::min( std::max( t, T( -1 ) ), T( 1 ) );
	}

	// Add 2*Pi to a Negative Value
	static
	T
	bump_up_angle( Tc t )
	{
		static T const Two_Pi( T( 2 ) * std::acos( -1.0 ) );
		return ( t >= T( 0 ) ? t : Two_Pi + t );
	}

public: // Data

	T x, y, z; // Elements

}; // Vector3

// Length
template< typename T >
inline
T
length( Vector3< T > const & v )
{
	return v.length();
}

// Length Squared
template< typename T >
inline
T
length_squared( Vector3< T > const & v )
{
	return v.length_squared();
}

// Magnitude
template< typename T >
inline
T
magnitude( Vector3< T > const & v )
{
	return v.magnitude();
}

// Magnitude
template< typename T >
inline
T
mag( Vector3< T > const & v )
{
	return v.mag();
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( Vector3< T > const & v )
{
	return v.magnitude_squared();
}

// Magnitude Squared
template< typename T >
inline
T
mag_squared( Vector3< T > const & v )
{
	return v.mag_squared();
}

// Vector3 == Vector3
template< typename T >
inline
bool
operator ==( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x == b.x ) && ( a.y == b.y ) && ( a.z == b.z );
}

// Vector3 != Vector3
template< typename T >
inline
bool
operator !=( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x != b.x ) || ( a.y != b.y ) || ( a.z != b.z );
}

// Vector3 < Vector3: Lexicographic
template< typename T >
inline
bool
operator <( Vector3< T > const & a, Vector3< T > const & b )
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
template< typename T >
inline
bool
operator <=( Vector3< T > const & a, Vector3< T > const & b )
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
template< typename T >
inline
bool
operator >=( Vector3< T > const & a, Vector3< T > const & b )
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
template< typename T >
inline
bool
operator >( Vector3< T > const & a, Vector3< T > const & b )
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
template< typename T >
inline
bool
lt( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x < b.x ) && ( a.y < b.y ) && ( a.z < b.z );
}

// Vector3 <= Vector3: Element-wise
template< typename T >
inline
bool
le( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x <= b.x ) && ( a.y <= b.y ) && ( a.z <= b.z );
}

// Vector3 >= Vector3: Element-wise
template< typename T >
inline
bool
ge( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x >= b.x ) && ( a.y >= b.y ) && ( a.z >= b.z );
}

// Vector3 > Vector3: Element-wise
template< typename T >
inline
bool
gt( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x > b.x ) && ( a.y > b.y ) && ( a.z > b.z );
}

// Vector3 == Value
template< typename T >
inline
bool
operator ==( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return ( v.x == t ) && ( v.y == t ) && ( v.z == t );
}

// Vector3 != Value
template< typename T >
inline
bool
operator !=( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return ( v.x != t ) || ( v.y != t ) || ( v.z != t );
}

// Vector3 < Value
template< typename T >
inline
bool
operator <( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return ( v.x < t ) && ( v.y < t ) && ( v.z < t );
}

// Vector3 <= Value
template< typename T >
inline
bool
operator <=( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return ( v.x <= t ) && ( v.y <= t ) && ( v.z <= t );
}

// Vector3 >= Value
template< typename T >
inline
bool
operator >=( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return ( v.x >= t ) && ( v.y >= t ) && ( v.z >= t );
}

// Vector3 > Value
template< typename T >
inline
bool
operator >( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return ( v.x > t ) && ( v.y > t ) && ( v.z > t );
}

// Value == Vector3
template< typename T >
inline
bool
operator ==( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return ( t == v.x ) && ( t == v.y ) && ( t == v.z );
}

// Value != Vector3
template< typename T >
inline
bool
operator !=( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return ( t != v.x ) || ( t != v.y ) || ( t != v.z );
}

// Value < Vector3
template< typename T >
inline
bool
operator <( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return ( t < v.x ) && ( t < v.y ) && ( t < v.z );
}

// Value <= Vector3
template< typename T >
inline
bool
operator <=( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return ( t <= v.x ) && ( t <= v.y ) && ( t <= v.z );
}

// Value >= Vector3
template< typename T >
inline
bool
operator >=( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return ( t >= v.x ) && ( t >= v.y ) && ( t >= v.z );
}

// Value > Vector3
template< typename T >
inline
bool
operator >( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return ( t > v.x ) && ( t > v.y ) && ( t > v.z );
}

// Equal Length?
template< typename T >
inline
bool
equal_length( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.length_squared() == b.length_squared() );
}

// Not Equal Length?
template< typename T >
inline
bool
not_equal_length( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.length_squared() != b.length_squared() );
}

// Vector3 + Vector3
template< typename T >
inline
Vector3< T >
operator +( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >( a.x + b.x, a.y + b.y, a.z + b.z );
}

// Vector3 + Value
template< typename T >
inline
Vector3< T >
operator +( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return Vector3< T >( v.x + t, v.y + t, v.z + t );
}

// Value + Vector3
template< typename T >
inline
Vector3< T >
operator +( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return Vector3< T >( t + v.x, t + v.y, t + v.z );
}

// Vector3 - Vector3
template< typename T >
inline
Vector3< T >
operator -( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >( a.x - b.x, a.y - b.y, a.z - b.z );
}

// Vector3 - Value
template< typename T >
inline
Vector3< T >
operator -( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return Vector3< T >( v.x - t, v.y - t, v.z - t );
}

// Value - Vector3
template< typename T >
inline
Vector3< T >
operator -( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return Vector3< T >( t - v.x, t - v.y, t - v.z );
}

// Vector3 * Vector3
template< typename T >
inline
Vector3< T >
operator *( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >( a.x * b.x, a.y * b.y, a.z * b.z );
}

// Vector3 * Value
template< typename T >
inline
Vector3< T >
operator *( Vector3< T > const & v, typename Vector3< T >::Tc t )
{
	return Vector3< T >( v.x * t, v.y * t, v.z * t );
}

// Value * Vector3
template< typename T >
inline
Vector3< T >
operator *( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	return Vector3< T >( t * v.x, t * v.y, t * v.z );
}

// Vector3 / Vector3
template< typename T >
inline
Vector3< T >
operator /( Vector3< T > const & a, Vector3< T > const & b )
{
	assert( b.x != T( 0 ) );
	assert( b.y != T( 0 ) );
	assert( b.z != T( 0 ) );
	return Vector3< T >( a.x / b.x, a.y / b.y, a.z / b.z );
}

// Vector3 / Value
template< typename T, typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
inline
Vector3< T >
operator /( Vector3< T > const & v, U const & u )
{
	assert( u != U( 0 ) );
	U const inv_u( U ( 1 ) / u );
	return Vector3< T >( v.x * inv_u, v.y * inv_u, v.z * inv_u );
}

// Vector3 / Value
template< typename T, typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
inline
Vector3< T >
operator /( Vector3< T > const & v, U const & u )
{
	assert( u != U( 0 ) );
	return Vector3< T >( v.x / u, v.y / u, v.z / u );
}

// Value / Vector3
template< typename T >
inline
Vector3< T >
operator /( typename Vector3< T >::Tc t, Vector3< T > const & v )
{
	assert( v.x != T( 0 ) );
	assert( v.y != T( 0 ) );
	assert( v.z != T( 0 ) );
	return Vector3< T >( t / v.x, t / v.y, t / v.z );
}

// Minimum of Two Vector3s
template< typename T >
inline
Vector3< T >
min( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >(
	 ( a.x <= b.x ? a.x : b.x ),
	 ( a.y <= b.y ? a.y : b.y ),
	 ( a.z <= b.z ? a.z : b.z )
	);
}

// Minimum of Three Vector3s
template< typename T >
inline
Vector3< T >
min( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	return Vector3< T >(
	 ObjexxFCL::min( a.x, b.x, c.x ),
	 ObjexxFCL::min( a.y, b.y, c.y ),
	 ObjexxFCL::min( a.z, b.z, c.z )
	);
}

// Minimum of Four Vector3s
template< typename T >
inline
Vector3< T >
min( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c, Vector3< T > const & d )
{
	return Vector3< T >(
	 ObjexxFCL::min( a.x, b.x, c.x, d.x ),
	 ObjexxFCL::min( a.y, b.y, c.y, d.y ),
	 ObjexxFCL::min( a.z, b.z, c.z, d.z )
	);
}

// Maximum of Two Vector3s
template< typename T >
inline
Vector3< T >
max( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >(
	 ( a.x >= b.x ? a.x : b.x ),
	 ( a.y >= b.y ? a.y : b.y ),
	 ( a.z >= b.z ? a.z : b.z )
	);
}

// Maximum of Three Vector3s
template< typename T >
inline
Vector3< T >
max( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	return Vector3< T >(
	 ObjexxFCL::max( a.x, b.x, c.x ),
	 ObjexxFCL::max( a.y, b.y, c.y ),
	 ObjexxFCL::max( a.z, b.z, c.z )
	);
}

// Maximum of Four Vector3s
template< typename T >
inline
Vector3< T >
max( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c, Vector3< T > const & d )
{
	return Vector3< T >(
	 ObjexxFCL::max( a.x, b.x, c.x, d.x ),
	 ObjexxFCL::max( a.y, b.y, c.y, d.y ),
	 ObjexxFCL::max( a.z, b.z, c.z, d.z )
	);
}

// Sum of Two Vector3s
template< typename T >
inline
Vector3< T >
sum( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >( a.x + b.x, a.y + b.y, a.z + b.z );
}

// Sum of Three Vector3s
template< typename T >
inline
Vector3< T >
sum( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	return Vector3< T >( a.x + b.x + c.x, a.y + b.y + c.y, a.z + b.z + c.z );
}

// Sum of Four Vector3s
template< typename T >
inline
Vector3< T >
sum( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c, Vector3< T > const & d )
{
	return Vector3< T >( a.x + b.x + c.x + d.x, a.y + b.y + c.y + d.y, a.z + b.z + c.z + d.z );
}

// Subtract of Two Vector3s
template< typename T >
inline
Vector3< T >
sub( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >( a.x - b.x, a.y - b.y, a.z - b.z );
}

// Subtract of Two Vector3s
template< typename T >
inline
Vector3< T >
subtract( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >( a.x - b.x, a.y - b.y, a.z - b.z );
}

// Midpoint of Two Vector3s
template< typename T >
inline
Vector3< T >
mid( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >(
	 T( 0.5 * ( a.x + b.x ) ),
	 T( 0.5 * ( a.y + b.y ) ),
	 T( 0.5 * ( a.z + b.z ) )
	);
}

// Center of Two Vector3s
template< typename T >
inline
Vector3< T >
cen( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >(
	 T( 0.5 * ( a.x + b.x ) ),
	 T( 0.5 * ( a.y + b.y ) ),
	 T( 0.5 * ( a.z + b.z ) )
	);
}

// Center of Three Vector3s
template< typename T >
inline
Vector3< T >
cen( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	static long double const third( 1.0 / 3.0 );
	return Vector3< T >(
	 T( third * ( a.x + b.x + c.x ) ),
	 T( third * ( a.y + b.y + c.y ) ),
	 T( third * ( a.z + b.z + c.z ) )
	);
}

// Center of Four Vector3s
template< typename T >
inline
Vector3< T >
cen( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c, Vector3< T > const & d )
{
	return Vector3< T >(
	 T( 0.25 * ( a.x + b.x + c.x + d.x ) ),
	 T( 0.25 * ( a.y + b.y + c.y + d.y ) ),
	 T( 0.25 * ( a.z + b.z + c.z + d.z ) )
	);
}

// Distance
template< typename T >
inline
T
distance( Vector3< T > const & a, Vector3< T > const & b )
{
	return std::sqrt( Vector3< T >::square( a.x - b.x ) + Vector3< T >::square( a.y - b.y ) + Vector3< T >::square( a.z - b.z ) );
}

// Distance Squared
template< typename T >
inline
T
distance_squared( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >::square( a.x - b.x ) + Vector3< T >::square( a.y - b.y ) + Vector3< T >::square( a.z - b.z );
}

// Dot Product
template< typename T >
inline
T
dot( Vector3< T > const & a, Vector3< T > const & b )
{
	return ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z );
}

// Cross Product
template< typename T >
inline
Vector3< T >
cross( Vector3< T > const & a, Vector3< T > const & b )
{
	return Vector3< T >(
	 ( a.y * b.z ) - ( a.z * b.y ),
	 ( a.z * b.x ) - ( a.x * b.z ),
	 ( a.x * b.y ) - ( a.y * b.x )
	);
}

// Angle Between Two Vector3s (in Radians on [0,pi])
template< typename T >
inline
T
angle( Vector3< T > const & a, Vector3< T > const & b )
{
	T const axb( a.cross( b ).magnitude() );
	T const adb( a.dot( b ) );
	return ( ( axb != T( 0 ) ) || ( adb != T( 0 ) ) ? Vector3< T >::bump_up_angle( std::atan2( axb, adb ) ) : T( 0 ) ); // More accurate than dot-based for angles near 0 and Pi
}

// Angle abc Formed by Three Vector3s (in Radians on [0,pi])
template< typename T >
inline
T
angle( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	return angle( a - b, c - b );
}

// Cosine of Angle Between Two Vector3s
template< typename T >
inline
T
cos( Vector3< T > const & a, Vector3< T > const & b )
{
	T const mag( std::sqrt( a.length_squared() * b.length_squared() ) );
	return ( mag > T( 0 ) ? Vector3< T >::sin_cos_range( a.dot( b ) / mag ) : T( 1 ) );
}

// Cosine of Angle abc Formed by Three Vector3s
template< typename T >
inline
T
cos( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	return cos( a - b, c - b );
}

// Sine of Angle Between Two Vector3s
template< typename T >
inline
T
sin( Vector3< T > const & a, Vector3< T > const & b )
{
	T const mag( std::sqrt( a.length_squared() * b.length_squared() ) );
	return ( mag > T( 0 ) ? std::abs( Vector3< T >::sin_cos_range( a.cross( b ).magnitude() / mag ) ) : T( 0 ) );
}

// Sine of Angle abc Formed by Three Vector3s
template< typename T >
inline
T
sin( Vector3< T > const & a, Vector3< T > const & b, Vector3< T > const & c )
{
	return sin( a - b, c - b );
}

// Stream << Vector3 output operator
template< typename T >
std::ostream &
operator <<( std::ostream & stream, Vector3< T > const & v )
{
	// Types
	typedef  TypeTraits< T >  Traits;

	// Save current stream state and set persistent state
	std::ios_base::fmtflags const old_flags( stream.flags() );
	std::streamsize const old_precision( stream.precision( Traits::precision ) );
	stream << std::right << std::showpoint << std::uppercase;

	// Output Vector3
	std::size_t const w( Traits::width );
	stream << std::setw( w ) << v.x << ' ' << std::setw( w ) << v.y << ' ' << std::setw( w ) << v.z;

	// Restore previous stream state
	stream.precision( old_precision );
	stream.flags( old_flags );

	return stream;
}

// Stream >> Vector3 input operator
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
