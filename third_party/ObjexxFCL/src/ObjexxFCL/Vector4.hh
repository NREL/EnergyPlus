#ifndef ObjexxFCL_Vector4_hh_INCLUDED
#define ObjexxFCL_Vector4_hh_INCLUDED

// Vector4: Fast 4-Element Vector
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
#include <ObjexxFCL/Vector4.fwd.hh>
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

// Vector4: Fast 4-Element Vector
// . Heap-free and loop-free for speed
// . Provides direct element access via .x style lookup
// . Use std::array< T, 4 > instead in array/vectorization context
template< typename T >
class Vector4
{

private: // Friends

	template< typename > friend class Vector4;

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
	Vector4()
#if defined(OBJEXXFCL_ARRAY_INIT) || defined(OBJEXXFCL_ARRAY_INIT_DEBUG)
	 :
	 x( Traits::initial_array_value() ),
	 y( Traits::initial_array_value() ),
	 z( Traits::initial_array_value() ),
	 w( Traits::initial_array_value() )
#endif
	{}

	// Copy Constructor
	Vector4( Vector4 const & v ) :
	 x( v.x ),
	 y( v.y ),
	 z( v.z ),
	 w( v.w )
	{}

	// Copy Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Vector4( Vector4< U > const & v ) :
	 x( v.x ),
	 y( v.y ),
	 z( v.z ),
	 w( v.w )
	{}

	// Uniform Value Constructor
	explicit
	Vector4( Tc t ) :
	 x( t ),
	 y( t ),
	 z( t ),
	 w( t )
	{}

	// Value Constructor
	Vector4(
	 Tc x_,
	 Tc y_,
	 Tc z_,
	 Tc w_
	) :
	 x( x_ ),
	 y( y_ ),
	 z( z_ ),
	 w( w_ )
	{}

	// Initializer List Constructor Template
	template< typename U, class = typename std::enable_if< std::is_constructible< T, U >::value >::type >
	Vector4( std::initializer_list< U > const l ) :
	 x( *l.begin() ),
	 y( *( l.begin() + 1 ) ),
	 z( *( l.begin() + 2 ) ),
	 w( *( l.begin() + 3 ) )
	{
		assert( l.size() == 4 );
	}

	// Array Constructor Template
	template< typename A, class = typename std::enable_if< std::is_constructible< T, typename A::value_type >::value >::type >
	Vector4( A const & a ) :
	 x( a[ 0 ] ),
	 y( a[ 1 ] ),
	 z( a[ 2 ] ),
	 w( a[ 3 ] )
	{
		assert( a.size() == 4 );
	}

	// Default Vector Named Constructor
	static
	Vector4
	default_vector()
	{
		return Vector4( T() );
	}

	// Zero Vector Named Constructor
	static
	Vector4
	zero_vector()
	{
		return Vector4( T( 0 ) );
	}

	// x Vector of Specified Length Named Constructor
	static
	Vector4
	x_vector( Tc tar_length = T( 1 ) )
	{
		return Vector4( tar_length, T( 0 ), T( 0 ), T( 0 ) );
	}

	// y Vector of Specified Length Named Constructor
	static
	Vector4
	y_vector( Tc tar_length = T( 1 ) )
	{
		return Vector4( T( 0 ), tar_length, T( 0 ), T( 0 ) );
	}

	// z Vector of Specified Length Named Constructor
	static
	Vector4
	z_vector( Tc tar_length = T( 1 ) )
	{
		return Vector4( T( 0 ), T( 0 ), tar_length, T( 0 ) );
	}

	// W Vector of Specified Length Named Constructor
	static
	Vector4
	W_vector( Tc tar_length = T( 1 ) )
	{
		return Vector4( T( 0 ), T( 0 ), T( 0 ), tar_length, T( 0 ) );
	}

	// Uniform Vector of Specified Length Named Constructor
	static
	Vector4
	uniform_vector( Tc tar_length = T( 1 ) )
	{
		return Vector4( tar_length / T( 2 ) );
	}

	// Destructor
	~Vector4()
	{}

public: // Assignment

	// Copy Assignment
	Vector4 &
	operator =( Vector4 const & v )
	{
		if ( this != &v ) {
			x = v.x;
			y = v.y;
			z = v.z;
			w = v.w;
		}
		return *this;
	}

	// Copy Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator =( Vector4< U > const & v )
	{
		x = v.x;
		y = v.y;
		z = v.z;
		w = v.w;
		return *this;
	}

	// Initializer List Assignment Template
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator =( std::initializer_list< U > const l )
	{
		assert( l.size() == 4 );
		auto i( l.begin() );
		x = *i;
		y = *(++i);
		z = *(++i);
		w = *(++i);
		return *this;
	}

	// Array Assignment Template
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	Vector4 &
	operator =( A const & a )
	{
		assert( a.size() == 4 );
		x = a[ 0 ];
		y = a[ 1 ];
		z = a[ 2 ];
		w = a[ 3 ];
		return *this;
	}

	// += Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator +=( Vector4< U > const & v )
	{
		x += v.x;
		y += v.y;
		z += v.z;
		w += v.w;
		return *this;
	}

	// -= Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator -=( Vector4< U > const & v )
	{
		x -= v.x;
		y -= v.y;
		z -= v.z;
		w -= v.w;
		return *this;
	}

	// *= Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator *=( Vector4< U > const & v )
	{
		x *= v.x;
		y *= v.y;
		z *= v.z;
		w *= v.w;
		return *this;
	}

	// /= Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator /=( Vector4< U > const & v )
	{
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		assert( v.w != T( 0 ) );
		x /= v.x;
		y /= v.y;
		z /= v.z;
		w /= v.w;
		return *this;
	}

	// += Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator +=( std::initializer_list< U > const l )
	{
		assert( l.size() == 4 );
		auto i( l.begin() );
		x += *i;
		y += *(++i);
		z += *(++i);
		w += *(++i);
		return *this;
	}

	// -= Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator -=( std::initializer_list< U > const l )
	{
		assert( l.size() == 4 );
		auto i( l.begin() );
		x -= *i;
		y -= *(++i);
		z -= *(++i);
		w -= *(++i);
		return *this;
	}

	// *= Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator *=( std::initializer_list< U > const l )
	{
		assert( l.size() == 4 );
		auto i( l.begin() );
		x *= *i;
		y *= *(++i);
		z *= *(++i);
		w *= *(++i);
		return *this;
	}

	// /= Initializer List
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	operator /=( std::initializer_list< U > const l )
	{
		assert( l.size() == 4 );
		auto i( l.begin() );
		assert( *i != T( 0 ) );
		assert( *(i+1) != T( 0 ) );
		assert( *(i+2) != T( 0 ) );
		assert( *(i+3) != T( 0 ) );
		x /= *i;
		y /= *(++i);
		z /= *(++i);
		w /= *(++i);
		return *this;
	}

	// += Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	Vector4 &
	operator +=( A const & a )
	{
		assert( a.size() == 4 );
		x += a[ 0 ];
		y += a[ 1 ];
		z += a[ 2 ];
		w += a[ 3 ];
		return *this;
	}

	// -= Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	Vector4 &
	operator -=( A const & a )
	{
		assert( a.size() == 4 );
		x -= a[ 0 ];
		y -= a[ 1 ];
		z -= a[ 2 ];
		w -= a[ 3 ];
		return *this;
	}

	// *= Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	Vector4 &
	operator *=( A const & a )
	{
		assert( a.size() == 4 );
		x *= a[ 0 ];
		y *= a[ 1 ];
		z *= a[ 2 ];
		w *= a[ 3 ];
		return *this;
	}

	// /= Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	Vector4 &
	operator /=( A const & a )
	{
		assert( a.size() == 4 );
		assert( a[ 0 ] != T( 0 ) );
		assert( a[ 1 ] != T( 0 ) );
		assert( a[ 2 ] != T( 0 ) );
		assert( a[ 3 ] != T( 0 ) );
		x /= a[ 0 ];
		y /= a[ 1 ];
		z /= a[ 2 ];
		w /= a[ 3 ];
		return *this;
	}

	// = Value
	Vector4 &
	operator =( Tc t )
	{
		x = y = z = w = t;
		return *this;
	}

	// += Value
	Vector4 &
	operator +=( Tc t )
	{
		x += t;
		y += t;
		z += t;
		w += t;
		return *this;
	}

	// -= Value
	Vector4 &
	operator -=( Tc t )
	{
		x -= t;
		y -= t;
		z -= t;
		w -= t;
		return *this;
	}

	// *= Value
	Vector4 &
	operator *=( Tc t )
	{
		x *= t;
		y *= t;
		z *= t;
		w *= t;
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
	Vector4 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		U const inv_u( U( 1 ) / u );
		x *= inv_u;
		y *= inv_u;
		z *= inv_u;
		w *= inv_u;
		return *this;
	}

	// /= Value
	template< typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void, typename = void >
	Vector4 &
	operator /=( U const & u )
	{
		assert( u != U( 0 ) );
		x /= u;
		y /= u;
		z /= u;
		w /= u;
		return *this;
	}

	// Value Assignment
	Vector4 &
	assign(
	 Tc x_,
	 Tc y_,
	 Tc z_,
	 Tc w_
	)
	{
		x = x_;
		y = y_;
		z = z_;
		w = w_;
		return *this;
	}

public: // Assignment: Scaled

	// Assign Value * Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	scaled_assign( Tc t, Vector4< U > const & v )
	{
		x = t * v.x;
		y = t * v.y;
		z = t * v.z;
		w = t * v.w;
		return *this;
	}

	// Add Value * Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	scaled_add( Tc t, Vector4< U > const & v )
	{
		x += t * v.x;
		y += t * v.y;
		z += t * v.z;
		w += t * v.w;
		return *this;
	}

	// Subtract Value * Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	scaled_sub( Tc t, Vector4< U > const & v )
	{
		x -= t * v.x;
		y -= t * v.y;
		z -= t * v.z;
		w -= t * v.w;
		return *this;
	}

	// Multiply by Value * Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	scaled_mul( Tc t, Vector4< U > const & v )
	{
		x *= t * v.x;
		y *= t * v.y;
		z *= t * v.z;
		w *= t * v.w;
		return *this;
	}

	// Divide by Value * Vector4
	template< typename U, class = typename std::enable_if< std::is_assignable< T&, U >::value >::type >
	Vector4 &
	scaled_div( Tc t, Vector4< U > const & v )
	{
		assert( t != T( 0 ) );
		assert( v.x != T( 0 ) );
		assert( v.y != T( 0 ) );
		assert( v.z != T( 0 ) );
		assert( v.w != T( 0 ) );
		x /= t * v.x;
		y /= t * v.y;
		z /= t * v.z;
		w /= t * v.w;
		return *this;
	}

public: // Subscript

	// Vector4[ i ] const: 0-Based Index
	Tr
	operator []( size_type const i ) const
	{
		assert( i <= 3 );
		return ( i < 2 ? ( i == 0 ? x : y ) : ( i == 2 ? z : w ) );
	}

	// Vector4[ i ]: 0-Based Index
	T &
	operator []( size_type const i )
	{
		assert( i <= 3 );
		return ( i < 2 ? ( i == 0 ? x : y ) : ( i == 2 ? z : w ) );
	}

	// Vector4( i ) const: 1-Based Index
	Tr
	operator ()( size_type const i ) const
	{
		assert( ( 1 <= i ) && ( i <= 4 ) );
		return ( i <= 2 ? ( i == 1 ? x : y ) : ( i == 3 ? z : w ) );
	}

	// Vector4( i ): 1-Based Index
	T &
	operator ()( size_type const i )
	{
		assert( ( 1 <= i ) && ( i <= 4 ) );
		return ( i <= 2 ? ( i == 1 ? x : y ) : ( i == 3 ? z : w ) );
	}

public: // Properties: Predicates

	// Is Zero Vector?
	bool
	is_zero() const
	{
		static T const ZERO( 0 );
		return ( x == ZERO ) && ( y == ZERO ) && ( z == ZERO ) && ( w == ZERO );
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
		return 4u;
	}

	// Length
	T
	length() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) + ( w * w ) );
	}

	// Length Squared
	T
	length_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z ) + ( w * w );
	}

	// Magnitude
	T
	magnitude() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) + ( w * w ) );
	}

	// Magnitude
	T
	mag() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) + ( w * w ) );
	}

	// Magnitude Squared
	T
	magnitude_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z ) + ( w * w );
	}

	// Magnitude Squared
	T
	mag_squared() const
	{
		return ( x * x ) + ( y * y ) + ( z * z ) + ( w * w );
	}

	// L1 Norm
	T
	norm_L1() const
	{
		return std::abs( x ) + std::abs( y ) + std::abs( z ) + std::abs( w );
	}

	// L2 Norm
	T
	norm_L2() const
	{
		return std::sqrt( ( x * x ) + ( y * y ) + ( z * z ) + ( w * w ) );
	}

	// L-infinity Norm
	T
	norm_Linf() const
	{
		return ObjexxFCL::max( std::abs( x ), std::abs( y ), std::abs( z ), std::abs( w ) );
	}

	// Distance to a Vector4
	T
	distance( Vector4 const & v ) const
	{
		return std::sqrt( square( x - v.x ) + square( y - v.y ) + square( z - v.z )  + square( w - v.w ) );
	}

	// Distance Squared to a Vector4
	T
	distance_squared( Vector4 const & v ) const
	{
		return square( x - v.x ) + square( y - v.y ) + square( z - v.z ) + square( w - v.w );
	}

	// Dot Product with a Vector4
	T
	dot( Vector4 const & v ) const
	{
		return ( x * v.x ) + ( y * v.y ) + ( z * v.z ) + ( w * v.w );
	}

	// Dot Product with an Array
	template< typename A, class = typename std::enable_if< std::is_assignable< T&, typename A::value_type >::value >::type >
	T
	dot( A const & a ) const
	{
		assert( a.size() == 4 );
		return ( x * a[ 0 ] ) + ( y * a[ 1 ] ) + ( z * a[ 2 ] ) + ( w * a[ 3 ] );
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

	// Alias for Element 4
	Tr
	x4() const
	{
		return w;
	}

	// Alias for Element 4
	T &
	x4()
	{
		return w;
	}

public: // Modifiers

	// Zero
	Vector4 &
	zero()
	{
		x = y = z = w = T( 0 );
		return *this;
	}

	// Negate
	Vector4 &
	negate()
	{
		x = -x;
		y = -y;
		z = -z;
		w = -w;
		return *this;
	}

	// Normalize to a Length
	Vector4 &
	normalize( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		assert( cur_length != T ( 0 ) );
		T const dilation( tar_length / cur_length );
		x *= dilation;
		y *= dilation;
		z *= dilation;
		w *= dilation;
		return *this;
	}

	// Normalize to a Length: Zero Vector4 if Length is Zero
	Vector4 &
	normalize_zero( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
			w *= dilation;
		} else { // Set zero vector
			x = y = z = w = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: Uniform Vector4 if Length is Zero
	Vector4 &
	normalize_uniform( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
			w *= dilation;
		} else { // Set uniform vector
			operator =( uniform_vector( tar_length ) );
		}
		return *this;
	}

	// Normalize to a Length: x Vector4 if Length is Zero
	Vector4 &
	normalize_x( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
			w *= dilation;
		} else { // Set x vector
			x = tar_length;
			y = z = w = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: y Vector4 if Length is Zero
	Vector4 &
	normalize_y( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
			w *= dilation;
		} else { // Set y vector
			y = tar_length;
			x = z = w = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: z Vector4 if Length is Zero
	Vector4 &
	normalize_z( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
			w *= dilation;
		} else { // Set z vector
			z = tar_length;
			x = y = w = T( 0 );
		}
		return *this;
	}

	// Normalize to a Length: w Vector4 if Length is Zero
	Vector4 &
	normalize_w( Tc tar_length = T( 1 ) )
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			x *= dilation;
			y *= dilation;
			z *= dilation;
			w *= dilation;
		} else { // Set z vector
			w = tar_length;
			x = y = z = T( 0 );
		}
		return *this;
	}

	// Minimum Coordinates with a Vector4
	Vector4 &
	min( Vector4 const & v )
	{
		x = ( x <= v.x ? x : v.x );
		y = ( y <= v.y ? y : v.y );
		z = ( z <= v.z ? z : v.z );
		w = ( w <= v.w ? w : v.w );
		return *this;
	}

	// Maximum Coordinates with a Vector4
	Vector4 &
	max( Vector4 const & v )
	{
		x = ( x >= v.x ? x : v.x );
		y = ( y >= v.y ? y : v.y );
		z = ( z >= v.z ? z : v.z );
		w = ( w >= v.w ? w : v.w );
		return *this;
	}

	// Add a Vector4
	Vector4 &
	add( Vector4 const & v )
	{
		x += v.x;
		y += v.y;
		z += v.z;
		w += v.w;
		return *this;
	}

	// Sum a Vector4
	Vector4 &
	sum( Vector4 const & v )
	{
		x += v.x;
		y += v.y;
		z += v.z;
		w += v.w;
		return *this;
	}

	// Subtract a Vector4
	Vector4 &
	sub( Vector4 const & v )
	{
		x -= v.x;
		y -= v.y;
		z -= v.z;
		w -= v.w;
		return *this;
	}

	// Subtract a Vector4
	Vector4 &
	subtract( Vector4 const & v )
	{
		x -= v.x;
		y -= v.y;
		z -= v.z;
		w -= v.w;
		return *this;
	}

	// Project Normal to a Vector4
	Vector4 &
	project_normal( Vector4 const & v )
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		x -= c * v.x;
		y -= c * v.y;
		z -= c * v.z;
		w -= c * v.w;
		return *this;
	}

	// Project onto a Vector4
	Vector4 &
	project_parallel( Vector4 const & v )
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		x = c * v.x;
		y = c * v.y;
		z = c * v.z;
		w = c * v.w;
		return *this;
	}

public: // Generators

	// -Vector4 (Negated)
	Vector4
	operator -() const
	{
		return Vector4( -x, -y, -z, -w );
	}

	// Negated
	Vector4
	negated() const
	{
		return Vector4( -x, -y, -z, -w );
	}

	// Normalized to a Length
	Vector4
	normalized( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		assert( cur_length != T ( 0 ) );
		T const dilation( tar_length / cur_length );
		return Vector4(
		 x * dilation,
		 y * dilation,
		 z * dilation,
		 w * dilation
		);
	}

	// Normalized to a Length: Zero Vector4 if Length is Zero
	Vector4
	normalized_zero( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector4(
			 x * dilation,
			 y * dilation,
			 z * dilation,
			 w * dilation
			);
		} else { // Return zero vector
			return Vector4( T( 0 ) );
		}
	}

	// Normalized to a Length: Uniform Vector4 if Length is Zero
	Vector4
	normalized_uniform( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector4(
			 x * dilation,
			 y * dilation,
			 z * dilation,
			 w * dilation
			);
		} else { // Return uniform vector
			return uniform_vector( tar_length );
		}
	}

	// Normalized to a Length: x Vector4 if Length is Zero
	Vector4
	normalized_x( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector4(
			 x * dilation,
			 y * dilation,
			 z * dilation,
			 w * dilation
			);
		} else { // Return x vector
			return Vector4( tar_length, T( 0 ), T( 0 ), T( 0 ) );
		}
	}

	// Normalized to a Length: y Vector4 if Length is Zero
	Vector4
	normalized_y( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector4(
			 x * dilation,
			 y * dilation,
			 z * dilation,
			 w * dilation
			);
		} else { // Return y vector
			return Vector4( T( 0 ), tar_length, T( 0 ), T( 0 ) );
		}
	}

	// Normalized to a Length: z Vector4 if Length is Zero
	Vector4
	normalized_z( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector4(
			 x * dilation,
			 y * dilation,
			 z * dilation,
			 w * dilation
			);
		} else { // Return z vector
			return Vector4( tar_length, T( 0 ), T( 0 ), tar_length, T( 0 ) );
		}
	}

	// Normalized to a Length: w Vector4 if Length is Zero
	Vector4
	normalized_w( Tc tar_length = T( 1 ) ) const
	{
		T const cur_length( length() );
		if ( cur_length > T( 0 ) ) {
			T const dilation( tar_length / cur_length );
			return Vector4(
			 x * dilation,
			 y * dilation,
			 z * dilation,
			 w * dilation
			);
		} else { // Return z vector
			return Vector4( tar_length, T( 0 ), T( 0 ), T( 0 ), tar_length );
		}
	}

	// Projected Normal to a Vector4
	Vector4
	projected_normal( Vector4 const & v ) const
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		return Vector4( x - ( c * v.x ), y - ( c * v.y ), z - ( c * v.z ), w - ( c * v.w ) );
	}

	// Projected onto a Vector4
	Vector4
	projected_parallel( Vector4 const & v ) const
	{
		assert( v.length_squared() != T( 0 ) );
		T const c( dot( v ) / v.length_squared() );
		return Vector4( c * v.x, c * v.y, c * v.z, c * v.w );
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

	T x, y, z, w; // Elements

}; // Vector4

// Length
template< typename T >
inline
T
length( Vector4< T > const & v )
{
	return v.length();
}

// Length Squared
template< typename T >
inline
T
length_squared( Vector4< T > const & v )
{
	return v.length_squared();
}

// Magnitude
template< typename T >
inline
T
magnitude( Vector4< T > const & v )
{
	return v.magnitude();
}

// Magnitude
template< typename T >
inline
T
mag( Vector4< T > const & v )
{
	return v.mag();
}

// Magnitude Squared
template< typename T >
inline
T
magnitude_squared( Vector4< T > const & v )
{
	return v.magnitude_squared();
}

// Magnitude Squared
template< typename T >
inline
T
mag_squared( Vector4< T > const & v )
{
	return v.mag_squared();
}

// Vector4 == Vector4
template< typename T >
inline
bool
operator ==( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x == b.x ) && ( a.y == b.y ) && ( a.z == b.z ) && ( a.w == b.w );
}

// Vector4 != Vector4
template< typename T >
inline
bool
operator !=( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x != b.x ) || ( a.y != b.y ) || ( a.z != b.z ) || ( a.w != b.w );
}

// Vector4 < Vector4: Lexicographic
template< typename T >
inline
bool
operator <( Vector4< T > const & a, Vector4< T > const & b )
{
	return (
	 ( a.x < b.x ? true :
	 ( b.x < a.x ? false : // a.x == b.x
	 ( a.y < b.y ? true :
	 ( b.y < a.y ? false : // a.y == b.y
	 ( a.z < b.z ? true :
	 ( b.z < a.z ? false : // a.z == b.z
	 ( a.w < b.w ) ) ) ) ) ) )
	);
}

// Vector4 <= Vector4: Lexicographic
template< typename T >
inline
bool
operator <=( Vector4< T > const & a, Vector4< T > const & b )
{
	return (
	 ( a.x < b.x ? true :
	 ( b.x < a.x ? false : // a.x == b.x
	 ( a.y < b.y ? true :
	 ( b.y < a.y ? false : // a.y == b.y
	 ( a.z < b.z ? true :
	 ( b.z < a.z ? false : // a.z == b.z
	 ( a.w <= b.w ) ) ) ) ) ) )
	);
}

// Vector4 >= Vector4: Lexicographic
template< typename T >
inline
bool
operator >=( Vector4< T > const & a, Vector4< T > const & b )
{
	return (
	 ( a.x > b.x ? true :
	 ( b.x > a.x ? false : // a.x == b.x
	 ( a.y > b.y ? true :
	 ( b.y > a.y ? false : // a.y == b.y
	 ( a.z > b.z ? true :
	 ( b.z > a.z ? false : // a.z == b.z
	 ( a.w >= b.w ) ) ) ) ) ) )
	);
}

// Vector4 > Vector4: Lexicographic
template< typename T >
inline
bool
operator >( Vector4< T > const & a, Vector4< T > const & b )
{
	return (
	 ( a.x > b.x ? true :
	 ( b.x > a.x ? false : // a.x == b.x
	 ( a.y > b.y ? true :
	 ( b.y > a.y ? false : // a.y == b.y
	 ( a.z > b.z ? true :
	 ( b.z > a.z ? false : // a.z == b.z
	 ( a.w > b.w ) ) ) ) ) ) )
	);
}

// Vector4 < Vector4: Element-wise
template< typename T >
inline
bool
lt( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x < b.x ) && ( a.y < b.y ) && ( a.z < b.z ) && ( a.w < b.w );
}

// Vector4 <= Vector4: Element-wise
template< typename T >
inline
bool
le( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x <= b.x ) && ( a.y <= b.y ) && ( a.z <= b.z ) && ( a.w <= b.w );
}

// Vector4 >= Vector4: Element-wise
template< typename T >
inline
bool
ge( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x >= b.x ) && ( a.y >= b.y ) && ( a.z >= b.z ) && ( a.w >= b.w );
}

// Vector4 > Vector4: Element-wise
template< typename T >
inline
bool
gt( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x > b.x ) && ( a.y > b.y ) && ( a.z > b.z ) && ( a.w > b.w );
}

// Vector4 == Value
template< typename T >
inline
bool
operator ==( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return ( v.x == t ) && ( v.y == t ) && ( v.z == t ) && ( v.w == t );
}

// Vector4 != Value
template< typename T >
inline
bool
operator !=( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return ( v.x != t ) || ( v.y != t ) || ( v.z != t ) || ( v.w != t );
}

// Vector4 < Value
template< typename T >
inline
bool
operator <( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return ( v.x < t ) && ( v.y < t ) && ( v.z < t ) && ( v.w < t );
}

// Vector4 <= Value
template< typename T >
inline
bool
operator <=( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return ( v.x <= t ) && ( v.y <= t ) && ( v.z <= t ) && ( v.w <= t );
}

// Vector4 >= Value
template< typename T >
inline
bool
operator >=( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return ( v.x >= t ) && ( v.y >= t ) && ( v.z >= t ) && ( v.w >= t );
}

// Vector4 > Value
template< typename T >
inline
bool
operator >( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return ( v.x > t ) && ( v.y > t ) && ( v.z > t ) && ( v.w > t );
}

// Value == Vector4
template< typename T >
inline
bool
operator ==( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return ( t == v.x ) && ( t == v.y ) && ( t == v.z ) && ( t == v.w );
}

// Value != Vector4
template< typename T >
inline
bool
operator !=( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return ( t != v.x ) || ( t != v.y ) || ( t != v.z ) || ( t != v.w );
}

// Value < Vector4
template< typename T >
inline
bool
operator <( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return ( t < v.x ) && ( t < v.y ) && ( t < v.z ) && ( t < v.w );
}

// Value <= Vector4
template< typename T >
inline
bool
operator <=( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return ( t <= v.x ) && ( t <= v.y ) && ( t <= v.z ) && ( t <= v.w );
}

// Value >= Vector4
template< typename T >
inline
bool
operator >=( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return ( t >= v.x ) && ( t >= v.y ) && ( t >= v.z ) && ( t >= v.w );
}

// Value > Vector4
template< typename T >
inline
bool
operator >( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return ( t > v.x ) && ( t > v.y ) && ( t > v.z ) && ( t > v.w );
}

// Equal Length?
template< typename T >
inline
bool
equal_length( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.length_squared() == b.length_squared() );
}

// Not Equal Length?
template< typename T >
inline
bool
not_equal_length( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.length_squared() != b.length_squared() );
}

// Vector4 + Vector4
template< typename T >
inline
Vector4< T >
operator +( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >( a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w );
}

// Vector4 + Value
template< typename T >
inline
Vector4< T >
operator +( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return Vector4< T >( v.x + t, v.y + t, v.z + t, v.w + t );
}

// Value + Vector4
template< typename T >
inline
Vector4< T >
operator +( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return Vector4< T >( t + v.x, t + v.y, t + v.z, t + v.w );
}

// Vector4 - Vector4
template< typename T >
inline
Vector4< T >
operator -( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >( a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w );
}

// Vector4 - Value
template< typename T >
inline
Vector4< T >
operator -( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return Vector4< T >( v.x - t, v.y - t, v.z - t, v.w - t );
}

// Value - Vector4
template< typename T >
inline
Vector4< T >
operator -( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return Vector4< T >( t - v.x, t - v.y, t - v.z, t - v.w );
}

// Vector4 * Vector4
template< typename T >
inline
Vector4< T >
operator *( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >( a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w );
}

// Vector4 * Value
template< typename T >
inline
Vector4< T >
operator *( Vector4< T > const & v, typename Vector4< T >::Tc t )
{
	return Vector4< T >( v.x * t, v.y * t, v.z * t, v.w * t );
}

// Value * Vector4
template< typename T >
inline
Vector4< T >
operator *( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	return Vector4< T >( t * v.x, t * v.y, t * v.z, t * v.w );
}

// Vector4 / Vector4
template< typename T >
inline
Vector4< T >
operator /( Vector4< T > const & a, Vector4< T > const & b )
{
	assert( b.x != T( 0 ) );
	assert( b.y != T( 0 ) );
	assert( b.z != T( 0 ) );
	assert( b.w != T( 0 ) );
	return Vector4< T >( a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w );
}

// Vector4 / Value
template< typename T, typename U, class = typename std::enable_if< std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type >
inline
Vector4< T >
operator /( Vector4< T > const & v, U const & u )
{
	assert( u != U( 0 ) );
	U const inv_u( U ( 1 ) / u );
	return Vector4< T >( v.x * inv_u, v.y * inv_u, v.z * inv_u, v.w * inv_u );
}

// Vector4 / Value
template< typename T, typename U, class = typename std::enable_if< ! std::is_floating_point< U >::value && std::is_assignable< T&, U >::value >::type, typename = void >
inline
Vector4< T >
operator /( Vector4< T > const & v, U const & u )
{
	assert( u != U( 0 ) );
	return Vector4< T >( v.x / u, v.y / u, v.z / u, v.w / u );
}

// Value / Vector4
template< typename T >
inline
Vector4< T >
operator /( typename Vector4< T >::Tc t, Vector4< T > const & v )
{
	assert( v.x != T( 0 ) );
	assert( v.y != T( 0 ) );
	assert( v.z != T( 0 ) );
	assert( v.w != T( 0 ) );
	return Vector4< T >( t / v.x, t / v.y, t / v.z, t / v.w );
}

// Minimum of Two Vector4s
template< typename T >
inline
Vector4< T >
min( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >(
	 ( a.x <= b.x ? a.x : b.x ),
	 ( a.y <= b.y ? a.y : b.y ),
	 ( a.z <= b.z ? a.z : b.z ),
	 ( a.w <= b.w ? a.w : b.w )
	);
}

// Minimum of Three Vector4s
template< typename T >
inline
Vector4< T >
min( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	return Vector4< T >(
	 ObjexxFCL::min( a.x, b.x, c.x ),
	 ObjexxFCL::min( a.y, b.y, c.y ),
	 ObjexxFCL::min( a.z, b.z, c.z ),
	 ObjexxFCL::min( a.w, b.w, c.w )
	);
}

// Minimum of Four Vector4s
template< typename T >
inline
Vector4< T >
min( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c, Vector4< T > const & d )
{
	return Vector4< T >(
	 ObjexxFCL::min( a.x, b.x, c.x, d.x ),
	 ObjexxFCL::min( a.y, b.y, c.y, d.y ),
	 ObjexxFCL::min( a.z, b.z, c.z, d.z ),
	 ObjexxFCL::min( a.w, b.w, c.w, d.w )
	);
}

// Maximum of Two Vector4s
template< typename T >
inline
Vector4< T >
max( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >(
	 ( a.x >= b.x ? a.x : b.x ),
	 ( a.y >= b.y ? a.y : b.y ),
	 ( a.z >= b.z ? a.z : b.z ),
	 ( a.w >= b.w ? a.w : b.w )
	);
}

// Maximum of Three Vector4s
template< typename T >
inline
Vector4< T >
max( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	return Vector4< T >(
	 ObjexxFCL::max( a.x, b.x, c.x ),
	 ObjexxFCL::max( a.y, b.y, c.y ),
	 ObjexxFCL::max( a.z, b.z, c.z ),
	 ObjexxFCL::max( a.w, b.w, c.w )
	);
}

// Maximum of Four Vector4s
template< typename T >
inline
Vector4< T >
max( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c, Vector4< T > const & d )
{
	return Vector4< T >(
	 ObjexxFCL::max( a.x, b.x, c.x, d.x ),
	 ObjexxFCL::max( a.y, b.y, c.y, d.y ),
	 ObjexxFCL::max( a.z, b.z, c.z, d.z ),
	 ObjexxFCL::max( a.w, b.w, c.w, d.w )
	);
}

// Sum of Two Vector4s
template< typename T >
inline
Vector4< T >
sum( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >( a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w );
}

// Sum of Three Vector4s
template< typename T >
inline
Vector4< T >
sum( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	return Vector4< T >( a.x + b.x + c.x, a.y + b.y + c.y, a.z + b.z + c.z, a.w + b.w + c.w );
}

// Sum of Four Vector4s
template< typename T >
inline
Vector4< T >
sum( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c, Vector4< T > const & d )
{
	return Vector4< T >( a.x + b.x + c.x + d.x, a.y + b.y + c.y + d.y, a.z + b.z + c.z + d.z, a.w + b.w + c.w + d.w );
}

// Subtract of Two Vector4s
template< typename T >
inline
Vector4< T >
sub( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >( a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w );
}

// Subtract of Two Vector4s
template< typename T >
inline
Vector4< T >
subtract( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >( a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w );
}

// Midpoint of Two Vector4s
template< typename T >
inline
Vector4< T >
mid( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >(
	 T( 0.5 * ( a.x + b.x ) ),
	 T( 0.5 * ( a.y + b.y ) ),
	 T( 0.5 * ( a.z + b.z ) ),
	 T( 0.5 * ( a.w + b.w ) )
	);
}

// Center of Two Vector4s
template< typename T >
inline
Vector4< T >
cen( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >(
	 T( 0.5 * ( a.x + b.x ) ),
	 T( 0.5 * ( a.y + b.y ) ),
	 T( 0.5 * ( a.z + b.z ) ),
	 T( 0.5 * ( a.w + b.w ) )
	);
}

// Center of Three Vector4s
template< typename T >
inline
Vector4< T >
cen( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	static long double const third( 1.0 / 3.0 );
	return Vector4< T >(
	 T( third * ( a.x + b.x + c.x ) ),
	 T( third * ( a.y + b.y + c.y ) ),
	 T( third * ( a.z + b.z + c.z ) ),
	 T( third * ( a.w + b.w + c.w ) )
	);
}

// Center of Four Vector4s
template< typename T >
inline
Vector4< T >
cen( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c, Vector4< T > const & d )
{
	return Vector4< T >(
	 T( 0.25 * ( a.x + b.x + c.x + d.x ) ),
	 T( 0.25 * ( a.y + b.y + c.y + d.y ) ),
	 T( 0.25 * ( a.z + b.z + c.z + d.z ) ),
	 T( 0.25 * ( a.w + b.w + c.w + d.w ) )
	);
}

// Distance
template< typename T >
inline
T
distance( Vector4< T > const & a, Vector4< T > const & b )
{
	return std::sqrt( Vector4< T >::square( a.x - b.x ) + Vector4< T >::square( a.y - b.y ) + Vector4< T >::square( a.z - b.z ) + Vector4< T >::square( a.w - b.w ) );
}

// Distance Squared
template< typename T >
inline
T
distance_squared( Vector4< T > const & a, Vector4< T > const & b )
{
	return Vector4< T >::square( a.x - b.x ) + Vector4< T >::square( a.y - b.y ) + Vector4< T >::square( a.z - b.z ) + Vector4< T >::square( a.w - b.w );
}

// Dot Product
template< typename T >
inline
T
dot( Vector4< T > const & a, Vector4< T > const & b )
{
	return ( a.x * b.x ) + ( a.y * b.y ) + ( a.z * b.z ) + ( a.w * b.w );
}

// Angle Between Two Vector4s (in Radians on [0,pi])
template< typename T >
inline
T
angle( Vector4< T > const & a, Vector4< T > const & b )
{
	T const mag( std::sqrt( a.length_squared() * b.length_squared() ) );
	return ( mag > T( 0 ) ? std::acos( Vector4< T >::sin_cos_range( a.dot( b ) / mag ) ) : T( 0 ) );
}

// Angle abc Formed by Three Vector4s (in Radians on [0,pi])
template< typename T >
inline
T
angle( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	return angle( a - b, c - b );
}

// Cosine of Angle Between Two Vector4s
template< typename T >
inline
T
cos( Vector4< T > const & a, Vector4< T > const & b )
{
	T const mag( std::sqrt( a.length_squared() * b.length_squared() ) );
	return ( mag > T( 0 ) ? Vector4< T >::sin_cos_range( a.dot( b ) / mag ) : T( 1 ) );
}

// Cosine of Angle abc Formed by Three Vector4s
template< typename T >
inline
T
cos( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	return cos( a - b, c - b );
}

// Sine of Angle Between Two Vector4s
template< typename T >
inline
T
sin( Vector4< T > const & a, Vector4< T > const & b )
{
	return std::sin( angle( a, b ) );
}

// Sine of Angle abc Formed by Three Vector4s
template< typename T >
inline
T
sin( Vector4< T > const & a, Vector4< T > const & b, Vector4< T > const & c )
{
	return sin( a - b, c - b );
}

// Stream << Vector4 output operator
template< typename T >
std::ostream &
operator <<( std::ostream & stream, Vector4< T > const & v )
{
	// Types
	typedef  TypeTraits< T >  Traits;

	// Save current stream state and set persistent state
	std::ios_base::fmtflags const old_flags( stream.flags() );
	std::streamsize const old_precision( stream.precision( Traits::precision ) );
	stream << std::right << std::showpoint << std::uppercase;

	// Output Vector4
	std::size_t const w( Traits::width );
	stream << std::setw( w ) << v.x << ' ' << std::setw( w ) << v.y << ' ' << std::setw( w ) << v.z << ' ' << std::setw( w ) << v.w;

	// Restore previous stream state
	stream.precision( old_precision );
	stream.flags( old_flags );

	return stream;
}

// Stream >> Vector4 input operator
//  Supports whitespace-separated values with optional commas between values as long as whitespace is also present
//  String or char values containing whitespace or commas or enclosed in quotes are not supported
//  Vector can optionally be enclosed in parentheses () or square brackets []
template< typename T >
std::istream &
operator >>( std::istream & stream, Vector4< T > & v )
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
		std::string::size_type const input_size( input_string.size() );
		if ( ( input_size > 0 ) && ( input_string[ input_size - 1 ] == ',' ) ) {
			input_string.erase( input_size - 1 ); // Remove trailing ,
		}
		std::istringstream num_stream( input_string );
		num_stream >> v.z;
	}

	{ // w
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
		num_stream >> v.w;
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

#endif // ObjexxFCL_Vector4_hh_INCLUDED
