// ObjexxFCL::Vector2 Unit Tests
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Array1D.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>
#include <cmath>
#include <string>
#include <vector>

using namespace ObjexxFCL;

TEST( Vector2Test, Basic )
{
	Vector2_float v( 15.0 ); // Uniform value construction
	EXPECT_EQ( 15.0f, v.x );
	EXPECT_EQ( 15.0f, v.y );
	v.normalize();
	EXPECT_FLOAT_EQ( 1.0f, v.length() );
	v.normalize( 5.0f );
	EXPECT_FLOAT_EQ( 5.0f, v.length() );
	v.zero();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.length() );
	v.normalize_zero();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	v.zero();
	v.normalize_x();
	EXPECT_EQ( 1.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	v.zero();
	v.normalize_y();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 1.0f, v.y );
	v.zero();
	v.normalize_uniform();
	EXPECT_EQ( v.x, v.y );
	EXPECT_FLOAT_EQ( 1.0f, v.length() );
}

TEST( Vector2Test, InitializerList )
{
	Vector2_int v( { 33, 52 } );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 33, v.x1() );
	EXPECT_EQ( 52, v.x2() );
	EXPECT_EQ( 33, v[ 0 ] );
	EXPECT_EQ( 52, v[ 1 ] );
	EXPECT_EQ( 33, v( 1 ) );
	EXPECT_EQ( 52, v( 2 ) );
	v = { 44, 55 };
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	v += 10;
	EXPECT_EQ( 54, v.x );
	EXPECT_EQ( 65, v.y );
	v -= 10;
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	v *= 2;
	EXPECT_EQ( 88, v.x );
	EXPECT_EQ( 110, v.y );
	v /= 2.0;
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	v.negate();
	EXPECT_EQ( -44, v.x );
	EXPECT_EQ( -55, v.y );
}

TEST( Vector2Test, StdArray )
{
	std::array< int, 2 > arr = {{ 33, 52 }};
	Vector2_int v( arr );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	arr = {{ 133, 152 }};
	v = arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	v += arr;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	v -= arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	arr = {{ 3, 2 }};
	v *= arr;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	v /= arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
}

TEST( Vector2Test, StdVector )
{
	std::vector< int > vec( { 33, 52 } );
	Vector2_int v( vec );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	vec = { 133, 152 };
	v = vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	v += vec;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	v -= vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	vec = { 3, 2 };
	v *= vec;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	v /= vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
}

TEST( Vector2Test, Array )
{
	Array1D_int a( 2, { 33, 52 } );
	Vector2_int v( a );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	a = { 133, 152 };
	v = a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	v += a;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	v -= a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	a = { 3, 2 };
	v *= a;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	v /= a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
}

TEST( Vector2Test, MinMax )
{
	Vector2_double v( 1.0, 5.0 );
	Vector2_double w( 3.0, 2.0 );
	Vector2_double min_vw( min( v, w ) );
	Vector2_double max_vw( max( v, w ) );
	EXPECT_EQ( 1.0, min_vw.x );
	EXPECT_EQ( 2.0, min_vw.y );
	EXPECT_EQ( 3.0, max_vw.x );
	EXPECT_EQ( 5.0, max_vw.y );
	v.max( w );
	EXPECT_EQ( 3.0, v.x );
	EXPECT_EQ( 5.0, v.y );
	w.max( v );
	EXPECT_EQ( v, w );
}

TEST( Vector2Test, Comparisons )
{
	Vector2_double v( 1.0, 2.0 );
	Vector2_double w( 1.0, 2.0 );

	EXPECT_EQ( v, w );

	// Reduce v and test inequality
	v -= 0.5;
	EXPECT_TRUE( v != w );
	EXPECT_TRUE( ! ( v == w ) );
	EXPECT_TRUE( v < w );
	EXPECT_TRUE( v <= w );

	// Increase v and test inequality
	v += 1.0;
	EXPECT_TRUE( v != w );
	EXPECT_TRUE( ! ( v == w ) );
	EXPECT_TRUE( v > w );
	EXPECT_TRUE( v >= w );

	// Test partial ordering: Set v.x to 0 but leave v.y > w.y v and w are not orderable
	v.x = 0.0;
	EXPECT_TRUE( v != w );
	EXPECT_TRUE( ! ( v == w ) );
	EXPECT_TRUE( ! lt( v, w ) );
	EXPECT_TRUE( ! le( v, w ) );
	EXPECT_TRUE( ! gt( v, w ) );
	EXPECT_TRUE( ! ge( v, w ) );

	// Test length relations
	EXPECT_TRUE( ! equal_length( v, w ) );
	EXPECT_TRUE( not_equal_length( v, w ) );
}

TEST( Vector2Test, Generators )
{
	Vector2_double v( 1.0, 12.0 );
	Vector2_double w( 2.0, 6.0 );
	EXPECT_EQ( Vector2_double( 3.0, 18.0 ), v + w );
	EXPECT_EQ( Vector2_double( -1.0, 6.0 ), v - w );
	EXPECT_EQ( Vector2_double( 2.0, 72.0 ), v * w );
	EXPECT_EQ( Vector2_double( 0.5, 2.0 ), v / w );
}

TEST( Vector2Test, Distance )
{
	Vector2_double v( 3.0, 3.0 );
	Vector2_double w( 3.0, 2.0 );
	EXPECT_DOUBLE_EQ( 1.0, distance( v, w ) );
	EXPECT_DOUBLE_EQ( 1.0, distance_squared( v, w ) );
}

TEST( Vector2Test, Dot )
{
	Vector2_double x( 3.0, 0.0 );
	Vector2_double y( 0.0, 2.0 );
	EXPECT_EQ( 0.0, dot( x, y ) );
}

TEST( Vector2Test, Cross )
{
	Vector2_double x( 3.0, 0.0 );
	Vector2_double y( 0.0, 2.0 );
	EXPECT_EQ( 6.0, cross( x, y ) );
}

TEST( Vector2Test, Center )
{
	Vector2_double x( 4.0, 0.0 );
	Vector2_double y( 0.0, 4.0 );
	EXPECT_EQ( Vector2_double( 2.0, 2.0 ), cen( x, y ) );
}

TEST( Vector2Test, Angle )
{
	double const Pi( std::acos( -1.0 ) );
	double const Pi_2( std::asin( 1.0 ) );
	{
		Vector2_double a( 4.0, 0.0 );
		Vector2_double b( 0.0, 4.0 );
		EXPECT_DOUBLE_EQ( Pi_2, angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, sin( a, b ) );
		EXPECT_DOUBLE_EQ( Pi_2, dir_angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, dir_cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, dir_sin( a, b ) );
	}
	{
		Vector2_double a( 4.0, 0.0 );
		Vector2_double b( 0.0, -4.0 );
		EXPECT_DOUBLE_EQ( Pi_2, angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, sin( a, b ) );
		EXPECT_DOUBLE_EQ( 3.0 * Pi_2, dir_angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, dir_cos( a, b ) );
		EXPECT_DOUBLE_EQ( -1.0, dir_sin( a, b ) );
	}
	{
		Vector2_double a( 4.0, 0.0 );
		Vector2_double b( -1.0, 0.0 );
		EXPECT_DOUBLE_EQ( Pi, angle( a, b ) );
		EXPECT_DOUBLE_EQ( -1.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, sin( a, b ) );
		EXPECT_DOUBLE_EQ( Pi, dir_angle( a, b ) );
		EXPECT_DOUBLE_EQ( -1.0, dir_cos( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, dir_sin( a, b ) );
	}
}

TEST( Vector2Test, BinaryOperations )
{
	Vector2_double v( 1.0, 2.0 );
	Vector2_double w( 1.0, 2.0 );
	Vector2_double const original( v );

	// Check dot product of equal vectors
	EXPECT_DOUBLE_EQ( v.length_squared(), dot( v, w ) ); // v == w here

	// Check midpoint (should match original vector)
	v += 1.0; w -= 1.0;
	Vector2_double const midpoint( mid( v, w ) );
	EXPECT_DOUBLE_EQ( original.x, midpoint.x );
	EXPECT_DOUBLE_EQ( original.y, midpoint.y );
}

TEST( Vector2Test, String )
{
	std::string const X( "X" );
	Vector2_string v( X );
	EXPECT_EQ( X, v.x );
	EXPECT_EQ( X, v.y );
	Vector2_string w;
	w = v;
	EXPECT_EQ( X, w.x );
	EXPECT_EQ( X, w.y );
}
