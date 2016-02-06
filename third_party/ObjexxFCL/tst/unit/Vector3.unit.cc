// ObjexxFCL::Vector3 Unit Tests
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
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/Array1D.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>
#include <cmath>
#include <vector>

using namespace ObjexxFCL;

TEST( Vector3Test, Basic )
{
	Vector3_float v( 15.0 ); // Uniform value construction
	EXPECT_EQ( 15.0f, v.x );
	EXPECT_EQ( 15.0f, v.y );
	EXPECT_EQ( 15.0f, v.z );
	v.normalize();
	EXPECT_FLOAT_EQ( 1.0f, v.length() );
	v.normalize( 5.0f );
	EXPECT_FLOAT_EQ( 5.0f, v.length() );
	v.zero();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	EXPECT_EQ( 0.0f, v.length() );
	v.normalize_zero();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	v.zero();
	v.normalize_x();
	EXPECT_EQ( 1.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	v.zero();
	v.normalize_y();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 1.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	v.zero();
	v.normalize_z();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 1.0f, v.z );
	v.zero();
	v.normalize_uniform();
	EXPECT_EQ( v.x, v.y );
	EXPECT_EQ( v.x, v.z );
	EXPECT_FLOAT_EQ( 1.0f, v.length() );
}

TEST( Vector3Test, InitializerList )
{
	Vector3_int v( { 33, 52, 17 } );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	EXPECT_EQ( 33, v.x1() );
	EXPECT_EQ( 52, v.x2() );
	EXPECT_EQ( 17, v.x3() );
	EXPECT_EQ( 33, v[ 0 ] );
	EXPECT_EQ( 52, v[ 1 ] );
	EXPECT_EQ( 17, v[ 2 ] );
	EXPECT_EQ( 33, v( 1 ) );
	EXPECT_EQ( 52, v( 2 ) );
	EXPECT_EQ( 17, v( 3 ) );
	v = { 44, 55, 66 };
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	EXPECT_EQ( 66, v.z );
	v += 10;
	EXPECT_EQ( 54, v.x );
	EXPECT_EQ( 65, v.y );
	EXPECT_EQ( 76, v.z );
	v -= 10;
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	EXPECT_EQ( 66, v.z );
	v *= 2;
	EXPECT_EQ( 88, v.x );
	EXPECT_EQ( 110, v.y );
	EXPECT_EQ( 132, v.z );
	v /= 2.0;
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	EXPECT_EQ( 66, v.z );
	v.negate();
	EXPECT_EQ( -44, v.x );
	EXPECT_EQ( -55, v.y );
	EXPECT_EQ( -66, v.z );
}

TEST( Vector3Test, StdArray )
{
	std::array< int, 3 > arr = {{ 33, 52, 17 }};
	Vector3_int v( arr );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	arr = {{ 133, 152, 117 }};
	v = arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	v += arr;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	v -= arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	arr = {{ 3, 2, 2 }};
	v *= arr;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	v /= arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
}

TEST( Vector3Test, StdVector )
{
	std::vector< int > vec( { 33, 52, 17 } );
	Vector3_int v( vec );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	vec = { 133, 152, 117 };
	v = vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	v += vec;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	v -= vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	vec = { 3, 2, 2 };
	v *= vec;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	v /= vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
}

TEST( Vector3Test, Array )
{
	Array1D_int a( 3, { 33, 52, 17 } );
	Vector3_int v( a );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	a = { 133, 152, 117 };
	v = a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	v += a;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	v -= a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	a = { 3, 2, 2 };
	v *= a;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	v /= a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
}

TEST( Vector3Test, MinMax )
{
	Vector3_double v( 1.0, 5.0, 3.0 );
	Vector3_double w( 3.0, 2.0, 7.0 );
	Vector3_double min_vw( min( v, w ) );
	Vector3_double max_vw( max( v, w ) );
	EXPECT_EQ( 1.0, min_vw.x );
	EXPECT_EQ( 2.0, min_vw.y );
	EXPECT_EQ( 3.0, min_vw.z );
	EXPECT_EQ( 3.0, max_vw.x );
	EXPECT_EQ( 5.0, max_vw.y );
	EXPECT_EQ( 7.0, max_vw.z );
	v.max( w );
	EXPECT_EQ( 3.0, v.x );
	EXPECT_EQ( 5.0, v.y );
	EXPECT_EQ( 7.0, v.z );
	w.max( v );
	EXPECT_EQ( v, w );
}

TEST( Vector3Test, Comparisons )
{
	Vector3_double v( 1.0, 2.0, 3.0 );
	Vector3_double w( 1.0, 2.0, 3.0 );

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

	// Test partial ordering: Set v.x to 0 but leave v.y > w.y and v.z > w.z so v and w are not orderable
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

TEST( Vector3Test, Generators )
{
	Vector3_double v( 1.0, 12.0, 21.0 );
	Vector3_double w( 2.0, 6.0, 7.0 );
	EXPECT_EQ( Vector3_double( 3.0, 18.0, 28.0 ), v + w );
	EXPECT_EQ( Vector3_double( -1.0, 6.0, 14.0 ), v - w );
	EXPECT_EQ( Vector3_double( 2.0, 72.0, 147.0 ), v * w );
	EXPECT_EQ( Vector3_double( 0.5, 2.0, 3.0 ), v / w );
}

TEST( Vector3Test, Distance )
{
	Vector3_double v( 3.0, 3.0, 0.0 );
	Vector3_double w( 3.0, 2.0, 0.0 );
	EXPECT_DOUBLE_EQ( 1.0, distance( v, w ) );
	EXPECT_DOUBLE_EQ( 1.0, distance_squared( v, w ) );
}

TEST( Vector3Test, Dot )
{
	Vector3_double x( 3.0, 0.0, 0.0 );
	Vector3_double y( 0.0, 2.0, 0.0 );
	EXPECT_EQ( 0.0, dot( x, y ) );
}

TEST( Vector3Test, Cross )
{
	Vector3_double x( 3.0, 0.0, 0.0 );
	Vector3_double y( 0.0, 2.0, 0.0 );
	EXPECT_EQ( Vector3_double( 0.0, 0.0, 6.0 ), cross( x, y ) );
}

TEST( Vector3Test, Center )
{
	Vector3_double x( 4.0, 0.0, 77.0 );
	Vector3_double y( 0.0, 4.0, 77.0 );
	EXPECT_EQ( Vector3_double( 2.0, 2.0, 77.0 ), cen( x, y ) );
}

TEST( Vector3Test, Angle )
{
	double const Pi( std::acos( -1.0 ) );
	double const Pi_2( std::asin( 1.0 ) );
	{
		Vector3_double a( 4.0, 0.0, 0.0 );
		Vector3_double b( 0.0, 4.0, 0.0 );
		EXPECT_DOUBLE_EQ( Pi_2, angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, sin( a, b ) );
	}
	{
		Vector3_double a( 4.0, 0.0, 0.0 );
		Vector3_double b( 0.0, 0.0, -4.0 );
		EXPECT_DOUBLE_EQ( Pi_2, angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, sin( a, b ) );
	}
	{
		Vector3_double a( 0.0, 4.0, 0.0 );
		Vector3_double b( 0.0, -1.0, 0.0 );
		EXPECT_DOUBLE_EQ( Pi, angle( a, b ) );
		EXPECT_DOUBLE_EQ( -1.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, sin( a, b ) );
	}
}

TEST( Vector3Test, BinaryOperations )
{
	Vector3_double v( 1.0, 2.0, 3.0 );
	Vector3_double w( 1.0, 2.0, 3.0 );
	Vector3_double const original( v );

	// Check dot product of equal vectors
	EXPECT_DOUBLE_EQ( v.length_squared(), dot( v, w ) ); // v == w here

	// Tweak the vectors and compute cross product
	v += 1.0; w -= 1.0;
	Vector3_double const c( cross( v, w ) );
	EXPECT_DOUBLE_EQ( dot( c, v ), 0.0 ); // t and v are orthogonal
	EXPECT_DOUBLE_EQ( dot( c, w ), 0.0 ); // t and w are orthogonal

	// Check midpoint (should match original vector)
	Vector3_double const midpoint( mid( v, w ) );
	EXPECT_DOUBLE_EQ( original.x, midpoint.x );
	EXPECT_DOUBLE_EQ( original.y, midpoint.y );
	EXPECT_DOUBLE_EQ( original.z, midpoint.z );
}
