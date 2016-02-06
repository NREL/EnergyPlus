// ObjexxFCL::Vector4 Unit Tests
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
#include <ObjexxFCL/Vector4.hh>
#include <ObjexxFCL/Array1D.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>
#include <cmath>
#include <vector>

using namespace ObjexxFCL;

TEST( Vector4Test, Basic )
{
	Vector4_float v( 15.0 ); // Uniform value construction
	EXPECT_EQ( 15.0f, v.x );
	EXPECT_EQ( 15.0f, v.y );
	EXPECT_EQ( 15.0f, v.z );
	EXPECT_EQ( 15.0f, v.w );
	v.normalize();
	EXPECT_FLOAT_EQ( 1.0f, v.length() );
	v.normalize( 5.0f );
	EXPECT_FLOAT_EQ( 5.0f, v.length() );
	v.zero();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	EXPECT_EQ( 0.0f, v.w );
	EXPECT_EQ( 0.0f, v.length() );
	v.normalize_zero();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	EXPECT_EQ( 0.0f, v.w );
	v.zero();
	v.normalize_x();
	EXPECT_EQ( 1.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	EXPECT_EQ( 0.0f, v.w );
	v.zero();
	v.normalize_y();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 1.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	EXPECT_EQ( 0.0f, v.w );
	v.zero();
	v.normalize_z();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 1.0f, v.z );
	EXPECT_EQ( 0.0f, v.w );
	v.zero();
	v.normalize_w();
	EXPECT_EQ( 0.0f, v.x );
	EXPECT_EQ( 0.0f, v.y );
	EXPECT_EQ( 0.0f, v.z );
	EXPECT_EQ( 1.0f, v.w );
	v.zero();
	v.normalize_uniform();
	EXPECT_EQ( v.x, v.y );
	EXPECT_EQ( v.x, v.z );
	EXPECT_EQ( v.x, v.w );
	EXPECT_FLOAT_EQ( 1.0f, v.length() );
}

TEST( Vector4Test, InitializerList )
{
	Vector4_int v( { 33, 52, 17, 42 } );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	EXPECT_EQ( 42, v.w );
	EXPECT_EQ( 33, v.x1() );
	EXPECT_EQ( 52, v.x2() );
	EXPECT_EQ( 17, v.x3() );
	EXPECT_EQ( 42, v.x4() );
	EXPECT_EQ( 33, v[ 0 ] );
	EXPECT_EQ( 52, v[ 1 ] );
	EXPECT_EQ( 17, v[ 2 ] );
	EXPECT_EQ( 42, v[ 3 ] );
	EXPECT_EQ( 33, v( 1 ) );
	EXPECT_EQ( 52, v( 2 ) );
	EXPECT_EQ( 17, v( 3 ) );
	EXPECT_EQ( 42, v( 4 ) );
	v = { 44, 55, 66, 77 };
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	EXPECT_EQ( 66, v.z );
	EXPECT_EQ( 77, v.w );
	v += 10;
	EXPECT_EQ( 54, v.x );
	EXPECT_EQ( 65, v.y );
	EXPECT_EQ( 76, v.z );
	EXPECT_EQ( 87, v.w );
	v -= 10;
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	EXPECT_EQ( 66, v.z );
	EXPECT_EQ( 77, v.w );
	v *= 2;
	EXPECT_EQ( 88, v.x );
	EXPECT_EQ( 110, v.y );
	EXPECT_EQ( 132, v.z );
	EXPECT_EQ( 154, v.w );
	v /= 2.0;
	EXPECT_EQ( 44, v.x );
	EXPECT_EQ( 55, v.y );
	EXPECT_EQ( 66, v.z );
	EXPECT_EQ( 77, v.w );
	v.negate();
	EXPECT_EQ( -44, v.x );
	EXPECT_EQ( -55, v.y );
	EXPECT_EQ( -66, v.z );
	EXPECT_EQ( -77, v.w );
}

TEST( Vector4Test, StdArray )
{
	std::array< int, 4 > arr = {{ 33, 52, 17, 42 }};
	Vector4_int v( arr );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	EXPECT_EQ( 42, v.w );
	arr = {{ 133, 152, 117, 123 }};
	v = arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
	v += arr;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	EXPECT_EQ( 246, v.w );
	v -= arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
	arr = {{ 3, 2, 2, 3 }};
	v *= arr;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	EXPECT_EQ( 369, v.w );
	v /= arr;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
}

TEST( Vector4Test, StdVector )
{
	std::vector< int > vec( { 33, 52, 17, 42 } );
	Vector4_int v( vec );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	EXPECT_EQ( 42, v.w );
	vec = { 133, 152, 117, 123 };
	v = vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
	v += vec;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	EXPECT_EQ( 246, v.w );
	v -= vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
	vec = { 3, 2, 2, 3 };
	v *= vec;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	EXPECT_EQ( 369, v.w );
	v /= vec;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
}

TEST( Vector4Test, Array )
{
	Array1D_int a( 4, { 33, 52, 17, 42 } );
	Vector4_int v( a );
	EXPECT_EQ( 33, v.x );
	EXPECT_EQ( 52, v.y );
	EXPECT_EQ( 17, v.z );
	EXPECT_EQ( 42, v.w );
	a = { 133, 152, 117, 123 };
	v = a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
	v += a;
	EXPECT_EQ( 266, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	EXPECT_EQ( 246, v.w );
	v -= a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
	a = { 3, 2, 2, 3 };
	v *= a;
	EXPECT_EQ( 399, v.x );
	EXPECT_EQ( 304, v.y );
	EXPECT_EQ( 234, v.z );
	EXPECT_EQ( 369, v.w );
	v /= a;
	EXPECT_EQ( 133, v.x );
	EXPECT_EQ( 152, v.y );
	EXPECT_EQ( 117, v.z );
	EXPECT_EQ( 123, v.w );
}

TEST( Vector4Test, MinMax )
{
	Vector4_double v( 1.0, 5.0, 3.0, 2.0 );
	Vector4_double w( 3.0, 2.0, 7.0, 5.0 );
	Vector4_double min_vw( min( v, w ) );
	Vector4_double max_vw( max( v, w ) );
	EXPECT_EQ( 1.0, min_vw.x );
	EXPECT_EQ( 2.0, min_vw.y );
	EXPECT_EQ( 3.0, min_vw.z );
	EXPECT_EQ( 2.0, min_vw.w );
	EXPECT_EQ( 3.0, max_vw.x );
	EXPECT_EQ( 5.0, max_vw.y );
	EXPECT_EQ( 7.0, max_vw.z );
	EXPECT_EQ( 5.0, max_vw.w );
	v.max( w );
	EXPECT_EQ( 3.0, v.x );
	EXPECT_EQ( 5.0, v.y );
	EXPECT_EQ( 7.0, v.z );
	EXPECT_EQ( 5.0, v.w );
	w.max( v );
	EXPECT_EQ( v, w );
}

TEST( Vector4Test, Comparisons )
{
	Vector4_double v( 1.0, 2.0, 3.0, 4.0 );
	Vector4_double w( 1.0, 2.0, 3.0, 4.0 );

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

	// Test partial ordering: Set v.x to 0 but leave v.y > w.y and v.z > w.z and v.w > w.w so v and w are not orderable
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

TEST( Vector4Test, Generators )
{
	Vector4_double v( 1.0, 12.0, 21.0, 18.0 );
	Vector4_double w( 2.0, 6.0, 7.0, 9.0 );
	EXPECT_EQ( Vector4_double( 3.0, 18.0, 28.0, 27.0 ), v + w );
	EXPECT_EQ( Vector4_double( -1.0, 6.0, 14.0, 9.0 ), v - w );
	EXPECT_EQ( Vector4_double( 2.0, 72.0, 147.0, 162.0 ), v * w );
	EXPECT_EQ( Vector4_double( 0.5, 2.0, 3.0, 2.0 ), v / w );
}

TEST( Vector4Test, Distance )
{
	Vector4_double v( 3.0, 3.0, 0.0, 1.0 );
	Vector4_double w( 3.0, 2.0, 0.0, 1.0 );
	EXPECT_DOUBLE_EQ( 1.0, distance( v, w ) );
	EXPECT_DOUBLE_EQ( 1.0, distance_squared( v, w ) );
}

TEST( Vector4Test, Dot )
{
	Vector4_double x( 3.0, 0.0, 0.0, 5.0 );
	Vector4_double y( 0.0, 2.0, 0.0, 0.0 );
	EXPECT_EQ( 0.0, dot( x, y ) );
}

TEST( Vector4Test, Center )
{
	Vector4_double x( 4.0, 0.0, 77.0, 42.0 );
	Vector4_double y( 0.0, 4.0, 77.0, 42.0 );
	EXPECT_EQ( Vector4_double( 2.0, 2.0, 77.0, 42.0 ), cen( x, y ) );
}

TEST( Vector4Test, Angle )
{
	double const Pi( std::acos( -1.0 ) );
	double const Pi_2( std::asin( 1.0 ) );
	{
		Vector4_double a( 4.0, 0.0, 0.0, 0.0 );
		Vector4_double b( 0.0, 4.0, 0.0, 0.0 );
		EXPECT_DOUBLE_EQ( Pi_2, angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, sin( a, b ) );
	}
	{
		Vector4_double a( 4.0, 0.0, 0.0, 0.0 );
		Vector4_double b( 0.0, 0.0, -6.0, 0.0 );
		EXPECT_DOUBLE_EQ( Pi_2, angle( a, b ) );
		EXPECT_DOUBLE_EQ( 0.0, cos( a, b ) );
		EXPECT_DOUBLE_EQ( 1.0, sin( a, b ) );
	}
	{
		Vector4_double a( 0.0, 4.0, 0.0, 0.0 );
		Vector4_double b( 0.0, -1.0, 0.0, 0.0 );
		EXPECT_DOUBLE_EQ( Pi, angle( a, b ) );
		EXPECT_DOUBLE_EQ( -1.0, cos( a, b ) );
		EXPECT_NEAR( 0.0, sin( a, b ), 1.0E-15 ); // EXPECT_DOUBLE_EQ tolerance is too small
	}
}

TEST( Vector4Test, BinaryOperations )
{
	Vector4_double v( 1.0, 2.0, 3.0, 4.0 );
	Vector4_double w( 1.0, 2.0, 3.0, 4.0 );
	Vector4_double const original( v );

	// Check dot product of equal vectors
	EXPECT_DOUBLE_EQ( v.length_squared(), dot( v, w ) ); // v == w here

	// Tweak the vectors
	v += 1.0; w -= 1.0;

	// Check midpoint (should match original vector)
	Vector4_double const midpoint( mid( v, w ) );
	EXPECT_DOUBLE_EQ( original.x, midpoint.x );
	EXPECT_DOUBLE_EQ( original.y, midpoint.y );
	EXPECT_DOUBLE_EQ( original.z, midpoint.z );
}
