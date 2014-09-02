// ObjexxFCL::FArray2 Unit Tests
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/FArray2.all.hh>
#include <ObjexxFCL/FArray2.io.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/DimensionExpressions.hh>
#include <ObjexxFCL/FArray.functions.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <array>
#include <vector>

using namespace ObjexxFCL;

TEST( FArray2Test, ConstructDefault )
{
	FArray2D_int A1;
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 0u, A1.size1() );
	EXPECT_EQ( 0u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 0, A1.u1() );
	EXPECT_EQ( 0, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR(), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR(), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_FALSE( A1.initializer_active() );

	FArray2D_int const C1;
	EXPECT_EQ( 0u, C1.size() );
	EXPECT_EQ( 0u, C1.size1() );
	EXPECT_EQ( 0u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 0, C1.u1() );
	EXPECT_EQ( 0, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR(), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR(), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_FALSE( C1.initializer_active() );
}

TEST( FArray2Test, ConstructCopy )
{
	FArray2D_int A1;
	FArray2D_int A2( A1 );
	EXPECT_EQ( A1.size(), A2.size() );
	EXPECT_EQ( A1.size1(), A2.size1() );
	EXPECT_EQ( A1.size2(), A2.size2() );
	EXPECT_EQ( A1.l1(), A2.l1() );
	EXPECT_EQ( A1.u1(), A2.u1() );
	EXPECT_EQ( A1.l2(), A2.l2() );
	EXPECT_EQ( A1.u2(), A2.u2() );
	EXPECT_EQ( A1.I1(), A2.I1() );
	EXPECT_EQ( A1.I2(), A2.I2() );
	EXPECT_EQ( A1.dimensions_initialized(), A2.dimensions_initialized() );
	EXPECT_EQ( A1.initializer_active(), A2.initializer_active() );
	EXPECT_TRUE( conformable( A1, A2 ) );
	EXPECT_TRUE( equal_dimensions( A1, A2 ) );
	EXPECT_TRUE( eq( A1, A2 ) );

	FArray2D_int const C1;
	FArray2D_int const C2( C1 );
	EXPECT_EQ( C1.size(), C2.size() );
	EXPECT_EQ( C1.size1(), C2.size1() );
	EXPECT_EQ( C1.size2(), C2.size2() );
	EXPECT_EQ( C1.l2(), C2.l2() );
	EXPECT_EQ( C1.u2(), C2.u2() );
	EXPECT_EQ( C1.l1(), C2.l1() );
	EXPECT_EQ( C1.u1(), C2.u1() );
	EXPECT_EQ( C1.I1(), C2.I1() );
	EXPECT_EQ( C1.I2(), C2.I2() );
	EXPECT_EQ( C1.dimensions_initialized(), C2.dimensions_initialized() );
	EXPECT_EQ( C1.initializer_active(), C2.initializer_active() );
	EXPECT_TRUE( eq( C1, C2 ) );
}

TEST( FArray2Test, ConstructOtherData )
{
	// Avoid dependency on static list constructor
	FArray2D_double A1( 2, 3 );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 )
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 )
			A1( i1, i2 ) = i1 + i2;
	FArray2D_int A2( A1 );
	EXPECT_EQ( A1.size(), A2.size() );
	EXPECT_EQ( A1.size1(), A2.size1() );
	EXPECT_EQ( A1.size2(), A2.size2() );
	EXPECT_EQ( A1.l1(), A2.l1() );
	EXPECT_EQ( A1.u1(), A2.u1() );
	EXPECT_EQ( A1.l2(), A2.l2() );
	EXPECT_EQ( A1.u2(), A2.u2() );
	EXPECT_EQ( A1.I1(), A2.I1() );
	EXPECT_EQ( A1.I2(), A2.I2() );
	EXPECT_EQ( A1.dimensions_initialized(), A2.dimensions_initialized() );
	EXPECT_EQ( A1.initializer_active(), A2.initializer_active() );
	EXPECT_TRUE( conformable( A1, A2 ) );
	EXPECT_TRUE( equal_dimensions( A1, A2 ) );
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( int( A1( i1, i2 ) ), A2( i1, i2 ) );
			EXPECT_DOUBLE_EQ( A1( i1, i2 ), double( A2( i1, i2 ) ) ); // Works because they are all integer values
		}
	}

	// No way to avoid dependency on value constructor
 	FArray2D_double const C1( 2, 3, 3.1459 );
	FArray2D_int const C2( C1 );
	EXPECT_EQ( C1.size(), C2.size() );
	EXPECT_EQ( C1.size1(), C2.size1() );
	EXPECT_EQ( C1.size2(), C2.size2() );
	EXPECT_EQ( C1.I1(), C2.I1() );
	EXPECT_EQ( C1.I2(), C2.I2() );
	EXPECT_EQ( C1.l1(), C2.l1() );
	EXPECT_EQ( C1.l2(), C2.l2() );
	EXPECT_EQ( C1.u1(), C2.u1() );
	EXPECT_EQ( C1.u2(), C2.u2() );
	EXPECT_EQ( C1.dimensions_initialized(), C2.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
	EXPECT_FALSE( C2.initializer_active() );
	EXPECT_TRUE( conformable( C1, C2 ) );
	EXPECT_TRUE( equal_dimensions( C1, C2 ) );
	for ( int i2 = C2.l2(); i2 <= C2.u2(); ++i2 )
		for ( int i1 = C2.l1(); i1 <= C2.u1(); ++i1 )
			EXPECT_DOUBLE_EQ( 3.0, double( C2( i1, i2 ) ) );
}

TEST( FArray2Test, ConstructProxy )
{
	FArray2D_int A1( 2, 3, 31459 );
	FArray2P_int A2( A1 );
	FArray2D_int A3( A2 );
 	EXPECT_EQ( A2.size(), A3.size() );
	EXPECT_EQ( A2.size1(), A3.size1() );
	EXPECT_EQ( A2.size2(), A3.size2() );
	EXPECT_EQ( A2.I1(), A3.I1() );
	EXPECT_EQ( A2.I2(), A3.I2() );
	EXPECT_EQ( A2.l1(), A3.l1() );
	EXPECT_EQ( A2.u1(), A3.u1() );
	EXPECT_EQ( A2.l2(), A3.l2() );
	EXPECT_EQ( A2.u2(), A3.u2() );
	EXPECT_EQ( A2.dimensions_initialized(), A3.dimensions_initialized() );
	EXPECT_EQ( A2.initializer_active(), A3.initializer_active() );
	EXPECT_TRUE( conformable( A1, A3 ) );
	EXPECT_TRUE( equal_dimensions( A1, A3 ) );
	EXPECT_TRUE( eq( A2, A3 ) );

	FArray2D_int const C1( 2, 3, 31459 );
	FArray2P_int const C2( C1 );
	FArray2D_int const C3( C2 );
	EXPECT_EQ( C2.size(), C3.size() );
	EXPECT_EQ( C2.size1(), C3.size1() );
	EXPECT_EQ( C2.size2(), C3.size2() );
	EXPECT_EQ( C2.l1(), C3.l1() );
	EXPECT_EQ( C2.l2(), C3.l2() );
	EXPECT_EQ( C2.u1(), C3.u1() );
	EXPECT_EQ( C2.u2(), C3.u2() );
	EXPECT_EQ( C2.dimensions_initialized(), C3.dimensions_initialized() );
	EXPECT_EQ( C2.initializer_active(), C3.initializer_active() );
	EXPECT_TRUE( conformable( C1, C3 ) );
	EXPECT_TRUE( equal_dimensions( C1, C3 ) );
	EXPECT_TRUE( eq( C2, C3 ) );
}

TEST( FArray2Test, ConstructArgument )
{
	FArray2D_int A1( 2, 3, 31459 );
	FArray2A_int A2( A1 );
	FArray2D_int A3( A2 );
 	EXPECT_EQ( A2.size(), A3.size() );
	EXPECT_EQ( A2.size1(), A3.size1() );
	EXPECT_EQ( A2.size2(), A3.size2() );
	EXPECT_EQ( A2.I1(), A3.I1() );
	EXPECT_EQ( A2.I2(), A3.I2() );
	EXPECT_EQ( A2.l1(), A3.l1() );
	EXPECT_EQ( A2.u1(), A3.u1() );
	EXPECT_EQ( A2.l2(), A3.l2() );
	EXPECT_EQ( A2.u2(), A3.u2() );
	EXPECT_EQ( A2.dimensions_initialized(), A3.dimensions_initialized() );
	EXPECT_EQ( A2.initializer_active(), A3.initializer_active() );
	EXPECT_TRUE( conformable( A1, A3 ) );
	EXPECT_TRUE( equal_dimensions( A1, A3 ) );
	EXPECT_TRUE( eq( A2, A3 ) );

 	FArray2D_int const C1( 2, 3, 31459 );
	FArray2P_int const C2( C1 );
	FArray2D_int const C3( C2 );
	EXPECT_EQ( C2.size(), C3.size() );
	EXPECT_EQ( C2.size1(), C3.size1() );
	EXPECT_EQ( C2.size2(), C3.size2() );
	EXPECT_EQ( C2.l1(), C3.l1() );
	EXPECT_EQ( C2.l2(), C3.l2() );
	EXPECT_EQ( C2.u1(), C3.u1() );
	EXPECT_EQ( C2.u2(), C3.u2() );
	EXPECT_EQ( C2.dimensions_initialized(), C3.dimensions_initialized() );
	EXPECT_EQ( C2.initializer_active(), C3.initializer_active() );
	EXPECT_TRUE( conformable( C1, C3 ) );
	EXPECT_TRUE( equal_dimensions( C1, C3 ) );
	EXPECT_TRUE( eq( C2, C3 ) );
}

TEST( FArray2Test, ConstructSticky )
{
	FArray2D_int A1( Sticky_int( 31459 ) );
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 0u, A1.size1() );
	EXPECT_EQ( 0u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 0, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 0, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR(), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR(), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	A1.dimension( 2, 2 );
	EXPECT_EQ( 31459, A1( 1, 1 ) );
	EXPECT_EQ( 31459, A1( 1, 2 ) );
	EXPECT_EQ( 31459, A1( 2, 1 ) );
	EXPECT_EQ( 31459, A1( 2, 2 ) );

	FArray2D_int const C1( Sticky_int( 31459 ) ); // Not a useful array: Can never hold anything
	EXPECT_EQ( 0u, C1.size() );
	EXPECT_EQ( 0u, C1.size1() );
	EXPECT_EQ( 0u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 0, C1.u1() );
	EXPECT_EQ( 0, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR(), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR(), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
}

TEST( FArray2Test, ConstructIndexes )
{
	FArray2D_int A1( 8, 10 );
	EXPECT_EQ( 80u, A1.size() );
	EXPECT_EQ( 8u, A1.size1() );
	EXPECT_EQ( 10u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 8, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 10, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 8 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 10 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_FALSE( A1.initializer_active() );

	FArray2D_int const C1( 8, 10 );
	EXPECT_EQ( 80u, C1.size() );
	EXPECT_EQ( 8u, C1.size1() );
	EXPECT_EQ( 10u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 8, C1.u1() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 10, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 8 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 10 ), A1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_FALSE( C1.initializer_active() );
}

TEST( FArray2Test, ConstructIndexRange )
{
	// Explicit index range, positive bounds
	FArray2D_int A11( SRange( 2, 5 ), SRange( 3, 7 ) );
	EXPECT_EQ( 20u, A11.size() );
	EXPECT_EQ( 4u, A11.size1() );
	EXPECT_EQ( 5u, A11.size2() );
	EXPECT_EQ( 2, A11.l1() );
	EXPECT_EQ( 5, A11.u1() );
	EXPECT_EQ( 3, A11.l2() );
	EXPECT_EQ( 7, A11.u2() );
	EXPECT_EQ( FArray2D_int::IR( 2, 5 ), A11.I1() );
	EXPECT_EQ( FArray2D_int::IR( 3, 7 ), A11.I2() );
	EXPECT_TRUE( A11.dimensions_initialized() );
	EXPECT_FALSE( A11.initializer_active() );

	// Explicit index range, negative bounds
 	FArray2D_int A12( SRange( -5, -2 ), SRange( -7, -3 ) );
	EXPECT_EQ( 20u, A12.size() );
	EXPECT_EQ( 4u, A12.size1() );
	EXPECT_EQ( 5u, A12.size2() );
	EXPECT_EQ( -5, A12.l1() );
	EXPECT_EQ( -2, A12.u1() );
	EXPECT_EQ( -7, A12.l2() );
	EXPECT_EQ( -3, A12.u2() );
	EXPECT_EQ( FArray2D_int::IR( -5, -2 ), A12.I1() );
	EXPECT_EQ( FArray2D_int::IR( -7, -3 ), A12.I2() );
	EXPECT_TRUE( A12.dimensions_initialized() );
	EXPECT_FALSE( A12.initializer_active() );

	// Explicit index range, bounds that cross zero
	FArray2D_int A13( SRange( -3, 3 ), SRange( -2, 2 ) );
	EXPECT_EQ( 35u, A13.size() );
	EXPECT_EQ( 7u, A13.size1() );
	EXPECT_EQ( 5u, A13.size2() );
	EXPECT_EQ( -3, A13.l1() );
	EXPECT_EQ( 3, A13.u1() );
	EXPECT_EQ( -2, A13.l2() );
	EXPECT_EQ( 2, A13.u2() );
	EXPECT_EQ( FArray2D_int::IR( -3, 3 ), A13.I1() );
	EXPECT_EQ( FArray2D_int::IR( -2, 2 ), A13.I2() );
	EXPECT_TRUE( A13.dimensions_initialized() );
	EXPECT_FALSE( A13.initializer_active() );

	// Index range initializer list, positive bounds
	FArray2D_int A21( { 2, 5 }, { 3, 7 } );
	EXPECT_EQ( 20u, A21.size() );
	EXPECT_EQ( 4u, A21.size1() );
	EXPECT_EQ( 5u, A21.size2() );
	EXPECT_EQ( 2, A21.l1() );
	EXPECT_EQ( 5, A21.u1() );
	EXPECT_EQ( 3, A21.l2() );
	EXPECT_EQ( 7, A21.u2() );
	EXPECT_EQ( FArray2D_int::IR( 2, 5 ), A21.I1() );
	EXPECT_EQ( FArray2D_int::IR( 3, 7 ), A21.I2() );
	EXPECT_TRUE( A21.dimensions_initialized() );
	EXPECT_FALSE( A21.initializer_active() );

	// Index range initializer list, negative bounds
	FArray2D_int A22( { -5, -2 }, { -7, -3 } );
	EXPECT_EQ( 20u, A22.size() );
	EXPECT_EQ( 4u, A22.size1() );
	EXPECT_EQ( 5u, A22.size2() );
	EXPECT_EQ( -5, A22.l1() );
	EXPECT_EQ( -2, A22.u1() );
	EXPECT_EQ( -7, A22.l2() );
	EXPECT_EQ( -3, A22.u2() );
	EXPECT_EQ( FArray2D_int::IR( -5, -2 ), A22.I1() );
	EXPECT_EQ( FArray2D_int::IR( -7, -3 ), A22.I2() );
	EXPECT_TRUE( A22.dimensions_initialized() );
	EXPECT_FALSE( A22.initializer_active() );

	// Index range initializer list, bounds that cross zero
	FArray2D_int A23( { -3, 3 }, { -2, 2 } );
	EXPECT_EQ( 35u, A23.size() );
	EXPECT_EQ( 7u, A23.size1() );
	EXPECT_EQ( 5u, A23.size2() );
	EXPECT_EQ( -3, A23.l1() );
	EXPECT_EQ( 3, A23.u1() );
	EXPECT_EQ( -2, A23.l2() );
	EXPECT_EQ( 2, A23.u2() );
	EXPECT_EQ( FArray2D_int::IR( -3, 3 ), A23.I1() );
	EXPECT_EQ( FArray2D_int::IR( -2, 2 ), A23.I2() );
	EXPECT_TRUE( A23.dimensions_initialized() );
	EXPECT_FALSE( A23.initializer_active() );
}

TEST( FArray2Test, ConstructIndexesInitializerValue )
{
	FArray2D_int A1( 8, 10, 31459 );
	EXPECT_EQ( 80u, A1.size() );
	EXPECT_EQ( 8u, A1.size1() );
	EXPECT_EQ( 10u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 8, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 10, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 8 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 10 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A1( i1, i2 ) );
		}
	}

	FArray2D_int const C1( 8, 10, 31459 );
	EXPECT_EQ( 80u, C1.size() );
	EXPECT_EQ( 8u, C1.size1() );
	EXPECT_EQ( 10u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 8, C1.u1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 10, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 8 ), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 10 ), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
	for ( int i2 = C1.l2(); i2 <= C1.u2(); ++i2 ) {
		for ( int i1 = C1.l1(); i1 <= C1.u1(); ++i1 ) {
			EXPECT_EQ( 31459, C1( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexRangeInitializerValue )
{
	// Explicit index range
	FArray2D_int A11( SRange( 2, 5 ), SRange( 3, 7 ), 31459 );
	EXPECT_EQ( 20u, A11.size() );
	EXPECT_EQ( 4u, A11.size1() );
	EXPECT_EQ( 5u, A11.size2() );
	EXPECT_EQ( 2, A11.l1() );
	EXPECT_EQ( 5, A11.u1() );
	EXPECT_EQ( 3, A11.l2() );
	EXPECT_EQ( 7, A11.u2() );
	EXPECT_EQ( FArray2D_int::IR( 2, 5 ), A11.I1() );
	EXPECT_EQ( FArray2D_int::IR( 3, 7 ), A11.I2() );
	EXPECT_TRUE( A11.dimensions_initialized() );
	EXPECT_TRUE( A11.initializer_active() );
	for ( int i2 = A11.l2(); i2 <= A11.u2(); ++i2 ) {
		for ( int i1 = A11.l1(); i1 <= A11.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A11( i1, i2 ) );
		}
	}

	// Explicit index range, bounds that cross zero
	FArray2D_int A12( SRange( -3, 3 ), SRange( -2, 2 ), -31459 );
	EXPECT_EQ( 35u, A12.size() );
	EXPECT_EQ( 7u, A12.size1() );
	EXPECT_EQ( 5u, A12.size2() );
	EXPECT_EQ( -3, A12.l1() );
	EXPECT_EQ( 3, A12.u1() );
	EXPECT_EQ( -2, A12.l2() );
	EXPECT_EQ( 2, A12.u2() );
	EXPECT_EQ( FArray2D_int::IR( -3, 3 ), A12.I1() );
	EXPECT_EQ( FArray2D_int::IR( -2, 2 ), A12.I2() );
	EXPECT_TRUE( A12.dimensions_initialized() );
	EXPECT_TRUE( A12.initializer_active() );
	for ( int i2 = A12.l2(); i2 <= A12.u2(); ++i2 ) {
		for ( int i1 = A12.l1(); i1 <= A12.u1(); ++i1 ) {
			EXPECT_EQ( -31459, A12( i1, i2 ) );
		}
	}

	// Index range initializer list
	FArray2D_int A21( { 2, 5 }, { 3, 7 }, 2718 );
	EXPECT_EQ( 20u, A21.size() );
	EXPECT_EQ( 4u, A21.size1() );
	EXPECT_EQ( 5u, A21.size2() );
	EXPECT_EQ( 2, A21.l1() );
	EXPECT_EQ( 5, A21.u1() );
	EXPECT_EQ( 3, A21.l2() );
	EXPECT_EQ( 7, A21.u2() );
	EXPECT_EQ( FArray2D_int::IR( 2, 5 ), A21.I1() );
	EXPECT_EQ( FArray2D_int::IR( 3, 7 ), A21.I2() );
	EXPECT_TRUE( A21.dimensions_initialized() );
	EXPECT_TRUE( A21.initializer_active() );
	for ( int i2 = A21.l2(); i2 <= A21.u2(); ++i2 ) {
		for ( int i1 = A21.l1(); i1 <= A21.u1(); ++i1 ) {
			EXPECT_EQ( 2718, A21( i1, i2 ) );
		}
	}

	// Index range initializer list, bounds that cross zero
	FArray2D_int A22( { -3, 3 }, { -2, 2 }, -2718 );
	EXPECT_EQ( 35u, A22.size() );
	EXPECT_EQ( 7u, A22.size1() );
	EXPECT_EQ( 5u, A22.size2() );
	EXPECT_EQ( -3, A22.l1() );
	EXPECT_EQ( 3, A22.u1() );
	EXPECT_EQ( -2, A22.l2() );
	EXPECT_EQ( 2, A22.u2() );
	EXPECT_EQ( FArray2D_int::IR( -3, 3 ), A22.I1() );
	EXPECT_EQ( FArray2D_int::IR( -2, 2 ), A22.I2() );
	EXPECT_TRUE( A22.dimensions_initialized() );
	EXPECT_TRUE( A22.initializer_active() );
	for ( int i2 = A22.l2(); i2 <= A22.u2(); ++i2 ) {
		for ( int i1 = A22.l1(); i1 <= A22.u1(); ++i1 ) {
			EXPECT_EQ( -2718, A22( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexesSticky )
{
	FArray2D_int A1( 2, 3, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 2718, A1( i1, i2 ) );
		}
	}
	A1.dimension( 2, 2 );
	EXPECT_EQ( 31459, A1( 1, 1 ) );
	EXPECT_EQ( 31459, A1( 1, 2 ) );
	EXPECT_EQ( 31459, A1( 2, 1 ) );
	EXPECT_EQ( 31459, A1( 2, 2 ) );

	FArray2D_int const C1( 2, 3, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, C1.size() );
	EXPECT_EQ( 2u, C1.size1() );
	EXPECT_EQ( 3u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 2, C1.u1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 3, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
	for ( int i2 = C1.l2(); i2 <= C1.u2(); ++i2 ) {
		for ( int i1 = C1.l1(); i1 <= C1.u1(); ++i1 ) {
			EXPECT_EQ( 2718, C1( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexRangeSticky )
{
	FArray2D_int A1( { 1, 2 }, { 1, 3 }, Sticky_int( 31459 ) );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	EXPECT_EQ( 31459, A1( 1, 1) );
	EXPECT_EQ( 31459, A1( 1, 2 ) );
	EXPECT_EQ( 31459, A1( 1, 3 ) );
	EXPECT_EQ( 31459, A1( 2, 1 ) );
	EXPECT_EQ( 31459, A1( 2, 2 ) );
	EXPECT_EQ( 31459, A1( 2, 3 ) );
	A1.dimension( 2, 2 );
	EXPECT_EQ( 31459, A1( 1, 1 ) );
	EXPECT_EQ( 31459, A1( 1, 2 ) );
	EXPECT_EQ( 31459, A1( 2, 1 ) );
	EXPECT_EQ( 31459, A1( 2, 2 ) );

	FArray2D_int const C1( { 1, 2 }, { 1, 3 }, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, C1.size() );
	EXPECT_EQ( 2u, C1.size1() );
	EXPECT_EQ( 3u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 2, C1.u1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 3, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
	EXPECT_EQ( 2718, C1( 1, 1) );
	EXPECT_EQ( 2718, C1( 1, 2 ) );
	EXPECT_EQ( 2718, C1( 1, 3 ) );
	EXPECT_EQ( 2718, C1( 2, 1 ) );
	EXPECT_EQ( 2718, C1( 2, 2 ) );
	EXPECT_EQ( 2718, C1( 2, 3 ) );
}

TEST( FArray2Test, ConstructIndexesStickyInitializerValue )
{
	FArray2D_int A1( 2, 3, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 2718, A1( i1, i2 ) );
		}
	}

	FArray2D_int const C1( 2, 3, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, C1.size() );
	EXPECT_EQ( 2u, C1.size1() );
	EXPECT_EQ( 3u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 2, C1.u1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 3, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
	for ( int i2 = C1.l2(); i2 <= C1.u2(); ++i2 ) {
		for ( int i1 = C1.l1(); i1 <= C1.u1(); ++i1 ) {
			EXPECT_EQ( 2718, C1( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexRangeStickyInitializerValue )
{
	FArray2D_int A1( { 1, 2 }, { 1, 3 }, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 2718, A1( i1, i2 ) );
		}
	}

	FArray2D_int const C1( { 1, 2 }, { 1, 3 }, Sticky_int( 31459 ), 2718 );
	EXPECT_EQ( 6u, C1.size() );
	EXPECT_EQ( 2u, C1.size1() );
	EXPECT_EQ( 3u, C1.size2() );
	EXPECT_EQ( 1, C1.l1() );
	EXPECT_EQ( 2, C1.u1() );
	EXPECT_EQ( 1, C1.l2() );
	EXPECT_EQ( 3, C1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), C1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), C1.I2() );
	EXPECT_TRUE( C1.dimensions_initialized() );
	EXPECT_TRUE( C1.initializer_active() );
	for ( int i2 = C1.l2(); i2 <= C1.u2(); ++i2 ) {
		for ( int i1 = C1.l1(); i1 <= C1.u1(); ++i1 ) {
			EXPECT_EQ( 2718, C1( i1, i2 ) );
		}
	}
}

static void initializer_function_int( FArray2D_int & A )
{
	for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
		for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
			A( i1, i2 ) = i1 * 10 + i2;
		}
	}
}

static void initializer_function_double( FArray2D_double & A )
{
	for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
		for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
			A( i1, i2 ) = i1 + i2 * 0.1;
		}
	}
}

static void initializer_function_Fstring( FArray2D_Fstring & A )
{
	for ( int i2 = A.l2(); i2 <= A.u2(); ++i2 ) {
		for ( int i1 = A.l1(); i1 <= A.u1(); ++i1 ) {
			A( i1, i2 ).reassign( Fstring_of( i1 ) + "," + Fstring_of( i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexesInitializerFunction )
{
	FArray2D_int A1( 2, 3, initializer_function_int);
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, { 11, 21, 12, 22, 13, 23 } ), A1 ) );
	FArray2D_double A2( 2, 3, initializer_function_double );
	EXPECT_TRUE( eq( FArray2D_double( 2, 3, { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } ), A2 ) );
	FArray2D_Fstring A3( 2, 3, initializer_function_Fstring );
	EXPECT_TRUE( eq( FArray2D_Fstring( 2, 3, { "1,1", "2,1", "1,2", "2,2", "1,3", "2,3" } ), A3 ) );

	FArray2D_int const C1( 2, 3, initializer_function_int);
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, { 11, 21, 12, 22, 13, 23 } ), C1 ) );
	FArray2D_double const C2( 2, 3, initializer_function_double );
	EXPECT_TRUE( eq( FArray2D_double( 2, 3, { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } ), C2 ) );
	FArray2D_Fstring const C3( 2, 3, initializer_function_Fstring );
	EXPECT_TRUE( eq( FArray2D_Fstring( 2, 3, { "1,1", "2,1", "1,2", "2,2", "1,3", "2,3" } ), C3 ) );
}

TEST( FArray2Test, ConstructIndexRangeInitializerFunction )
{
	FArray2D_int A1( { 0, 1 }, { -1, 1 }, initializer_function_int );
	EXPECT_TRUE( eq( FArray2D_int( { 0, 1 }, { -1, 1 }, { -1, 9, 0, 10, 1, 11 } ), A1 ) );
	FArray2D_double A2( { 0, 1 }, { -1, 1 }, initializer_function_double );
	EXPECT_TRUE( eq( FArray2D_double( { 0, 1 }, { -1, 1 }, { -0.1, 0.9, 0.0, 1.0, 0.1, 1.1 } ), A2 ) );
	FArray2D_Fstring A3( { 0, 1 }, { -1, 1 }, initializer_function_Fstring );
	EXPECT_TRUE( eq( FArray2D_Fstring( { 0, 1 }, { -1, 1 }, { "0,-1", "1,-1", "0,0", "1,0", "0,1", "1,1" } ), A3 ) );

	FArray2D_int const C1( { 0, 1 }, { -1, 1 }, initializer_function_int);
	EXPECT_TRUE( eq( FArray2D_int( { 0, 1 }, { -1, 1 }, { -1, 9, 0, 10, 1, 11 } ), C1 ) );
	FArray2D_double const C2( { 0, 1 }, { -1, 1 }, initializer_function_double );
	EXPECT_TRUE( eq( FArray2D_double( { 0, 1 }, { -1, 1 }, { -0.1, 0.9, 0.0, 1.0, 0.1, 1.1 } ), C2 ) );
	FArray2D_Fstring const C3( { 0, 1 }, { -1, 1 }, initializer_function_Fstring );
	EXPECT_TRUE( eq( FArray2D_Fstring( { 0, 1 }, { -1, 1 }, { "0,-1", "1,-1", "0,0", "1,0", "0,1", "1,1" } ), C3 ) );
}

TEST( FArray2Test, ConstructIndexesInitializerList )
{
 	FArray2D_int A1( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_FALSE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A1( i1, i2 ) );
		}
	}

	FArray2D_uint A2( 2, 3, { 11u, 21u, 12u, 22u, 13u, 23u } );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_FALSE( A2.initializer_active() );
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( unsigned( i1 * 10 + i2 ), A2( i1, i2 ) );
		}
	}

	FArray2D_double A3( 2, 3, { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A3.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A3.I2() );
	EXPECT_TRUE( A3.dimensions_initialized() );
	EXPECT_FALSE( A3.initializer_active() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_DOUBLE_EQ( i1 + i2 * 0.1, A3( i1, i2 ) );
		}
	}

	FArray2D_Fstring A4( 2, 3, { "1,1", "2,1", "1,2", "2,2", "1,3", "2,3" } );
	EXPECT_EQ( 6u, A4.size() );
	EXPECT_EQ( 2u, A4.size1() );
	EXPECT_EQ( 3u, A4.size2() );
	EXPECT_EQ( 1, A4.l1() );
	EXPECT_EQ( 2, A4.u1() );
	EXPECT_EQ( 1, A4.l2() );
	EXPECT_EQ( 3, A4.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A4.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A4.I2() );
	EXPECT_TRUE( A4.dimensions_initialized() );
	EXPECT_FALSE( A4.initializer_active() );
	char const * chars[] = { "", "1", "2", "3" };
	for ( int i2 = A4.l2(); i2 <= A4.u2(); ++i2 ) {
		for ( int i1 = A4.l1(); i1 <= A4.u1(); ++i1 ) {
			Fstring c1( chars[ i1 ] ), c2( chars[ i2 ] );
			EXPECT_EQ( c1 + "," + c2, A4( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexRangeInitializerList )
{
	FArray2D_int A1( { 0, 1 }, { -1, 1 }, { -1, 9, 0, 10, 1, 11 } );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 0, A1.l1() );
	EXPECT_EQ( 1, A1.u1() );
	EXPECT_EQ( -1, A1.l2() );
	EXPECT_EQ( 1, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 0, 1 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( -1, 1 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_FALSE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A1( i1, i2 ) );
		}
	}

	FArray2D_Fstring A2( { 0, 1 }, { -1, 1 }, { "0,-1", "1,-1", "0,0", "1,0", "0,1", "1,1" } );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 0, A2.l1() );
	EXPECT_EQ( 1, A2.u1() );
	EXPECT_EQ( -1, A2.l2() );
	EXPECT_EQ( 1, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 0, 1 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( -1, 1 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_FALSE( A2.initializer_active() );
	char const * chars1[] = { "0", "1" };
	char const * chars2[] = { "-1", "0", "1" };
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			Fstring c1( chars1[ i1 ] ), c2( chars2[ i2 + 1 ] );
			EXPECT_EQ( c1 + "," + c2, A2( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexesStickyInitializerList )
{
	FArray2D_int A1( 2, 3, Sticky_int( 31459 ), { 11, 21, 12, 22, 13, 23 } );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A1( i1, i2 ) );
		}
	}
	A1.dimension( 2, 2 );
	EXPECT_EQ( 31459, A1( 1, 1 ) );
	EXPECT_EQ( 31459, A1( 1, 2 ) );
	EXPECT_EQ( 31459, A1( 2, 1 ) );
	EXPECT_EQ( 31459, A1( 2, 2 ) );

	FArray2D_uint A2( 2, 3, Sticky_uint( 31459u ), { 11u, 21u, 12u, 22u, 13u, 23u } );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_TRUE( A2.initializer_active() );
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( unsigned( i1 * 10 + i2 ), A2( i1, i2 ) );
		}
	}

	FArray2D_double A3( 2, 3, Sticky_double( 3.1459 ), { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A3.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A3.I2() );
	EXPECT_TRUE( A3.dimensions_initialized() );
	EXPECT_TRUE( A3.initializer_active() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_DOUBLE_EQ( i1 + i2 * 0.1, A3( i1, i2 ) );
		}
	}

	//Note The sticky argument must be at least as long as the longest argument in the initializer list, or those arguments end up truncated
	FArray2D_Fstring A4( 2, 3, Sticky_Fstring( "X,X" ), { "1,1", "2,1", "1,2", "2,2", "1,3", "2,3" } );
	EXPECT_EQ( 6u, A4.size() );
	EXPECT_EQ( 2u, A4.size1() );
	EXPECT_EQ( 3u, A4.size2() );
	EXPECT_EQ( 1, A4.l1() );
	EXPECT_EQ( 2, A4.u1() );
	EXPECT_EQ( 1, A4.l2() );
	EXPECT_EQ( 3, A4.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A4.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A4.I2() );
	EXPECT_TRUE( A4.dimensions_initialized() );
	EXPECT_TRUE( A4.initializer_active() );
	char const * chars[] = { "", "1", "2", "3" };
	for ( int i2 = A4.l2(); i2 <= A4.u2(); ++i2 ) {
		for ( int i1 = A4.l1(); i1 <= A4.u1(); ++i1 ) {
			Fstring c1( chars[ i1 ] ), c2( chars[ i2 ] );
			EXPECT_EQ( c1 + "," + c2, A4( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructIndexRangeStickyInitializerList )
{
	FArray2D_int A1( { 1, 2 }, { 1, 3 }, Sticky_int( 31459 ), { 11, 21, 12, 22, 13, 23 } );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_TRUE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A1( i1, i2 ) );
		}
	}
	A1.dimension( 2, 2 );
	EXPECT_EQ( 31459, A1( 1, 1 ) );
	EXPECT_EQ( 31459, A1( 1, 2 ) );
	EXPECT_EQ( 31459, A1( 2, 1 ) );
	EXPECT_EQ( 31459, A1( 2, 2 ) );

	FArray2D_uint A2( { 1, 2 }, { 1, 3 }, Sticky_uint( 31459u ), { 11u, 21u, 12u, 22u, 13u, 23u } );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_TRUE( A2.initializer_active() );
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( unsigned( i1 * 10 + i2 ), A2( i1, i2 ) );
		}
	}

	FArray2D_double A3( { 1, 2 }, { 1, 3 }, Sticky_double( 3.1459 ), { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A3.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A3.I2() );
	EXPECT_TRUE( A3.dimensions_initialized() );
	EXPECT_TRUE( A3.initializer_active() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_DOUBLE_EQ( i1 + i2 * 0.1, A3( i1, i2 ) );
		}
	}

	//Note The sticky argument must be at least as long as the longest argument in the initializer list, or those arguments end up truncated
	FArray2D_Fstring A4( { 1, 2 }, { 1, 3 }, Sticky_Fstring( "X,X" ), { "1,1", "2,1", "1,2", "2,2", "1,3", "2,3" } );
	EXPECT_EQ( 6u, A4.size() );
	EXPECT_EQ( 2u, A4.size1() );
	EXPECT_EQ( 3u, A4.size2() );
	EXPECT_EQ( 1, A4.l1() );
	EXPECT_EQ( 2, A4.u1() );
	EXPECT_EQ( 1, A4.l2() );
	EXPECT_EQ( 3, A4.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A4.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A4.I2() );
	EXPECT_TRUE( A4.dimensions_initialized() );
	EXPECT_TRUE( A4.initializer_active() );
	char const * chars[] = { "", "1", "2", "3" };
	for ( int i2 = A4.l2(); i2 <= A4.u2(); ++i2 ) {
		for ( int i1 = A4.l1(); i1 <= A4.u1(); ++i1 ) {
			Fstring c1( chars[ i1 ] ), c2( chars[ i2 ] );
			EXPECT_EQ( c1 + "," + c2, A4( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ConstructRange )
{
	FArray2D_int A1( 2, 3 );

	FArray2D_int A2( FArray2D_int::range( A1 ) );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_FALSE( A2.initializer_active() );
	// Values remain uninitialized

	FArray2D_int A3( FArray2D_int::range( A1, 31459 ) );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A3.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A3.I2() );
	EXPECT_TRUE( A3.dimensions_initialized() );
	EXPECT_TRUE( A3.initializer_active() );
	for ( int i2 = 1; i2 <= 3; ++i2 )
		for ( int i1 = 1; i1 <= 2; ++i1 )
			EXPECT_EQ( 31459, A3( i1, i2 ) );
}

TEST( FArray2Test, ConstructShape )
{
	FArray2D_int A1( 2, 3 );

	FArray2D_int A2( FArray2D_int::shape( A1 ) );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_FALSE( A2.initializer_active() );
	// Values remain uninitialized

	FArray2D_int A3( FArray2D_int::shape( A1, 31459 ) );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A3.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A3.I2() );
	EXPECT_TRUE( A3.dimensions_initialized() );
	EXPECT_TRUE( A3.initializer_active() );
	for ( int i2 = 1; i2 <= 3; ++i2 )
		for ( int i1 = 1; i1 <= 2; ++i1 )
			EXPECT_EQ( 31459, A3( i1, i2 ) );
}

TEST( FArray2Test, ConstructOneBased )
{
	FArray2D_int A1( 2, 3 );
	FArray2D_int A2( FArray2D_int::one_based( A1 ) );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 2 ), A2.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A2.I2() );
	EXPECT_TRUE( A2.dimensions_initialized() );
	EXPECT_FALSE( A2.initializer_active() );
	// Values remain uninitialized
}

TEST( FArray2Test, ConstructDiag )
{
	FArray2D_int A1( FArray2D_int::diag( 3, 31459 ) );
	EXPECT_EQ( 9u, A1.size() );
	EXPECT_EQ( 3u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 3, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_FALSE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 )
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 )
			EXPECT_EQ( ( i1 == i2 ) ? 31459 : 0, A1( i1, i2 ) );
}

TEST( FArray2Test, ConstructIdentity )
{
	FArray2D_int A1( FArray2D_int::identity( 3 ) );
	EXPECT_EQ( 9u, A1.size() );
	EXPECT_EQ( 3u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 3, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I1() );
	EXPECT_EQ( FArray2D_int::IR( 1, 3 ), A1.I2() );
	EXPECT_TRUE( A1.dimensions_initialized() );
	EXPECT_FALSE( A1.initializer_active() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 )
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 )
			EXPECT_EQ( ( i1 == i2 ) ? 1 : 0, A1( i1, i2 ) );
}

TEST( FArray2Test, AssignDefault )
{
	FArray2D_double A1( 2, 3, 3.1459 );
	FArray2D_double const A2( 2, 3, 2.718 );
	EXPECT_FALSE( eq( A1, A2 ) );

	A1 = A2;
	EXPECT_TRUE( eq( A1, A2 ) );

	A1 = 3.1459;
	for ( int i2 = 1; i2 <= 3; ++i2 ) {
		for ( int i1 = 1; i1 <= 2; ++i1 ) {
			EXPECT_EQ( 3.1459, A1( i1, i2 ) );
		}
	}

	A1 = { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 };
	for ( int i2 = 1; i2 <= 3; ++i2 ) {
		for ( int i1 = 1; i1 <= 2; ++i1 ) {
			EXPECT_EQ( i1 + i2 * 0.1, A1( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, AssignOtherData )
{
	FArray2D_int A1( 2, 3, 31459 );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A1( i1, i2 ) );
		}
	}

	FArray2D_double const A2( 2, 3, 3.1459 );
	A1 = A2;
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 3, A1( i1, i2 ) );
		}
	}

// These unit tests can generate warnings for certain templated types, that's fine
#ifdef __llvm__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wliteral-conversion"
#endif

	A1 = 2.718; // May cause warnings about conversion
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 2, A1( i1, i2 ) );
		}
	}

	A1 = { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 }; // May cause warnings about conversion
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1, A1( i1, i2 ) );
		}
	}

#ifdef __llvm__
#pragma clang diagnostic pop
#endif

}

TEST( FArray2Test, AssignProxy )
{
	FArray2D_int A1( 2, 3, 31459 );
	FArray2P_int A2( A1 );
	FArray2D_int A3( 2, 3, 2718 );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A1( i1, i2 ) );
		}
	}

	A3 = A2;
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, AssignArgument )
{
	FArray2D_int A1( 2, 3, 31459 );
	FArray2A_int A2( A1 );
	FArray2D_int A3( 2, 3, 2718 );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A1( i1, i2 ) );
		}
	}

	A3 = A2;
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, AssignArithmetic )
{
	FArray2D_int A1( 2, 3, 11 );
	FArray2D_int const A2( 2, 3, 10 );
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );

	A1 += A2;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 21 ), A1 ) );
	A1 -= A2;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 += 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 44 ), A1 ) );
	A1 -= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 *= A2;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 110 ), A1 ) );
	A1 /= A2;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 *= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 363 ), A1 ) );
	A1 /= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
}

TEST( FArray2Test, AssignArithmeticProxy )
{
	FArray2D_int A1( 2, 3, 11 );
	FArray2D_int const A2( 2, 3, 10 );
	FArray2P_int A3( A2 );

	A1 += A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 21 ), A1 ) );
	A1 -= A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 += 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 44 ), A1 ) );
	A1 -= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 *= A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 110 ), A1 ) );
	A1 /= A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 *= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 363 ), A1 ) );
	A1 /= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
}

TEST( FArray2Test, AssignArithmeticArgument )
{
	FArray2D_int A1( 2, 3, 11 );
	FArray2D_int const A2( 2, 3, 10 );
	FArray2P_int A3( A2 );

	A1 += A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 21 ), A1 ) );
	A1 -= A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 += 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 44 ), A1 ) );
	A1 -= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 *= A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 110 ), A1 ) );
	A1 /= A3;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
	A1 *= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 363 ), A1 ) );
	A1 /= 33;
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 11 ), A1 ) );
}

TEST( FArray2Test, SubscriptTail )
{
	FArray2D_double A1( 2, 3, { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } );
	FArrayTail_double A2( A1.a( 1, 2 ) ); // Tail of last 4 values
	EXPECT_EQ( 4u, A2.size() );
	FArray2P_double A3( A2 );
	EXPECT_EQ( 4u, A3.size() );
	EXPECT_EQ( 1u, A3.size1() );
	EXPECT_EQ( 4u, A3.size2() );
	EXPECT_TRUE( eq( FArray2D_double( 1, 4, { 1.2, 2.2, 1.3, 2.3 } ), A3 ) );

	FArray2D_double const C1( 2, 3, { 1.1, 2.1, 1.2, 2.2, 1.3, 2.3 } );
	FArrayTail_double const C2( C1.a( 1, 2 ) ); // Tail of last 4 values
	EXPECT_EQ( 4u, C2.size() );
	FArray2P_double const C3( C2 );
	EXPECT_EQ( 4u, C3.size() );
	EXPECT_EQ( 1u, C3.size1() );
	EXPECT_EQ( 4u, C3.size2() );
	EXPECT_TRUE( eq( FArray2D_double( 1, 4, { 1.2, 2.2, 1.3, 2.3 } ), C3 ) );
}

TEST( FArray2Test, SubscriptIndex )
{
	FArray2D_int A1( 2, 3 );
	EXPECT_EQ( 0u, A1.index( 1, 1 ) );
	EXPECT_EQ( 2u, A1.index( 1, 2 ) );
	EXPECT_EQ( 4u, A1.index( 1, 3 ) );
	EXPECT_EQ( 1u, A1.index( 2, 1 ) );
	EXPECT_EQ( 3u, A1.index( 2, 2 ) );
	EXPECT_EQ( 5u, A1.index( 2, 3 ) );

	FArray2D_int const C1( 2, 3 );
	EXPECT_EQ( 0u, C1.index( 1, 1 ) );
	EXPECT_EQ( 2u, C1.index( 1, 2 ) );
	EXPECT_EQ( 4u, C1.index( 1, 3 ) );
	EXPECT_EQ( 1u, C1.index( 2, 1 ) );
	EXPECT_EQ( 3u, C1.index( 2, 2 ) );
	EXPECT_EQ( 5u, C1.index( 2, 3 ) );
}

TEST( FArray2Test, SubscriptOperator )
{
	FArray2D_int A1( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 11, A1[ 0 ] );
	EXPECT_EQ( 21, A1[ 1 ] );
	EXPECT_EQ( 12, A1[ 2 ] );
	EXPECT_EQ( 22, A1[ 3 ] );
	EXPECT_EQ( 13, A1[ 4 ] );
	EXPECT_EQ( 23, A1[ 5 ] );

	for ( std::size_t i = 0; i < A1.size(); ++i ) A1[ i ] = static_cast< int >( i * 10 );
	EXPECT_EQ( 0, A1[ 0 ] );
	EXPECT_EQ( 10, A1[ 1 ] );
	EXPECT_EQ( 20, A1[ 2 ] );
	EXPECT_EQ( 30, A1[ 3 ] );
	EXPECT_EQ( 40, A1[ 4 ] );
	EXPECT_EQ( 50, A1[ 5 ] );

	FArray2D_int const C1( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_EQ( 11, C1[ 0 ] );
	EXPECT_EQ( 21, C1[ 1 ] );
	EXPECT_EQ( 12, C1[ 2 ] );
	EXPECT_EQ( 22, C1[ 3 ] );
	EXPECT_EQ( 13, C1[ 4 ] );
	EXPECT_EQ( 23, C1[ 5 ] );
}

TEST( FArray2Test, Predicates )
{
	FArray2D_int A1;
	EXPECT_FALSE( A1.active() );
	EXPECT_FALSE( A1.allocated() );
	EXPECT_TRUE( A1.is_contiguous() );
	EXPECT_TRUE( A1.data_size_bounded() );
	EXPECT_FALSE( A1.data_size_unbounded() );
	EXPECT_TRUE( A1.empty() );
	EXPECT_TRUE( A1.size_bounded() );
	EXPECT_FALSE( A1.size_unbounded() );
	EXPECT_TRUE( A1.owner() );
	EXPECT_FALSE( A1.proxy() );
	EXPECT_TRUE( A1.is_default() );
	EXPECT_TRUE( A1.is_zero() );
	EXPECT_TRUE( A1.is_uniform() );
	EXPECT_TRUE( A1.is_uniform( 0 ) );

	FArray2D_int A2( 2, 3 ); // Uninitialized
	EXPECT_TRUE( A2.active() );
	EXPECT_TRUE( A2.allocated() );
	EXPECT_TRUE( A2.is_contiguous() );
	EXPECT_TRUE( A2.data_size_bounded() );
	EXPECT_FALSE( A2.data_size_unbounded() );
	EXPECT_FALSE( A2.empty() );
	EXPECT_TRUE( A2.data_size_bounded() );
	EXPECT_FALSE( A2.data_size_unbounded() );
	EXPECT_TRUE( A2.owner() );
	EXPECT_FALSE( A2.proxy() );

	FArray2D_int A3( 2, 3, 31459 );
	EXPECT_TRUE( A3.active() );
	EXPECT_TRUE( A3.allocated() );
	EXPECT_TRUE( A3.is_contiguous() );
	EXPECT_TRUE( A3.data_size_bounded() );
	EXPECT_FALSE( A3.data_size_unbounded() );
	EXPECT_FALSE( A3.empty() );
	EXPECT_TRUE( A3.data_size_bounded() );
	EXPECT_FALSE( A3.data_size_unbounded() );
	EXPECT_TRUE( A3.owner() );
	EXPECT_FALSE( A3.proxy() );
	EXPECT_FALSE( A3.is_default() );
	EXPECT_FALSE( A3.is_zero() );
	EXPECT_TRUE( A3.is_uniform() );
	EXPECT_TRUE( A3.is_uniform( 31459 ) );

	FArray2D_int A4( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_TRUE( A4.active() );
	EXPECT_TRUE( A4.allocated() );
	EXPECT_TRUE( A4.is_contiguous() );
	EXPECT_TRUE( A4.data_size_bounded() );
	EXPECT_FALSE( A4.data_size_unbounded() );
	EXPECT_FALSE( A4.empty() );
	EXPECT_TRUE( A4.data_size_bounded() );
	EXPECT_FALSE( A4.data_size_unbounded() );
	EXPECT_TRUE( A4.owner() );
	EXPECT_FALSE( A4.proxy() );
	EXPECT_FALSE( A4.is_default() );
	EXPECT_FALSE( A4.is_zero() );
	EXPECT_FALSE( A4.is_uniform() );
	EXPECT_FALSE( A4.is_uniform( 0 ) );
	EXPECT_FALSE( A4.is_uniform( 11 ) );
}

TEST( FArray2Test, PredicateComparisonsValues )
{
	FArray2D_int A1;
	EXPECT_TRUE( eq( A1, 0 ) && eq( 0, A1 ) ); // Empty array is considered to equal any scalar (no values don't equal the scalar)
	EXPECT_FALSE( ne( A1, 0 ) || ne( 0, A1 ) );
	EXPECT_FALSE( lt( A1, 0 ) || lt( 0, A1 ) );
	EXPECT_TRUE( le( A1, 0 ) && le( 0, A1 ) );
	EXPECT_FALSE( gt( A1, 0 ) || gt( 0, A1 ) );
	EXPECT_TRUE( ge( A1, 0 ) && ge( 0, A1 ) );

	FArray2D_int A2( 2, 3, 31459 );
	EXPECT_TRUE( eq( A2, 31459 ) && eq( 31459, A1 ) );
	EXPECT_FALSE( ne( A2, 31459 ) || ne( 31459, A2 ) );
	EXPECT_TRUE( lt( A2, 31460 ) && lt( 31458, A2 ) );
	EXPECT_TRUE( le( A2, 31459 ) && le( 31459, A2 ) );
	EXPECT_TRUE( le( A2, 31460 ) && le( 31458, A2 ) );
	EXPECT_TRUE( gt( A2, 31458 ) && gt( 31460, A2 ) );
	EXPECT_TRUE( ge( A2, 31459 ) && ge( 31459, A2 ) );
	EXPECT_TRUE( ge( A2, 31458 ) && ge( 31460, A2 ) );

	// Elements compared in order
	FArray2D_int A3( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_FALSE( eq( A3, 11 ) || eq( 23, A3 ) );
	EXPECT_TRUE( ne( A3, 11 ) && ne( 23, A3 ) );
	EXPECT_TRUE( lt( A3, 24 ) && lt( 10, A3 ) );
	EXPECT_FALSE( lt( A3, 23 ) || lt( 11, A3 ) );
	EXPECT_TRUE( le( A3, 23 ) && le( 11, A3 ) );
	EXPECT_TRUE( gt( A3, 10 ) && gt( 24, A3 ) );
	EXPECT_FALSE( gt( A3, 11 ) || gt( 23, A3 ) );
	EXPECT_TRUE( ge( A3, 11 ) && ge( 23, A3 ) );
}

TEST( FArray2Test, PredicateComparisonArrays )
{
	//Note Illegal to compare non-conformable arrays

	FArray2D_int A1;
	EXPECT_TRUE( eq( A1, A1 ) );
	EXPECT_FALSE( ne( A1, A1 ) );
	EXPECT_FALSE( lt( A1, A1 ) );
	EXPECT_TRUE( le( A1, A1 ) );
	EXPECT_FALSE( gt( A1, A1 ) );
	EXPECT_TRUE( ge( A1, A1 ) );

	FArray2D_int A2( 2, 3, 20 );
	EXPECT_TRUE( eq( A2, A2 ) );
	EXPECT_FALSE( ne( A2, A2 ) );
	EXPECT_FALSE( lt( A2, A2 ) );
	EXPECT_TRUE( le( A2, A2 ) );
	EXPECT_FALSE( gt( A2, A2 ) );
	EXPECT_TRUE( ge( A2, A2 ) );

	FArray2D_int A3( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_TRUE( eq( A3, A3 ) );
	EXPECT_FALSE( ne( A3, A3 ) );
	EXPECT_FALSE( lt( A3, A3 ) );
	EXPECT_TRUE( le( A3, A3 ) );
	EXPECT_FALSE( gt( A3, A3 ) );
	EXPECT_TRUE( ge( A3, A3 ) );

	EXPECT_FALSE( eq( A2, A3 ) || eq( A3, A2 ) );
	EXPECT_TRUE( ne( A2, A3 ) && ne( A3, A2 ) );
	EXPECT_FALSE( lt( A2, A3 ) || lt( A3, A2 ) );
	EXPECT_FALSE( le( A2, A3 ) || le( A3, A2 ) );
	EXPECT_FALSE( gt( A2, A3 ) || gt( A3, A2 ) );
	EXPECT_FALSE( ge( A2, A3 ) || ge( A3, A2 ) );

	FArray2D_int A4( 2, 3, { 11, 21, 12, 21, 12, 22 } );
	EXPECT_FALSE( eq( A3, A4 ) || eq( A4, A3 ) );
	EXPECT_TRUE( ne( A3, A4 ) && ne( A4, A3 ) );
	EXPECT_FALSE( lt( A3, A4 ) );
	EXPECT_FALSE( lt( A4, A3 ) );
	EXPECT_FALSE( le( A3, A4 ) );
	EXPECT_TRUE( le( A4, A3 ) );
	EXPECT_FALSE( gt( A3, A4 ) );
	EXPECT_FALSE( gt( A4, A3 ) );
	EXPECT_TRUE( ge( A3, A4 ) );
	EXPECT_FALSE( ge( A4, A3 ) );

	FArray2D_int A5( 2, 3, { 11, 21, 12, 23, 14, 24 } );
	EXPECT_FALSE( eq( A3, A4 ) || eq( A4, A3 ) );
	EXPECT_TRUE( ne( A3, A4 ) && ne( A4, A3 ) );
}

TEST( FArray2Test, PredicateContains )
{
	FArray2D_int A1( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_TRUE( A1.contains( 1, 1 ) && A1.contains( 2, 3 ) );
	EXPECT_FALSE( A1.contains( 3, 3 ) && A1.contains( 2, 4 ) );
	EXPECT_FALSE( A1.contains( 0, 1 ) && A1.contains( 1, 0 ) );

	FArray2D_int A2( { -3, -2 }, { -1, 1 }, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_TRUE( A2.contains( -3, -1 ) && A2.contains( -2, 1 ) );
	EXPECT_FALSE( A2.contains( 0, 1 ) && A2.contains( -2, 2 ) );
	EXPECT_FALSE( A2.contains( -4, -1 ) && A2.contains( -3, -2 ) );

	FArray2D_int const C1( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_TRUE( C1.contains( 1, 1 ) && C1.contains( 2, 3 ) );
	EXPECT_FALSE( C1.contains( 3, 3 ) && C1.contains( 2, 4 ) );
	EXPECT_FALSE( C1.contains( 0, 1 ) && C1.contains( 1, 0 ) );

	FArray2D_int const C2( { -3, -2 }, { -1, 1 }, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_TRUE( C2.contains( -3, -1 ) && C2.contains( -2, 1 ) );
	EXPECT_FALSE( C2.contains( 0, 1 ) && C2.contains( -2, 2 ) );
	EXPECT_FALSE( C2.contains( -4, -1 ) && C2.contains( -3, -2 ) );
}

TEST( FArray2Test, PredicateConformable )
{
	FArray2D_int A1;
	FArray2D_int A2( 2, 3 );
	FArray2D_int A3( 2, 4 );
	FArray2D_int A4( { 2, 3 }, { 2, 4 } );

	EXPECT_FALSE( A1.conformable( A2 ) || A2.conformable( A1 ) );
	EXPECT_FALSE( A1.conformable( A3 ) || A3.conformable( A1 ) );
	EXPECT_FALSE( A1.conformable( A4 ) || A4.conformable( A1 ) );
	EXPECT_FALSE( A2.conformable( A3 ) || A3.conformable( A2 ) );
	EXPECT_TRUE( A2.conformable( A4 ) && A4.conformable( A2 ) );
	EXPECT_FALSE( A3.conformable( A4 ) || A4.conformable( A3 ) );

	FArray2D_int const C1;
	FArray2D_int const C2( 2, 3 );
	FArray2D_int const C3( 2, 4 );
	FArray2D_int const C4( { 2, 3 }, { 2, 4 } );

	EXPECT_FALSE( C1.conformable( C2 ) || C2.conformable( C1 ) );
	EXPECT_FALSE( C1.conformable( C3 ) || C3.conformable( C1 ) );
	EXPECT_FALSE( C1.conformable( C4 ) || C4.conformable( C1 ) );
	EXPECT_FALSE( C2.conformable( C3 ) || C3.conformable( C2 ) );
	EXPECT_TRUE( C2.conformable( C4 ) && C4.conformable( C2 ) );
	EXPECT_FALSE( C3.conformable( C4 ) || C4.conformable( C3 ) );
}

TEST( FArray2Test, PredicateConformableOtherData )
{
	FArray2D_int A1( 2, 3 );
	FArray2D_double A2( 2, 3 );
	FArray2D_Fstring A3( 2, 3 );

	EXPECT_TRUE( A1.conformable( A2 ) && A2.conformable( A1 ) );
	EXPECT_TRUE( A1.conformable( A3 ) && A3.conformable( A1 ) );
	EXPECT_TRUE( A2.conformable( A3 ) && A3.conformable( A2 ) );

	FArray2D_int const C1( 2, 3 );
	FArray2D_double const C2( 2, 3 );
	FArray2D_Fstring const C3( 2, 3 );

	EXPECT_TRUE( C1.conformable( C2 ) && C2.conformable( C1 ) );
	EXPECT_TRUE( C1.conformable( C3 ) && C3.conformable( C1 ) );
	EXPECT_TRUE( C2.conformable( C3 ) && C3.conformable( C2 ) );
}

TEST( FArray2Test, PredicateConformableOtherArray )
{
	FArray2D_int A1( 2, 3 );
	FArray2P_int A2( A1 );
	FArray2A_int A3( A1 );
	FArray2D_int A4( 2, 3 );

	EXPECT_TRUE( A2.conformable( A3 ) && A3.conformable( A2 ) );
	EXPECT_TRUE( A2.conformable( A4 ) && A4.conformable( A2 ) );
	EXPECT_TRUE( A3.conformable( A4 ) && A4.conformable( A3 ) );

	FArray2D_int const C1( 2, 3 );
	FArray2P_int const C2( C1 );
	FArray2A_int const C3( C1 );
	FArray2D_int const C4( 2, 3 );

	EXPECT_TRUE( C2.conformable( C3 ) && C3.conformable( C2 ) );
	EXPECT_TRUE( C2.conformable( C4 ) && C4.conformable( C2 ) );
	EXPECT_TRUE( C3.conformable( C4 ) && C4.conformable( C3 ) );
}

TEST( FArray2Test, PredicateEqualDimensions )
{
	FArray2D_int A1;
	FArray2D_int A2( 2, 3 );
	FArray2D_int A3( 2, 4 );
	FArray2D_int A4( { 2, 3 }, { 2, 4 } );

	EXPECT_FALSE( A1.equal_dimensions( A2 ) || A2.equal_dimensions( A1 ) );
	EXPECT_FALSE( A1.equal_dimensions( A3 ) || A3.equal_dimensions( A1 ) );
	EXPECT_FALSE( A1.equal_dimensions( A4 ) || A4.equal_dimensions( A1 ) );
	EXPECT_FALSE( A2.equal_dimensions( A3 ) || A3.equal_dimensions( A2 ) );
	EXPECT_FALSE( A2.equal_dimensions( A4 ) || A4.equal_dimensions( A2 ) );
	EXPECT_FALSE( A3.equal_dimensions( A4 ) || A4.equal_dimensions( A3 ) );

	FArray2D_int A5( 2, 3, 31459 );
	EXPECT_TRUE( A2.equal_dimensions( A5 ) && A5.equal_dimensions( A2 ) );

	FArray2D_int const C1;
	FArray2D_int const C2( 2, 3 );
	FArray2D_int const C3( 2, 4 );
	FArray2D_int const C4( { 2, 3 }, { 2, 4 } );

	EXPECT_FALSE( C1.equal_dimensions( C2 ) || C2.equal_dimensions( C1 ) );
	EXPECT_FALSE( C1.equal_dimensions( C3 ) || C3.equal_dimensions( C1 ) );
	EXPECT_FALSE( C1.equal_dimensions( C4 ) || C4.equal_dimensions( C1 ) );
	EXPECT_FALSE( C2.equal_dimensions( C3 ) || C3.equal_dimensions( C2 ) );
	EXPECT_FALSE( C2.equal_dimensions( C4 ) || C4.equal_dimensions( C2 ) );
	EXPECT_FALSE( C3.equal_dimensions( C4 ) || C4.equal_dimensions( C3 ) );

	FArray2D_int C5( 2, 3, 31459 );
	EXPECT_TRUE( C2.equal_dimensions( C5 ) && C5.equal_dimensions( C2 ) );
}

TEST( FArray2Test, PredicateIdentity )
{
	FArray2D_int A1;
	EXPECT_TRUE( A1.is_identity() );

	FArray2D_int A2( 1, 1, 1 );
	EXPECT_TRUE( A2.is_identity() );

	FArray2D_int A3( 1, 1, 2 );
	EXPECT_FALSE( A3.is_identity() );

	FArray2D_int A4( 2, 2, 1 );
	EXPECT_FALSE( A4.is_identity() );

	FArray2D_int A5( 2, 2, 2 );
	EXPECT_FALSE( A5.is_identity() );

	FArray2D_int A6( 2, 2, { 1, 2, 2, 1 } );
	EXPECT_FALSE( A6.is_identity() );

	FArray2D_int A7( 2, 2, { 1, 0, 0, 1 } );
	EXPECT_TRUE( A7.is_identity() );
}

TEST( FArray2DTest, PredicateSymmetric )
{
	FArray2D_int A1;
	EXPECT_TRUE( A1.symmetric() );

	FArray2D_int A2( 1, 1, 1 );
	EXPECT_TRUE( A2.symmetric() );

	FArray2D_int A3( 1, 1, 2 );
	EXPECT_TRUE( A3.symmetric() );

	FArray2D_int A4( 2, 2, 1 );
	EXPECT_TRUE( A4.symmetric() );

	FArray2D_int A5( 2, 2, 2 );
	EXPECT_TRUE( A5.symmetric() );

	FArray2D_int A6( 2, 2, { 1, 2, 3, 1 } );
	EXPECT_FALSE( A6.symmetric() );

	FArray2D_int A7( 2, 2, { 1, 2, 2, 1 } );
	EXPECT_TRUE( A7.symmetric() );
}

TEST( FArray2Test, PredicateDimensionsInitialized )
{
	FArray2D_int A1;
	EXPECT_TRUE( A1.dimensions_initialized() );

	FArray2D_int A2( 2, 3 );
	EXPECT_TRUE( A2.dimensions_initialized() );

	FArray2D_int A3( 2, 3, 31459 );
	EXPECT_TRUE( A3.dimensions_initialized() );

	FArray2D_int const C1;
	EXPECT_TRUE( C1.dimensions_initialized() );

	FArray2D_int const C2( 2, 3 );
	EXPECT_TRUE( C2.dimensions_initialized() );

	FArray2D_int const C3( 2, 3, 31459 );
	EXPECT_TRUE( C3.dimensions_initialized() );
}

TEST( FArray2Test, PredicateInitializerActive )
{
	FArray2D_int A1;
	EXPECT_FALSE( A1.initializer_active() );

	FArray2D_int A2( 2, 3 );
	EXPECT_FALSE( A2.initializer_active() );

	FArray2D_int A3( 2, 3, 31459 );
	EXPECT_TRUE( A3.initializer_active() );

	FArray2D_int A4( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_FALSE( A4.initializer_active() );

	FArray2D_int A5( 2, 3, initializer_function_int );
	EXPECT_TRUE( A5.initializer_active() );
}

TEST( FArray2Test, Inspectors )
{
	FArray2D_int const C1;
	// Rank
	EXPECT_EQ( 2, C1.rank() );
	// Size
	EXPECT_EQ( 0u, C1.size() );
	EXPECT_EQ( 0u, C1.data_size() );
	EXPECT_EQ( 0u, C1.size( 1 ) );
	EXPECT_EQ( C1.size1(), C1.size( 1 ) );
	EXPECT_EQ( 0u, C1.size( 2 ) );
	EXPECT_EQ( C1.size2(), C1.size( 2 ) );
	// Indexes
	EXPECT_EQ( SRange(), C1.I( 1 ) );
	EXPECT_EQ( C1.I1(), C1.I( 1 ) );
	EXPECT_EQ( SRange(), C1.I( 2 ) );
	EXPECT_EQ( C1.I2(), C1.I( 2 ) );
	EXPECT_EQ( 1, C1.l( 1 ) );
	EXPECT_EQ( C1.l1(), C1.l( 1 ) );
	EXPECT_EQ( 1, C1.l( 2 ) );
	EXPECT_EQ( C1.l2(), C1.l( 2 ) );
 	EXPECT_EQ( 0, C1.u( 1 ) );
	EXPECT_EQ( C1.u1(), C1.u( 1 ) );
	EXPECT_EQ( 0, C1.u( 2 ) );
	EXPECT_EQ( C1.u2(), C1.u( 2 ) );
	// Data
	EXPECT_EQ( nullptr, C1.data() );
	EXPECT_EQ( nullptr, C1.data_beg() );
	EXPECT_EQ( nullptr, C1.data_end() );

	FArray2D_int const C2( 2, 3 );
	// Rank
	EXPECT_EQ( 2, C2.rank() );
	// Size
	EXPECT_EQ( 6u, C2.size() );
	EXPECT_EQ( 6u, C2.data_size() );
	EXPECT_EQ( 2u, C2.size( 1 ) );
	EXPECT_EQ( C2.size1(), C2.size( 1 ) );
	EXPECT_EQ( 3u, C2.size( 2 ) );
	EXPECT_EQ( C2.size2(), C2.size( 2 ) );
	// Indexes
	EXPECT_EQ( SRange( 1, 2 ), C2.I( 1 ) );
	EXPECT_EQ( C2.I1(), C2.I( 1 ) );
	EXPECT_EQ( SRange( 1, 3 ), C2.I( 2 ) );
	EXPECT_EQ( C2.I2(), C2.I( 2 ) );
	EXPECT_EQ( 1, C2.l( 1 ) );
	EXPECT_EQ( C2.l1(), C2.l( 1 ) );
	EXPECT_EQ( 1, C2.l( 2 ) );
	EXPECT_EQ( C2.l2(), C2.l( 2 ) );
 	EXPECT_EQ( 2, C2.u( 1 ) );
	EXPECT_EQ( C2.u1(), C2.u( 1 ) );
	EXPECT_EQ( 3, C2.u( 2 ) );
	EXPECT_EQ( C2.u2(), C2.u( 2 ) );
	// Data
	EXPECT_NE( nullptr, C2.data() );
	EXPECT_NE( nullptr, C2.data_beg() );
	EXPECT_NE( nullptr, C2.data_end() );
}

TEST( FArray2Test, ModifierClear )
{
	// Changes nothing about an empty array
	FArray2D_int A1;
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 0u, A1.size1() );
	EXPECT_EQ( 0u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 0, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 0, A1.u2() );
	EXPECT_FALSE( A1.initializer_active() );
	A1.clear();
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 0u, A1.size1() );
	EXPECT_EQ( 0u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 0, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 0, A1.u2() );
	EXPECT_FALSE( A1.initializer_active() );

	// Resets the size, indexes and initializer of an uninitialized array
	FArray2D_int A2( { 2, 3 }, { 2, 4 } );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 2, A2.l1() );
	EXPECT_EQ( 3, A2.u1() );
	EXPECT_EQ( 2, A2.l2() );
	EXPECT_EQ( 4, A2.u2() );
	EXPECT_FALSE( A2.initializer_active() );
	A2.clear();
	EXPECT_EQ( 0u, A2.size() );
	EXPECT_EQ( 0u, A2.size1() );
	EXPECT_EQ( 0u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 0, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 0, A2.u2() );
	EXPECT_FALSE( A2.initializer_active() );
	EXPECT_EQ( nullptr, A2.data() );
	EXPECT_EQ( nullptr, A2.data_beg() );
	EXPECT_EQ( nullptr, A2.data_end() );

	// Resets the size, indexes, initializer and contents  of an initialized array
	FArray2D_int A3( 2, 3, 31459 );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_TRUE( A3.initializer_active() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}
	A3.clear();
	EXPECT_EQ( 0u, A3.size() );
	EXPECT_EQ( 0u, A3.size1() );
	EXPECT_EQ( 0u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 0, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 0, A3.u2() );
	EXPECT_FALSE( A3.initializer_active() );
}

TEST( FArray2Test, ModifierToDefault )
{
	// Changes nothing about an empty array
	FArray2D_int A1;
	A1.to_default();
	EXPECT_TRUE( eq( FArray2D_int(), A1 ) );

	// Zeroes out an uninitialized array
	FArray2D_int A2( 2, 3 );
	EXPECT_FALSE( A2.initializer_active() );
	A2.to_default(); // Default value assigned depends on macros
	EXPECT_FALSE( A2.initializer_active() );

	// Zeroes out (not initializes) an initialized array
	FArray2D_int A3( 2, 3, 31459 );
	EXPECT_TRUE( A3.initializer_active() );
	A3.to_default(); // Default value assigned depends on macros
	EXPECT_TRUE( A3.initializer_active() );
}

TEST( FArray2Test, ModifierZero )
{
	// Changes nothing about an empty array
	FArray2D_int A1;
	A1.zero();
	EXPECT_TRUE( eq( FArray2D_int(), A1 ) );

	// Initializes an uninitialized array with actual dimensions
	FArray2D_int A2( 2, 3 );
	A2.zero();
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( 0, A2( i1, i2 ) );
		}
	}

	// Zeroes out an array with contents
	FArray2D_int A3( 2, 3, 31459 );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}
	A3.zero();
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 0, A3( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ModifierToZero )
{
	// Changes nothing about an empty array
	FArray2D_int A1;
	A1.to_zero();
	EXPECT_TRUE( eq( FArray2D_int(), A1 ) );

	// Initializes an uninitialized array with actual dimensions
	FArray2D_int A2( 2, 3 );
	A2.to_zero();
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( 0, A2( i1, i2 ) );
		}
	}

	// Zeroes out an array with contents
	FArray2D_int A3( 2, 3, 31459 );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}
	A3.to_zero();
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( 0, A3( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, ModifierInvert )
{
	// Changes nothing about an empty array
	FArray2D_double A1;
	A1.invert();
	EXPECT_TRUE( eq( FArray2D_double(), A1 ) );

	// Illegal to call on an uninitialized array

	// Inverts values of an initialized array
	FArray2D_double A2( 2, 3, { 1.0, 10.0, 100.0, 0.1, 0.01 } );
	A2.invert();
	EXPECT_TRUE( eq( FArray2D_double( 2, 3, { 1.0, 0.1, 0.01, 10.0, 100.0 } ), A2 ) );

	// Inverts values of an initialized array
	FArray2D_double A3( 2, 3, { -1.0, -10.0, -100.0, -0.1, -0.01 } );
	A3.invert();
	EXPECT_TRUE( eq( FArray2D_double( 2, 3, { -1.0, -0.1, -0.01, -10.0, -100.0 } ), A3 ) );
}

TEST( FArray2Test, ModifierAllocateDeallocate )
{
	FArray2D_int A1;
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 0u, A1.size1() );
	EXPECT_EQ( 0u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 0, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 0, A1.u2() );
	EXPECT_FALSE( A1.allocated() );

	A1.allocate( 2, 3 );
	EXPECT_EQ( 6u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 3u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 3, A1.u2() );
	EXPECT_TRUE( A1.allocated() );

	A1.deallocate();
	EXPECT_EQ( 0u, A1.size() );
	EXPECT_EQ( 0u, A1.size1() );
	EXPECT_EQ( 0u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 0, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 0, A1.u2() );
	EXPECT_FALSE( allocated( A1 ) );

	FArray2D_int A2( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_EQ( 6u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 3u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 2, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 3, A2.u2() );
	EXPECT_TRUE( A2.allocated() );

	A2.deallocate();
	EXPECT_EQ( 0u, A2.size() );
	EXPECT_EQ( 0u, A2.size1() );
	EXPECT_EQ( 0u, A2.size2() );
	EXPECT_EQ( 1, A2.l1() );
	EXPECT_EQ( 0, A2.u1() );
	EXPECT_EQ( 1, A2.l2() );
	EXPECT_EQ( 0, A2.u2() );
	EXPECT_FALSE( allocated( A2 ) );

	FArray2D_int A3;
	EXPECT_EQ( 0u, A3.size() );
	EXPECT_EQ( 0u, A3.size1() );
	EXPECT_EQ( 0u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 0, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 0, A3.u2() );
	EXPECT_FALSE( A3.allocated() );

	A3.allocate( FArray2D_int( 2, 3 ) );
	EXPECT_EQ( 6u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 3u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 2, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 3, A3.u2() );
	EXPECT_TRUE( A3.allocated() );

	A3.deallocate();
	EXPECT_EQ( 0u, A3.size() );
	EXPECT_EQ( 0u, A3.size1() );
	EXPECT_EQ( 0u, A3.size2() );
	EXPECT_EQ( 1, A3.l1() );
	EXPECT_EQ( 0, A3.u1() );
	EXPECT_EQ( 1, A3.l2() );
	EXPECT_EQ( 0, A3.u2() );
	EXPECT_FALSE( allocated( A3 ) );
}

static void dimension_initializer_function( FArray2D_int & A1 )
{
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			A1( i1, i2 ) = i1 * 10 + i2;
		}
	}
}

TEST( FArray2Test, DimensionIndexRange )
{
	FArray2D_int A1( 3, 4 );
	EXPECT_EQ( 12u, A1.size() );
	EXPECT_EQ( 3u, A1.size1() );
	EXPECT_EQ( 4u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 3, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 4, A1.u2() );
	// Values are uninitialized

	// 1:3, 1:4 -> 2:4, 2:5.
	A1.dimension( { 2, 4 }, { 2, 5 } );
	EXPECT_EQ( 12u, A1.size() );
	EXPECT_EQ( 3u, A1.size1() );
	EXPECT_EQ( 4u, A1.size2() );
	EXPECT_EQ( 2, A1.l1() );
	EXPECT_EQ( 4, A1.u1() );
	EXPECT_EQ( 2, A1.l2() );
	EXPECT_EQ( 5, A1.u2() );
	// Values are uninitialized

	// 1:3, 1:4 -> 2:4, 2:5 := 2:3, 2:4
	FArray2D_int A2( 3, 4 );
	A2.dimension( { 2, 4 }, { 2, 5 }, 31459 ); // Without new initial value array is uninitialized (POD) or default contructed (UDT)
	EXPECT_EQ( 3u, A2.size1() );
	EXPECT_EQ( 4u, A2.size2() );
	EXPECT_EQ( 2, A2.l1() );
	EXPECT_EQ( 4, A2.u1() );
	EXPECT_EQ( 2, A2.l2() );
	EXPECT_EQ( 5, A2.u2() );
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A2( i1, i2 ) );
		}
	}

	// 1:3, 1:4 -> 2:4, 2:5 := 2:3, 2:4
	FArray2D_int A3( 3, 4 );
	A3.dimension( { 2, 4 }, { 2, 5 }, dimension_initializer_function );
	EXPECT_EQ( 3u, A3.size1() );
	EXPECT_EQ( 4u, A3.size2() );
	EXPECT_EQ( 2, A3.l1() );
	EXPECT_EQ( 4, A3.u1() );
	EXPECT_EQ( 2, A3.l2() );
	EXPECT_EQ( 5, A3.u2() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A3( i1, i2 ) );
		}
	}

	// 1:3, 1:4 -> 2:4, 2:5 := 2:3, 2:4
	FArray2D_int A4( 3, 4, Sticky_int( 31459 ) ); // Sticky initializer is reapplied after dimension call
	A4.dimension( { 2, 4 }, { 2, 5 } );
	EXPECT_EQ( 3u, A4.size1() );
	EXPECT_EQ( 4u, A4.size2() );
	EXPECT_EQ( 2, A4.l1() );
	EXPECT_EQ( 4, A4.u1() );
	EXPECT_EQ( 2, A4.l2() );
	EXPECT_EQ( 5, A4.u2() );
	for ( int i2 = A4.l2(); i2 <= A4.u2(); ++i2 ) {
		for ( int i1 = A4.l1(); i1 <= A4.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A4( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, DimensionArrays )
{
	FArray2D_int A1( 3, 4 );
	EXPECT_EQ( 12u, A1.size() );
	EXPECT_EQ( 3u, A1.size1() );
	EXPECT_EQ( 4u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 3, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 4, A1.u2() );
	// Values are uninitialized

	// 1:3, 1:4 -> 2:4, 2:5.
	A1.dimension( FArray2D_int( { 2, 4 }, { 2, 5 } ) );
	EXPECT_EQ( 12u, A1.size() );
	EXPECT_EQ( 3u, A1.size1() );
	EXPECT_EQ( 4u, A1.size2() );
	EXPECT_EQ( 2, A1.l1() );
	EXPECT_EQ( 4, A1.u1() );
	EXPECT_EQ( 2, A1.l2() );
	EXPECT_EQ( 5, A1.u2() );
	// Values are uninitialized

	// 1:3, 1:4 -> 2:4, 2:5 := 2:3, 2:4
	FArray2D_int A2( 3, 4 );
	A2.dimension( FArray2D_int( { 2, 4 }, { 2, 5 } ), 31459 );
	EXPECT_EQ( 3u, A2.size1() );
	EXPECT_EQ( 4u, A2.size2() );
	EXPECT_EQ( 2, A2.l1() );
	EXPECT_EQ( 4, A2.u1() );
	EXPECT_EQ( 2, A2.l2() );
	EXPECT_EQ( 5, A2.u2() );
	for ( int i2 = A2.l2(); i2 <= A2.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A2.u1(); ++i1 ) {
			EXPECT_EQ( 31459, A2( i1, i2 ) );
		}
	}

	// 1:3, 1:4 -> 2:4, 2:5 := 2:3, 2:4
	FArray2D_int A3( 3, 4 );
	A3.dimension( FArray2D_int( { 2, 4 }, { 2, 5 } ), dimension_initializer_function );
	EXPECT_EQ( 3u, A3.size1() );
	EXPECT_EQ( 4u, A3.size2() );
	EXPECT_EQ( 2, A3.l1() );
	EXPECT_EQ( 4, A3.u1() );
	EXPECT_EQ( 2, A3.l2() );
	EXPECT_EQ( 5, A3.u2() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A3( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, RedimensionIndexRange )
{
	FArray2D_int A1( 2, 5, { 11, 21, 12, 22, 13, 23, 14, 24, 15, 25 } );
	FArray2D_int A2( A1 );
	FArray2D_int A3( A1 );

	EXPECT_EQ( 10u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 5u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 5, A1.u2() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A1( i1, i2 ) );
		}
	}

	// 1:2, 1:5 -> 2:3, 3:7 := 2:2, 3:5
	A2.redimension( { 2, 3 }, { 3, 7 } );
	EXPECT_EQ( 10u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 5u, A2.size2() );
	EXPECT_EQ( 2, A2.l1() );
	EXPECT_EQ( 3, A2.u1() );
	EXPECT_EQ( 3, A2.l2() );
	EXPECT_EQ( 7, A2.u2() );
	// Values for previous range are now uninitialized
	for ( int i2 = A2.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A2( i1, i2 ) );
		}
	}

	// 1:2, 1:5 -> 2:3, 3:7 := 2:2, 3:5
	A3.redimension( { 2, 3 }, { 3, 7 }, 31459 );
	EXPECT_EQ( 10u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 5u, A3.size2() );
	EXPECT_EQ( 2, A3.l1() );
	EXPECT_EQ( 3, A3.u1() );
	EXPECT_EQ( 3, A3.l2() );
	EXPECT_EQ( 7, A3.u2() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			if ( ( i1 >= A3.l1() && i1 <= A1.u1() ) && ( i2 >= A3.l2() && i2 <= A1.u2() ) )
				EXPECT_EQ( i1 * 10 + i2, A3( i1, i2 ) );
			else
				EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}
}

TEST( FArray2Test, RedimensionArrays )
{
	FArray2D_int A1( 2, 5, { 11, 21, 12, 22, 13, 23, 14, 24, 15, 25 } );
	FArray2D_int A2( A1 );
	FArray2D_int A3( A1 );
	FArray2D_int A4( A1 );

	EXPECT_EQ( 10u, A1.size() );
	EXPECT_EQ( 2u, A1.size1() );
	EXPECT_EQ( 5u, A1.size2() );
	EXPECT_EQ( 1, A1.l1() );
	EXPECT_EQ( 2, A1.u1() );
	EXPECT_EQ( 1, A1.l2() );
	EXPECT_EQ( 5, A1.u2() );
	for ( int i2 = A1.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A1.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A1( i1, i2 ) );
		}
	}

	A2.redimension( FArray2D_int( { 2, 3 }, { 3, 7 } ) );
	EXPECT_EQ( 10u, A2.size() );
	EXPECT_EQ( 2u, A2.size1() );
	EXPECT_EQ( 5u, A2.size2() );
	EXPECT_EQ( 2, A2.l1() );
	EXPECT_EQ( 3, A2.u1() );
	EXPECT_EQ( 3, A2.l2() );
	EXPECT_EQ( 7, A2.u2() );
	// Values for previous range are now uninitialized
	for ( int i2 = A2.l2(); i2 <= A1.u2(); ++i2 ) {
		for ( int i1 = A2.l1(); i1 <= A1.u1(); ++i1 ) {
			EXPECT_EQ( i1 * 10 + i2, A2( i1, i2 ) );
		}
	}

	A3.redimension( FArray2D_int( { 2, 3 }, { 3, 7 } ), 31459 );
	EXPECT_EQ( 10u, A3.size() );
	EXPECT_EQ( 2u, A3.size1() );
	EXPECT_EQ( 5u, A3.size2() );
	EXPECT_EQ( 2, A3.l1() );
	EXPECT_EQ( 3, A3.u1() );
	EXPECT_EQ( 3, A3.l2() );
	EXPECT_EQ( 7, A3.u2() );
	for ( int i2 = A3.l2(); i2 <= A3.u2(); ++i2 ) {
		for ( int i1 = A3.l1(); i1 <= A3.u1(); ++i1 ) {
			if ( ( i1 >= A3.l1() && i1 <= A1.u1() ) && ( i2 >= A3.l2() && i2 <= A1.u2() ) )
				EXPECT_EQ( i1 * 10 + i2, A3( i1, i2 ) );
			else
				EXPECT_EQ( 31459, A3( i1, i2 ) );
		}
	}

	A4.redimension( FArray2D_int( { 2, 3 }, { 3, 7 } ) ); // "New" elements are uninitialized
	EXPECT_EQ( 10u, A4.size() );
	EXPECT_EQ( 2u, A4.size1() );
	EXPECT_EQ( 5u, A4.size2() );
	EXPECT_EQ( 2, A4.l1() );
	EXPECT_EQ( 3, A4.u1() );
	EXPECT_EQ( 3, A4.l2() );
	EXPECT_EQ( 7, A4.u2() );
	for ( int i2 = A4.l2(); i2 <= A4.u2(); ++i2 ) {
		for ( int i1 = A4.l1(); i1 <= A4.u1(); ++i1 ) {
			if ( ( i1 >= A4.l1() && i1 <= A1.u1() ) && ( i2 >= A4.l2() && i2 <= A1.u2() ) )
				EXPECT_EQ( i1 * 10 + i2, A4( i1, i2 ) );
			else
				EXPECT_EQ( ( i1 - 1 ) * 10 + ( i2 - 2 ), A1( i1 - 1, i2 - 2 ) );
		}
	}
}

TEST( FArray2Test, Initializer )
{
	FArray2D_int A1( 2, 3 );
	EXPECT_FALSE( A1.initializer_active() );
	A1.initializer( 31459 );
	EXPECT_TRUE( A1.initializer_active() );

	FArray2D_int A2( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_FALSE( A2.initializer_active() );
	A2.initializer( 31459 );
	EXPECT_TRUE( A2.initializer_active() );

	FArray2D_int A3( 2, 3 );
	EXPECT_FALSE( A3.initializer_active() );
	A3.initializer( initializer_function_int );
	EXPECT_TRUE( A3.initializer_active() );

	FArray2D_int A4( 2, 3, 31459 );
	EXPECT_TRUE( A4.initializer_active() );
	A4.initializer( initializer_function_int );
	EXPECT_TRUE( A4.initializer_active() );

	FArray2D_int A5( 2, 3, initializer_function_int );
	EXPECT_TRUE( A5.initializer_active() );
	A5.initializer( 31459 );
	EXPECT_TRUE( A5.initializer_active() );
}

TEST( FArray2Test, InitializerClear )
{
	FArray2D_int A1( 2, 3 );
	EXPECT_FALSE( A1.initializer_active() );
	A1.initializer_clear();
	EXPECT_FALSE( A1.initializer_active() );

	FArray2D_int A2( 2, 3, 31459 );
	EXPECT_TRUE( A2.initializer_active() );
	A2.initializer_clear();
	EXPECT_FALSE( A2.initializer_active() );

	FArray2D_int A3( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_FALSE( A3.initializer_active() );
	A3.initializer_clear();
	EXPECT_FALSE( A3.initializer_active() );

	FArray2D_int A4( 2, 3, initializer_function_int );
	EXPECT_TRUE( A4.initializer_active() );
	A4.initializer_clear();
	EXPECT_FALSE( A4.initializer_active() );
}

TEST( FArray2Test, Initialize )
{
	FArray2D_int A1( 2, 3 );
	EXPECT_FALSE( A1.initializer_active() );
	A1.initializer( 31459 );
	EXPECT_TRUE( A1.initializer_active() );
	A1.initialize();
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 31459 ), A1 ) );

	FArray2D_int A2( 2, 3 );
	EXPECT_FALSE( A2.initializer_active() );
	A2.initializer( initializer_function_int );
	EXPECT_TRUE( A2.initializer_active() );
	A2.initialize();
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, { 11, 21, 12, 22, 13, 23 } ), A2 ) );

	FArray2D_int A3( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	EXPECT_FALSE( A3.initializer_active() );
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, { 11, 21, 12, 22, 13, 23 } ), A3 ) );
	A3.initializer( 31459 );
	EXPECT_TRUE( A3.initializer_active() );
	A3.initialize();
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 31459 ), A3 ) );

	FArray2D_int A4( 2, 3, 31459 );
	EXPECT_TRUE( A4.initializer_active() );
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, 31459 ), A4 ) );
	A4.initializer( initializer_function_int );
	EXPECT_TRUE( A4.initializer_active() );
	A4.initialize();
	EXPECT_TRUE( eq( FArray2D_int( 2, 3, { 11, 21, 12, 22, 13, 23 } ), A4 ) );
}

TEST( FArray2Test, Swap )
{
	FArray2D_int A1( 2, 3, { 11, 21, 12, 22, 13, 23 } );
	FArray2D_int A2;
	FArray2D_int const A3( A1 );
	EXPECT_TRUE( eq( A1, A3 ) );
	EXPECT_TRUE( eq( FArray2D_int(), A2 ) );
	A1.swap( A2 );
	EXPECT_TRUE( eq( A2, A3 ) );
	EXPECT_TRUE( eq( FArray2D_int(), A1 ) );
}
