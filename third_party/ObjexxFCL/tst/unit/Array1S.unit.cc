// ObjexxFCL::Array1S Unit Tests
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4244) // Suppress conversion warnings: Intentional narrowing assignments present
#endif

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include "ObjexxFCL.unit.hh"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

using namespace ObjexxFCL;

TEST( Array1STest, FunctionAbs )
{
	Array1D_int const A( { -1, -2, -3 } );
	Array1S_int a( A( {1,3} ) );
	Array1D_int const E( { 1, 2, 3 } );
	EXPECT_TRUE( eq( E, abs( a ) ) );
}

TEST( Array1STest, FunctionNegation )
{
	Array1D_bool const A( { true, false, true } );
	Array1S_bool a( A( {1,3} ) );
	Array1D_bool const E( { false, true, false } );
	EXPECT_TRUE( eq( E, !a ) );
}

TEST( Array1STest, FunctionBitNot )
{
	Array1D< uint8_t > const A( 2, { 1, 128 } );
	Array1D< uint8_t > const E( 2, { 254, 127 } );
	Array1S< uint8_t > a( A( {1,2} ) );
	EXPECT_TRUE( eq( E, bit_not( a ) ) );
}

TEST( Array1STest, FunctionBitAnd )
{
	Array1D< uint8_t > const A( 2, { 1, 128 } );
	Array1D< uint8_t > const B( 2, { 1, 128 } );
	Array1D< uint8_t > const Z( 2, { 0, 0 } );
	Array1D< uint8_t > const O( 2, { 255, 255 } );
	Array1S< uint8_t > a( A( {1,2} ) );
	Array1S< uint8_t > b( B( {1,2} ) );
	Array1S< uint8_t > z( Z( {1,2} ) );
	Array1S< uint8_t > o( O( {1,2} ) );
	EXPECT_TRUE( eq( A, bit_and( a, b ) ) );
	EXPECT_TRUE( eq( Z, bit_and( a, z ) ) );
	EXPECT_TRUE( eq( A, bit_and( a, o ) ) );

	Array1D< uint8_t > const I( 2, { 0xF0, 0xAB } );
	Array1D< uint8_t > const J( 2, { 0xAB, 0xF0 } );
	Array1D< uint8_t > const K( 2, { 0xA0u, 0xA0u } );
	Array1S< uint8_t > i( I( {1,2} ) );
	Array1S< uint8_t > j( J( {1,2} ) );
	EXPECT_TRUE( eq( K, bit_and( i, j ) ) );
}

TEST( Array1STest, FunctionBitOr )
{
	Array1D< uint8_t > const A( 2, { 1, 128 } );
	Array1D< uint8_t > const B( 2, { 1, 128 } );
	Array1D< uint8_t > const Z( 2, { 0, 0 } );
	Array1D< uint8_t > const O( 2, { 255, 255 } );
	Array1S< uint8_t > a( A( {1,2} ) );
	Array1S< uint8_t > b( B( {1,2} ) );
	Array1S< uint8_t > z( Z( {1,2} ) );
	Array1S< uint8_t > o( O( {1,2} ) );
	EXPECT_TRUE( eq( A, bit_or( a, b ) ) );
	EXPECT_TRUE( eq( A, bit_or( a, z ) ) );
	EXPECT_TRUE( eq( O, bit_or( a, o ) ) );

	Array1D< uint8_t > const I( 2, { 0xF0, 0xAB } );
	Array1D< uint8_t > const J( 2, { 0xAB, 0xF0 } );
	Array1D< uint8_t > const K( 2, { 0xFBu, 0xFBu } );
	Array1S< uint8_t > i( I( {1,2} ) );
	Array1S< uint8_t > j( J( {1,2} ) );
	EXPECT_TRUE( eq( K, bit_or( i, j ) ) );
}

TEST( Array1STest, FunctionBitXor )
{
	Array1D< uint8_t > const A( 2, { 1, 128 } );
	Array1D< uint8_t > const B( 2, { 1, 128 } );
	Array1D< uint8_t > const Z( 2, { 0, 0 } );
	Array1S< uint8_t > a( A( {1,2} ) );
	Array1S< uint8_t > b( B( {1,2} ) );
	Array1S< uint8_t > z( Z( {1,2} ) );
	EXPECT_TRUE( eq( Z, bit_xor( a, b ) ) );
	EXPECT_TRUE( eq( A, bit_xor( a, z ) ) );

	Array1D< uint8_t > const I( 2, { 0xF0, 0xFF } );
	Array1D< uint8_t > const J( 2, { 0xFF, 0xF0 } );
	Array1D< uint8_t > const K( 2, { 0x0Fu, 0x0Fu } );
	Array1S< uint8_t > i( I( {1,2} ) );
	Array1S< uint8_t > j( J( {1,2} ) );
	EXPECT_TRUE( eq( K, bit_xor( i, j ) ) );
}

TEST( Array1STest, FunctionMerge )
{
	{
		Array1D_int const A( 3, 1 );
		Array1D_int const B( 3, 2 );
		Array1S_int a( A( {1,3} ) );
		Array1S_int b( B( {1,3} ) );
		EXPECT_TRUE( eq( A, merge( a, b, true ) ) );
		EXPECT_TRUE( eq( B, merge( a, b, false ) ) );
	}

	{
		Array1D_int const A( 3, 1 );
		int const b( 2 );
		Array1D_int const B( 3, 2 );
		Array1S_int a( A( {1,3} ) );
		EXPECT_TRUE( eq( A, merge( a, b, true ) ) );
		EXPECT_TRUE( eq( B, merge( a, b, false ) ) );
	}

	{
		int const a( 1 );
		Array1D_int const A( 3, 1 );
		Array1D_int const B( 3, 2 );
		Array1S_int b( B( {1,3} ) );
		EXPECT_TRUE( eq( A, merge( a, b, true ) ) );
		EXPECT_TRUE( eq( B, merge( a, b, false ) ) );
	}

	{
		Array1D_int const A( 3, 1 );
		Array1D_int const B( 3, 2 );
		Array1S_int a( A( {1,3} ) );
		Array1S_int b( B( {1,3} ) );
		Array1D_bool const mask( 3, { true, false, true } );
		Array1D_int const m( 3, { 1, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}

	{
		Array1D_int const A( 3, 1 );
		int const b( 2 );
		Array1S_int a( A( {1,3} ) );
		Array1D_bool const mask( 3, { true, false, true } );
		Array1D_int const m( 3, { 1, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}

	{
		int const a( 1 );
		Array1D_int const B( 3, 2 );
		Array1S_int b( B( {1,3} ) );
		Array1D_bool const mask( 3, { true, false, true } );
		Array1D_int const m( 3, { 1, 2, 1 } );
		EXPECT_TRUE( eq( m, merge( a, b, mask ) ) );
	}
}
