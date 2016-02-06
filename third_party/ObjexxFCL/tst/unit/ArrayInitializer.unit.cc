// ObjexxFCL::ArrayInitializer Unit Tests
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
#include <ObjexxFCL/ArrayInitializer.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( ArrayInitializerTest, Construction )
{
	ArrayInitializer_int a;
	EXPECT_FALSE( a.active() );
	ArrayInitializer_int b( a );
	EXPECT_FALSE( b.active() );
	ArrayInitializer_int c( 42 );
	EXPECT_TRUE( c.active() );
	EXPECT_EQ( 42, c.value() );
	EXPECT_EQ( 42, c() );
	ArrayInitializer_int d( ArrayInitializer_int( 42 ) );
	EXPECT_TRUE( d.active() );
	EXPECT_EQ( 42, d.value() );
}

TEST( ArrayInitializerTest, Assignment )
{
	ArrayInitializer_int a( 42 );
	ArrayInitializer_int b( 99 );
	EXPECT_TRUE( a.active() );
	EXPECT_TRUE( b.active() );
	a = b;
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 99, a.value() );
	a = ArrayInitializer_int( 88 );
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 88, a.value() );
	a = 77;
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 77, a.value() );
	a = static_cast< short int >( 33 );
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 33, a.value() );
}

TEST( ArrayInitializerTest, Clear )
{
	ArrayInitializer_int a( 42 );
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 42, a.value() );
	a.clear();
	EXPECT_FALSE( a.active() );
}

TEST( ArrayInitializerTest, Swap )
{
	ArrayInitializer_int a( 42 );
	ArrayInitializer_int b;
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 42, a() );
	EXPECT_FALSE( b.active() );
	a.swap( b );
	EXPECT_FALSE( a.active() );
	EXPECT_TRUE( b.active() );
	EXPECT_EQ( 42, b.value() );
	swap( a, b );
	EXPECT_TRUE( a.active() );
	EXPECT_EQ( 42, a() );
	EXPECT_FALSE( b.active() );
}
