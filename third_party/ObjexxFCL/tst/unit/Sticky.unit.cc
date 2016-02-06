// ObjexxFCL::Sticky Unit Tests
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
#include <ObjexxFCL/Sticky.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( StickyTest, Construction )
{
	Sticky_int a;
	Sticky_int b( a );
	Sticky_int c( 42 );
	EXPECT_EQ( 42, c.value() );
	EXPECT_EQ( 42, c() );
	EXPECT_EQ( 42, c );
	Sticky_int d( Sticky_int( 42 ) );
	EXPECT_EQ( 42, d.value() );
}

TEST( StickyTest, Conversion )
{
	Sticky_int a( 42 );
	EXPECT_EQ( 42, a );
}

TEST( StickyTest, Assignment )
{
	Sticky_int a( 42 );
	Sticky_int b( 99 );
	a = b;
	EXPECT_EQ( 99, a.value() );
	a = Sticky_int( 88 );
	EXPECT_EQ( 88, a.value() );
	a = 77;
	EXPECT_EQ( 77, a.value() );
	a = static_cast< short int >( 33 );
	EXPECT_EQ( 33, a.value() );
}

TEST( StickyTest, Clear )
{
	Sticky_int a( 42 );
	EXPECT_EQ( 42, a );
	a.clear();
}

TEST( StickyTest, Swap )
{
	Sticky_int a( 42 );
	Sticky_int b;
	EXPECT_EQ( 42, a );
	a.swap( b );
	EXPECT_EQ( 42, b );
	swap( a, b );
	EXPECT_EQ( 42, a );
}
