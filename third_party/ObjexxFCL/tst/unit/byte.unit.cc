// ObjexxFCL::byte Unit Tests
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
#include <ObjexxFCL/byte.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( byteTest, Construction )
{
	byte b( 22 );
	EXPECT_EQ( byte(22), b );
	EXPECT_EQ( 22, b );
}

TEST( byteTest, Assignment )
{
	byte b( 55 );
	EXPECT_EQ( 55, b );
	EXPECT_EQ( 57, b += 2 );
	EXPECT_EQ( 50, b -= 7 );
	EXPECT_EQ( 100, b *= 2 );
	EXPECT_EQ( 20, b /= 5 );
}

TEST( byteTest, IncrementDecrement )
{
	byte b( 22 );
	EXPECT_EQ( byte(23), ++b );
	EXPECT_EQ( short(24), ++b );
	EXPECT_EQ( byte(23), --b );
	EXPECT_EQ( 22, --b );
}

TEST( byteTest, Math )
{
	byte b( 77 );
	EXPECT_EQ( b, +b );
	EXPECT_EQ( -77, -b );
	byte c( 10 );
	EXPECT_EQ( 87, b+c );
	EXPECT_EQ( 67, b-c );
	EXPECT_EQ( 100, c*c );
	b = 20;
	c = 2;
	EXPECT_EQ( 10, b/c );
}
