// ObjexxFCL::char.functions Unit Tests
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
#include <ObjexxFCL/char.functions.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( charFunctionsTest, CaseChange )
{
	char s( 'F' );
	lowercase( s );
	EXPECT_EQ( 'f', s );
	uppercase( s );
	EXPECT_EQ( 'F', s );
	EXPECT_TRUE( equali( s, 'f' ) );
	EXPECT_EQ( 'f', lowercased( s ) );
	EXPECT_EQ( 'F', uppercased( s ) );
}

TEST( charFunctionsTest, Predicate )
{
	EXPECT_TRUE( equal( 'a', 'a' ) );
	EXPECT_TRUE( equal( 'a', 'a', true ) );
	EXPECT_FALSE( equal( 'a', 'A' ) );
	EXPECT_TRUE( equal( 'a', 'A', false ) );
	EXPECT_TRUE( equali( 'a', 'A' ) );
	EXPECT_TRUE( is_blank( ' ' ) );
	EXPECT_FALSE( is_blank( 'x' ) );
	EXPECT_TRUE( not_blank( 'x' ) );
	EXPECT_TRUE( is_any_of( 'x', "xyz" ) );
	EXPECT_TRUE( is_any_of( 'x', std::string( "xyz" ) ) );
	EXPECT_FALSE( is_any_of( 'b', "xyz" ) );
}
