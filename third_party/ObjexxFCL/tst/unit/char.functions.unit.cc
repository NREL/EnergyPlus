// ObjexxFCL::char.functions Unit Tests
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

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/char.functions.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( charFunctionsTest, Predicate )
{
	EXPECT_TRUE( is_blank( ' ' ) );
	EXPECT_FALSE( is_blank( 'x' ) );
	EXPECT_TRUE( not_blank( 'x' ) );
	EXPECT_TRUE( is_whitespace( ' ' ) );
	EXPECT_TRUE( is_whitespace( '\t' ) );
	EXPECT_FALSE( is_whitespace( 'x' ) );
	EXPECT_FALSE( not_whitespace( ' ' ) );
	EXPECT_FALSE( not_whitespace( '\t' ) );
	EXPECT_TRUE( not_whitespace( 'x' ) );
	EXPECT_TRUE( is_alpha( 'x' ) );
	EXPECT_FALSE( is_alpha( '3' ) );
	EXPECT_TRUE( is_consonant( 'x' ) );
	EXPECT_FALSE( is_consonant( 'a' ) );
	EXPECT_TRUE( is_vowel( 'e' ) );
	EXPECT_FALSE( is_vowel( 'z' ) );
	EXPECT_TRUE( is_alpha_numeric( 'B' ) );
	EXPECT_TRUE( is_alpha_numeric( 'y' ) );
	EXPECT_TRUE( is_alpha_numeric( '4' ) );
	EXPECT_FALSE( is_alpha_numeric( '$' ) );
	EXPECT_TRUE( is_digit( '4' ) );
	EXPECT_FALSE( is_digit( 'P' ) );
	EXPECT_TRUE( is_lower( 'e' ) );
	EXPECT_FALSE( is_lower( 'E' ) );
	EXPECT_TRUE( is_upper( 'B' ) );
	EXPECT_FALSE( is_upper( 'b' ) );
	EXPECT_TRUE( is_any_of( 'x', "xyz" ) );
	EXPECT_TRUE( is_any_of( 'x', std::string( "xyz" ) ) );
	EXPECT_FALSE( is_any_of( 'b', "xyz" ) );
	EXPECT_TRUE( has_any_of( 'x', "xyz" ) );
	EXPECT_TRUE( has_any_of( 'x', "xyz" ) );
	EXPECT_FALSE( has_any_of( 'b', std::string( "xyz" ) ) );
	EXPECT_FALSE( has_any_of( 'b', std::string( "xyz" ) ) );
	EXPECT_FALSE( not_any_of( 'x', "xyz" ) );
	EXPECT_FALSE( not_any_of( 'x', "xyz" ) );
	EXPECT_TRUE( not_any_of( 'b', std::string( "xyz" ) ) );
	EXPECT_TRUE( not_any_of( 'b', std::string( "xyz" ) ) );
}

TEST( charFunctionsTest, Comparison )
{
	EXPECT_TRUE( equal( 'a', 'a' ) );
	EXPECT_TRUE( equal( 'a', 'a', true ) );
	EXPECT_FALSE( equal( 'a', 'A' ) );
	EXPECT_TRUE( equal( 'a', 'A', false ) );
	EXPECT_TRUE( equali( 'a', 'A' ) );
	EXPECT_FALSE( equali( 'a', 'X' ) );
	EXPECT_TRUE( lessthan( 'a', 'b' ) );
	EXPECT_TRUE( lessthan( 'a', 'b', true ) );
	EXPECT_FALSE( lessthan( 'a', 'B', true ) );
	EXPECT_TRUE( lessthan( 'A', 'b', false ) );
	EXPECT_FALSE( lessthan( 'b', 'A', false ) );
	EXPECT_TRUE( lessthani( 'a', 'b' ) );
	EXPECT_TRUE( lessthani( 'a', 'B' ) );
	EXPECT_TRUE( lessthani( 'A', 'b' ) );

	EXPECT_TRUE( LLT( 'a', 'b' ) );
	EXPECT_FALSE( LLT( 'a', 'a' ) );
	EXPECT_FALSE( LLT( 'b', 'a' ) );
	EXPECT_TRUE( LLE( 'a', 'b' ) );
	EXPECT_TRUE( LLE( 'a', 'a' ) );
	EXPECT_FALSE( LLE( 'b', 'a' ) );
	EXPECT_FALSE( LGT( 'a', 'b' ) );
	EXPECT_FALSE( LGT( 'a', 'a' ) );
	EXPECT_TRUE( LGT( 'b', 'a' ) );
	EXPECT_FALSE( LGE( 'a', 'b' ) );
	EXPECT_TRUE( LGE( 'a', 'a' ) );
	EXPECT_TRUE( LGE( 'b', 'a' ) );

	EXPECT_TRUE( llt( 'a', 'b' ) );
	EXPECT_FALSE( llt( 'a', 'a' ) );
	EXPECT_FALSE( llt( 'b', 'a' ) );
	EXPECT_TRUE( lle( 'a', 'b' ) );
	EXPECT_TRUE( lle( 'a', 'a' ) );
	EXPECT_FALSE( lle( 'b', 'a' ) );
	EXPECT_FALSE( lgt( 'a', 'b' ) );
	EXPECT_FALSE( lgt( 'a', 'a' ) );
	EXPECT_TRUE( lgt( 'b', 'a' ) );
	EXPECT_FALSE( lge( 'a', 'b' ) );
	EXPECT_TRUE( lge( 'a', 'a' ) );
	EXPECT_TRUE( lge( 'b', 'a' ) );
}

TEST( charFunctionsTest, Conversion )
{
	EXPECT_EQ( 99, ICHAR( 'c' ) );
	EXPECT_EQ( 99, IACHAR( 'c' ) );
}

TEST( charFunctionsTest, Modifier )
{
	char s( 'F' );
	EXPECT_EQ( 'F', s );
	lowercase( s );
	EXPECT_EQ( 'f', s );
	uppercase( s );
	EXPECT_EQ( 'F', s );
	EXPECT_TRUE( equal( s, 'F' ) );
	EXPECT_FALSE( equal( s, 'f' ) );
	EXPECT_TRUE( equal( s, 'f', false ) );
	EXPECT_TRUE( equali( s, 'F' ) );
	EXPECT_TRUE( equali( s, 'f' ) );
	EXPECT_EQ( 'f', lowercase( s ) );
	EXPECT_EQ( 'F', uppercase( s ) );
}

TEST( charFunctionsTest, Generator )
{
	char const s( 'F' );
	EXPECT_EQ( 'f', lowercased( s ) );
	EXPECT_EQ( 'F', uppercased( s ) );
	EXPECT_EQ( 'f', to_lower( s ) );
	EXPECT_EQ( 'F', to_upper( s ) );
}
