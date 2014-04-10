// ObjexxFCL::string.functions Unit Tests
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
#include <ObjexxFCL/string.functions.hh>

using namespace ObjexxFCL;
using std::string;

TEST( stringFunctionsTest, CaseChange )
{
	string s( "Fish" );
	lowercase( s );
	EXPECT_EQ( "fish", s );
	uppercase( s );
	EXPECT_EQ( "FISH", s );
	EXPECT_TRUE( equali( s, "fiSh" ) );
	EXPECT_EQ( "fish", lowercased( s ) );
	EXPECT_EQ( "FISH", uppercased( s ) );
}

TEST( stringFunctionsTest, JustificationTrimming )
{
	string s( "  Fish " );
	EXPECT_EQ( 6u, len_trim( s ) );
	EXPECT_EQ( 6u, len_trim_whitespace( s ) );
	left_justify( s );
	EXPECT_EQ( "Fish   ", s );
	right_justify( s );
	EXPECT_EQ( "   Fish", s );
	EXPECT_EQ( "Fish   ", left_justified( s ) );
	left_justify( s );
	EXPECT_EQ( "   Fish", right_justified( s ) );
	s = "Bozo \t ";
	EXPECT_EQ( 6u, len_trim( s ) );
	EXPECT_EQ( 4u, len_trim_whitespace( s ) );
	EXPECT_EQ( "Bozo \t", trimmed( s ) );
	EXPECT_EQ( "Bozo", trimmed_whitespace( s ) );
	trim_whitespace( s );
	EXPECT_EQ( "Bozo", s );
}

TEST( stringFunctionsTest, StripSpace )
{
	string s( "  Fish " );
	EXPECT_EQ( "Fish", stripped( s ) );
	EXPECT_EQ( "Fish ", lstripped( s ) );
	EXPECT_EQ( "  Fish", rstripped( s ) );
	lstrip( s );
	EXPECT_EQ( "Fish ", s );
	rstrip( s );
	EXPECT_EQ( "Fish", s );
	s = "  Cow  ";
	strip( s );
	EXPECT_EQ( "Cow", s );
}

TEST( stringFunctionsTest, StripWhitespace )
{
	string s( " \0\0\t \t\0\t Fish \t\0 ", 17 );
	EXPECT_EQ( string( "\0\0\t \t\0\t Fish \t\0", 15 ), stripped( s ) );
	EXPECT_EQ( "Fish", stripped_whitespace( s ) );
	EXPECT_EQ( string( "Fish \t\0 ", 8 ), lstripped_whitespace( s ) );
	EXPECT_EQ( string( " \0\0\t \t\0\t Fish", 13 ), rstripped_whitespace( s ) );
	lstrip_whitespace( s );
	EXPECT_EQ( string( "Fish \t\0 ", 8 ), s );
	rstrip_whitespace( s );
	EXPECT_EQ( "Fish", s );
	s = string( " \0\0\t\t \0\t  Cow \t\t \t \0 ", 21 );
	strip_whitespace( s );
	EXPECT_EQ( "Cow", s );
}

TEST( stringFunctionsTest, StripSpecifiedCharacters )
{
	string s( "Fish" );
	EXPECT_EQ( "Fis", stripped( s, "h" ) );
	EXPECT_EQ( "ish", stripped( s, "F" ) );
	EXPECT_EQ( "is", stripped( s, "Fh" ) );
	EXPECT_EQ( "Fish", lstripped( s, "xyz" ) );
	EXPECT_EQ( "ish", lstripped( s, "Fx" ) );
	EXPECT_EQ( "Fish", rstripped( s, "abc" ) );
	EXPECT_EQ( "Fi", rstripped( s, "asbch" ) );
	lstrip( s, "F" );
	EXPECT_EQ( "ish", s );
	rstrip( s, "sh" );
	EXPECT_EQ( "i", s );
}

TEST( stringFunctionsTest, PrefixSuffix )
{
	string const s( "Fish Tank" );
	EXPECT_TRUE( has_prefix( s, "Fi" ) );
	EXPECT_FALSE( has_prefix( s, "Fin" ) );
	EXPECT_TRUE( has_suffix( s, "Tank" ) );
	EXPECT_FALSE( has_suffix( s, "Face" ) );
	string const t( "A cat is a cat" );
	EXPECT_TRUE( has_suffix( t, "cat" ) ); // Find last instance
}

TEST( stringFunctionsTest, Centering )
{
	string s( "    Fish Tank" );
	EXPECT_EQ( "  Fish Tank  ", centered( s ) );
	EXPECT_EQ( "  Fish Tank  ", center( s ) );
	EXPECT_EQ( "  Fish Tank  ", s );
}

TEST( stringFunctionsTest, Replace )
{
	string const s( "Fish Tank" );
	EXPECT_EQ( "Dish Tank", replaced( s, "F", "D" ) );
	EXPECT_EQ( "Foolish Tank", replaced( s, "ish", "oolish" ) );
	EXPECT_EQ( "Fish Truck", replaced( s, "ank", "ruck" ) );
	// VC++2013 can't handle some escape sequences in macros like EXPECT that use the # stringize operator so we define them out of line
	string const t( "A \\\"Fishy\\\" Story" );
	string const t2( "A \"Fishy\" Story" );
	string const slash( "\\" );
	string const dslash( "\\\\" );
	string const slash_dquote( "\\\"" );
	string const dquote( "\"" );
	EXPECT_EQ( t2, replaced( replaced( t, dslash, slash ), slash_dquote, dquote ) );
}
