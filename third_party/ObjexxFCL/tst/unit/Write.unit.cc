// ObjexxFCL::Write Unit Tests
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
#include <ObjexxFCL/Write.hh>

// C++ Headers
#include <sstream>

using namespace ObjexxFCL;

TEST( WriteTest, WriteCharToStream )
{
	std::ostringstream stream;
	char c( 'X' );
	Write( stream, "*" ) << c;
	EXPECT_EQ( " X\n", stream.str() );
}

TEST( WriteTest, WriteStringToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	Write( stream, "*" ) << s;
	EXPECT_EQ( " ABC\n", stream.str() );
}

TEST( WriteTest, NonAdvancingListDirected )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	IOFlags flags;
	flags.na_on();
	Write( stream, "(*)", flags ) << s;
	Write( stream, "(*)", flags ) << s;
	EXPECT_EQ( " ABC ABC", stream.str() );
}

TEST( WriteTest, WriteMultilineStringToStream )
{
	std::ostringstream stream;
	std::string s( "ABC\nXYZ" );
	Write( stream, "(2A3)" ) << s;
	EXPECT_EQ( "ABC\nXYZ\n", stream.str() );
}

TEST( WriteTest, WriteStringLiteralToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	Write( stream, "(A3,\"A big fish\"$)" ) << s;
	EXPECT_EQ( "ABCA big fish", stream.str() );
}

TEST( WriteTest, WriteStringLiteralNestedToStream )
{
	std::ostringstream stream;
	Write( stream, "(\"A \\\"big\\\" fish\"$)" ); // Also tests that Write does the output with no << arg
	EXPECT_EQ( "A \"big\" fish", stream.str() );
}

TEST( WriteTest, WriteStringLiteralNestedWithBackslashesToStream )
{
	{
		std::ostringstream stream;
		Write( stream, "(\"A ++big++ fish\"$)" );
		EXPECT_EQ( "A ++big++ fish", stream.str() );
	}
	{
		std::ostringstream stream;
		Write( stream, "(\"A \\\\\\\\big\\\\\\\\ fish\"$)" ); // Nested string so we have to quadruple the backslashes
		EXPECT_EQ( "A \\\\big\\\\ fish", stream.str() ); // Want \\big\\ so we double the backslashes here
	}
}

TEST( WriteTest, WriteSingleQuoteStringLiteralToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	Write( stream, "(A3,'A big fish'$)" ) << s;
	EXPECT_EQ( "ABCA big fish", stream.str() );
}

TEST( WriteTest, WriteSingleQuoteStringLiteralNestedToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	Write( stream, "(A3,'A \"big\" fish'$)" ) << s;
	EXPECT_EQ( "ABCA \"big\" fish", stream.str() );
}

TEST( WriteTest, WriteSingleQuoteStringLiteralNestedToStream2 )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	Write( stream, "(A3,'A \"big\" \\\'fish\\\\'$)" ) << s;
	EXPECT_EQ( "ABCA \"big\" 'fish\\", stream.str() );
}

TEST( WriteTest, WriteFloatToStream )
{
	std::ostringstream stream;
	float f( 1.125 );
	Write( stream, "(F5.0$)" ) << f;
	EXPECT_EQ( "   1.", stream.str() );
}

TEST( WriteTest, WriteFloatDToStream )
{
	std::ostringstream stream;
	float f( 1125 );
	Write( stream, "(F6.1$)" ) << f; // Precision specified
	EXPECT_EQ( "1125.0", stream.str() );
}

TEST( WriteTest, WriteFloatEToStream )
{
	{
		std::ostringstream stream;
		float f( 1.125E3 );
		Write( stream, "(F7$)" ) << f;
		EXPECT_EQ( "  1125.", stream.str() );
	}
	{
		std::ostringstream stream;
		float f( 1.125E3 );
		Write( stream, "(F7.0$)" ) << f;
		EXPECT_EQ( "  1125.", stream.str() );
	}
}
