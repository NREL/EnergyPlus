// ObjexxFCL::Write Unit Tests
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
#include <ObjexxFCL/Write.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <sstream>

using namespace ObjexxFCL;

TEST( WriteTest, WriteCharLDToStream )
{
	std::ostringstream stream;
	char c( 'X' );
	write( stream, "*" ) << c;
	EXPECT_EQ( " X\n", stream.str() );
}

TEST( WriteTest, WriteStringLDToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	write( stream, "*" ) << s;
	EXPECT_EQ( " ABC\n", stream.str() );
}

TEST( WriteTest, WriteFloatLDToStream )
{
	std::ostringstream stream;
	float f( 2.5 );
	write( stream, "*" ) << f;
	EXPECT_EQ( "   2.500000    \n", stream.str() );
}

TEST( WriteTest, WriteDoubleLDToStream )
{
	std::ostringstream stream;
	double d( 2.5 );
	write( stream, "*" ) << d;
	EXPECT_EQ( "   2.50000000000000     \n", stream.str() );
}

TEST( WriteTest, NonAdvancingLDToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	IOFlags flags;
	flags.na_on();
	write( stream, "(*)", flags ) << s;
	write( stream, "(*)", flags ) << s;
	EXPECT_EQ( " ABC ABC", stream.str() );
}

TEST( WriteTest, WriteMultilineStringToStream )
{
	std::ostringstream stream;
	std::string s( "ABC\nXYZ" );
	write( stream, "(2A3)" ) << s;
	EXPECT_EQ( "ABC\nXYZ\n", stream.str() );
}

TEST( WriteTest, WriteStringLiteralToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	write( stream, "(A3,\"A big fish\"$)" ) << s;
	EXPECT_EQ( "ABCA big fish", stream.str() );
}

TEST( WriteTest, WriteStringLiteralNestedToStream )
{
	std::ostringstream stream;
	write( stream, "(\"A \\\"big\\\" fish\"$)" ); // Also tests that Write does the output with no << arg
	EXPECT_EQ( "A \"big\" fish", stream.str() );
}

TEST( WriteTest, WriteStringLiteralNestedWithBackslashesToStream )
{
	{
		std::ostringstream stream;
		write( stream, "(\"A ++big++ fish\"$)" );
		EXPECT_EQ( "A ++big++ fish", stream.str() );
	}
	{
		std::ostringstream stream;
		write( stream, "(\"A \\\\\\\\big\\\\\\\\ fish\"$)" ); // Nested string so we have to quadruple the backslashes
		EXPECT_EQ( "A \\\\big\\\\ fish", stream.str() ); // Want \\big\\ so we double the backslashes here
	}
}

TEST( WriteTest, WriteSingleQuoteStringLiteralToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	write( stream, "(A3,'A big fish'$)" ) << s;
	EXPECT_EQ( "ABCA big fish", stream.str() );
}

TEST( WriteTest, WriteSingleQuoteStringLiteralNestedToStream )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	write( stream, "(A3,'A \"big\" fish'$)" ) << s;
	EXPECT_EQ( "ABCA \"big\" fish", stream.str() );
}

TEST( WriteTest, WriteSingleQuoteStringLiteralNestedToStream2 )
{
	std::ostringstream stream;
	std::string s( "ABC" );
	write( stream, "(A3,'A \"big\" \\\'fish\\\\'$)" ) << s;
	EXPECT_EQ( "ABCA \"big\" 'fish\\", stream.str() );
}

TEST( WriteTest, WriteFloatToStream )
{
	std::ostringstream stream;
	float f( 1.125 );
	write( stream, "(F5.0$)" ) << f;
	EXPECT_EQ( "   1.", stream.str() );
}

TEST( WriteTest, WriteFloatDToStream )
{
	std::ostringstream stream;
	float f( 1125 );
	write( stream, "(F6.1$)" ) << f; // Precision specified
	EXPECT_EQ( "1125.0", stream.str() );
}

TEST( WriteTest, WriteFloatEToStream )
{
	{
		std::ostringstream stream;
		float f( 1.125E3 );
		write( stream, "(F7$)" ) << f;
		EXPECT_EQ( "  1125.", stream.str() );
	}
	{
		std::ostringstream stream;
		float f( 1.125E3 );
		write( stream, "(F7.0$)" ) << f;
		EXPECT_EQ( "  1125.", stream.str() );
	}
}

TEST( WriteTest, WriteExponentToStream )
{
	std::ostringstream stream;
	float f( 1.125 );
	write( stream, "(1PE15.6$)" ) << f;
	EXPECT_EQ( "   1.125000E+00", stream.str() );
}

TEST( WriteTest, WriteCharLDToString )
{
	std::string o;
	char c( 'X' );
	write( o, "*" ) << c;
	EXPECT_EQ( " X", o );
}

TEST( WriteTest, WriteStringLDToString )
{
	std::string o;
	std::string s( "ABC" );
	write( o, "*" ) << s;
	EXPECT_EQ( " ABC", o );
}

TEST( WriteTest, WriteFloatLDToString )
{
	std::string o;
	float f( 2.5 );
	write( o, "*" ) << f;
	EXPECT_EQ( "   2.500000    ", o );
}

TEST( WriteTest, WriteDoubleLDToString )
{
	std::string o;
	double d( 2.5 );
	write( o, "*" ) << d;
	EXPECT_EQ( "   2.50000000000000     ", o );
}

TEST( WriteTest, WriteMultilineStringToString )
{
	std::string o;
	std::string s( "ABC\nXYZ" );
	write( o, "(2A3)" ) << s;
	EXPECT_EQ( "ABC\nXYZ", o );
}

TEST( WriteTest, WriteStringLiteralToString )
{
	std::string o;
	std::string s( "ABC" );
	write( o, "(A3,\"A big fish\"$)" ) << s;
	EXPECT_EQ( "ABCA big fish", o );
}

TEST( WriteTest, WriteStringLiteralNestedToString )
{
	std::string o;
	write( o, "(\"A \\\"big\\\" fish\"$)" ); // Also tests that Write does the output with no << arg
	EXPECT_EQ( "A \"big\" fish", o );
}

TEST( WriteTest, WriteStringLiteralNestedWithBackslashesToString )
{
	{
		std::string o;
		write( o, "(\"A ++big++ fish\"$)" );
		EXPECT_EQ( "A ++big++ fish", o );
	}
	{
		std::string o;
		write( o, "(\"A \\\\\\\\big\\\\\\\\ fish\"$)" ); // Nested string so we have to quadruple the backslashes
		EXPECT_EQ( "A \\\\big\\\\ fish", o ); // Want \\big\\ so we double the backslashes here
	}
}

TEST( WriteTest, WriteSingleQuoteStringLiteralToString )
{
	std::string o;
	std::string s( "ABC" );
	write( o, "(A3,'A big fish'$)" ) << s;
	EXPECT_EQ( "ABCA big fish", o );
}

TEST( WriteTest, WriteSingleQuoteStringLiteralNestedToString )
{
	std::string o;
	std::string s( "ABC" );
	write( o, "(A3,'A \"big\" fish'$)" ) << s;
	EXPECT_EQ( "ABCA \"big\" fish", o );
}

TEST( WriteTest, WriteSingleQuoteStringLiteralNestedToString2 )
{
	std::string o;
	std::string s( "ABC" );
	write( o, "(A3,'A \"big\" \\\'fish\\\\'$)" ) << s;
	EXPECT_EQ( "ABCA \"big\" 'fish\\", o );
}

TEST( WriteTest, WriteFloatToString )
{
	std::string o;
	float f( 1.125 );
	write( o, "(F5.0$)" ) << f;
	EXPECT_EQ( "   1.", o );
}

TEST( WriteTest, WriteFloatDToString )
{
	std::string o;
	float f( 1125 );
	write( o, "(F6.1$)" ) << f; // Precision specified
	EXPECT_EQ( "1125.0", o );
}

TEST( WriteTest, WriteFloatEToString )
{
	{
		std::string o;
		float f( 1.125E3 );
		write( o, "(F7$)" ) << f;
		EXPECT_EQ( "  1125.", o );
	}
	{
		std::string o;
		float f( 1.125E3 );
		write( o, "(F7.0$)" ) << f;
		EXPECT_EQ( "  1125.", o );
	}
}

TEST( WriteTest, WriteExponentToString )
{
	std::string o;
	float f( 1.125 );
	write( o, "(1PE15.6$)" ) << f;
	EXPECT_EQ( "   1.125000E+00", o );
}
