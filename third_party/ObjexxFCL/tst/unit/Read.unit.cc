// ObjexxFCL::Read Unit Tests
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
#include <ObjexxFCL/Read.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <sstream>

using namespace ObjexxFCL;

TEST( ReadTest, ReadCharFromEmptyStream )
{
	std::istringstream stream; // Empty
	char c( 'X' );
	IOFlags flags;
	read( stream, "*", flags ) >> c;
	EXPECT_EQ( 'X', c ); // Nothing read: Value not changed: Matches Fortran behavior
}

TEST( ReadTest, ReadStringFromEmptyStream )
{
	std::istringstream stream; // Empty
	std::string s;
	IOFlags flags;
	read( stream, "*", flags ) >> s;
	EXPECT_TRUE( s.empty() ); // Nothing read
}

TEST( ReadTest, ReadCharFromStream )
{
	std::istringstream stream( "ABC" );
	char c( 'X' );
	read( stream, "*" ) >> c;
	EXPECT_EQ( 'A', c );
}

TEST( ReadTest, ReadStringFromStream )
{
	std::istringstream stream( "ABC" );
	std::string s;
	read( stream, "*" ) >> s;
	EXPECT_EQ( "ABC", s );
}

TEST( ReadTest, ReadStringFromMultilineStream )
{
	std::istringstream stream( "ABC\nXYZ" );
	std::string s;
	read( stream, "*" ) >> s;
	EXPECT_EQ( "ABC", s );
	read( stream, "*" ) >> s;
	EXPECT_EQ( "XYZ", s );
}

TEST( ReadTest, ReadStringFromMultilineCRLFStream )
{
	std::istringstream stream( "ABC\r\nXYZ" );
	std::string s;
	read( stream, "*" ) >> s;
	EXPECT_EQ( "ABC", s );
	read( stream, "*" ) >> s;
	EXPECT_EQ( "XYZ", s );
}

TEST( ReadTest, ReadFloatFromStream )
{
	std::istringstream stream( "1.125" );
	float f;
	read( stream, "(F5.0)" ) >> f;
	EXPECT_EQ( 1.125f, f );
}

TEST( ReadTest, ReadFloatDFromStream )
{
	std::istringstream stream( "1125" );
	float f;
	read( stream, "(F4.3)" ) >> f; // Precision specified
	EXPECT_EQ( 1.125f, f );
}

TEST( ReadTest, ReadFloatEFromStream )
{
	{
		std::istringstream stream( "1.125E3" );
		float f;
		read( stream, "(F7)" ) >> f;
		EXPECT_EQ( 1125.0f, f );
	}
	{
		std::istringstream stream( "1.125E3" );
		float f;
		read( stream, "(F7.0)" ) >> f;
		EXPECT_EQ( 1125.0f, f );
	}
}

TEST( ReadTest, ReadFloatPFromStream )
{
	{
		std::istringstream stream( "1.125+3" );
		float f;
		read( stream, "(F7)" ) >> f;
		EXPECT_EQ( 1125.0f, f );
	}
	{
		std::istringstream stream( "1.125+3" );
		float f;
		read( stream, "(F7.0)" ) >> f;
		EXPECT_EQ( 1125.0f, f );
	}
}

TEST( ReadTest, ReadStringFromString )
{
	{
		std::string const icity( "Paris" );
		std::string ocity;
		read( icity, "*" ) >> ocity;
		EXPECT_EQ( icity, ocity );
	}
	{
		std::string const icity( "Paris" );
		std::string ocity;
		read( icity, "(A)" ) >> ocity;
		EXPECT_EQ( icity, ocity );
	}
}
