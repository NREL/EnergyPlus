// ObjexxFCL::Backspace Unit Tests
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
#include <ObjexxFCL/Backspace.hh>
#include <ObjexxFCL/Read.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <sstream>

using namespace ObjexxFCL;

TEST( BackspaceTest, BackspaceEmptyIStream )
{
	std::istringstream stream; // Empty
	Backspace( stream );
	EXPECT_EQ( 0u, stream.tellg() );
}

TEST( BackspaceTest, BackspaceIStream )
{
	std::istringstream stream( "Line 1\nLine 2\nLine 3\n" );
	std::string s;
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 1", s );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 2", s );
	Backspace( stream );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 2", s );
	Backspace( stream );
	Backspace( stream );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 1", s );
	Backspace( stream );
	Backspace( stream );
	Backspace( stream );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 1", s );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 2", s );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 3", s );
	Backspace( stream );
	read( stream, "(A)" ) >> s;
	EXPECT_EQ( "Line 3", s );
}
