// ObjexxFCL::Stream Unit Tests
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
#include <ObjexxFCL/Stream.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <cstdio>

using namespace ObjexxFCL;

TEST( StreamTest, OStream )
{
	OStream stream( std::cout, "stdout" );
	EXPECT_EQ( "stdout", stream.name() );
	EXPECT_FALSE( stream.is_file() );
}

TEST( StreamTest, IStream )
{
	IStream stream( std::cin, "stdin" );
	EXPECT_EQ( "stdin", stream.name() );
	EXPECT_FALSE( stream.is_file() );
}

TEST( StreamTest, IStringStream )
{
	IStringStream stream( "A short text" );
	EXPECT_EQ( std::string(), stream.name() );
	EXPECT_FALSE( stream.is_file() );
	EXPECT_EQ( "A short text", stream().str() );
}

TEST( StreamTest, OStringStream )
{
	OStringStream stream( "A short text" );
	EXPECT_EQ( stream.name(), std::string() );
	EXPECT_FALSE( stream.is_file() );
	EXPECT_EQ( "A short text", stream().str() );
}

TEST( StreamTest, FileStream )
{
	{ // Output
		OFileStream stream( "StreamTestFileStream.txt" );
		EXPECT_EQ( "StreamTestFileStream.txt", stream.name() );
		EXPECT_TRUE( stream.is_file() );
		EXPECT_TRUE( stream.is_open() );
		stream() << "This is line 1" << std::endl;
		stream() << "This is line 2" << std::endl;
		stream.close(); // Keep file for input test
	}

	{ // Input
		IFileStream stream( "StreamTestFileStream.txt" );
		EXPECT_EQ( "StreamTestFileStream.txt", stream.name() );
		EXPECT_TRUE( stream.is_file() );
		EXPECT_TRUE( stream.is_open() );
		std::string line;
		std::getline( stream(), line );
		EXPECT_EQ( "This is line 1", line );
		std::getline( stream(), line );
		EXPECT_EQ( "This is line 2", line );
		stream.close();
		EXPECT_FALSE( stream.is_open() );
		stream.open();
		EXPECT_TRUE( stream.is_open() );
		stream.close();
		EXPECT_FALSE( stream.is_open() );
		std::remove( stream.name().c_str() ); // Clean up
	}
}

TEST( StreamTest, ScratchFileName )
{
	std::string const sf_name( Stream::scratch_name() );
	EXPECT_TRUE( has_suffix( sf_name, ".tmp" ) );
	EXPECT_TRUE( sf_name.length() >= 12 );
	std::string::size_type const b( sf_name.length() - 12 );
	EXPECT_EQ( sf_name[ b ], 's' );
	EXPECT_TRUE( is_digit( sf_name.substr( b + 1, 7 ) ) );
}

TEST( StreamTest, ScratchFileUse )
{
	IOFlags flags;
	flags.scratch_on();
	OFileStream stream( Stream::scratch_name(), flags );
	EXPECT_TRUE( stream.is_file() );
	EXPECT_TRUE( stream.is_open() );
	stream() << "This is line 1" << std::endl;
	stream() << "This is line 2" << std::endl;
	stream.close();
	EXPECT_FALSE( std::ifstream( stream.name() ).good() ); // Should have been deleted when closed
}
