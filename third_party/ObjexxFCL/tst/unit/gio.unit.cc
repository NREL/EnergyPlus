// ObjexxFCL::gio Unit Tests
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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/IOFlags.hh>

// C++ Headers
#include <cstdio>
#include <string>

using namespace ObjexxFCL;
using namespace ObjexxFCL::gio;

TEST( GioTest, Basic )
{
	IOFlags flags;
	int unit1( gio::get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTest.txt" );
	open( unit1, name1, flags );
	EXPECT_FALSE( flags.err() );
	EXPECT_EQ( 0, flags.ios() );
	inquire( unit1, flags );
	EXPECT_TRUE( flags.exists() );
	EXPECT_TRUE( flags.open() );
	EXPECT_TRUE( flags.read() );
	EXPECT_TRUE( flags.write() );
	EXPECT_TRUE( flags.asis() );
	EXPECT_EQ( unit1, flags.unit() );
	EXPECT_EQ( name1, flags.name() );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";

	rewind( unit1 );
	// Alternative: Close and re-open
//	gio::close( unit1 );
//	flags.read_on();
//	open( unit1, name1, flags );

	std::string line;
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 1", line );
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 2", line );
	flags.clear().del_on(); // Enable deletion
	close( unit1, flags );
	inquire( name1, flags );
	EXPECT_FALSE( flags.exists() ); // File should be deleted
}

TEST( GioTest, InquireByName )
{
	IOFlags flags;
	int unit1( gio::get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTest.txt" );
	open( unit1, name1, flags );
	EXPECT_FALSE( flags.err() );
	EXPECT_EQ( 0, flags.ios() );
	inquire( unit1, flags );
	EXPECT_TRUE( flags.exists() );
	EXPECT_TRUE( flags.open() );
	EXPECT_TRUE( flags.read() );
	EXPECT_TRUE( flags.write() );
	EXPECT_TRUE( flags.asis() );
	EXPECT_EQ( unit1, flags.unit() );
	EXPECT_EQ( name1, flags.name() );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	gio::close( unit1 );
	inquire( name1, flags );
	EXPECT_TRUE( flags.exists() );
	std::remove( name1.c_str() );
}

TEST( GioTest, AsIsOn )
{
	IOFlags flags;
	int unit1( gio::get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	EXPECT_TRUE( flags.asis() );
	std::string const name1( "GioTest.txt" );
	open( unit1, name1, flags );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	open( unit1, flags ); // Should continue with open stream
	write( unit1, "(A)" ) << "Line 3";

	rewind( unit1 );

	std::string line;
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 1", line );
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 2", line );
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 3", line );
	flags.clear().del_on(); // Enable deletion
	close( unit1, flags );
	inquire( name1, flags );
	EXPECT_FALSE( flags.exists() ); // File should be deleted
}

TEST( GioTest, AsIsOff )
{
	IOFlags flags;
	int unit1( gio::get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	EXPECT_TRUE( flags.asis() );
	std::string const name1( "GioTest.txt" );
	open( unit1, name1, flags );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	flags.rewind_on();
	EXPECT_TRUE( flags.rewind() );
	EXPECT_FALSE( flags.asis() );
	open( unit1, name1, flags ); // Should re-open
	write( unit1, "(A)" ) << "Line 3";

	rewind( unit1 );

	std::string line;
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 3", line );
	flags.clear().del_on(); // Enable deletion
	close( unit1, flags );
	inquire( name1, flags );
	EXPECT_FALSE( flags.exists() ); // File should be deleted
}

TEST( GioTest, EndOfFile )
{
	IOFlags flags;
	int unit1( gio::get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTest.txt" );
	open( unit1, name1, flags );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	gio::close( unit1 );

	flags.read_on();
	open( unit1, name1, flags );
	flags.clear();
	std::string line;
	read( unit1, "(A)", flags ) >> line;
	EXPECT_EQ( "Line 1", line );
	EXPECT_EQ( 0, flags.ios() );
	read( unit1, "(A)", flags ) >> line;
	EXPECT_EQ( "Line 2", line );
	EXPECT_EQ( 0, flags.ios() );
	read( unit1, "(A8)", flags ) >> line; // Try to read more characters than present
	EXPECT_EQ( "", line );
	EXPECT_EQ( -1, flags.ios() ); // Hit eof since we couldn't read specified number of characters
	EXPECT_TRUE( flags.end() );

	flags.clear().del_on(); // Enable deletion
	close( unit1, flags );
	inquire( name1, flags );
	EXPECT_FALSE( flags.exists() ); // File should be deleted
}
