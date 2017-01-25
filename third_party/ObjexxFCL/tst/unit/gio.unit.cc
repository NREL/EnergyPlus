// ObjexxFCL::gio Unit Tests
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
#include <ObjexxFCL/gio.hh>
#include <ObjexxFCL/IOFlags.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <cstdio>
#include <string>

using namespace ObjexxFCL;
using namespace ObjexxFCL::gio;

TEST( GioTest, BasicLF )
{
	IOFlags flags;
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTestLF.txt" );
	inquire( name1, flags );
	if ( flags.exists() ) std::remove( name1.c_str() ); // Clean up from prior aborted run
	flags.ter_lf();
	open( unit1, name1, flags );
	EXPECT_FALSE( flags.err() );
	EXPECT_EQ( 0, flags.ios() );
	inquire( unit1, flags );
	EXPECT_TRUE( flags.exists() );
	EXPECT_TRUE( flags.open() );
	EXPECT_TRUE( flags.readable() );
	EXPECT_TRUE( flags.writable() );
	EXPECT_TRUE( flags.asis() );
	EXPECT_EQ( unit1, flags.unit() );
	EXPECT_EQ( name1, flags.name() );
	EXPECT_EQ( 0u, flags.size() );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	inquire( unit1, flags );
	EXPECT_EQ( 14u, flags.size() );
	rewind( unit1 );
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

TEST( GioTest, BasicCRLF )
{
	IOFlags flags;
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTestCRLF.txt" );
	flags.ter_crlf();
	open( unit1, name1, flags );
	EXPECT_FALSE( flags.err() );
	EXPECT_EQ( 0, flags.ios() );
	inquire( unit1, flags );
	EXPECT_TRUE( flags.exists() );
	EXPECT_TRUE( flags.open() );
	EXPECT_TRUE( flags.readable() );
	EXPECT_TRUE( flags.writable() );
	EXPECT_TRUE( flags.asis() );
	EXPECT_EQ( unit1, flags.unit() );
	EXPECT_EQ( name1, flags.name() );
	EXPECT_EQ( 0u, flags.size() );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	inquire( unit1, flags );
	EXPECT_EQ( 16u, flags.size() );
	rewind( unit1 );
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

TEST( GioTest, BasicEOF )
{
	IOFlags flags;
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTestBasicEOF.txt" );
	flags.ter_lf();
	open( unit1, name1, flags );
	EXPECT_FALSE( flags.err() );
	EXPECT_EQ( 0, flags.ios() );
	inquire( unit1, flags );
	EXPECT_TRUE( flags.exists() );
	EXPECT_TRUE( flags.open() );
	EXPECT_TRUE( flags.readable() );
	EXPECT_TRUE( flags.writable() );
	EXPECT_TRUE( flags.asis() );
	EXPECT_EQ( unit1, flags.unit() );
	EXPECT_EQ( name1, flags.name() );
	EXPECT_EQ( 0u, flags.size() );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	inquire( unit1, flags );
	EXPECT_EQ( 14u, flags.size() );
	rewind( unit1 );
	std::string line;
	IOFlags rflags;
	int i( 0 );
	while ( ! rflags.end() ) {
		read( unit1, "(A)", rflags ) >> line;
		if ( ! rflags.end() ) EXPECT_EQ( "Line " + std::to_string( ++i ), line );
	}
	EXPECT_EQ( 2, i );
	flags.clear().del_on(); // Enable deletion
	close( unit1, flags );
	inquire( name1, flags );
	EXPECT_FALSE( flags.exists() ); // File should be deleted
}

TEST( GioTest, InquireByName )
{
	IOFlags flags;
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTestInquireByName.txt" );
	flags.ter_lf();
	open( unit1, name1, flags );
	EXPECT_FALSE( flags.err() );
	EXPECT_EQ( 0, flags.ios() );
	inquire( unit1, flags );
	EXPECT_TRUE( flags.exists() );
	EXPECT_TRUE( flags.open() );
	EXPECT_TRUE( flags.readable() );
	EXPECT_TRUE( flags.writable() );
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

TEST( GioTest, InquireByNameNotExist )
{
	IOFlags flags;
	inquire( "GioTestInquireByNameNotExist.txt", flags );
	EXPECT_FALSE( flags.exists() );
}

TEST( GioTest, AsIsOn )
{
	IOFlags flags;
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	EXPECT_TRUE( flags.asis() );
	std::string const name1( "GioTestAsIsOn.txt" );
	flags.ter_lf();
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
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	EXPECT_TRUE( flags.asis() );
	std::string const name1( "GioTestAsIsOff.txt" );
	flags.ter_lf();
	open( unit1, name1, flags );
	write( unit1, "(A)" ) << "Line 1";
	write( unit1, "(A)" ) << "Line 2";
	flags.ter_lf();
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
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTestEndOfFile.txt" );
	flags.ter_lf();
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

TEST( GioTest, Flush )
{
	int unit1( get_unit() );
	EXPECT_TRUE( unit1 > 0 );
	std::string const name1( "GioTestFlush.txt" );
	open( unit1, name1 );
	write( unit1, "(A)" ) << "Line 1";
	flush( unit1 ); // Doesn't test flush but checks that it does no harm
	write( unit1, "(A)" ) << "Line 2";
	rewind( unit1 );
	std::string line;
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 1", line );
	read( unit1, "(A)" ) >> line;
	EXPECT_EQ( "Line 2", line );
	IOFlags flags;
	flags.del_on(); // Enable deletion
	close( unit1, flags );
}
