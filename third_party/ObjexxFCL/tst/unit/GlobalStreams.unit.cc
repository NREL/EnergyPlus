// ObjexxFCL::GlobalStreams Unit Tests
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
#include <ObjexxFCL/GlobalStreams.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <cstdio>

using namespace ObjexxFCL;
using namespace ObjexxFCL::gio;

TEST( GlobalStreamsTest, Basic )
{
	GlobalStreams streams;
	EXPECT_EQ( 3u, streams.size() );
	streams.add( 11, "GlobalStreams11.txt" );
	streams.add( 12, "GlobalStreams12.txt" );
	EXPECT_EQ( 5u, streams.size() );
	EXPECT_TRUE( streams.has( 11 ) );
	EXPECT_TRUE( streams.has( 12 ) );
	EXPECT_FALSE( streams.has( 13 ) );
	EXPECT_TRUE( streams.has( "GlobalStreams11.txt" ) );
	EXPECT_TRUE( streams.has( "GlobalStreams12.txt" ) );
	EXPECT_FALSE( streams.has( "NoSuchFile.txt" ) );
	streams.del( 11 );
	EXPECT_FALSE( streams.has( 11 ) );
	EXPECT_TRUE( streams.has( 12 ) );
	EXPECT_FALSE( streams.has( 13 ) );
	EXPECT_FALSE( streams.has( "GlobalStreams11.txt" ) );
	EXPECT_TRUE( streams.has( "GlobalStreams12.txt" ) );
	EXPECT_FALSE( streams.has( "NoSuchFile.txt" ) );
	EXPECT_EQ( 4u, streams.size() );
	streams.clear();
	EXPECT_FALSE( streams.has( 11 ) );
	EXPECT_FALSE( streams.has( 12 ) );
	EXPECT_FALSE( streams.has( 13 ) );
	EXPECT_FALSE( streams.has( "GlobalStreams11.txt" ) );
	EXPECT_FALSE( streams.has( "GlobalStreams12.txt" ) );
	EXPECT_FALSE( streams.has( "NoSuchFile.txt" ) );
	EXPECT_EQ( 3u, streams.size() );
	std::remove( "GlobalStreams11.txt" );
	std::remove( "GlobalStreams12.txt" );
}
