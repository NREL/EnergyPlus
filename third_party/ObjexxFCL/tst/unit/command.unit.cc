// ObjexxFCL::command Unit Tests
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
#include <ObjexxFCL/command.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( CommandTest, GetCommandArgumentCount )
{
	EXPECT_TRUE( GET_COMMAND_ARGUMENT_COUNT() >= 0 );
}

TEST( CommandTest, Iargc )
{
	EXPECT_TRUE( IARGC() >= 0 );
}

TEST( CommandTest, Iarg )
{
	EXPECT_TRUE( IARG() >= 0 );
}

TEST( CommandTest, Numarg )
{
	EXPECT_TRUE( NUMARG() >= 0 );
}

TEST( CommandTest, Nargs )
{
	EXPECT_TRUE( NARGS() >= 1 );
}

TEST( CommandTest, GetCommand )
{
	{
		std::string command;
		int length;
		int status;
		GET_COMMAND( command, length, status );
#ifdef OBJEXX_BUILD
		EXPECT_EQ( "ObjexxFCL.unit", command.substr( 0, 14 ) );
		EXPECT_TRUE( 14 <= length );
#endif
		EXPECT_EQ( 0, status );
	}

	{
		std::string command;
		int length;
		int status;
		get_command( command, length, status );
#ifdef OBJEXX_BUILD
		EXPECT_EQ( "ObjexxFCL.unit", command.substr( 0, 14 ) );
		EXPECT_TRUE( 14 <= length );
#endif
		EXPECT_EQ( 0, status );
	}
}

TEST( CommandTest, GetCommandArgument )
{
	{
		std::string value;
		int length;
		int status;
		GET_COMMAND_ARGUMENT( 0, value, length, status );
#ifdef OBJEXX_BUILD
#ifdef _WIN32
		EXPECT_EQ( "ObjexxFCL.unit.exe", value );
		EXPECT_EQ( 18, length );
#else
		EXPECT_EQ( "ObjexxFCL.unit", value );
		EXPECT_EQ( 14, length );
#endif
#endif
		EXPECT_EQ( 0, status );
	}

	{
		std::string value;
		int length;
		int status;
		GET_COMMAND_ARGUMENT( 999, value, length, status ); // No argument number 999
		EXPECT_TRUE( value.empty() );
		EXPECT_EQ( 0, length );
		EXPECT_EQ( 1, status );
	}

	{
		std::string value;
		int length;
		int status;
		get_command_argument( 0, value, length, status );
#ifdef OBJEXX_BUILD
#ifdef _WIN32
		EXPECT_EQ( "ObjexxFCL.unit.exe", value );
		EXPECT_EQ( 18, length );
#else
		EXPECT_EQ( "ObjexxFCL.unit", value );
		EXPECT_EQ( 14, length );
#endif
#endif
		EXPECT_EQ( 0, status );
	}

	{
		std::string value;
		int length;
		int status;
		get_command_argument( 999, value, length, status ); // No argument number 999
		EXPECT_TRUE( value.empty() );
		EXPECT_EQ( 0, length );
		EXPECT_EQ( 1, status );
	}
}

TEST( CommandTest, Getarg )
{
	{
		std::string buffer;
		int status;
		GETARG( 0, buffer, status );
#ifdef OBJEXX_BUILD
#ifdef _WIN32
		EXPECT_EQ( "ObjexxFCL.unit.exe", buffer );
		EXPECT_EQ( 18, status );
#else
		EXPECT_EQ( "ObjexxFCL.unit", buffer );
		EXPECT_EQ( 14, status );
#endif
#endif
	}

	{
		std::string buffer;
		int status;
		GETARG( 999, buffer, status ); // No argument number 999
		EXPECT_TRUE( buffer.empty() );
		EXPECT_EQ( -1, status );
	}

	{
		std::string buffer;
		int status;
		getarg( 0, buffer, status );
#ifdef OBJEXX_BUILD
#ifdef _WIN32
		EXPECT_EQ( "ObjexxFCL.unit.exe", buffer );
		EXPECT_EQ( 18, status );
#else
		EXPECT_EQ( "ObjexxFCL.unit", buffer );
		EXPECT_EQ( 14, status );
#endif
#endif
	}

	{
		std::string buffer;
		int status;
		getarg( 999, buffer, status ); // No argument number 999
		EXPECT_TRUE( buffer.empty() );
		EXPECT_EQ( -1, status );
	}
}
