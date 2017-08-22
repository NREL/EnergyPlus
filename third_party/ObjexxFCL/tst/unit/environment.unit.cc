// ObjexxFCL::environment Unit Tests
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
#include <ObjexxFCL/environment.hh>
#include "ObjexxFCL.unit.hh"

using namespace ObjexxFCL;

TEST( EnvironmentTest, GetEnvironmentVariable )
{
	{
		EXPECT_TRUE( SETENV( "ObjexxFPL_Test_Env_Var", "GET_ENVIRONMENT_VARIABLE" ) );
		std::string val;
		GET_ENVIRONMENT_VARIABLE( "ObjexxFPL_Test_Env_Var", val );
		EXPECT_EQ( "GET_ENVIRONMENT_VARIABLE", val );
	}

	{
		EXPECT_TRUE( SETENV( "ObjexxFCL_Test_Env_Var", "get_environment_variable" ) );
		std::string val;
		get_environment_variable( "ObjexxFCL_Test_Env_Var", val );
		EXPECT_EQ( "get_environment_variable", val );
	}
}

TEST( EnvironmentTest, Getenv )
{
	EXPECT_TRUE( SETENV( "ObjexxFCL_Test_Env_Var", "GETENV" ) );
	std::string val;
	GETENV( "ObjexxFCL_Test_Env_Var", val );
	EXPECT_EQ( "GETENV", val );
}

TEST( EnvironmentTest, Getenvqq )
{
	EXPECT_TRUE( SETENV( "ObjexxFCL_Test_Env_Var", "GETENVQQ" ) );
	std::string val;
	EXPECT_EQ( 8u, GETENVQQ( "ObjexxFCL_Test_Env_Var", val ) );
	EXPECT_EQ( "GETENVQQ", val );
}

TEST( EnvironmentTest, GetEnvVar )
{
	EXPECT_TRUE( SETENV( "ObjexxFCL_Test_Env_Var", "GETENVQQ" ) );
	EXPECT_EQ( "GETENVQQ", GET_ENV_VAR( "ObjexxFCL_Test_Env_Var" ) );
}

TEST( EnvironmentTest, Setenv )
{
	EXPECT_TRUE( SETENV( "ObjexxFCL_Test_Env_Var", "SETENV" ) );
	EXPECT_EQ( "SETENV", GET_ENV_VAR( "ObjexxFCL_Test_Env_Var" ) );
}

TEST( EnvironmentTest, Setenvqq )
{
	EXPECT_TRUE( SETENVQQ( "ObjexxFCL_Test_Env_Var=SETENVQQ" ) );
	EXPECT_EQ( "SETENVQQ", GET_ENV_VAR( "ObjexxFCL_Test_Env_Var" ) );
}
