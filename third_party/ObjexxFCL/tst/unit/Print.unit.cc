// ObjexxFCL::Print Unit Tests
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
#include <ObjexxFCL/Print.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <sstream>

using namespace ObjexxFCL;

class PrintTest : public testing::Test
{

public:

	std::stringstream buf;
	std::streambuf * sbuf;

	virtual void SetUp() // Capture std::cout in a buffer
	{
		sbuf = std::cout.rdbuf();
		std::cout.rdbuf( buf.rdbuf() );
	}

	virtual void TearDown() // Restore std::cout
	{
		std::cout.rdbuf( sbuf ); // Reset cout
	}

};

TEST_F( PrintTest, PrintCharToStream )
{
	char c( 'X' );
	Print( "*" ) << c;
	EXPECT_EQ( " X\n", buf.str() );
}

TEST_F( PrintTest, PrintStringToStream )
{
	std::string s( "ABC" );
	Print( "*" ) << s;
	EXPECT_EQ( " ABC\n", buf.str() );
}

TEST_F( PrintTest, PrintStringLiteralToStream )
{
	std::string s( "ABC" );
	Print( "(A3,\"A big fish\"$)" ) << s;
	EXPECT_EQ( "ABCA big fish", buf.str() );
}

TEST_F( PrintTest, PrintLDGeneral )
{
	Print( "*" ) << "Text" << "ual" << 123 << "Done";
	EXPECT_EQ( " Textual         123 Done\n", buf.str() );
}

TEST_F( PrintTest, PrintLDDefaultGeneral )
{
	Print() << "Text" << "ual" << 123 << "Done";
	EXPECT_EQ( " Textual         123 Done\n", buf.str() );
}

TEST_F( PrintTest, PrintLD_1 )
{
	Print( "*" ) << 0.1;
	EXPECT_EQ( "  0.10000", buf.str().substr( 0, 9 ) );
}

TEST_F( PrintTest, PrintLD_01 )
{
	Print( "*" ) << 0.01;
	EXPECT_EQ( "  1.000000000000000E-002\n", buf.str() );
}

TEST_F( PrintTest, PrintLD_0101 )
{
	Print( "*" ) << 0.0101;
	EXPECT_EQ( "  1.010000000000000E-002\n", buf.str() );
}

TEST_F( PrintTest, PrintLD_02 )
{
	Print( "*" ) << 0.02;
	EXPECT_EQ( "  2.000000000000000E-002\n", buf.str() );
}

TEST_F( PrintTest, PrintLD_04 )
{
	Print( "*" ) << 0.04;
	EXPECT_EQ( "  4.000000000000000E-002\n", buf.str() );
}

TEST_F( PrintTest, PrintLD_1E8 )
{
	Print( "*" ) << 1.0e8f;
	EXPECT_EQ( " 100000000.    \n", buf.str() );
}

TEST_F( PrintTest, PrintLD_1E9 )
{
	Print( "*" ) << 1.0e9f;
	EXPECT_EQ( "  1.0000000E+09\n", buf.str() );
}

TEST_F( PrintTest, PrintLD_1E16 )
{
	Print( "*" ) << 1.0e16;
	EXPECT_EQ( " 10000000000000000.     \n", buf.str() );
}

TEST_F( PrintTest, PrintLD_1E17 ) // Exact output is platform/compiler dependent
{
	Print( "*" ) << 1.0e17;
#ifdef __GNUC__
#ifdef _WIN32
	EXPECT_EQ( "  1.000000000000002E+017\n", buf.str() );
#elif defined __unix__
	EXPECT_EQ( "  1.000000000000000E+017\n", buf.str() );
#endif
#endif
}

TEST_F( PrintTest, PrintStringLiteralNestedToStream )
{
	Print( "(\"A \\\"big\\\" fish\"$)" ); // Also tests that Print does the output with no << arg
	EXPECT_EQ( "A \"big\" fish", buf.str() );
}

TEST_F( PrintTest, PrintStringLiteralNestedWithBackslashesToStream )
{
	Print( "(\"A \\\\\\\\big\\\\\\\\ fish\"$)" ); // Nested string so we have to quadruple the backslashes
	EXPECT_EQ( "A \\\\big\\\\ fish", buf.str() ); // Want \\big\\ so we double the backslashes here
}

TEST_F( PrintTest, PrintFloatToStream )
{
	float f( 1.125 );
	Print( "(F5.0$)" ) << f;
	EXPECT_EQ( "   1.", buf.str() );
}

TEST_F( PrintTest, PrintFloatDToStream )
{
	float f( 1125 );
	Print( "(F6.1$)" ) << f; // Precision specified
	EXPECT_EQ( "1125.0", buf.str() );
}

TEST_F( PrintTest, PrintFloatEToStream )
{
	float f( 1.125E3 );
	Print( "(F7.0$)" ) << f;
	EXPECT_EQ( "  1125.", buf.str() );
}

TEST_F( PrintTest, PrintIntSpaceString )
{
	Print( "(I5,1X,A$)" ) << 123 << "End";
	EXPECT_EQ( "  123 End", buf.str() );
}

TEST_F( PrintTest, PrintIntSpace2String )
{
	Print( "(I5,2X,A$)" ) << 123 << "End";
	EXPECT_EQ( "  123  End", buf.str() );
}

TEST_F( PrintTest, PrintRepeatedStringLiteral )
{
	Print( "(2(\"ABC\"))" );
	EXPECT_EQ( "ABCABC\n", buf.str() );
}

TEST_F( PrintTest, ZeroES126 )
{
	Print( "(ES12.6)" ) << 0.0;
	EXPECT_EQ( "0.000000E+00\n", buf.str() );
}

TEST_F( PrintTest, NegZeroES126 )
{
	Print( "(ES12.6)" ) << -0.0;
	EXPECT_EQ( "************\n", buf.str() );
}

TEST_F( PrintTest, Fmt )
{
	gio::Fmt const fmt( "(F7.0$)" );
	float f( 1.125E3 );
	Print( fmt ) << f;
	EXPECT_EQ( "  1125.", buf.str() );
}

TEST_F( PrintTest, Array1DOut )
{
	Array1D_int const A( 3, { 1, 2, 3 } );
	gio::Fmt const fmt( "(3I3)" );
	Print( fmt ) << A;
	EXPECT_EQ( "  1  2  3\n", buf.str() );
}

TEST_F( PrintTest, Array1SOut )
{
	Array1D_int const A( 3, { 1, 2, 3 } );
	Array1S_int S( A );
	gio::Fmt const fmt( "(3I3)" );
	Print( fmt ) << S;
	EXPECT_EQ( "  1  2  3\n", buf.str() );
}
