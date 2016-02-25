// ObjexxFCL::fmt Unit Tests
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
#include <ObjexxFCL/fmt.hh>
#include "ObjexxFCL.unit.hh"

// C++ Headers
#include <sstream>

using namespace ObjexxFCL;
using namespace ObjexxFCL::fmt;

TEST( fmtTest, A )
{
	std::ostringstream stream;
	stream << A("fish",9);
	EXPECT_EQ( "     fish", stream.str() );
}

TEST( fmtTest, L )
{
	{
		std::ostringstream stream;
		stream << L(false,7);
		EXPECT_EQ( "      F", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << L(false);
		EXPECT_EQ( "F", stream.str() );
	}
}

TEST( fmtTest, I )
{
	{
		std::ostringstream stream;
		stream << I(123,6);
		EXPECT_EQ( "   123", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << I(123,6,4);
		EXPECT_EQ( "  0123", stream.str() );
	}
}

TEST( fmtTest, E )
{
	{
		std::ostringstream stream;
		stream << E(0.123456789123,20,9);
		EXPECT_EQ( "     0.123456789E+00", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << E(-0.123456789123,20,9);
		EXPECT_EQ( "    -0.123456789E+00", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << E(0.0,12,6);
		EXPECT_EQ( "0.000000E+00", stream.str() );
	}
}

TEST( fmtTest, ES )
{
	{
		std::ostringstream stream;
		stream << ES(0.123456789123,20,9);
		EXPECT_EQ( "     1.234567891E-01", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << ES(-0.123456789123,20,9);
		EXPECT_EQ( "    -1.234567891E-01", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << ES(0.0,12,6);
		EXPECT_EQ( "0.000000E+00", stream.str() );
	}
}

TEST( fmtTest, EN )
{
	{
		std::ostringstream stream;
		stream << EN(0.123456789123,20,9);
		EXPECT_EQ( "   123.456789123E-03", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << EN(-0.123456789123,20,9);
		EXPECT_EQ( "  -123.456789123E-03", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << EN(0.0,12,6);
		EXPECT_EQ( "0.000000E+00", stream.str() );
	}
}

TEST( fmtTest, F )
{
	{
		std::ostringstream stream;
		stream << F(0.123456789123,15,9);
		EXPECT_EQ( "    0.123456789", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << F(-0.123456789123,15,9);
		EXPECT_EQ( "   -0.123456789", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << F(0.123456789123,9,9);
		EXPECT_EQ( "*********", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << F(-0.123456789123,9,8);
		EXPECT_EQ( "*********", stream.str() );
	}
}

TEST( fmtTest, G )
{
	{
		std::ostringstream stream;
		stream << G(0.123456789123,15,9);
		EXPECT_EQ( "0.123456789    ", stream.str() );
	}

	{
		std::ostringstream stream;
		stream << G(-0.123456789123,15,9);
		EXPECT_EQ( "-.123456789    ", stream.str() );
	}
}
