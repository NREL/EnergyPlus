// ObjexxFCL::Array1S Unit Tests
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

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4244) // Suppress conversion warnings: Intentional narrowing assignments present
#endif

// Google Test Headers
#include <gtest/gtest.h>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1S.hh>
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/ArrayS.functions.hh>
#include "ObjexxFCL.unit.hh"

#ifdef _MSC_VER
#pragma warning(pop)
#endif

using namespace ObjexxFCL;

TEST( Array1STest, FunctionAbs )
{
	Array1D_int const A( { -1, -2, -3 } );
	Array1S_int a( A( {1,3} ) );
	Array1D_int const E( { 1, 2, 3 } );
	EXPECT_TRUE( eq( E, abs( a ) ) );
}

TEST( Array1STest, FunctionNegation )
{
	Array1D_bool const A( { true, false, true } );
	Array1S_bool a( A( {1,3} ) );
	Array1D_bool const E( { false, true, false } );
	EXPECT_TRUE( eq( E, !a ) );
}
