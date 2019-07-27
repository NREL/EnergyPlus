// ObjexxFCL Unit Test Driver
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

// ObjexxFCL Headers
#ifndef _WIN32
#include <ObjexxFCL/command.hh>
#endif

// Google Test Headers
#include <gtest/gtest.h>

// Google Test main
int
main( int argc, char **argv )
{
#ifndef _WIN32
__argc = argc;
__argv = argv;
#endif
	::testing::InitGoogleTest( &argc, argv );
	return RUN_ALL_TESTS();
}
