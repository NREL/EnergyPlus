#ifndef ObjexxFCL_proxy_const_assert_hh_INCLUDED
#define ObjexxFCL_proxy_const_assert_hh_INCLUDED

// Proxy Const-Correctness Assertion Macro
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

// C++ Headers
#include <cassert>
#if !defined(NDEBUG) && defined(__linux__) && defined(__GNUC__) && OBJEXXFCL_PROXY_CONST_CHECKS == 2
#include <cstddef>
#include <cstdio>
#include <execinfo.h>
#endif

namespace ObjexxFCL {

#ifdef OBJEXXFCL_PROXY_CONST_CHECKS
#if !defined(NDEBUG) && defined(__linux__) && defined(__GNUC__) && OBJEXXFCL_PROXY_CONST_CHECKS == 2
void
proxy_const_backtrace( char const * file, int const line )
{
	fprintf( stderr, "ObjexxFCL proxy constness violation at %s line %d\n", file, line );
	fprintf( stderr, "Backtrace (addr2line can convert addresses to line numbers):\n" );
	int const depth( 1000 );
	void * array[depth];
	std::size_t const size( backtrace( array, depth ) );
	backtrace_symbols_fd( array, size, 2 ); // Send backtrace to stderr
}
#define proxy_const_assert(x) ( (x) ? ((void)0) : proxy_const_backtrace( __FILE__, __LINE__ ) )
#else
#define proxy_const_assert(x) assert(x)
#endif
#else
#define proxy_const_assert(x) ((void)0)
#endif

} // ObjexxFCL

#endif // ObjexxFCL_proxy_const_assert_hh_INCLUDED
