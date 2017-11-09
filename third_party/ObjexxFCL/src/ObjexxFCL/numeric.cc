// Numeric Support Functions
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
#include <ObjexxFCL/numeric.hh>

// C++ Headers
#include <cstring>

namespace ObjexxFCL {

int
SELECTED_INT_KIND( int const r )
{
	if ( r <= 2 ) {
		return 1;
	} else if ( r <= 4 ) {
		return 2;
	} else if ( r <= 9 ) {
		return 4;
	} else if ( r <= 18 ) {
		return 8;
	} else {
		return -1;
	}
}

int
SELECTED_REAL_KIND( int const p, int const r, int const radix )
{
	if ( radix != 2 ) return -5; // Unsupported radix

	// Intel Fortran treatment of p
	int i; // Kind for precision p
	if ( p < 0 ) {
		i = -1;
	} else if ( p <= 6 ) {
		i = 4;
	} else if ( p <= 15 ) {
		i = 8;
	} else if ( p <= 33 ) {
		i = 16;
	} else {
		i = -1;
	}

	// Intel Fortran treatment of r
	int j; // Kind for exponent r
	if ( r < 0 ) {
		j = -2;
	} else if ( r <= 37 ) {
		j = 4;
	} else if ( r <= 307 ) {
		j = 8;
	} else if ( r <= 4931 ) {
		j = 16;
	} else {
		j = -2;
	}

	if ( i == -1 ) {
		if ( j == -2 ) {
			return -3; // Neither is available
		} else {
			return i;
		}
	} else if ( j == -2 ) {
		return j;
	} else {
		return ( i >= j ? i : j );
	}
}

int
SELECTED_CHAR_KIND( std::string const & s )
{
	if ( s == "DEFAULT" ) {
		return 1;
	} else if ( s == "ASCII" ) {
		return 1;
	} else {
		return -1;
	}
}

std::size_t
SIZEOF( char const * x )
{
	return std::strlen( x );
}

} // ObjexxFCL
