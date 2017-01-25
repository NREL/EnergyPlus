// Numeric Support Functions
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

// ObjexxFCL Headers
#include <ObjexxFCL/numeric.hh>

namespace ObjexxFCL {

int
selected_int_kind( int const r )
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
selected_real_kind( int const p, int const r ) // Fortran 2008 variant with radix argument not supported
{
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

	if ( ( i == -1 ) && ( j == -2 ) ) {
		return -3; // Neither is available
	} else {
		return ( i <= j ? i : j );
	} // Other cases not supported
}

int
selected_char_kind( std::string const & s )
{
	if ( s == "DEFAULT" ) {
		return 1;
	} else if ( s == "ASCII" ) {
		return 1;
	} else {
		return -1;
	}
}

} // ObjexxFCL
