// Character Functions
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
#include <ObjexxFCL/char.functions.hh>

// C++ Headers
#include <cstddef>
#include <cstring>

namespace ObjexxFCL {

// Character is in a cstring?
bool
is_any_of( char const c, c_cstring const s )
{
	for ( std::size_t i = 0, e = std::strlen( s ); i < e; ++i ) {
		if ( c == s[ i ] ) return true;
	}
	return false; // No matches
}

// Character is not in a cstring?
bool
not_any_of( char const c, c_cstring const s )
{
	for ( std::size_t i = 0, e = std::strlen( s ); i < e; ++i ) {
		if ( c == s[ i ] ) return false;
	}
	return true; // No matches
}

} // ObjexxFCL
