// Character Functions
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

// ObjexxFCL Headers
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/char.constants.hh>

// C++ Headers
#include <cctype>
#include <cstddef>
#include <cstring>

namespace ObjexxFCL {

// char == char Case-Optionally?
bool
equal( char const c, char const d, bool const exact_case )
{
	if ( exact_case ) {
		return ( c == d );
	} else {
		return ( std::tolower( c ) == std::tolower( d ) );
	}
}

// char == char Case-Insensitively
bool
equali( char const c, char const d )
{
	return ( std::tolower( c ) == std::tolower( d ) );
}

// Character is Blank?
bool
is_blank( char const c )
{
	return ( c == SPC );
}

// Character is Not Blank?
bool
not_blank( char const c )
{
	return ( c != SPC );
}

// Character is in a string?
bool
is_any_of( char const c, std::string const & s )
{
	return ( s.find( c ) != std::string::npos );
}

// Character is in a cstring?
bool
is_any_of( char const c, c_cstring const s )
{
	for ( std::size_t i = 0, e = std::strlen( s ); i < e; ++i ) {
		if ( c == s[ i ] ) return true;
	}
	return false; // No matches
}

// Character is not in a string?
bool
not_any_of( char const c, std::string const & s )
{
	return ( s.find( c ) == std::string::npos );
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

// Modifier

// Lowercase a Character
char &
lowercase( char & c )
{
	c = std::tolower( c );
	return c;
}

// Uppercase a Character
char &
uppercase( char & c )
{
	c = std::toupper( c );
	return c;
}

// Generator

// Lowercased Copy of a Character
char
lowercased( char const c )
{
	return std::tolower( c );
}

// Uppercased Copy of a Character
char
uppercased( char const c )
{
	return std::toupper( c );
}

} // ObjexxFCL
