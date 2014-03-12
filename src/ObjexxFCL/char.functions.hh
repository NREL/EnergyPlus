#ifndef ObjexxFCL_char_functions_hh_INCLUDED
#define ObjexxFCL_char_functions_hh_INCLUDED

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

// C++ Headers
#include <string>

namespace ObjexxFCL {

// Types
typedef  char const *  c_cstring;

// Predicate

// char == char Case-Optionally?
bool
equal( char const c, char const d, bool const exact_case = true );

// char == char Case-Insensitively
bool
equali( char const c, char const d );

// Character is Blank?
bool
is_blank( char const c );

// Character is Not Blank?
bool
not_blank( char const c );

// Character is in a string?
bool
is_any_of( char const c, std::string const & s );

// Character is in a cstring?
bool
is_any_of( char const c, c_cstring const s );

// Character is not in a string?
bool
not_any_of( char const c, std::string const & s );

// Character is not in a cstring?
bool
not_any_of( char const c, c_cstring const s );

// ASCII Lexical >= Comparison
inline
bool
lge( char const s, char const t )
{
	return ( s >= t );
}

// ASCII Lexical < Comparison
inline
bool
lgt( char const s, char const t )
{
	return ( s > t );
}

// ASCII Lexical <= Comparison
inline
bool
lle( char const s, char const t )
{
	return ( s <= t );
}

// ASCII Lexical < Comparison
inline
bool
llt( char const s, char const t )
{
	return ( s < t );
}

// Integer Conversion

// Integer Value of a Given One-Character Fstring
inline
int
ICHAR( char const s )
{
	return static_cast< int >( s );
}

// ASCII Integer Value for a Given One-Character Fstring
inline
int
IACHAR( char const s )
{
	return static_cast< int >( s );
}

// Modifier

// Lowercase a Character
char &
lowercase( char & c );

// Uppercase a Character
char &
uppercase( char & c );

// Generator

// Lowercased Copy of a Character
char
lowercased( char const c );

// Uppercased Copy of a Character
char
uppercased( char const c );

} // ObjexxFCL

#endif // ObjexxFCL_char_functions_hh_INCLUDED
