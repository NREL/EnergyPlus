#ifndef ObjexxFCL_char_functions_hh_INCLUDED
#define ObjexxFCL_char_functions_hh_INCLUDED

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

// C++ Headers
#include <string>

namespace ObjexxFCL {

// Types
typedef  char const *  c_cstring;

// Inspector

// Lowercased 8-bit ASCII char
inline
char
to_lower( char const c )
{
	int const i( static_cast< int >( c ) );
	return ( ( ( 65 <= i ) && ( i <= 90 ) ) || ( ( 192 <= i ) && ( i <= 223 ) ) ? i + 32 : i );
}

// Uppercased 8-bit ASCII char
inline
char
to_upper( char const c )
{
	int const i( static_cast< int >( c ) );
	return ( ( ( 97 <= i ) && ( i <= 122 ) ) || ( ( 224 <= i ) && ( i <= 255 ) ) ? i - 32 : i );
}

// Predicate

// char == char Case-Insensitively
inline
bool
equali( char const c, char const d )
{
	return ( to_lower( c ) == to_lower( d ) );
}

// char == char Case-Optionally?
inline
bool
equal( char const c, char const d, bool const exact_case = true )
{
	return ( exact_case ? c == d : to_lower( c ) == to_lower( d ) );
}

// Character is Blank?
inline
bool
is_blank( char const c )
{
	return ( c == ' ' );
}

// Character is Not Blank?
inline
bool
not_blank( char const c )
{
	return ( c != ' ' );
}

// Character is in a string?
inline
bool
is_any_of( char const c, std::string const & s )
{
	return ( s.find( c ) != std::string::npos );
}

// Character is in a cstring?
bool
is_any_of( char const c, c_cstring const s );

// Character is in a string?
inline
bool
has_any_of( char const c, std::string const & s )
{
	return ( s.find( c ) != std::string::npos );
}

// Character is in a cstring?
inline
bool
has_any_of( char const c, c_cstring const s )
{
	return is_any_of( c, s );
}

// Character is not in a string?
inline
bool
not_any_of( char const c, std::string const & s )
{
	return ( s.find( c ) == std::string::npos );
}

// Character is not in a cstring?
bool
not_any_of( char const c, c_cstring const s );

// ASCII Lexical >= Comparison
inline
bool
lge( char const c, char const d )
{
	return ( c >= d );
}

// ASCII Lexical < Comparison
inline
bool
lgt( char const c, char const d )
{
	return ( c > d );
}

// ASCII Lexical <= Comparison
inline
bool
lle( char const c, char const d )
{
	return ( c <= d );
}

// ASCII Lexical < Comparison
inline
bool
llt( char const c, char const d )
{
	return ( c < d );
}

// Integer Conversion

// Integer Value of a Character
inline
int
ICHAR( char const c )
{
	return static_cast< int >( c );
}

// ASCII Integer Value of a Character
inline
int
IACHAR( char const c )
{
	return static_cast< int >( c );
}

// Modifier

// Lowercase a Character
inline
char &
lowercase( char & c )
{
	c = to_lower( c );
	return c;
}

// Uppercase a Character
inline
char &
uppercase( char & c )
{
	c = to_upper( c );
	return c;
}

// Generator

// Lowercased Copy of a Character
inline
char
lowercased( char const c )
{
	return to_lower( c );
}

// Uppercased Copy of a Character
inline
char
uppercased( char const c )
{
	return to_upper( c );
}

} // ObjexxFCL

#endif // ObjexxFCL_char_functions_hh_INCLUDED
