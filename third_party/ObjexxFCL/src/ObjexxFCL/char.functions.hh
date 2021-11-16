#ifndef ObjexxFCL_char_functions_hh_INCLUDED
#define ObjexxFCL_char_functions_hh_INCLUDED

// Character Functions
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

// C++ Headers
#include <cctype>
#include <string>

namespace ObjexxFCL {

// Predicate /////

// char is Blank?
constexpr
bool
is_blank( char const c )
{
	return ( c == ' ' );
}

// char is Not Blank?
constexpr
bool
not_blank( char const c )
{
	return ( c != ' ' );
}

// char is Alphabetic?
inline
bool
is_alpha( char const c )
{
	return ( std::isalpha( c ) != 0 );
}

// char is Digits?
inline
bool
is_digit( char const c )
{
	return ( std::isdigit( c ) != 0 );
}

// char is Lowercase Alphabetic?
inline
bool
is_lower( char const c )
{
	return ( std::islower( c ) != 0 );
}

// char is Uppercase Alphabetic?
inline
bool
is_upper( char const c )
{
	return ( std::isupper( c ) != 0 );
}

// char is in a string?
constexpr
bool
is_any_of( char const c, std::string_view const s )
{
	return ( s.find( c ) != std::string::npos );
}

// char is in a string?
constexpr
bool
has_any_of( char const c, std::string_view const s )
{
	return ( s.find( c ) != std::string::npos );
}

// char is not in a string?
constexpr
bool
not_any_of( char const c, std::string_view const s )
{
	return ( s.find( c ) == std::string::npos );
}


// Comparison /////

// Lowercased 8-bit ASCII char
constexpr
char
to_lower( char const c ); // Defined below

// Uppercased 8-bit ASCII char
constexpr
char
to_upper( char const c ); // Defined below

// char == char Case-Optionally?
constexpr
bool
equal( char const c, char const d, bool const exact_case = true )
{
	return ( exact_case ? c == d : to_lower( c ) == to_lower( d ) );
}

// char == char Case-Insensitively
constexpr
bool
equali( char const c, char const d )
{
	return ( to_lower( c ) == to_lower( d ) );
}

// char < char Case-Optionally?
constexpr
bool
lessthan( char const c, char const d, bool const exact_case = true )
{
	return ( exact_case ? c < d : to_lower( c ) < to_lower( d ) );
}

// char < char Case-Insensitively?
constexpr
bool
lessthani( char const c, char const d )
{
	return ( to_lower( c ) < to_lower( d ) );
}

// ASCII Lexical < Comparison
constexpr
bool
LLT( char const c, char const d )
{
	return ( c < d );
}

// ASCII Lexical <= Comparison
constexpr
bool
LLE( char const c, char const d )
{
	return ( c <= d );
}

// ASCII Lexical >= Comparison
constexpr
bool
LGE( char const c, char const d )
{
	return ( c >= d );
}

// ASCII Lexical < Comparison
constexpr
bool
llt( char const c, char const d )
{
	return ( c < d );
}

// ASCII Lexical <= Comparison
constexpr
bool
lle( char const c, char const d )
{
	return ( c <= d );
}

// ASCII Lexical >= Comparison
constexpr
bool
lge( char const c, char const d )
{
	return ( c >= d );
}

// ASCII Lexical < Comparison
constexpr
bool
lgt( char const c, char const d )
{
	return ( c > d );
}

// Conversion /////

// Integer Value of a char
constexpr
int
ICHAR( char const c )
{
	return static_cast< int >( c );
}

// Modifier /////

// Lowercase a char
constexpr
char &
lowercase( char & c )
{
	c = to_lower( c );
	return c;
}

// Uppercase a char
constexpr
char &
uppercase( char & c )
{
	c = to_upper( c );
	return c;
}

// Generator /////

// Lowercased Copy of a char
constexpr
char
lowercased( char const c )
{
	return to_lower( c );
}

// Uppercased Copy of a char
constexpr
char
uppercased( char const c )
{
	return to_upper( c );
}

// Lowercased 8-bit ASCII char
constexpr
char
to_lower( char const c )
{
	int const i( static_cast< int >( c ) );
	return ( ( ( 65 <= i ) && ( i <= 90 ) ) || ( ( 192 <= i ) && ( i <= 223 ) ) ? i + 32 : i );
}

// Uppercased 8-bit ASCII char
constexpr
char
to_upper( char const c )
{
	int const i( static_cast< int >( c ) );
	return ( ( ( 97 <= i ) && ( i <= 122 ) ) || ( ( 224 <= i ) && ( i <= 255 ) ) ? i - 32 : i );
}

} // ObjexxFCL

#endif // ObjexxFCL_char_functions_hh_INCLUDED
