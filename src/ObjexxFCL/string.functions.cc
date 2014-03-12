// String Functions
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
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/string.constants.hh>

// C++ Headers
#include <algorithm>
#include <cctype>
#include <cstring>

namespace ObjexxFCL {

// Predicate

// char == char Case-Insensitively (non-inline for use by equali below)?
bool
char_equali( char const c, char const d )
{
	return ( std::tolower( c ) == std::tolower( d ) );
}

// string == string Case-Insensitively?
bool
equali( std::string const & s, std::string const & t )
{
	if ( s.length() != t.length() ) {
		return false;
	} else {
		return std::equal( s.begin(), s.end(), t.begin(), char_equali );
	}
}

// string == cstring Case-Insensitively?
bool
equali( std::string const & s, c_cstring const t )
{
	if ( s.length() != std::strlen( t ) ) {
		return false;
	} else {
		return std::equal( s.begin(), s.end(), t, char_equali );
	}
}

// cstring == string Case-Insensitively?
bool
equali( c_cstring const s, std::string const & t )
{
	if ( std::strlen( s ) != t.length() ) {
		return false;
	} else {
		return std::equal( t.begin(), t.end(), s, char_equali );
	}
}

// Has a Prefix Case-Optionally?
bool
has_prefix( std::string const & s, std::string const & pre, bool const exact_case )
{
	std::string::size_type const pre_len( pre.length() );
	if ( pre_len == 0 ) {
		return false;
	} else if ( s.length() < pre_len ) {
		return false;
	} else if ( exact_case ) {
		return ( s.find( pre ) == 0 );
	} else {
		return ( lowercased( s ).find( lowercased( pre ) ) == 0 );
	}
}

// Has a Suffix Case-Optionally?
bool
has_suffix( std::string const & s, std::string const & suf, bool const exact_case )
{
	std::string::size_type const suf_len( suf.length() );
	if ( suf_len == 0 ) {
		return false;
	} else {
		std::string::size_type const s_len( s.length() );
		if ( s_len < suf_len ) {
			return false;
		} else if ( exact_case ) {
			return ( s.rfind( suf ) == s_len - suf_len );
		} else {
			return ( lowercased( s ).rfind( lowercased( suf ) ) == s_len - suf_len );
		}
	}
}

// Modifier

// Lowercase a string
std::string &
lowercase( std::string & s )
{
	std::string::size_type const s_len( s.length() );
	for ( std::string::size_type i = 0; i < s_len; ++i ) {
		s[ i ] = std::tolower( s[ i ] );
	}
	return s;
}

// Uppercase a string
std::string &
uppercase( std::string & s )
{
	std::string::size_type const s_len( s.length() );
	for ( std::string::size_type i = 0; i < s_len; ++i ) {
		s[ i ] = std::toupper( s[ i ] );
	}
	return s;
}

// Left Justify a string
std::string &
left_justify( std::string & s )
{
	std::string::size_type const off( s.find_first_not_of( SPC ) );
	if ( ( off > 0 ) && ( off != std::string::npos ) ) {
		s.erase( 0, off ).append( off, SPC );
	}
	return s;
}

// Right Justify a string
std::string &
right_justify( std::string & s )
{
	std::string::size_type const s_len_trim( len_trim( s ) );
	std::string::size_type const off( s.length() - s_len_trim );
	if ( off > 0 ) {
		s.erase( s_len_trim ).insert( 0, off, SPC );
	}
	return s;
}

// Trim Trailing Space from a string
std::string &
trim( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( SPC ) );
		if ( ie == std::string::npos ) { // Blank string: return empty string
			s.clear();
		} else if ( ie + 1 < s.length() ) { // Trim tail
			s.erase( ie + 1 );
		}
	}
	return s;
}

// Trim Trailing Whitespace from a string
std::string &
trim_whitespace( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( WHITESPACE ) );
		if ( ie == std::string::npos ) { // Blank string: return empty string
			s.clear();
		} else if ( ie + 1 < s.length() ) { // Trim tail
			s.erase( ie + 1 );
		}
	}
	return s;
}

// Strip Specified Characters from a string's Tails
std::string &
strip( std::string & s, std::string const & chars )
{
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( chars ) );
		std::string::size_type const ie( s.find_last_not_of( chars ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is from chars
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
			if ( ib > 0 ) s.erase( 0, ib );
		}
	}
	return s;
}

// Strip Specified Characters from a string's Left Tail
std::string &
lstrip( std::string & s, std::string const & chars )
{
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( chars ) );
		if ( ib == std::string::npos ) { // All of string is from chars
			s.clear();
		} else if ( ib > 0 ) {
			s.erase( 0, ib );
		}
	}
	return s;
}

// Strip Specified Characters from a string's Right Tail
std::string &
rstrip( std::string & s, std::string const & chars )
{
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( chars ) );
		if ( ie == std::string::npos ) { // All of string is from chars
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
		}
	}
	return s;
}

// Strip Space from a string's Tails
std::string &
strip( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( SPC ) );
		std::string::size_type const ie( s.find_last_not_of( SPC ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is SPC
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
			if ( ib > 0 ) s.erase( 0, ib );
		}
	}
	return s;
}

// Strip Space from a string's Left Tail
std::string &
lstrip( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( SPC ) );
		if ( ib == std::string::npos ) { // All of string is SPC
			s.clear();
		} else if ( ib > 0 ) {
			s.erase( 0, ib );
		}
	}
	return s;
}

// Strip Space from a string's Right Tail
std::string &
rstrip( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( SPC ) );
		if ( ie == std::string::npos ) { // All of string is SPC
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
		}
	}
	return s;
}

// Strip Whitespace from a string's Tails
std::string &
strip_whitespace( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( WHITESPACE ) );
		std::string::size_type const ie( s.find_last_not_of( WHITESPACE ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is from WHITESPACE
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
			if ( ib > 0 ) s.erase( 0, ib );
		}
	}
	return s;
}

// Strip Whitespace from a string's Left Tail
std::string &
lstrip_whitespace( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( WHITESPACE ) );
		if ( ib == std::string::npos ) { // All of string is from WHITESPACE
			s.clear();
		} else if ( ib > 0 ) {
			s.erase( 0, ib );
		}
	}
	return s;
}

// Strip Whitespace from a string's Right Tail
std::string &
rstrip_whitespace( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( WHITESPACE ) );
		if ( ie == std::string::npos ) { // All of string is from WHITESPACE
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
		}
	}
	return s;
}

// Pad a string to a Specified Length
std::string &
pad( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Pad
		s.append( len - s_len, SPC );
	}
	return s;
}

// Left-Pad a string to a Specified Length
std::string &
lpad( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Left-pad
		s.insert( static_cast< std::string::size_type >( 0 ), len - s_len, SPC );
	}
	return s;
}

// Right-Pad a string to a Specified Length
std::string &
rpad( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Pad
		s.append( len - s_len, SPC );
	}
	return s;
}

// Size a string to a Specified Length
std::string &
size( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Pad
		s.append( len - s_len, SPC );
	} else if ( s_len > len ) { // Truncate
		s.erase( len );
	}
	return s;
}

// Center a string wrt its Whitespace
std::string &
center( std::string & s )
{
	std::string::size_type const s_len( s.length() );
	s = centered( strip_whitespace( s ), s_len );
	return s;
}

// Center a string with a Specified Length
std::string &
center( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Pad
		std::string::size_type const off( ( len - s_len ) / 2 );
		s = std::string( off, SPC ).append( s ).append( std::string( len - s_len - off, SPC ) );
	} else if ( s_len > len ) { // Truncate
		s.erase( len );
	}
	return s;
}

// Remove Repeat Characters from a Possibly Unsorted string Preserving Order
std::string &
unique( std::string & s )
{
	std::string u;
	std::string::size_type const s_len( s.length() );
	for ( std::string::size_type i = 0; i < s_len; ++i ) {
		if ( u.find( s[ i ] ) == std::string::npos ) {
			u.push_back( s[ i ] );
		}
	}
	s.swap( u );
	return s;
}

// Substring Replaced in a string
std::string &
replace( std::string & s, std::string const & a, std::string const & b )
{
	std::string::size_type const la( a.length() ), lb( b.length() );
	std::string::size_type pos( 0 );
	while ( ( pos = s.find( a, pos ) ) != std::string::npos ) {
		s.replace( pos, la, b );
		pos += lb;
	}
	return s;
}

// Overlay a string with Another string, Expanding Size as Needed
std::string &
overlay( std::string & s, std::string const & t, std::string::size_type const pos )
{
	std::string::size_type const t_len( t.length() );
	std::string::size_type const l_len( pos + t_len ); // Lower bound on new string length
	if ( l_len > s.length() ) s.resize( l_len, SPC ); // Expand
	s.replace( pos, t_len, t ); // Overlay the string
	return s;
}

// Generator

// Lowercased Copy of a string
std::string
lowercased( std::string const & s )
{
	std::string t( s );
	std::string::size_type const t_len( t.length() );
	for ( std::string::size_type i = 0; i < t_len; ++i ) {
		t[ i ] = std::tolower( t[ i ] );
	}
	return t;
}

// Uppercased Copy of a string
std::string
uppercased( std::string const & s )
{
	std::string t( s );
	std::string::size_type const t_len( t.length() );
	for ( std::string::size_type i = 0; i < t_len; ++i ) {
		t[ i ] = std::toupper( t[ i ] );
	}
	return t;
}

// Left-Justified Copy of a string
std::string
left_justified( std::string const & s )
{
	std::string::size_type const off( s.find_first_not_of( SPC ) );
	if ( ( off > 0 ) && ( off != std::string::npos ) ) {
		return s.substr( off ).append( off, SPC );
	} else {
		return s;
	}
}

// Right-Justified Copy of a string
std::string
right_justified( std::string const & s )
{
	std::string::size_type const s_len_trim( len_trim( s ) );
	std::string::size_type const off( s.length() - s_len_trim );
	if ( off > 0 ) {
		return std::string( off, SPC ).append( s.substr( 0, s_len_trim ) );
	} else {
		return s;
	}
}

// Trailing Space Trimmed Copy of a string
std::string
trimmed( std::string const & s )
{
	if ( s.empty() ) { // Empty string
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( SPC ) );
		if ( ie == std::string::npos ) { // Blank string: return empty string
			return std::string();
		} else if ( ie < s.length() - 1 ) { // Trimmed
			return s.substr( 0, ie + 1 );
		} else { // Unchanged
			return s;
		}
	}
}

// Trailing Whitespace Trimmed Copy of a string
std::string
trimmed_whitespace( std::string const & s )
{
	if ( s.empty() ) { // Empty string
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( WHITESPACE ) );
		if ( ie == std::string::npos ) { // Blank string: return empty string
			return std::string();
		} else if ( ie < s.length() - 1 ) { // Trimmed
			return s.substr( 0, ie + 1 );
		} else { // Unchanged
			return s;
		}
	}
}

// Specified Characters Stripped from a string's Tails Copy of a string
std::string
stripped( std::string const & s, std::string const & chars )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( chars ) );
		std::string::size_type const ie( s.find_last_not_of( chars ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is from chars
			return std::string(); // Return empty string
		} else {
			return s.substr( ib, ie - ib + 1 );
		}
	}
}

// Specified Characters Stripped from a string's Left Tail Copy of a string
std::string
lstripped( std::string const & s, std::string const & chars )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( chars ) );
		if ( ib == std::string::npos ) { // All of string is from chars
			return std::string(); // Return empty string
		} else if ( ib > 0 ) {
			return s.substr( ib );
		} else {
			return s;
		}
	}
}

// Specified Characters Stripped from a string's Right Tail Copy of a string
std::string
rstripped( std::string const & s, std::string const & chars )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( chars ) );
		if ( ie == std::string::npos ) { // All of string is from chars
			return std::string(); // Return empty string
		} else {
			return s.substr( 0, ie + 1 );
		}
	}
}

// Space Stripped from a string's Tails Copy of a string
std::string
stripped( std::string const & s )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( SPC ) );
		std::string::size_type const ie( s.find_last_not_of( SPC ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is SPC
			return std::string(); // Return empty string
		} else {
			return s.substr( ib, ie - ib + 1 );
		}
	}
}

// Space Stripped from a string's Left Tail Copy of a string
std::string
lstripped( std::string const & s )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( SPC ) );
		if ( ib == std::string::npos ) { // All of string is SPC
			return std::string(); // Return empty string
		} else if ( ib > 0 ) {
			return s.substr( ib );
		} else {
			return s;
		}
	}
}

// Space Stripped from a string's Right Tail Copy of a string
std::string
rstripped( std::string const & s )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( SPC ) );
		if ( ie == std::string::npos ) { // All of string is SPC
			return std::string(); // Return empty string
		} else {
			return s.substr( 0, ie + 1 );
		}
	}
}

// Whitespace Stripped from a string's Tails Copy of a string
std::string
stripped_whitespace( std::string const & s )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( WHITESPACE ) );
		std::string::size_type const ie( s.find_last_not_of( WHITESPACE ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is from WHITESPACE
			return std::string(); // Return empty string
		} else {
			return s.substr( ib, ie - ib + 1 );
		}
	}
}

// Whitespace Stripped from a string's Left Tail Copy of a string
std::string
lstripped_whitespace( std::string const & s )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( WHITESPACE ) );
		if ( ib == std::string::npos ) { // All of string is from WHITESPACE
			return std::string(); // Return empty string
		} else if ( ib > 0 ) {
			return s.substr( ib );
		} else {
			return s;
		}
	}
}

// Whitespace Stripped from a string's Right Tail Copy of a string
std::string
rstripped_whitespace( std::string const & s )
{
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( WHITESPACE ) );
		if ( ie == std::string::npos ) { // All of string is from WHITESPACE
			return std::string(); // Return empty string
		} else {
			return s.substr( 0, ie + 1 );
		}
	}
}

// Padded to a Specified Length Copy of a string
std::string
padded( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Padded
		return s + std::string( len - s_len, SPC );
	} else { // Unchanged
		return s;
	}
}

// Left-Padded to a Specified Length Copy of a string
std::string
lpadded( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Left-padded
		return std::string( len - s_len, SPC ).append( s );
	} else { // Unchanged
		return s;
	}
}

// Right-Padded to a Specified Length Copy of a string
std::string
rpadded( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Padded
		return s + std::string( len - s_len, SPC );
	} else { // Unchanged
		return s;
	}
}

// Sized to a Specified Length Copy of a string
std::string
sized( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Padded
		return s + std::string( len - s_len, SPC );
	} else if ( s_len == len ) { // Unchanged
		return s;
	} else { // Truncated
		return s.substr( 0, len );
	}
}

// Centered wrt Whitespace Copy of a string
std::string
centered( std::string const & s )
{
	return centered( stripped_whitespace( s ), s.length() );
}

// Centered in a string of Specified Length Copy of a string
std::string
centered( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Padded
		std::string::size_type const off( ( len - s_len ) / 2 );
		return std::string( off, SPC ).append( s ).append( std::string( len - s_len - off, SPC ) );
	} else if ( s_len == len ) { // Unchanged
		return s;
	} else { // Truncated
		return s.substr( 0, len );
	}
}

// Removed Repeat Characters from a Possibly Unsorted string Preserving Order Copy of a string
std::string
uniqued( std::string const & s )
{
	std::string u;
	std::string::size_type const s_len( s.length() );
	for ( std::string::size_type i = 0; i < s_len; ++i ) {
		if ( u.find( s[ i ] ) == std::string::npos ) {
			u.push_back( s[ i ] );
		}
	}
	return u;
}

// Substring Replaced Copy of a string
std::string
replaced( std::string const & s, std::string const & a, std::string const & b )
{
	std::string r( s );
	replace( r, a, b );
	return r;
}

// Space-Free Head Copy of a string
std::string
head( std::string const & s )
{
	if ( s.empty() ) { // Empty string
		return s;
	} else {
		std::string::size_type const ie( s.find( SPC ) );
		if ( ie == std::string::npos ) { // Space-free string
			return s;
		} else {
			return s.substr( 0, ie );
		}
	}
}

// string is Readable as a Decimal Integer?
bool
is_decimal( std::string const & s, bool const allow_sign )
{
	static std::string const signed_digits( "+-0123456789" );
	static std::string const number_digits( "0123456789" );
	if ( s.empty() ) {
		return false;
	} else if ( allow_sign && ( s.length() > 1 ) ) {
		if ( signed_digits.find( s[ 0 ] ) == std::string::npos ) return false;
		return ! has_any_not_of( s.substr( 1 ), number_digits );
	} else {
		return ! has_any_not_of( s, number_digits );
	}
}

// string is Readable as a Binary Integer?
bool
is_binary( std::string const & s, bool const allow_sign )
{
	static std::string const signed_digits( "+-01" );
	static std::string const number_digits( "01" );
	if ( s.empty() ) {
		return false;
	} else if ( allow_sign && ( s.length() > 1 ) ) {
		if ( signed_digits.find( s[ 0 ] ) == std::string::npos ) return false;
		return ! has_any_not_of( s.substr( 1 ), number_digits );
	} else {
		return ! has_any_not_of( s, number_digits );
	}
}

// string is Readable as an Octal Integer?
bool
is_octal( std::string const & s, bool const allow_sign )
{
	static std::string const signed_digits( "+-01234567" );
	static std::string const number_digits( "01234567" );
	if ( s.empty() ) {
		return false;
	} else if ( allow_sign && ( s.length() > 1 ) ) {
		if ( signed_digits.find( s[ 0 ] ) == std::string::npos ) return false;
		return ! has_any_not_of( s.substr( 1 ), number_digits );
	} else {
		return ! has_any_not_of( s, number_digits );
	}
}

// string is Readable as a Hexidecimal Integer?
bool
is_hexidecimal( std::string const & s, bool const allow_sign )
{
	static std::string const signed_digits( "+-0123456789ABCDEFabcdef" );
	static std::string const number_digits( "0123456789ABCDEFabcdef" );
	if ( s.empty() ) {
		return false;
	} else if ( allow_sign && ( s.length() > 1 ) ) {
		if ( signed_digits.find( s[ 0 ] ) == std::string::npos ) return false;
		return ! has_any_not_of( s.substr( 1 ), number_digits );
	} else {
		return ! has_any_not_of( s, number_digits );
	}
}

} // ObjexxFCL
