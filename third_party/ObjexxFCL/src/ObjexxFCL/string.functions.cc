// String Functions
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
#include <ObjexxFCL/string.functions.hh>

namespace ObjexxFCL {

// Predicate

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
		for ( std::string::size_type i = 0; i < pre_len; ++i ) {
			if ( s[ i ] != pre[ i ] ) return false;
		}
		return true;
	} else {
		for ( std::string::size_type i = 0; i < pre_len; ++i ) {
			if ( ! equali( s[ i ], pre[ i ] ) ) return false;
		}
		return true;
	}
}

// Has a Prefix Case-Optionally?
bool
has_prefix( std::string const & s, c_cstring const pre, bool const exact_case )
{
	std::string::size_type const pre_len( std::strlen( pre ) );
	if ( pre_len == 0 ) {
		return false;
	} else if ( s.length() < pre_len ) {
		return false;
	} else if ( exact_case ) {
		for ( std::string::size_type i = 0; i < pre_len; ++i ) {
			if ( s[ i ] != pre[ i ] ) return false;
		}
		return true;
	} else {
		for ( std::string::size_type i = 0; i < pre_len; ++i ) {
			if ( ! equali( s[ i ], pre[ i ] ) ) return false;
		}
		return true;
	}
}

// Has a Prefix Case-Optionally?
bool
has_prefix( std::string const & s, char const pre, bool const exact_case )
{
	if ( s.length() == 0 ) {
		return false;
	} else if ( exact_case ) {
		return ( s[ 0 ] == pre );
	} else {
		return equali( s[ 0 ], pre );
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
		} else {
			std::string::size_type const del_len( s_len - suf_len );
			if ( exact_case ) {
				for ( std::string::size_type i = 0; i < suf_len; ++i ) {
					if ( s[ del_len + i ] != suf[ i ] ) return false;
				}
			} else {
				for ( std::string::size_type i = 0; i < suf_len; ++i ) {
					if ( ! equali( s[ del_len + i ], suf[ i ] ) ) return false;
				}
			}
			return true;
		}
	}
}

// Has a Suffix Case-Optionally?
bool
has_suffix( std::string const & s, c_cstring const suf, bool const exact_case )
{
	std::string::size_type const suf_len( std::strlen( suf ) );
	if ( suf_len == 0 ) {
		return false;
	} else {
		std::string::size_type const s_len( s.length() );
		if ( s_len < suf_len ) {
			return false;
		} else {
			std::string::size_type const del_len( s_len - suf_len );
			if ( exact_case ) {
				for ( std::string::size_type i = 0; i < suf_len; ++i ) {
					if ( s[ del_len + i ] != suf[ i ] ) return false;
				}
			} else {
				for ( std::string::size_type i = 0; i < suf_len; ++i ) {
					if ( ! equali( s[ del_len + i ], suf[ i ] ) ) return false;
				}
			}
			return true;
		}
	}
}

// Has a Suffix Case-Optionally?
bool
has_suffix( std::string const & s, char const pre, bool const exact_case )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len == 0 ) {
		return false;
	} else if ( exact_case ) {
		return ( s[ s_len - 1 ] == pre );
	} else {
		return equali( s[ s_len - 1 ], pre );
	}
}

// Modifier

// Lowercase a string
std::string &
lowercase( std::string & s )
{
	std::string::size_type const s_len( s.length() );
	for ( std::string::size_type i = 0; i < s_len; ++i ) {
		lowercase( s[ i ] );
	}
	return s;
}

// Uppercase a string
std::string &
uppercase( std::string & s )
{
	std::string::size_type const s_len( s.length() );
	for ( std::string::size_type i = 0; i < s_len; ++i ) {
		uppercase( s[ i ] );
	}
	return s;
}

// Left Justify a string
std::string &
left_justify( std::string & s )
{
	std::string::size_type const off( s.find_first_not_of( ' ' ) );
	if ( ( off > 0 ) && ( off != std::string::npos ) ) {
		s.erase( 0, off ).append( off, ' ' );
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
		s.erase( s_len_trim ).insert( 0, off, ' ' );
	}
	return s;
}

// Trim Trailing Space from a string
std::string &
trim( std::string & s )
{
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
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
	static std::string const WHITE( " \t\0", 3 );
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
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
		std::string::size_type const ib( s.find_first_not_of( ' ' ) );
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is ' '
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
		std::string::size_type const ib( s.find_first_not_of( ' ' ) );
		if ( ib == std::string::npos ) { // All of string is ' '
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
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ie == std::string::npos ) { // All of string is ' '
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
	static std::string const WHITE( " \t\0", 3 );
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( WHITE ) );
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is WHITE
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
	static std::string const WHITE( " \t\0", 3 );
	if ( ! s.empty() ) {
		std::string::size_type const ib( s.find_first_not_of( WHITE ) );
		if ( ib == std::string::npos ) { // All of string is WHITE
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
	static std::string const WHITE( " \t\0", 3 );
	if ( ! s.empty() ) {
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
		if ( ie == std::string::npos ) { // All of string is WHITE
			s.clear();
		} else {
			if ( ie < s.length() - 1 ) s.erase( ie + 1 );
		}
	}
	return s;
}

// Size a string to a Specified Length
std::string &
size( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Pad
		s.append( len - s_len, ' ' );
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
		s = std::string( off, ' ' ).append( s ).append( std::string( len - s_len - off, ' ' ) );
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
	std::string::size_type pos( 0u );
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
	if ( l_len > s.length() ) s.resize( l_len, ' ' ); // Expand
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
		lowercase( t[ i ] );
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
		uppercase( t[ i ] );
	}
	return t;
}

// Left-Justified Copy of a string
std::string
left_justified( std::string const & s )
{
	std::string::size_type const off( s.find_first_not_of( ' ' ) );
	if ( ( off > 0 ) && ( off != std::string::npos ) ) {
		return s.substr( off ).append( off, ' ' );
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
		return std::string( off, ' ' ).append( s.substr( 0, s_len_trim ) );
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
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
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
	static std::string const WHITE( " \t\0", 3 );
	if ( s.empty() ) { // Empty string
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
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
		std::string::size_type const ib( s.find_first_not_of( ' ' ) );
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is ' '
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
		std::string::size_type const ib( s.find_first_not_of( ' ' ) );
		if ( ib == std::string::npos ) { // All of string is ' '
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
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ie == std::string::npos ) { // All of string is ' '
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
	static std::string const WHITE( " \t\0", 3 );
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( WHITE ) );
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is WHITE
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
	static std::string const WHITE( " \t\0", 3 );
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ib( s.find_first_not_of( WHITE ) );
		if ( ib == std::string::npos ) { // All of string is WHITE
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
	static std::string const WHITE( " \t\0", 3 );
	if ( s.empty() ) {
		return s;
	} else {
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
		if ( ie == std::string::npos ) { // All of string is WHITE
			return std::string(); // Return empty string
		} else {
			return s.substr( 0, ie + 1 );
		}
	}
}

// Sized to a Specified Length Copy of a string
std::string
sized( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Right-padded
		return s + std::string( len - s_len, ' ' );
	} else if ( s_len == len ) { // Unchanged
		return s;
	} else { // Truncated
		return s.substr( 0, len );
	}
}

// Left-Sized to a Specified Length Copy of a string
std::string
lsized( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Left-padded
		return std::string( len - s_len, ' ' ) + s;
	} else if ( s_len == len ) { // Unchanged
		return s;
	} else { // Truncated
		return s.substr( 0, len );
	}
}

// Centered in a string of Specified Length Copy of a string
std::string
centered( std::string const & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Padded
		std::string::size_type const off( ( len - s_len ) / 2 );
		return std::string( off, ' ' ).append( s ).append( std::string( len - s_len - off, ' ' ) );
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

// Space-Free Head Copy of a string
std::string
head( std::string const & s )
{
	if ( s.empty() ) { // Empty string
		return s;
	} else {
		std::string::size_type const ie( s.find( ' ' ) );
		if ( ie == std::string::npos ) { // Space-free string
			return s;
		} else {
			return s.substr( 0, ie );
		}
	}
}

} // ObjexxFCL
