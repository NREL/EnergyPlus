// String Functions
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
#include <ObjexxFCL/string.functions.hh>

namespace ObjexxFCL {

// Predicate /////

// Modifier /////

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

// Right Justify a string
std::string &
rjustify( std::string & s )
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

// Left-Size a string to a Specified Length
std::string &
lsize( std::string & s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Left pad
		s.insert( 0, len - s_len, ' ' );
	} else if ( s_len > len ) { // Left truncate
		s.erase( 0, s_len - len );
	}
	return s;
}

// Right-Size a string to a Specified Length
std::string &
rsize( std::string & s, std::string::size_type const len )
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
	s = centered( s, s.length() );
	return s;
}

// Center a string to a Specified Length
std::string &
center( std::string & s, std::string::size_type const len )
{
	s = centered( s, len );
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
replace( std::string & s, std::string_view const a, std::string_view const b )
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
overlay( std::string & s, std::string_view const t, std::string::size_type const pos )
{
	std::string::size_type const t_len( t.length() );
	std::string::size_type const l_len( pos + t_len ); // Lower bound on new string length
	if ( l_len > s.length() ) s.resize( l_len, ' ' ); // Expand
	s.replace( pos, t_len, t ); // Overlay the string
	return s;
}

// Generator /////

// Lowercased Copy of a string
std::string
lowercased( std::string_view const s )
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
uppercased( std::string_view const s )
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
ljustified( std::string_view const s )
{
	std::string::size_type const off( s.find_first_not_of( ' ' ) );
	if ( ( off > 0 ) && ( off != std::string::npos ) ) {
		return std::string{s.substr( off )}.append( off, ' ' );
	} else {
		return std::string{s};
	}
}

// Right-Justified Copy of a string
std::string
rjustified( std::string_view const s )
{
	std::string::size_type const s_len_trim( len_trim( s ) );
	std::string::size_type const off( s.length() - s_len_trim );
	if ( off > 0 ) {
		return std::string( off, ' ' ).append( s.substr( 0, s_len_trim ) );
	} else {
		return std::string{s};
	}
}

// Trailing Space Trimmed Copy of a string
std::string
trimmed( std::string_view const s )
{
	if ( s.empty() ) { // Empty string
		return std::string{};
	} else {
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ie == std::string::npos ) { // Blank string: return empty string
			return std::string();
		} else if ( ie < s.length() - 1 ) { // Trimmed
			return std::string{s.substr( 0, ie + 1 )};
		} else { // Unchanged
			return std::string{s};
		}
	}
}

// Trailing Whitespace Trimmed Copy of a string
std::string
trimmed_whitespace( std::string_view const s )
{
	static constexpr std::string_view WHITE( " \t\0", 3 );
	if ( s.empty() ) { // Empty string
		return std::string{};
	} else {
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
		if ( ie == std::string::npos ) { // Blank string: return empty string
			return std::string();
		} else if ( ie < s.length() - 1 ) { // Trimmed
			return std::string{s.substr( 0, ie + 1 )};
		} else { // Unchanged
			return std::string{s};
		}
	}
}

// Specified Characters Stripped from a string's Tails Copy of a string
std::string
stripped( std::string_view const s, std::string_view const chars )
{
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ib( s.find_first_not_of( chars ) );
		std::string::size_type const ie( s.find_last_not_of( chars ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is from chars
			return std::string(); // Return empty string
		} else {
			return std::string{s.substr( ib, ie - ib + 1 )};
		}
	}
}

// Specified Characters Stripped from a string's Left Tail Copy of a string
std::string
lstripped( std::string_view const s, std::string_view const chars )
{
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ib( s.find_first_not_of( chars ) );
		if ( ib == std::string::npos ) { // All of string is from chars
			return std::string(); // Return empty string
		} else if ( ib > 0 ) {
			return std::string{s.substr( ib )};
		} else {
			return std::string{s};
		}
	}
}

// Specified Characters Stripped from a string's Right Tail Copy of a string
std::string
rstripped( std::string_view const s, std::string_view const chars )
{
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ie( s.find_last_not_of( chars ) );
		if ( ie == std::string::npos ) { // All of string is from chars
			return std::string(); // Return empty string
		} else {
			return std::string{s.substr( 0, ie + 1 )};
		}
	}
}

// Space Stripped from a string's Tails Copy of a string
std::string
stripped( std::string_view const s )
{
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ib( s.find_first_not_of( ' ' ) );
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is ' '
			return std::string(); // Return empty string
		} else {
			return std::string{s.substr( ib, ie - ib + 1 )};
		}
	}
}

// Space Stripped from a string's Left Tail Copy of a string
std::string
lstripped( std::string_view const s )
{
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ib( s.find_first_not_of( ' ' ) );
		if ( ib == std::string::npos ) { // All of string is ' '
			return std::string(); // Return empty string
		} else if ( ib > 0 ) {
			return std::string{s.substr( ib )};
		} else {
			return std::string{s};
		}
	}
}

// Space Stripped from a string's Right Tail Copy of a string
std::string
rstripped( std::string_view const s )
{
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ie( s.find_last_not_of( ' ' ) );
		if ( ie == std::string::npos ) { // All of string is ' '
			return std::string(); // Return empty string
		} else {
			return std::string{s.substr( 0, ie + 1 )};
		}
	}
}

// Whitespace Stripped from a string's Tails Copy of a string
std::string
stripped_whitespace( std::string_view const s )
{
	static constexpr std::string_view WHITE( " \t\0", 3 );
	if ( s.empty() ) {
		return std::string{};
	} else {
		std::string::size_type const ib( s.find_first_not_of( WHITE ) );
		std::string::size_type const ie( s.find_last_not_of( WHITE ) );
		if ( ( ib == std::string::npos ) || ( ie == std::string::npos ) ) { // All of string is WHITE
			return std::string(); // Return empty string
		} else {
			return std::string{s.substr( ib, ie - ib + 1 )};
		}
	}
}

// Sized to a Specified Length Copy of a string
std::string
sized( std::string_view const s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Right-padded
		return std::string{s} + std::string( len - s_len, ' ' );
	} else if ( s_len == len ) { // Unchanged
		return std::string{s};
	} else { // Truncated
		return std::string{s.substr( 0, len )};
	}
}

// Left-Sized to a Specified Length Copy of a string
std::string
lsized( std::string_view const s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Left-padded
		return std::string( len - s_len, ' ' ) + std::string{s};
	} else if ( s_len == len ) { // Unchanged
		return std::string{s};
	} else { // Truncated
		return std::string{s.substr( s_len - len )};
	}
}

// Right-Sized to a Specified Length Copy of a string
std::string
rsized( std::string_view const s, std::string::size_type const len )
{
	std::string::size_type const s_len( s.length() );
	if ( s_len < len ) { // Right-padded
		return std::string{s} + std::string( len - s_len, ' ' );
	} else if ( s_len == len ) { // Unchanged
		return std::string{s};
	} else { // Truncated
		return std::string{s.substr( 0, len )};
	}
}

// Centered String to Specified Length
std::string
centered( std::string_view const s, std::string::size_type const len )
{
	std::string const t( stripped_whitespace( s ) );
	std::string::size_type const t_len( t.length() );
	if ( t_len < len ) { // Padded
		std::string::size_type const off( ( len - t_len ) / 2 );
		return std::string( off, ' ' ).append( t ).append( std::string( len - t_len - off, ' ' ) );
	} else if ( t_len == len ) { // Unchanged
		return t;
	} else { // Truncated
		std::string::size_type const off( ( t_len - len ) / 2 );
		return t.substr( off, len );
	}
}

// Removed Repeat Characters from a Possibly Unsorted string Preserving Order Copy of a string
std::string
uniqued( std::string_view const s )
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

// Overlayed string with Another string, Expanding Size as Needed
std::string
overlayed( std::string_view const s, std::string_view const t, std::string::size_type const pos )
{
	std::string::size_type const s_len( s.length() );
	std::string::size_type const t_len( t.length() );
	std::string::size_type const l_len( pos + t_len ); // Lower bound on new string length
	std::string o( s );
	if ( l_len > s_len ) o.resize( l_len, ' ' ); // Expand
	o.replace( pos, t_len, t ); // Overlay the string
	return o;
}

// Repeated Copies
std::string
repeated( std::string_view const s, int const n )
{
	if ( n <= 0 ) return std::string();
	std::string::size_type const l( s.length() );
	std::string o;
	o.reserve( n * l );
	for ( int i = 0; i < n; ++i ) {
		o += s;
	}
	return o;
}

// Repeated Copies
std::string
repeat( std::string_view const s, int const n )
{
	return repeated(s, n);
}

// Space-Free Head Copy of a string
std::string
head( std::string_view const s )
{
	if ( s.empty() ) { // Empty string
		return std::string{};
	} else {
		std::string::size_type const ie( s.find( ' ' ) );
		if ( ie == std::string::npos ) { // Space-free string
			return std::string{s};
		} else {
			return std::string{s.substr( 0, ie )};
		}
	}
}

} // ObjexxFCL
