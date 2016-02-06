// Cstring: C String Wrapper
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
#include <ObjexxFCL/Cstring.hh>
#include <ObjexxFCL/char.functions.hh>

// C++ Headers
#include <cctype>
#include <istream>
#include <ostream>

namespace ObjexxFCL {

	// Has any Character of a Cstring?
	bool
	Cstring::has_any_of( Cstring const & s ) const
	{
		size_type const s_len( s.length() );
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i ] == s.str_[ j ] ) return true;
			}
		}
		return false; // No matches
	}

	// Has any Character of a cstring?
	bool
	Cstring::has_any_of( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i ] == s[ j ] ) return true;
			}
		}
		return false; // No matches
	}

	// Has any Character of a std::string?
	bool
	Cstring::has_any_of( std::string const & s ) const
	{
		size_type const s_len( s.length() );
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i ] == s[ j ] ) return true;
			}
		}
		return false; // No matches
	}

	// Has a Character?
	bool
	Cstring::has_any_of( char const c ) const
	{
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			if ( str_[ i ] == c ) return true;
		}
		return false; // No matches
	}

	// Has a Character?
	bool
	Cstring::has( char const c ) const
	{
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			if ( str_[ i ] == c ) return true;
		}
		return false; // No matches
	}

	// Length Space-Trimmed
	Cstring::size_type
	Cstring::len_trim() const
	{
		for ( size_type i = std::strlen( str_ ); i > 0; --i ) {
			if ( str_[ i - 1 ] != ' ' ) return i;
		}
		return 0;
	}

	// Length Whitespace-Trimmed
	Cstring::size_type
	Cstring::len_trim_whitespace() const
	{
		for ( size_type i = std::strlen( str_ ); i > 0; --i ) {
			char const c( str_[ i - 1 ] );
			if ( ( c != ' ' ) && ( c != '\t' ) && ( c != '\0' ) ) return i;
		}
		return 0;
	}

	// Find First Occurrence of a Character
	Cstring::size_type
	Cstring::find( char const c ) const
	{
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			if ( str_[ i ] == c ) return i;
		}
		return npos; // No matches
	}

	// Find Last Occurrence of a Character
	Cstring::size_type
	Cstring::find_last( char const c ) const
	{
		for ( size_type i = std::strlen( str_ ); i > 0; --i ) {
			if ( str_[ i - 1 ] == c ) return i;
		}
		return npos; // No matches
	}

	// Lowercase
	Cstring &
	Cstring::lowercase()
	{
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			str_[ i ] = to_lower( str_[ i ] );
		}
		return *this;
	}

	// Uppercase
	Cstring &
	Cstring::uppercase()
	{
		for ( size_type i = 0; i < std::strlen( str_ ); ++i ) {
			str_[ i ] = to_upper( str_[ i ] );
		}
		return *this;
	}

	// Left Justify
	Cstring &
	Cstring::left_justify()
	{
		size_type const len( std::strlen( str_ ) );
		for ( size_type i = 0; i < len; ++i ) {
			if ( str_[ i ] != ' ' ) {
				if ( i > 0 ) {
					std::memmove( str_, str_ + i, len - i );
					std::memset( str_ + len - i, ' ', i );
				}
				return *this;
			}
		}
		return *this;
	}

	// Right Justify
	Cstring &
	Cstring::right_justify()
	{
		size_type const len( std::strlen( str_ ) );
		for ( size_type i = len; i > 0; --i ) {
			if ( str_[ i - 1 ] != ' ' ) {
				if ( i < len ) {
					std::memmove( str_ + len - i, str_, i );
					std::memset( str_, ' ', len - i );
				}
				return *this;
			}
		}
		return *this;
	}

	// Center
	Cstring &
	Cstring::center()
	{
		left_justify();
		size_type const len_t( len_trim() );
		size_type const pad( ( length() - len_t ) / 2 );
		if ( pad > 0 ) {
			std::memmove( str_ + pad, str_, len_t );
			std::memset( str_, ' ', pad );
		}
		return *this;
	}

	// Compress Out Whitespace
	Cstring &
	Cstring::compress()
	{
		size_type const len( std::strlen( str_ ) );
		size_type j = 0;
		for ( size_type i = 0; i < len; ++i ) {
			char const c( str_[ i ] );
			if ( ( c != ' ' ) && ( c != '\t' ) && ( c != '\0' ) ) str_[ j++ ] = c;
		}
		if ( j < len ) std::memset( str_ + j, ' ', len - j );
		return *this;
	}

	// Cstring == Cstring Case-Insensitively?
	bool
	equali( Cstring const & s, Cstring const & t )
	{
		typedef  Cstring::size_type  size_type;
		size_type const s_len( s.length() );
		if ( s_len != t.length() ) {
			return false;
		} else {
			for ( size_type i = 0; i < s_len; ++i ) {
				if ( to_lower( s.str_[ i ] ) != to_lower( t.str_[ i ] ) ) return false;
			}
			return true;
		}
	}

	// Cstring == cstring Case-Insensitively?
	bool
	equali( Cstring const & s, c_cstring const t )
	{
		typedef  Cstring::size_type  size_type;
		size_type const s_len( s.length() );
		if ( s_len != std::strlen( t ) ) {
			return false;
		} else {
			for ( size_type i = 0; i < s_len; ++i ) {
				if ( to_lower( s.str_[ i ] ) != to_lower( t[ i ] ) ) return false;
			}
			return true;
		}
	}

	// cstring == Cstring Case-Insensitively?
	bool
	equali( c_cstring const s, Cstring const & t )
	{
		typedef  Cstring::size_type  size_type;
		size_type const s_len( std::strlen( s ) );
		if ( s_len != t.length() ) {
			return false;
		} else {
			for ( size_type i = 0; i < s_len; ++i ) {
				if ( to_lower( s[ i ] ) != to_lower( t.str_[ i ] ) ) return false;
			}
			return true;
		}
	}

	// Cstring == std::string Case-Insensitively?
	bool
	equali( Cstring const & s, std::string const & t )
	{
		typedef  Cstring::size_type  size_type;
		size_type const s_len( s.length() );
		if ( s_len != t.length() ) {
			return false;
		} else {
			for ( size_type i = 0; i < s_len; ++i ) {
				if ( to_lower( s.str_[ i ] ) != to_lower( t[ i ] ) ) return false;
			}
			return true;
		}
	}

	// std::string == Cstring Case-Insensitively?
	bool
	equali( std::string const & s, Cstring const & t )
	{
		typedef  Cstring::size_type  size_type;
		size_type const s_len( s.length() );
		if ( s_len != t.length() ) {
			return false;
		} else {
			for ( size_type i = 0; i < s_len; ++i ) {
				if ( to_lower( s[ i ] ) != to_lower( t.str_[ i ] ) ) return false;
			}
			return true;
		}
	}

	// Cstring == char Case-Insensitively?
	bool
	equali( Cstring const & s, char const c )
	{
		return ( ( s.length() == 1 ) && ( to_lower( s.str_[ 0 ] ) == to_lower( c ) ) );
	}

	// char == Cstring Case-Insensitively?
	bool
	equali( char const c, Cstring const & s )
	{
		return ( ( s.length() == 1 ) && ( to_lower( s.str_[ 0 ] ) == to_lower( c ) ) );
	}

	// Stream >> Cstring
	std::istream &
	operator >>( std::istream & stream, Cstring & s )
	{
		std::string buffer;
		stream >> buffer;
		s = buffer;
		return stream;
	}

	// Stream << Cstring
	std::ostream &
	operator <<( std::ostream & stream, Cstring const & s )
	{
		for ( Cstring::size_type i = 0; i < std::strlen( s.str_ ); ++i ) {
			stream << s.str_[ i ];
		}
		return stream;
	}

	// Static Data Member Definitions
	Cstring::size_type const Cstring::npos = static_cast< size_type >( -1 );

} // ObjexxFCL
