// Fstring: Fixed-Length Fortran-Compatible String and Substring
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
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/char.constants.hh>
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/string.constants.hh>

// C++ Headers
#include <algorithm>
#include <iostream>

namespace ObjexxFCL {

	// Copy Constructor
	Fstring::Fstring( Fstring const & s ) :
		len_( s.len_ ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memcpy( str_, s.str_, len_ );
	}

	// Move Constructor
	Fstring::Fstring( Fstring && s ) :
		len_( s.len_ ),
		str_( nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		if ( s.sub_ ) { // Copy
			if ( len_ > 0u ) {
				str_ = new char[ len_ ];
				std::memcpy( str_, s.str_, len_ );
			}
		} else { // Move
			str_ = s.str_;
			c_str_ = s.c_str_;
			s.len_ = 0;
			s.str_ = s.c_str_ = nullptr;
		}
	}

	// Substring Constructor
	Fstring::Fstring( Fsubstring const & s ) :
		len_( s.len_ ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memcpy( str_, s.str_, len_ );
	}

	// string Constructor
	Fstring::Fstring( std::string const & s ) :
		len_( s.length() ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		s.copy( str_, len_ );
	}

	// cstring Constructor
	Fstring::Fstring( c_cstring const s ) :
		len_( std::strlen( s ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memcpy( str_, s, len_ );
	}

	// Length Constructor
	Fstring::Fstring( short int const len ) :
		len_( static_cast< size_type >( std::max( static_cast< int >( len ), 0 ) ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( int const len ) :
		len_( static_cast< size_type >( std::max( len, 0 ) ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( long int const len ) :
		len_( static_cast< size_type >( std::max( len, 0l ) ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( long long int const len ) :
		len_( static_cast< size_type >( std::max( len, 0ll ) ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( unsigned short int const len ) :
		len_( static_cast< size_type >( len ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( unsigned int const len ) :
		len_( static_cast< size_type >( len ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( unsigned long int const len ) :
		len_( static_cast< size_type >( len ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length Constructor
	Fstring::Fstring( unsigned long long int const len ) :
		len_( static_cast< size_type >( len ) ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
	}

	// Length + Fstring Constructor
	Fstring::Fstring( size_type const len, Fstring const & s ) :
		len_( len ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		if ( len_ > s.len_ ) {
			if ( s.len_ > 0u ) std::memcpy( str_, s.str_, s.len_ );
			std::memset( str_ + s.len_, SPC, len_ - s.len_ ); // Space pad
		} else if ( len_ > 0u ) {
			std::memcpy( str_, s.str_, len_ );
		}
	}

	// Length + string Constructor
	Fstring::Fstring( size_type const len, std::string const & s ) :
		len_( len ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		size_type const s_len( s.length() );
		if ( len_ > s_len ) {
			if ( s_len > 0u ) s.copy( str_, s_len );
			std::memset( str_ + s_len, SPC, len_ - s_len ); // Space pad
		} else if ( len_ > 0u ) {
			s.copy( str_, len_ );
		}
	}

	// Length + cstring Constructor
	Fstring::Fstring( size_type const len, c_cstring const s ) :
		len_( len ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		size_type const s_len( std::strlen( s ) );
		if ( len_ > s_len ) {
			if ( s_len > 0u ) std::memcpy( str_, s, s_len );
			std::memset( str_ + s_len, SPC, len_ - s_len ); // Space pad
		} else if ( len_ > 0u ) {
			std::memcpy( str_, s, len_ );
		}
	}

	// Length + char Constructor
	//  Fills with specified char => Use Fstring( len, "c" ) for space-padded single character
	Fstring::Fstring( size_type const len, char const c ) :
		len_( len ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, c, len_ );
	}

	// Length + Initializer Constructor
	Fstring::Fstring( size_type const len, InitializerFunction init ) :
		len_( len ),
		str_( len_ > 0u ? new char[ len_ ] : nullptr ),
		c_str_( nullptr ),
		sub_( false )
	{
		std::memset( str_, SPC, len_ );
		init( *this );
	}

	// Substring Range Constructor
	Fstring::Fstring( Fstring const & s, size_type const l, size_type const u ) :
		len_( l <= std::min( u, s.len_ ) ? std::min( u, s.len_ ) - l + 1u : static_cast< size_type >( 0 ) ),
		str_( l <= s.len_ ? s.str_ + l - 1u : s.str_ ),
		c_str_( nullptr ),
		sub_( true )
	{
		assert( l > 0u );
	}

	// Substring Tail Constructor
	Fstring::Fstring( Fstring const & s, size_type const l ) :
		len_( l <= s.len_ ? s.len_ - l + 1u : static_cast< size_type >( 0 ) ),
		str_( l <= s.len_ ? s.str_ + l - 1u : s.str_ ),
		c_str_( nullptr ),
		sub_( true )
	{
		assert( l > 0u );
	}

	// Copy Assignment
	Fstring &
	Fstring::operator =( Fstring const & s )
	{
		if ( this != &s ) {
			if ( len_ > s.len_ ) {
				if ( s.len_ > 0u ) std::memmove( str_, s.str_, s.len_ );
				std::memset( str_ + s.len_, SPC, len_ - s.len_ ); // Space pad
			} else if ( len_ > 0u ) {
				std::memmove( str_, s.str_, len_ );
			}
		}
		return *this;
	}

	// = string
	Fstring &
	Fstring::operator =( std::string const & s )
	{
		size_type const s_len( s.length() );
		if ( len_ > s_len ) {
			if ( s_len > 0u ) s.copy( str_, s_len );
			std::memset( str_ + s_len, SPC, len_ - s_len ); // Space pad
		} else if ( len_ > 0u ) {
			s.copy( str_, len_ );
		}
		return *this;
	}

	// = cstring
	Fstring &
	Fstring::operator =( c_cstring const s )
	{
		size_type const s_len( std::strlen( s ) );
		if ( len_ > s_len ) {
			if ( s_len > 0u ) std::memmove( str_, s, s_len );
			std::memset( str_ + s_len, SPC, len_ - s_len ); // Space pad
		} else if ( len_ > 0u ) {
			std::memmove( str_, s, len_ );
		}
		return *this;
	}

	// = char
	Fstring &
	Fstring::operator =( char const c )
	{
		if ( len_ > 0u ) {
			str_[ 0 ] = c;
			if ( len_ > 1u ) std::memset( str_ + 1, SPC, len_ - 1 ); // Space pad
		}
		return *this;
	}

	// Fstring Reassignment
	void
	Fstring::reassign( Fstring const & s )
	{
		if ( this != &s ) {
			if ( sub_ ) { // Substring
				operator =( s ); // Do normal assignment
			} else {
				if ( len_ != s.len_ ) {
					delete[] str_;
					len_ = s.len_;
					str_ = ( len_ > 0u ? new char[ len_ ] : nullptr );
				}
				std::memcpy( str_, s.str_, len_ );
			}
		}
	}

	// Has an Fstring?
	bool
	Fstring::has( Fstring const & s ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			if ( (*this)( i ) == s ) return true;
		}
		return false; // No matches
	}

	// Has a string?
	bool
	Fstring::has( std::string const & s ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			if ( (*this)( i ) == s ) return true;
		}
		return false; // No matches
	}

	// Has a cstring?
	bool
	Fstring::has( c_cstring const s ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			if ( (*this)( i ) == s ) return true;
		}
		return false; // No matches
	}

	// Has a Character?
	bool
	Fstring::has( char const c ) const
	{
		for ( size_type i = 0; i < len_; ++i ) {
			if ( str_[ i ] == c ) return true;
		}
		return false; // No matches
	}

	// Has any Character of an Fstring?
	bool
	Fstring::has_any_of( Fstring const & s ) const
	{
		for ( size_type i = 0; i < len_; ++i ) {
			for ( size_type j = 0, e = s.len_; j < e; ++j ) {
				if ( str_[ i ] == s.str_[ j ] ) return true;
			}
		}
		return false; // No matches
	}

	// Has any Character of a string?
	bool
	Fstring::has_any_of( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		for ( size_type i = 0; i < len_; ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i ] == s[ j ] ) return true;
			}
		}
		return false; // No matches
	}

	// Has any Character of a cstring?
	bool
	Fstring::has_any_of( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		for ( size_type i = 0; i < len_; ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i ] == s[ j ] ) return true;
			}
		}
		return false; // No matches
	}

	// Has a Character?
	bool
	Fstring::has_any_of( char const c ) const
	{
		for ( size_type i = 0; i < len_; ++i ) {
			if ( str_[ i ] == c ) return true;
		}
		return false; // No matches
	}

	// Has a Prefix Case-Optionally?
	bool
	Fstring::has_prefix( Fstring const & s, bool const exact_case ) const
	{
		if ( s.len_ == 0u ) {
			return false;
		} else if ( len_ < s.len_ ) {
			return false;
		} else if ( exact_case ) {
			return ( (*this)( 1, s.len_ ) == s );
		} else {
			return ( lowercased()( 1, s.len_ ) == s.lowercased() );
		}
	}

	// Has a Prefix Case-Optionally?
	bool
	Fstring::has_prefix( c_cstring const s, bool const exact_case ) const
	{
		size_type const s_len( std::strlen( s ) );
		if ( s_len == 0 ) {
			return false;
		} else if ( len_ < s_len ) {
			return false;
		} else if ( exact_case ) {
			return ( (*this)( 1, s_len ) == s );
		} else {
			return ( lowercased()( 1, s_len ) == Fstring( s ).lowercase() );
		}
	}

	// Length Space-Trimmed
	Fstring::size_type
	Fstring::len_trim() const
	{
#ifdef OBJEXXFCL_NO_UNROLL
		if ( len_ == 0u ) return 0;
		for ( size_type i = len_ - 1; i > 0u; --i ) {
			if ( str_[ i ] != SPC ) return i + 1;
		}
		return ( str_[ 0 ] != SPC ? 1 : 0 );
#else
		if ( len_ == 0u ) return 0;
		size_type i( len_ );
		while ( i >= 8 ) {
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( str_[ --i ] != SPC ) return i + 1;
		}
		if ( i > 0u ) {
			if ( str_[ --i ] != SPC ) return i + 1;
			if ( i > 0u ) {
				if ( str_[ --i ] != SPC ) return i + 1;
				if ( i > 0u ) {
					if ( str_[ --i ] != SPC ) return i + 1;
					if ( i > 0u ) {
						if ( str_[ --i ] != SPC ) return i + 1;
						if ( i > 0u ) {
							if ( str_[ --i ] != SPC ) return i + 1;
							if ( i > 0u ) {
								if ( str_[ --i ] != SPC ) return i + 1;
								if ( i > 0u ) {
									if ( str_[ --i ] != SPC ) return i + 1;
								}
							}
						}
					}
				}
			}
		}
		return 0;
#endif
	}

	// Length Whitespace-Trimmed
	Fstring::size_type
	Fstring::len_trim_whitespace() const
	{
		if ( len_ == 0u ) return 0;
		for ( size_type i = len_ - 1; i > 0u; --i ) {
			char const c( str_[ i ] );
			if ( ( c != SPC ) && ( c != TAB ) && ( c != NUL ) ) return i + 1;
		}
		char const c( str_[ 0 ] );
		return ( ( c != SPC ) && ( c != TAB ) && ( c != NUL ) ? 1 : 0 );
	}

	// Find First Occurrence of a Whitespace Character
	Fstring::size_type
	Fstring::find_whitespace() const
	{
		if ( len_ == 0u ) return 0;
		for ( size_type i = 0; i < len_; ++i ) {
			char const c( str_[ i ] );
			if ( ( c == SPC ) || ( c == TAB ) || ( c == NUL ) ) return i + 1;
		}
		return 0; // All are non-whitespace
	}

	// Find First Occurrence of a Non-Whitespace Character
	Fstring::size_type
	Fstring::find_non_whitespace() const
	{
		if ( len_ == 0u ) return 0;
		for ( size_type i = 0; i < len_; ++i ) {
			char const c( str_[ i ] );
			if ( ( c != SPC ) && ( c != TAB ) && ( c != NUL ) ) return i + 1;
		}
		return 0; // All are whitespace
	}

	// Find Last Occurrence of a Whitespace Character
	Fstring::size_type
	Fstring::find_last_whitespace() const
	{
		if ( len_ == 0u ) return 0;
		for ( size_type i = len_ - 1; i > 0u; --i ) {
			char const c( str_[ i ] );
			if ( ( c == SPC ) || ( c == TAB ) || ( c == NUL ) ) return i + 1;
		}
		char const c( str_[ 0 ] );
		return ( ( c == SPC ) || ( c == TAB ) || ( c == NUL ) ? 1 : 0 );
	}

	// Find Last Occurrence of a Non-Whitespace Character
	Fstring::size_type
	Fstring::find_last_non_whitespace() const
	{
		if ( len_ == 0u ) return 0;
		for ( size_type i = len_ - 1; i > 0u; --i ) {
			char const c( str_[ i ] );
			if ( ( c != SPC ) && ( c != TAB ) && ( c != NUL ) ) return i + 1;
		}
		char const c( str_[ 0 ] );
		return ( ( c != SPC ) && ( c != TAB ) && ( c != NUL ) ? 1 : 0 );
	}

	// Get Range of Whitespace-Trimmed Portion and Return its Length
	Fstring::size_type
	Fstring::trimmed_whitespace_range( size_type & b, size_type & e ) const
	{
		b = std::max( find_non_whitespace(), static_cast< size_type >( 1 ) );
		e = len_trim_whitespace();
		return e - b + 1;
	}

	// Find First Occurrence of an Fstring
	Fstring::size_type
	Fstring::find( Fstring const & s ) const
	{
		size_type const s_len( s.length() );
		if ( ( s_len > 0u ) && ( s_len <= len_ ) ) {
			for ( size_type i = 1, e = len_ - s_len + 1; i <= e; ++i ) {
				if ( (*this)( i, i + s_len - 1 ) == s ) return i;
			}
		}
		return 0; // No matches
	}

	// Find First Occurrence of a string
	Fstring::size_type
	Fstring::find( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		if ( ( s_len > 0u ) && ( s_len <= len_ ) ) {
			for ( size_type i = 1, e = len_ - s_len + 1; i <= e; ++i ) {
				if ( (*this)( i, i + s_len - 1 ) == s ) return i;
			}
		}
		return 0; // No matches
	}

	// Find First Occurrence of a cstring
	Fstring::size_type
	Fstring::find( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		if ( ( s_len > 0u ) && ( s_len <= len_ ) ) {
			for ( size_type i = 1, e = len_ - s_len + 1; i <= e; ++i ) {
				if ( (*this)( i, i + s_len - 1 ) == s ) return i;
			}
		}
		return 0; // No matches
	}

	// Find First Occurrence of a Character
	Fstring::size_type
	Fstring::find( char const c ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			if ( str_[ i - 1 ] == c ) return i;
		}
		return 0; // No matches
	}

	// Find Last Occurrence of an Fstring
	Fstring::size_type
	Fstring::find_last( Fstring const & s ) const
	{
		size_type const s_len( s.length() );
		if ( ( s_len > 0u ) && ( s_len <= len_ ) ) {
			for ( size_type i = len_ - s_len + 1; i > 0u; --i ) {
				if ( (*this)( i, i + s_len - 1 ) == s ) return i;
			}
		}
		return 0; // No matches
	}

	// Find Last Occurrence of a string
	Fstring::size_type
	Fstring::find_last( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		if ( ( s_len > 0u ) && ( s_len <= len_ ) ) {
			for ( size_type i = len_ - s_len + 1; i > 0u; --i ) {
				if ( (*this)( i, i + s_len - 1 ) == s ) return i;
			}
		}
		return 0; // No matches
	}

	// Find Last Occurrence of a cstring
	Fstring::size_type
	Fstring::find_last( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		if ( ( s_len > 0u ) && ( s_len <= len_ ) ) {
			for ( size_type i = len_ - s_len + 1; i > 0u; --i ) {
				if ( (*this)( i, i + s_len - 1 ) == s ) return i;
			}
		}
		return 0; // No matches
	}

	// Find Last Occurrence of a Character
	Fstring::size_type
	Fstring::find_last( char const c ) const
	{
		for ( size_type i = len_; i > 0u; --i ) {
			if ( str_[ i - 1 ] == c ) return i;
		}
		return 0; // No matches
	}

	// Find First Occurrence of any Character of an Fstring
	Fstring::size_type
	Fstring::find_first_of( Fstring const & s ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			for ( size_type j = 0, e = s.len_; j < e; ++j ) {
				if ( str_[ i - 1 ] == s.str_[ j ] ) return i;
			}
		}
		return 0; // No matches
	}

	// Find First Occurrence of any Character of a string
	Fstring::size_type
	Fstring::find_first_of( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		for ( size_type i = 1; i <= len_; ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) return i;
			}
		}
		return 0; // No matches
	}

	// Find First Occurrence of any Character of a cstring
	Fstring::size_type
	Fstring::find_first_of( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		for ( size_type i = 1; i <= len_; ++i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) return i;
			}
		}
		return 0; // No matches
	}

	// Find First Occurrence of a Character
	Fstring::size_type
	Fstring::find_first_of( char const c ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			if ( str_[ i - 1 ] == c ) return i;
		}
		return 0; // No matches
	}

	// Find First Occurrence of any Character not of an Fstring
	Fstring::size_type
	Fstring::find_first_not_of( Fstring const & s ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			bool found( false );
			for ( size_type j = 0, e = s.len_; j < e; ++j ) {
				if ( str_[ i - 1 ] == s.str_[ j ] ) {
					found = true;
					break;
				}
			}
			if ( ! found ) return i;
		}
		return 0; // No matches
	}

	// Find First Occurrence of any Character not of a string
	Fstring::size_type
	Fstring::find_first_not_of( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		for ( size_type i = 1; i <= len_; ++i ) {
			bool found( false );
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) {
					found = true;
					break;
				}
			}
			if ( ! found ) return i;
		}
		return 0; // No matches
	}

	// Find First Occurrence of any Character not of a cstring
	Fstring::size_type
	Fstring::find_first_not_of( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		for ( size_type i = 1; i <= len_; ++i ) {
			bool found( false );
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) {
					found = true;
					break;
				}
			}
			if ( ! found ) return i;
		}
		return 0; // No matches
	}

	// Find First Occurrence not of a Character
	Fstring::size_type
	Fstring::find_first_not_of( char const c ) const
	{
		for ( size_type i = 1; i <= len_; ++i ) {
			if ( str_[ i - 1 ] != c ) return i;
		}
		return 0; // No matches
	}

	// Find Last Occurrence of any Character of an Fstring
	Fstring::size_type
	Fstring::find_last_of( Fstring const & s ) const
	{
		for ( size_type i = len_; i > 0u; --i ) {
			for ( size_type j = 0, e = s.len_; j < e; ++j ) {
				if ( str_[ i - 1 ] == s.str_[ j ] ) return i;
			}
		}
		return 0; // No matches
	}

	// Find Last Occurrence of any Character of a string
	Fstring::size_type
	Fstring::find_last_of( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		for ( size_type i = len_; i > 0u; --i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) return i;
			}
		}
		return 0; // No matches
	}

	// Find Last Occurrence of any Character of a cstring
	Fstring::size_type
	Fstring::find_last_of( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		for ( size_type i = len_; i > 0u; --i ) {
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) return i;
			}
		}
		return 0; // No matches
	}

	// Find Last Occurrence of a Character
	Fstring::size_type
	Fstring::find_last_of( char const c ) const
	{
		for ( size_type i = len_; i > 0u; --i ) {
			if ( str_[ i - 1 ] == c ) return i;
		}
		return 0; // No matches
	}

	// Find Last Occurrence of any Character not of an Fstring
	Fstring::size_type
	Fstring::find_last_not_of( Fstring const & s ) const
	{
		for ( size_type i = len_; i > 0u; --i ) {
			bool found( false );
			for ( size_type j = 0, e = s.len_; j < e; ++j ) {
				if ( str_[ i - 1 ] == s.str_[ j ] ) {
					found = true;
					break;
				}
			}
			if ( ! found ) return i;
		}
		return 0; // No matches
	}

	// Find Last Occurrence of any Character not of a string
	Fstring::size_type
	Fstring::find_last_not_of( std::string const & s ) const
	{
		std::string::size_type const s_len( s.length() );
		for ( size_type i = len_; i > 0u; --i ) {
			bool found( false );
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) {
					found = true;
					break;
				}
			}
			if ( ! found ) return i;
		}
		return 0; // No matches
	}

	// Find Last Occurrence of any Character not of a cstring
	Fstring::size_type
	Fstring::find_last_not_of( c_cstring const s ) const
	{
		size_type const s_len( std::strlen( s ) );
		for ( size_type i = len_; i > 0u; --i ) {
			bool found( false );
			for ( size_type j = 0; j < s_len; ++j ) {
				if ( str_[ i - 1 ] == s[ j ] ) {
					found = true;
					break;
				}
			}
			if ( ! found ) return i;
		}
		return 0; // No matches
	}

	// Find Last Occurrence not of a Character
	Fstring::size_type
	Fstring::find_last_not_of( char const c ) const
	{
		for ( size_type i = len_; i > 0u; --i ) {
			if ( str_[ i - 1 ] != c ) return i;
		}
		return 0; // No matches
	}

	// Lowercase
	Fstring &
	Fstring::lowercase()
	{
		for ( size_type i = 0; i < len_; ++i ) {
			str_[ i ] = std::tolower( str_[ i ] );
		}
		return *this;
	}

	// Uppercase
	Fstring &
	Fstring::uppercase()
	{
		for ( size_type i = 0; i < len_; ++i ) {
			str_[ i ] = std::toupper( str_[ i ] );
		}
		return *this;
	}

	// Left Justify
	Fstring &
	Fstring::left_justify()
	{
		for ( size_type i = 0; i < len_; ++i ) {
			if ( str_[ i ] != SPC ) {
				if ( i > 0u ) {
					std::memmove( str_, str_ + i, len_ - i );
					std::memset( str_ + len_ - i, SPC, i );
				}
				return *this;
			}
		}
		return *this;
	}

	// Right Justify
	Fstring &
	Fstring::right_justify()
	{
		for ( size_type i = len_; i > 0u; --i ) {
			if ( str_[ i - 1 ] != SPC ) {
				if ( i < len_ ) {
					std::memmove( str_ + len_ - i, str_, i );
					std::memset( str_, SPC, len_ - i );
				}
				return *this;
			}
		}
		return *this;
	}

	// Center
	Fstring &
	Fstring::center()
	{
		left_justify();
		size_type const len_t( len_trim() );
		size_type const pad( ( len_ - len_t ) / 2 );
		if ( pad > 0u ) {
			std::memmove( str_ + pad, str_, len_t );
			std::memset( str_, SPC, pad );
		}
		return *this;
	}

	// Compress Out Whitespace
	Fstring &
	Fstring::compress()
	{
		size_type j( 0 );
		for ( size_type i = 0; i < len_; ++i ) {
			char const c( str_[ i ] );
			if ( ( c != SPC ) && ( c != TAB ) && ( c != NUL ) ) str_[ j++ ] = c;
		}
		if ( j < len_ ) std::memset( str_ + j, SPC, len_ - j );
		return *this;
	}

	// Trim Trailing Whitespace Replacing it with Space
	Fstring &
	Fstring::trim_whitespace()
	{
		if ( len_ > 0u ) {
			size_type const ie( len_trim_whitespace() );
			if ( ie < len_ ) {
				std::memset( str_ + ie, SPC, len_ - ie ); // Space pad
			}
		}
		return *this;
	}

	// Strip Specified Characters from the Tails
	Fstring &
	Fstring::strip( std::string const & chars )
	{
		if ( len_ > 0u ) {
			size_type const ib( find_first_not_of( chars ) );
			if ( ib == 0 ) {
				clear();
			} else {
				size_type const ie( find_last_not_of( chars ) );
				assert( ie >= ib );
				size_type const len_sub( ie - ib + 1 );
				if ( len_sub < len_ ) {
					if ( ib > 1 ) std::memmove( str_, str_ + ib - 1, len_sub );
					std::memset( str_ + len_sub, SPC, len_ - len_sub ); // Space pad
				}
			}
		}
		return *this;
	}

	// Strip Specified Characters from the Left Tail
	Fstring &
	Fstring::lstrip( std::string const & chars )
	{
		if ( len_ > 0u ) {
			size_type const ib( find_first_not_of( chars ) );
			if ( ib == 0 ) {
				clear();
			} else {
				size_type const len_sub( len_ - ib + 1 );
				if ( len_sub < len_ ) {
					if ( ib > 1 ) std::memmove( str_, str_ + ib - 1, len_sub );
					std::memset( str_ + len_sub, SPC, len_ - len_sub ); // Space pad
				}
			}
		}
		return *this;
	}

	// Strip Specified Characters from the Right Tail
	Fstring &
	Fstring::rstrip( std::string const & chars )
	{
		if ( len_ > 0u ) {
			size_type const ie( find_last_not_of( chars ) );
			if ( ie == 0 ) {
				clear();
			} else if ( ie < len_ ) {
				std::memset( str_ + ie, SPC, len_ - ie ); // Space pad
			}
		}
		return *this;
	}

	// Strip Space from the Tails
	Fstring &
	Fstring::strip()
	{
		if ( len_ > 0u ) {
			size_type const ib( find_first_not_of( SPC ) );
			if ( ib == 0 ) {
				clear();
			} else {
				size_type const ie( find_last_not_of( SPC ) );
				assert( ie >= ib );
				size_type const len_sub( ie - ib + 1 );
				if ( len_sub < len_ ) {
					if ( ib > 1 ) std::memmove( str_, str_ + ib - 1, len_sub );
					std::memset( str_ + len_sub, SPC, len_ - len_sub ); // Space pad
				}
			}
		}
		return *this;
	}

	// Strip Space from the Left Tail
	Fstring &
	Fstring::lstrip()
	{
		if ( len_ > 0u ) {
			size_type const ib( find_first_not_of( SPC ) );
			if ( ib == 0 ) {
				clear();
			} else {
				size_type const len_sub( len_ - ib + 1 );
				if ( len_sub < len_ ) {
					if ( ib > 1 ) std::memmove( str_, str_ + ib - 1, len_sub );
					std::memset( str_ + len_sub, SPC, len_ - len_sub ); // Space pad
				}
			}
		}
		return *this;
	}

	// Strip Space from the Right Tail
	Fstring &
	Fstring::rstrip()
	{
		if ( len_ > 0u ) {
			size_type const ie( find_last_not_of( SPC ) );
			if ( ie == 0 ) {
				clear();
			} else if ( ie < len_ ) {
				std::memset( str_ + ie, SPC, len_ - ie ); // Space pad
			}
		}
		return *this;
	}

	// Strip Whitespace from the Tails
	Fstring &
	Fstring::strip_whitespace()
	{
		if ( len_ > 0u ) {
			size_type const ib( find_first_not_of( WHITESPACE ) );
			if ( ib == 0 ) {
				clear();
			} else {
				size_type const ie( find_last_not_of( WHITESPACE ) );
				assert( ie >= ib );
				size_type const len_sub( ie - ib + 1 );
				if ( len_sub < len_ ) {
					if ( ib > 1 ) std::memmove( str_, str_ + ib - 1, len_sub );
					std::memset( str_ + len_sub, SPC, len_ - len_sub ); // Space pad
				}
			}
		}
		return *this;
	}

	// Strip Whitespace from the Left Tail
	Fstring &
	Fstring::lstrip_whitespace()
	{
		if ( len_ > 0u ) {
			size_type const ib( find_first_not_of( WHITESPACE ) );
			if ( ib == 0 ) {
				clear();
			} else {
				size_type const len_sub( len_ - ib + 1 );
				if ( len_sub < len_ ) {
					if ( ib > 1 ) std::memmove( str_, str_ + ib - 1, len_sub );
					std::memset( str_ + len_sub, SPC, len_ - len_sub ); // Space pad
				}
			}
		}
		return *this;
	}

	// Strip Whitespace from the Right Tail
	Fstring &
	Fstring::rstrip_whitespace()
	{
		if ( len_ > 0u ) {
			size_type const ie( find_last_not_of( WHITESPACE ) );
			if ( ie == 0 ) {
				clear();
			} else if ( ie < len_ ) {
				std::memset( str_ + ie, SPC, len_ - ie ); // Space pad
			}
		}
		return *this;
	}

	// Overlay an Fstring
	Fstring &
	Fstring::overlay( Fstring const & s, size_type const pos )
	{
		(*this)( pos, std::min( ( pos + s.len_ ) - 1, len_ ) ) = s;
		return *this;
	}

	// Overlay a string
	Fstring &
	Fstring::overlay( std::string const & s, size_type const pos )
	{
		(*this)( pos, std::min( ( pos + s.length() ) - 1, len_ ) ) = s;
		return *this;
	}

	// Overlay a cstring
	Fstring &
	Fstring::overlay( c_cstring const s, size_type const pos )
	{
		(*this)( pos, std::min( ( pos + std::strlen( s ) ) - 1, len_ ) ) = s;
		return *this;
	}

	// Null-Terminated cstring Copy of the Fstring that is Owned by the Fstring
	c_cstring
	Fstring::c_str() const
	{
		delete[] c_str_; c_str_ = new char[ len_ + 1 ];
		if ( len_ > 0u ) std::memmove( c_str_, str_, len_ ); // Copy the string data
		c_str_[ len_ ] = NUL; // Null-terminate
		return c_str_;
	}

	// Whitespace-Trimmed Null-Terminated cstring Copy of the Fstring that is Owned by the Fstring
	// This shares data/pointer with c_str()
	c_cstring
	Fstring::t_str() const
	{
		size_type const len_trim_whitespace_( len_trim_whitespace() );
		delete[] c_str_; c_str_ = new char[ len_trim_whitespace_ + 1 ];
		if ( len_trim_whitespace_ > 0u ) std::memmove( c_str_, str_, len_trim_whitespace_ ); // Copy the string data
		c_str_[ len_trim_whitespace_ ] = NUL; // Null-terminate
		return c_str_;
	}

	// Copy to a Pre-Allocated String
	Fstring::size_type
	Fstring::copy( cstring str, size_type const len, size_type const off ) const
	{
		assert( off <= len_ );
		size_type const len_copied( std::min( len_ - std::min( off, len_ ), len ) );
		if ( len_copied > 0u ) std::memmove( str, str_ + off, len_copied );
		return len_copied;
	}

	// Constant Substring: s( l, u )
	Fsubstring const
	Fstring::operator ()( size_type const l, size_type const u ) const
	{
		return Fsubstring( *this, l, u );
	}

	// Substring: s( l, u )
	Fsubstring
	Fstring::operator ()( size_type const l, size_type const u )
	{
		return Fsubstring( *this, l, u );
	}

	// Constant Substring: s( {l,u} )
	Fsubstring const
	Fstring::operator ()( std::initializer_list< int > const lu ) const
	{
		size_type const n( lu.size() );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			return Fsubstring( *this, 1 );
			break;
		case 1: // {l}
			return Fsubstring( *this, *i );
			break;
		case 2: // {l,u}
			return Fsubstring( *this, *i, *(i+1) );
			break;
		default:
			assert( false ); // Illegal
			return Fsubstring( *this, 1 );
			break;
		}
	}

	// Substring: s( {l,u} )
	Fsubstring
	Fstring::operator ()( std::initializer_list< int > const lu )
	{
		size_type const n( lu.size() );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			return Fsubstring( *this, 1 );
			break;
		case 1: // {l}
			return Fsubstring( *this, *i );
			break;
		case 2: // {l,u}
			return Fsubstring( *this, *i, *(i+1) );
			break;
		default:
			assert( false ); // Illegal
			return Fsubstring( *this, 1 );
			break;
		}
	}

	// Constant Substring: s( {l,u} )
	Fsubstring const
	Fstring::operator ()( std::initializer_list< Index > const lu ) const
	{
		size_type const n( lu.size() );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			return Fsubstring( *this, 1 );
			break;
		case 1: // {l}
			return Fsubstring( *this, i->initialized() ? int( *i ) : 1 );
			break;
		case 2: // {l,u}
			{
				auto const u( i + 1 );
				if ( u->initialized() ) {
					return Fsubstring( *this, i->initialized() ? int( *i ) : 1, int( *u ) );
				} else {
					return Fsubstring( *this, i->initialized() ? int( *i ) : 1 );
				}
			}
			break;
		default:
			assert( false ); // Illegal
			return Fsubstring( *this, 1 );
			break;
		}
	}

	// Substring: s( {l,u} )
	Fsubstring
	Fstring::operator ()( std::initializer_list< Index > const lu )
	{
		size_type const n( lu.size() );
		auto i( lu.begin() );
		switch ( n ) {
		case 0: // {}
			return Fsubstring( *this, 1 );
			break;
		case 1: // {l}
			return Fsubstring( *this, i->initialized() ? int( *i ) : 1 );
			break;
		case 2: // {l,u}
			{
				auto const u( i + 1 );
				if ( u->initialized() ) {
					return Fsubstring( *this, i->initialized() ? int( *i ) : 1, int( *u ) );
				} else {
					return Fsubstring( *this, i->initialized() ? int( *i ) : 1 );
				}
			}
			break;
		default:
			assert( false ); // Illegal
			return Fsubstring( *this, 1 );
			break;
		}
	}

	// Constant Tail Substring: s( l )
	Fstring const
	Fstring::operator ()( size_type const l ) const
	{
		return Fsubstring( *this, l );
	}

	// Tail Substring: s( l )
	Fsubstring
	Fstring::operator ()( size_type const l )
	{
		return Fsubstring( *this, l );
	}

	// Space-Free Head Constant Substring
	Fsubstring const
	Fstring::head() const
	{
		size_type const ie( find( SPC ) );
		if ( ie == 0 ) {
			return Fsubstring( *this, 1, len_ );
		} else {
			return Fsubstring( *this, 1, ie - 1 );
		}
	}

	// Space-Free Head Substring
	Fsubstring
	Fstring::head()
	{
		size_type const ie( find( SPC ) );
		if ( ie == 0 ) {
			return Fsubstring( *this, 1, len_ );
		} else {
			return Fsubstring( *this, 1, ie - 1 );
		}
	}

	// Space Tail Constant Substring
	Fsubstring const
	Fstring::tail() const
	{
		return Fsubstring( *this, len_trim() + 1 );
	}

	// Space Tail Substring
	Fsubstring
	Fstring::tail()
	{
		return Fsubstring( *this, len_trim() + 1 );
	}

	// Copy Assignment
	Fsubstring &
	Fsubstring::operator =( Fsubstring const & s )
	{
		if ( this != &s ) {
			if ( len_ > s.len_ ) {
				if ( s.len_ > 0u ) std::memmove( str_, s.str_, s.len_ );
				std::memset( str_ + s.len_, SPC, len_ - s.len_ ); // Space pad
			} else if ( len_ > 0u ) {
				std::memmove( str_, s.str_, len_ );
			}
		}
		return *this;
	}

	// = Fstring
	Fsubstring &
	Fsubstring::operator =( Fstring const & s )
	{
		if ( this != &s ) {
			if ( len_ > s.len_ ) {
				if ( s.len_ > 0u ) std::memmove( str_, s.str_, s.len_ );
				std::memset( str_ + s.len_, SPC, len_ - s.len_ ); // Space pad
			} else if ( len_ > 0u ) {
				std::memmove( str_, s.str_, len_ );
			}
		}
		return *this;
	}

	// = string
	Fsubstring &
	Fsubstring::operator =( std::string const & s )
	{
		size_type const s_len( s.length() );
		if ( len_ > s_len ) {
			if ( s_len > 0u ) s.copy( str_, s_len );
			std::memset( str_ + s_len, SPC, len_ - s_len ); // Space pad
		} else if ( len_ > 0u ) {
			s.copy( str_, len_ );
		}
		return *this;
	}

	// = cstring
	Fsubstring &
	Fsubstring::operator =( c_cstring const s )
	{
		size_type const s_len( std::strlen( s ) );
		if ( len_ > s_len ) {
			if ( s_len > 0u ) std::memmove( str_, s, s_len );
			std::memset( str_ + s_len, SPC, len_ - s_len ); // Space pad
		} else if ( len_ > 0u ) {
			std::memmove( str_, s, len_ );
		}
		return *this;
	}

	// = char
	Fsubstring &
	Fsubstring::operator =( char const c )
	{
		if ( len_ > 0u ) {
			str_[ 0 ] = c;
			if ( len_ > 1u ) std::memset( str_ + 1, SPC, len_ - 1 ); // Space pad
		}
		return *this;
	}

// Fstring Friends

// Fstring == Fstring
bool
operator ==( Fstring const & s, Fstring const & t )
{
	Fstring::size_type const min_len( std::min( s.len_, t.len_ ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		if ( s.str_[ i ] != t.str_[ i ] ) return false;
	}
	if ( s.len_ < t.len_ ) {
		for ( Fstring::size_type i = s.len_, e = t.len_; i < e; ++i ) {
			if ( t.str_[ i ] != SPC ) return false;
		}
	} else if ( s.len_ > t.len_ ) {
		for ( Fstring::size_type i = t.len_, e = s.len_; i < e; ++i ) {
			if ( s.str_[ i ] != SPC ) return false;
		}
	}
	return true;
}

// Fstring == string
bool
operator ==( Fstring const & s, std::string const & t )
{
	Fstring::size_type const t_len( t.length() );
	Fstring::size_type const min_len( std::min( s.len_, t_len ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		if ( s.str_[ i ] != t[ i ] ) return false;
	}
	if ( s.len_ < t_len ) {
		for ( Fstring::size_type i = s.len_; i < t_len; ++i ) {
			if ( t[ i ] != SPC ) return false;
		}
	} else if ( s.len_ > t_len ) {
		for ( Fstring::size_type i = t_len, e = s.len_; i < e; ++i ) {
			if ( s.str_[ i ] != SPC ) return false;
		}
	}
	return true;
}

// Fstring == cstring
bool
operator ==( Fstring const & s, c_cstring const t )
{
	Fstring::size_type const t_len( std::strlen( t ) );
	Fstring::size_type const min_len( std::min( s.len_, t_len ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		if ( s.str_[ i ] != t[ i ] ) return false;
	}
	if ( s.len_ < t_len ) {
		for ( Fstring::size_type i = s.len_; i < t_len; ++i ) {
			if ( t[ i ] != SPC ) return false;
		}
	} else if ( s.len_ > t_len ) {
		for ( Fstring::size_type i = t_len, e = s.len_; i < e; ++i ) {
			if ( s.str_[ i ] != SPC ) return false;
		}
	}
	return true;
}

// Fstring == char
bool
operator ==( Fstring const & s, char const c )
{
	if ( s.empty() ) { // Zero-length Fstring
		return false;
	} else if ( s.str_[ 0 ] == c ) { // First character matches
		return ( s( 2 ).is_blank() ); // Rest is blank
	} else { // First character doesn't match
		return false;
	}
}

// Fstring <= Fstring
bool
operator <=( Fstring const & s, Fstring const & t )
{
	Fstring::size_type const min_len( std::min( s.len_, t.len_ ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		unsigned char const s_i( s.str_[ i ] );
		unsigned char const t_i( t.str_[ i ] );
		if ( s_i < t_i ) {
			return true;
		} else if ( s_i > t_i ) {
			return false;
		}
	}
	if ( s.len_ < t.len_ ) {
		for ( Fstring::size_type i = s.len_, e = t.len_; i < e; ++i ) {
			unsigned char const t_i( t.str_[ i ] );
			if ( SPC < t_i ) {
				return true;
			} else if ( SPC > t_i ) {
				return false;
			}
		}
	} else if ( s.len_ > t.len_ ) {
		for ( Fstring::size_type i = t.len_, e = s.len_; i < e; ++i ) {
			unsigned char const s_i( s.str_[ i ] );
			if ( s_i < SPC ) {
				return true;
			} else if ( s_i > SPC ) {
				return false;
			}
		}
	}
	return true; // Equal
}

// Fstring < Fstring
bool
operator <( Fstring const & s, Fstring const & t )
{
	Fstring::size_type const min_len( std::min( s.len_, t.len_ ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		unsigned char const s_i( s.str_[ i ] );
		unsigned char const t_i( t.str_[ i ] );
		if ( s_i < t_i ) {
			return true;
		} else if ( s_i > t_i ) {
			return false;
		}
	}
	if ( s.len_ < t.len_ ) {
		for ( Fstring::size_type i = s.len_, e = t.len_; i < e; ++i ) {
			unsigned char const t_i( t.str_[ i ] );
			if ( SPC < t_i ) {
				return true;
			} else if ( SPC > t_i ) {
				return false;
			}
		}
	} else if ( s.len_ > t.len_ ) {
		for ( Fstring::size_type i = t.len_, e = s.len_; i < e; ++i ) {
			unsigned char const s_i( s.str_[ i ] );
			if ( s_i < SPC ) {
				return true;
			} else if ( s_i > SPC ) {
				return false;
			}
		}
	}
	return false; // Equal
}

// Fstring <= string
bool
operator <=( Fstring const & s, std::string const & t )
{
	Fstring::size_type const t_len( t.length() );
	Fstring::size_type const min_len( std::min( s.len_, t_len ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		unsigned char const s_i( s.str_[ i ] );
		unsigned char const t_i( t[ i ] );
		if ( s_i < t_i ) {
			return true;
		} else if ( s_i > t_i ) {
			return false;
		}
	}
	if ( s.len_ < t_len ) {
		for ( Fstring::size_type i = s.len_; i < t_len; ++i ) {
			unsigned char const t_i( t[ i ] );
			if ( SPC < t_i ) {
				return true;
			} else if ( SPC > t_i ) {
				return false;
			}
		}
	} else if ( s.len_ > t_len ) {
		for ( Fstring::size_type i = t_len, e = s.len_; i < e; ++i ) {
			unsigned char const s_i( s.str_[ i ] );
			if ( s_i < SPC ) {
				return true;
			} else if ( s_i > SPC ) {
				return false;
			}
		}
	}
	return true; // Equal
}

// Fstring < string
bool
operator <( Fstring const & s, std::string const & t )
{
	Fstring::size_type const t_len( t.length() );
	Fstring::size_type const min_len( std::min( s.len_, t_len ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		unsigned char const s_i( s.str_[ i ] );
		unsigned char const t_i( t[ i ] );
		if ( s_i < t_i ) {
			return true;
		} else if ( s_i > t_i ) {
			return false;
		}
	}
	if ( s.len_ < t_len ) {
		for ( Fstring::size_type i = s.len_; i < t_len; ++i ) {
			unsigned char const t_i( t[ i ] );
			if ( SPC < t_i ) {
				return true;
			} else if ( SPC > t_i ) {
				return false;
			}
		}
	} else if ( s.len_ > t_len ) {
		for ( Fstring::size_type i = t_len, e = s.len_; i < e; ++i ) {
			unsigned char const s_i( s.str_[ i ] );
			if ( s_i < SPC ) {
				return true;
			} else if ( s_i > SPC ) {
				return false;
			}
		}
	}
	return false; // Equal
}

// Fstring <= cstring
bool
operator <=( Fstring const & s, c_cstring const t )
{
	Fstring::size_type const t_len( std::strlen( t ) );
	Fstring::size_type const min_len( std::min( s.len_, t_len ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		unsigned char const s_i( s.str_[ i ] );
		unsigned char const t_i( t[ i ] );
		if ( s_i < t_i ) {
			return true;
		} else if ( s_i > t_i ) {
			return false;
		}
	}
	if ( s.len_ < t_len ) {
		for ( Fstring::size_type i = s.len_; i < t_len; ++i ) {
			unsigned char const t_i( t[ i ] );
			if ( SPC < t_i ) {
				return true;
			} else if ( SPC > t_i ) {
				return false;
			}
		}
	} else if ( s.len_ > t_len ) {
		for ( Fstring::size_type i = t_len, e = s.len_; i < e; ++i ) {
			unsigned char const s_i( s.str_[ i ] );
			if ( s_i < SPC ) {
				return true;
			} else if ( s_i > SPC ) {
				return false;
			}
		}
	}
	return true; // Equal
}

// Fstring < cstring
bool
operator <( Fstring const & s, c_cstring const t )
{
	Fstring::size_type const t_len( std::strlen( t ) );
	Fstring::size_type const min_len( std::min( s.len_, t_len ) );
	for ( Fstring::size_type i = 0; i < min_len; ++i ) {
		unsigned char const s_i( s.str_[ i ] );
		unsigned char const t_i( t[ i ] );
		if ( s_i < t_i ) {
			return true;
		} else if ( s_i > t_i ) {
			return false;
		}
	}
	if ( s.len_ < t_len ) {
		for ( Fstring::size_type i = s.len_; i < t_len; ++i ) {
			unsigned char const t_i( t[ i ] );
			if ( SPC < t_i ) {
				return true;
			} else if ( SPC > t_i ) {
				return false;
			}
		}
	} else if ( s.len_ > t_len ) {
		for ( Fstring::size_type i = t_len, e = s.len_; i < e; ++i ) {
			unsigned char const s_i( s.str_[ i ] );
			if ( s_i < SPC ) {
				return true;
			} else if ( s_i > SPC ) {
				return false;
			}
		}
	}
	return false; // Equal
}

// Stream Input
std::istream &
operator >>( std::istream & stream, Fstring & s )
{
	std::string ss;
	stream >> std::setw( s.len_ ) >> ss;
	s = ss;
	return stream;
}

// Get from Stream
std::istream &
get( std::istream & stream, Fstring & s )
{
	if ( s.len_ > 0u ) {
		char * const buff( new char[ s.len_ + 1 ] );
		stream.get( buff, s.len_ + 1 ); // get adds null-terminator
		std::size_t const lb( std::strlen( buff ) );
		std::memcpy( s.str_, buff, lb );
		std::memset( s.str_ + lb, SPC, s.len_ - lb );
		delete[] buff;
	}
	return stream;
}

// Get Line from Stream
std::istream &
getline( std::istream & stream, Fstring & s )
{
	std::string ss;
	stream.width( s.len_ );
	std::getline( stream, ss );
	s = ss;
	return stream;
}

// Read from Stream
std::istream &
read( std::istream & stream, Fstring & s )
{
	stream.read( s.str_, s.len_ );
	return stream;
}

// Read Available Characters from Stream
std::istream &
readsome( std::istream & stream, Fstring & s )
{
	stream.readsome( s.str_, s.len_ );
	return stream;
}

// Stream Output
std::ostream &
operator <<( std::ostream & stream, Fstring const & s )
{
	stream.write( s.str_, s.len_ );
	return stream;
}

} // ObjexxFCL
