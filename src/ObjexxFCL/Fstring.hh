#ifndef ObjexxFCL_Fstring_hh_INCLUDED
#define ObjexxFCL_Fstring_hh_INCLUDED

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
#include <ObjexxFCL/Fstring.fwd.hh>
#include <ObjexxFCL/char.constants.hh>
#include <ObjexxFCL/Index.hh>
#include <ObjexxFCL/Sticky.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/TypeTraits.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iosfwd>
#include <initializer_list>
#include <sstream>
#include <string>
#include <type_traits>

namespace ObjexxFCL {

// Types
typedef  char       *    cstring;
typedef  char const *  c_cstring;

// Fstring: Fixed-Length Fortran-Compatible String
//
// Note:
//  Subscripts run from 1 to the length
//  Space-padding is used in comparisons and assignments
//  Internal string rep is not null-terminated
//  Zero-length Fstrings are supported but cannot be indexed into (no valid indices)
//  All the length constructors are needed to avoid ambiguity with the char constructor
//  Assignment can set length/string if Fstring is uninitialized (default constructed)
//  Substrings: Use s( l, u ) or s( l ) / Pass s( l, u ).ref() to a non-const Fstring& argument
//  Assumes that char is a single-byte ASCII-collated character
class Fstring
{

private: // Friend

	friend class Fsubstring;

public: // Types

	// STL Style
	typedef  std::size_t  size_type;

	// C++ Style
	typedef  std::size_t  Size;

	typedef  std::function< void( Fstring & ) >  InitializerFunction;

public: // Creation

	// Default Constructor
	inline
	Fstring() :
		len_( 1 ),
		str_( new char[ 1 ] ), // Single space as in Fortran
		c_str_( nullptr ),
		sub_( false )
	{
		str_[ 0 ] = SPC;
	}

	// Copy Constructor
	Fstring( Fstring const & s );

	// Move Constructor
	Fstring( Fstring && s );

	// Substring Constructor
	Fstring( Fsubstring const & s );

	// string Constructor
	Fstring( std::string const & s );

	// cstring Constructor
	Fstring( c_cstring const s );

	// char Constructor
	inline
	explicit
	Fstring( char const c ) :
		len_( 1 ),
		str_( new char[ 1 ] ),
		c_str_( nullptr ),
		sub_( false )
	{
		str_[ 0 ] = c;
	}

	// signed char Constructor
	inline
	explicit
	Fstring( signed char const c ) :
		len_( 1 ),
		str_( new char[ 1 ] ),
		c_str_( nullptr ),
		sub_( false )
	{
		str_[ 0 ] = static_cast< char >( c );
	}

	// unsigned char Constructor
	inline
	explicit
	Fstring( unsigned char const c ) :
		len_( 1 ),
		str_( new char[ 1 ] ),
		c_str_( nullptr ),
		sub_( false )
	{
		str_[ 0 ] = static_cast< char >( c );
	}

	// Length Constructor
	explicit
	Fstring( short int const len );

	// Length Constructor
	explicit
	Fstring( int const len );

	// Length Constructor
	explicit
	Fstring( long int const len );

	// Length Constructor
	explicit
	Fstring( long long int const len );

	// Length Constructor
	explicit
	Fstring( unsigned short int const len );

	// Length Constructor
	explicit
	Fstring( unsigned int const len );

	// Length Constructor
	explicit
	Fstring( unsigned long int const len );

	// Length Constructor
	explicit
	Fstring( unsigned long long int const len );

	// Length + Fstring Constructor
	Fstring( size_type const len, Fstring const & s );

	// Length + string Constructor
	Fstring( size_type const len, std::string const & s );

	// Length + cstring Constructor
	Fstring( size_type const len, c_cstring const s );

	// Length + char Constructor
	// Fills with specified char => Use Fstring( len, "c" ) for space-padded single character
	Fstring( size_type const len, char const c );

	// Length + Initializer Constructor
	Fstring( size_type const len, InitializerFunction init );

	// Destructor
	inline
	virtual
	~Fstring()
	{
		if ( ! sub_ ) delete[] str_; // Substrings don't own/delete data
		delete[] c_str_;
	}

protected: // Creation

	// Substring Range Constructor
	Fstring( Fstring const & s, size_type const l, size_type const u );

	// Substring Tail Constructor
	Fstring( Fstring const & s, size_type const l );

	// From Substring Named Constructor
	inline
	static
	Fstring
	of_substring( Fstring const & s, size_type const l, size_type const u )
	{
		size_type const len( l <= std::min( u, s.len_ ) ? std::min( u, s.len_ ) - l + 1u : static_cast< size_type >( 0 ) );
		Fstring r( len );
		if ( len > 0u ) std::memcpy( r.str_, s.str_ + l - 1u, len );
		return r;
	}

	// From Substring Tail Named Constructor
	inline
	static
	Fstring
	of_substring( Fstring const & s, size_type const l )
	{
		size_type const len( l <= s.len_ ? s.len_ - l + 1u : static_cast< size_type >( 0 ) );
		Fstring r( len );
		if ( len > 0u ) std::memcpy( r.str_, s.str_ + l - 1u, len );
		return r;
	}

public: // Conversion

	// string Conversion
	inline
	operator std::string() const
	{
		return std::string( str_, len_ );
	}

public: // Assignment

	// Copy Assignment
	Fstring &
	operator =( Fstring const & s );

	// = string
	Fstring &
	operator =( std::string const & s );

	// = cstring
	Fstring &
	operator =( c_cstring const s );

	// = char
	Fstring &
	operator =( char const c );

public: // Reassignment

	// Fstring Reassignment
	void
	reassign( Fstring const & s );

public: // Subscript

	// Constant char: s[ i ]
	inline
	char
	operator []( size_type const i ) const
	{
		assert( i > 0 );
		assert( i <= len_ );
		return str_[ i - 1 ];
	}

	// char: s[ i ]
	inline
	char &
	operator []( size_type const i )
	{
		assert( i > 0 );
		assert( i <= len_ );
		return str_[ i - 1 ];
	}

public: // Predicate

	// Empty?
	inline
	bool
	empty() const
	{
		return ( len_ == 0u );
	}

	// Blank?
	inline
	bool
	is_blank() const
	{
		return ( len_trim() == 0 );
	}

	// Not blank?
	inline
	bool
	not_blank() const
	{
		return ( len_trim() > 0 );
	}

	// Whitespace?
	inline
	bool
	is_whitespace() const
	{
		return ( len_trim_whitespace() == 0 );
	}

	// Not whitespace?
	inline
	bool
	not_whitespace() const
	{
		return ( len_trim_whitespace() > 0 );
	}

	// Has an Fstring?
	bool
	has( Fstring const & s ) const;

	// Has a string?
	bool
	has( std::string const & s ) const;

	// Has a cstring?
	bool
	has( c_cstring const s ) const;

	// Has a Character?
	bool
	has( char const c ) const;

	// Has any Character of an Fstring?
	bool
	has_any_of( Fstring const & s ) const;

	// Has any Character of a string?
	bool
	has_any_of( std::string const & s ) const;

	// Has any Character of a cstring?
	bool
	has_any_of( c_cstring const s ) const;

	// Has a Character?
	bool
	has_any_of( char const c ) const;

	// Has a Prefix Case-Optionally?
	bool
	has_prefix( Fstring const & s, bool const exact_case = true ) const;

	// Has a Prefix Case-Optionally?
	bool
	has_prefix( c_cstring const s, bool const exact_case = true ) const;

	// Fstring is Readable as a Type Supporting Stream Input?
	template< typename T >
	inline
	bool
	is_type() const
	{
		if ( is_whitespace() ) { // Don't accept empty or whitespace Fstring
			return false;
		} else { // Try to read the Fstring as a T
			size_type b, e;
			size_type const trimmed_whitespace_length( trimmed_whitespace_range( b, e ) );
			std::istringstream t_stream( std::string( str_ + b - 1, trimmed_whitespace_length ) );
			T t;
			t_stream >> t;
			return ( ( t_stream ) && ( t_stream.eof() ) );
		}
	}

	// Fstring is Readable as a bool?
	inline
	bool
	is_bool() const
	{
		return is_type< bool >();
	}

	// Fstring is Readable as a short int?
	inline
	bool
	is_short() const
	{
		return is_type< short int >();
	}

	// Fstring is Readable as an int?
	inline
	bool
	is_int() const
	{
		return is_type< int >();
	}

	// Fstring is Readable as a long int?
	inline
	bool
	is_long() const
	{
		return is_type< long int >();
	}

	// Fstring is Readable as an unsigned short int?
	inline
	bool
	is_ushort() const
	{
		return is_type< unsigned short int >();
	}

	// Fstring is Readable as an unsigned int?
	inline
	bool
	is_uint() const
	{
		return is_type< unsigned int >();
	}

	// Fstring is Readable as an unsigned long int?
	inline
	bool
	is_ulong() const
	{
		return is_type< unsigned long int >();
	}

	// Fstring is Readable as a float?
	inline
	bool
	is_float() const
	{
		return is_type< float >();
	}

	// Fstring is Readable as a double?
	inline
	bool
	is_double() const
	{
		return is_type< double >();
	}

	// Fstring is Readable as a long double?
	inline
	bool
	is_longdouble() const
	{
		return is_type< long double >();
	}

	// Fstring is Readable as a char?
	inline
	bool
	is_char() const
	{
		return ( size() == 1 );
	}

	// Fstring is Readable as a string?
	inline
	bool
	is_string() const
	{
		return true;
	}

public: // Inspector

	// Size
	inline
	size_type
	size() const
	{
		return len_;
	}

	// Length
	inline
	size_type
	length() const
	{
		return len_;
	}

	// Length
	inline
	size_type
	len() const
	{
		return len_;
	}

	// Length Space-Trimmed
	size_type
	len_trim() const;

	// Length Whitespace-Trimmed
	size_type
	len_trim_whitespace() const;

	// Find First Occurrence of a Whitespace Character
	size_type
	find_whitespace() const;

	// Find First Occurrence of a Non-Whitespace Character
	size_type
	find_non_whitespace() const;

	// Find Last Occurrence of a Whitespace Character
	size_type
	find_last_whitespace() const;

	// Find Last Occurrence of a Non-Whitespace Character
	size_type
	find_last_non_whitespace() const;

	// Get Range of Whitespace-Trimmed Portion and Return its Length
	size_type
	trimmed_whitespace_range( size_type & b, size_type & e ) const;

	// Find First Occurrence of an Fstring
	size_type
	find( Fstring const & s ) const;

	// Find First Occurrence of a string
	size_type
	find( std::string const & s ) const;

	// Find First Occurrence of a cstring
	size_type
	find( c_cstring const s ) const;

	// Find First Occurrence of a Character
	size_type
	find( char const c ) const;

	// Find Last Occurrence of an Fstring
	size_type
	find_last( Fstring const & s ) const;

	// Find Last Occurrence of a string
	size_type
	find_last( std::string const & s ) const;

	// Find Last Occurrence of a cstring
	size_type
	find_last( c_cstring const s ) const;

	// Find Last Occurrence of a Character
	size_type
	find_last( char const c ) const;

	// Find First Occurrence of any Character of an Fstring
	size_type
	find_first_of( Fstring const & s ) const;

	// Find First Occurrence of any Character of a string
	size_type
	find_first_of( std::string const & s ) const;

	// Find First Occurrence of any Character of a cstring
	size_type
	find_first_of( c_cstring const s ) const;

	// Find First Occurrence of a Character
	size_type
	find_first_of( char const c ) const;

	// Find First Occurrence of any Character not of an Fstring
	size_type
	find_first_not_of( Fstring const & s ) const;

	// Find First Occurrence of any Character not of a string
	size_type
	find_first_not_of( std::string const & s ) const;

	// Find First Occurrence of any Character not of a cstring
	size_type
	find_first_not_of( c_cstring const s ) const;

	// Find First Occurrence of not a Character
	size_type
	find_first_not_of( char const c ) const;

	// Find Last Occurrence of any Character of an Fstring
	size_type
	find_last_of( Fstring const & s ) const;

	// Find Last Occurrence of any Character of a string
	size_type
	find_last_of( std::string const & s ) const;

	// Find Last Occurrence of any Character of a cstring
	size_type
	find_last_of( c_cstring const s ) const;

	// Find Last Occurrence of a Character
	size_type
	find_last_of( char const c ) const;

	// Find Last Occurrence of any Character not of an Fstring
	size_type
	find_last_not_of( Fstring const & s ) const;

	// Find Last Occurrence of any Character not of a string
	size_type
	find_last_not_of( std::string const & s ) const;

	// Find Last Occurrence of any Character not of a cstring
	size_type
	find_last_not_of( c_cstring const s ) const;

	// Find Last Occurrence not of a Character
	size_type
	find_last_not_of( char const c ) const;

	// Type of an Fstring for Type Supporting Stream Input
	template< typename T >
	inline
	T
	type_of() const
	{
		size_type b, e;
		size_type const trimmed_whitespace_length( trimmed_whitespace_range( b, e ) );
		std::istringstream t_stream( std::string( str_ + b - 1, trimmed_whitespace_length ) );
		T t;
		t_stream >> t;
		return ( t_stream && t_stream.eof() ? t : T() ); // Check is_type first
	}

	// short int of the Fstring
	inline
	short int
	short_of() const
	{
		return type_of< short int >();
	}

	// int of the Fstring
	inline
	int
	int_of() const
	{
		return type_of< int >();
	}

	// long int of the Fstring
	inline
	long int
	long_of() const
	{
		return type_of< long int >();
	}

	// unsigned short int of the Fstring
	inline
	unsigned short int
	ushort_of() const
	{
		return type_of< unsigned short int >();
	}

	// unsigned int of the Fstring
	inline
	unsigned int
	uint_of() const
	{
		return type_of< unsigned int >();
	}

	// unsigned long int of the Fstring
	inline
	unsigned long int
	ulong_of() const
	{
		return type_of< unsigned long int >();
	}

	// float of the Fstring
	inline
	float
	float_of() const
	{
		return type_of< float >();
	}

	// double of the Fstring
	inline
	double
	double_of() const
	{
		return type_of< double >();
	}

	// long double of the Fstring
	inline
	long double
	longdouble_of() const
	{
		return type_of< long double >();
	}

	// long double of the Fstring
	inline
	long double
	long_double_of() const
	{
		return type_of< long double >();
	}

	// char of the Fstring
	inline
	char
	char_of() const
	{
		return ( len_ == 1u ? str_[ 0 ] : char() ); // Check is_type first
	}

	// string of the Fstring
	inline
	std::string
	string_of() const
	{
		return std::string( str_, len_ );
	}

	// string of the Fstring
	inline
	std::string
	str() const
	{
		return std::string( str_, len_ );
	}

public: // Modifier

	// Lowercase
	Fstring &
	lowercase();

	// Uppercase
	Fstring &
	uppercase();

	// Left Justify
	Fstring &
	left_justify();

	// Right Justify
	Fstring &
	right_justify();

	// Center
	Fstring &
	center();

	// Compress Out Whitespace
	Fstring &
	compress();

	// Trim Trailing Space
	//  No effect for Fstring: Included for interface consistency
	inline
	Fstring &
	trim()
	{
		return *this;
	}

	// Trim Trailing Whitespace Replacing it with Space
	Fstring &
	trim_whitespace();

	// Strip Specified Characters from an Fstring's Tails
	Fstring &
	strip( std::string const & chars );

	// Strip Specified Characters from an Fstring's Left Tail
	Fstring &
	lstrip( std::string const & chars );

	// Strip Specified Characters from an Fstring's Right Tail
	Fstring &
	rstrip( std::string const & chars );

	// Strip Space from an Fstring's Tails
	Fstring &
	strip();

	// Strip Space from an Fstring's Left Tail
	Fstring &
	lstrip();

	// Strip Space from an Fstring's Right Tail
	Fstring &
	rstrip();

	// Strip Whitespace from an Fstring's Tails
	Fstring &
	strip_whitespace();

	// Strip Whitespace from an Fstring's Left Tail
	Fstring &
	lstrip_whitespace();

	// Strip Whitespace from an Fstring's Right Tail
	Fstring &
	rstrip_whitespace();

	// Clear
	inline
	Fstring &
	clear()
	{
		std::memset( str_, SPC, len_ );
		return *this;
	}

	// Overlay an Fstring
	Fstring &
	overlay( Fstring const & s, size_type const pos = 1 );

	// Overlay a string
	Fstring &
	overlay( std::string const & s, size_type const pos = 1 );

	// Overlay a cstring
	Fstring &
	overlay( c_cstring const s, size_type const pos = 1 );

public: // Generator

	// Left-Justified Copy
	inline
	Fstring
	left_justified() const
	{
		return Fstring( *this ).left_justify();
	}

	// Right-Justified Copy
	inline
	Fstring
	right_justified() const
	{
		return Fstring( *this ).right_justify();
	}

	// Centered Copy
	inline
	Fstring
	centered() const
	{
		return Fstring( *this ).center();
	}

	// Compressed Copy
	inline
	Fstring
	compressed() const
	{
		return Fstring( *this ).compress();
	}

	// Lowercased Copy
	inline
	Fstring
	lowercased() const
	{
		return Fstring( *this ).lowercase();
	}

	// Uppercased Copy
	inline
	Fstring
	uppercased() const
	{
		return Fstring( *this ).uppercase();
	}

	// Trailing Space Trimmed Copy
	inline
	Fstring
	trimmed() const
	{
		return Fstring::of_substring( *this, 1, len_trim() );
	}

	// Trailing Whitespace Trimmed Copy
	inline
	Fstring
	trimmed_whitespace() const
	{
		return Fstring::of_substring( *this, 1, len_trim_whitespace() );
	}

	// Specified Characters Stripped from Tails Copy
	inline
	Fstring
	stripped( std::string const & chars ) const
	{
		size_type const ib( find_first_not_of( chars ) );
		if ( ib > 0 ) {
			return Fstring::of_substring( *this, ib, find_last_not_of( chars ) );
		} else {
			return Fstring();
		}
	}

	// Specified Characters Stripped from Left Tail Copy
	inline
	Fstring
	lstripped( std::string const & chars ) const
	{
		size_type const ib( find_first_not_of( chars ) );
		if ( ib > 0 ) {
			return Fstring::of_substring( *this, ib );
		} else {
			return Fstring();
		}
	}

	// Specified Characters Stripped from Right Tail Copy
	inline
	Fstring
	rstripped( std::string const & chars ) const
	{
		return Fstring::of_substring( *this, 1, find_last_not_of( chars ) );
	}

	// Space Stripped from Tails Copy
	inline
	Fstring
	stripped() const
	{
		size_type const ib( find_first_not_of( SPC ) );
		if ( ib > 0 ) {
			return Fstring::of_substring( *this, ib, find_last_not_of( SPC ) );
		} else {
			return Fstring();
		}
	}

	// Space Stripped from Left Tail Copy
	inline
	Fstring
	lstripped() const
	{
		size_type const ib( find_first_not_of( SPC ) );
		if ( ib > 0 ) {
			return Fstring::of_substring( *this, ib );
		} else {
			return Fstring();
		}
	}

	// Space Stripped from Right Tail Copy
	inline
	Fstring
	rstripped() const
	{
		return Fstring::of_substring( *this, 1, find_last_not_of( SPC ) );
	}

	// Whitespace Stripped from Tails Copy
	inline
	Fstring
	stripped_whitespace() const
	{
		size_type const ib( find_first_not_of( " \t\000" ) );
		if ( ib > 0 ) {
			return Fstring::of_substring( *this, ib, find_last_not_of( " \t\000" ) );
		} else {
			return Fstring();
		}
	}

	// Whitespace Stripped from Left Tail Copy
	inline
	Fstring
	lstripped_whitespace() const
	{
		size_type const ib( find_first_not_of( " \t\000" ) );
		if ( ib > 0 ) {
			return Fstring::of_substring( *this, ib );
		} else {
			return Fstring();
		}
	}

	// Whitespace Stripped from Right Tail Copy
	inline
	Fstring
	rstripped_whitespace() const
	{
		return Fstring::of_substring( *this, 1, find_last_not_of( " \t\000" ) );
	}

	// Null-Terminated cstring Copy of the Fstring that is Owned by the Fstring
	c_cstring
	c_str() const;

	// Whitespace-Trimmed Null-Terminated cstring Copy of the Fstring that is Owned by the Fstring
	c_cstring
	t_str() const;

	// Non-Null-Terminated cstring Copy of the Fstring Data
	inline
	c_cstring
	data() const
	{
		return str_;
	}

	// Copy to a Pre-Allocated String
	size_type
	copy( cstring str, size_type const len, size_type const off = 0 ) const;

public: // Concatenation

	// Fstring + Fstring
	friend
	Fstring
	operator +( Fstring const & s, Fstring const & t );

	// Fstring + string
	friend
	std::string
	operator +( Fstring const & s, std::string const & t );

	// string + Fstring
	friend
	std::string
	operator +( std::string const & t, Fstring const & s );

	// Fstring + cstring
	friend
	Fstring
	operator +( Fstring const & s, c_cstring const t );

	// cstring + Fstring
	friend
	Fstring
	operator +( c_cstring const s, Fstring const & t );

	// Fstring + char
	friend
	Fstring
	operator +( Fstring const & s, char const c );

	// char + Fstring
	friend
	Fstring
	operator +( char const c, Fstring const & s );

public: // Comparison

	// Fstring == Fstring
	friend
	bool
	operator ==( Fstring const & s, Fstring const & t );

	// Fstring != Fstring
	friend
	bool
	operator !=( Fstring const & s, Fstring const & t );

	// Fstring == string
	friend
	bool
	operator ==( Fstring const & s, std::string const & t );

	// string == Fstring
	friend
	bool
	operator ==( std::string const & t, Fstring const & s );

	// Fstring != string
	friend
	bool
	operator !=( Fstring const & s, std::string const & t );

	// string != Fstring
	friend
	bool
	operator !=( std::string const & t, Fstring const & s );

	// Fstring == cstring
	friend
	bool
	operator ==( Fstring const & s, c_cstring const t );

	// cstring == Fstring
	friend
	bool
	operator ==( c_cstring const t, Fstring const & s );

	// Fstring != cstring
	friend
	bool
	operator !=( Fstring const & s, c_cstring const t );

	// cstring != Fstring
	friend
	bool
	operator !=( c_cstring const t, Fstring const & s );

	// Fstring == char
	friend
	bool
	operator ==( Fstring const & s, char const c );

	// char == Fstring
	friend
	bool
	operator ==( char const c, Fstring const & s );

	// Fstring != char
	friend
	bool
	operator !=( Fstring const & s, char const c );

	// char != Fstring
	friend
	bool
	operator !=( char const c, Fstring const & s );

	// Fstring == Fstring Case-Insensitively?
	friend
	bool
	equali( Fstring const & s, Fstring const & t );

	// Fstring == string Case-Insensitively?
	friend
	bool
	equali( Fstring const & s, std::string const & t );

	// string == Fstring Case-Insensitively?
	friend
	bool
	equali( std::string const & s, Fstring const & t );

	// Fstring == char Case-Insensitively?
	friend
	bool
	equali( Fstring const & s, char const c );

	// char == Fstring Case-Insensitively?
	friend
	bool
	equali( char const c, Fstring const & s );

	// Fstring == Fstring Case-Optionally?
	friend
	bool
	equal( Fstring const & s, Fstring const & t, bool const exact_case );

	// Fstring == char Case-Optionally?
	friend
	bool
	equal( Fstring const & s, char const c, bool const exact_case );

	// char == Fstring Case-Optionally?
	friend
	bool
	equal( char const c, Fstring const & s, bool const exact_case );

	// Fstring <= Fstring
	friend
	bool
	operator <=( Fstring const & s, Fstring const & t );

	// Fstring < Fstring
	friend
	bool
	operator <( Fstring const & s, Fstring const & t );

	// Fstring >= Fstring
	friend
	bool
	operator >=( Fstring const & s, Fstring const & t );

	// Fstring > Fstring
	friend
	bool
	operator >( Fstring const & s, Fstring const & t );

	// Fstring <= string
	friend
	bool
	operator <=( Fstring const & s, std::string const & t );

	// Fstring < string
	friend
	bool
	operator <( Fstring const & s, std::string const & t );

	// Fstring >= string
	friend
	bool
	operator >=( Fstring const & s, std::string const & t );

	// Fstring > string
	friend
	bool
	operator >( Fstring const & s, std::string const & t );

	// string <= Fstring
	friend
	bool
	operator <=( std::string const & s, Fstring const & t );

	// string < Fstring
	friend
	bool
	operator <( std::string const & s, Fstring const & t );

	// string >= Fstring
	friend
	bool
	operator >=( std::string const & s, Fstring const & t );

	// string > Fstring
	friend
	bool
	operator >( std::string const & s, Fstring const & t );

	// Fstring <= cstring
	friend
	bool
	operator <=( Fstring const & s, c_cstring const t );

	// Fstring < cstring
	friend
	bool
	operator <( Fstring const & s, c_cstring const t );

	// Fstring >= cstring
	friend
	bool
	operator >=( Fstring const & s, c_cstring const t );

	// Fstring > cstring
	friend
	bool
	operator >( Fstring const & s, c_cstring const t );

	// cstring <= Fstring
	friend
	bool
	operator <=( c_cstring const s, Fstring const & t );

	// cstring < Fstring
	friend
	bool
	operator <( c_cstring const s, Fstring const & t );

	// cstring >= Fstring
	friend
	bool
	operator >=( c_cstring const s, Fstring const & t );

	// cstring > Fstring
	friend
	bool
	operator >( c_cstring const s, Fstring const & t );

public: // Substring

	// Constant Substring: s( l, u )
	Fsubstring const
	operator ()( size_type const l, size_type const u ) const;

	// Substring: s( l, u )
	Fsubstring
	operator ()( size_type const l, size_type const u );

	// Constant Substring: s( {l,u} )
	Fsubstring const
	operator ()( std::initializer_list< int > const lu ) const;

	// Substring: s( {l,u} )
	Fsubstring
	operator ()( std::initializer_list< int > const lu );

	// Constant Substring: s( {l,u} )
	template< typename U >
	Fsubstring const
	operator ()( std::initializer_list< U > const lu, typename std::enable_if< std::is_constructible< int, U >::value >::type * = 0 ) const;

	// Substring: s( {l,u} )
	template< typename U >
	Fsubstring
	operator ()( std::initializer_list< U > const lu, typename std::enable_if< std::is_constructible< int, U >::value >::type * = 0 );

	// Constant Substring: s( {l,u} )
	Fsubstring const
	operator ()( std::initializer_list< Index > const lu ) const;

	// Substring: s( {l,u} )
	Fsubstring
	operator ()( std::initializer_list< Index > const lu );

	// Constant Tail Substring: s( l )
	Fstring const
	operator ()( size_type const l ) const;

	// Tail Substring: s( l )
	Fsubstring
	operator ()( size_type const l );

	// Space-Free Head Constant Substring
	Fsubstring const
	head() const;

	// Space-Free Head Substring
	Fsubstring
	head();

	// Space Tail Substring
	Fsubstring
	tail();

	// Space Tail Constant Substring
	Fsubstring const
	tail() const;

public: // I/O

	// Stream Input
	friend
	std::istream &
	operator >>( std::istream & stream, Fstring & s );

	// Get from Stream
	friend
	std::istream &
	get( std::istream & stream, Fstring & s );

	// Get Line from Stream
	friend
	std::istream &
	getline( std::istream & stream, Fstring & s );

	// Read from Stream
	friend
	std::istream &
	read( std::istream & stream, Fstring & s );

	// Read Available Characters from Stream
	friend
	std::istream &
	readsome( std::istream & stream, Fstring & s );

	// Stream Output
	friend
	std::ostream &
	operator <<( std::ostream & stream, Fstring const & s );

private: // Data

	size_type len_; // Length

	char * str_; // String

	mutable char * c_str_; // cstring

	bool const sub_; // Substring flag

}; // Fstring

// Fstring Member Function Explicit Specializations

	// Fstring is Readable as a char Supporting Stream Input?
	template<>
	inline
	bool
	Fstring::is_type< char >() const
	{
		return ( size() == 1 );
	}

	// Fstring is Readable as a string Supporting Stream Input?
	template<>
	inline
	bool
	Fstring::is_type< std::string >() const
	{
		return true;
	}

	// char of an Fstring
	template<>
	inline
	char
	Fstring::type_of< char >() const
	{
		return ( len_ == 1u ? str_[ 0 ] : char() ); // Check is_type first
	}

	// string of an Fstring
	template<>
	inline
	std::string
	Fstring::type_of< std::string >() const
	{
		return std::string( str_, len_ );
	}

// Fsubstring: Fixed-Length Fortran-Compatible Substring
//
// Note:
//  Subscripts run from 1 to the length
//  Space-padding is used in comparisons and assignments
//  Internal string rep is not null-terminated
//  Zero-length Fsubstrings are supported but cannot be indexed into (no valid indices)
//  Fsubstring not for explicit use in client code: Client code uses Fstring::operator () to get substrings
//  Pass s( i, j ).ref() to a non-const Fstring& argument
//  Don't return a substring of a local as an Fsubstring since its copy ctor uses ref semantics: Return as an Fstring to get a copy
class Fsubstring : public Fstring
{

private: // Types

	typedef  Fstring  Super;

	friend class Fstring;

public: // Creation

	// Copy Constructor
	inline
	Fsubstring( Fsubstring const & s ) :
		Fstring( s, 1, s.len_ )
	{}

	// Destructor
	inline
	virtual
	~Fsubstring()
	{}

private: // Creation

	// Fstring Range Constructor
	inline
	Fsubstring( Fstring const & s, size_type const i, size_type const j ) :
		Fstring( s, i, j )
	{}

	// Fstring Tail Constructor
	inline
	Fsubstring( Fstring const & s, size_type const i ) :
		Fstring( s, i )
	{}

public: // Assignment

	// Copy Assignment
	Fsubstring &
	operator =( Fsubstring const & s );

	// = Fstring
	Fsubstring &
	operator =( Fstring const & s );

	// = string
	Fsubstring &
	operator =( std::string const & s );

	// = cstring
	Fsubstring &
	operator =( c_cstring const s );

	// = char
	Fsubstring &
	operator =( char const c );

public: // Modifier

	// Reference to Fstring: Can Pass s( i, j ).ref() to an Fstring& Argument
	inline
	Fstring &
	ref()
	{
		return *this;
	}

}; // Fsubstring

// Fstring Member Function Definitions

	// Constant Substring: s( {l,u} )
	template< typename U >
	Fsubstring const
	Fstring::operator ()( std::initializer_list< U > const lu, typename std::enable_if< std::is_constructible< int, U >::value >::type * ) const
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
	template< typename U >
	Fsubstring
	Fstring::operator ()( std::initializer_list< U > const lu, typename std::enable_if< std::is_constructible< int, U >::value >::type * )
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

// Fstring Friends

// Fstring + Fstring
inline
Fstring
operator +( Fstring const & s, Fstring const & t )
{
	Fstring u( static_cast< Fstring::size_type >( s.len_ + t.len_ ) );
	std::memcpy( u.str_, s.str_, s.len_ );
	std::memcpy( u.str_ + s.len_, t.str_, t.len_ );
	return u;
}

// Fstring + string
inline
std::string
operator +( Fstring const & s, std::string const & t )
{
	return ( static_cast< std::string >( s ) + t );
}

// string + Fstring
inline
std::string
operator +( std::string const & t, Fstring const & s )
{
	return ( t + static_cast< std::string >( s ) );
}

// Fstring + cstring
inline
Fstring
operator +( Fstring const & s, c_cstring const t )
{
	Fstring::size_type const t_len( std::strlen( t ) );
	Fstring u( s.len_ + t_len );
	std::memcpy( u.str_, s.str_, s.len_ );
	std::memcpy( u.str_ + s.len_, t, t_len );
	return u;
}

// cstring + Fstring
inline
Fstring
operator +( c_cstring const s, Fstring const & t )
{
	Fstring::size_type const s_len( std::strlen( s ) );
	Fstring u( s_len + t.len_ );
	std::memcpy( u.str_, s, s_len );
	std::memcpy( u.str_ + s_len, t.str_, t.len_ );
	return u;
}

// Fstring + char
inline
Fstring
operator +( Fstring const & s, char const c )
{
	Fstring u( s.len_ + 1 );
	std::memcpy( u.str_, s.str_, s.len_ );
	u.str_[ s.len_ ] = c;
	return u;
}

// char + Fstring
inline
Fstring
operator +( char const c, Fstring const & s )
{
	Fstring u( 1 + s.len_ );
	u.str_[ 0 ] = c;
	std::memcpy( u.str_ + 1, s.str_, s.len_ );
	return u;
}

// Fstring == Fstring
bool
operator ==( Fstring const & s, Fstring const & t );

// Fstring != Fstring
inline
bool
operator !=( Fstring const & s, Fstring const & t )
{
	return !( s == t );
}

// Fstring == string
bool
operator ==( Fstring const & s, std::string const & t );

// string == Fstring
inline
bool
operator ==( std::string const & t, Fstring const & s )
{
	return ( s == t );
}

// Fstring != string
inline
bool
operator !=( Fstring const & s, std::string const & t )
{
	return !( s == t );
}

// string != Fstring
inline
bool
operator !=( std::string const & t, Fstring const & s )
{
	return !( s == t );
}

// Fstring == cstring
bool
operator ==( Fstring const & s, c_cstring const t );

// cstring == Fstring
inline
bool
operator ==( c_cstring const t, Fstring const & s )
{
	return ( s == t );
}

// Fstring != cstring
inline
bool
operator !=( Fstring const & s, c_cstring const t )
{
	return !( s == t );
}

// cstring != Fstring
inline
bool
operator !=( c_cstring const t, Fstring const & s )
{
	return !( s == t );
}

// Fstring == char
bool
operator ==( Fstring const & s, char const c );

// char == Fstring
inline
bool
operator ==( char const c, Fstring const & s )
{
	return ( s == c );
}

// Fstring != char
inline
bool
operator !=( Fstring const & s, char const c )
{
	return !( s == c );
}

// char != Fstring
inline
bool
operator !=( char const c, Fstring const & s )
{
	return !( s == c );
}

// Fstring == Fstring Case-Insensitively?
inline
bool
equali( Fstring const & s, Fstring const & t )
{
	return ( s.lowercased() == t.lowercased() );
}

// Fstring == string Case-Insensitively?
inline
bool
equali( Fstring const & s, std::string const & t )
{
	return ( s.lowercased() == ObjexxFCL::lowercased( t ) );
}

// string == Fstring Case-Insensitively?
inline
bool
equali( std::string const & s, Fstring const & t )
{
	return ( ObjexxFCL::lowercased( s ) == t.lowercased() );
}

// Fstring == char Case-Insensitively?
inline
bool
equali( Fstring const & s, char const c )
{
	return ( s.lowercased() == char( std::tolower( c ) ) );
}

// char == Fstring Case-Insensitively?
inline
bool
equali( char const c, Fstring const & s )
{
	return ( s.lowercased() == char( std::tolower( c ) ) );
}

// Fstring == Fstring Case-Optionally?
inline
bool
equal( Fstring const & s, Fstring const & t, bool const exact_case = true )
{
	if ( exact_case ) {
		return ( s == t );
	} else {
		return ( s.lowercased() == t.lowercased() );
	}
}

// Fstring == char Case-Optionally?
inline
bool
equal( Fstring const & s, char const c, bool const exact_case = true )
{
	if ( exact_case ) {
		return ( s == c );
	} else {
		return ( s.lowercased() == char( std::tolower( c ) ) );
	}
}

// char == Fstring Case-Optionally?
inline
bool
equal( char const c, Fstring const & s, bool const exact_case = true )
{
	if ( exact_case ) {
		return ( s == c );
	} else {
		return ( s.lowercased() == char( std::tolower( c ) ) );
	}
}

// Fstring <= Fstring
bool
operator <=( Fstring const & s, Fstring const & t );

// Fstring < Fstring
bool
operator <( Fstring const & s, Fstring const & t );

// Fstring >= Fstring
inline
bool
operator >=( Fstring const & s, Fstring const & t )
{
	return !( s < t );
}

// Fstring > Fstring
inline
bool
operator >( Fstring const & s, Fstring const & t )
{
	return !( s <= t );
}

// Fstring <= string
bool
operator <=( Fstring const & s, std::string const & t );

// Fstring < string
bool
operator <( Fstring const & s, std::string const & t );

// Fstring >= string
inline
bool
operator >=( Fstring const & s, std::string const & t )
{
	return !( s < t );
}

// Fstring > string
inline
bool
operator >( Fstring const & s, std::string const & t )
{
	return !( s <= t );
}

// string <= Fstring
inline
bool
operator <=( std::string const & s, Fstring const & t )
{
	return ( t >= s );
}

// string < Fstring
inline
bool
operator <( std::string const & s, Fstring const & t )
{
	return ( t > s );
}

// string >= Fstring
inline
bool
operator >=( std::string const & s, Fstring const & t )
{
	return ( t <= s );
}

// string > Fstring
inline
bool
operator >( std::string const & s, Fstring const & t )
{
	return ( t < s );
}

// Fstring <= cstring
bool
operator <=( Fstring const & s, c_cstring const t );

// Fstring < cstring
bool
operator <( Fstring const & s, c_cstring const t );

// Fstring >= cstring
inline
bool
operator >=( Fstring const & s, c_cstring const t )
{
	return !( s < t );
}

// Fstring > cstring
inline
bool
operator >( Fstring const & s, c_cstring const t )
{
	return !( s <= t );
}

// cstring <= Fstring
inline
bool
operator <=( c_cstring const s, Fstring const & t )
{
	return ( t >= s );
}

// cstring < Fstring
inline
bool
operator <( c_cstring const s, Fstring const & t )
{
	return ( t > s );
}

// cstring >= Fstring
inline
bool
operator >=( c_cstring const s, Fstring const & t )
{
	return ( t <= s );
}

// cstring > Fstring
inline
bool
operator >( c_cstring const s, Fstring const & t )
{
	return ( t < s );
}

// Stream Input
std::istream &
operator >>( std::istream & stream, Fstring & s );

// Get from Stream
std::istream &
get( std::istream & stream, Fstring & s );

// Get Line from Stream
std::istream &
getline( std::istream & stream, Fstring & s );

// Read from Stream
std::istream &
read( std::istream & stream, Fstring & s );

// Read Available Characters from Stream
std::istream &
readsome( std::istream & stream, Fstring & s );

// Stream Output
std::ostream &
operator <<( std::ostream & stream, Fstring const & s );

// Fortran-Intrinsic-Compatible String Functions

// One-Character Fstring of a Given ASCII Integer Value
inline
Fstring
CHAR( int const i )
{
	return Fstring( static_cast< char >( i ) );
}

// One-Character Fstring of a Given ASCII Integer Value
inline
Fstring
achar( int const i )
{
	return Fstring( static_cast< char >( i ) );
}

// One-Character Fstring of a Given ASCII Integer Value
inline
Fstring
ACHAR( int const i )
{
	return Fstring( static_cast< char >( i ) );
}

// Integer Value of a Given One-Character Fstring
inline
int
ichar( Fstring const & s )
{
	assert( s.length() == 1 );
	return static_cast< int >( s[ 1 ] );
}

// Integer Value of a Given One-Character Fstring
inline
int
ICHAR( Fstring const & s )
{
	assert( s.length() == 1 );
	return static_cast< int >( s[ 1 ] );
}

// ASCII Integer Value of a Given One-Character Fstring
inline
int
iachar( Fstring const & s )
{
	assert( s.length() == 1 );
	return static_cast< int >( s[ 1 ] );
}

// ASCII Integer Value of a Given One-Character Fstring
inline
int
IACHAR( Fstring const & s )
{
	assert( s.length() == 1 );
	return static_cast< int >( s[ 1 ] );
}

// First Index Position of a Substring in an Fstring
inline
int
index( Fstring const & s, Fstring const & ss, bool const back = false )
{
	return ( back ? s.find_last( ss ) : s.find( ss ) );
}

// First Index Position of a Substring in an Fstring
inline
int
index( Fstring const & s, std::string const & ss, bool const back = false )
{
	return ( back ? s.find_last( ss ) : s.find( ss ) );
}

// First Index Position of a Substring in an Fstring
inline
int
index( Fstring const & s, c_cstring const ss, bool const back = false )
{
	return ( back ? s.find_last( ss ) : s.find( ss ) );
}

// First Index Position of a Character in an Fstring
inline
int
index( Fstring const & s, char const c, bool const back = false )
{
	return ( back ? s.find_last( c ) : s.find( c ) );
}

// First Index Position of a Substring in an Fstring
inline
int
INDEX( Fstring const & s, Fstring const & ss, bool const back = false )
{
	return ( back ? s.find_last( ss ) : s.find( ss ) );
}

// First Index Position of a Substring in an Fstring
inline
int
INDEX( Fstring const & s, std::string const & ss, bool const back = false )
{
	return ( back ? s.find_last( ss ) : s.find( ss ) );
}

// First Index Position of a Substring in an Fstring
inline
int
INDEX( Fstring const & s, c_cstring const ss, bool const back = false )
{
	return ( back ? s.find_last( ss ) : s.find( ss ) );
}

// First Index Position of a Character in an Fstring
inline
int
INDEX( Fstring const & s, char const c, bool const back = false )
{
	return ( back ? s.find_last( c ) : s.find( c ) );
}

// Find any Characters of Another String
inline
int
scan( Fstring const & s, Fstring const & t, bool const back = false )
{
	return ( back ? s.find_last_of( t ) : s.find_first_of( t ) );
}

// Find any Characters of Another String
inline
int
scan( Fstring const & s, std::string const & t, bool const back = false )
{
	return ( back ? s.find_last_of( t ) : s.find_first_of( t ) );
}

// Find any Characters of Another String
inline
int
scan( Fstring const & s, c_cstring const t, bool const back = false )
{
	return ( back ? s.find_last_of( t ) : s.find_first_of( t ) );
}

// Find a Character
inline
int
scan( Fstring const & s, char const c, bool const back = false )
{
	return ( back ? s.find_last_of( c ) : s.find_first_of( c ) );
}

// Find any Characters of Another String
inline
int
SCAN( Fstring const & s, Fstring const & t, bool const back = false )
{
	return ( back ? s.find_last_of( t ) : s.find_first_of( t ) );
}

// Find any Characters of Another String
inline
int
SCAN( Fstring const & s, std::string const & t, bool const back = false )
{
	return ( back ? s.find_last_of( t ) : s.find_first_of( t ) );
}

// Find any Characters of Another String
inline
int
SCAN( Fstring const & s, c_cstring const t, bool const back = false )
{
	return ( back ? s.find_last_of( t ) : s.find_first_of( t ) );
}

// Find a Character
inline
int
SCAN( Fstring const & s, char const c, bool const back = false )
{
	return ( back ? s.find_last_of( c ) : s.find_first_of( c ) );
}

// Find any Characters not of Another String
inline
int
verify( Fstring const & s, Fstring const & t, bool const back = false )
{
	return ( back ? s.find_last_not_of( t ) : s.find_first_not_of( t ) );
}

// Find any Characters not of Another String
inline
int
verify( Fstring const & s, std::string const & t, bool const back = false )
{
	return ( back ? s.find_last_not_of( t ) : s.find_first_not_of( t ) );
}

// Find any Characters not of Another String
inline
int
verify( Fstring const & s, c_cstring const t, bool const back = false )
{
	return ( back ? s.find_last_not_of( t ) : s.find_first_not_of( t ) );
}

// Find Character not a Character
inline
int
verify( Fstring const & s, char const c, bool const back = false )
{
	return ( back ? s.find_last_not_of( c ) : s.find_first_not_of( c ) );
}

// Find any Characters not of Another String
inline
int
VERIFY( Fstring const & s, Fstring const & t, bool const back = false )
{
	return ( back ? s.find_last_not_of( t ) : s.find_first_not_of( t ) );
}

// Find any Characters not of Another String
inline
int
VERIFY( Fstring const & s, std::string const & t, bool const back = false )
{
	return ( back ? s.find_last_not_of( t ) : s.find_first_not_of( t ) );
}

// Find any Characters not of Another String
inline
int
VERIFY( Fstring const & s, c_cstring const t, bool const back = false )
{
	return ( back ? s.find_last_not_of( t ) : s.find_first_not_of( t ) );
}

// Find Character not a Character
inline
int
VERIFY( Fstring const & s, char const c, bool const back = false )
{
	return ( back ? s.find_last_not_of( c ) : s.find_first_not_of( c ) );
}

// Length
inline
int
len( Fstring const & s )
{
	return s.length();
}

// Length
inline
int
LEN( Fstring const & s )
{
	return s.length();
}

// Length Space-Trimmed
inline
int
len_trim( Fstring const & s )
{
	return s.len_trim();
}

// Length Space-Trimmed
inline
int
LEN_TRIM( Fstring const & s )
{
	return s.len_trim();
}

// Space-Trimmed Copy
inline
Fstring
trimmed( Fstring const & s )
{
	return s.trimmed();
}

// Space-Trimmed Copy
inline
Fstring
TRIMMED( Fstring const & s )
{
	return s.trimmed();
}

// Space-Trimmed Copy
inline
Fstring
trim( Fstring const & s )
{
	return s.trimmed();
}

// Space-Trimmed Copy
inline
Fstring
TRIM( Fstring const & s )
{
	return s.trimmed();
}

// Left-Justified Copy
inline
Fstring
adjustl( Fstring const & s )
{
	return s.left_justified();
}

// Left-Justified Copy
inline
Fstring
ADJUSTL( Fstring const & s )
{
	return s.left_justified();
}

// Right-Justified Copy
inline
Fstring
adjustr( Fstring const & s )
{
	return s.right_justified();
}

// Right-Justified Copy
inline
Fstring
ADJUSTR( Fstring const & s )
{
	return s.right_justified();
}

// ASCII Lexical >= Comparison
inline
bool
lge( Fstring const & s, Fstring const & t )
{
	return ( s >= t );
}

// ASCII Lexical < Comparison
inline
bool
lgt( Fstring const & s, Fstring const & t )
{
	return ( s > t );
}

// ASCII Lexical <= Comparison
inline
bool
lle( Fstring const & s, Fstring const & t )
{
	return ( s <= t );
}

// ASCII Lexical < Comparison
inline
bool
llt( Fstring const & s, Fstring const & t )
{
	return ( s < t );
}

// Fortran Migration Support String Functions

// Predicate

// Fstring is Blank?
inline
bool
is_blank( Fstring const & s )
{
	return s.is_blank();
}

// Fstring is Not Blank?
inline
bool
not_blank( Fstring const & s )
{
	return s.not_blank();
}

// Fstring has any Characters of a Set?
inline
bool
has_any_of( Fstring const & s, Fstring const & t )
{
	return s.has_any_of( t );
}

// Fstring has any Characters of a Set?
inline
bool
has_any_of( Fstring const & s, std::string const & t )
{
	return s.has_any_of( t );
}

// Fstring has any Characters of a Set?
inline
bool
has_any_of( Fstring const & s, c_cstring const t )
{
	return s.has_any_of( t );
}

// Search

// Last Index Position of a Substring in an Fstring
inline
int
last_index( Fstring const & s, Fstring const & ss )
{
	return s.find_last( ss );
}

// Last Index Position of a Substring in an Fstring
inline
int
last_index( Fstring const & s, std::string const & ss )
{
	return s.find_last( ss );
}

// Last Index Position of a Substring in an Fstring
inline
int
last_index( Fstring const & s, c_cstring const ss )
{
	return s.find_last( ss );
}

// Last Index Position of a Character in an Fstring
inline
int
last_index( Fstring const & s, char const c )
{
	return s.find_last( c );
}

// Modifier

// Lowercase an Fstring
inline
Fstring &
lowercase( Fstring & s )
{
	return s.lowercase();
}

// Uppercase an Fstring
inline
Fstring &
uppercase( Fstring & s )
{
	return s.uppercase();
}

// Lowercase an Fstring
inline
void
str_dn( Fstring & s )
{
	s.lowercase();
}

// Uppercase an Fstring
inline
void
str_up( Fstring & s )
{
	s.uppercase();
}

// Lowercased Copy in an Output Fstring
inline
void
str_dncase( Fstring & s_out, Fstring const & s_in )
{
	s_out = s_in;
	s_out.lowercase();
}

// Uppercased Copy in an Output Fstring
inline
void
str_upcase( Fstring & s_out, Fstring const & s_in )
{
	s_out = s_in;
	s_out.uppercase();
}

// Generator

// Left-Justified Copy
inline
Fstring
ljust( Fstring const & s )
{
	return s.left_justified();
}

// Left-Justified Copy
inline
Fstring
left_justified( Fstring const & s )
{
	return s.left_justified();
}

// Right-Justified Copy
inline
Fstring
rjust( Fstring const & s )
{
	return s.right_justified();
}

// Right-Justified Copy
inline
Fstring
right_justified( Fstring const & s )
{
	return s.right_justified();
}

// Compressed Copy
inline
Fstring
compressed( Fstring const & s )
{
	return s.compressed();
}

// Compressed Copy
inline
Fstring
compress( Fstring const & s )
{
	return s.compressed();
}

// Centered Copy
inline
Fstring
centered( Fstring const & s )
{
	return s.centered();
}

// Centered Copy
inline
Fstring
center( Fstring const & s )
{
	return s.centered();
}

// Lowercased Copy
inline
Fstring
lowercased( Fstring const & s )
{
	return s.lowercased();
}

// Uppercased Copy
inline
Fstring
uppercased( Fstring const & s )
{
	return s.uppercased();
}

// Lowercased Copy
inline
Fstring
dncase( Fstring const & s )
{
	return s.lowercased();
}

// Uppercased Copy
inline
Fstring
upcase( Fstring const & s )
{
	return s.uppercased();
}

// Conversion To Fstring

// Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
Fstring_of( T const & t )
{
	std::ostringstream t_stream;
	t_stream << std::uppercase << std::setprecision( TypeTraits< T >::precision() ) << t;
	return t_stream.str();
}

// Fstring of a string Specialization
template<>
inline
Fstring
Fstring_of< std::string >( std::string const & t )
{
	return Fstring( t );
}

// Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
Fstring_of(
	T const & t,
	int const p // Precision
)
{
	std::ostringstream t_stream;
	t_stream << std::uppercase << std::setprecision( p ) << t;
	return t_stream.str();
}

// Left-Justified Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
left_Fstring_of(
	T const & t,
	int const w, // Minimum width
	char const f = SPC // Fill character
)
{
	std::ostringstream t_stream;
	t_stream << std::left << std::uppercase
	 << std::setw( w ) << std::setfill( f ) << std::setprecision( TypeTraits< T >::precision() ) << t;
	return t_stream.str();
}

// Right-Justified Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
right_Fstring_of(
	T const & t,
	int const w, // Minimum width
	char const f = SPC // Fill character
)
{
	std::ostringstream t_stream;
	t_stream << std::right << std::uppercase
	 << std::setw( w ) << std::setfill( f ) << std::setprecision( TypeTraits< T >::precision() ) << t;
	return t_stream.str();
}

// Leading-Zero Right-Justified Fstring of a Template Argument Type Supporting Stream Output
// Negative numbers appear with the minus sign on the left of the filled zeros
template< typename T >
inline
Fstring
lead_zero_Fstring_of(
	T const & t,
	int const w // Minimum width
)
{
	std::ostringstream t_stream;
	t_stream << std::internal << std::uppercase
	 << std::setw( w ) << std::setfill( '0' ) << std::setprecision( TypeTraits< T >::precision() ) << t;
	return t_stream.str();
}

// Right-Justified General Format Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
general_Fstring_of(
	T const & t,
	int const w = TypeTraits< T >::width(), // Minimum width
	int const p = TypeTraits< T >::precision() // Precision
)
{
	std::ostringstream t_stream;
	t_stream << std::right << std::uppercase << std::showpoint
	 << std::setw( w ) << std::setprecision( p ) << t;
	return t_stream.str();
}

// Right-Justified Fixed Format Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
fixed_Fstring_of(
	T const & t,
	int const w = TypeTraits< T >::width(), // Minimum width
	int const p = TypeTraits< T >::precision() // Precision
)
{
	std::ostringstream t_stream;
	t_stream << std::right << std::uppercase << std::fixed << std::showpoint
	 << std::setw( w ) << std::setprecision( p ) << t;
	return t_stream.str();
}

// Right-Justified Scientific Format Fstring of a Template Argument Type Supporting Stream Output
template< typename T >
inline
Fstring
scientific_Fstring_of(
	T const & t,
	int const w = TypeTraits< T >::width(), // Minimum width
	int const p = TypeTraits< T >::precision() // Precision
)
{
	std::ostringstream t_stream;
	t_stream << std::right << std::uppercase << std::scientific << std::showpoint
	 << std::setw( w ) << std::setprecision( p ) << t;
	return t_stream.str();
}

// Conversion From Fstring

// Fstring is Readable as a Type Supporting Stream Input?
template< typename T >
inline
bool
is_type( Fstring const & s )
{
	if ( s.is_blank() ) { // Don't accept blank Fstring
		return false;
	} else { // Try to read the Fstring as a T
		std::istringstream t_stream( s.trimmed_whitespace() );
		T t;
		t_stream >> t;
		return ( ( t_stream ) && ( t_stream.eof() ) );
	}
}

// Fstring is Readable as a string Supporting Stream Input?
template<>
inline
bool
is_type< std::string >( Fstring const & )
{
	return true;
}

// Fstring is Readable as a char Supporting Stream Input?
template<>
inline
bool
is_type< char >( Fstring const & s )
{
	return ( s.size() == 1 );
}

// Fstring is Readable as a bool?
inline
bool
is_bool( Fstring const & s )
{
	return is_type< bool >( s );
}

// Fstring is Readable as a short int?
inline
bool
is_short( Fstring const & s )
{
	return is_type< short int >( s );
}

// Fstring is Readable as an int?
inline
bool
is_int( Fstring const & s )
{
	return is_type< int >( s );
}

// Fstring is Readable as a long int?
inline
bool
is_long( Fstring const & s )
{
	return is_type< long int >( s );
}

// Fstring is Readable as an unsigned short int?
inline
bool
is_ushort( Fstring const & s )
{
	return is_type< unsigned short int >( s );
}

// Fstring is Readable as an unsigned int?
inline
bool
is_uint( Fstring const & s )
{
	return is_type< unsigned int >( s );
}

// Fstring is Readable as an unsigned long int?
inline
bool
is_ulong( Fstring const & s )
{
	return is_type< unsigned long int >( s );
}

// Fstring is Readable as a float?
inline
bool
is_float( Fstring const & s )
{
	return is_type< float >( s );
}

// Fstring is Readable as a double?
inline
bool
is_double( Fstring const & s )
{
	return is_type< double >( s );
}

// Fstring is Readable as a long double?
inline
bool
is_longdouble( Fstring const & s )
{
	return is_type< long double >( s );
}

// Fstring is Readable as a char?
inline
bool
is_char( Fstring const & s )
{
	return is_type< char >( s );
}

// Fstring is Readable as a string?
inline
bool
is_string( Fstring const & )
{
	return true;
}

// Type of an Fstring for Type Supporting Stream Input
template< typename T >
inline
T
type_of( Fstring const & s )
{
	std::istringstream t_stream( s.trimmed_whitespace() );
	T t;
	t_stream >> t;
	return ( t_stream && t_stream.eof() ? t : T() ); // Check is_type first
}

// string of an Fstring
template<>
inline
std::string
type_of< std::string >( Fstring const & s )
{
	return std::string( s );
}

// char of an Fstring
template<>
inline
char
type_of< char >( Fstring const & s )
{
	return ( s.size() == 1 ? s[ 0 ] : char() ); // Check is_type first
}

// short int of an Fstring
inline
short int
short_of( Fstring const & s )
{
	return type_of< short int >( s );
}

// int of an Fstring
inline
int
int_of( Fstring const & s )
{
	return type_of< int >( s );
}

// long int of an Fstring
inline
long int
long_of( Fstring const & s )
{
	return type_of< long int >( s );
}

// unsigned short int of an Fstring
inline
unsigned short int
ushort_of( Fstring const & s )
{
	return type_of< unsigned short int >( s );
}

// unsigned int of an Fstring
inline
unsigned int
uint_of( Fstring const & s )
{
	return type_of< unsigned int >( s );
}

// unsigned long int of an Fstring
inline
unsigned long int
ulong_of( Fstring const & s )
{
	return type_of< unsigned long int >( s );
}

// float of an Fstring
inline
float
float_of( Fstring const & s )
{
	return type_of< float >( s );
}

// double of an Fstring
inline
double
double_of( Fstring const & s )
{
	return type_of< double >( s );
}

// long double of an Fstring
inline
long double
longdouble_of( Fstring const & s )
{
	return type_of< long double >( s );
}

// long double of an Fstring
inline
long double
long_double_of( Fstring const & s )
{
	return type_of< long double >( s );
}

// char of an Fstring
inline
char
char_of( Fstring const & s )
{
	return type_of< char >( s );
}

// string of an Fstring
inline
std::string
string_of( Fstring const & s )
{
	return std::string( s );
}

// Sticky Wrapped Fstring Created with One Constructor Argument
template< typename T >
inline
Sticky< Fstring >
stickyFstring( T const & t )
{
	return Sticky< Fstring >( Fstring( t ) );
}

// Sticky Wrapped Fstring Created with One Constructor Argument
template< typename T >
inline
Sticky< Fstring >
sFstring( T const & t )
{
	return Sticky< Fstring >( Fstring( t ) );
}

} // ObjexxFCL

#endif // ObjexxFCL_Fstring_hh_INCLUDED
