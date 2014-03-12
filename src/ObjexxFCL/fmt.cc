// Fortran-Compatible Formatted Input/Output Support
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
#include <ObjexxFCL/fmt.hh>
#include <ObjexxFCL/byte.hh>
#include <ObjexxFCL/ubyte.hh>
#include <ObjexxFCL/char.functions.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/string.constants.hh>

namespace ObjexxFCL {
namespace fmt {

// Input /////

// Input a Skip from Stream
std::istream &
operator >>( std::istream & stream, Skip const & skip )
{
	char c;
	Size i( 0 );
	while ( ( i < skip.w_ ) && stream && not_any_of( stream.peek(), READ_STOPPERS ) ) {
		stream.get( c );
		++i;
	}
	return stream;
}

// Output /////

// string
std::string
A( std::string const & s, Size const w )
{
	std::string::size_type const l( s.length() );
	if ( w == 0ul ) {
		return s;
	} else if ( l > w ) { // Trim
		return s.substr( 0, w );
	} else if ( l == w ) {
		return s;
	} else { // l < w: Pad
		return std::string( w - l, SPC ) + s;
	}
}

// Fstring Format
std::string
A( Fstring const & s, Size const w )
{
	std::string::size_type const l( s.length() );
	if ( w == 0ul ) {
		return s;
	} else if ( l > w ) { // Trim
		return s( 1, w );
	} else if ( l == w ) {
		return s;
	} else { // l < w: Pad
		return std::string( w - l, SPC ) + s;
	}
}

// List-Directed: byte Specialization
std::string
LD( byte const & b )
{
	return I( static_cast< short int >( b ), 5 );
}

// List-Directed: ubyte Specialization
std::string
LD( ubyte const & b )
{
	return I( static_cast< short int >( b ), 5 );
}

// List-Directed: Fstring Specialization
std::string
LD( Fstring const & s )
{
	return s.str();
}

} // fmt
} // ObjexxFCL
