// Fortran-Compatible Formatted Input/Output Support
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
#include <ObjexxFCL/fmt.hh>
#include <ObjexxFCL/byte.hh>
#include <ObjexxFCL/ubyte.hh>
#include <ObjexxFCL/char.functions.hh>

namespace ObjexxFCL {
namespace fmt {

// Globals
fmt::Binary_num_put * binary_num_put( new fmt::Binary_num_put );
fmt::Exponent_num_put * exponent_num_put( new fmt::Exponent_num_put );
fmt::Engineering_num_put * engineering_num_put( new fmt::Engineering_num_put );
fmt::Scientific_num_put * scientific_num_put( new fmt::Scientific_num_put );
std::locale const binary_locale( std::locale(), binary_num_put );
std::locale const exponent_locale( std::locale(), exponent_num_put );
std::locale const engineering_locale( std::locale(), engineering_num_put );
std::locale const scientific_locale( std::locale(), scientific_num_put );

// Input /////

// Input a Skip from Stream
std::istream &
operator >>( std::istream & stream, Skip const & skip )
{
	static std::string const STOPPERS( "\r\n" + std::string( 1, std::istream::traits_type::eof() ) );
	char c;
	Size i( 0 );
	while ( ( i < skip.w_ ) && stream && not_any_of( stream.peek(), STOPPERS ) ) {
		stream.get( c );
		++i;
	}
	// Don't clear eof bit since specified width read
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
		return std::string( w - l, ' ' ) + s;
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

} // fmt
} // ObjexxFCL
