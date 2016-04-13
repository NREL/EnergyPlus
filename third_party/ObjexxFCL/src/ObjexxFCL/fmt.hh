#ifndef ObjexxFCL_fmt_hh_INCLUDED
#define ObjexxFCL_fmt_hh_INCLUDED

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
#include <ObjexxFCL/fmt.manipulators.hh>
#include <ObjexxFCL/string.functions.hh>
#include <ObjexxFCL/TraitsB.hh>
#include <ObjexxFCL/TraitsE.hh>
#include <ObjexxFCL/TraitsF.hh>
#include <ObjexxFCL/TraitsG.hh>
#include <ObjexxFCL/TraitsI.hh>
#include <ObjexxFCL/TraitsLD.hh>

// C++ Headers
#include <algorithm>
#include <cmath>
#include <complex>
#include <cstddef>
#include <iomanip>
#include <istream>
#include <limits>
#include <locale>
#include <sstream>
#include <string>

namespace ObjexxFCL {

// Forward
class byte;
class ubyte;

namespace fmt {

// Types
typedef  char       *    cstring;
typedef  char const *  c_cstring;
typedef  std::size_t  Size;

// Constants
static Size const NOSIZE = static_cast< Size >( -1 );

// Globals
extern fmt::Binary_num_put * binary_num_put;
extern fmt::Exponent_num_put * exponent_num_put;
extern fmt::Engineering_num_put * engineering_num_put;
extern fmt::Scientific_num_put * scientific_num_put;
extern std::locale const binary_locale;
extern std::locale const exponent_locale;
extern std::locale const engineering_locale;
extern std::locale const scientific_locale;

// Input /////

// Skip: Skips Over a Bite of Specified Width from the Input Stream
class Skip
{

public: // Creation

	// Constructor
	explicit
	Skip( Size const w = 1ul ) :
	 w_( w )
	{}

	// Destructor
	~Skip()
	{}

public: // I/O

	// Input a Skip from Stream
	friend
	std::istream &
	operator >>( std::istream & stream, Skip const & skip );

private: // Data

	Size w_; // Width

}; // Skip

// Input a Skip from Stream
std::istream &
operator >>( std::istream & stream, Skip const & skip );

// Skip Maker and Manipulator

// Skip Maker
inline
Skip
skip( Size const w = 1ul )
{
	return Skip( w );
}

// Skip Rest of Line and Line Terminator (Manipulator)
inline
std::istream &
skip( std::istream & stream )
{
	return stream.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
}

// Output /////

// Blanks
inline
std::string
X( Size const w = 1ul )
{
	return std::string( w, ' ' );
}

// Spaces
inline
std::string
space( Size const w = 1ul )
{
	return std::string( w, ' ' );
}

// char
inline
std::string
A( char const c, Size const w = 1ul )
{
	return ( w > 0ul ? std::string( w-1, ' ' ) + c : std::string() );
}

// string
std::string
A( std::string const & s, Size const w = 0ul );

// cstring
inline
std::string
A( c_cstring const s, Size const w = 0ul )
{
	return A( std::string( s ), w );
}

// Logical
template< typename T >
inline
std::string
L( T const & t, Size const w = 1ul )
{
	return ( w <= 1 ? std::string() : std::string( w-1, ' ' ) ) + std::string( 1, ( bool( t ) ? 'T' : 'F' ) );
}

// Integer
template< typename T >
inline
std::string
I( T const & t, Size w = TraitsI< T >::w, Size const m = 0ul )
{
	if ( w == NOSIZE ) w = TraitsI< T >::w;
	std::ostringstream stream;
	if ( w > 0ul ) stream << std::setw( m > 0ul ? std::min( m, w ) : w );
	if ( m > 0ul ) stream << std::setfill( '0' );
	stream << std::right << t;
	return ( w > 0ul ? lpadded( stream.str(), w ) : stream.str() );
}

// Binary
template< typename T >
inline
std::string
B( T const & t, Size w = TraitsB< T >::w, Size const m = 0ul )
{
	if ( w == NOSIZE ) w = TraitsB< T >::w;
	std::ostringstream stream;
	if ( w > 0ul ) stream << std::setw( m > 0ul ? std::min( m, w ) : w );
	if ( m > 0ul ) stream << std::setfill( '0' );
	stream.imbue( binary_locale );
	stream << t;
	return ( w > 0ul ? lpadded( stream.str(), w ) : stream.str() );
}

// Octal
template< typename T >
inline
std::string
O( T const & t, Size w = TraitsI< T >::w, Size const m = 0ul )
{
	if ( w == NOSIZE ) w = TraitsI< T >::w;
	std::ostringstream stream;
	if ( w > 0ul ) stream << std::setw( m > 0ul ? std::min( m, w ) : w );
	if ( m > 0ul ) stream << std::setfill( '0' );
	stream << std::right << std::oct << t;
	return ( w > 0ul ? lpadded( stream.str(), w ) : stream.str() );
}

// Hexidecimal
template< typename T >
inline
std::string
Z( T const & t, Size w = TraitsI< T >::w, Size const m = 0ul )
{
	if ( w == NOSIZE ) w = TraitsI< T >::w;
	std::ostringstream stream;
	if ( w > 0ul ) stream << std::setw( m > 0ul ? std::min( m, w ) : w );
	if ( m > 0ul ) stream << std::setfill( '0' );
	stream << std::right << std::uppercase << std::hex << t;
	return ( w > 0ul ? lpadded( stream.str(), w ) : stream.str() );
}

// Fixed Point
template< typename T >
inline
std::string
F( T const & t, Size w = TraitsF< T >::w, Size const d = TraitsF< T >::d, int const k = 0 )
{
	if ( w == NOSIZE ) w = TraitsF< T >::w;
	T const v( k == 0 ? t : t * std::pow( T( 10.0 ), k ) );
	std::stringstream stream;
	stream << std::showpoint << std::setprecision( d ) << std::fixed;
	if ( w > 0ul ) stream << std::setw( w );
	stream << v;
#ifdef OBJEXXFCL_FMT_NO_NEGATIVE_ZEROS
	if ( ( v < T( 0.0 ) ) && ( v >= T( -0.5 ) ) ) { // Remove sign from -0.0
		T x;
		stream >> x;
		if ( x == T( 0.0 ) ) return F( T( 0.0 ), w, d, k );
	}
#endif
	std::string v_str( stream.str() );
	std::string::size_type v_wid( v_str.length() );
	if ( ( w > 0ul ) && ( v_wid > std::string::size_type( w ) ) ) { // Exceeded field width
		if ( v_str[ 0 ] == '0' ) {
			v_str.erase( 0, 1 ); // Trim lead zero
			--v_wid;
		} else if ( v_str.substr( 0, 2 ) == "-0" ) {
			v_str.erase( 1, 1 ); // Trim lead zero
			--v_wid;
		}
	}
	if ( v_wid > std::string::size_type( w ) ) return std::string( w, '*' ); // Fortran *-fills when output is too wide
	return v_str;
}

// Fixed Point: Complex Specialization
template< typename T >
inline
std::string
F( std::complex< T > const & c, Size const w = TraitsF< T >::w, Size const d = TraitsF< T >::d, int const k = 0 )
{
	return '(' + F( c.real(), w, d, k ) + ',' + F( c.imag(), w, d, k ) + ')';
}

// Exponent
template< typename T >
inline
std::string
E( T const & t, Size w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e, int const k = 0 )
{
	if ( w == NOSIZE ) w = TraitsE< T >::w;
	if ( w == 0ul ) return std::string();
	exponent_num_put->set( d, e, k );
	std::ostringstream stream;
	stream.imbue( exponent_locale );
	stream << std::showpoint << std::uppercase << std::setw( w ) << t;
	return stream.str();
}

// Exponent: Complex Specialization
template< typename T >
inline
std::string
E( std::complex< T > const & c, Size const w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e, int const k = 0 )
{
	return '(' + E( c.real(), w, d, e, k ) + ',' + E( c.imag(), w, d, e, k ) + ')';
}

// D Exponent
template< typename T >
inline
std::string
D( T const & t, Size w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e, int const k = 0 )
{
	if ( w == NOSIZE ) w = TraitsE< T >::w;
	if ( w == 0ul ) return std::string();
	exponent_num_put->set( d, e, k, 'D' );
	std::ostringstream stream;
	stream.imbue( exponent_locale );
	stream << std::showpoint << std::uppercase << std::setw( w ) << t;
	return stream.str();
}

// D Exponent: Complex Specialization
template< typename T >
inline
std::string
D( std::complex< T > const & c, Size const w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e, int const k = 0 )
{
	return '(' + D( c.real(), w, d, e, k ) + ',' + D( c.imag(), w, d, e, k ) + ')';
}

// Engineering
template< typename T >
inline
std::string
EN( T const & t, Size w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e )
{
	if ( w == NOSIZE ) w = TraitsE< T >::w;
	if ( w == 0ul ) return std::string();
	engineering_num_put->set( d, e );
	std::ostringstream stream;
	stream.imbue( engineering_locale );
	stream << std::showpoint << std::uppercase << std::setw( w ) << t;
	return stream.str();
}

// Engineering: Complex Specialization
template< typename T >
inline
std::string
EN( std::complex< T > const & c, Size const w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e )
{
	return '(' + EN( c.real(), w, d, e ) + ',' + EN( c.imag(), w, d, e ) + ')';
}

// Scientific
template< typename T >
inline
std::string
ES( T const & t, Size w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e )
{
	if ( w == NOSIZE ) w = TraitsE< T >::w;
	if ( w == 0ul ) return std::string();
	scientific_num_put->set( d, e );
	std::ostringstream stream;
	stream.imbue( scientific_locale );
	stream << std::showpoint << std::uppercase << std::setw( w ) << t;
	return stream.str();
}

// Scientific: Complex Specialization
template< typename T >
inline
std::string
ES( std::complex< T > const & c, Size const w = TraitsE< T >::w, Size const d = TraitsE< T >::d, Size const e = TraitsE< T >::e )
{
	return '(' + ES( c.real(), w, d, e ) + ',' + ES( c.imag(), w, d, e ) + ')';
}

// General
template< typename T >
inline
std::string
G( T const & t, Size w = TraitsG< T >::w, Size const d = TraitsG< T >::d, Size const e = TraitsG< T >::e, int const k = 0 )
{
	if ( w == NOSIZE ) w = TraitsG< T >::w;
	if ( std::numeric_limits< T >::is_integer ) { // Integer
		return I( t, w );
	} else { // Treat as floating point
		T const m( std::abs( t ) );
		if ( m == T( 0.0 ) ) {
			Size const n( std::min( e + 2, w ) );
			return F( t, w - n, d - 1 ) + std::string( n, ' ' );
		} else {
			int const p( static_cast< int >( std::floor( std::log10( m ) + T( 1 ) ) ) ); // Fortran 2003 rounding modes are not supported
//			int const p( static_cast< int >( std::log10( m / ( T( 1.0 ) - ( T( 0.5 ) * std::pow( T( 10.0 ), -d ) ) ) ) + T( 1.0 ) ) ); // "Compatible" rounding mode: r = 0.5
			if ( ( 0 <= p ) && ( p <= int( d ) + 2 ) ) { // Use F editing
				Size const n( std::min( e + 2, w ) );
				return F( t, w - n, d - std::min( Size( p ), d ) ) + std::string( n, ' ' );
			} else { // Use E editing
				if ( w == 0ul ) { // Choose width
					Size const e_( TraitsG< T >::e ); // G0.dEe form not allowed in Fortran: Set exponent width based on type
					return E( t, d + e_ + 4, d, e_, k );
				} else { // Use specified width
					return E( t, w, d, e, k );
				}
			}
		}
	}
}

// General: bool Specialization
inline
std::string
G( bool const & b, Size const w = TraitsG< bool >::w, Size const = 0ul, Size const = 0ul, int const = 0 )
{
	return L( b, w );
}

// General: char Specialization
inline
std::string
G( char const & c, Size const w = TraitsG< char >::w, Size const = 0ul, Size const = 0ul, int const = 0 )
{
	return A( c, w );
}

// General: string Specialization
inline
std::string
G( std::string const & s, Size const w = TraitsG< std::string >::w, Size const = 0ul, Size const = 0ul, int const = 0 )
{
	return A( s, w );
}

// General: cstring Specialization
inline
std::string
G( c_cstring const s, Size const w = TraitsG< c_cstring >::w, Size const = 0ul, Size const = 0ul, int const = 0 )
{
	return A( s, w );
}

// General: Complex Specialization
template< typename T >
inline
std::string
G( std::complex< T > const & c, Size const w = TraitsG< T >::w, Size const d = TraitsG< T >::d, Size const e = TraitsG< T >::e, int const k = 0 )
{
	return '(' + G( c.real(), w, d, e, k ) + ',' + G( c.imag(), w, d, e, k ) + ')';
}

// List-Directed: Intel Fortran Formatting: Customize to Match Other Compilers

// List-Directed: Default Implementation
template< typename T >
inline
std::string
LD( T const & t )
{
	std::ostringstream stream;
	stream << std::left << std::noshowpoint << std::uppercase << t;
	return stream.str();
}

// List-Directed: bool Specialization
inline
std::string
LD( bool const b )
{
	return L( b, TraitsLD< bool >::w );
}

// List-Directed: byte Specialization
std::string
LD( byte const & b );

// List-Directed: ubyte Specialization
std::string
LD( ubyte const & b );

// List-Directed: short Specialization
inline
std::string
LD( short int const i )
{
	return I( i, TraitsLD< short int >::w );
}

// List-Directed: unsigned short Specialization
inline
std::string
LD( unsigned short int const i )
{
	return I( i, TraitsLD< unsigned short int >::w );
}

// List-Directed: int Specialization
inline
std::string
LD( int const i )
{
	return I( i, TraitsLD< int >::w );
}

// List-Directed: unsigned int Specialization
inline
std::string
LD( unsigned int const i )
{
	return I( i, TraitsLD< unsigned int >::w );
}

// List-Directed: long int Specialization
inline
std::string
LD( long int const i )
{
	return I( i, TraitsLD< long int >::w );
}

// List-Directed: unsigned long int Specialization
inline
std::string
LD( unsigned long int const i )
{
	return I( i, TraitsLD< unsigned long int >::w );
}

// List-Directed: float Specialization
inline
std::string
LD( float const v )
{
	typedef  TraitsLD< float >  Tr;
	return G( v, Tr::w, Tr::d, Tr::e, 1 );
}

// List-Directed: double Specialization
inline
std::string
LD( double const v )
{
	typedef  TraitsLD< double >  Tr;
	return G( v, Tr::w, Tr::d, Tr::e, 1 );
}

// List-Directed: long double Specialization
inline
std::string
LD( long double const v )
{
	typedef  TraitsLD< long double >  Tr;
	return G( v, Tr::w, Tr::d, Tr::e, 1 );
}

// List-Directed: complex< float > Specialization
inline
std::string
LD( std::complex< float > const & c )
{
	typedef  TraitsLD< std::complex< float > >  Tr;
	return '(' + stripped( G( c.real(), Tr::w, Tr::d, Tr::e, 1 ) ) + ',' + stripped( G( c.imag(),  Tr::w, Tr::d, Tr::e, 1 ) ) + ')';
}

// List-Directed: complex< double > Specialization
inline
std::string
LD( std::complex< double > const & c )
{
	typedef  TraitsLD< std::complex< double > >  Tr;
	return '(' + stripped( G( c.real(), Tr::w, Tr::d, Tr::e, 1 ) ) + ',' + stripped( G( c.imag(),  Tr::w, Tr::d, Tr::e, 1 ) ) + ')';
}

// List-Directed: complex< long double > Specialization
inline
std::string
LD( std::complex< long double > const & c )
{
	typedef  TraitsLD< std::complex< long double > >  Tr;
	return '(' + stripped( G( c.real(), Tr::w, Tr::d, Tr::e, 1 ) ) + ',' + stripped( G( c.imag(),  Tr::w, Tr::d, Tr::e, 1 ) ) + ')';
}

// char
inline
std::string
LD( char const c )
{
	return std::string( 1, c );
}

// List-Directed: string Specialization
inline
std::string
LD( c_cstring const s )
{
	return std::string( s );
}

// List-Directed: string Specialization
inline
std::string
LD( std::string const & s )
{
	return s;
}

// Extras /////

// Left-Justified
template< typename T >
inline
std::string
LJ( int const w, T const & t )
{
	std::ostringstream stream;
	stream << std::left << std::setw( w ) << t;
	return stream.str();
}

// Right-Justified
template< typename T >
inline
std::string
RJ( int const w, T const & t )
{
	std::ostringstream stream;
	stream << std::right << std::setw( w ) << t;
	return stream.str();
}

// Manipulators /////

// general: Manipulator to Turn Off scientific or fixed
inline
std::ios_base &
general( std::ios_base & base )
{
	base.unsetf( std::ios_base::fixed );
	base.unsetf( std::ios_base::scientific );
	return base;
}

// Utility /////

// Newline utility for formatted output implied DO loop emulation
inline
std::string
nl_if( int const i, int const n )
{
	if ( ( i > 1 ) && ( ( i - 1 ) % n == 0 ) ) {
		return "\n";
	} else {
		return "";
	}
}

} // fmt
} // ObjexxFCL

#endif // ObjexxFCL_fmt_hh_INCLUDED
