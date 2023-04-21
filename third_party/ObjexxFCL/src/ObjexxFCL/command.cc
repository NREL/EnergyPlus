// Command Line Functions
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
#include <ObjexxFCL/command.hh>

// C++ Headers
#ifdef _WIN32
#include <cstdlib> // For __argc and __argv
#else
int __argc( 0 );
char ** __argv( nullptr );
#endif

namespace ObjexxFCL {

// Get Number of Command Line Arguments
int
GET_COMMAND_ARGUMENT_COUNT()
{
	return ( __argc > 0 ? __argc - 1 : 0 );
}

// Get Number of Command Line Arguments
int
IARGC()
{
	return ( __argc > 0 ? __argc - 1 : 0 );
}

// Get Number of Command Line Arguments
int
IARG()
{
	return ( __argc > 0 ? __argc - 1 : 0 );
}

// Get Number of Command Line Arguments
int
NUMARG()
{
	return ( __argc > 0 ? __argc - 1 : 0 );
}

// Get Number of Command Line Arguments Including the Command
int
NARGS()
{
	return __argc;
}

// Get a Command Line Argument
void
GET_COMMAND(
 Optional< std::string > command,
 Optional< int > length,
 Optional< int > status
)
{
	std::string c;
	for ( int i = 0; i < __argc; ++i ) { // Reconstruct the command line with single spacing
		try {
			if ( __argv[ i ] != nullptr ) {
				c += __argv[ i ];
				if ( i + 1 < __argc ) c += ' ';
			}
		} catch (...) {} // Keep going
	}
	if ( command.present() ) {
		command = c;
	}
	if ( length.present() ) {
		length = c.length(); // This doesn't account for multiple spaces between entered arguments
	}
	if ( status.present() ) {
		if ( __argc == 0 ) { // Assume command retrieval failed
			status = 1;
		} else if ( command.present() && ( command().length() < c.length() ) ) {
			status = -1;
		} else {
			status = 0;
		}
	}
}

// Get a Command Line Argument
void
get_command(
 Optional< std::string > command,
 Optional< int > length,
 Optional< int > status
)
{
	std::string c;
	for ( int i = 0; i < __argc; ++i ) { // Reconstruct the command line with single spacing
		try {
			if ( __argv[ i ] != nullptr ) {
				c += __argv[ i ];
				if ( i + 1 < __argc ) c += ' ';
			}
		} catch (...) {} // Keep going
	}
	if ( command.present() ) {
		command = c;
	}
	if ( length.present() ) {
		length = c.length(); // This doesn't account for multiple spaces between entered arguments
	}
	if ( status.present() ) {
		if ( __argc == 0 ) { // Assume command retrieval failed
			status = 1;
		} else {
			status = 0;
		}
	}
}

// Get a Command Line Argument
void
GET_COMMAND_ARGUMENT(
 int const n,
 Optional< std::string > value,
 Optional< int > length,
 Optional< int > status
)
{
	std::string a;
	if ( ( 0 <= n ) && ( n <= __argc ) ) { // Get the argument
		try {
			if ( __argv[ n ] != nullptr ) a = __argv[ n ];
		} catch (...) {} // Keep going
	}
	if ( value.present() ) {
		value = a;
	}
	if ( length.present() ) {
		length = a.length();
	}
	if ( status.present() ) {
		if ( ( n < 0 ) || ( __argc < n ) ) { // Command retrieval failed
			status = 1;
		} else if ( value.present() && ( value().length() < a.length() ) ) {
			status = -1;
		} else {
			status = 0;
		}
	}
}

// Get a Command Line Argument
void
get_command_argument(
 int const n,
 Optional< std::string > value,
 Optional< int > length,
 Optional< int > status
)
{
	std::string a;
	if ( ( 0 <= n ) && ( n <= __argc ) ) { // Get the argument
		try {
			if ( __argv[ n ] != nullptr ) a = __argv[ n ];
		} catch (...) {} // Keep going
	}
	if ( value.present() ) {
		value = a;
	}
	if ( length.present() ) {
		length = a.length();
	}
	if ( status.present() ) {
		if ( ( n < 0 ) || ( __argc < n ) ) { // Command retrieval failed
			status = 1;
		} else {
			status = 0;
		}
	}
}

// Get a Command Line Argument
void
GETARG(
 int const n,
 std::string & buffer,
 Optional< int > status
)
{
	std::string a;
	if ( ( 0 <= n ) && ( n <= __argc ) ) { // Get the argument
		try {
			if ( __argv[ n ] != nullptr ) a = __argv[ n ];
		} catch (...) {} // Keep going
	}
	buffer = a;
	if ( status.present() ) {
		if ( ( n < 0 ) || ( __argc < n ) ) { // Command retrieval failed
			status = -1;
		} else {
			status = static_cast< int >( a.length() );
		}
	}
}

// Get a Command Line Argument
void
getarg(
 int const n,
 std::string & buffer,
 Optional< int > status
)
{
	std::string a;
	if ( ( 0 <= n ) && ( n <= __argc ) ) { // Get the argument
		try {
			if ( __argv[ n ] != nullptr ) a = __argv[ n ];
		} catch (...) {} // Keep going
	}
	buffer = a;
	if ( status.present() ) {
		if ( ( n < 0 ) || ( __argc < n ) ) { // Command retrieval failed
			status = -1;
		} else {
			status = static_cast< int >( a.length() );
		}
	}
}

} // ObjexxFCL
