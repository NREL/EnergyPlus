// System Environment Functions
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
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/string.functions.hh>

// C++ Headers
#include <cstdlib>

namespace ObjexxFCL {

// Get Environment Variable
void
GET_ENVIRONMENT_VARIABLE(
 std::string const & name,
 Optional< std::string > value,
 Optional< int > length,
 Optional< int > status,
 Optional< bool const > trim_name
)
{
	char const * const cval( std::getenv( name.c_str() ) );
	std::string val( cval != nullptr ? cval : "" );
	if ( ( ! trim_name.present() ) || ( trim_name() ) ) rstrip( val ); // Strip any trailing spaces
	if ( value.present() ) value = val;
	if ( length.present() ) length = static_cast< int >( val.length() );
	if ( status.present() ) {
		if ( cval == nullptr ) { // Env var does not exist
			status = 1;
		} else { // Env var exists
			if ( value.present() ) {
				status = ( value().length() >= val.length() ? 0 : -1 );
			} else {
				status = 0;
			}
		}
	}
}

// Get Environment Variable
void
get_environment_variable(
 std::string const & name,
 Optional< std::string > value,
 Optional< int > length,
 Optional< int > status,
 Optional< bool const > trim_name
)
{
	char const * const cval( std::getenv( name.c_str() ) );
	std::string val( cval != nullptr ? cval : "" );
	if ( ( ! trim_name.present() ) || ( trim_name() ) ) rstrip( val ); // Strip any trailing spaces
	if ( value.present() ) value = val;
	if ( length.present() ) length = static_cast< int >( val.length() );
	if ( status.present() ) {
		if ( cval == nullptr ) { // Env var does not exist
			status = 1;
		} else { // Env var exists
			if ( value.present() ) {
				status = ( value().length() >= val.length() ? 0 : -1 );
			} else {
				status = 0;
			}
		}
	}
}

// Get Environment Variable
void
GETENV( std::string const & name, std::string & value )
{
	char const * const cval( std::getenv( name.c_str() ) );
	value = ( cval != nullptr ? cval : "" );
}

// Get Environment Variable
std::string::size_type
GETENVQQ( std::string const & name, std::string & value )
{
	char const * const cval( std::getenv( name.c_str() ) );
	value = ( cval != nullptr ? cval : "" );
	return value.length();
}

// Get Environment Variable
std::string
GET_ENV_VAR( std::string const & name )
{
	char const * const cval( std::getenv( name.c_str() ) );
	return std::string( cval != nullptr ? cval : "" );
}

// Set Environment Variable
bool
SETENV( std::string const & name, std::string const & value )
{
#ifdef OBJEXXFCL_NO_PUTENV
	return false;
#elif defined(_MSC_VER) && !defined(__INTEL_COMPILER)
	return ( _putenv_s( name.c_str(), value.c_str() ) == 0 );
#elif defined(__GNUC__) && !defined(_WIN32)
	return ( setenv( name.c_str(), value.c_str(), 1 ) == 0 );
#else
	std::string const name_eq_value( name + '=' + value );
	return ( putenv( name_eq_value.c_str() ) == 0 ); // Not standard but widely supported
#endif
}

// Split Name=Value String into Name and Value
void
split_name_eq_value( std::string const & name_eq_value, std::string & name, std::string & value )
{
	name.clear();
	value.clear();
	if ( name_eq_value.empty() ) return;
	std::string::size_type const l( name_eq_value.length() );
	std::string::size_type i( 0 );
	while ( ( i < l ) && ( name_eq_value[ i ] != '=' ) ) {
		name += name_eq_value[ i ];
		++i;
	}
	if ( ( i < l ) && ( name_eq_value[ i ] == '=' ) ) ++i;
	while ( i < l ) {
		value += name_eq_value[ i ];
		++i;
	}
}

// Set Environment Variable
bool
SETENVQQ( std::string const & name_eq_value )
{
#ifdef OBJEXXFCL_NO_PUTENV
	return false;
#elif defined(_MSC_VER) && !defined(__INTEL_COMPILER)
	return ( _putenv( name_eq_value.c_str() ) == 0 );
#elif defined(__GNUC__) && !defined(_WIN32)
	std::string name, value;
	split_name_eq_value( name_eq_value, name, value );
	return ( setenv( name.c_str(), value.c_str(), 1 ) == 0 );
#else
	return ( putenv( name_eq_value.c_str() ) == 0 ); // Not standard but widely supported
#endif
}

} // ObjexxFCL
