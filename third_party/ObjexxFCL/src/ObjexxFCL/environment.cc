// System Environment Functions
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
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/string.functions.hh>

// C++ Headers
#include <cstdlib>

namespace ObjexxFCL {

// Get Environment Variable
void
get_environment_variable( std::string const & name, Optional< std::string > value, Optional< int > length, Optional< int > status, Optional< bool const > trim_name )
{
	std::string val;
	char const * const cval( getenv( name.c_str() ) );
	if ( cval ) val = cval;
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

// Get Environment Variable Value
std::string
get_env_var( std::string const & name )
{
	char const * const cval( getenv( name.c_str() ) );
	return ( cval != nullptr ? cval : "" );
}

// Get Environment Variable Value
std::string::size_type
getenvqq( std::string const & name, std::string & value )
{
	char const * const cval( getenv( name.c_str() ) );
	value = ( cval != nullptr ? cval : "" );
	return value.length();
}

// Set Environment Variable Value
bool
setenvqq( std::string const & name_eq_value )
{
#ifdef OBJEXXFCL_NO_PUTENV
	return false;
#else
#ifdef __GNUC__
	return ( putenv( const_cast< char * >( name_eq_value.c_str() ) ) == 0 ? true : false ); // Hack for non-const interface: Should really copy the string or use setenv
#else
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)
	return ( _putenv( name_eq_value.c_str() ) == 0 ? true : false );
#else
	return ( putenv( name_eq_value.c_str() ) == 0 ? true : false ); // Not standard but widely supported
#endif
#endif
#endif
}

} // ObjexxFCL
