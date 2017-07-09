#ifndef ObjexxFCL_environment_hh_INCLUDED
#define ObjexxFCL_environment_hh_INCLUDED

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
#include <ObjexxFCL/Optional.hh>

// C++ Headers
#include <string>

namespace ObjexxFCL {

// Get Environment Variable
void
GET_ENVIRONMENT_VARIABLE(
 std::string const & name,
 Optional< std::string > value = _,
 Optional< int > length = _,
 Optional< int > status = _,
 Optional< bool const > trim_name = _
);

// Get Environment Variable
void
get_environment_variable(
 std::string const & name,
 Optional< std::string > value = _,
 Optional< int > length = _,
 Optional< int > status = _,
 Optional< bool const > trim_name = _
);

// Get Environment Variable
void
GETENV( std::string const & name, std::string & value );

// Get Environment Variable
std::string::size_type
GETENVQQ( std::string const & name, std::string & value );

// Get Environment Variable
std::string
GET_ENV_VAR( std::string const & name );

// Set Environment Variable
bool
SETENV( std::string const & name, std::string const & value );

// Set Environment Variable
bool
SETENVQQ( std::string const & name_eq_value );

} // ObjexxFCL

#endif // ObjexxFCL_environment_hh_INCLUDED
