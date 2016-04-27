#ifndef ObjexxFCL_environment_hh_INCLUDED
#define ObjexxFCL_environment_hh_INCLUDED

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
#include <ObjexxFCL/Optional.hh>

// C++ Headers
#include <string>

namespace ObjexxFCL {

// Get Environment Variable
void
get_environment_variable( std::string const & name, Optional< std::string > value = _, Optional< int > length = _, Optional< int > status = _, Optional< bool const > trim_name = _ );

// Get Environment Variable Value
std::string
get_env_var( std::string const & name );

// Get Environment Variable Value
std::string::size_type
getenvqq( std::string const & name, std::string & value );

// Set Environment Variable Value
bool
setenvqq( std::string const & name_eq_value );

} // ObjexxFCL

#endif // ObjexxFCL_environment_hh_INCLUDED
