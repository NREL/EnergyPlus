#ifndef ObjexxFCL_command_hh_INCLUDED
#define ObjexxFCL_command_hh_INCLUDED

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
#include <ObjexxFCL/Optional.hh>

// C++ Headers
#include <string>

#ifndef _WIN32
extern int __argc;
extern char ** __argv;
#endif

namespace ObjexxFCL {

// Get Number of Command Line Arguments
int
GET_COMMAND_ARGUMENT_COUNT();

// Get Number of Command Line Arguments
int
IARGC();

// Get Number of Command Line Arguments
int
IARG();

// Get Number of Command Line Arguments
int
NUMARG();

// Get Number of Command Line Arguments Including the Command
int
NARGS();

// Get a Command Line Argument
void
GET_COMMAND(
 Optional< std::string > command = _,
 Optional< int > length = _,
 Optional< int > status = _
);

// Get a Command Line Argument
void
get_command(
 Optional< std::string > command = _,
 Optional< int > length = _,
 Optional< int > status = _
);

// Get a Command Line Argument
void
GET_COMMAND_ARGUMENT(
 int const n,
 Optional< std::string > value = _,
 Optional< int > length = _,
 Optional< int > status = _
);

// Get a Command Line Argument
void
get_command_argument(
 int const n,
 Optional< std::string > value = _,
 Optional< int > length = _,
 Optional< int > status = _
);

// Get a Command Line Argument
void
GETARG(
 int const n,
 std::string & buffer,
 Optional< int > status = _
);

// Get a Command Line Argument
void
getarg(
 int const n,
 std::string & buffer,
 Optional< int > status = _
);

} // ObjexxFCL

#endif // ObjexxFCL_command_hh_INCLUDED
