#ifndef ObjexxFCL_stream_functions_hh_INCLUDED
#define ObjexxFCL_stream_functions_hh_INCLUDED

// Stream Functions
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

// C++ Headers
#include <istream>
#include <string>

namespace ObjexxFCL {

// Is an Output Stream a File Stream?
bool
is_fstream( std::ostream const & stream );

// Read a Line from a Text Input Stream: Cross-Platform: Linux (\n) or Windows (\r\n)
std::istream &
cross_platform_get_line( std::istream & stream, std::string & line );

// Read a Line from a Text Input Stream with an Extra Delimiter: Cross-Platform: Linux (\n) or Windows (\r\n)
std::istream &
cross_platform_get_line( std::istream & stream, std::string & line, char const delim );

// Auto-Detected Line Terminator from a Text Input Stream: Cross-Platform: Linux (\n) or Windows (\r\n)
std::string
line_terminator( std::istream & stream );

} // ObjexxFCL

#endif // ObjexxFCL_stream_functions_hh_INCLUDED
