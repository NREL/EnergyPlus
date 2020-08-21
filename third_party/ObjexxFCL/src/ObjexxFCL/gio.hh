#ifndef ObjexxFCL_gio_hh_INCLUDED
#define ObjexxFCL_gio_hh_INCLUDED

// Global I/O Support
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

// Notes:
//  String read/write are non-global convenience functions

// ObjexxFCL Headers
#include <ObjexxFCL/gio_Fmt.hh> // Convenience include
#include <ObjexxFCL/Read.hh>

// C++ Headers
#include <ios>
#include <string>

namespace ObjexxFCL {

// Forward
class IOFlags;

namespace gio {

// Read /////

// Read from String
inline
ReadString
read( std::string const & str, std::string const & fmt )
{
	return ReadString( str, fmt );
}

// Read from String
inline
ReadString
read( std::string const & str, Fmt & fmt )
{
	return ReadString( str, fmt );
}

// Read from String
inline
ReadString
read( std::string const & str, std::string const & fmt, IOFlags & flags )
{
	return ReadString( str, fmt, flags );
}

// Read from String
inline
ReadString
read( std::string const & str, Fmt & fmt, IOFlags & flags )
{
	return ReadString( str, fmt, flags );
}


} // gio
} // ObjexxFCL

#endif // ObjexxFCL_gio_hh_INCLUDED
