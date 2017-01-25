#ifndef ObjexxFCL_Inquire_hh_INCLUDED
#define ObjexxFCL_Inquire_hh_INCLUDED

// Inquire Support
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

// Notes:
//  Supports some common Fortran INQUIRE capabilities

// C++ Headers
#include <cassert>
#include <iosfwd>
#include <string>

namespace ObjexxFCL {

// Forward
class IOFlags;
class Stream;
class IStream;
class OStream;
class IStringStream;
class OStringStream;
class StringStream;
class IFileStream;
class OFileStream;
class FileStream;

// Types
typedef  char const *  c_cstring;

// Inquire by Name
void
Inquire( std::string const & name, IOFlags & flags );

// Inquire by Name
void
Inquire( c_cstring const name, IOFlags & flags );

// Inquire by Stream
void
Inquire( Stream const & stream, IOFlags & flags );

// Inquire by istream
void
Inquire( std::istream const & stream, IOFlags & flags );

// Inquire by ostream
void
Inquire( std::ostream const & stream, IOFlags & flags );

// Inquire by iostream
void
Inquire( std::iostream const & stream, IOFlags & flags );

// Inquire by ifstream
void
Inquire( std::ifstream const & stream, IOFlags & flags );

// Inquire by ofstream
void
Inquire( std::ofstream const & stream, IOFlags & flags );

// Inquire by fstream
void
Inquire( std::fstream const & stream, IOFlags & flags );

} // ObjexxFCL

#endif // ObjexxFCL_Inquire_hh_INCLUDED
