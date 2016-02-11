#ifndef ObjexxFCL_Rewind_hh_INCLUDED
#define ObjexxFCL_Rewind_hh_INCLUDED

// Rewind Support
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
#include <iosfwd>

namespace ObjexxFCL {

// Forward
class IOFlags;

// Rewind Input Stream
void
Rewind( std::istream & stream );

// Rewind Input Stream and Set Status Flags
void
Rewind( std::istream & stream, IOFlags & flags );

// Rewind Output Stream
void
Rewind( std::ostream & stream );

// Rewind Output Stream and Set Status Flags
void
Rewind( std::ostream & stream, IOFlags & flags );

// Rewind Input/Output Stream
void
Rewind( std::iostream & stream );

// Rewind Input/Output Stream and Set Status Flags
void
Rewind( std::iostream & stream, IOFlags & flags );

} // ObjexxFCL

#endif // ObjexxFCL_Rewind_hh_INCLUDED
