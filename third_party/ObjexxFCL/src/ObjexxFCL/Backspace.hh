#ifndef ObjexxFCL_Backspace_hh_INCLUDED
#define ObjexxFCL_Backspace_hh_INCLUDED

// Backspace Support
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
//  Backspace with standard C++ streams is problematic because they don't have Fortran's record-based i/o logic
//  Can't peek at ostream characters so can't backspace on ostream without a much clumsier and slower approach
//  Backspace with writes:
//   Open files in binary mode if Linux \n line terminators used
//   Fortran BACKSPACE will cause file truncation after any new WRITEs: Must add an explicit EOF to get this effect in C++
//    A full emulation of Fortran formatted sequential files could handle this but would harm migration to native C++ usage

// C++ Headers
#include <iosfwd>

namespace ObjexxFCL {

// Forward
class IOFlags;

// Backspace Input Stream
void
Backspace( std::istream & stream );

// Backspace Input Stream and Set Status Flags
void
Backspace( std::istream & stream, IOFlags & flags );

// Backspace Output Stream
void
Backspace( std::ostream & stream );

// Backspace Output Stream and Set Status Flags
void
Backspace( std::ostream & stream, IOFlags & flags );

// Backspace Input/Output Stream
void
Backspace( std::iostream & stream );

// Backspace Input/Output Stream and Set Status Flags
void
Backspace( std::iostream & stream, IOFlags & flags );

} // ObjexxFCL

#endif // ObjexxFCL_Backspace_hh_INCLUDED
