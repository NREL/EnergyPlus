#ifndef ObjexxFCL_gio_hh_INCLUDED
#define ObjexxFCL_gio_hh_INCLUDED

// Global I/O Support
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2014 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// Notes:
//  String read/write are non-global convenience functions

// ObjexxFCL Headers
#include <ObjexxFCL/IOFlags.fwd.hh>
#include <ObjexxFCL/Fstring.fwd.hh>
#include <ObjexxFCL/Print.hh>
#include <ObjexxFCL/Read.hh>
#include <ObjexxFCL/Write.hh>

// C++ Headers
#include <ios>
#include <string>

namespace ObjexxFCL {
namespace gio {

// Types
typedef  int  Unit;
typedef  std::string  Name;
typedef  std::string  Fmt;
typedef  char const *  c_cstring;

// Unit /////

int
get_unit();

// Open /////

// Open File on Specified Unit
bool
open( Unit const unit, Name const & name, IOFlags & flags );

// Open File on Specified Unit
bool
open( Unit const unit, Fstring const & name, IOFlags & flags );

// Open File on Specified Unit
bool
open( Unit const unit, c_cstring const name, IOFlags & flags );

// Open File on Specified Unit
bool
open( Unit const unit, Name const & name, std::ios_base::openmode const mode );

// Open File on Specified Unit
bool
open( Unit const unit, Fstring const & name, std::ios_base::openmode const mode );

// Open File on Specified Unit
bool
open( Unit const unit, c_cstring const name, std::ios_base::openmode const mode );

// Open File on Specified Unit
bool
open( Unit const unit, Name const & name );

// Open File on Specified Unit
bool
open( Unit const unit, Fstring const & name );

// Open File on Specified Unit
bool
open( Unit const unit, c_cstring const name );

// Open Default File on Specified Unit
bool
open( Unit const unit, IOFlags & flags );

// Open Default File on Specified Unit
bool
open( Unit const unit, std::ios_base::openmode const mode );

// Open Default File on Specified Unit
bool
open( Unit const unit );

// Open File and Return Unit
Unit
open( Name const & name, IOFlags & flags );

// Open File and Return Unit
Unit
open( Fstring const & name, IOFlags & flags );

// Open File and Return Unit
Unit
open( c_cstring const name, IOFlags & flags );

// Open File and Return Unit
Unit
open( Name const & name, std::ios_base::openmode const mode );

// Open File and Return Unit
Unit
open( Fstring const & name, std::ios_base::openmode const mode );

// Open File and Return Unit
Unit
open( c_cstring const name, std::ios_base::openmode const mode );

// Open File and Return Unit
Unit
open( Name const & name );

// Open File and Return Unit
Unit
open( Fstring const & name );

// Open File and Return Unit
Unit
open( c_cstring const name );

// Open Default File and Return Unit
Unit
open( IOFlags & flags );

// Open Default File and Return Unit
Unit
open();

// Read /////

// Read from Unit
Read
read( Unit const unit, Fmt const & fmt, IOFlags & flags );

// Read from Unit
Read
read( Unit const unit, Fmt const & fmt );

// Read from stdin
Read
read( Fmt const & fmt, IOFlags & flags );

// Read from stdin
Read
read( Fmt const & fmt );

// Read from String
Read
read( std::string const & str, Fmt const & fmt, IOFlags & flags );

// Read from String
Read
read( std::string const & str, Fmt const & fmt );

// Write /////

// Write to Unit
Write
write( Unit const unit, Fmt const & fmt, IOFlags & flags );

// Write to Unit
Write
write( Unit const unit, Fmt const & fmt );

// Write End-of-Line to Unit
void
write( Unit const unit );

// Write to stdout
Write
write( Fmt const & fmt, IOFlags & flags );

// Write to stdout
Write
write( Fmt const & fmt );

// Write to String
Write
write( std::string & str, Fmt const & fmt, IOFlags & flags );

// Write to String
Write
write( std::string & str, Fmt const & fmt );

// Write to Fstring
Write
write( Fstring & str, Fmt const & fmt, IOFlags & flags );

// Write to Fstring
Write
write( Fstring & str, Fmt const & fmt );

// Print /////

// Print to stdout
Print
print( Fmt const & fmt );

// Print to stdout with List-Directed Format
Print
print();

// Inquire /////

// Inquire by Unit
void
inquire( Unit const unit, IOFlags & flags );

// Inquire by Name
void
inquire( Name const & name, IOFlags & flags );

// Inquire by Name
void
inquire( Fstring const & name, IOFlags & flags );

// Inquire by Name
void
inquire( c_cstring const name, IOFlags & flags );

// Backspace /////

// Backspace
void
backspace( Unit const unit, IOFlags & flags );

// Backspace
void
backspace( Unit const unit );

// Rewind /////

// Rewind
void
rewind( Unit const unit, IOFlags & flags );

// Rewind
void
rewind( Unit const unit );

// Rewind and Truncate
void
rewind_truncate( Unit const unit );

// Close /////

// Close File
void
close( Unit const unit );

// Close File
void
close( Unit const unit, IOFlags & flags );

} // gio
} // ObjexxFCL

#endif // ObjexxFCL_gio_hh_INCLUDED
