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
#include <ObjexxFCL/gio_Fmt.hh> // Convenience include
#include <ObjexxFCL/Print.hh>
#include <ObjexxFCL/Read.hh>
#include <ObjexxFCL/Write.hh>

// C++ Headers
#include <ios>
#include <string>

namespace ObjexxFCL {

// Forward
class Fstring;
class IOFlags;

namespace gio {

// Types
typedef  int  Unit;
typedef  std::string  Name;
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
inline
bool
open( Unit const unit, c_cstring const name, IOFlags & flags )
{
	return open( unit, std::string( name ), flags );
}

// Open File on Specified Unit
bool
open( Unit const unit, Name const & name, std::ios_base::openmode const mode );

// Open File on Specified Unit
bool
open( Unit const unit, Fstring const & name, std::ios_base::openmode const mode );

// Open File on Specified Unit
inline
bool
open( Unit const unit, c_cstring const name, std::ios_base::openmode const mode )
{
	return open( unit, std::string( name ), mode );
}

// Open File on Specified Unit
bool
open( Unit const unit, Name const & name );

// Open File on Specified Unit
bool
open( Unit const unit, Fstring const & name );

// Open File on Specified Unit
inline
bool
open( Unit const unit, c_cstring const name )
{
	return open( unit, std::string( name ) );
}

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
inline
Unit
open( c_cstring const name, IOFlags & flags )
{
	return open( std::string( name ), flags );
}

// Open File and Return Unit
Unit
open( Name const & name, std::ios_base::openmode const mode );

// Open File and Return Unit
Unit
open( Fstring const & name, std::ios_base::openmode const mode );

// Open File and Return Unit
inline
Unit
open( c_cstring const name, std::ios_base::openmode const mode )
{
	return open( std::string( name ), mode );
}

// Open File and Return Unit
Unit
open( Name const & name );

// Open File and Return Unit
Unit
open( Fstring const & name );

// Open File and Return Unit
inline
Unit
open( c_cstring const name )
{
	return open( std::string( name ) );
}

// Open Default File and Return Unit
Unit
open( IOFlags & flags );

// Open Default File and Return Unit
Unit
open();

// Read /////

// Read from Unit
Read
read( Unit const unit, std::string const & fmt, IOFlags & flags );

// Read from Unit
Read
read( Unit const unit, gio::Fmt const & fmt, IOFlags & flags );

// Read from Unit
Read
read( Unit const unit, std::string const & fmt );

// Read from Unit
Read
read( Unit const unit, gio::Fmt const & fmt );

// Read from stdin
Read
read( std::string const & fmt, IOFlags & flags );

// Read from stdin
Read
read( gio::Fmt const & fmt, IOFlags & flags );

// Read from stdin
Read
read( std::string const & fmt );

// Read from stdin
Read
read( gio::Fmt const & fmt );

// Read from String
inline
Read
read( std::string const & str, std::string const & fmt, IOFlags & flags )
{
	return Read( str, fmt, flags );
}

// Read from String
inline
Read
read( std::string const & str, gio::Fmt const & fmt, IOFlags & flags )
{
	return Read( str, fmt, flags );
}

// Read from String
inline
Read
read( std::string const & str, std::string const & fmt )
{
	return Read( str, fmt );
}

// Read from String
inline
Read
read( std::string const & str, gio::Fmt const & fmt )
{
	return Read( str, fmt );
}

// Read Line from Unit
void
read_line( Unit const unit, IOFlags & flags, std::string & line );

// Read Line from Unit
void
read_line( Unit const unit, std::string & line );

// Write /////

// Write to Unit
Write
write( Unit const unit, std::string const & fmt, IOFlags & flags );

// Write to Unit
Write
write( Unit const unit, gio::Fmt const & fmt, IOFlags & flags );

// Write to Unit
Write
write( Unit const unit, std::string const & fmt );

// Write to Unit
Write
write( Unit const unit, gio::Fmt const & fmt );

// Write End-of-Line to Unit
void
write( Unit const unit );

// Write to stdout
Write
write( std::string const & fmt, IOFlags & flags );

// Write to stdout
Write
write( gio::Fmt const & fmt, IOFlags & flags );

// Write to stdout
Write
write( std::string const & fmt );

// Write to stdout
Write
write( gio::Fmt const & fmt );

// Write to String
inline
Write
write( std::string & str, std::string const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to String
inline
Write
write( std::string & str, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to String
inline
Write
write( std::string & str, std::string const & fmt )
{
	return Write( str, fmt );
}

// Write to String
inline
Write
write( std::string & str, gio::Fmt const & fmt )
{
	return Write( str, fmt );
}

// Write to Fstring
inline
Write
write( Fstring & str, std::string const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to Fstring
inline
Write
write( Fstring & str, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to Fstring
inline
Write
write( Fstring & str, std::string const & fmt )
{
	return Write( str, fmt );
}

// Write to Fstring
inline
Write
write( Fstring & str, gio::Fmt const & fmt )
{
	return Write( str, fmt );
}

// Print /////

// Print to stdout
Print
print( std::string const & fmt );

// Print to stdout
Print
print( Fmt const & fmt );

// Print to stdout with List-Directed Format
Print
print();

// Flush /////

// Flush
void
flush( Unit const unit );

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
