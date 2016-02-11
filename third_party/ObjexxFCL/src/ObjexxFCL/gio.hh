#ifndef ObjexxFCL_gio_hh_INCLUDED
#define ObjexxFCL_gio_hh_INCLUDED

// Global I/O Support
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
class IOFlags;

namespace gio {

// Types
typedef  int  Unit;
typedef  std::string  Name;
typedef  char const *  c_cstring;

// Data
extern std::string const LF; // Linefeed

// Unit /////

int
get_unit();

// Open /////

// Open File on Specified Unit
bool
open( Unit const unit, Name const & name, IOFlags & flags );

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
ReadStream
read( Unit const unit, std::string const & fmt, bool const beg = false );

// Read from Unit
ReadStream
read( Unit const unit, Fmt const & fmt, bool const beg = false );

// Read from Unit
ReadStream
read( Unit const unit, Fmt & fmt, bool const beg = false );

// Read from Unit
ReadStream
read( Unit const unit, std::string const & fmt, IOFlags & flags, bool const beg = false );

// Read from Unit
ReadStream
read( Unit const unit, Fmt const & fmt, IOFlags & flags, bool const beg = false );

// Read from Unit
ReadStream
read( Unit const unit, Fmt & fmt, IOFlags & flags, bool const beg = false );

// Read from stdin
ReadStream
read( std::string const & fmt );

// Read from stdin
ReadStream
read( Fmt const & fmt );

// Read from stdin
ReadStream
read( Fmt & fmt );

// Read from stdin
ReadStream
read( std::string const & fmt, IOFlags & flags );

// Read from stdin
ReadStream
read( Fmt const & fmt, IOFlags & flags );

// Read from stdin
ReadStream
read( Fmt & fmt, IOFlags & flags );

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
read( std::string const & str, Fmt const & fmt )
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
read( std::string const & str, Fmt const & fmt, IOFlags & flags )
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

// Read Line from Unit
void
read_line( Unit const unit, std::string & line );

// Read Line from Unit
void
read_line( Unit const unit, IOFlags & flags, std::string & line );

// Input Stream of Unit
std::istream *
inp_stream( Unit const unit );

// Write /////

// Write to Unit
Write
write( Unit const unit, std::string const & fmt );

// Write to Unit
Write
write( Unit const unit, Fmt const & fmt );

// Write to Unit
Write
write( Unit const unit, Fmt & fmt );

// Write to Unit
Write
write( Unit const unit, std::string const & fmt, IOFlags & flags );

// Write to Unit
Write
write( Unit const unit, Fmt const & fmt, IOFlags & flags );

// Write to Unit
Write
write( Unit const unit, Fmt & fmt, IOFlags & flags );

// Write End-of-Line to Unit
void
write( Unit const unit );

// Write to stdout
Write
write( std::string const & fmt );

// Write to stdout
Write
write( Fmt const & fmt );

// Write to stdout
Write
write( Fmt & fmt );

// Write to stdout
Write
write( std::string const & fmt, IOFlags & flags );

// Write to stdout
Write
write( Fmt const & fmt, IOFlags & flags );

// Write to stdout
Write
write( Fmt & fmt, IOFlags & flags );

// Write End-of-Line to stdout
inline
void
write()
{
	std::cout << LF;
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
write( std::string & str, Fmt const & fmt )
{
	return Write( str, fmt );
}

// Write to String
inline
Write
write( std::string & str, Fmt & fmt )
{
	return Write( str, fmt );
}

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
write( std::string & str, Fmt const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to String
inline
Write
write( std::string & str, Fmt & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Output Stream of Unit
std::ostream *
out_stream( Unit const unit );

// Print /////

// Print to stdout
Print
print( std::string const & fmt );

// Print to stdout
Print
print( Fmt const & fmt );

// Print to stdout
Print
print( Fmt & fmt );

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
inquire( c_cstring const name, IOFlags & flags );

// File Exists?
bool
file_exists( std::string const & file_name );

// File Exists?
bool
file_exists( c_cstring const file_name );

// File Openable?
bool
file_openable( std::string const & file_name );

// File Openable?
bool
file_openable( c_cstring const file_name );

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
