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

// ObjexxFCL Headers
#include <ObjexxFCL/Backspace.hh>
#include <ObjexxFCL/IOFlags.hh>

// C++ Headers
#include <istream>
#include <ostream>

namespace ObjexxFCL {

// Backspace Input Stream
void
Backspace( std::istream & stream )
{
	stream.clear();
	std::streamoff g1( stream.tellg() ); // Current position
	stream.seekg( 0, std::ios::beg ); // Beginning of file
	std::streampos const g0( stream.tellg() );
	stream.seekg( g1, std::ios::beg ); // Restore position
	if ( g1 > g0 ) --g1;
	while ( g1 > g0 ) {
		stream.seekg( --g1, std::ios::beg ); // Backup by 1
		if ( stream.peek() == '\n' ) { // Found end of previous record
			stream.seekg( ++g1, std::ios::beg );
			break;
		}
	}
}

// Backspace Input Stream and Set Status Flags
void
Backspace( std::istream & stream, IOFlags & flags )
{
	flags.clear_status();
	Backspace( stream );
	flags.set_status( stream );
}

// Backspace Output Stream
void
Backspace( std::ostream & stream )
{
	stream.clear();
	std::streamoff p1( stream.tellp() ); // Current position
	stream.seekp( 0, std::ios::beg ); // Beginning of file
	std::streampos const p0( stream.tellp() );
	stream.seekp( p1, std::ios::beg ); // Restore position
	if ( p1 > p0 ) --p1;
	stream.seekp( --p1, std::ios::beg ); // Backup by 1
}

// Backspace Output Stream and Set Status Flags
void
Backspace( std::ostream & stream, IOFlags & flags )
{
	flags.clear_status();
	Backspace( stream );
	flags.set_status( stream );
}

// Backspace Input/Output Stream
void
Backspace( std::iostream & stream )
{
	Backspace( static_cast< std::istream & >( stream ) );
	Backspace( static_cast< std::ostream & >( stream ) );
}

// Backspace Input/Output Stream and Set Status Flags
void
Backspace( std::iostream & stream, IOFlags & flags )
{
	flags.clear_status();
	Backspace( stream );
	flags.set_status( stream );
}

} // ObjexxFCL
