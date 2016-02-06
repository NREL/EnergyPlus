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

// ObjexxFCL Headers
#include <ObjexxFCL/Inquire.hh>
#include <ObjexxFCL/IOFlags.hh>
#include <ObjexxFCL/Stream.hh>

// C++ Headers
#include <cassert>
#include <fstream>
#include <istream>
#include <ostream>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef _WIN32
#define stat _stat
#endif

namespace ObjexxFCL {

// Size of Input Stream
inline
std::streamsize
size( std::istream const & stream )
{
	std::istream & s( const_cast< std::istream & >( stream ) );
	std::streamoff const pc( s.tellg() ); // Current position
	s.seekg( 0, std::ios::beg ); // Beginning of file
	std::streampos const pb( s.tellg() );
	s.seekg( 0, std::ios::end ); // End of file
	std::streampos const pe( s.tellg() );
	s.seekg( pc, std::ios::beg ); // Restore position
	return pe - pb;
}

// Size of Output Stream
inline
std::streamsize
size( std::ostream const & stream )
{
	std::ostream & s( const_cast< std::ostream & >( stream ) );
	std::streamoff const pc( s.tellp() ); // Current position
	s.seekp( 0, std::ios::beg ); // Beginning of file
	std::streampos const pb( s.tellp() );
	s.seekp( 0, std::ios::end ); // End of file
	std::streampos const pe( s.tellp() );
	s.seekp( pc, std::ios::beg ); // Restore position
	return pe - pb;
}

// Size of Input/Output Stream
inline
std::streamsize
size( std::iostream const & stream )
{
	return size( static_cast< std::istream const & >( stream ) );
}

// Inquire by Name
void
Inquire( std::string const & name, IOFlags & flags )
{
	flags.clear();
	flags.name( name );
	flags.exists( std::ifstream( name ).good() );
	struct stat fs;
	stat( name.c_str(), &fs );
	flags.size( fs.st_size );
}

// Inquire by Name
void
Inquire( c_cstring const name, IOFlags & flags )
{
	Inquire( std::string( name ), flags );
}

// Inquire by Stream
void
Inquire( Stream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.name( stream.name() );
	flags.exists( stream.is_open() ? true : std::ifstream( stream.name() ).good() );
	flags.open( stream.is_open() );
	flags.access( stream.access() );
	flags.action( stream.action() );
	flags.form( stream.form() );
	flags.positioning( stream.positioning() );
	if ( stream.is_open() ) {
		flags.size( stream.size() );
		flags.pos( stream.pos() );
	} else {
		struct stat fs;
		stat( stream.name().c_str(), &fs );
		flags.size( fs.st_size );
	}
}

// Inquire by istream
void
Inquire( std::istream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.exists( false );
	flags.open( true );
	flags.read_on();
	flags.size( size( stream ) );
	flags.pos( const_cast< std::istream & >( stream ).tellg() );
}

// Inquire by ostream
void
Inquire( std::ostream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.exists( false );
	flags.open( true );
	flags.write_on();
	flags.size( size( stream ) );
	flags.pos( const_cast< std::ostream & >( stream ).tellp() );
}

// Inquire by iostream
void
Inquire( std::iostream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.exists( false );
	flags.open( true );
	flags.readwrite_on();
	flags.size( size( stream ) );
	flags.pos( const_cast< std::iostream & >( stream ).tellg() );
}

// Inquire by ifstream
void
Inquire( std::ifstream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.exists( stream.is_open() );
	flags.open( stream.is_open() );
	flags.read_on();
	if ( stream.is_open() ) {
		flags.size( size( stream ) );
		flags.pos( const_cast< std::ifstream & >( stream ).tellg() );
	}
}

// Inquire by ofstream
void
Inquire( std::ofstream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.exists( stream.is_open() );
	flags.open( stream.is_open() );
	flags.write_on();
	if ( stream.is_open() ) {
		flags.size( size( stream ) );
		flags.pos( const_cast< std::ofstream & >( stream ).tellp() );
	}
}

// Inquire by fstream
void
Inquire( std::fstream const & stream, IOFlags & flags )
{
	flags.clear();
	flags.exists( stream.is_open() );
	flags.open( stream.is_open() );
	flags.readwrite_on();
	if ( stream.is_open() ) {
		flags.size( size( stream ) );
		flags.pos( const_cast< std::fstream & >( stream ).tellg() );
	}
}

} // ObjexxFCL
