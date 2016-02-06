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

// ObjexxFCL Headers
#include <ObjexxFCL/stream.functions.hh>

// C++ Headers
#include <fstream>

namespace ObjexxFCL {

// Is an Output Stream a File Stream?
bool
is_fstream( std::ostream const & stream )
{
	return ( ( dynamic_cast< std::fstream const * >( &stream ) != nullptr ) || ( dynamic_cast< std::ofstream const * >( &stream ) != nullptr ) );
}

// Read a Line from a Text Input Stream: Cross-Platform: Linux (\n) or Windows (\r\n)
std::istream &
cross_platform_get_line( std::istream & stream, std::string & line )
{
#ifdef _MSC_VER // Fast algorithm for Visual C++ and Intel C++ on Windows
	if ( stream.eof() ) {
		stream.setstate( std::ios::failbit );
		return stream;
	}
	std::istream::sentry local_sentry( stream, true );
	std::streambuf * stream_buffer( stream.rdbuf() );
	int c;
	line.clear();
	while ( true ) {
		c = stream_buffer->sbumpc();
		switch ( c ) {
		case EOF:
			if ( line.empty() ) stream.setstate( std::ios::eofbit ); // Nothing read: Set eof bit
			return stream;
		case '\n':
			return stream;
		case '\r':
			if ( stream_buffer->sgetc() == '\n' ) {
				stream_buffer->sbumpc();
				return stream;
			}
			// Flow into default
		default:
			line += static_cast< char >( c );
		}
	}
#else // Fast algorithm for GCC and Clang
	if ( std::getline( stream, line ) && ( ! line.empty() ) ) {
		std::string::size_type const len( line.length() - 1 );
		if ( line[ len ] == '\r' ) {
			line.erase( len );
			if ( ! line.empty() ) stream.clear( stream.rdstate() & ~std::ios::eofbit ); // Clear eof bit since we read something
		} else {
			stream.clear( stream.rdstate() & ~std::ios::eofbit ); // Clear eof bit since we read something
		}
	}
	return stream;
#endif
}

// Read a Line from a Text Input Stream with an Extra Delimiter: Cross-Platform: Linux (\n) or Windows (\r\n)
std::istream &
cross_platform_get_line( std::istream & stream, std::string & line, char const delim )
{
	if ( stream.eof() ) {
		stream.setstate( std::ios::failbit );
		return stream;
	}
	std::istream::sentry local_sentry( stream, true );
	std::streambuf * stream_buffer( stream.rdbuf() );
	int c;
	bool saving( true ); // Saving characters (didn't hit delimiter)?
	line.clear();
	while ( true ) {
		c = stream_buffer->sbumpc();
		switch ( c ) {
		case EOF:
			if ( line.empty() ) stream.setstate( std::ios::eofbit ); // Nothing read: Set eof bit
			return stream;
		case '\n':
			return stream;
		case '\r':
			if ( stream_buffer->sgetc() == '\n' ) {
				stream_buffer->sbumpc();
				return stream;
			}
			// Flow into default
		default:
			if ( saving ) {
				if ( c == delim ) {
					saving = false;
				} else {
					line += static_cast< char >( c );
				}
			}
		}
	}
}

// Auto-Detected Line Terminator from a Text Input Stream: Cross-Platform: Linux (\n) or Windows (\r\n)
std::string
line_terminator( std::istream & stream )
{ // Assumes stream was opened in binary mode: C++ doesn't let us check that
	std::streamoff const start_pos( stream.tellg() ); // Current position
	stream.clear();
	stream.seekg( 0, std::ios::beg ); // Jump to beginning
	std::string line;
	std::string terminator;
	if ( std::getline( stream, line ) ) {
		if ( line.empty() ) {
			terminator = "\n";
		} else {
			terminator = ( line.back() == '\r' ? "\r\n" : "\n" );
		}
	} else { // No terminators in file
		// Terminator stays empty
	}
	stream.seekg( start_pos, std::ios::beg ); // Jump back to starting position
	stream.clear();
	return terminator;
}

} // ObjexxFCL
