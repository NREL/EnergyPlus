#ifndef ObjexxFCL_Stream_hh_INCLUDED
#define ObjexxFCL_Stream_hh_INCLUDED

// Stream Wrapper Hierarchy
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
//  Don't open or close underlying file streams directly: That will break the lookup maps

// ObjexxFCL Headers
#include <ObjexxFCL/IOFlags.hh>
#include <ObjexxFCL/string.functions.hh>

// C++ Headers
#include <cassert>
#include <fstream>
#include <sstream>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef WIN32
#define stat _stat
#endif

namespace ObjexxFCL {

// Stream Wrapper Base Class
class Stream
{

public: // Types

	typedef  std::string  Name;
	typedef  std::streamsize  Size;
	typedef  std::streampos  Pos;

protected: // Creation

	// Name Constructor
	inline
	explicit
	Stream( Name name = Name(), bool const binary = false ) :
		name_( stripped_whitespace( name ) )
	{
		flags_.name( name_ );
		flags_.binary( binary );
	}

	// Name + Flags Constructor
	inline
	Stream( Name name, IOFlags flags ) :
		name_( stripped_whitespace( name ) ),
		flags_( flags )
	{
		flags_.name( name_ );
	}

public: // Creation

	// Destructor
	inline
	virtual
	~Stream()
	{}

public: // Properties

	// Name
	inline
	Name const &
	name() const
	{
		return name_;
	}

	// Old?
	inline
	bool
	old() const
	{
		return flags_.old();
	}

	// Binary?
	inline
	bool
	binary() const
	{
		return flags_.binary();
	}

	// Append?
	inline
	bool
	append() const
	{
		return flags_.append();
	}

	// AsIs?
	inline
	bool
	asis() const
	{
		return flags_.asis();
	}

	// Stream
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
	std::ios const &
	stream() const;
#else
	virtual
	std::ios const &
	stream() const = 0;
#endif

	// Stream
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
	std::ios &
	stream();
#else
	virtual
	std::ios &
	stream() = 0;
#endif

	// Read?
	virtual
	bool
	read() const = 0;

	// Write?
	virtual
	bool
	write() const = 0;

	// File Stream?
	virtual
	bool
	is_file() const = 0;

	// Open for I/O?
	virtual
	bool
	is_open() const = 0;

	// Size
	virtual
	Size
	size() const = 0;

	// Position
	virtual
	Pos
	pos() const = 0;

	// AsIs Compatible?
	inline
	bool
	asis_compatible( IOFlags const & flags ) const
	{
		return flags_.asis_compatible( flags );
	}

public: // Operators

	// Stream Conversion
	inline
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
	virtual
#endif
	operator std::ios const &() const
	{
		return stream();
	}

	// Stream Conversion
	inline
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
	virtual
#endif
	operator std::ios &()
	{
		return stream();
	}

	// Stream
	inline
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
	virtual
#endif
	std::ios const &
	operator ()() const
	{
		return stream();
	}

	// Stream
	inline
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
	virtual
#endif
	std::ios &
	operator ()()
	{
		return stream();
	}

public: // Methods

	// Open
	inline
	virtual
	bool
	open()
	{ // Default implementation
		return false;
	}

	// Rewind
	inline
	virtual
	bool
	rewind( bool const = true )
	{ // Default implementation
		return false;
	}

	// Close
	inline
	virtual
	void
	close()
	{}

	// AsIs Update
	inline
	void
	asis_update( IOFlags const & flags )
	{
		return flags_.asis_update( flags );
	}

protected: // Properties

	// Flags
	inline
	IOFlags const &
	flags() const
	{
		return flags_;
	}

	// Flags
	inline
	IOFlags &
	flags()
	{
		return flags_;
	}

private: // Data

	Name name_; // Name
	IOFlags flags_; // I/o flags

}; // Stream

// Input Stream Wrapper
class IStream : public Stream
{

public: // Creation

	// Constructor
	inline
	explicit
	IStream( std::istream & stream, Name const & name = Name() ) :
		Stream( name ),
		stream_( stream )
	{}

	// Destructor
	inline
	virtual
	~IStream()
	{}

public: // Properties

	// Stream
	inline
	std::istream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::istream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return true;
	}

	// Write?
	inline
	bool
	write() const
	{
		return false;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return true;
	}

	// Size
	inline
	Size
	size() const
	{
		std::istream & s( const_cast< std::istream & >( stream_ ) );
		std::streamoff const pc( s.tellg() ); // Current position
		s.seekg( 0, std::ios::beg ); // Beginning of file
		std::streampos const pb( s.tellg() );
		s.seekg( 0, std::ios::end ); // End of file
		std::streampos const pe( s.tellg() );
		s.seekg( pc, std::ios::beg ); // Restore position
		return pe - pb;
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::istream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::istream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::istream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::istream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::istream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		return true;
	}

private: // Data

	std::istream & stream_; // Stream

}; // IStream

// Output Stream Wrapper
class OStream : public Stream
{

public: // Creation

	// Constructor
	inline
	explicit
	OStream( std::ostream & stream, Name const & name = Name() ) :
		Stream( name ),
		stream_( stream )
	{}

	// Destructor
	inline
	virtual
	~OStream()
	{}

public: // Properties

	// Stream
	inline
	std::ostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return false;
	}

	// Write?
	inline
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return true;
	}

	// Size
	inline
	Size
	size() const
	{
		std::ostream & s( const_cast< std::ostream & >( stream_ ) );
		std::streamoff const pc( s.tellp() ); // Current position
		s.seekp( 0, std::ios::beg ); // Beginning of file
		std::streampos const pb( s.tellp() );
		s.seekp( 0, std::ios::end ); // End of file
		std::streampos const pe( s.tellp() );
		s.seekp( pc, std::ios::beg ); // Restore position
		return pe - pb;
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::ostream & >( stream_ ).tellp();
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::ostream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::ostream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::ostream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekp( 0, std::ios::beg );
		return true;
	}

private: // Data

	std::ostream & stream_; // Stream

}; // OStream

// Input/Output Stream Wrapper
class IOStream : public Stream
{

public: // Creation

	// Constructor
	inline
	explicit
	IOStream( std::iostream & stream, Name const & name = Name() ) :
		Stream( name ),
		stream_( stream )
	{}

	// Destructor
	inline
	virtual
	~IOStream()
	{}

public: // Properties

	// Stream
	inline
	std::iostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::iostream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return true;
	}

	// Write?
	inline
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return true;
	}

	// Size
	inline
	Size
	size() const
	{
		std::iostream & s( const_cast< std::iostream & >( stream_ ) );
		std::streamoff const pc( s.tellg() ); // Current position
		s.seekg( 0, std::ios::beg ); // Beginning of file
		std::streampos const pb( s.tellg() );
		s.seekg( 0, std::ios::end ); // End of file
		std::streampos const pe( s.tellg() );
		s.seekg( pc, std::ios::beg ); // Restore position
		return pe - pb;
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::iostream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::iostream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::iostream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::iostream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::iostream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		stream_.seekp( 0, std::ios::beg );
		return true;
	}

private: // Data

	std::iostream & stream_; // Stream

}; // IOStream

// Input String Stream Wrapper
class IStringStream : public Stream
{

public: // Creation

	// Constructor
	inline
	explicit
	IStringStream( std::string const & s = std::string() ) :
		stream_( s )
	{}

	// Destructor
	inline
	virtual
	~IStringStream()
	{}

public: // Properties

	// Stream
	inline
	std::istringstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::istringstream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return true;
	}

	// Write?
	inline
	bool
	write() const
	{
		return false;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return true;
	}

	// Size
	inline
	Size
	size() const
	{
		return stream_.str().size();
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::istringstream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::istringstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::istringstream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::istringstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::istringstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		return true;
	}

private: // Data

	std::istringstream stream_; // Stream

}; // IStringStream

// Output String Stream Wrapper
class OStringStream : public Stream
{

public: // Creation

	// Constructor
	inline
	explicit
	OStringStream( std::string const & s = std::string() ) :
		stream_( s )
	{}

	// Destructor
	inline
	virtual
	~OStringStream()
	{}

public: // Properties

	// Stream
	inline
	std::ostringstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostringstream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return false;
	}

	// Write?
	inline
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return true;
	}

	// Size
	inline
	Size
	size() const
	{
		return stream_.str().size();
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::ostringstream & >( stream_ ).tellp();
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::ostringstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::ostringstream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::ostringstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostringstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekp( 0, std::ios::beg );
		return true;
	}

private: // Data

	std::ostringstream stream_; // Stream

}; // OStringStream

// Input/Output Stream Wrapper
class StringStream : public Stream
{

public: // Creation

	// Constructor
	inline
	explicit
	StringStream( Name const & name = Name() ) :
		Stream( name )
	{}

	// Destructor
	inline
	virtual
	~StringStream()
	{}

public: // Properties

	// Stream
	inline
	std::stringstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::stringstream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return true;
	}

	// Write?
	inline
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return true;
	}

	// Size
	inline
	Size
	size() const
	{
		return stream_.str().size();
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::stringstream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::stringstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::stringstream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::stringstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::stringstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		stream_.seekp( 0, std::ios::beg );
		return true;
	}

private: // Data

	std::stringstream stream_; // Stream

}; // StringStream

// Input File Stream Wrapper
class IFileStream : public Stream
{

public: // Creation

	// Name Constructor
	inline
	explicit
	IFileStream( Name const & name, bool const binary = false ) :
		Stream( name, binary )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) stream_.open( name, mode() );
	}

	// Name + Flags Constructor
	inline
	IFileStream( Name const & name, IOFlags const & flags ) :
		Stream( name, flags )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) stream_.open( name, mode() );
	}

	// Destructor
	inline
	virtual
	~IFileStream()
	{
		if ( stream_.is_open() ) stream_.close();
	}

public: // Properties

	// Stream
	inline
	std::ifstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ifstream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return true;
	}

	// Write?
	inline
	bool
	write() const
	{
		return false;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return true;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return stream_.is_open();
	}

	// Size
	inline
	Size
	size() const
	{
		if ( stream_.is_open() ) {
			std::ifstream & s( const_cast< std::ifstream & >( stream_ ) );
			std::streamoff const pc( s.tellg() ); // Current position
			s.seekg( 0, std::ios::beg ); // Beginning of file
			std::streampos const pb( s.tellg() );
			s.seekg( 0, std::ios::end ); // End of file
			std::streampos const pe( s.tellg() );
			s.seekg( pc, std::ios::beg ); // Restore position
			return pe - pb;
		} else if ( ! name().empty() ) {
			struct stat fs;
			stat( name().c_str(), &fs );
			return fs.st_size;
		} else {
			return 0;
		}
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::ifstream & >( stream_ ).tellg();
	}

	// Open Mode
	inline
	std::ios_base::openmode
	mode() const
	{
		std::ios_base::openmode m( std::ios_base::in );
#ifdef OBJEXXFCL_BINARY_STREAM_OPTIONAL
		if ( binary() ) m |= std::ios_base::binary;
#else
		m |= std::ios_base::binary; // Force binary for reliable format positioning: Requires input files to have *nix \n line terminators
#endif
		if ( append() ) m |= std::ios_base::app;
		return m;
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::ifstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::ifstream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::ifstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ifstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Open
	inline
	bool
	open()
	{
		if ( stream_.is_open() ) close();
		stream_.open( name(), mode() );
		return !!stream_;
	}

	// Rewind
	inline
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		return true;
	}

	// Close
	inline
	void
	close()
	{
		if ( stream_.is_open() ) stream_.close();
		if ( ! name().empty() && ( flags().del() || flags().scratch() ) ) std::remove( name().c_str() );
	}

private: // Data

	std::ifstream stream_; // Stream

}; // IFileStream

// Output File Stream Wrapper
class OFileStream : public Stream
{

public: // Creation

	// Name Constructor
	inline
	explicit
	OFileStream( Name const & name, bool const binary = false ) :
		Stream( name, binary )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) stream_.open( name, mode() );
	}

	// Name + Flags Constructor
	inline
	OFileStream( Name const & name, IOFlags const & flags ) :
		Stream( name, flags )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) stream_.open( name, mode() );
	}

	// Destructor
	inline
	virtual
	~OFileStream()
	{
		if ( stream_.is_open() ) stream_.close();
	}

public: // Properties

	// Stream
	inline
	std::ofstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ofstream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return false;
	}

	// Write?
	inline
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return true;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return stream_.is_open();
	}

	// Size
	inline
	Size
	size() const
	{
		if ( stream_.is_open() ) {
			std::ofstream & s( const_cast< std::ofstream & >( stream_ ) );
			std::streamoff const pc( s.tellp() ); // Current position
			s.seekp( 0, std::ios::beg ); // Beginning of file
			std::streampos const pb( s.tellp() );
			s.seekp( 0, std::ios::end ); // End of file
			std::streampos const pe( s.tellp() );
			s.seekp( pc, std::ios::beg ); // Restore position
			return pe - pb;
		} else if ( ! name().empty() ) {
			struct stat fs;
			stat( name().c_str(), &fs );
			return fs.st_size;
		} else {
			return 0;
		}
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::ofstream & >( stream_ ).tellp();
	}

	// Open Mode
	inline
	std::ios_base::openmode
	mode() const
	{
		std::ios_base::openmode m( std::ios_base::out );
#ifdef OBJEXXFCL_BINARY_STREAM_OPTIONAL
		if ( binary() ) m |= std::ios_base::binary;
#else
		m |= std::ios_base::binary; // Force binary for reliable format positioning: Requires input files to have *nix \n line terminators
#endif
		if ( append() ) m |= std::ios_base::app;
		return m;
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::ofstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::ofstream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::ofstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ofstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Open
	inline
	bool
	open()
	{
		if ( stream_.is_open() ) close();
		stream_.open( name(), mode() );
		return !!stream_;
	}

	// Rewind
	inline
	bool
	rewind( bool const truncate = true )
	{
		stream_.clear();
		stream_.seekp( 0, std::ios::beg );
		if ( truncate && stream_.is_open() ) { // Fortran only truncates on WRITE after REWIND but we can't do that portably in C++
			close();
			stream_.open( name(), mode() | std::ios_base::trunc );
		}
		return true;
	}

	// Close
	inline
	void
	close()
	{
		if ( stream_.is_open() ) stream_.close();
		if ( ! name().empty() && ( flags().del() || flags().scratch() ) ) std::remove( name().c_str() );
	}

private: // Data

	std::ofstream stream_; // Stream

}; // OFileStream

// Input/Output File Stream Wrapper
class FileStream : public Stream
{

public: // Creation

	// Name Constructor
	inline
	explicit
	FileStream( Name const & name, bool const binary = false ) :
		Stream( name, binary )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) {
			stream_.open( name, mode() );
			if ( ( ! stream_.is_open() ) && ( ! std::ifstream( name ).good() ) ) { // Didn't open and doesn't exist
				stream_.clear();
				stream_.open( name, mode() | std::ios_base::trunc ); // Try as a new file
			}
		}
	}

	// Name + Flags Constructor
	inline
	FileStream( Name const & name, IOFlags const & flags ) :
		Stream( name, flags )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) {
			stream_.open( name, mode() );
			if ( ( ! stream_.is_open() ) && ( ! old() ) && ( ! std::ifstream( name ).good() ) ) { // Didn't open and doesn't exist
				stream_.clear();
				stream_.open( name, mode() | std::ios_base::trunc ); // Try as a new file
			}
		}
	}

	// Destructor
	inline
	virtual
	~FileStream()
	{
		if ( stream_.is_open() ) stream_.close();
	}

public: // Properties

	// Stream
	inline
	std::fstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::fstream &
	stream()
	{
		return stream_;
	}

	// Read?
	inline
	bool
	read() const
	{
		return true;
	}

	// Write?
	inline
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	inline
	bool
	is_file() const
	{
		return true;
	}

	// Open for I/O?
	inline
	bool
	is_open() const
	{
		return stream_.is_open();
	}

	// Size
	inline
	Size
	size() const
	{
		if ( stream_.is_open() ) {
			std::fstream & s( const_cast< std::fstream & >( stream_ ) );
			std::streamoff const pc( s.tellg() ); // Current position
			s.seekg( 0, std::ios::beg ); // Beginning of file
			std::streampos const pb( s.tellg() );
			s.seekg( 0, std::ios::end ); // End of file
			std::streampos const pe( s.tellg() );
			s.seekg( pc, std::ios::beg ); // Restore position
			return pe - pb;
		} else if ( ! name().empty() ) {
			struct stat fs;
			stat( name().c_str(), &fs );
			return fs.st_size;
		} else {
			return 0;
		}
	}

	// Position
	inline
	Pos
	pos() const
	{
		return const_cast< std::fstream & >( stream_ ).tellg();
	}

	// Open Mode
	inline
	std::ios_base::openmode
	mode() const
	{
		std::ios_base::openmode m( std::ios_base::in | std::ios_base::out );
#ifdef OBJEXXFCL_BINARY_STREAM_OPTIONAL
		if ( binary() ) m |= std::ios_base::binary;
#else
		m |= std::ios_base::binary; // Force binary for reliable format positioning: Requires input files to have *nix \n line terminators
#endif
		if ( append() ) m |= std::ios_base::app;
		return m;
	}

public: // Operators

	// Stream Conversion
	inline
	operator std::fstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	inline
	operator std::fstream &()
	{
		return stream_;
	}

	// Stream
	inline
	std::fstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	inline
	std::fstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Open
	inline
	bool
	open()
	{
		if ( stream_.is_open() ) close();
		stream_.open( name(), mode() );
		if ( ( ! stream_.is_open() ) && ( ! old() ) && ( ! std::ifstream( name() ).good() ) ) { // Didn't open and doesn't exist
			stream_.clear();
			stream_.open( name(), mode() | std::ios_base::trunc ); // Try as a new file
		}
		return !!stream_;
	}

	// Rewind
	inline
	bool
	rewind( bool const truncate = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		stream_.seekp( 0, std::ios::beg );
		if ( truncate && stream_.is_open() ) { // Fortran only truncates on WRITE after REWIND but we can't do that portably in C++
			close();
			stream_.open( name(), mode() | std::ios_base::trunc );
		}
		return true;
	}

	// Close
	inline
	void
	close()
	{
		if ( stream_.is_open() ) stream_.close();
		if ( ! name().empty() && ( flags().del() || flags().scratch() ) ) std::remove( name().c_str() );
	}

private: // Data

	std::fstream stream_; // Stream

}; // FileStream

// Methods

#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around

	// Stream
	inline
	std::ios const &
	Stream::stream() const
	{
		if ( IStream const * p = dynamic_cast< IStream const * >( this ) ) {
			return p->stream();
		} else if ( OStream const * p = dynamic_cast< OStream const * >( this ) ) {
			return p->stream();
		} else if ( IOStream const * p = dynamic_cast< IOStream const * >( this ) ) {
			return p->stream();
		} else if ( IStringStream const * p = dynamic_cast< IStringStream const * >( this ) ) {
			return p->stream();
		} else if ( OStringStream const * p = dynamic_cast< OStringStream const * >( this ) ) {
			return p->stream();
		} else if ( StringStream const * p = dynamic_cast< StringStream const * >( this ) ) {
			return p->stream();
		} else if ( IFileStream const * p = dynamic_cast< IFileStream const * >( this ) ) {
			return p->stream();
		} else if ( OFileStream const * p = dynamic_cast< OFileStream const * >( this ) ) {
			return p->stream();
		} else if ( FileStream const * p = dynamic_cast< FileStream const * >( this ) ) {
			return p->stream();
		} else {
			return dynamic_cast< FileStream const * >( this )->stream();
		}
	}

	// Stream
	inline
	std::ios &
	Stream::stream()
	{
		if ( IStream * p = dynamic_cast< IStream * >( this ) ) {
			return p->stream();
		} else if ( OStream * p = dynamic_cast< OStream * >( this ) ) {
			return p->stream();
		} else if ( IOStream * p = dynamic_cast< IOStream * >( this ) ) {
			return p->stream();
		} else if ( IStringStream * p = dynamic_cast< IStringStream * >( this ) ) {
			return p->stream();
		} else if ( OStringStream * p = dynamic_cast< OStringStream * >( this ) ) {
			return p->stream();
		} else if ( StringStream * p = dynamic_cast< StringStream * >( this ) ) {
			return p->stream();
		} else if ( IFileStream * p = dynamic_cast< IFileStream * >( this ) ) {
			return p->stream();
		} else if ( OFileStream * p = dynamic_cast< OFileStream * >( this ) ) {
			return p->stream();
		} else if ( FileStream * p = dynamic_cast< FileStream * >( this ) ) {
			return p->stream();
		} else {
			return dynamic_cast< FileStream * >( this )->stream();
		}
	}

#endif

// Functions

// Stream < Stream (Ordering)
inline
bool
operator <( Stream const & sl, Stream const & sr )
{
	return ( &sl.stream() < &sr.stream() ); // Use address of underlying streams
}

} // ObjexxFCL

#endif // ObjexxFCL_Stream_hh_INCLUDED
