#ifndef ObjexxFCL_Stream_hh_INCLUDED
#define ObjexxFCL_Stream_hh_INCLUDED

// Stream Wrapper Hierarchy
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
#ifdef _WIN32
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
	explicit
	Stream( Name const & name = Name() ) :
	 name_( stripped_whitespace( name ) )
	{
		flags_.name( name_ );
	}

	// Name + Flags Constructor
	Stream( Name const & name, IOFlags const & flags ) :
	 name_( stripped_whitespace( name ) ),
	 flags_( flags )
	{
		flags_.name( name_ );
	}

public: // Creation

	// Destructor
	virtual
	~Stream()
	{}

public: // Properties

	// Name
	Name const &
	name() const
	{
		return name_;
	}

	// Flags
	IOFlags const &
	flags() const
	{
		return flags_;
	}

	// Status
	IOFlags::Status
	status() const
	{
		return flags_.status();
	}

	// Old?
	bool
	old() const
	{
		return flags_.old();
	}

	// Scratch?
	bool
	scratch() const
	{
		return flags_.scratch();
	}

	// Access
	IOFlags::Access
	access() const
	{
		return flags_.access();
	}

	// Action
	IOFlags::Action
	action() const
	{
		return flags_.action();
	}

	// Read?
	virtual
	bool
	read() const = 0;

	// Write?
	virtual
	bool
	write() const = 0;

	// Form
	IOFlags::Form
	form() const
	{
		return flags_.form();
	}

	// Binary?
	bool
	binary() const
	{
		return flags_.binary();
	}

	// Positioning
	IOFlags::Positioning
	positioning() const
	{
		return flags_.positioning();
	}

	// AsIs?
	bool
	asis() const
	{
		return flags_.asis();
	}

	// AsIs Compatible?
	bool
	asis_compatible( IOFlags const & flags ) const
	{
		return flags_.asis_compatible( flags );
	}

	// Append?
	bool
	append() const
	{
		return flags_.append();
	}

	// Delete?
	bool
	del() const
	{
		return flags_.del();
	}

	// Stream
	virtual
	std::ios const &
	stream() const = 0;

	// Stream
	virtual
	std::ios &
	stream() = 0;

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

	// Terminator
	std::string const &
	ter() const
	{
		return flags_.ter();
	}

public: // Operators

	// Stream Conversion
	operator std::ios const &() const
	{
		return stream();
	}

	// Stream Conversion
	operator std::ios &()
	{
		return stream();
	}

	// Stream
	std::ios const &
	operator ()() const
	{
		return stream();
	}

	// Stream
	std::ios &
	operator ()()
	{
		return stream();
	}

public: // Methods

	// Open
	virtual
	bool
	open()
	{ // Default implementation
		return false;
	}

	// Rewind
	virtual
	bool
	rewind( bool const = true )
	{ // Default implementation
		return false;
	}

	// Close
	virtual
	void
	close()
	{}

	// AsIs Update
	void
	asis_update( IOFlags const & flags )
	{
		return flags_.asis_update( flags );
	}

public: // Static Methods

	// Scratch File Name
	static
	std::string
	scratch_name();

protected: // Properties

	// Name Set
	Stream &
	name( Name const & name )
	{
		name_ = name;
		flags_.name( name );
		return *this;
	}

	// Flags
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
	explicit
	IStream( std::istream & stream, Name const & name = Name() ) :
	 Stream( name ),
	 stream_( stream )
	{}

	// Destructor
	virtual
	~IStream()
	{}

public: // Properties

	// Stream
	std::istream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::istream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return true;
	}

	// Write?
	bool
	write() const
	{
		return false;
	}

	// File Stream?
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return true;
	}

	// Size
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
	Pos
	pos() const
	{
		return const_cast< std::istream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	operator std::istream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::istream &()
	{
		return stream_;
	}

	// Stream
	std::istream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::istream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
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
	explicit
	OStream( std::ostream & stream, Name const & name = Name() ) :
	 Stream( name ),
	 stream_( stream )
	{
		flags().ter_lf();
	}

	// Destructor
	virtual
	~OStream()
	{}

public: // Properties

	// Stream
	std::ostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::ostream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return false;
	}

	// Write?
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return true;
	}

	// Size
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
	Pos
	pos() const
	{
		return const_cast< std::ostream & >( stream_ ).tellp();
	}

public: // Operators

	// Stream Conversion
	operator std::ostream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::ostream &()
	{
		return stream_;
	}

	// Stream
	std::ostream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::ostream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
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
	explicit
	IOStream( std::iostream & stream, Name const & name = Name() ) :
	 Stream( name ),
	 stream_( stream )
	{
		flags().ter_lf();
	}

	// Destructor
	virtual
	~IOStream()
	{}

public: // Properties

	// Stream
	std::iostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::iostream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return true;
	}

	// Write?
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return true;
	}

	// Size
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
	Pos
	pos() const
	{
		return const_cast< std::iostream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	operator std::iostream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::iostream &()
	{
		return stream_;
	}

	// Stream
	std::iostream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::iostream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
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
	explicit
	IStringStream( std::string const & s = std::string() ) :
	 stream_( s )
	{}

	// Destructor
	virtual
	~IStringStream()
	{}

public: // Properties

	// Stream
	std::istringstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::istringstream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return true;
	}

	// Write?
	bool
	write() const
	{
		return false;
	}

	// File Stream?
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return true;
	}

	// Size
	Size
	size() const
	{
		return stream_.str().size();
	}

	// Position
	Pos
	pos() const
	{
		return const_cast< std::istringstream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	operator std::istringstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::istringstream &()
	{
		return stream_;
	}

	// Stream
	std::istringstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::istringstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
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
	explicit
	OStringStream( std::string const & s = std::string() ) :
	 stream_( s )
	{
		flags().ter_lf();
	}

	// Destructor
	virtual
	~OStringStream()
	{}

public: // Properties

	// Stream
	std::ostringstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::ostringstream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return false;
	}

	// Write?
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return true;
	}

	// Size
	Size
	size() const
	{
		return stream_.str().size();
	}

	// Position
	Pos
	pos() const
	{
		return const_cast< std::ostringstream & >( stream_ ).tellp();
	}

public: // Operators

	// Stream Conversion
	operator std::ostringstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::ostringstream &()
	{
		return stream_;
	}

	// Stream
	std::ostringstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::ostringstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
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
	explicit
	StringStream( Name const & name = Name() ) :
	 Stream( name )
	{
		flags().ter_lf();
	}

	// Destructor
	virtual
	~StringStream()
	{}

public: // Properties

	// Stream
	std::stringstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::stringstream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return true;
	}

	// Write?
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	bool
	is_file() const
	{
		return false;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return true;
	}

	// Size
	Size
	size() const
	{
		return stream_.str().size();
	}

	// Position
	Pos
	pos() const
	{
		return const_cast< std::stringstream & >( stream_ ).tellg();
	}

public: // Operators

	// Stream Conversion
	operator std::stringstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::stringstream &()
	{
		return stream_;
	}

	// Stream
	std::stringstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::stringstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Rewind
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
	explicit
	IFileStream( Name const & name ) :
	 Stream( name )
	{
		assert( ! name.empty() );
		if ( ! name.empty() ) stream_.open( name, mode() );
	}

	// Name + Flags Constructor
	IFileStream( Name const & name, IOFlags const & flags ) :
	 Stream( name, flags )
	{
		assert( ! name.empty() );
		assert( ! flags.scratch() );
		if ( ! name.empty() ) stream_.open( name, mode() );
	}

	// Destructor
	virtual
	~IFileStream()
	{
		if ( stream_.is_open() ) stream_.close();
	}

public: // Properties

	// Stream
	std::ifstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::ifstream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return true;
	}

	// Write?
	bool
	write() const
	{
		return false;
	}

	// File Stream?
	bool
	is_file() const
	{
		return true;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return stream_.is_open();
	}

	// Size
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
	Pos
	pos() const
	{
		return const_cast< std::ifstream & >( stream_ ).tellg();
	}

	// Open Mode
	std::ios_base::openmode
	mode() const
	{
		std::ios_base::openmode m( std::ios_base::in | std::ios_base::binary ); // Binary mode for reliable format positioning and line terminator control
		if ( append() ) m |= std::ios_base::app;
		return m;
	}

public: // Operators

	// Stream Conversion
	operator std::ifstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::ifstream &()
	{
		return stream_;
	}

	// Stream
	std::ifstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::ifstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Open
	bool
	open()
	{
		if ( stream_.is_open() ) close();
		if ( ! name().empty() ) stream_.open( name(), mode() );
		return !!stream_;
	}

	// Rewind
	bool
	rewind( bool const = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		return true;
	}

	// Close
	void
	close()
	{
		if ( stream_.is_open() ) stream_.close();
		stream_.clear();
		if ( ! name().empty() && del() ) std::remove( name().c_str() );
	}

private: // Data

	std::ifstream stream_; // Stream

}; // IFileStream

// Output File Stream Wrapper
class OFileStream : public Stream
{

public: // Creation

	// Name Constructor
	explicit
	OFileStream( Name const & name ) :
	 Stream( name )
	{
		assert( ! name.empty() );
		open_file();
	}

	// Name + Flags Constructor
	OFileStream( Name const & name, IOFlags const & flags ) :
	 Stream( name, flags )
	{
		assert( ! name.empty() );
		open_file();
	}

	// Destructor
	virtual
	~OFileStream()
	{
		if ( stream_.is_open() ) stream_.close();
	}

public: // Properties

	// Stream
	std::ofstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::ofstream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return false;
	}

	// Write?
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	bool
	is_file() const
	{
		return true;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return stream_.is_open();
	}

	// Size
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
	Pos
	pos() const
	{
		return const_cast< std::ofstream & >( stream_ ).tellp();
	}

	// Open Mode
	std::ios_base::openmode
	mode() const
	{
		std::ios_base::openmode m( std::ios_base::out | std::ios_base::binary ); // Binary mode for reliable format positioning and line terminator control
		if ( scratch() ) { // Overwrite if happens to exist
			m |= std::ios_base::trunc;
		} else if ( append() ) { // Ignore append for scratch files
			m |= std::ios_base::app;
		}
		return m;
	}

public: // Operators

	// Stream Conversion
	operator std::ofstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::ofstream &()
	{
		return stream_;
	}

	// Stream
	std::ofstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::ofstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Open
	bool
	open()
	{
		if ( stream_.is_open() ) close();
		open_file();
		return !!stream_;
	}

	// Rewind
	bool
	rewind( bool const truncate = true )
	{
		stream_.clear();
		stream_.seekp( 0, std::ios::beg );
		if ( truncate && stream_.is_open() ) { // Fortran only truncates on WRITE after REWIND but we can't do that portably in C++
			stream_.close();
			stream_.clear();
			if ( ! name().empty() ) stream_.open( name(), mode() | std::ios_base::trunc );
		}
		return true;
	}

	// Close
	void
	close()
	{
		if ( stream_.is_open() ) stream_.close();
		stream_.clear();
		if ( ! name().empty() && ( del() || scratch() ) ) std::remove( name().c_str() );
	}

private: // Methods

	// Open
	void
	open_file();

private: // Data

	std::ofstream stream_; // Stream

}; // OFileStream

// Input/Output File Stream Wrapper
class FileStream : public Stream
{

public: // Creation

	// Name Constructor
	explicit
	FileStream( Name const & name ) :
	 Stream( name )
	{
		assert( ! name.empty() );
		open_file();
	}

	// Name + Flags Constructor
	FileStream( Name const & name, IOFlags const & flags ) :
	 Stream( name, flags )
	{
		assert( ! name.empty() );
		open_file();
	}

	// Destructor
	virtual
	~FileStream()
	{
		if ( stream_.is_open() ) stream_.close();
	}

public: // Properties

	// Stream
	std::fstream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	std::fstream &
	stream()
	{
		return stream_;
	}

	// Read?
	bool
	read() const
	{
		return true;
	}

	// Write?
	bool
	write() const
	{
		return true;
	}

	// File Stream?
	bool
	is_file() const
	{
		return true;
	}

	// Open for I/O?
	bool
	is_open() const
	{
		return stream_.is_open();
	}

	// Size
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
	Pos
	pos() const
	{
		return const_cast< std::fstream & >( stream_ ).tellg();
	}

	// Open Mode
	std::ios_base::openmode
	mode() const
	{
		std::ios_base::openmode m( std::ios_base::in | std::ios_base::out | std::ios_base::binary ); // Binary mode for reliable format positioning and line terminator control
		if ( scratch() ) { // Overwrite if happens to exist
			m |= std::ios_base::trunc;
		} else if ( append() ) { // Ignore append for scratch files
			m |= std::ios_base::app;
		}
		return m;
	}

public: // Operators

	// Stream Conversion
	operator std::fstream const &() const
	{
		return stream_;
	}

	// Stream Conversion
	operator std::fstream &()
	{
		return stream_;
	}

	// Stream
	std::fstream const &
	operator ()() const
	{
		return stream_;
	}

	// Stream
	std::fstream &
	operator ()()
	{
		return stream_;
	}

public: // Methods

	// Open
	bool
	open()
	{
		if ( stream_.is_open() ) close();
		open_file();
		return !!stream_;
	}

	// Rewind
	bool
	rewind( bool const truncate = true )
	{
		stream_.clear();
		stream_.seekg( 0, std::ios::beg );
		stream_.seekp( 0, std::ios::beg );
		if ( truncate && stream_.is_open() ) { // Fortran only truncates on WRITE after REWIND but we can't do that portably in C++
			stream_.close();
			stream_.clear();
			if ( ! name().empty() ) stream_.open( name(), mode() | std::ios_base::trunc );
		}
		return true;
	}

	// Close
	void
	close()
	{
		if ( stream_.is_open() ) stream_.close();
		stream_.clear();
		if ( ! name().empty() && ( del() || scratch() ) ) std::remove( name().c_str() );
	}

private: // Methods

	// Open
	void
	open_file();

private: // Data

	std::fstream stream_; // Stream

}; // FileStream

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
