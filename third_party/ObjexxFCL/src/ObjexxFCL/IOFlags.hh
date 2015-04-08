#ifndef ObjexxFCL_IOFlags_hh_INCLUDED
#define ObjexxFCL_IOFlags_hh_INCLUDED

// IOFlags: I/O Control and Status Flags
//
// Project: Objexx Fortran Compatibility Library (ObjexxFCL)
//
// Version: 4.0.0
//
// Language: C++
//
// Copyright (c) 2000-2015 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// C++ Headers
#include <cstdlib>
#include <ios>
#include <string>

namespace ObjexxFCL {

// IOFlags: I/O Control and Status Flags
class IOFlags
{

public: // Types

	typedef  std::string  Msg;
	typedef  std::string  Name;
	typedef  std::streamsize  Size;
	typedef  std::streampos  Pos;

public: // Creation

	// Default Constructor
	inline
	IOFlags() :
	 unit_( 0 ),
	 exists_( false ),
	 new_( false ),
	 old_( false ),
	 scratch_( false ),
	 unknown_( false ),
	 open_( false ),
	 read_( false ),
	 write_( false ),
	 binary_( false ),
	 append_( false ),
	 truncate_( false ),
	 asis_( true ),
	 bz_( false ),
	 nad_( false ),
	 del_( false ),
	 her_( false ),
	 size_( 0u ),
	 pos_( 0 ),
	 ter_( default_ter() ),
	 err_( false ),
	 end_( false ),
	 eor_( false ),
	 ios_( 0 )
	{}

	// Handle Errors Named Constructor
	inline
	static
	IOFlags
	handler()
	{
		IOFlags flags;
		flags.her_on();
		return flags;
	}

	// Handle Errors with Line Terminator Named Constructor
	inline
	static
	IOFlags
	handler( std::string const & ter )
	{
		IOFlags flags;
		flags.her_on();
		flags.ter( ter );
		return flags;
	}

public: // Properties

	// Unit
	inline
	int
	unit() const
	{
		return unit_;
	}

	// Unit
	inline
	IOFlags &
	unit( int const unit )
	{
		unit_ = unit;
		return *this;
	}

	// Name
	inline
	Name const &
	name() const
	{
		return name_;
	}

	// Name Set
	inline
	IOFlags &
	name( Name const & name )
	{
		name_ = name;
		return *this;
	}

	// Named?
	inline
	bool
	named() const
	{
		return ! name_.empty();
	}

	// Exists?
	inline
	bool
	exists() const
	{
		return exists_;
	}

	// Exists Set
	inline
	IOFlags &
	exists( bool const exists )
	{
		exists_ = exists;
		return *this;
	}

	// Exists On
	inline
	IOFlags &
	exists_on()
	{
		exists_ = true;
		return *this;
	}

	// Exists?
	inline
	bool
	exist() const
	{
		return exists_;
	}

	// Exists Set
	inline
	IOFlags &
	exist( bool const exists )
	{
		exists_ = exists;
		return *this;
	}

	// Exists On
	inline
	IOFlags &
	exist_on()
	{
		exists_ = true;
		return *this;
	}

	// New?
	inline
	bool
	New() const
	{
		return new_;
	}

	// New Set
	inline
	IOFlags &
	New( bool const new_a )
	{
		new_ = new_a;
		if ( new_ ) {
			old_ = false;
			scratch_ = false;
			unknown_ = false;
		}
		return *this;
	}

	// New On
	inline
	IOFlags &
	New_on()
	{
		new_ = true;
		old_ = false;
		scratch_ = false;
		unknown_ = false;
		return *this;
	}

	// New On
	inline
	IOFlags &
	new_on()
	{
		new_ = true;
		old_ = false;
		scratch_ = false;
		unknown_ = false;
		return *this;
	}

	// Old?
	inline
	bool
	old() const
	{
		return old_;
	}

	// Old Set
	inline
	IOFlags &
	old( bool const old )
	{
		old_ = old;
		if ( old_ ) {
			new_ = false;
			scratch_ = false;
			unknown_ = false;
		}
		return *this;
	}

	// Old On
	inline
	IOFlags &
	old_on()
	{
		old_ = true;
		new_ = false;
		scratch_ = false;
		unknown_ = false;
		return *this;
	}

	// Scratch?
	inline
	bool
	scratch() const
	{
		return scratch_;
	}

	// Scratch Set
	inline
	IOFlags &
	scratch( bool const scratch )
	{
		scratch_ = scratch;
		if ( scratch_ ) {
			new_ = false;
			old_ = false;
			unknown_ = false;
		}
		return *this;
	}

	// Scratch On
	inline
	IOFlags &
	scratch_on()
	{
		scratch_ = true;
		new_ = false;
		old_ = false;
		unknown_ = false;
		return *this;
	}

	// Unknown?
	inline
	bool
	unknown() const
	{
		return unknown_;
	}

	// Unknown Set
	inline
	IOFlags &
	unknown( bool const unknown )
	{
		unknown_ = unknown;
		if ( unknown_ ) {
			new_ = false;
			old_ = false;
			scratch_ = false;
		}
		return *this;
	}

	// Unknown On
	inline
	IOFlags &
	unknown_on()
	{
		unknown_ = true;
		new_ = false;
		old_ = false;
		scratch_ = false;
		return *this;
	}

	// Status String
	inline
	std::string
	STATUS() const
	{
		return ( new_ ? "NEW" : ( old_ ? "OLD" : ( scratch_ ? "SCRATCH" : ( unknown_ ? "UNKNOWN" : "" ) ) ) );
	}

	// Status String Set
	IOFlags &
	STATUS( std::string const & status );

	// Open?
	inline
	bool
	open() const
	{
		return open_;
	}

	// Open Set
	inline
	IOFlags &
	open( bool const open )
	{
		open_ = open;
		return *this;
	}

	// Read?
	inline
	bool
	read() const
	{
		return read_;
	}

	// Read Set
	inline
	IOFlags &
	read( bool const read )
	{
		read_ = read;
		return *this;
	}

	// Read On
	inline
	IOFlags &
	read_on()
	{
		read_ = true;
		return *this;
	}

	// Read-Only?
	inline
	bool
	readonly() const
	{
		return read_ && ! write_;
	}

	// Read Only On
	inline
	IOFlags &
	readonly_on()
	{
		read_ = true;
		write_ = false;
		return *this;
	}

	// Read String
	inline
	std::string
	READ() const
	{
		return ( read_ ? "YES" : ( write_ ? "NO" : "UNKNOWN" ) );
	}

	// Write?
	inline
	bool
	write() const
	{
		return write_;
	}

	// Write Set
	inline
	IOFlags &
	write( bool const write )
	{
		write_ = write;
		return *this;
	}

	// Write On
	inline
	IOFlags &
	write_on()
	{
		write_ = true;
		return *this;
	}

	// Write String
	inline
	std::string
	WRITE() const
	{
		return ( write_ ? "YES" : ( read_ ? "NO" : "UNKNOWN" ) );
	}

	// Read+Write?
	inline
	bool
	rw() const
	{
		return read_ && write_;
	}

	// Read-Write String
	inline
	std::string
	READWRITE() const
	{
		return ( read_ && write_ ? "YES" : "NO" );
	}

	// Access String
	inline
	std::string
	ACCESS() const
	{
		return "SEQUENTIAL"; // Only sequential supported
	}

	// Access String Set
	IOFlags &
	ACCESS( std::string const & access );

	// Action String
	inline
	std::string
	ACTION() const
	{
		return ( read_ ? ( write_ ? "READWRITE" : "READ" ) : ( write_ ? "WRITE" : "UNDEFINED" ) );
	}

	// Action String Set
	IOFlags &
	ACTION( std::string const & action );

	// Position String
	inline
	std::string
	POSITION() const
	{
		return ( append_ ? "APPEND" : ( asis_ ? "ASIS" : "REWIND" ) );
	}

	// Position String Set
	IOFlags &
	POSITION( std::string const & position );

	// Binary?
	inline
	bool
	binary() const
	{
		return binary_;
	}

	// Binary Set
	inline
	IOFlags &
	binary( bool const binary )
	{
		binary_ = binary;
		return *this;
	}

	// Binary On
	inline
	IOFlags &
	binary_on()
	{
		binary_ = true;
		return *this;
	}

	// Binary String
	inline
	std::string
	BINARY() const
	{
		return ( binary_ ? "YES" : "NO" );
	}

	// Binary String Set
	IOFlags &
	BINARY( std::string const & binary );

	// Append?
	inline
	bool
	append() const
	{
		return append_;
	}

	// Append Set
	inline
	IOFlags &
	append( bool const append )
	{
		append_ = append;
		return *this;
	}

	// Append On
	inline
	IOFlags &
	append_on()
	{
		append_ = true;
		if ( append_ ) asis_ = false;
		return *this;
	}

	// Append String
	inline
	std::string
	APPEND() const
	{
		return ( append_ ? "YES" : "NO" );
	}

	// Append String Set
	IOFlags &
	APPEND( std::string const & append );

	// Truncate?
	inline
	bool
	truncate() const
	{
		return truncate_;
	}

	// Truncate Set
	inline
	IOFlags &
	truncate( bool const truncate )
	{
		truncate_ = truncate;
		return *this;
	}

	// Truncate On
	inline
	IOFlags &
	truncate_on()
	{
		truncate_ = true;
		return *this;
	}

	// AsIs?
	inline
	bool
	asis() const
	{
		return asis_;
	}

	// AsIs Set
	inline
	IOFlags &
	asis( bool const asis )
	{
		asis_ = asis;
		if ( asis_ ) append_ = false;
		return *this;
	}

	// AsIs On
	inline
	IOFlags &
	asis_on()
	{
		asis_ = true;
		append_ = false;
		return *this;
	}

	// AsIs Compatible?
	inline
	bool
	asis_compatible( IOFlags const & flags ) const
	{
		if ( read_ != flags.read_ ) {
			return false;
		} else if ( write_ != flags.write_ ) {
			return false;
		} else if ( binary_ != flags.binary_ ) {
			return false;
		} else if ( scratch_ != flags.scratch_ ) {
			return false;
		}
		return true;
	}

	// Rewind?
	inline
	bool
	rewind() const
	{
		return ! ( asis_ || append_ );
	}

	// Rewind On
	inline
	IOFlags &
	rewind_on()
	{
		append_ = false;
		asis_ = false;
		return *this;
	}

	// Form String
	inline
	std::string
	FORM() const
	{
		return ( binary_ ? "BINARY" : "FORMATTED" );
	}

	// Form String Set
	IOFlags &
	FORM( std::string const & form );

	// Formatted String
	inline
	std::string
	FORMATTED() const
	{
		return ( binary_ ? "NO" : "YES" );
	}

	// Unformatted String
	inline
	std::string
	UNFORMATTED() const
	{
		return "NO"; // Unformatted files not supported
	}

	// Sequential String
	inline
	std::string
	SEQUENTIAL() const
	{
		return "YES"; // Only sequential supported
	}

	// Treat Blanks in Numeric Inputs as Zero?
	inline
	bool
	bz() const
	{
		return bz_;
	}

	// Treat Blanks in Numeric Inputs as Zero?
	inline
	bool
	blank_zero() const
	{
		return bz_;
	}

	// Blank Zero Set
	inline
	IOFlags &
	bz( bool const blank_zero )
	{
		bz_ = blank_zero;
		return *this;
	}

	// Blank Zero Set
	inline
	IOFlags &
	blank_zero( bool const blank_zero )
	{
		bz_ = blank_zero;
		return *this;
	}

	// Blank Zero On
	inline
	IOFlags &
	bz_on()
	{
		bz_ = true;
		return *this;
	}

	// Treat Blanks in Numeric Inputs as Null?
	inline
	bool
	bn() const
	{
		return ! bz_;
	}

	// Treat Blanks in Numeric Inputs as Null?
	inline
	bool
	blank_null() const
	{
		return ! bz_;
	}

	// Blank Null Set
	inline
	IOFlags &
	bn( bool const blank_null )
	{
		bz_ = ! blank_null;
		return *this;
	}

	// Blank Null Set
	inline
	IOFlags &
	blank_null( bool const blank_null )
	{
		bz_ = ! blank_null;
		return *this;
	}

	// Blank Null On
	inline
	IOFlags &
	bn_on()
	{
		bz_ = false;
		return *this;
	}

	// Blank String
	inline
	std::string
	BLANK() const
	{
		return ( bz_ ? "ZERO" : "NULL" );
	}

	// Blank String Set
	IOFlags &
	BLANK( std::string const & blank );

	// Advancing I/O?
	inline
	bool
	advance() const
	{
		return ! nad_;
	}

	// Advancing I/O?
	inline
	bool
	advancing() const
	{
		return ! nad_;
	}

	// Non-Advancing I/O?
	inline
	bool
	non_advancing() const
	{
		return nad_;
	}

	// Advancing I/O Set
	inline
	IOFlags &
	advance( bool const advance )
	{
		nad_ = ! advance;
		return *this;
	}

	// Advancing I/O Set
	inline
	IOFlags &
	advancing( bool const advancing )
	{
		nad_ = ! advancing;
		return *this;
	}

	// Non-Advancing I/O Set
	inline
	IOFlags &
	non_advancing( bool const non_advancing )
	{
		nad_ = non_advancing;
		return *this;
	}

	// Non-Advancing I/O On
	inline
	IOFlags &
	na_on()
	{
		nad_ = true;
		return *this;
	}

	// Advancing I/O String Set
	IOFlags &
	ADVANCE( std::string const & advance );

	// Delete?
	inline
	bool
	del() const
	{
		return del_;
	}

	// Delete Set
	inline
	IOFlags &
	del( bool const del )
	{
		del_ = del;
		return *this;
	}

	// Delete On
	inline
	IOFlags &
	del_on()
	{
		del_ = true;
		return *this;
	}

	// Dispose String
	inline
	std::string
	DISPOSE() const
	{
		return ( del_ ? "DELETE" : "KEEP" );
	}

	// Dispose String Set
	IOFlags &
	DISPOSE( std::string const & dispose );

	// Size
	inline
	Size
	size() const
	{
		return size_;
	}

	// Size Set
	inline
	IOFlags &
	size( Size const size )
	{
		size_ = size;
		return *this;
	}

	// Position
	inline
	Pos
	pos() const
	{
		return pos_;
	}

	// Position Set
	inline
	IOFlags &
	pos( Pos const pos )
	{
		pos_ = pos;
		return *this;
	}

	// Terminator
	inline
	std::string const &
	ter() const
	{
		return ter_;
	}

	// Terminator Set
	inline
	IOFlags &
	ter( std::string const & ter )
	{
		ter_ = ter;
		return *this;
	}

	// Linux Terminator Set
	inline
	IOFlags &
	ter_linux()
	{
		ter_ = "\n";
		return *this;
	}

	// Linux Terminator Set
	inline
	IOFlags &
	ter_Linux()
	{
		ter_ = "\n";
		return *this;
	}

	// OS X Terminator Set
	inline
	IOFlags &
	ter_osx()
	{
		ter_ = "\n";
		return *this;
	}

	// OS X Terminator Set
	inline
	IOFlags &
	ter_OSX()
	{
		ter_ = "\n";
		return *this;
	}

	// Windows Terminator Set
	inline
	IOFlags &
	ter_windows()
	{
		ter_ = "\r\n";
		return *this;
	}

	// Windows Terminator Set
	inline
	IOFlags &
	ter_Windows()
	{
		ter_ = "\r\n";
		return *this;
	}

	// DOS Terminator Set
	inline
	IOFlags &
	ter_dos()
	{
		ter_ = "\r\n";
		return *this;
	}

	// DOS Terminator Set
	inline
	IOFlags &
	ter_DOS()
	{
		ter_ = "\r\n";
		return *this;
	}

	// Linefeed Terminator Set
	inline
	IOFlags &
	ter_lf()
	{
		ter_ = "\n";
		return *this;
	}

	// Carriage Return + Linefeed Terminator Set
	inline
	IOFlags &
	ter_crlf()
	{
		ter_ = "\r\n";
		return *this;
	}

	// Native Terminator Set
	inline
	IOFlags &
	ter_native()
	{
#ifdef _WIN32
		ter_ = "\r\n";
#else
		ter_ = "\n";
#endif
		return *this;
	}

	// Error?
	inline
	bool
	err() const
	{
		return err_;
	}

	// Error Set
	inline
	IOFlags &
	err( bool const err )
	{
		err_ = err;
		return *this;
	}

	// Error On
	inline
	IOFlags &
	err_on()
	{
		err_ = true;
		return *this;
	}

	// End of File?
	inline
	bool
	end() const
	{
		return end_;
	}

	// End of File Set
	inline
	IOFlags &
	end( bool const end )
	{
		end_ = end;
		return *this;
	}

	// End On
	inline
	IOFlags &
	end_on()
	{
		end_ = true;
		return *this;
	}

	// End of Record?
	inline
	bool
	eor() const
	{
		return eor_;
	}

	// End of Record Set
	inline
	IOFlags &
	eor( bool const eor )
	{
		eor_ = eor;
		return *this;
	}

	// End of Record On
	inline
	IOFlags &
	eor_on()
	{
		eor_ = true;
		return *this;
	}

	// Status Code
	inline
	int
	ios() const
	{
		return ios_;
	}

	// Status Code Set
	inline
	IOFlags &
	ios( int const ios )
	{
		ios_ = ios;
		return *this;
	}

	// Status Message
	inline
	Msg const &
	msg() const
	{
		return msg_;
	}

	// Status Message Set
	inline
	IOFlags &
	msg( Msg const & msg )
	{
		msg_ = msg;
		return *this;
	}

	// Handle Errors?
	inline
	bool
	her() const
	{
		return her_;
	}

	// Handle Errors Set
	inline
	IOFlags &
	her( bool const her )
	{
		her_ = her;
		return *this;
	}

	// Handle Errors On
	inline
	IOFlags &
	her_on()
	{
		her_ = true;
		return *this;
	}

public: // Methods

	// Clear State
	inline
	IOFlags &
	clear()
	{
		unit_ = 0;
		name_.clear();
		exists_ = false;
		new_ = false;
		old_ = false;
		scratch_ = false;
		unknown_ = false;
		open_ = false;
		read_ = false;
		write_ = false;
		binary_ = false;
		append_ = false;
		truncate_ = false;
		asis_ = true;
		size_ = 0u;
		pos_ = 0;
		ter_ = default_ter();
		bz_ = false;
		nad_ = false;
		del_ = false;
		her_ = false;
		err_ = false;
		end_ = false;
		eor_ = false;
		ios_ = 0;
		msg_.clear();
		return *this;
	}

	// Clear Status
	inline
	IOFlags &
	clear_status()
	{
		err_ = false;
		end_ = false;
		eor_ = false;
		ios_ = 0;
		msg_.clear();
		return *this;
	}

	// Set Status Flags
	inline
	void
	set_status( std::ios const & stream )
	{
		if ( stream.eof() ) {
			end_ = true; // Assume fail due to reading past end of file
			ios_ = -1; // Negative => End of File
			msg_ = "I/O Error: End of file";
			if ( her_ ) error();
		} else if ( ! stream ) {
			err_ = true;
			ios_ = 1; // Positive => Error other than End of File
			msg_ = "I/O Error";
			if ( her_ ) error();
		}
	}

	// Error Handler
#ifdef OBJEXXFCL_IO_ERROR_SUPPRESS
	inline
	void
	error() const
	{}
#else
	void
	error() const;
#endif

	// AsIs Update
	inline
	void
	asis_update( IOFlags const & flags )
	{
		// Update fields that AsIs open can modify
		bz_ = flags.bz_;
		err_ = flags.err_;
		end_ = flags.end_;
		eor_ = flags.eor_;
		ios_ = flags.ios_;
	}

public: // Static Methods

	// Default Output Line Terminator
	inline
	static
	std::string const &
	default_ter()
	{
#ifdef OBJEXXFCL_DEFAULT_LINE_TERMINATOR_LF
		static std::string const dter( "\n" );
#elif defined(OBJEXXFCL_DEFAULT_LINE_TERMINATOR_CRLF)
		static std::string const dter( "\r\n" );
#else // Use native terminator
#ifdef _WIN32
		static std::string const dter( "\r\n" );
#else
		static std::string const dter( "\n" );
#endif
#endif
		return dter;
	}

	// Linefeed
	inline
	static
	std::string const &
	lf()
	{
		static std::string const s( "\n" );
		return s;
	}

	// Linefeed
	inline
	static
	std::string const &
	crlf()
	{
		static std::string const s( "\r\n" );
		return s;
	}

private: // Data

	// Control/Inquiry
	int unit_; // Unit
	Name name_; // Name
	bool exists_; // Exists?
	bool new_; // New?
	bool old_; // Old?
	bool scratch_; // Scratch?
	bool unknown_; // Unknown?
	bool open_; // Open?
	bool read_; // Read?
	bool write_; // Write?
	bool binary_; // Binary?
	bool append_; // Append?
	bool truncate_; // Truncate?
	bool asis_; // AsIs?
	bool bz_; // Treat blanks as zero on numeric input?
	bool nad_; // Non-advancing i/o?
	bool del_; // Delete?
	bool her_; // Handle Errors?
	Size size_; // Size
	Pos pos_; // Position
	std::string ter_; // Output line terminator

	// Status
	bool err_; // Error?
	bool end_; // End of file?
	bool eor_; // End of record?
	int ios_; // Status code
	Msg msg_; // Status message

}; // IOFlags

} // ObjexxFCL

#endif // ObjexxFCL_IOFlags_hh_INCLUDED
