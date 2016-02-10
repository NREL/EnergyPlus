#ifndef ObjexxFCL_IOFlags_hh_INCLUDED
#define ObjexxFCL_IOFlags_hh_INCLUDED

// IOFlags: I/O Control and Status Flags
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

// C++ Headers
#include <cassert>
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

	enum class Status { Old, New, Scratch, Replace, Unknown };
	enum class Access { Sequential, Direct, Stream };
	enum class Action { Read, Write, ReadWrite };
	enum class Form { Formatted, Unformatted, Binary };
	enum class Positioning { AsIs, Rewind, Append };
	enum class Blank { Null, Zero };
	enum class Advance { Yes, No };
	enum class Dispose { Keep, Delete };

public: // Creation

	// Default Constructor
	IOFlags() :
	 unit_( 0 ),
	 exists_( false ),
	 open_( false ),
	 status_( Status::Unknown ),
	 access_( Access::Sequential ),
	 action_( Action::ReadWrite ),
	 form_( Form::Formatted ),
	 positioning_( Positioning::AsIs ),
	 truncate_( false ),
	 blank_( Blank::Null ),
	 advance_( Advance::Yes ),
	 dispose_( Dispose::Keep ),
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
	static
	IOFlags
	handler()
	{
		IOFlags flags;
		flags.her_on();
		return flags;
	}

	// Handle Errors with Line Terminator Named Constructor
	static
	IOFlags
	handler( std::string const & ter )
	{
		IOFlags flags;
		flags.her_on();
		flags.ter( ter );
		return flags;
	}

public: // Unit

	// Unit
	int
	unit() const
	{
		return unit_;
	}

	// Unit
	IOFlags &
	unit( int const unit )
	{
		unit_ = unit;
		return *this;
	}

public: // Name

	// Name
	Name const &
	name() const
	{
		return name_;
	}

	// Name Set
	IOFlags &
	name( Name const & name )
	{
		name_ = name;
		return *this;
	}

	// Named?
	bool
	named() const
	{
		return !name_.empty();
	}

public: // Exists

	// Exists?
	bool
	exists() const
	{
		return exists_;
	}

	// Exists Set
	IOFlags &
	exists( bool const exists )
	{
		exists_ = exists;
		return *this;
	}

	// Exists On
	IOFlags &
	exists_on()
	{
		exists_ = true;
		return *this;
	}

	// Exists?
	bool
	exist() const
	{
		return exists_;
	}

	// Exists Set
	IOFlags &
	exist( bool const exists )
	{
		exists_ = exists;
		return *this;
	}

	// Exists On
	IOFlags &
	exist_on()
	{
		exists_ = true;
		return *this;
	}

public: // Open

	// Open?
	bool
	open() const
	{
		return open_;
	}

	// Open Set
	IOFlags &
	open( bool const open )
	{
		open_ = open;
		return *this;
	}

public: // Status

	// Status
	Status
	status() const
	{
		return status_;
	}

	// Status Set
	IOFlags &
	status( Status const new_status )
	{
		assert( new_status <= Status::Unknown );
		status_ = new_status;
		return *this;
	}

	// Status String
	std::string
	STATUS() const
	{
		switch ( status_ ) {
		case Status::Old:
			return "OLD";
		case Status::New:
			return "NEW";
		case Status::Scratch:
			return "SCRATCH";
		case Status::Replace:
			return "REPLACE";
		case Status::Unknown:
			return "UNKNOWN";
		default:
			assert( false );
			return "UNKNOWN";
		}
	}

	// Status String Set
	IOFlags &
	STATUS( std::string const & status );

	// Old?
	bool
	old() const
	{
		return status_ == Status::Old;
	}

	// Old On
	IOFlags &
	old_on()
	{
		status_ = Status::Old;
		return *this;
	}

	// New?
	bool
	New() const
	{
		return status_ == Status::New;
	}

	// New?
	bool
	new_status() const
	{
		return status_ == Status::New;
	}

	// New On
	IOFlags &
	new_on()
	{
		status_ = Status::New;
		return *this;
	}

	// Scratch?
	bool
	scratch() const
	{
		return status_ == Status::Scratch;
	}

	// Scratch On
	IOFlags &
	scratch_on()
	{
		status_ = Status::Scratch;
		return *this;
	}

	// Replace?
	bool
	replace() const
	{
		return status_ == Status::Replace;
	}

	// Replace On
	IOFlags &
	replace_on()
	{
		status_ = Status::Replace;
		return *this;
	}

	// Unknown?
	bool
	unknown() const
	{
		return status_ == Status::Unknown;
	}

	// Unknown On
	IOFlags &
	unknown_on()
	{
		status_ = Status::Unknown;
		return *this;
	}

public: // Access

	// Access
	Access
	access() const
	{
		return access_;
	}

	// Access Set
	IOFlags &
	access( Access const new_access )
	{
		assert( new_access <= Access::Stream );
		access_ = new_access;
		return *this;
	}

	// Access String
	std::string
	ACCESS() const
	{
		switch ( access_ ) {
		case Access::Sequential:
			return "SEQUENTIAL";
		case Access::Direct:
			return "DIRECT";
		case Access::Stream:
			return "STREAM";
		default:
			assert( false );
			return "SEQUENTIAL";
		}
	}

	// Access String Set
	IOFlags &
	ACCESS( std::string const & access );

	// Sequential?
	bool
	sequential() const
	{
		return access_ == Access::Sequential;
	}

	// Sequential On
	IOFlags &
	sequential_on()
	{
		access_ = Access::Sequential;
		return *this;
	}

	// Direct?
	bool
	direct() const
	{
		return access_ == Access::Direct;
	}

	// Direct On
	IOFlags &
	direct_on()
	{
		access_ = Access::Direct;
		return *this;
	}

	// Stream?
	bool
	stream() const
	{
		return access_ == Access::Stream;
	}

	// Stream On
	IOFlags &
	stream_on()
	{
		access_ = Access::Stream;
		return *this;
	}

public: // Action

	// Action
	Action
	action() const
	{
		return action_;
	}

	// Action Set
	IOFlags &
	action( Action const new_action )
	{
		assert( new_action <= Action::ReadWrite );
		action_ = new_action;
		return *this;
	}

	// Action String
	std::string
	ACTION() const
	{
		switch ( action_ ) {
		case Action::Read:
			return "READ";
		case Action::Write:
			return "WRITE";
		case Action::ReadWrite:
			return "READWRITE";
		default:
			assert( false );
			return "READWRITE";
		}
	}

	// Action String Set
	IOFlags &
	ACTION( std::string const & action );

	// Read?
	bool
	read() const
	{
		return action_ == Action::Read;
	}

	// Read-Only?
	bool
	readonly() const
	{
		return action_ == Action::Read;
	}

	// Read-Only?
	bool
	read_only() const
	{
		return action_ == Action::Read;
	}

	// Read On
	IOFlags &
	read_on()
	{
		action_ = Action::Read;
		return *this;
	}

	// Read-Only On
	IOFlags &
	readonly_on()
	{
		action_ = Action::Read;
		return *this;
	}

	// Read-Only On
	IOFlags &
	read_only_on()
	{
		action_ = Action::Read;
		return *this;
	}

	// Readable?
	bool
	readable() const
	{
		return ( action_ == Action::Read ) || ( action_ == Action::ReadWrite );
	}

	// Write?
	bool
	write() const
	{
		return action_ == Action::Write;
	}

	// Write-Only?
	bool
	writeonly() const
	{
		return action_ == Action::Write;
	}

	// Write-Only?
	bool
	write_only() const
	{
		return action_ == Action::Write;
	}

	// Write On
	IOFlags &
	write_on()
	{
		action_ = Action::Write;
		return *this;
	}

	// Write-Only On
	IOFlags &
	writeonly_on()
	{
		action_ = Action::Write;
		return *this;
	}

	// Write-Only On
	IOFlags &
	write_only_on()
	{
		action_ = Action::Write;
		return *this;
	}

	// Writable?
	bool
	writable() const
	{
		return ( action_ == Action::Write ) || ( action_ == Action::ReadWrite );
	}

	// Read-Write?
	bool
	rw() const
	{
		return action_ == Action::ReadWrite;
	}

	// Read-Write?
	bool
	readwrite() const
	{
		return action_ == Action::ReadWrite;
	}

	// Read-Write?
	bool
	read_write() const
	{
		return action_ == Action::ReadWrite;
	}

	// Read-Write On
	IOFlags &
	rW_on()
	{
		action_ = Action::ReadWrite;
		return *this;
	}

	// Read-Write On
	IOFlags &
	readwrite_on()
	{
		action_ = Action::ReadWrite;
		return *this;
	}

	// Read-Write On
	IOFlags &
	read_write_on()
	{
		action_ = Action::ReadWrite;
		return *this;
	}

public: // Form

	// Form
	Form
	form() const
	{
		return form_;
	}

	// Form Set
	IOFlags &
	form( Form const new_form )
	{
		assert( new_form <= Form::Binary );
		form_ = new_form;
		return *this;
	}

	// Form String
	std::string
	FORM() const
	{
		switch ( form_ ) {
		case Form::Formatted:
			return "FORMATTED";
		case Form::Unformatted:
			return "UNFORMATTED";
		case Form::Binary:
			return "BINARY";
		default:
			assert( false );
			return "FORMATTED";
		}
	}

	// Form String Set
	IOFlags &
	FORM( std::string const & form );

	// Formatted?
	bool
	formatted() const
	{
		return form_ == Form::Formatted;
	}

	// Formatted On
	IOFlags &
	formatted_on()
	{
		form_ = Form::Formatted;
		return *this;
	}

	// Unformatted?
	bool
	unformatted() const
	{
		return form_ == Form::Unformatted;
	}

	// Unformatted On
	IOFlags &
	unformatted_on()
	{
		form_ = Form::Unformatted;
		return *this;
	}

	// Binary?
	bool
	binary() const
	{
		return form_ == Form::Binary;
	}

	// Binary On
	IOFlags &
	binary_on()
	{
		form_ = Form::Binary;
		return *this;
	}

public: // Positioning

	// Positioning
	Positioning
	positioning() const
	{
		return positioning_;
	}

	// Positioning Set
	IOFlags &
	positioning( Positioning const new_positioning )
	{
		assert( new_positioning <= Positioning::Append );
		positioning_ = new_positioning;
		return *this;
	}

	// Position String
	std::string
	POSITION() const
	{
		switch ( positioning_ ) {
		case Positioning::AsIs:
			return "ASIS";
		case Positioning::Rewind:
			return "REWIND";
		case Positioning::Append:
			return "APPEND";
		default:
			assert( false );
			return "ASIS";
		}
	}

	// Positioning String Set
	IOFlags &
	POSITION( std::string const & position );

	// AsIs?
	bool
	asis() const
	{
		return positioning_ == Positioning::AsIs;
	}

	// AsIs On
	IOFlags &
	asis_on()
	{
		positioning_ = Positioning::AsIs;
		return *this;
	}

	// AsIs Compatible?
	bool
	asis_compatible( IOFlags const & flags ) const
	{
		if ( action_ != flags.action_ ) {
			return false;
		} else if ( form_ != flags.form_ ) {
			return false;
		} else if ( scratch() != flags.scratch() ) {
			return false;
		}
		return true;
	}

	// Rewind?
	bool
	rewind() const
	{
		return positioning_ == Positioning::Rewind;
	}

	// Rewind On
	IOFlags &
	rewind_on()
	{
		positioning_ = Positioning::Rewind;
		return *this;
	}

	// Append?
	bool
	append() const
	{
		return positioning_ == Positioning::Append;
	}

	// Append On
	IOFlags &
	append_on()
	{
		positioning_ = Positioning::Append;
		return *this;
	}

public: // Truncate

	// Truncate?
	bool
	truncate() const
	{
		return truncate_;
	}

	// Truncate Set
	IOFlags &
	truncate( bool const truncate )
	{
		truncate_ = truncate;
		return *this;
	}

	// Truncate On
	IOFlags &
	truncate_on()
	{
		truncate_ = true;
		return *this;
	}

public: // Blank

	// Blank String
	std::string
	BLANK() const
	{
		switch ( blank_ ) {
		case Blank::Null:
			return "NULL";
		case Blank::Zero:
			return "ZERO";
		default:
			assert( false );
			return "NULL";
		}
	}

	// Blank String Set
	IOFlags &
	BLANK( std::string const & blank );

	// Treat Blanks in Numeric Inputs as Null?
	bool
	bn() const
	{
		return blank_ == Blank::Null;
	}

	// Treat Blanks in Numeric Inputs as Null?
	bool
	blank_null() const
	{
		return blank_ == Blank::Null;
	}

	// Blank Null On
	IOFlags &
	bn_on()
	{
		blank_ = Blank::Null;
		return *this;
	}

	// Blank Null On
	IOFlags &
	blank_null_on()
	{
		blank_ = Blank::Null;
		return *this;
	}

	// Treat Blanks in Numeric Inputs as Zero?
	bool
	bz() const
	{
		return blank_ == Blank::Zero;
	}

	// Treat Blanks in Numeric Inputs as Zero?
	bool
	blank_zero() const
	{
		return blank_ == Blank::Zero;
	}

	// Blank Zero On
	IOFlags &
	bz_on()
	{
		blank_ = Blank::Zero;
		return *this;
	}

	// Blank Zero On
	IOFlags &
	blank_zero_on()
	{
		blank_ = Blank::Zero;
		return *this;
	}

public: // Advancing I/O

	// Advance String
	std::string
	ADVANCE() const
	{
		switch ( advance_ ) {
		case Advance::Yes:
			return "YES";
		case Advance::No:
			return "NO";
		default:
			assert( false );
			return "YES";
		}
	}

	// Advancing I/O String Set
	IOFlags &
	ADVANCE( std::string const & advance );

	// Advancing I/O?
	bool
	advance() const
	{
		return advance_ == Advance::Yes;
	}

	// Advancing I/O?
	bool
	advancing() const
	{
		return advance_ == Advance::Yes;
	}

	// Advancing I/O On
	IOFlags &
	advancing_on()
	{
		advance_ = Advance::Yes;
		return *this;
	}

	// Non-Advancing I/O?
	bool
	na() const
	{
		return advance_ == Advance::No;
	}

	// Non-Advancing I/O?
	bool
	non_advancing() const
	{
		return advance_ == Advance::No;
	}

	// Non-Advancing I/O On
	IOFlags &
	na_on()
	{
		advance_ = Advance::No;
		return *this;
	}

	// Non-Advancing I/O On
	IOFlags &
	non_advancing_on()
	{
		advance_ = Advance::No;
		return *this;
	}

public: // Dispose

	// Dispose String
	std::string
	DISPOSE() const
	{
		switch ( dispose_ ) {
		case Dispose::Keep:
			return "KEEP";
		case Dispose::Delete:
			return "DELETE";
		default:
			assert( false );
			return "KEEP";
		}
	}

	// Dispose String Set
	IOFlags &
	DISPOSE( std::string const & dispose );

	// Keep?
	bool
	keep() const
	{
		return dispose_ == Dispose::Keep;
	}

	// Keep On
	IOFlags &
	keep_on()
	{
		dispose_ = Dispose::Keep;
		return *this;
	}

	// Save?
	bool
	save() const
	{
		return dispose_ == Dispose::Keep;
	}

	// Save On
	IOFlags &
	save_on()
	{
		dispose_ = Dispose::Keep;
		return *this;
	}

	// Delete?
	bool
	del() const
	{
		return dispose_ == Dispose::Delete;
	}

	// Delete On
	IOFlags &
	del_on()
	{
		dispose_ = Dispose::Delete;
		return *this;
	}

	// Delete On
	IOFlags &
	delete_on()
	{
		dispose_ = Dispose::Delete;
		return *this;
	}

public: // Size

	// Size
	Size
	size() const
	{
		return size_;
	}

	// Size Set
	IOFlags &
	size( Size const size )
	{
		size_ = size;
		return *this;
	}

public: // Position

	// Position
	Pos
	pos() const
	{
		return pos_;
	}

	// Position Set
	IOFlags &
	pos( Pos const pos )
	{
		pos_ = pos;
		return *this;
	}

public: // Line Terminator

	// Terminator
	std::string const &
	ter() const
	{
		return ter_;
	}

	// Terminator Set
	IOFlags &
	ter( std::string const & ter )
	{
		ter_ = ter;
		return *this;
	}

	// Linux Terminator Set
	IOFlags &
	ter_linux()
	{
		ter_ = "\n";
		return *this;
	}

	// Linux Terminator Set
	IOFlags &
	ter_Linux()
	{
		ter_ = "\n";
		return *this;
	}

	// OS X Terminator Set
	IOFlags &
	ter_osx()
	{
		ter_ = "\n";
		return *this;
	}

	// OS X Terminator Set
	IOFlags &
	ter_OSX()
	{
		ter_ = "\n";
		return *this;
	}

	// Windows Terminator Set
	IOFlags &
	ter_windows()
	{
		ter_ = "\r\n";
		return *this;
	}

	// Windows Terminator Set
	IOFlags &
	ter_Windows()
	{
		ter_ = "\r\n";
		return *this;
	}

	// DOS Terminator Set
	IOFlags &
	ter_dos()
	{
		ter_ = "\r\n";
		return *this;
	}

	// DOS Terminator Set
	IOFlags &
	ter_DOS()
	{
		ter_ = "\r\n";
		return *this;
	}

	// Linefeed Terminator Set
	IOFlags &
	ter_lf()
	{
		ter_ = "\n";
		return *this;
	}

	// Carriage Return + Linefeed Terminator Set
	IOFlags &
	ter_crlf()
	{
		ter_ = "\r\n";
		return *this;
	}

	// Native Terminator Set
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

public: // Error Handling

	// Error?
	bool
	err() const
	{
		return err_;
	}

	// Error Set
	IOFlags &
	err( bool const err )
	{
		err_ = err;
		return *this;
	}

	// Error On
	IOFlags &
	err_on()
	{
		err_ = true;
		return *this;
	}

	// End of File?
	bool
	end() const
	{
		return end_;
	}

	// End of File Set
	IOFlags &
	end( bool const end )
	{
		end_ = end;
		return *this;
	}

	// End On
	IOFlags &
	end_on()
	{
		end_ = true;
		return *this;
	}

	// End of Record?
	bool
	eor() const
	{
		return eor_;
	}

	// End of Record Set
	IOFlags &
	eor( bool const eor )
	{
		eor_ = eor;
		return *this;
	}

	// End of Record On
	IOFlags &
	eor_on()
	{
		eor_ = true;
		return *this;
	}

	// Status Code
	int
	ios() const
	{
		return ios_;
	}

	// Status Code Set
	IOFlags &
	ios( int const ios )
	{
		ios_ = ios;
		return *this;
	}

	// Status Message
	Msg const &
	msg() const
	{
		return msg_;
	}

	// Status Message Set
	IOFlags &
	msg( Msg const & msg )
	{
		msg_ = msg;
		return *this;
	}

	// Handle Errors?
	bool
	her() const
	{
		return her_;
	}

	// Handle Errors Set
	IOFlags &
	her( bool const her )
	{
		her_ = her;
		return *this;
	}

	// Handle Errors On
	IOFlags &
	her_on()
	{
		her_ = true;
		return *this;
	}

public: // Methods

	// Clear State
	IOFlags &
	clear()
	{
		unit_ = 0;
		name_.clear();
		exists_ = false;
		open_ = false;
		status_ = Status::Unknown;
		access_ = Access::Sequential;
		action_ = Action::ReadWrite;
		form_ = Form::Formatted;
		positioning_ = Positioning::AsIs;
		truncate_ = false;
		blank_ = Blank::Null;
		advance_ = Advance::Yes;
		dispose_ = Dispose::Keep;
		her_ = false;
		size_ = 0u;
		pos_ = 0;
		ter_ = default_ter();
		err_ = false;
		end_ = false;
		eor_ = false;
		ios_ = 0;
		msg_.clear();
		return *this;
	}

	// Clear Status
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
	void
	set_status( std::ios const & stream )
	{
		if ( stream.eof() ) {
			end_ = true; // Assume fail due to reading past end of file
			ios_ = -1; // Negative => End of File
			msg_ = "I/O Error: End of file";
			if ( her_ ) error();
		} else if ( !stream ) {
			err_ = true;
			ios_ = 1; // Positive => Error other than End of File
			msg_ = "I/O Error";
			if ( her_ ) error();
		}
	}

	// Error Handler
#ifdef OBJEXXFCL_IO_ERROR_SUPPRESS
	void
	error() const
	{}
#else
	void
	error() const;
#endif

	// AsIs Update
	void
	asis_update( IOFlags const & flags )
	{
		// Update fields that AsIs open can modify
		blank_ = flags.blank_;
		err_ = flags.err_;
		end_ = flags.end_;
		eor_ = flags.eor_;
		ios_ = flags.ios_;
	}

public: // Static Methods

	// Default Output Line Terminator
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
	static
	std::string const &
	lf()
	{
		static std::string const s( "\n" );
		return s;
	}

	// Linefeed
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
	bool open_; // Open?
	Status status_; // Status flag
	Access access_; // Access flag
	Action action_; // Action flag
	Form form_; // Form flag
	Positioning positioning_; // Positioning flag
	bool truncate_; // Truncate?
	Blank blank_; // Blanks in numeric input flag
	Advance advance_; // Advancing i/o flag
	Dispose dispose_; // Dispose on close flag
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
