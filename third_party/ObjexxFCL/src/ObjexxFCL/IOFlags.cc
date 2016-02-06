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

// ObjexxFCL Headers
#include <ObjexxFCL/IOFlags.hh>
#include <ObjexxFCL/string.functions.hh>

// C++ Headers
#ifndef OBJEXXFCL_IO_ERROR_SUPPRESS
#include <iostream>
#endif

namespace ObjexxFCL {

	// Status String Set
	IOFlags &
	IOFlags::STATUS( std::string const & status )
	{
		std::string const STATUS( uppercased( status ) );
		if ( STATUS == "OLD" ) {
			old_on();
		} else if ( STATUS == "NEW" ) {
			new_on();
		} else if ( STATUS == "SCRATCH" ) {
			scratch_on();
		} else if ( STATUS == "REPLACE" ) {
			replace_on();
		} else if ( STATUS == "UNKNOWN" ) {
			unknown_on();
		} else {
			assert( false ); // Invalid i/o flag value
			unknown_on();
		}
		return *this;
	}

	// Access String Set
	IOFlags &
	IOFlags::ACCESS( std::string const & access )
	{
		std::string const ACCESS( uppercased( access ) );
		if ( ACCESS == "SEQUENTIAL" ) {
			sequential_on();
		} else if ( ACCESS == "DIRECT" ) {
			assert( false ); // DIRECT not supported
			direct_on();
		} else if ( ACCESS == "STREAM" ) {
			assert( false ); // STREAM not supported
			stream_on();
		} else if ( ACCESS == "APPEND" ) {
			sequential_on(); // APPEND => SEQUENTIAL
			append_on();
		} else {
			assert( false ); // Invalid i/o flag value
			sequential_on();
		}
		return *this;
	}

	// Action String Set
	IOFlags &
	IOFlags::ACTION( std::string const & action )
	{
		std::string const ACTION( uppercased( action ) );
		if ( ACTION == "READ" ) {
			read_on();
		} else if ( ACTION == "WRITE" ) {
			write_on();
		} else if ( ACTION == "READWRITE" ) {
			readwrite_on();
		} else {
			assert( false ); // Invalid i/o flag value
			readwrite_on();
		}
		return *this;
	}

	// Form String Set
	IOFlags &
	IOFlags::FORM( std::string const & form )
	{
		std::string const FORM( uppercased( form ) );
		if ( FORM == "FORMATTED" ) {
			formatted_on();
		} else if ( FORM == "UNFORMATTED" ) {
			binary_on(); // Treat as binary
		} else if ( FORM == "BINARY" ) {
			binary_on();
		} else {
			assert( false ); // Invalid i/o flag value
			formatted_on();
		}
		return *this;
	}

	// Positioning String Set
	IOFlags &
	IOFlags::POSITION( std::string const & position )
	{
		std::string const POSITION( uppercased( position ) );
		if ( POSITION == "ASIS" ) {
			asis_on();
		} else if ( POSITION == "REWIND" ) {
			rewind_on();
		} else if ( POSITION == "APPEND" ) {
			append_on();
		} else {
			assert( false ); // Invalid i/o flag value
			asis_on();
		}
		return *this;
	}

	// Blank String Set
	IOFlags &
	IOFlags::BLANK( std::string const & blank )
	{
		std::string const BLANK( uppercased( blank ) );
		if ( BLANK == "NULL" ) {
			blank_null_on();
		} else if ( BLANK == "ZERO" ) {
			blank_zero_on();
		} else {
			assert( false ); // Invalid i/o flag value
			blank_null_on();
		}
		return *this;
	}

	// Advancing I/O String Set
	IOFlags &
	IOFlags::ADVANCE( std::string const & advance )
	{
		std::string const ADVANCE( uppercased( advance ) );
		if ( ADVANCE == "YES" ) {
			advancing_on();
		} else if ( ADVANCE == "NO" ) {
			non_advancing_on();
		} else {
			assert( false ); // Invalid i/o flag value
			advancing_on();
		}
		return *this;
	}

	// Dispose String Set
	IOFlags &
	IOFlags::DISPOSE( std::string const & dispose )
	{
		std::string const DISPOSE( uppercased( dispose ) );
		if ( DISPOSE == "KEEP" ) {
			keep_on();
		} else if ( DISPOSE == "SAVE" ) {
			keep_on();
		} else if ( DISPOSE == "DELETE" ) {
			delete_on();
		} else if ( DISPOSE == "PRINT" ) {
			keep_on();
		} else if ( DISPOSE == "PRINT/DELETE" ) {
			delete_on();
		} else if ( DISPOSE == "SUBMIT" ) {
			keep_on();
		} else if ( DISPOSE == "SUBMIT/DELETE" ) {
			delete_on();
		} else {
			assert( false ); // Invalid i/o flag value
			keep_on();
		}
		return *this;
	}

	// Error Handler
#ifndef OBJEXXFCL_IO_ERROR_SUPPRESS
	void
	IOFlags::error() const
	{
		std::cerr << '\n' << msg_ << std::endl;
		std::exit( EXIT_FAILURE );
	}
#endif

} // ObjexxFCL
