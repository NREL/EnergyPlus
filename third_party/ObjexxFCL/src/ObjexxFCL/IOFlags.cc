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

// ObjexxFCL Headers
#include <ObjexxFCL/IOFlags.hh>
#include <ObjexxFCL/string.functions.hh>

// C++ Headers
#include <cassert>
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
			unknown_on();
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
		if ( ACCESS == "DIRECT" ) {
			// Not supported
		} else if ( ACCESS == "SEQUENTIAL" ) {
			// Default
		} else if ( ACCESS == "STREAM" ) {
			// Not supported
		} else if ( ACCESS == "APPEND" ) {
			append_ = true;
			asis_ = false;
		} else {
			assert( false ); // Invalid i/o flag value
		}
		return *this;
	}

	// Action String Set
	IOFlags &
	IOFlags::ACTION( std::string const & action )
	{
		std::string const ACTION( uppercased( action ) );
		if ( ACTION == "READ" ) {
			read_ = true;
			write_ = false;
		} else if ( ACTION == "WRITE" ) {
			read_ = false;
			write_ = true;
		} else if ( ACTION == "READWRITE" ) {
			read_ = true;
			write_ = true;
		} else {
			assert( false ); // Invalid i/o flag value
			read_ = true;
			write_ = true;
		}
		return *this;
	}

	// Position String Set
	IOFlags &
	IOFlags::POSITION( std::string const & position )
	{
		std::string const POSITION( uppercased( position ) );
		if ( POSITION == "ASIS" ) {
			append_ = false;
			asis_ = true;
		} else if ( POSITION == "REWIND" ) { // Default
			append_ = false;
			asis_ = false;
		} else if ( POSITION == "APPEND" ) {
			append_ = true;
			asis_ = false;
		} else {
			assert( false ); // Invalid i/o flag value
			append_ = false;
			asis_ = true;
		}
		return *this;
	}

	// Binary String Set
	IOFlags &
	IOFlags::BINARY( std::string const & binary )
	{
		std::string const BINARY( uppercased( binary ) );
		if ( BINARY == "YES" ) {
			binary_ = true;
		} else if ( BINARY == "NO" ) {
			binary_ = false;
		} else {
			assert( false ); // Invalid i/o flag value
			binary_ = false;
		}
		return *this;
	}

	// Append String Set
	IOFlags &
	IOFlags::APPEND( std::string const & append )
	{
		std::string const APPEND( uppercased( append ) );
		if ( APPEND == "YES" ) {
			append_ = true;
		} else if ( APPEND == "NO" ) {
			append_ = false;
		} else {
			assert( false ); // Invalid i/o flag value
			append_ = false;
		}
		return *this;
	}

	// Form String Set
	IOFlags &
	IOFlags::FORM( std::string const & form )
	{
		std::string const FORM( uppercased( form ) );
		if ( FORM == "FORMATTED" ) {
			binary_ = false;
		} else if ( FORM == "UNFORMATTED" ) {
			binary_ = true; // Unsupported: Treat as binary
		} else if ( FORM == "BINARY" ) {
			binary_ = true;
		} else {
			assert( false ); // Invalid i/o flag value
			binary_ = false;
		}
		return *this;
	}

	// Blank String Set
	IOFlags &
	IOFlags::BLANK( std::string const & blank )
	{
		std::string const BLANK( uppercased( blank ) );
		if ( BLANK == "NULL" ) {
			bz_ = false;
		} else if ( BLANK == "ZERO" ) {
			bz_ = true;
		} else {
			assert( false ); // Invalid i/o flag value
			bz_ = false;
		}
		return *this;
	}

	// Advancing I/O String Set
	IOFlags &
	IOFlags::ADVANCE( std::string const & advance )
	{
		std::string const ADVANCE( uppercased( advance ) );
		if ( ADVANCE == "YES" ) {
			nad_ = false;
		} else if ( ADVANCE == "NO" ) {
			nad_ = true;
		} else {
			assert( false ); // Invalid i/o flag value
			nad_ = false;
		}
		return *this;
	}

	// Dispose String Set
	IOFlags &
	IOFlags::DISPOSE( std::string const & dispose )
	{
		std::string const DISPOSE( uppercased( dispose ) );
		if ( DISPOSE == "KEEP" ) {
			del_ = false;
		} else if ( DISPOSE == "SAVE" ) {
			del_ = false;
		} else if ( DISPOSE == "DELETE" ) {
			del_ = true;
		} else if ( DISPOSE == "PRINT" ) {
			del_ = false;
		} else if ( DISPOSE == "PRINT/DELETE" ) {
			del_ = true;
		} else if ( DISPOSE == "SUBMIT" ) {
			del_ = false;
		} else if ( DISPOSE == "SUBMIT/DELETE" ) {
			del_ = true;
		} else {
			assert( false ); // Invalid i/o flag value
			del_ = false;
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
