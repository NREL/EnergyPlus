#ifndef ObjexxFCL_GlobalStreams_hh_INCLUDED
#define ObjexxFCL_GlobalStreams_hh_INCLUDED

// Global Streams Collection
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
#include <string>
#include <unordered_map>

namespace ObjexxFCL {

// Forward
class IOFlags;
class Stream;

namespace gio {

// Global Streams Collection
class GlobalStreams
{

public: // Types

	typedef  int  Unit;
	typedef  std::string  Name;
	typedef  std::unordered_map< Unit, Stream * >  UnitStreams;
	typedef  std::unordered_map< Name, Stream * >  NameStreams;

public: // Creation

	// Default Constructor
	GlobalStreams();

	// Destructor
	~GlobalStreams();

private: // Creation

	// Copy Constructor
	GlobalStreams( GlobalStreams const & ); // Disallow

private: // Assignment

	// Copy Assignment
	GlobalStreams &
	operator =( GlobalStreams const & ); // Disallow

public: // Lookup

	// Lookup by Unit
	Stream *
	operator []( Unit const unit );

	// Lookup by Name
	Stream *
	operator []( Name const & name );

public: // Predicates

	// Has a Unit
	bool
	has( Unit const unit ) const;

	// Has a Name
	bool
	has( Name const & name ) const;

public: // Inspectors

	// Size
	UnitStreams::size_type
	size() const
	{
		return fix_name_streams_.size() + usr_unit_streams_.size();
	}

	// Next Free Unit
	Unit
	next_unit() const
	{
		return next_unit_;
	}

	// Unit of Name
	Unit
	unit( Name const & name ) const;

	// Unit of Stream
	Unit
	unit( Stream const & stream ) const;

public: // Methods

	// Add an Input File Stream
	Stream *
	add_i( Unit const unit, Name const & name );

	// Add an Output File Stream
	Stream *
	add_o( Unit const unit, Name const & name );

	// Add a File Stream
	Stream *
	add( Unit const unit, Name const & name );

	// Add an Input File Stream
	Stream *
	add_i( Unit const unit, Name const & name, IOFlags const & flags );

	// Add an Output File Stream
	Stream *
	add_o( Unit const unit, Name const & name, IOFlags const & flags );

	// Add a File Stream
	Stream *
	add( Unit const unit, Name const & name, IOFlags const & flags );

	// Remove a File Stream
	void
	del( Unit const unit );

	// Remove a File Stream
	void
	rem( Unit const unit )
	{
		del( unit );
	}

	// Remove a File Stream
	void
	remove( Unit const unit )
	{
		del( unit );
	}

	// Clear the User Streams
	void
	clear();

private: // Methods

	// Add a File Stream for a Unit
	Stream *
	add_stream( Unit const unit, Stream * stream_p );

	// Update the Next Unit for an Added Unit
	void
	next_unit_add( Unit const unit );

private: // Data

	int next_unit_; // Next available unit

	UnitStreams fix_unit_streams_; // Fixed streams keyed by Unit
	UnitStreams usr_unit_streams_; // User streams keyed by Unit

	NameStreams fix_name_streams_; // Fixed streams keyed by Name
	NameStreams usr_name_streams_; // User streams keyed by Name

}; // GlobalStreams

} // gio
} // ObjexxFCL

#endif // ObjexxFCL_GlobalStreams_hh_INCLUDED
