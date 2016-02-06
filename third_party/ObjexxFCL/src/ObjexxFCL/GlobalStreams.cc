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

// ObjexxFCL Headers
#include <ObjexxFCL/GlobalStreams.hh>
#include <ObjexxFCL/Stream.hh>

// C++ Headers
#include <algorithm>
#include <cassert>
#include <iostream>
#include <limits>

namespace ObjexxFCL {
namespace gio {

	// Default Constructor
	GlobalStreams::GlobalStreams() :
	 next_unit_( 1 )
	{
		IStream * const cin_p( new IStream( std::cin, "stdin" ) );
		OStream * const cout_p( new OStream( std::cout, "stdout" ) );
		OStream * const cerr_p( new OStream( std::cerr, "stderr" ) );

		// May need to customize this to match some Fortran compilers
		fix_unit_streams_[ 5 ] = fix_unit_streams_[ 100 ] = cin_p;
		fix_unit_streams_[ 6 ] = fix_unit_streams_[ 101 ] = cout_p;
		fix_unit_streams_[ 0 ] = fix_unit_streams_[ 102 ] = cerr_p;

		fix_name_streams_[ cin_p->name() ] = cin_p;
		fix_name_streams_[ cout_p->name() ] = cout_p;
		fix_name_streams_[ cerr_p->name() ] = cerr_p;
	}

	// Destructor
	GlobalStreams::~GlobalStreams()
	{
		for ( auto & s : fix_name_streams_ ) delete s.second;
		for ( auto & s : usr_unit_streams_ ) delete s.second;
	}

	// Lookup by Unit
	Stream *
	GlobalStreams::operator []( Unit const unit )
	{
		auto i( usr_unit_streams_.find( unit ) );
		if ( i != usr_unit_streams_.end() ) {
			return i->second;
		} else {
			auto i( fix_unit_streams_.find( unit ) );
			return ( i != fix_unit_streams_.end() ? i->second : nullptr );
		}
	}

	// Lookup by Name
	Stream *
	GlobalStreams::operator []( Name const & name )
	{
		auto i( usr_name_streams_.find( name ) );
		if ( i != usr_name_streams_.end() ) {
			return i->second;
		} else {
			auto i( fix_name_streams_.find( name ) );
			return ( i != fix_name_streams_.end() ? i->second : nullptr );
		}
	}

	// Has a Unit
	bool
	GlobalStreams::has( Unit const unit ) const
	{
		auto i( usr_unit_streams_.find( unit ) );
		if ( i != usr_unit_streams_.end() ) {
			return true;
		} else {
			auto i( fix_unit_streams_.find( unit ) );
			return ( i != fix_unit_streams_.end() ? true : false );
		}
	}

	// Has a Name
	bool
	GlobalStreams::has( Name const & name ) const
	{
		auto i( usr_name_streams_.find( name ) );
		if ( i != usr_name_streams_.end() ) {
			return true;
		} else {
			auto i( fix_name_streams_.find( name ) );
			return ( i != fix_name_streams_.end() ? true : false );
		}
	}

	// Unit of Name
	GlobalStreams::Unit
	GlobalStreams::unit( Name const & name ) const
	{ // Stream doesn't cache unit so this is not efficient
		for ( auto const & s : usr_unit_streams_ ) {
			if ( s.second->name() == name ) return s.first;
		}
		for ( auto const & s : fix_unit_streams_ ) {
			if ( s.second->name() == name ) return s.first;
		}
		return -1; // => No such name connected
	}

	// Unit of Stream
	GlobalStreams::Unit
	GlobalStreams::unit( Stream const & stream ) const
	{ // Stream doesn't cache unit so this is not efficient
		for ( auto const & s : usr_unit_streams_ ) {
			if ( s.second == &stream ) return s.first;
		}
		for ( auto const & s : fix_unit_streams_ ) {
			if ( s.second == &stream ) return s.first;
		}
		return -1; // => No such stream connected
	}

	// Add an Input File Stream
	Stream *
	GlobalStreams::add_i( Unit const unit, Name const & name )
	{
		del( unit ); // Remove any existing Stream on that unit
		return add_stream( unit, new IFileStream( name ) );
	}

	// Add an Output File Stream
	Stream *
	GlobalStreams::add_o( Unit const unit, Name const & name )
	{
		del( unit ); // Remove any existing Stream on that unit
		return add_stream( unit, new OFileStream( name ) );
	}

	// Add a File Stream
	Stream *
	GlobalStreams::add( Unit const unit, Name const & name )
	{
		del( unit ); // Remove any existing Stream on that unit
		return add_stream( unit, new FileStream( name ) );
	}

	// Add an Input File Stream
	Stream *
	GlobalStreams::add_i( Unit const unit, Name const & name, IOFlags const & flags )
	{
		del( unit ); // Remove any existing Stream on that unit
		return add_stream( unit, new IFileStream( name, flags ) );
	}

	// Add an Output File Stream
	Stream *
	GlobalStreams::add_o( Unit const unit, Name const & name, IOFlags const & flags )
	{
		del( unit ); // Remove any existing Stream on that unit
		return add_stream( unit, new OFileStream( name, flags ) );
	}

	// Add a File Stream
	Stream *
	GlobalStreams::add( Unit const unit, Name const & name, IOFlags const & flags )
	{
		del( unit ); // Remove any existing Stream on that unit
		return add_stream( unit, new FileStream( name, flags ) );
	}

	// Remove a File Stream
	void
	GlobalStreams::del( Unit const unit )
	{
		auto i( usr_unit_streams_.find( unit ) );
		if ( i != usr_unit_streams_.end() ) {
			usr_name_streams_.erase( i->second->name() );
			delete i->second;
			usr_unit_streams_.erase( i );
			next_unit_ = std::min( next_unit_, unit );
		}
	}

	// Clear the User Streams
	void
	GlobalStreams::clear()
	{
		for ( auto & s : usr_unit_streams_ ) delete s.second;
		usr_unit_streams_.clear();
		usr_name_streams_.clear();
	}

	// Add a File Stream for a Unit
	Stream *
	GlobalStreams::add_stream( Unit const unit, Stream * stream_p )
	{
		if ( unit >= 0 ) {
			usr_unit_streams_[ unit ] = stream_p;
			usr_name_streams_[ stream_p->name() ] = stream_p;
			next_unit_add( unit );
			return stream_p;
		} else {
			delete stream_p;
			return nullptr;
		}
	}

	// Update the Next Unit for an Added Unit
	void
	GlobalStreams::next_unit_add( Unit const unit )
	{
		if ( unit == next_unit_ ) {
			while ( has( ++next_unit_ ) ) { // Increment until a free unit found
				if ( next_unit_ == std::numeric_limits< Unit >::max() ) {
					next_unit_ = -1; // Indicates failure
					break;
				}
			}
		}
	}

} // gio
} // ObjexxFCL
