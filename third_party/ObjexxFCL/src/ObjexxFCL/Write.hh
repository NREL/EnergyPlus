#ifndef ObjexxFCL_Write_hh_INCLUDED
#define ObjexxFCL_Write_hh_INCLUDED

// Formatted Write Support
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
#include <ObjexxFCL/Format.hh>
#include <ObjexxFCL/Array.all.hh>
#include <ObjexxFCL/ArrayS.all.hh>
#include <ObjexxFCL/gio_Fmt.hh>
#include <ObjexxFCL/IOFlags.hh>
#include <ObjexxFCL/MArray.all.hh>
#include <ObjexxFCL/stream.functions.hh>

// C++ Headers
#include <complex>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

class Write
{

public: // Creation

	// Default Constructor
	Write() :
	 os_( nullptr ),
	 str_( nullptr ),
	 format_( nullptr ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Move Constructor
	Write( Write && w ) NOEXCEPT :
	 os_( w.os_ ),
	 str_( w.str_ ),
	 format_( w.format_ ? &w.format_->reset() : nullptr ),
	 format_own_( w.format_own_ ),
	 ter_( w.ter_ ),
	 flags_( w.flags_ ? &w.flags_->clear_status() : nullptr ),
#if !defined(__GNUC__) || __GNUC__ >= 5 // GCC 5 adds move constructor
	 stream_( std::move( w.stream_ ) ),
#endif
	 pos_( 0 ),
	 reverts_( 0 )
	{
		w.os_ = nullptr;
		w.str_ = nullptr;
		w.format_ = nullptr;
		w.format_own_ = false;
		w.ter_ = LF;
#if !defined(__GNUC__) || __GNUC__ >= 5
		stream_.clear();
		stream_.seekp( 0, std::ios::beg );
		stream_.str( std::string() );
#endif
		w.pos_ = 0;
		w.reverts_ = 0;
	}

	// Flags Constructor
	explicit
	Write( IOFlags & flags ) :
	 os_( nullptr ),
	 str_( nullptr ),
	 format_( nullptr ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( &flags.clear_status().ter( LF ) ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Stream + Format Constructor
	Write( std::ostream & stream, std::string const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( is_fstream( stream ) ? IOFlags::default_ter() : LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Stream + Format Constructor
	Write( std::ostream & stream, gio::Fmt const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( is_fstream( stream ) ? IOFlags::default_ter() : LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Stream + Format Constructor
	Write( std::ostream & stream, gio::Fmt & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( is_fstream( stream ) ? IOFlags::default_ter() : LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Stream + Format + Flags Constructor
	Write( std::ostream & stream, std::string const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( is_fstream( stream ) ? flags.ter() : LF ),
	 flags_( &flags.clear_status().ter( ter_ ) ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Stream + Format + Flags Constructor
	Write( std::ostream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( is_fstream( stream ) ? flags.ter() : LF ),
	 flags_( &flags.clear_status().ter( ter_ ) ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Stream + Format + Flags Constructor
	Write( std::ostream & stream, gio::Fmt & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( is_fstream( stream ) ? flags.ter() : LF ),
	 flags_( &flags.clear_status().ter( ter_ ) ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Stream + Format + Terminator Constructor
	Write( std::ostream & stream, std::string const & fmt, std::string const & ter ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( ter ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Stream + Format + Terminator Constructor
	Write( std::ostream & stream, gio::Fmt const & fmt, std::string const & ter ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( ter ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Stream + Format + Terminator Constructor
	Write( std::ostream & stream, gio::Fmt & fmt, std::string const & ter ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( ter ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// File Stream + Format Constructor
	Write( std::fstream & stream, std::string const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( IOFlags::default_ter() ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// File Stream + Format Constructor
	Write( std::fstream & stream, gio::Fmt const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( IOFlags::default_ter() ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// File Stream + Format Constructor
	Write( std::fstream & stream, gio::Fmt & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( IOFlags::default_ter() ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// File Stream + Format + Flags Constructor
	Write( std::fstream & stream, std::string const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( flags.ter() ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// File Stream + Format + Flags Constructor
	Write( std::fstream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( flags.ter() ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// File Stream + Format + Flags Constructor
	Write( std::fstream & stream, gio::Fmt & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( flags.ter() ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Output File Stream + Format Constructor
	Write( std::ofstream & stream, std::string const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( IOFlags::default_ter() ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Output File Stream + Format Constructor
	Write( std::ofstream & stream, gio::Fmt const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( IOFlags::default_ter() ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Output File Stream + Format Constructor
	Write( std::ofstream & stream, gio::Fmt & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( IOFlags::default_ter() ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Output File Stream + Format + Flags Constructor
	Write( std::ofstream & stream, std::string const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( flags.ter() ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Output File Stream + Format + Flags Constructor
	Write( std::ofstream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( flags.ter() ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Output File Stream + Format + Flags Constructor
	Write( std::ofstream & stream, gio::Fmt & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( flags.ter() ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// String Stream + Format Constructor
	Write( std::stringstream & stream, std::string const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String Stream + Format Constructor
	Write( std::stringstream & stream, gio::Fmt const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String Stream + Format Constructor
	Write( std::stringstream & stream, gio::Fmt & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String Stream + Format + Flags Constructor
	Write( std::stringstream & stream, std::string const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// String Stream + Format + Flags Constructor
	Write( std::stringstream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// String Stream + Format + Flags Constructor
	Write( std::stringstream & stream, gio::Fmt & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Output String Stream + Format Constructor
	Write( std::ostringstream & stream, std::string const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Output String Stream + Format Constructor
	Write( std::ostringstream & stream, gio::Fmt const & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Output String Stream + Format Constructor
	Write( std::ostringstream & stream, gio::Fmt & fmt ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Output String Stream + Format + Flags Constructor
	Write( std::ostringstream & stream, std::string const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Output String Stream + Format + Flags Constructor
	Write( std::ostringstream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// Output String Stream + Format + Flags Constructor
	Write( std::ostringstream & stream, gio::Fmt & fmt, IOFlags & flags ) :
	 os_( &stream ),
	 str_( nullptr ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{
		if ( format_ ) format_->non_advancing() = flags_->non_advancing();
	}

	// String + Format Constructor
	Write( std::string & str, std::string const & fmt ) :
	 os_( nullptr ),
	 str_( &str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String + Format Constructor
	Write( std::string & str, gio::Fmt const & fmt ) :
	 os_( nullptr ),
	 str_( &str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String + Format Constructor
	Write( std::string & str, gio::Fmt & fmt ) :
	 os_( nullptr ),
	 str_( &str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( nullptr ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String + Format + Flags Constructor
	Write( std::string & str, std::string const & fmt, IOFlags & flags ) :
	 os_( nullptr ),
	 str_( &str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String + Format + Flags Constructor
	Write( std::string & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 os_( nullptr ),
	 str_( &str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// String + Format + Flags Constructor
	Write( std::string & str, gio::Fmt & fmt, IOFlags & flags ) :
	 os_( nullptr ),
	 str_( &str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 ter_( LF ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 ),
	 reverts_( 0 )
	{}

	// Destructor
	~Write()
	{
		if ( format_ ) {
			if ( stream_ ) {
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts() ) && active->output_no_arg( stream_, pos_, ter_ ) ) { // Outputs up to arg-based format
					active = active->next();
				}
				if ( str_ || format_->non_advancing() ) { // Non-advancing
					format_->output_pos( stream_, pos_ ); // Move to virtual position
				} else { // Advancing
					stream_ << ter_; // Add line terminator
				}
				set_status();
			}
			if ( format_own_ ) delete format_;
		}
		if ( os_ ) { // Transfer to the stream
			*os_ << stream_.str();
		} else if ( str_ ) { // Transfer to the string
			*str_ = stream_.str();
		}
	}

private: // Creation

	// Copy Constructor
	Write( Write const & ); // Disallow

private: // Assignment

	// Copy Assignment
	Write &
	operator =( Write const & ); // Disallow

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

	// Format
	Format const *
	format() const
	{
		return format_;
	}

	// Format
	Format *
	format()
	{
		return format_;
	}

	// Line Terminator
	std::string const &
	ter() const
	{
		return ter_;
	}

	// Reversion Count Before Last next()
	Format::Size
	reverts() const
	{
		return reverts_;
	}

public: // Operators

	// Stream Manipulator Output
	Write &
	operator <<( std::ostream & (*pf)( std::ostream & ) )
	{
		if ( stream_ ) stream_ << pf;
		pos_ = stream_.tellp();
		set_status();
		return *this;
	}

	// Stream Manipulator Output
	Write &
	operator <<( std::basic_ios< char > & (*pf)( std::basic_ios< char > & ) )
	{
		if ( stream_ ) stream_ << pf;
		pos_ = stream_.tellp();
		set_status();
		return *this;
	}

	// Stream << T
	template< typename T >
	typename std::enable_if< ! std::is_base_of< BArray, T >::value, Write & >::type // Force array overload selection for array types
	operator <<( T const & t )
	{
		if ( stream_ ) {
			if ( format_ ) {
				reverts_ = format_->reverts();
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts_ ) && active->output_no_arg( stream_, pos_, ter_ ) ) { // Outputs up to arg-based format
					active = active->next();
				}
				if ( stream_ && active && active->uses_arg() && active->output_val( stream_, pos_, t, ter_ ) ) { // Output arg using active format
					reverts_ = format_->reverts();
					active = active->next();
					while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts_ ) && format_->not_colon_terminated() && active->output_no_arg( stream_, pos_, ter_ ) ) { // Outputs up to next arg-based format if not : terminated
						active = active->next();
					}
				}
			}
		}
		set_status();
		return *this;
	}

	// Stream << complex
	template< typename T >
	Write &
	operator <<( std::complex< T > const & t )
	{
		if ( stream_ ) {
			if ( format_ ) {
				bool const ld( format_->is_list_directed() );
				if ( ld ) stream_ << '(';
				*this << t.real(); // Fortran uses separate format descriptors for real and imag
				if ( ld && stream_ ) stream_ << ',';
				*this << t.imag(); // Fortran uses separate format descriptors for real and imag
				if ( ld && stream_ ) stream_ << ')';
			}
		}
		pos_ = stream_.tellp();
		set_status();
		return *this;
	}

	// Stream << Array
	template< typename T >
	Write &
	operator <<( Array< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( typename Array< T >::size_type i = 0; i < t.size(); ++i ) {
				*this << t[ i ];
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array1S
	template< typename T >
	Write &
	operator <<( Array1S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this << t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array2S
	template< typename T >
	Write &
	operator <<( Array2S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this << t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array3S
	template< typename T >
	Write &
	operator <<( Array3S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this << t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array44S
	template< typename T >
	Write &
	operator <<( Array4S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this << t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array5S
	template< typename T >
	Write &
	operator <<( Array5S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this << t( i1, i2, i3, i4, i5 );
								if ( ! stream_ ) break;
							} if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array6S
	template< typename T >
	Write &
	operator <<( Array6S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this << t( i1, i2, i3, i4, i5, i6 );
									if ( ! stream_ ) break;
								} if ( ! stream_ ) break;
							} if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << MArray1
	template< class A, typename T >
	Write &
	operator <<( MArray1< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this << t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << MArray2
	template< class A, typename T >
	Write &
	operator <<( MArray2< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this << t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << MArray3
	template< class A, typename T >
	Write &
	operator <<( MArray3< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this << t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << Array44S
	template< class A, typename T >
	Write &
	operator <<( MArray4< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this << t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << MArray5
	template< class A, typename T >
	Write &
	operator <<( MArray5< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this << t( i1, i2, i3, i4, i5 );
								if ( ! stream_ ) break;
							} if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream << MArray6
	template< class A, typename T >
	Write &
	operator <<( MArray6< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this << t( i1, i2, i3, i4, i5, i6 );
									if ( ! stream_ ) break;
								} if ( ! stream_ ) break;
							} if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

private: // Methods

	// Status Flags Set
	void
	set_status()
	{
		if ( flags_ ) flags_->set_status( stream_ );
	}

private: // Data

	std::ostream * os_; // Output stream
	std::string * str_; // Output string
	Format * format_; // Format expression
	bool format_own_; // Own the Format?
	std::string ter_; // Line terminator
	IOFlags * flags_; // Internal i/o flags
	std::ostringstream stream_; // Internal stream
	std::streampos pos_; // Stream position
	Format::Size reverts_; // Reversion count before last next() call

public: // Static Data

	static std::string const LF; // Linefeed

}; // Write

// Write to Stream
inline
Write
write( std::ostream & stream, std::string const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ostream & stream, gio::Fmt const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ostream & stream, gio::Fmt & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ostream & stream, std::string const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ostream & stream, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ostream & stream, gio::Fmt & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ostream & stream, std::string const & fmt, std::string const & ter )
{
	return Write( stream, fmt, ter );
}

// Write to Stream
inline
Write
write( std::ostream & stream, gio::Fmt const & fmt, std::string const & ter )
{
	return Write( stream, fmt, ter );
}

// Write to Stream
inline
Write
write( std::ostream & stream, gio::Fmt & fmt, std::string const & ter )
{
	return Write( stream, fmt, ter );
}

// Write to Stream
inline
Write
write( std::fstream & stream, std::string const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::fstream & stream, gio::Fmt const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::fstream & stream, gio::Fmt & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::fstream & stream, std::string const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::fstream & stream, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::fstream & stream, gio::Fmt & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ofstream & stream, std::string const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ofstream & stream, gio::Fmt const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ofstream & stream, gio::Fmt & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ofstream & stream, std::string const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ofstream & stream, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ofstream & stream, gio::Fmt & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::stringstream & stream, std::string const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::stringstream & stream, gio::Fmt const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::stringstream & stream, gio::Fmt & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::stringstream & stream, std::string const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::stringstream & stream, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::stringstream & stream, gio::Fmt & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ostringstream & stream, std::string const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ostringstream & stream, gio::Fmt const & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ostringstream & stream, gio::Fmt & fmt )
{
	return Write( stream, fmt );
}

// Write to Stream
inline
Write
write( std::ostringstream & stream, std::string const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ostringstream & stream, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to Stream
inline
Write
write( std::ostringstream & stream, gio::Fmt & fmt, IOFlags & flags )
{
	return Write( stream, fmt, flags );
}

// Write to stdout
inline
Write
write( std::string const & fmt )
{
	return Write( std::cout, fmt );
}

// Write to stdout
inline
Write
write( gio::Fmt const & fmt )
{
	return Write( std::cout, fmt );
}

// Write to stdout
inline
Write
write( gio::Fmt & fmt )
{
	return Write( std::cout, fmt );
}

// Write to stdout
inline
Write
write( std::string const & fmt, IOFlags & flags )
{
	return Write( std::cout, fmt, flags );
}

// Write to stdout
inline
Write
write( gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( std::cout, fmt, flags );
}

// Write to stdout
inline
Write
write( gio::Fmt & fmt, IOFlags & flags )
{
	return Write( std::cout, fmt, flags );
}

// Write to String
inline
Write
write( std::string & str, std::string const & fmt )
{
	return Write( str, fmt );
}

// Write to String
inline
Write
write( std::string & str, gio::Fmt const & fmt )
{
	return Write( str, fmt );
}

// Write to String
inline
Write
write( std::string & str, gio::Fmt & fmt )
{
	return Write( str, fmt );
}

// Write to String
inline
Write
write( std::string & str, std::string const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to String
inline
Write
write( std::string & str, gio::Fmt const & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

// Write to String
inline
Write
write( std::string & str, gio::Fmt & fmt, IOFlags & flags )
{
	return Write( str, fmt, flags );
}

} // ObjexxFCL

#endif // ObjexxFCL_Write_hh_INCLUDED
