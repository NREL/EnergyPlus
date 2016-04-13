#ifndef ObjexxFCL_gio_Fmt_hh_INCLUDED
#define ObjexxFCL_gio_Fmt_hh_INCLUDED

// Global I/O Format Wrapper
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
#include <ObjexxFCL/noexcept.hh>

// C++ Headers
#include <cassert>
#include <string>

namespace ObjexxFCL {
namespace gio {

// Format Wrapper
class Fmt
{

public: // Creation

	// Default Constructor
	Fmt() :
	 format_( nullptr )
	{}

	// Copy Constructor
	Fmt( Fmt const & fmt ) :
	 format_( fmt.format_ ? fmt.format_->clone() : nullptr )
	{}

	// Move Constructor
	Fmt( Fmt && fmt ) NOEXCEPT :
	 format_( fmt.format_ ? &fmt.format_->reset() : nullptr )
	{
		fmt.format_ = nullptr;
	}

	// String Constructor
	Fmt( std::string const & format_string ) :
	 format_( FormatFactory::create( format_string ) )
	{}

	// Destructor
	~Fmt()
	{
		if ( format_ ) delete format_;
	}

public: // Assignment

	// Copy Assignment
	Fmt &
	operator =( Fmt const & fmt )
	{
		if ( this != &fmt ) {
			if ( format_ ) delete format_;
			format_ = ( fmt.format_ ? fmt.format_->clone() : nullptr );
		}
		return *this;
	}

	// Move Assignment
	Fmt &
	operator =( Fmt && fmt )
	{
		assert ( this != &fmt );
		if ( format_ ) delete format_;
		format_ = ( fmt.format_ ? &fmt.format_->reset() : nullptr );
		fmt.format_ = nullptr;
		return *this;
	}

	// String Assignment
	Fmt &
	operator =( std::string const & format_string )
	{
		if ( format_ ) delete format_;
		format_ = FormatFactory::create( format_string );
		return *this;
	}

public: // Properties

	// Format
	Format const *
	format() const
	{
		return format_;
	}

	// Format Clone
	Format *
	format_clone() const
	{
		return ( format_ ? format_->clone() : nullptr );
	}

public: // Methods

	// Reset
	Format *
	format_reset()
	{
		return ( format_ ? &format_->reset() : nullptr );
	}

private: // Data

	Format * format_; // Format

}; // Fmt

} // gio
} // ObjexxFCL

#endif // ObjexxFCL_gio_Fmt_hh_INCLUDED
