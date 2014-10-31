#ifndef ObjexxFCL_gio_Fmt_hh_INCLUDED
#define ObjexxFCL_gio_Fmt_hh_INCLUDED

// Global I/O Format Wrapper
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

// ObjexxFCL Headers
#include <ObjexxFCL/Format.hh>

// C++ Headers
#include <string>

namespace ObjexxFCL {
namespace gio {

// Format Wrapper
class Fmt
{

public: // Creation

	// Default Constructor
	inline
	Fmt() :
	 format_( nullptr )
	{}

	// Copy Constructor
	inline
	Fmt( Fmt const & fmt ) :
	 format_( fmt.format_ ? fmt.format_->clone() : nullptr )
	{}

	// String Constructor
	inline
	Fmt( std::string const & format_string ) :
	 format_( FormatFactory::create( format_string ) )
	{}

	// Destructor
	inline
	~Fmt()
	{
		if ( format_ ) delete format_;
	}

public: // Assignment

	// Copy Assignment
	inline
	Fmt &
	operator =( Fmt const & fmt )
	{
		if ( this != &fmt ) {
			if ( format_ ) delete format_;
			format_ = ( fmt.format_ ? fmt.format_->clone() : nullptr );
		}
		return *this;
	}

	// String Assignment
	inline
	Fmt &
	operator =( std::string const & format_string )
	{
		if ( format_ ) delete format_;
		format_ = FormatFactory::create( format_string );
		return *this;
	}

public: // Properties

	// Format
	inline
	Format const *
	format() const
	{
		return format_;
	}

	// Format Clone
	inline
	Format *
	format_clone() const
	{
		return ( format_ ? format_->clone() : nullptr );
	}

private: // Data

	Format * format_; // Format

}; // Fmt

} // gio
} // ObjexxFCL

#endif // ObjexxFCL_gio_Fmt_hh_INCLUDED
