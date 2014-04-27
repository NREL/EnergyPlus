#ifndef ObjexxFCL_Write_hh_INCLUDED
#define ObjexxFCL_Write_hh_INCLUDED

// Formatted Write Support
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
#include <ObjexxFCL/FArray.all.hh>
#include <ObjexxFCL/FArrayS.all.hh>
#include <ObjexxFCL/Fstring.hh>
#include <ObjexxFCL/gio_Fmt.hh>
#include <ObjexxFCL/IOFlags.hh>
#include <ObjexxFCL/MArray.all.hh>

// C++ Headers
#include <complex>
#include <ostream>
#include <sstream>
#include <string>
#include <type_traits>

namespace ObjexxFCL {

// Base Class for Formatted Write
class WriteBase
{

protected: // Creation

	// Constructor
	inline
	WriteBase() :
		reverts_( 0 )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~WriteBase()
	{}
private: // Creation

	// Copy Constructor
	WriteBase( WriteBase const & ); // Disallow

private: // Assignment

	// Copy Assignment
	WriteBase &
	operator =( WriteBase const & ); // Disallow

protected: // Properties

	// Stream
	virtual
	std::ostream const &
	stream() const = 0;

	// Stream
	virtual
	std::ostream &
	stream() = 0;

	// Stream Position
	virtual
	std::streampos
	pos() const = 0;

	// Stream Position
	virtual
	std::streampos &
	pos() = 0;

	// Format
	virtual
	Format const *
	format() const = 0;

	// Format
	virtual
	Format *
	format() = 0;

	// Flags
	virtual
	IOFlags const &
	flags() const = 0;

	// Stream Position
	virtual
	IOFlags &
	flags() = 0;

	// Reversion Count Before Last next()
	inline
	Format::Size
	reverts() const
	{
		return reverts_;
	}

public: // Operators

	// Stream Output
	template< typename T >
	inline
	typename std::enable_if< ! std::is_base_of< BArray, T >::value, WriteBase & >::type // Force array overload selection for array types
	operator <<( T const & t )
	{
		if ( stream() && format() ) {
			reverts_ = format()->reverts();
			Format * active( format()->current() );
			while ( stream() && active && active->no_arg() && ( format()->reverts() == reverts_ ) && active->output( stream(), pos() ) ) { // Outputs up to arg-based format
				active = active->next();
			}
			if ( stream() && active && active->uses_arg() && active->output( stream(), pos(), t ) ) { // Output arg using active format
				reverts_ = format()->reverts();
				active = active->next();
				while ( stream() && active && active->no_arg() && ( format()->reverts() == reverts_ ) && format()->not_colon_terminated() && active->output( stream(), pos() ) ) { // Outputs up to next arg-based format if not : terminated
					active = active->next();
				}
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: complex Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( std::complex< T > const & t )
	{
		if ( stream() && format() ) {
			bool const ld( format()->is_list_directed() );
			if ( ld ) stream() << '(';
			*this << t.real(); // Fortran uses separate format descriptors for real and imag
			if ( ld && stream() ) stream() << ',';
			*this << t.imag(); // Fortran uses separate format descriptors for real and imag
			if ( ld && stream() ) stream() << ')';
		}
		pos() = stream().tellp();
		status_set();
		return *this;
	}

	// Stream Output: FArray Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray< T > const & t )
	{
		if ( stream() && format() ) {
			for ( typename FArray< T >::size_type i = 0; i < t.size(); ++i ) {
				*this << t[ i ];
				if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray1S Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray1S< T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this << t( i );
				if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray2S Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray2S< T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this << t( i1, i2 );
					if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray3S Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray3S< T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this << t( i1, i2, i3 );
						if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray44S Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray4S< T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this << t( i1, i2, i3, i4 );
							if ( ! stream() ) break;
						} if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray5S Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray5S< T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this << t( i1, i2, i3, i4, i5 );
								if ( ! stream() ) break;
							} if ( ! stream() ) break;
						} if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray6S Overload
	template< typename T >
	inline
	WriteBase &
	operator <<( FArray6S< T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this << t( i1, i2, i3, i4, i5, i6 );
									if ( ! stream() ) break;
								} if ( ! stream() ) break;
							} if ( ! stream() ) break;
						} if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: MArray1 Overload
	template< class A, typename T >
	inline
	WriteBase &
	operator <<( MArray1< A, T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this << t( i );
				if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: MArray2 Overload
	template< class A, typename T >
	inline
	WriteBase &
	operator <<( MArray2< A, T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this << t( i1, i2 );
					if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: MArray3 Overload
	template< class A, typename T >
	inline
	WriteBase &
	operator <<( MArray3< A, T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this << t( i1, i2, i3 );
						if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: FArray44S Overload
	template< class A, typename T >
	inline
	WriteBase &
	operator <<( MArray4< A, T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this << t( i1, i2, i3, i4 );
							if ( ! stream() ) break;
						} if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: MArray5 Overload
	template< class A, typename T >
	inline
	WriteBase &
	operator <<( MArray5< A, T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this << t( i1, i2, i3, i4, i5 );
								if ( ! stream() ) break;
							} if ( ! stream() ) break;
						} if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Output: MArray6 Overload
	template< class A, typename T >
	inline
	WriteBase &
	operator <<( MArray6< A, T > const & t )
	{
		if ( stream() && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this << t( i1, i2, i3, i4, i5, i6 );
									if ( ! stream() ) break;
								} if ( ! stream() ) break;
							} if ( ! stream() ) break;
						} if ( ! stream() ) break;
					} if ( ! stream() ) break;
				} if ( ! stream() ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Manipulator Output
	inline
	WriteBase &
	operator <<( std::ostream & (*pf)( std::ostream & ) )
	{
		if ( stream() ) stream() << pf;
		pos() = stream().tellp();
		status_set();
		return *this;
	}

	// Stream Manipulator Output
	inline
	WriteBase &
	operator <<( std::basic_ios< char > & (*pf)( std::basic_ios< char > & ) )
	{
		if ( stream() ) stream() << pf;
		pos() = stream().tellp();
		status_set();
		return *this;
	}

protected: // Methods

	// Status Flags Set
	inline
	void
	status_set()
	{
		flags().set_status( stream() );
	}

private: // Data

	Format::Size reverts_; // Reversion count before last next() call

}; // WriteBase

class WriteStream : public WriteBase
{

public: // Creation

	// Constructor
	inline
	WriteStream( std::ostream & stream, std::string const & fmt, IOFlags & flags ) :
		os_( stream ),
		pos_( 0 ),
		format_( FormatFactory::create( fmt ) ),
		flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->non_advancing() = flags_.non_advancing(); // Allowed with list-directed format but Fortran doesn't
		}
	}

	// Constructor
	inline
	WriteStream( std::ostream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
		os_( stream ),
		pos_( 0 ),
		format_( fmt.format_clone() ),
		flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->non_advancing() = flags_.non_advancing(); // Allowed with list-directed format but Fortran doesn't
		}
	}

	// Destructor
	inline
	virtual
	~WriteStream()
	{
		if ( format_ ) {
			if ( stream_ ) {
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts() ) && active->output( stream_, pos_ ) ) { // Outputs up to arg-based format
					active = active->next();
				}
				if ( format_->non_advancing() ) { // Non-advancing
					format_->output_pos( stream_, pos_ ); // Move to virtual position
				} else { // Advancing
					stream_ << '\n'; // Add newline
				}
				flags_.set_status( stream_ );
			}
			delete format_;
		}
		os_ << stream_.str(); // Transfer to the external stream
	}

protected: // Properties

	// Stream
	inline
	std::ostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostream &
	stream()
	{
		return stream_;
	}

	// Stream Position
	inline
	std::streampos
	pos() const
	{
		return pos_;
	}

	// Stream Position
	inline
	std::streampos &
	pos()
	{
		return pos_;
	}

	// Format
	inline
	Format const *
	format() const
	{
		return format_;
	}

	// Format
	inline
	Format *
	format()
	{
		return format_;
	}

	// Flags
	inline
	IOFlags const &
	flags() const
	{
		return flags_;
	}

	// Stream Position
	inline
	IOFlags &
	flags()
	{
		return flags_;
	}

private: // Data

	std::ostream & os_; // Output stream
	std::ostringstream stream_; // Internal stream
	std::streampos pos_; // Virtual stream position
	Format * format_; // Format expression
	IOFlags & flags_; // I/o flags

}; // WriteStream

class WriteString : public WriteBase
{

public: // Creation

	// Constructor
	inline
	WriteString( std::string & str, std::string const & fmt, IOFlags & flags ) :
		str_( str ),
		pos_( 0 ),
		format_( FormatFactory::create( fmt ) ),
		flags_( flags )
	{
		flags_.clear_status();
	}

	// Constructor
	inline
	WriteString( std::string & str, gio::Fmt const & fmt, IOFlags & flags ) :
		str_( str ),
		pos_( 0 ),
		format_( fmt.format_clone() ),
		flags_( flags )
	{
		flags_.clear_status();
	}

	// Destructor
	inline
	virtual
	~WriteString()
	{
		if ( format_ ) {
			if ( stream_ ) {
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts() ) && active->output( stream_, pos_ ) ) { // Outputs up to arg-based format
					active = active->next();
				}
				format_->output_pos( stream_, pos_ ); // Move to virtual position
				flags_.set_status( stream_ );
			}
			delete format_;
		}
		str_ = stream_.str(); // Transfer to the string
	}

protected: // Properties

	// Stream
	inline
	std::ostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostream &
	stream()
	{
		return stream_;
	}

	// Stream Position
	inline
	std::streampos
	pos() const
	{
		return pos_;
	}

	// Stream Position
	inline
	std::streampos &
	pos()
	{
		return pos_;
	}

	// Format
	inline
	Format const *
	format() const
	{
		return format_;
	}

	// Format
	inline
	Format *
	format()
	{
		return format_;
	}

	// Flags
	inline
	IOFlags const &
	flags() const
	{
		return flags_;
	}

	// Stream Position
	inline
	IOFlags &
	flags()
	{
		return flags_;
	}

private: // Data

	std::string & str_; // Output string
	std::ostringstream stream_; // Internal stream
	std::streampos pos_; // Stream position
	Format * format_; // Format expression
	IOFlags & flags_; // I/o flags

}; // WriteString

class WriteFstring : public WriteBase
{

public: // Creation

	// Constructor
	inline
	WriteFstring( Fstring & str, std::string const & fmt, IOFlags & flags ) :
		str_( str ),
		pos_( 0 ),
		format_( FormatFactory::create( fmt ) ),
		flags_( flags )
	{
		flags_.clear_status();
	}

	// Constructor
	inline
	WriteFstring( Fstring & str, gio::Fmt const & fmt, IOFlags & flags ) :
		str_( str ),
		pos_( 0 ),
		format_( fmt.format_clone() ),
		flags_( flags )
	{
		flags_.clear_status();
	}

	// Destructor
	inline
	virtual
	~WriteFstring()
	{
		if ( format_ ) {
			if ( stream_ ) {
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts() ) && active->output( stream_, pos_ ) ) { // Outputs up to arg-based format
					active = active->next();
				}
				format_->output_pos( stream_, pos_ ); // Move to virtual position
				flags_.set_status( stream_ );
			}
			delete format_;
		}
		str_ = stream_.str(); // Transfer to the Fstring
	}

protected: // Properties

	// Stream
	inline
	std::ostream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::ostream &
	stream()
	{
		return stream_;
	}

	// Stream Position
	inline
	std::streampos
	pos() const
	{
		return pos_;
	}

	// Stream Position
	inline
	std::streampos &
	pos()
	{
		return pos_;
	}

	// Format
	inline
	Format const *
	format() const
	{
		return format_;
	}

	// Format
	inline
	Format *
	format()
	{
		return format_;
	}

	// Flags
	inline
	IOFlags const &
	flags() const
	{
		return flags_;
	}

	// Stream Position
	inline
	IOFlags &
	flags()
	{
		return flags_;
	}

private: // Data

	Fstring & str_; // Output string
	std::ostringstream stream_; // Internal stream
	std::streampos pos_; // Stream position
	Format * format_; // Format expression
	IOFlags & flags_; // I/o flags

}; // WriteFstring

// Write Wrapper Factory
class Write
{

public: // Creation

	// Default Constructor
	inline
	Write() :
		write_( nullptr )
	{}

	// Move Constructor
	inline
	Write( Write && w ) :
		flags_( w.flags_ ),
		write_( w.write_ )
	{
		w.write_ = nullptr;
	}

	// Flags Constructor
	inline
	explicit
	Write( IOFlags & ) :
		write_( nullptr )
	{}

	// Stream + Format Constructor
	inline
	Write( std::ostream & stream, std::string const & fmt ) :
		flags_( IOFlags::handler() ),
		write_( new WriteStream( stream, fmt, flags_ ) )
	{}

	// Stream + Format Constructor
	inline
	Write( std::ostream & stream, gio::Fmt const & fmt ) :
		flags_( IOFlags::handler() ),
		write_( new WriteStream( stream, fmt, flags_ ) )
	{}

	// Stream + Format + Flags Constructor
	inline
	Write( std::ostream & stream, std::string const & fmt, IOFlags & flags ) :
		write_( new WriteStream( stream, fmt, flags ) )
	{}

	// Stream + Format + Flags Constructor
	inline
	Write( std::ostream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
		write_( new WriteStream( stream, fmt, flags ) )
	{}

	// String + Format Constructor
	inline
	Write( std::string & str, std::string const & fmt ) :
		flags_( IOFlags::handler() ),
		write_( new WriteString( str, fmt, flags_ ) )
	{}

	// String + Format Constructor
	inline
	Write( std::string & str, gio::Fmt const & fmt ) :
		flags_( IOFlags::handler() ),
		write_( new WriteString( str, fmt, flags_ ) )
	{}

	// String + Format + Flags Constructor
	inline
	Write( std::string & str, std::string const & fmt, IOFlags & flags ) :
		write_( new WriteString( str, fmt, flags ) )
	{}

	// String + Format + Flags Constructor
	inline
	Write( std::string & str, gio::Fmt const & fmt, IOFlags & flags ) :
		write_( new WriteString( str, fmt, flags ) )
	{}

	// Fstring + Format Constructor
	inline
	Write( Fstring & str, std::string const & fmt ) :
		flags_( IOFlags::handler() ),
		write_( new WriteFstring( str, fmt, flags_ ) )
	{}

	// Fstring + Format Constructor
	inline
	Write( Fstring & str, gio::Fmt const & fmt ) :
		flags_( IOFlags::handler() ),
		write_( new WriteFstring( str, fmt, flags_ ) )
	{}

	// Fstring + Format + Flags Constructor
	inline
	Write( Fstring & str, std::string const & fmt, IOFlags & flags ) :
		write_( new WriteFstring( str, fmt, flags ) )
	{}

	// Fstring + Format + Flags Constructor
	inline
	Write( Fstring & str, gio::Fmt const & fmt, IOFlags & flags ) :
		write_( new WriteFstring( str, fmt, flags ) )
	{}

	// Destructor
	inline
	~Write()
	{
		if ( write_ ) delete write_;
	}

private: // Creation

	// Copy Constructor
	Write( Write const & ); // Disallow

private: // Assignment

	// Copy Assignment
	Write &
	operator =( Write const & ); // Disallow

public: // Operators

	// Stream Output
	template< typename T >
	inline
	Write &
	operator <<( T const & t )
	{
		if ( write_ ) *write_ << t;
		return *this;
	}

	// Stream Manipulator Output
	inline
	Write &
	operator <<( std::ostream & (*pf)( std::ostream & ) )
	{
		if ( write_ ) *write_ << pf;
		return *this;
	}

	// Stream Manipulator Output
	inline
	Write &
	operator <<( std::basic_ios< char > & (*pf)( std::basic_ios< char > & ) )
	{
		if ( write_ ) *write_ << pf;
		return *this;
	}

private: // Data

	IOFlags flags_; // Internal i/o flags
	WriteBase * write_; // Implementation object

}; // Write

} // ObjexxFCL

#endif // ObjexxFCL_Write_hh_INCLUDED
