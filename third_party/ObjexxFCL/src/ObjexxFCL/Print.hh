#ifndef ObjexxFCL_Print_hh_INCLUDED
#define ObjexxFCL_Print_hh_INCLUDED

// Formatted Print Support
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
#include <ObjexxFCL/MArray.all.hh>

// C++ Headers
#include <complex>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

class Print
{

public: // Creation

	// Move Constructor
	Print( Print && p ) NOEXCEPT :
#if !defined(__GNUC__) || __GNUC__ >= 5 // GCC 5 adds move constructor
	 stream_( std::move( p.stream_ ) ),
#endif
	 pos_( 0 ),
	 format_( p.format_ ? &p.format_->reset() : nullptr ),
	 own_( p.own_ ),
	 reverts_( 0 )
	{
#if !defined(__GNUC__) || __GNUC__ >= 5
		stream_.clear();
		stream_.seekp( 0, std::ios::beg );
		stream_.str( std::string() );
#endif
		p.pos_ = 0;
		p.format_ = nullptr;
		p.own_ = false;
		p.reverts_ = 0;
	}

	// Format String Constructor
	explicit
	Print( std::string const & fmt = asterisk ) :
	 pos_( 0 ),
	 format_( FormatFactory::create( fmt ) ),
	 own_( true ),
	 reverts_( 0 )
	{}

	// Format Wrapper Constructor
	explicit
	Print( gio::Fmt const & fmt ) :
	 pos_( 0 ),
	 format_( fmt.format_clone() ),
	 own_( true ),
	 reverts_( 0 )
	{}

	// Format Wrapper Constructor
	explicit
	Print( gio::Fmt & fmt ) :
	 pos_( 0 ),
	 format_( fmt.format_reset() ),
	 own_( false ),
	 reverts_( 0 )
	{}

	// Destructor
	~Print()
	{
		if ( format_ ) {
			if ( stream_ ) {
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts_ ) && active->output_no_arg( stream_, pos_ ) ) { // Outputs up to arg-based format
					active = active->next();
				}
				if ( format_->non_advancing() ) { // Non-advancing
					format_->output_pos( stream_, pos_ ); // Move to virtual position
				} else { // Advancing
					stream_ << '\n'; // Add newline
				}
			}
			if ( own_ ) delete format_;
		}
		std::cout << stream_.str(); // Transfer to cout
		status_check();
	}

private: // Creation

	// Copy Constructor
	Print( Print const & ); // Disallow

private: // Assignment

	// Copy Assignment
	Print &
	operator =( Print const & ); // Disallow

public: // Operators

	// Stream << T
	template< typename T >
	typename std::enable_if< ! std::is_base_of< BArray, T >::value, Print & >::type // Force array overload selection for array types
	operator <<( T const & t )
	{
		if ( stream_ && format_ ) {
			reverts_ = format_->reverts();
			Format * active( format_->current() );
			while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts_ ) && active->output_no_arg( stream_, pos_ ) ) { // Outputs up to arg-based format
				active = active->next();
			}
			if ( stream_ && active && active->uses_arg() && active->output_val( stream_, pos_, t ) ) { // Output arg using active format
				reverts_ = format_->reverts();
				active = active->next();
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts_ ) && format_->not_colon_terminated() && active->output_no_arg( stream_, pos_ ) ) { // Outputs up to next arg-based format if not : terminated
					active = active->next();
				}
			}
		}
		status_check();
		return *this;
	}

	// Stream << complex
	template< typename T >
	Print &
	operator <<( std::complex< T > const & t )
	{
		if ( stream_ && format_ ) {
			bool const ld( format_->is_list_directed() );
			if ( ld ) stream_ << '(';
			*this << t.real(); // Fortran uses separate format descriptors for real and imag
			if ( ld && stream_ ) stream_ << ',';
			*this << t.imag(); // Fortran uses separate format descriptors for real and imag
			if ( ld && stream_ ) stream_ << ')';
		}
		pos_ = stream_.tellp();
		status_check();
		return *this;
	}

	// Stream << Array
	template< typename T >
	Print &
	operator <<( Array< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( typename Array< T >::size_type i = 0; i < t.size(); ++i ) {
				*this << t[ i ];
				if ( ! stream_ ) break;
			}
		}
		status_check();
		return *this;
	}

	// Stream << Array1S
	template< typename T >
	Print &
	operator <<( Array1S< T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this << t( i );
				if ( ! stream_ ) break;
			}
		}
		status_check();
		return *this;
	}

	// Stream << Array2S
	template< typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << Array3S
	template< typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << Array4S
	template< typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << Array5S
	template< typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << Array6S
	template< typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << MArray1
	template< class A, typename T >
	Print &
	operator <<( MArray1< A, T > const & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this << t( i );
				if ( ! stream_ ) break;
			}
		}
		status_check();
		return *this;
	}

	// Stream << MArray2
	template< class A, typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << MArray3
	template< class A, typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << MArray4
	template< class A, typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << MArray5
	template< class A, typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream << MArray6
	template< class A, typename T >
	Print &
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
		status_check();
		return *this;
	}

	// Stream Manipulator Output
	Print &
	operator <<( std::ostream & (*pf)( std::ostream & ) )
	{
		if ( stream_ ) stream_ << pf;
		pos_ = stream_.tellp();
		status_check();
		return *this;
	}

	// Stream Manipulator Output
	Print &
	operator <<( std::basic_ios< char > & (*pf)( std::basic_ios< char > & ) )
	{
		if ( stream_ ) stream_ << pf;
		pos_ = stream_.tellp();
		status_check();
		return *this;
	}

private: // Methods

	// Stream Status Check
	void
	status_check() const
	{
#ifndef OBJEXXFCL_IO_ERROR_SUPPRESS
		if ( ! stream_ ) {
			std::cerr << "\nObjexxFCL::Print I/O Error" << std::endl;
			std::exit( EXIT_FAILURE );
		}
#endif
	}

private: // Data

	std::ostringstream stream_; // Internal stream
	std::streampos pos_; // Virtual stream position
	Format * format_; // Format expression
	bool own_; // Own the Format?
	Format::Size reverts_; // Reversion count before last next() call

private: // Static Data

	static std::string const asterisk; // List-directed format string

}; // Print

} // ObjexxFCL

#endif // ObjexxFCL_Print_hh_INCLUDED
