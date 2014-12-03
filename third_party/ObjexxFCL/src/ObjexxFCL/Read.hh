#ifndef ObjexxFCL_Read_hh_INCLUDED
#define ObjexxFCL_Read_hh_INCLUDED

// Formatted Read Support
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
#include <ObjexxFCL/stream.functions.hh>

// C++ Headers
#include <algorithm>
#include <complex>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>

namespace ObjexxFCL {

class ReadStream
{

public: // Creation

	// Default Constructor
	inline
	ReadStream() :
	 sstream_( nullptr ),
	 stream_( internal_stream_ ),
	 format_( nullptr ),
	 format_own_( false ),
	 flags_( nullptr ),
	 poa_( 0 ),
	 por_( 0 )
	{}

	// Move Constructor
	inline
	ReadStream( ReadStream && r ) :
	 sstream_( nullptr ),
	 stream_( r.stream_ ),
	 format_( r.format_ ),
	 format_own_( r.format_own_ ),
	 flags_( r.flags_ ? &r.flags_->clear_status() : nullptr ),
	 poa_( r.poa_ ),
	 por_( r.por_ )
	{
		r.format_ = nullptr;
		r.format_own_ = false;
	}

	// Flags Constructor
	inline
	explicit
	ReadStream( IOFlags & flags ) :
	 sstream_( nullptr ),
	 stream_( internal_stream_ ),
	 format_( nullptr ),
	 format_own_( false ),
	 flags_( &flags.clear_status() ),
	 poa_( 0 ),
	 por_( 0 )
	{}

	// Stream + Format Constructor
	inline
	ReadStream( std::istream & stream, std::string const & fmt, bool const beg = false ) :
	 sstream_( stream.rdbuf() == std::cin.rdbuf() ? new std::istringstream : nullptr ),
	 stream_( sstream_ ? *sstream_ : stream ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( nullptr ),
	 poa_( sstream_ || beg ? static_cast< std::streampos >( 0 ) : stream.tellg() ),
	 por_( 0 )
	{
		if ( sstream_ ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			sstream_->str( s );
		}
	}

	// Stream + Format Constructor
	inline
	ReadStream( std::istream & stream, gio::Fmt const & fmt, bool const beg = false ) :
	 sstream_( stream.rdbuf() == std::cin.rdbuf() ? new std::istringstream : nullptr ),
	 stream_( sstream_ ? *sstream_ : stream ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( nullptr ),
	 poa_( sstream_ || beg ? static_cast< std::streampos >( 0 ) : stream.tellg() ),
	 por_( 0 )
	{
		if ( sstream_ ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			sstream_->str( s );
		}
	}

	// Stream + Format Constructor
	inline
	ReadStream( std::istream & stream, gio::Fmt & fmt, bool const beg = false ) :
	 sstream_( stream.rdbuf() == std::cin.rdbuf() ? new std::istringstream : nullptr ),
	 stream_( sstream_ ? *sstream_ : stream ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( nullptr ),
	 poa_( sstream_ || beg ? static_cast< std::streampos >( 0 ) : stream.tellg() ),
	 por_( 0 )
	{
		if ( sstream_ ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			sstream_->str( s );
		}
	}

	// Stream + Format + Flags Constructor
	inline
	ReadStream( std::istream & stream, std::string const & fmt, IOFlags & flags, bool const beg = false ) :
	 sstream_( stream.rdbuf() == std::cin.rdbuf() ? new std::istringstream : nullptr ),
	 stream_( sstream_ ? *sstream_ : stream ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 poa_( sstream_ || beg ? static_cast< std::streampos >( 0 ) : stream.tellg() ),
	 por_( 0 )
	{
		if ( format_ ) {
			format_->blank_zero() = flags_->blank_zero();
			format_->non_advancing() = flags_->non_advancing();
		}
		if ( sstream_ ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			sstream_->str( s );
		}
	}

	// Stream + Format + Flags Constructor
	inline
	ReadStream( std::istream & stream, gio::Fmt const & fmt, IOFlags & flags, bool const beg = false ) :
	 sstream_( stream.rdbuf() == std::cin.rdbuf() ? new std::istringstream : nullptr ),
	 stream_( sstream_ ? *sstream_ : stream ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 poa_( sstream_ || beg ? static_cast< std::streampos >( 0 ) : stream.tellg() ),
	 por_( 0 )
	{
		if ( format_ ) {
			format_->blank_zero() = flags_->blank_zero();
			format_->non_advancing() = flags_->non_advancing();
		}
		if ( sstream_ ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			sstream_->str( s );
		}
	}

	// Stream + Format + Flags Constructor
	inline
	ReadStream( std::istream & stream, gio::Fmt & fmt, IOFlags & flags, bool const beg = false ) :
	 sstream_( stream.rdbuf() == std::cin.rdbuf() ? new std::istringstream : nullptr ),
	 stream_( sstream_ ? *sstream_ : stream ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( &flags.clear_status() ),
	 poa_( sstream_ || beg ? static_cast< std::streampos >( 0 ) : stream.tellg() ),
	 por_( 0 )
	{
		if ( format_ ) {
			format_->blank_zero() = flags_->blank_zero();
			format_->non_advancing() = flags_->non_advancing();
		}
		if ( sstream_ ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			sstream_->str( s );
		}
	}

	// Destructor
	inline
	~ReadStream()
	{
		if ( format_ ) {
			if ( stream_ && ! sstream_ ) {
				if ( format_->non_advancing() ) { // Non-advancing
					format_->input_pos( stream_, poa_ + por_ ); // Set final stream position
				} else { // Advancing
					stream_ >> Format::skip; // Advance to next line
				}
			}
			if ( format_own_ ) delete format_;
			if ( sstream_ ) delete sstream_;
		}
	}

private: // Creation

	// Copy Constructor
	ReadStream( ReadStream const & ); // Disallow

private: // Assignment

	// Copy Assignment
	ReadStream &
	operator =( ReadStream const & ); // Disallow

public: // Properties

	// Stream
	inline
	std::istream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::istream &
	stream()
	{
		return stream_;
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

public: // Operators

	// Stream Input
	template< typename T, class = typename std::enable_if< ! std::is_base_of< BArray, T >::value >::type >
	inline
	ReadStream &
	operator >>( T & t )
	{
		if ( stream_ ) {
			if ( format_ && format_->not_slash_terminated() ) {
				Format::Size const reverts( format_->reverts() );
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts ) && format_->not_slash_terminated() && active->input( stream_, poa_, por_ ) ) { // Inputs up to arg-based format
					active = active->next();
				}
				if ( stream_ && active && active->uses_arg() && format_->not_slash_terminated() && active->input( stream_, poa_, por_, t ) ) { // Input arg using active format
					Format::Size const reverts( format_->reverts() );
					active = active->next();
					while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts ) && format_->not_terminated() && active->input( stream_, poa_, por_ ) ) { // Inputs up to next arg-based format if not terminated
						active = active->next();
					}
				}
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: complex Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( std::complex< T > & t )
	{
		if ( stream_ ) {
			if ( format_ ) {
				bool const ld( format_->is_list_directed() );
				bool bad( false );
				if ( ld ) {
					while ( stream_ && ( stream_.peek() == ' ' ) ) {
						stream_.ignore();
						por_ += 1;
					}
					if ( stream_ && stream_.peek() == '(' ) {
						stream_.ignore();
						por_ += 1;
					} else {
						bad = true;
					}
				}
				T real( 0.0 ), imag( 0.0 );
				*this >> real; // Fortran uses separate format descriptors for real and imag
				if ( bad ) { // No leading ( so treat as real part only specified
					t = std::complex< T >( real, T( 0.0 ) ); // Intel Fortran will set the real part when the (real,imag) structure is not present
				} else {
					*this >> imag; // Fortran uses separate format descriptors for real and imag
					if ( ld && stream_ ) {
						while ( stream_ && ( stream_.peek() == ' ' ) ) {
							stream_.ignore();
							por_ += 1;
						}
						if ( stream_ && stream_.peek() == ')' ) {
							stream_.ignore();
							por_ += 1;
						} else {
							bad = true;
						}
					}
					if ( bad ) { // Partial (real,imag) structure: Bad input
						t = std::complex< T >( T( 0.0 ), T( 0.0 ) );
					} else {
						t = std::complex< T >( real, imag );
					}
				}
			}
		}
		por_ = stream_.tellg() - poa_;
		set_status();
		return *this;
	}

	// Stream Input: FArray Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( typename FArray< T >::size_type i = 0; i < t.size(); ++i ) {
				*this >> t[ i ];
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray1S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray2S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray3S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray4S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray5S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray6S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray1S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray2S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray3S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray4S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray5S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	ReadStream &
	operator >>( FArray6S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray1< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray2< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray3< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray4< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray5< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray6< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray1< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray2< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray3< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray4< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray5< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	ReadStream &
	operator >>( MArray6< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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
	inline
	void
	set_status()
	{
		if ( flags_ ) flags_->set_status( stream_ );
	}

private: // Data

	std::istringstream * sstream_; // Internal stream for std::cin reads
	std::istream & stream_; // Input stream
	Format * format_; // Format expression
	bool format_own_; // Own the Format?
	IOFlags * flags_; // I/o flags
	std::streampos const poa_; // Absolute stream position
	std::streampos por_; // Relative virtual stream position

private: // Static Data

	static std::istringstream internal_stream_; // Internal stream

}; // ReadStream

class ReadString
{

public: // Creation

	// Default Constructor
	inline
	ReadString() :
	 format_( nullptr ),
	 format_own_( false ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// Move Constructor
	inline
	ReadString( ReadString && r ) :
#if defined(__GNUC__) && __GNUC__ < 5 // GCC 5 will add missing move constructor
	 stream_( r.stream_.str() ),
#else
	 stream_( std::move( r.stream_ ) ),
#endif
	 format_( r.format_ ),
	 format_own_( r.format_own_ ),
	 flags_( r.flags_ ? &r.flags_->clear_status() : nullptr ),
	 pos_( r.pos_ )
	{
#if defined(__GNUC__) && __GNUC__ < 5 // Finish copying state
		stream_.copyfmt( r.stream_ );
		stream_.clear( r.stream_.rdstate() );
		stream_.seekg( r.stream_.tellg() );
#endif
		r.format_ = nullptr;
		r.format_own_ = false;
	}

	// Flags Constructor
	inline
	explicit
	ReadString( IOFlags & flags ) :
	 format_( nullptr ),
	 format_own_( false ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{}

	// String + Format Constructor
	inline
	ReadString( std::string const & str, std::string const & fmt ) :
	 stream_( str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// String + Format Constructor
	inline
	ReadString( std::string const & str, gio::Fmt const & fmt ) :
	 stream_( str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// String + Format Constructor
	inline
	ReadString( std::string const & str, gio::Fmt & fmt ) :
	 stream_( str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// String + Format + Flags Constructor
	inline
	ReadString( std::string const & str, std::string const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// String + Format + Flags Constructor
	inline
	ReadString( std::string const & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// String + Format + Flags Constructor
	inline
	ReadString( std::string const & str, gio::Fmt & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// Fstring + Format Constructor
	inline
	ReadString( Fstring const & str, std::string const & fmt ) :
	 stream_( str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// Fstring + Format Constructor
	inline
	ReadString( Fstring const & str, gio::Fmt const & fmt ) :
	 stream_( str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// Fstring + Format Constructor
	inline
	ReadString( Fstring const & str, gio::Fmt & fmt ) :
	 stream_( str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// Fstring + Format + Flags Constructor
	inline
	ReadString( Fstring const & str, std::string const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// Fstring + Format + Flags Constructor
	inline
	ReadString( Fstring const & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// Fstring + Format + Flags Constructor
	inline
	ReadString( Fstring const & str, gio::Fmt & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// C-String + Format Constructor
	inline
	ReadString( char const * str, std::string const & fmt ) :
	 stream_( str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// C-String + Format Constructor
	inline
	ReadString( char const * str, gio::Fmt const & fmt ) :
	 stream_( str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// C-String + Format Constructor
	inline
	ReadString( char const * str, gio::Fmt & fmt ) :
	 stream_( str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( nullptr ),
	 pos_( 0 )
	{}

	// C-String + Format + Flags Constructor
	inline
	ReadString( char const * str, std::string const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( FormatFactory::create( fmt ) ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// C-String + Format + Flags Constructor
	inline
	ReadString( char const * str, gio::Fmt const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( fmt.format_clone() ),
	 format_own_( true ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// C-String + Format + Flags Constructor
	inline
	ReadString( char const * str, gio::Fmt & fmt, IOFlags & flags ) :
	 stream_( str ),
	 format_( fmt.format_reset() ),
	 format_own_( false ),
	 flags_( &flags.clear_status() ),
	 pos_( 0 )
	{
		if ( format_ ) format_->blank_zero() = flags_->blank_zero();
	}

	// Destructor
	inline
	~ReadString()
	{
		if ( format_ && format_own_ ) delete format_;
	}

private: // Creation

	// Copy Constructor
	ReadString( ReadString const & ); // Disallow

private: // Assignment

	// Copy Assignment
	ReadString &
	operator =( ReadString const & ); // Disallow

public: // Properties

	// Stream
	inline
	std::istream const &
	stream() const
	{
		return stream_;
	}

	// Stream
	inline
	std::istream &
	stream()
	{
		return stream_;
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

public: // Operators

	// Stream Input
	template< typename T, class = typename std::enable_if< ! std::is_base_of< BArray, T >::value >::type >
	inline
	ReadString &
	operator >>( T & t )
	{
		if ( stream_ ) {
			if ( format_ && format_->not_slash_terminated() ) {
				Format::Size const reverts( format_->reverts() );
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts ) && format_->not_slash_terminated() && active->input( stream_, pos_ ) ) { // Inputs up to arg-based format
					active = active->next();
				}
				if ( stream_ && active && active->uses_arg() && format_->not_slash_terminated() && active->input( stream_, pos_, t ) ) { // Input arg using active format
					Format::Size const reverts( format_->reverts() );
					active = active->next();
					while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts ) && format_->not_terminated() && active->input( stream_, pos_ ) ) { // Inputs up to next arg-based format if not terminated
						active = active->next();
					}
				}
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: complex Overload
	template< typename T >
	inline
	ReadString &
	operator >>( std::complex< T > & t )
	{
		if ( stream_ ) {
			if ( format_ ) {
				bool const ld( format_->is_list_directed() );
				bool bad( false );
				if ( ld ) {
					while ( stream_ && ( stream_.peek() == ' ' ) ) {
						stream_.ignore();
						pos_ += 1;
					}
					if ( stream_ && stream_.peek() == '(' ) {
						stream_.ignore();
						pos_ += 1;
					} else {
						bad = true;
					}
				}
				T real( 0.0 ), imag( 0.0 );
				*this >> real; // Fortran uses separate format descriptors for real and imag
				if ( bad ) { // No leading ( so treat as real part only specified
					t = std::complex< T >( real, T( 0.0 ) ); // Intel Fortran will set the real part when the (real,imag) structure is not present
				} else {
					*this >> imag; // Fortran uses separate format descriptors for real and imag
					if ( ld && stream_ ) {
						while ( stream_ && ( stream_.peek() == ' ' ) ) {
							stream_.ignore();
							pos_ += 1;
						}
						if ( stream_ && stream_.peek() == ')' ) {
							stream_.ignore();
							pos_ += 1;
						} else {
							bad = true;
						}
					}
					if ( bad ) { // Partial (real,imag) structure: Bad input
						t = std::complex< T >( T( 0.0 ), T( 0.0 ) );
					} else {
						t = std::complex< T >( real, imag );
					}
				}
			}
		}
		pos_ = stream_.tellg();
		set_status();
		return *this;
	}

	// Stream Input: FArray Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( typename FArray< T >::size_type i = 0; i < t.size(); ++i ) {
				*this >> t[ i ];
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray1S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray2S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray3S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray4S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray5S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray6S< T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray1S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray2S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray3S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray4S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray5S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	ReadString &
	operator >>( FArray6S< T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray1< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray2< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray3< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray4< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray5< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray6< A, T > & t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray1< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray2< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray3< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray4< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							*this >> t( i1, i2, i3, i4 );
							if ( ! stream_ ) break;
						} if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		set_status();
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray5< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								*this >> t( i1, i2, i3, i4, i5 );
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

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	ReadString &
	operator >>( MArray6< A, T > && t )
	{
		if ( stream_ && format_ ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						for ( int i4 = 1, e4 = t.u4(); i4 <= e4; ++i4 ) {
							for ( int i5 = 1, e5 = t.u5(); i5 <= e5; ++i5 ) {
								for ( int i6 = 1, e6 = t.u6(); i6 <= e6; ++i6 ) {
									*this >> t( i1, i2, i3, i4, i5, i6 );
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
	inline
	void
	set_status()
	{
		if ( flags_ ) flags_->set_status( stream_ );
	}

private: // Data

	std::istringstream stream_; // Internal stream
	Format * format_; // Format expression
	bool format_own_; // Own the Format?
	IOFlags * flags_; // I/o flags
	std::streampos pos_; // Stream position

}; // ReadString

// Read from Stream
inline
ReadStream
read( std::istream & stream, std::string const & fmt )
{
	return ReadStream( stream, fmt );
}

// Read from Stream
inline
ReadStream
read( std::istream & stream, gio::Fmt const & fmt )
{
	return ReadStream( stream, fmt );
}

// Read from Stream
inline
ReadStream
read( std::istream & stream, gio::Fmt & fmt )
{
	return ReadStream( stream, fmt );
}

// Read from Stream
inline
ReadStream
read( std::istream & stream, std::string const & fmt, IOFlags & flags )
{
	return ReadStream( stream, fmt, flags );
}

// Read from Stream
inline
ReadStream
read( std::istream & stream, gio::Fmt const & fmt, IOFlags & flags )
{
	return ReadStream( stream, fmt, flags );
}

// Read from Stream
inline
ReadStream
read( std::istream & stream, gio::Fmt & fmt, IOFlags & flags )
{
	return ReadStream( stream, fmt, flags );
}

// Read from stdin
inline
ReadStream
read( std::string const & fmt )
{
	return ReadStream( std::cin, fmt );
}

// Read from stdin
inline
ReadStream
read( gio::Fmt const & fmt )
{
	return ReadStream( std::cin, fmt );
}

// Read from stdin
inline
ReadStream
read( gio::Fmt & fmt )
{
	return ReadStream( std::cin, fmt );
}

// Read from stdin
inline
ReadStream
read( std::string const & fmt, IOFlags & flags )
{
	return ReadStream( std::cin, fmt, flags );
}

// Read from stdin
inline
ReadStream
read( gio::Fmt const & fmt, IOFlags & flags )
{
	return ReadStream( std::cin, fmt, flags );
}

// Read from stdin
inline
ReadStream
read( gio::Fmt & fmt, IOFlags & flags )
{
	return ReadStream( std::cin, fmt, flags );
}

// Read from String
inline
ReadString
read( std::string const & str, std::string const & fmt )
{
	return ReadString( str, fmt );
}

// Read from String
inline
ReadString
read( std::string const & str, gio::Fmt const & fmt )
{
	return ReadString( str, fmt );
}

// Read from String
inline
ReadString
read( std::string const & str, gio::Fmt & fmt )
{
	return ReadString( str, fmt );
}

// Read from String
inline
ReadString
read( std::string const & str, std::string const & fmt, IOFlags & flags )
{
	return ReadString( str, fmt, flags );
}

// Read from String
inline
ReadString
read( std::string const & str, gio::Fmt const & fmt, IOFlags & flags )
{
	return ReadString( str, fmt, flags );
}

// Read from String
inline
ReadString
read( std::string const & str, gio::Fmt & fmt, IOFlags & flags )
{
	return ReadString( str, fmt, flags );
}

} // ObjexxFCL

#endif // ObjexxFCL_Read_hh_INCLUDED
