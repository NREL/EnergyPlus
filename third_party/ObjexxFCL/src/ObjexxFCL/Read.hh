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

namespace ObjexxFCL {

// Base Class for Formatted Read
class ReadBase
{

protected: // Creation

	// Constructor
	inline
	ReadBase()
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~ReadBase()
	{}

private: // Creation

	// Copy Constructor
	ReadBase( ReadBase const & ); // Disallow

private: // Assignment

	// Copy Assignment
	ReadBase &
	operator =( ReadBase const & ); // Disallow

protected: // Properties

	// Stream
	virtual
	std::istream const &
	stream() const = 0;

	// Stream
	virtual
	std::istream &
	stream() = 0;

	// Absolute Stream Position
	inline
	virtual
	std::streampos
	poa() const
	{ // Default implementation
		return 0;
	}

	// Relative Virtual Stream Position
	virtual
	std::streampos
	por() const = 0;

	// Relative Virtual Stream Position
	virtual
	std::streampos &
	por() = 0;

	// Stream Position
	virtual
	std::streampos
	pos() const = 0;

	// Stream Position Set
	virtual
	void
	pos( std::streampos const pos ) = 0;

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

public: // Operators

	// Stream Input
	template< typename T, class = typename std::enable_if< ! std::is_base_of< BArray, T >::value >::type >
	inline
	ReadBase &
	operator >>( T & t )
	{
		auto & stream_( stream() );
		if ( stream_ ) {
			auto const format_( format() );
			if ( format_ && format_->not_slash_terminated() ) {
				Format::Size const reverts( format_->reverts() );
				Format * active( format_->current() );
				while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts ) && format_->not_slash_terminated() && active->input( stream_, poa(), por() ) ) { // Inputs up to arg-based format
					active = active->next();
				}
				if ( stream_ && active && active->uses_arg() && format_->not_slash_terminated() && active->input( stream_, poa(), por(), t ) ) { // Input arg using active format
					Format::Size const reverts( format_->reverts() );
					active = active->next();
					while ( stream_ && active && active->no_arg() && ( format_->reverts() == reverts ) && format_->not_terminated() && active->input( stream_, poa(), por() ) ) { // Inputs up to next arg-based format if not terminated
						active = active->next();
					}
				}
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: complex Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( std::complex< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ ) {
			auto const format_( format() );
			if ( format_ ) {
				bool const ld( format_->is_list_directed() );
				bool bad( false );
				if ( ld ) {
					auto & por_( por() );
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
						auto & por_( por() );
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
		pos( stream_.tellg() );
		status_set();
		return *this;
	}

	// Stream Input: FArray Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( typename FArray< T >::size_type i = 0; i < t.size(); ++i ) {
				*this >> t[ i ];
				if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray1S< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray2S< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray3S< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray4S< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray5S< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray6S< T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray1S< T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray2S< T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray3S< T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray4S< T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray5S< T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	ReadBase &
	operator >>( FArray6S< T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray1< A, T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray2< A, T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray3< A, T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray4< A, T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray5< A, T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray6< A, T > & t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray1< A, T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i = 1, e = t.u(); i <= e; ++i ) {
				*this >> t( i );
				if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray2< A, T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					*this >> t( i1, i2 );
					if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray3< A, T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
			for ( int i1 = 1, e1 = t.u1(); i1 <= e1; ++i1 ) {
				for ( int i2 = 1, e2 = t.u2(); i2 <= e2; ++i2 ) {
					for ( int i3 = 1, e3 = t.u3(); i3 <= e3; ++i3 ) {
						*this >> t( i1, i2, i3 );
						if ( ! stream_ ) break;
					} if ( ! stream_ ) break;
				} if ( ! stream_ ) break;
			}
		}
		status_set();
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray4< A, T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray5< A, T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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
		status_set();
		return *this;
	}

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	ReadBase &
	operator >>( MArray6< A, T > && t )
	{
		auto & stream_( stream() );
		if ( stream_ && format() ) {
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

}; // ReadBase

class ReadStream : public ReadBase
{

public: // Creation

	// Format String Constructor
	inline
	ReadStream( std::istream & stream, std::string const & fmt, IOFlags & flags ) :
	 stream_( stream ),
	 poa_( stream.tellg() ),
	 por_( 0 ),
	 format_( FormatFactory::create( fmt ) ),
	 flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->blank_zero() = flags_.blank_zero();
			format_->non_advancing() = flags_.non_advancing(); // Allowed with list-directed format but Fortran doesn't
		}
	}

	// Format Wrapper Constructor
	inline
	ReadStream( std::istream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 stream_( stream ),
	 poa_( stream.tellg() ),
	 por_( 0 ),
	 format_( fmt.format_clone() ),
	 flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->blank_zero() = flags_.blank_zero();
			format_->non_advancing() = flags_.non_advancing(); // Allowed with list-directed format but Fortran doesn't
		}
	}

	// Destructor
	inline
	virtual
	~ReadStream()
	{
		if ( format_ ) {
			if ( stream_ ) {
				if ( format_->non_advancing() ) { // Non-advancing
					format_->input_pos( stream_, pos() ); // Set final stream position
				} else { // Advancing
					stream_ >> Format::skip; // Advance to next line
				}
			}
			delete format_;
		}
	}

protected: // Properties

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

	// Absolute Initial Stream Position
	inline
	std::streampos
	poa() const
	{
		return poa_;
	}

	// Relative Virtual Stream Position
	inline
	std::streampos
	por() const
	{
		return por_;
	}

	// Relative Virtual Stream Position
	inline
	std::streampos &
	por()
	{
		return por_;
	}

	// Stream Position
	inline
	std::streampos
	pos() const
	{
		return poa_ + por_;
	}

	// Stream Position Set
	inline
	void
	pos( std::streampos const pos )
	{
		por_ = pos - poa_;
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

	std::istream & stream_; // Input stream
	std::streampos const poa_; // Absolute initial stream position
	std::streampos por_; // Relative virtual stream position
	Format * format_; // Format expression
	IOFlags & flags_; // I/o flags

}; // ReadStream

class ReadString : public ReadBase
{

public: // Creation

	// Format String Constructor
	inline
	ReadString( std::string const & str, std::string const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 pos_( 0 ),
	 format_( FormatFactory::create( fmt ) ),
	 flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->blank_zero() = flags_.blank_zero();
		}
	}

	// Format Wrapper Constructor
	inline
	ReadString( std::string const & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 pos_( 0 ),
	 format_( fmt.format_clone() ),
	 flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->blank_zero() = flags_.blank_zero();
		}
	}

	// Destructor
	inline
	virtual
	~ReadString()
	{
		if ( format_ ) delete format_;
	}

protected: // Properties

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

	// Relative Virtual Stream Position
	inline
	std::streampos
	por() const
	{
		return pos_;
	}

	// Relative Virtual Stream Position
	inline
	std::streampos &
	por()
	{
		return pos_;
	}

	// Stream Position
	inline
	std::streampos
	pos() const
	{
		return pos_;
	}

	// Stream Position Set
	virtual
	void
	pos( std::streampos const pos )
	{
		pos_ = pos;
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

	std::istringstream stream_; // Internal stream
	std::streampos pos_; // Stream position
	Format * format_; // Format expression
	IOFlags & flags_; // I/o flags

}; // ReadString

class ReadFstring : public ReadBase
{

public: // Creation

	// Format String Constructor
	inline
	ReadFstring( Fstring const & str, std::string const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 pos_( 0 ),
	 format_( FormatFactory::create( fmt ) ),
	 flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->blank_zero() = flags_.blank_zero();
		}
	}

	// Format Wrapper Constructor
	inline
	ReadFstring( Fstring const & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 stream_( str ),
	 pos_( 0 ),
	 format_( fmt.format_clone() ),
	 flags_( flags )
	{
		flags_.clear_status();
		if ( format_ ) {
			format_->blank_zero() = flags_.blank_zero();
		}
	}

	// Destructor
	inline
	virtual
	~ReadFstring()
	{
		if ( format_ ) delete format_;
	}

protected: // Properties

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

	// Relative Virtual Stream Position
	inline
	std::streampos
	por() const
	{
		return pos_;
	}

	// Relative Virtual Stream Position
	inline
	std::streampos &
	por()
	{
		return pos_;
	}

	// Stream Position
	inline
	std::streampos
	pos() const
	{
		return pos_;
	}

	// Stream Position Set
	virtual
	void
	pos( std::streampos const pos )
	{
		pos_ = pos;
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

	std::istringstream stream_; // Internal stream
	std::streampos pos_; // Stream position
	Format * format_; // Format expression
	IOFlags & flags_; // I/o flags

}; // ReadFstring

// Read Wrapper Factory
class Read
{

public: // Creation

	// Default Constructor
	inline
	Read() :
	 read_( nullptr )
	{}

	// Move Constructor
	inline
	Read( Read && r ) :
	 flags_( r.flags_ ),
	 read_( r.read_ )
	{
		r.read_ = nullptr;
	}

	// Flags Constructor
	inline
	explicit
	Read( IOFlags & ) :
	 read_( nullptr )
	{}

	// Stream + Format Constructor
	inline
	Read( std::istream & stream, std::string const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( stream.rdbuf() == std::cin.rdbuf() ? nullptr : new ReadStream( stream, fmt, flags_ ) )
	{
		if ( stream.rdbuf() == std::cin.rdbuf() ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			read_ = new ReadString( s, fmt, flags_ );
			flags_.set_status( stream );
		}
	}

	// Stream + Format Constructor
	inline
	Read( std::istream & stream, gio::Fmt const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( stream.rdbuf() == std::cin.rdbuf() ? nullptr : new ReadStream( stream, fmt, flags_ ) )
	{
		if ( stream.rdbuf() == std::cin.rdbuf() ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			read_ = new ReadString( s, fmt, flags_ );
			flags_.set_status( stream );
		}
	}

	// Stream + Format + Flags Constructor
	inline
	Read( std::istream & stream, std::string const & fmt, IOFlags & flags ) :
	 read_( stream.rdbuf() == std::cin.rdbuf() ? nullptr : new ReadStream( stream, fmt, flags ) )
	{
		if ( stream.rdbuf() == std::cin.rdbuf() ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			read_ = new ReadString( s, fmt, flags );
			flags.set_status( stream );
		}
	}

	// Stream + Format + Flags Constructor
	inline
	Read( std::istream & stream, gio::Fmt const & fmt, IOFlags & flags ) :
	 read_( stream.rdbuf() == std::cin.rdbuf() ? nullptr : new ReadStream( stream, fmt, flags ) )
	{
		if ( stream.rdbuf() == std::cin.rdbuf() ) { // Do the stdin read
			std::string s;
			cross_platform_get_line( stream, s );
			read_ = new ReadString( s, fmt, flags );
			flags.set_status( stream );
		}
	}

	// String + Format Constructor
	inline
	Read( std::string const & str, std::string const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( new ReadString( str, fmt, flags_ ) )
	{}

	// String + Format Constructor
	inline
	Read( std::string const & str, gio::Fmt const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( new ReadString( str, fmt, flags_ ) )
	{}

	// String + Format + Flags Constructor
	inline
	Read( std::string const & str, std::string const & fmt, IOFlags & flags ) :
	 read_( new ReadString( str, fmt, flags ) )
	{}

	// String + Format + Flags Constructor
	inline
	Read( std::string const & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 read_( new ReadString( str, fmt, flags ) )
	{}

	// Fstring + Format Constructor
	inline
	Read( Fstring const & str, std::string const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( new ReadFstring( str, fmt, flags_ ) )
	{}

	// Fstring + Format Constructor
	inline
	Read( Fstring const & str, gio::Fmt const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( new ReadFstring( str, fmt, flags_ ) )
	{}

	// Fstring + Format + Flags Constructor
	inline
	Read( Fstring const & str, std::string const & fmt, IOFlags & flags ) :
	 read_( new ReadFstring( str, fmt, flags ) )
	{}

	// Fstring + Format + Flags Constructor
	inline
	Read( Fstring const & str, gio::Fmt const & fmt, IOFlags & flags ) :
	 read_( new ReadFstring( str, fmt, flags ) )
	{}

	// C-String + Format Constructor
	inline
	Read( char const * str, std::string const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( new ReadString( std::string( str ), fmt, flags_ ) )
	{}

	// C-String + Format Constructor
	inline
	Read( char const * str, gio::Fmt const & fmt ) :
	 flags_( IOFlags::handler() ),
	 read_( new ReadString( std::string( str ), fmt, flags_ ) )
	{}

	// C-String + Format + Flags Constructor
	inline
	Read( char const * str, std::string const & fmt, IOFlags & flags ) :
	 read_( new ReadString( std::string( str ), fmt, flags ) )
	{}

	// C-String + Format + Flags Constructor
	inline
	Read( char const * str, gio::Fmt const & fmt, IOFlags & flags ) :
	 read_( new ReadString( std::string( str ), fmt, flags ) )
	{}

	// Destructor
	inline
	~Read()
	{
		if ( read_ ) delete read_;
	}

private: // Creation

	// Copy Constructor
	Read( Read const & ); // Disallow

private: // Assignment

	// Copy Assignment
	Read &
	operator =( Read const & ); // Disallow

public: // Operators

	// Stream Input
	template< typename T >
	inline
	Read &
	operator >>( T & t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: FArray1S Overload
	template< typename T >
	inline
	Read &
	operator >>( FArray1S< T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: FArray2S Overload
	template< typename T >
	inline
	Read &
	operator >>( FArray2S< T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: FArray3S Overload
	template< typename T >
	inline
	Read &
	operator >>( FArray3S< T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: FArray4S Overload
	template< typename T >
	inline
	Read &
	operator >>( FArray4S< T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: FArray5S Overload
	template< typename T >
	inline
	Read &
	operator >>( FArray5S< T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: FArray6S Overload
	template< typename T >
	inline
	Read &
	operator >>( FArray6S< T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: MArray1 Overload
	template< class A, typename T >
	inline
	Read &
	operator >>( MArray1< A, T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: MArray2 Overload
	template< class A, typename T >
	inline
	Read &
	operator >>( MArray2< A, T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: MArray3 Overload
	template< class A, typename T >
	inline
	Read &
	operator >>( MArray3< A, T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: MArray4 Overload
	template< class A, typename T >
	inline
	Read &
	operator >>( MArray4< A, T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: MArray5 Overload
	template< class A, typename T >
	inline
	Read &
	operator >>( MArray5< A, T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

	// Stream Input: MArray6 Overload
	template< class A, typename T >
	inline
	Read &
	operator >>( MArray6< A, T > && t )
	{
		if ( read_ ) *read_ >> t;
		return *this;
	}

private: // Data

	IOFlags flags_; // Internal i/o flags
	ReadBase * read_; // Implementation object

}; // Read

} // ObjexxFCL

#endif // ObjexxFCL_Read_hh_INCLUDED
