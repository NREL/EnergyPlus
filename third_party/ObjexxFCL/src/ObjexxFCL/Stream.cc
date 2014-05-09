// Stream Wrapper Hierarchy
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
#include <ObjexxFCL/Stream.hh>
#include <ObjexxFCL/environment.hh>
#include <ObjexxFCL/stream.functions.hh>
#include <ObjexxFCL/string.functions.hh>

// C++ Headers
#include <random>

namespace ObjexxFCL {

// Stream

#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around

	// Stream
	std::ios const &
	Stream::stream() const
	{
		if ( IStream const * p = dynamic_cast< IStream const * >( this ) ) {
			return p->stream();
		} else if ( OStream const * p = dynamic_cast< OStream const * >( this ) ) {
			return p->stream();
		} else if ( IOStream const * p = dynamic_cast< IOStream const * >( this ) ) {
			return p->stream();
		} else if ( IStringStream const * p = dynamic_cast< IStringStream const * >( this ) ) {
			return p->stream();
		} else if ( OStringStream const * p = dynamic_cast< OStringStream const * >( this ) ) {
			return p->stream();
		} else if ( StringStream const * p = dynamic_cast< StringStream const * >( this ) ) {
			return p->stream();
		} else if ( IFileStream const * p = dynamic_cast< IFileStream const * >( this ) ) {
			return p->stream();
		} else if ( OFileStream const * p = dynamic_cast< OFileStream const * >( this ) ) {
			return p->stream();
		} else if ( FileStream const * p = dynamic_cast< FileStream const * >( this ) ) {
			return p->stream();
		} else {
			return dynamic_cast< FileStream const * >( this )->stream();
		}
	}

	// Stream
	std::ios &
	Stream::stream()
	{
		if ( IStream * p = dynamic_cast< IStream * >( this ) ) {
			return p->stream();
		} else if ( OStream * p = dynamic_cast< OStream * >( this ) ) {
			return p->stream();
		} else if ( IOStream * p = dynamic_cast< IOStream * >( this ) ) {
			return p->stream();
		} else if ( IStringStream * p = dynamic_cast< IStringStream * >( this ) ) {
			return p->stream();
		} else if ( OStringStream * p = dynamic_cast< OStringStream * >( this ) ) {
			return p->stream();
		} else if ( StringStream * p = dynamic_cast< StringStream * >( this ) ) {
			return p->stream();
		} else if ( IFileStream * p = dynamic_cast< IFileStream * >( this ) ) {
			return p->stream();
		} else if ( OFileStream * p = dynamic_cast< OFileStream * >( this ) ) {
			return p->stream();
		} else if ( FileStream * p = dynamic_cast< FileStream * >( this ) ) {
			return p->stream();
		} else {
			return dynamic_cast< FileStream * >( this )->stream();
		}
	}

#endif

	// Scratch File Name
	std::string
	Stream::scratch_name()
	{
		// Directory
		std::string dir;
		dir = get_env_var( "TMP" );
		if ( dir.empty() ) {
			dir = get_env_var( "TMPDIR" );
			if ( dir.empty() ) {
				dir = get_env_var( "TEMP" );
				if ( dir.empty() ) {
					dir = get_env_var( "TEMPDIR" );
				}
			}
		}

		// Name
		static std::random_device device;
		static std::ranlux24_base generator( device() );
		static std::uniform_int_distribution<> distribution( 1000000, 9999999 );
#ifdef _WIN32
		static char const sep( '\\' );
#else
		static char const sep( '/' );
#endif
		static std::string const pre( "s" );
		static std::string const ext( ".tmp" );
		if ( dir.empty() ) {
			return pre + string_of( distribution( generator ) ) + ext;
		} else {
			return dir + sep + pre + string_of( distribution( generator ) ) + ext;
		}
	}

// OFileStream

	// Open
	void
	OFileStream::open_file()
	{
		if ( ! name().empty() ) {
			if ( append() ) { // Try to auto-detect line terminator
				std::ifstream is( name(), std::ios_base::in | std::ios_base::binary );
				std::string const auto_ter( line_terminator( is ) );
				is.close();
				if ( ! auto_ter.empty() ) flags().ter( auto_ter );
			}
			stream_.open( name(), mode() );
			if ( ! stream_.is_open() && scratch() ) { // Try other scratch file names
				int n( 0 ); // Number of tries
				while ( ( ! stream_.is_open() ) && ( n < 1000 ) ) {
					stream_.clear();
					name( scratch_name() );
					stream_.open( name(), mode() );
					++n;
				}
			}
		}
	}

// FileStream

	// Open
	void
	FileStream::open_file()
	{
		if ( ! name().empty() ) {
			stream_.open( name(), mode() );
			if ( ! stream_.is_open() ) {
				if ( ! old() && ! scratch() && ! std::ifstream( name() ).good() ) {
					std::ios_base::openmode mode_( mode() );
					if ( ! ( mode_ & std::ios_base::trunc ) ) {
						mode_ |= std::ios_base::trunc;
						stream_.clear();
						stream_.open( name(), mode_ ); // Try as a new file
					}
				}
				if ( ! stream_.is_open() && scratch() ) { // Try other scratch file names
					int n( 0 ); // Number of tries
					while ( ( ! stream_.is_open() ) && ( n < 1000 ) ) {
						stream_.clear();
						name( scratch_name() );
						stream_.open( name(), mode() );
						++n;
					}
				}
			}
			if ( stream_.is_open() ) { // Try to auto-detect line terminator
				std::string const auto_ter( line_terminator( stream_ ) );
				if ( ! auto_ter.empty() ) flags().ter( auto_ter );
			}
		}
	}

} // ObjexxFCL
