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

namespace ObjexxFCL {

	// Stream
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
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
#endif

	// Stream
#if defined(_MSC_VER) && !defined(__INTEL_COMPILER) // VC++2013 covariant return bug work-around
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

} // ObjexxFCL
