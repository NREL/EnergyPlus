#ifndef ObjexxFCL_rvalue_cast_hh_INCLUDED
#define ObjexxFCL_rvalue_cast_hh_INCLUDED

// rvalue_cast: Wrapper for Passing an rvalue as a non-const Reference Argument
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

namespace ObjexxFCL {

// rvalue_cast: Wrapper for Passing an rvalue as a non-const Reference Argument
template< typename T >
inline
T &
rvalue_cast( T const & t ) {
	return const_cast< T & >( t );
}

} // ObjexxFCL

#endif // ObjexxFCL_rvalue_cast_hh_INCLUDED
