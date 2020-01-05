#ifndef ObjexxFCL_rvalue_cast_hh_INCLUDED
#define ObjexxFCL_rvalue_cast_hh_INCLUDED

// Wrapper for Passing an rvalue as a non-const Reference Argument
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2020 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

namespace ObjexxFCL {

// Wrapper for Passing an rvalue as a non-const Reference Argument
template< typename T >
inline
T &
rvalue_cast( T const & t ) {
	return const_cast< T & >( t );
}

} // ObjexxFCL

#endif // ObjexxFCL_rvalue_cast_hh_INCLUDED
