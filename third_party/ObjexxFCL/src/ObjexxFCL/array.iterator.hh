#ifndef ObjexxFCL_array_iterator_hh_INCLUDED
#define ObjexxFCL_array_iterator_hh_INCLUDED

// C Array Iterator Functions
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

// C++ Headers
#include <cstddef>

namespace ObjexxFCL {

// Begin Iterator for C Array
template< typename T, std::size_t N >
inline
T *
begin( T (&a)[N] )
{
	return a + 0;
}

// End Iterator for C Array
template< typename T, std::size_t N >
inline
T *
end( T (&a)[N] )
{
	return a + N;
}

} // ObjexxFCL

#endif // ObjexxFCL_array_iterator_hh_INCLUDED
