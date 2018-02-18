// BArray: Array Abstract Base Class
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.2.0
//
// Language: C++
//
// Copyright (c) 2000-2017 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.:  http://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/BArray.hh>

namespace ObjexxFCL {

	// Static Data Member Definitions
	BArray::size_type const BArray::npos = static_cast< size_type >( -1 );
	BArray::size_type const BArray::max_size = static_cast< size_type >( -1 ) - static_cast< size_type >( 1 );

} // ObjexxFCL
