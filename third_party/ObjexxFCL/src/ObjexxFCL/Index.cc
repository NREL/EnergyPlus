// Index: Index Class
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
#include <ObjexxFCL/Index.hh>

// C++ Headers
#include <istream>
#include <ostream>

namespace ObjexxFCL {

	// Stream >> Index
	std::istream &
	operator >>( std::istream & stream, Index & a )
	{
		int i;
		stream >> i;
		a.i( i );
		return stream;
	}

	// Stream << Index
	std::ostream &
	operator <<( std::ostream & stream, Index const & a )
	{
		if ( a.initialized() ) {
			stream << "[" << a.i() << "]";
		} else {
			stream << "[]";
		}
		return stream;
	}

} // ObjexxFCL
