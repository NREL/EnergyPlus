#ifndef ObjexxFCL_BArray_hh_INCLUDED
#define ObjexxFCL_BArray_hh_INCLUDED

// BArray: Array Abstract Base Class
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
#include <ObjexxFCL/noexcept.hh>

namespace ObjexxFCL {

// BArray: Array Abstract Base Class
class BArray
{

protected: // Creation

	// Default Constructor
	BArray()
	{}

	// Copy Constructor
	BArray( BArray const & )
	{}

	// Move Constructor
	BArray( BArray && ) NOEXCEPT
	{}

public: // Creation

	// Destructor
	virtual
	~BArray()
	{}

protected: // Assignment

	// Copy Assignment
	void
	operator =( BArray const & )
	{}

	// Move Assignment
	void
	operator =( BArray && ) NOEXCEPT
	{}

}; // BArray

} // ObjexxFCL

#endif // ObjexxFCL_BArray_hh_INCLUDED
