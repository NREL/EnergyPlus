#ifndef ObjexxFCL_BArray_hh_INCLUDED
#define ObjexxFCL_BArray_hh_INCLUDED

// BArray: Array Abstract Base Class
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

namespace ObjexxFCL {

// BArray: Array Abstract Base Class
class BArray
{

protected: // Creation

	// Default Constructor
	inline
	BArray()
	{}

	// Copy Constructor
	inline
	BArray( BArray const & )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~BArray()
	{}

protected: // Assignment

	// Copy Assignment
	inline
	void
	operator =( BArray const & )
	{}

}; // BArray

} // ObjexxFCL

#endif // ObjexxFCL_BArray_hh_INCLUDED
