#ifndef ObjexxFCL_BArray_hh_INCLUDED
#define ObjexxFCL_BArray_hh_INCLUDED

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
#include <ObjexxFCL/noexcept.hh>
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/IndexSlice.hh>

namespace ObjexxFCL {

// BArray: Array Abstract Base Class
class BArray
{

public: // Types

	typedef  IndexRange  IR;
	typedef  IndexSlice  IS;
	typedef  DimensionSlice  DS;

	// STL style
	typedef  std::size_t  size_type;
	typedef  std::ptrdiff_t  difference_type;

	// C++ style
	typedef  std::size_t  Size;
	typedef  std::ptrdiff_t  Difference;

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

public: // Static Data

	static size_type const npos; // Unbounded "size"
	static size_type const max_size; // Max array size

}; // BArray

} // ObjexxFCL

#endif // ObjexxFCL_BArray_hh_INCLUDED
