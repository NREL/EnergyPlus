#ifndef ObjexxFCL_BArray_hh_INCLUDED
#define ObjexxFCL_BArray_hh_INCLUDED

// Array Abstract Base Class
//
// Project: Objexx Fortran-C++ Library (ObjexxFCL)
//
// Version: 4.3.0
//
// Language: C++
//
// Copyright (c) 2000-2019 Objexx Engineering, Inc. All Rights Reserved.
// Use of this source code or any derivative of it is restricted by license.
// Licensing is available from Objexx Engineering, Inc.: https://objexx.com

// ObjexxFCL Headers
#include <ObjexxFCL/noexcept.hh>
#include <ObjexxFCL/DimensionSlice.hh>
#include <ObjexxFCL/IndexRange.hh>
#include <ObjexxFCL/IndexSlice.hh>

namespace ObjexxFCL {

// Array Abstract Base Class
class BArray
{

public: // Types

	using IR = IndexRange;
	using IS = IndexSlice;
	using DS = DimensionSlice;

	// STL style
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	// C++ style
	using Size = std::size_t;
	using Difference = std::ptrdiff_t;

protected: // Creation

	// Default Constructor
	BArray() = default;

	// Copy Constructor
	BArray( BArray const & ) = default;

	// Move Constructor
	BArray( BArray && ) NOEXCEPT = default;

public: // Creation

	// Destructor
	virtual
	~BArray() = default;

protected: // Assignment

	// Copy Assignment
	BArray &
	operator =( BArray const & ) = default;

	// Move Assignment
	BArray &
	operator =( BArray && ) NOEXCEPT = default;

public: // Static Data

	static size_type const npos{ static_cast< size_type >( -1 ) }; // Unbounded "size"
	static size_type const max_size{ static_cast< size_type >( -1 ) - static_cast< size_type >( 1 ) }; // Max array size

}; // BArray

} // ObjexxFCL

#endif // ObjexxFCL_BArray_hh_INCLUDED
