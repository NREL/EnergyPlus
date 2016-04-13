#ifndef ObjexxFCL_ChunkExponent_hh_INCLUDED
#define ObjexxFCL_ChunkExponent_hh_INCLUDED

// ChunkExponent: ChunkVector Exponent Wrapper for Function Disambiguation and Range Clipping
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
#include <algorithm>
#include <cstddef>
#include <limits>

namespace ObjexxFCL {

// ChunkExponent: ChunkVector Exponent Wrapper for Function Disambiguation and Range Clipping
//
// Note:
//  The exponent is clipped to be less than the number of bits in its type so that 2^exponent can be stored in that type
class ChunkExponent
{

public: // Types

	typedef  std::size_t  T;
	typedef  T  value_type;

public: // Creation

	// Constructor (Implicit): Clips Exponent to Valid Range
	ChunkExponent( T const exponent ) :
	 exponent_( std::min( exponent, static_cast< T >( std::numeric_limits< T >::digits - 1 ) ) )
	{}

	// Destructor
	~ChunkExponent()
	{}

public: // Conversion

	// Exponent Value Conversion
	operator T() const
	{
		return exponent_;
	}

private: // Data

	T exponent_; // Exponent value

}; // ChunkExponent

} // ObjexxFCL

#endif // ObjexxFCL_ChunkExponent_hh_INCLUDED
