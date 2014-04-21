// DimensionExpression: DimensionExpression Interface Class
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

// ObjexxFCL Headers
#include <ObjexxFCL/DimensionExpression.hh>

// C++ Headers
#include <ostream>

namespace ObjexxFCL {

// Stream Output
std::ostream &
operator <<( std::ostream & stream, DimensionExpression const & exp )
{
	stream << exp.value();
	return stream;
}

} // ObjexxFCL
