// IndexSlice: Index Slice Class
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
#include <ObjexxFCL/IndexSlice.hh>

// C++ Headers
#include <istream>
#include <ostream>

namespace ObjexxFCL {

	// Stream >> IndexSlice
	std::istream &
	operator >>( std::istream & stream, IndexSlice & I )
	{
		int l, u, s;
		stream >> l >> u >> s;
		I.assign( l, u, s );
		return stream;
	}

	// Stream << IndexSlice
	std::ostream &
	operator <<( std::ostream & stream, IndexSlice const & I )
	{
		if ( I.s_ == 1 ) {
			if ( I.initialized() ) {
				stream << '[' << I.l_ << ':' << I.u_ << ']';
			} else if ( I.l_initialized() ) {
				stream << '[' << I.l_ << ":]";
			} else if ( I.u_initialized() ) {
				stream << "[:" << I.u_ << ']';
			} else {
				stream << "[:]";
			}
		} else {
			if ( I.initialized() ) {
				stream << '[' << I.l_ << ':' << I.u_ << ':' << I.s_ << ']';
			} else if ( I.l_initialized() ) {
				stream << '[' << I.l_ << "::" << I.s_ << ']';
			} else if ( I.u_initialized() ) {
				stream << "[:" << I.u_ << ':' << I.s_ << ']';
			} else {
				stream << "[::" << I.s_ << ']';
			}
		}
		return stream;
	}

} // ObjexxFCL
