#ifndef ObjexxFCL_ArrayTraits_hh_INCLUDED
#define ObjexxFCL_ArrayTraits_hh_INCLUDED

// ArrayTraits: Array Traits Template
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

// ArrayTraits: Type Traits Template
template< class A >
struct ArrayTraits
{
	typedef  A  traits_type;

	// Is the type an array
	inline
	static
	bool
	is_array()
	{
		return false;
	}

};

} // ObjexxFCL

#endif // ObjexxFCL_ArrayTraits_hh_INCLUDED
