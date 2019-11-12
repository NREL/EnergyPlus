#ifndef ObjexxFCL_SetWrapper_hh_INCLUDED
#define ObjexxFCL_SetWrapper_hh_INCLUDED

// Insulating Wrapper of std::set that can be Forward Declared
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

// C++ Headers
#include <set>

namespace ObjexxFCL {

// Insulating Wrapper of std::set that can be Forward Declared
// For objects that manage their own memory not pointers to owned objects
template< typename T >
class SetWrapper
{

public: // Types

	using Container = std::set< T >;

	// STL style
	using value_type = T;
	using iterator = typename Container::iterator;
	using const_iterator = typename Container::const_iterator;

	// C++ style
	using Value = T;
	using Iterator = typename Container::iterator;
	using ConstIterator = typename Container::const_iterator;

public: // Operator

	// set Accessor
	Container const &
	operator ()() const
	{
		return container_;
	}

	// set Accessor
	Container &
	operator ()()
	{
		return container_;
	}

private: // Data

	Container container_; // std::set being wrapped

}; // SetWrapper

} // ObjexxFCL

#endif // ObjexxFCL_SetWrapper_hh_INCLUDED
