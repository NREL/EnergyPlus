#ifndef ObjexxFCL_SetWrapper_hh_INCLUDED
#define ObjexxFCL_SetWrapper_hh_INCLUDED

// SetWrapper: Insulating Wrapper of std::set that can be Forward Declared
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
#include <set>

namespace ObjexxFCL {

// SetWrapper: Insulating Wrapper of std::set that can be Forward Declared
// For objects that manage their own memory not pointers to owned objects
template< typename T >
class SetWrapper
{

public: // Types

	typedef  std::set< T >  Container;

	// STL style
	typedef  T  value_type;
	typedef  typename Container::iterator  iterator;
	typedef  typename Container::const_iterator  const_iterator;

	// C++ style
	typedef  T  Value;
	typedef  typename Container::iterator  Iterator;
	typedef  typename Container::const_iterator  ConstIterator;

public: // Creation

	// Default Constructor
	SetWrapper()
	{}

	// Destructor
	~SetWrapper()
	{}

public: // Inspector

	// set Accessor
	Container const &
	operator ()() const
	{
		return container_;
	}

public: // Modifier

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
