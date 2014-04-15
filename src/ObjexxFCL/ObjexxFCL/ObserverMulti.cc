// ObserverMulti: Combined Subject + Multi Observer Abstract Base Class
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
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/SetWrapper.hh>

// C++ Headers
#include <cassert>

namespace ObjexxFCL {

	// Destructor
	ObserverMulti::~ObserverMulti()
	{
		notify_destructed();
		delete observers_p_;
	}

	// Insert an Observer
	void
	ObserverMulti::insert_observer( Observer & observer ) const
	{
		assert( this != &observer );
		assert( acyclic( observer ) );
		if ( ! observers_p_ ) observers_p_ = new Observers;
		(*observers_p_)().insert( &observer );
	}

	// Remove an Observer
	void
	ObserverMulti::do_remove_observer( Observer & observer ) const
	{
		assert( observers_p_ );
		(*observers_p_)().erase( &observer );
	}

	// Has At Least One Observer?
	bool
	ObserverMulti::do_has_observer() const
	{
		assert( observers_p_ );
		return !(*observers_p_)().empty();
	}

	// Notify Observers that this Subject is Being Destructed
	void
	ObserverMulti::do_notify_destructed() const
	{
		assert( observers_p_ );
		for ( Observers::const_iterator io = (*observers_p_)().begin(), eo = (*observers_p_)().end(); io != eo; ++io ) {
			assert( *io );
			(*io)->destructed( *this );
		}
	}

} // ObjexxFCL
