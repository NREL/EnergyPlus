#ifndef ObjexxFCL_ObserverMulti_hh_INCLUDED
#define ObjexxFCL_ObserverMulti_hh_INCLUDED

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
#include <ObjexxFCL/Observer.hh>

// C++ Headers
#include <cassert>

namespace ObjexxFCL {

// Forward
template< typename > class SetWrapper;

// ObserverMulti: Combined Subject + Multi Observer Abstract Base Class
class ObserverMulti : public Observer
{

public: // Types

	typedef  SetWrapper< Observer * >  Observers;

protected: // Creation

	// Default Constructor
	inline
	ObserverMulti() :
	 observers_p_( nullptr )
	{}

	// Copy Constructor
	inline
	ObserverMulti( ObserverMulti const & ) :
	 Observer(),
	 observers_p_( nullptr )
	{}

public: // Creation

	// Destructor
	virtual
	~ObserverMulti();

protected: // Assignment

	// Copy Assignment
	inline
	void
	operator =( ObserverMulti const & )
	{} // observers_p_ is identity, not value, state and is intentionally not assigned

public: // Subject Inspector

	// Insert an Observer
	void
	insert_observer( Observer & observer ) const;

	// Remove an Observer
	inline
	void
	remove_observer( Observer & observer ) const
	{
		if ( observers_p_ ) do_remove_observer( observer );
	}

	// Has At Least One Observer?
	inline
	bool
	has_observer() const
	{
		return ( observers_p_ ? do_has_observer() : false );
	}

	// Observers Pointer
	inline
	Observers const *
	observers_p() const
	{
		return observers_p_;
	}

	// Observers
	inline
	Observers const &
	observers() const
	{
		assert( observers_p_ );
		return *observers_p_;
	}

	// Notify Observers that this Subject is Being Destructed
	inline
	void
	notify_destructed() const
	{
		if ( observers_p_ ) do_notify_destructed();
	}

private: // Functions

	// Remove an Observer
	void
	do_remove_observer( Observer & observer ) const;

	// Has At Least One Observer?
	bool
	do_has_observer() const;

	// Notify Observers that this Subject is Being Destructed
	void
	do_notify_destructed() const;

private: // Data

	mutable Observers * observers_p_; // Observers of this Subject

}; // ObserverMulti

// Types
typedef  ObserverMulti  SubjectMulti;

} // ObjexxFCL

#endif // ObjexxFCL_ObserverMulti_hh_INCLUDED
