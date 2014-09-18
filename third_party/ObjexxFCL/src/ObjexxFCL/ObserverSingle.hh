#ifndef ObjexxFCL_ObserverSingle_hh_INCLUDED
#define ObjexxFCL_ObserverSingle_hh_INCLUDED

// ObserverSingle: Combined Subject + Single Observer Abstract Base Class
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

// ObserverSingle: Combined Subject + Single Observer Abstract Base Class
class ObserverSingle : public Observer
{

protected: // Creation

	// Default Constructor
	inline
	ObserverSingle() :
	 observer_p_( nullptr )
	{}

	// Copy Constructor
	inline
	ObserverSingle( ObserverSingle const & ) :
	 Observer(),
	 observer_p_( nullptr )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~ObserverSingle()
	{
		notify_destructed();
	}

protected: // Assignment

	// Copy Assignment
	inline
	void
	operator =( ObserverSingle const & )
	{} // observer_p_ is identity, not value, state and is intentionally not assigned

public: // Subject Inspector

	// Insert an Observer
	inline
	void
	insert_observer( Observer & observer ) const
	{
		assert( acyclic( observer ) );
		assert( ( ! observer_p_ ) || ( observer_p_ == &observer ) );
		observer_p_ = &observer;
	}

	// Remove an Observer
	inline
	void
	remove_observer( Observer & observer ) const
	{
		if ( observer_p_ == &observer ) observer_p_ = nullptr;
	}

	// Has At Least One Observer?
	inline
	bool
	has_observer() const
	{
		return ( observer_p_ != nullptr );
	}

	// Notify Observers that this Subject is Being Destructed
	inline
	void
	notify_destructed() const
	{
		if ( observer_p_ ) observer_p_->destructed( *this );
	}

	// Observer
	inline
	Observer *
	observer_p() const
	{
		return observer_p_;
	}

private: // Data

	mutable Observer * observer_p_; // Observer of this Subject (non-owning pointer)

}; // ObserverSingle

// Types
typedef  ObserverSingle  SubjectSingle;

} // ObjexxFCL

#endif // ObjexxFCL_ObserverSingle_hh_INCLUDED
