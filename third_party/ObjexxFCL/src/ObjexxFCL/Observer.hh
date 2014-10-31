#ifndef ObjexxFCL_Observer_hh_INCLUDED
#define ObjexxFCL_Observer_hh_INCLUDED

// Observer: Combined Subject + Observer Abstract Base Class
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

// Forward
class Observer;

// Types
typedef  Observer  Subject;

// Observer: Combined Subject + Observer Abstract Base Class
class Observer
{

protected: // Creation

	// Default Constructor
	inline
	Observer()
	{}

	// Copy Constructor
	inline
	Observer( Observer const & )
	{}

public: // Creation

	// Destructor
	inline
	virtual
	~Observer()
	{}

protected: // Assignment

	// Copy Assignment
	inline
	void
	operator =( Observer const & )
	{}

public: // Subject Inspector

	// Insert an Observer
	virtual
	void
	insert_observer( Observer & ) const = 0;

	// Remove an Observer
	virtual
	void
	remove_observer( Observer & ) const = 0;

	// Has At Least One Observer?
	virtual
	bool
	has_observer() const = 0;

	// Notify Observers that this Subject has Changed
	void
	notify() const;

	// Acyclic After Adding an Observer of this Subject?
	bool
	acyclic( Observer & ) const;

public: // Observer Modifier

	// Update
	virtual
	void
	update() = 0;

	// Update for Destruction of a Subject
	virtual
	void
	destructed( Subject const & ) = 0;

}; // Observer

} // ObjexxFCL

#endif // ObjexxFCL_Observer_hh_INCLUDED
