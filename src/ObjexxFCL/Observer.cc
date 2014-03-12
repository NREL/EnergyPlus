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

// ObjexxFCL Headers
#include <ObjexxFCL/Observer.hh>
#include <ObjexxFCL/ObserverMediator.hh>

namespace ObjexxFCL {

	// Notify Observers that this Subject has Changed
	void
	Observer::notify() const
	{
		if ( has_observer() ) internal::ObserverMediator::notify( *this );
	}

	// Acyclic After Adding an Observer of this Subject?
	bool
	Observer::acyclic( Observer & observer ) const
	{
		return internal::ObserverMediator::acyclic( *this, observer );
	}

} // ObjexxFCL
