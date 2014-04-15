// ObserverMediator: Observer Mediator Functions
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
#include <ObjexxFCL/ObserverMediator.hh>
#include <ObjexxFCL/ObserverSingle.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/ObserverGraph.hh>
#include <ObjexxFCL/SetWrapper.hh>

namespace ObjexxFCL {
namespace internal {
namespace ObserverMediator {

// Notify Observers About Change in a Subject
void
notify( Subject const & s )
{
	internal::ObserverGraph graph( s );
	if ( ! graph.empty() ) {
		while ( Observer * const o_p = graph.pop() ) {
			assert( o_p );
			o_p->update();
		}
	}
}

// Acyclic After Adding a Subject-Observer Relation?
bool
acyclic( Subject const & s, Observer & o )
{
	if ( Subject const * const ss_p = dynamic_cast< Subject const * >( &o ) ) { // Observer is a Subject
		Subject const & ss( *ss_p );
		if ( ss_p == &s ) { // Two Cycle
			return false;
		} else { // Check subgraph rooted at Observer
			Observers accum_observers; // All Observers in subgraph
			return accumulate( s, ss, accum_observers );
		}
	} else { // Observer is not a Subject so this relation can't close a cycle
		return true;
	}
}

// Accumulate a Subject's Transitive Observers and Return Acyclicity
bool
accumulate( Subject const & s_root, Subject const & s, Observers & accum_observers )
{
	if ( SubjectSingle const * const ss_p = dynamic_cast< SubjectSingle const * >( &s ) ) { // Single Observer
		Observer * const o_p( ss_p->observer_p() );
		if ( o_p ) { // Subject has an Observer
			if ( accum_observers().find( o_p ) == accum_observers().end() ) { // New Observer
				accum_observers().insert( o_p ); // Add it
				if ( ( o_p == &s ) || ( o_p == &s_root ) ) return false; // Cyclic
				if ( ! accumulate( s_root, *o_p, accum_observers ) ) return false; // Recurse: Abort if cyclic
			}
		}
	} else if ( SubjectMulti const * const sm_p = dynamic_cast< SubjectMulti const * >( &s ) ) { // Multi Observer
		if ( sm_p->observers_p() ) { // Subject has Observers
			ObserverMulti::Observers const & observers( sm_p->observers() );
			for ( ObserverMulti::Observers::const_iterator io = observers().begin(), eo = observers().end(); io != eo; ++io ) {
				Observer * const o_p( *io );
				if ( accum_observers().find( o_p ) == accum_observers().end() ) { // New Observer
					accum_observers().insert( o_p ); // Add it
					if ( ( o_p == &s ) || ( o_p == &s_root ) ) return false; // Cyclic
					if ( ! accumulate( s_root, *o_p, accum_observers ) ) return false; // Recurse: Abort if cyclic
				}
			}
		}
	}
	return true;
}

} // ObserverMediator
} // internal
} // ObjexxFCL
