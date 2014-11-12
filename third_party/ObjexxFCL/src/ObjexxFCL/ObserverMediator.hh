#ifndef ObjexxFCL_ObserverMediator_hh_INCLUDED
#define ObjexxFCL_ObserverMediator_hh_INCLUDED

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

namespace ObjexxFCL {

// Forward
class Observer;
template< typename > class SetWrapper;

// Types
typedef  Observer  Subject;

namespace internal {
namespace ObserverMediator {

// Types
typedef  SetWrapper< Observer * >  Observers;

// Notify Observers About Change in a Subject
void
notify( Subject const & s );

// Acyclic After Adding a Subject-Observer Relation?
bool
acyclic( Subject const & s, Observer & o );

// Accumulate a Subject's Observers into Accumulated Observers and Recurse: Return Acyclicity
bool
accumulate( Subject const & s_root, Subject const & s, Observers & accum_observers );

} // ObserverMediator
} // internal
} // ObjexxFCL

#endif // ObjexxFCL_ObserverMediator_hh_INCLUDED
