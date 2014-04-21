// ObserverGraph: Observer Graph Representation
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
#include <ObjexxFCL/ObserverGraph.hh>
#include <ObjexxFCL/ObserverSingle.hh>
#include <ObjexxFCL/ObserverMulti.hh>
#include <ObjexxFCL/SetWrapper.hh>

// C++ Headers
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <utility>

namespace ObjexxFCL {
namespace internal {

	// Subject Constructor
	ObserverGraph::ObserverGraph( Subject const & s )
	{
		// Construct the graph with zero in-degree counts
		if ( ! push( s, s ) ) {
#ifndef OBJEXXFCL_ERROR_SUPPRESS
			std::cerr << "\nObjexxFCL Error: Cyclic FArray/Dimension dependency detected" << std::endl;
			std::exit( EXIT_FAILURE );
#endif
			assert( false ); // Cyclic FArray/Dimension dependency
			return; // Can't continue if cyclic
		}

		// Set the in-degree counts
		for ( Graph::iterator ig = graph_.begin(), eg = graph_.end(); ig != eg; ++ig ) {
			Observer * const observer_p( ig->first );
			if ( ObserverSingle * const os_p = dynamic_cast< ObserverSingle * >( observer_p ) ) { // Single Observer
				Observer * const oso_p( os_p->observer_p() );
				if ( oso_p ) {
					assert( graph_.find( oso_p ) != graph_.end() );
					++graph_[ oso_p ]; // Increment the in-degree count
				}
			} else if ( ObserverMulti * const om_p = dynamic_cast< ObserverMulti * >( observer_p ) ) { // Multi Observer
				if ( om_p->observers_p() ) {
					ObserverMulti::Observers const & observers( om_p->observers() );
					for ( ObserverMulti::Observers::const_iterator io = observers().begin(), eo = observers().end(); io != eo; ++io ) {
						assert( graph_.find( *io ) != graph_.end() );
						++graph_[ *io ]; // Increment the in-degree count
					}
				}
			}
		}

		// Set the sources
		for ( Graph::iterator ig = graph_.begin(), eg = graph_.end(); ig != eg; ++ig ) {
			if ( ig->second == 0 ) sources_.push_back( ig ); // In-degree == zero => Source Observer
		}
		assert( ( ! sources_.empty() ) || ( graph_.empty() ) );
	}

	// Push a Subject's Transitive Observers onto Graph and Return Acyclicity
	bool
	ObserverGraph::push( Subject const & s_root, Subject const & s )
	{
		if ( SubjectSingle const * const ss_p = dynamic_cast< SubjectSingle const * >( &s ) ) { // Single Observer
			Observer * const o_p( ss_p->observer_p() );
			if ( o_p ) { // Subject has an Observer
				if ( graph_.find( o_p ) == graph_.end() ) { // New Observer
					graph_.insert( std::make_pair( o_p, static_cast< size_type >( 0 ) ) ); // Add it
					if ( ( o_p == &s ) || ( o_p == &s_root ) ) return false; // Cyclic
					if ( ! push( s_root, *o_p ) ) return false; // Recurse: Abort if cyclic
				}
			}
		} else if ( SubjectMulti const * const sm_p = dynamic_cast< SubjectMulti const * >( &s ) ) { // Multi Observer
			if ( sm_p->observers_p() ) { // Subject has Observers
				ObserverMulti::Observers const & observers( sm_p->observers() );
				for ( ObserverMulti::Observers::const_iterator io = observers().begin(), eo = observers().end(); io != eo; ++io ) {
					Observer * const o_p( *io );
					if ( graph_.find( o_p ) == graph_.end() ) { // New Observer
						graph_.insert( std::make_pair( o_p, static_cast< size_type >( 0 ) ) ); // Add it
						if ( ( o_p == &s ) || ( o_p == &s_root ) ) return false; // Cyclic
						if ( ! push( s_root, *o_p ) ) return false; // Recurse: Abort if cyclic
					}
				}
			}
		}
		return true;
	}

	// Pop a Source Observer from Graph
	Observer *
	ObserverGraph::pop()
	{
		if ( sources_.empty() ) { // No more sources
			assert( graph_.empty() );
			return 0;
		} else { // Pop Last source
			Graph::iterator const ig( sources_.back() ); // Last source
			sources_.pop_back(); // Remove the last source

			// Decrement in-degree counts of its Observers
			Observer * const observer_p( ig->first );
			if ( ObserverSingle * const os_p = dynamic_cast< ObserverSingle * >( observer_p ) ) { // Single Observer
				Observer * const oso_p( os_p->observer_p() );
				if ( oso_p ) {
					Graph::iterator const igo( graph_.find( oso_p ) );
					assert( igo != graph_.end() );
					size_type & in_degree( igo->second );
					assert( in_degree > 0u );
					if ( --in_degree == 0 ) sources_.push_back( igo ); // Decrement the in-degree count / Add to sources if zero
				}
			} else if ( ObserverMulti * const om_p = dynamic_cast< ObserverMulti * >( observer_p ) ) { // Multi Observer
				if ( om_p->observers_p() ) {
					ObserverMulti::Observers const & observers( om_p->observers() );
					for ( ObserverMulti::Observers::const_iterator io = observers().begin(), eo = observers().end(); io != eo; ++io ) {
						Graph::iterator const igo( graph_.find( *io ) );
						assert( igo != graph_.end() );
						size_type & in_degree( igo->second );
						assert( in_degree > 0u );
						if ( --in_degree == 0 ) sources_.push_back( igo ); // Decrement the in-degree count / Add to sources if zero
					}
				}
			}

			graph_.erase( ig ); // Remove the source Observer from the graph

			return observer_p;
		}
	}

} // internal
} // ObjexxFCL
