#ifndef ObjexxFCL_ObserverGraph_hh_INCLUDED
#define ObjexxFCL_ObserverGraph_hh_INCLUDED

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

// C++ Headers
#include <cstddef>
#include <map>
#include <vector>

namespace ObjexxFCL {

// Forward
class Observer;

// Types
typedef  Observer  Subject;

namespace internal {

// ObserverGraph: Observer Graph Representation
class ObserverGraph
{

public: // Types

	// STL style
	typedef  std::size_t  size_type;

	// C++ style
	typedef  std::size_t  Size;

	typedef  std::map< Observer *, size_type >  Graph; // Maps Observers to their in-degree counts
	typedef  std::vector< Graph::iterator >  Sources; // Iterators to zero in-degree graph nodes

public: // Creation

	// Subject Constructor
	ObserverGraph( Subject const & s );

	// Destructor
	inline
	~ObserverGraph()
	{}

public: // Inspector

	// Empty?
	inline
	bool
	empty() const
	{
		return graph_.empty();
	}

public: // Modifier

	// Push a Subject's Transitive Observers onto Graph and Return Acyclicity
	bool
	push( Subject const & s_root, Subject const & s );

	// Pop a Source Observer from Graph
	Observer *
	pop();

private: // Data

	Graph graph_; // Graph representation

	Sources sources_; // Source Observers with in-degree == zero

}; // ObserverGraph

} // internal
} // ObjexxFCL

#endif // ObjexxFCL_ObserverGraph_hh_INCLUDED
