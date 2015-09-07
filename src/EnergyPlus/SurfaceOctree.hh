#ifndef EnergyPlus_SurfaceOctree_hh_INCLUDED
#define EnergyPlus_SurfaceOctree_hh_INCLUDED

// EnergyPlus Headers
#include <EnergyPlus/EnergyPlus.hh>

// ObjexxFCL Headers
#include <ObjexxFCL/Array1.fwd.hh>
#include <ObjexxFCL/Vector3.hh>

// C++ Headers
#include <cassert>
#include <cstdint>
#include <vector>

namespace EnergyPlus {

// Forward
namespace DataSurfaces { struct SurfaceData; }

// Package: Surface Octree System
//
// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
//
// Purpose: Spatial sort of surfaces for fast, scalable identification of active surfaces
// for some algorithms such as solar shading, solar reflection, and daylighting obstruction
//
// See the .cc file for details

class SurfaceOctreeCube
{

public: // Types

	using Real = Real64;
	using Surface = DataSurfaces::SurfaceData;
	using Vertex = ObjexxFCL::Vector3< Real >;
	using Surfaces = std::vector< Surface * >;
	using size_type = Surfaces::size_type;

public: // Creation

	// Default Constructor
	SurfaceOctreeCube()
	{
#if defined(_MSC_VER) && _MSC_VER < 1900 && !defined(__INTEL_COMPILER) // Visual C++ 2013 doesn't support array initializers in member declaration
		SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ]; for ( int i = 0; i < 8; ++i ) p[ i ] = nullptr;
#endif
	}

	// Surfaces Outer Cube Constructor
	SurfaceOctreeCube( ObjexxFCL::Array1< Surface > & surfaces )
	{
#if defined(_MSC_VER) && _MSC_VER < 1900 && !defined(__INTEL_COMPILER) // Visual C++ 2013 doesn't support array initializers in member declaration
		SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ]; for ( int i = 0; i < 8; ++i ) p[ i ] = nullptr;
#endif
		init( surfaces );
	}

	// Box Constructor
	SurfaceOctreeCube(
	 std::uint16_t const d,
	 Vertex const & l,
	 Vertex const & u,
	 Real const w
	) :
	 d_( d ),
	 l_( l ),
	 u_( u ),
	 c_( cen( l, u ) ),
	 w_( w ),
	 r_( 0.75 * ( w * w ) )
	{
#if defined(_MSC_VER) && _MSC_VER < 1900 && !defined(__INTEL_COMPILER) // Visual C++ 2013 doesn't support array initializers in member declaration
		SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ]; for ( int i = 0; i < 8; ++i ) p[ i ] = nullptr;
#endif
		assert( valid() );
	}

	// Destructor
	~SurfaceOctreeCube()
	{
		SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
		for ( int i = 0; i < 8; ++i ) {
			if ( p[ i ] ) delete p[ i ];
		}
	}

public: // Properties

	// Depth
	std::uint16_t
	d() const
	{
		return d_;
	}

	// Depth
	std::uint16_t
	depth() const
	{
		return d_;
	}

	// Lower Corner
	Vertex const &
	l() const
	{
		return l_;
	}

	// Upper Corner
	Vertex const &
	u() const
	{
		return u_;
	}

	// Center Point
	Vertex const &
	c() const
	{
		return c_;
	}

	// Center Point
	Vertex const &
	center() const
	{
		return c_;
	}

	// Width
	Real
	w() const
	{
		return w_;
	}

	// Width
	Real
	width() const
	{
		return w_;
	}

	// Surfaces
	Surfaces const &
	surfaces() const
	{
		return surfaces_;
	}

	// Surfaces
	Surfaces::size_type
	surfaces_size() const
	{
		return surfaces_.size();
	}

	// Surfaces Begin Iterator
	Surfaces::const_iterator
	surfaces_begin() const
	{
		return surfaces_.begin();
	}

	// Surfaces Begin Iterator
	Surfaces::iterator
	surfaces_begin()
	{
		return surfaces_.begin();
	}

	// Surfaces End Iterator
	Surfaces::const_iterator
	surfaces_end() const
	{
		return surfaces_.end();
	}

	// Surfaces End Iterator
	Surfaces::iterator
	surfaces_end()
	{
		return surfaces_.end();
	}

public: // Predicates

	// Vertex in Cube?
	bool
	contains( Vertex const & v ) const
	{
		return ( l_.x <= v.x ) && ( v.x <= u_.x ) && ( l_.y <= v.y ) && ( v.y <= u_.y ) && ( l_.z <= v.z ) && ( v.z <= u_.z );
	}

	// Surface in Cube?
	bool
	contains( Surface const & surface ) const;

	// Line Segment Intersects Enclosing Sphere?
	bool
	segmentIntersectsSphere( Vertex const & a, Vertex const & b ) const;

	// Ray Intersects Enclosing Sphere?
	bool
	rayIntersectsSphere( Vertex const & a, Vertex const & dir ) const; // Ray is from a with direction dir

	// Line Intersects Enclosing Sphere?
	bool
	lineIntersectsSphere( Vertex const & a, Vertex const & dir ) const;

	// Line Segment Intersects Cube?
	bool
	segmentIntersectsCube( Vertex const & a, Vertex const & b ) const;

	// Ray Intersects Cube?
	bool
	rayIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv ) const;

	// Line Intersects Cube?
	bool
	lineIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv ) const;

public: // Methods

	// Surfaces Outer Cube Initilization
	void
	init( ObjexxFCL::Array1< Surface > & surfaces );

	// Surfaces that Line Segment Intersects Enclosing Sphere
	void
	surfacesSegmentIntersectsSphere( Vertex const & a, Vertex const & b, Surfaces & surfaces );

	// Surfaces that Ray Intersects Enclosing Sphere
	void
	surfacesRayIntersectsSphere( Vertex const & a, Vertex const & dir, Surfaces & surfaces );

	// Surfaces that Line Intersects Enclosing Sphere
	void
	surfacesLineIntersectsSphere( Vertex const & a, Vertex const & dir, Surfaces & surfaces );

	// Surfaces that Line Segment Intersects Cube
	void
	surfacesSegmentIntersectsCube( Vertex const & a, Vertex const & b, Surfaces & surfaces );

	// Surfaces that Ray Intersects Cube
	void
	surfacesRayIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv, Surfaces & surfaces );

	// Surfaces that Line Intersects Cube
	void
	surfacesLineIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv, Surfaces & surfaces );

private: // Methods

	// Valid?
	bool
	valid() const;

	// Add a Surface
	void
	add( Surface & surface )
	{
		surfaces_.push_back( &surface );
	}

	// Branch to Sub-Tree
	void
	branch();

	// Surface Branch Processing
	void
	surfaceBranch( Surface & surface );

private: // Static Methods

	// Vertex in Cube?
	static
	bool
	contains(
	 Vertex const & l,
	 Vertex const & u,
	 Vertex const & v
	)
	{
		return ( l.x <= v.x ) && ( v.x <= u.x ) && ( l.y <= v.y ) && ( v.y <= u.y ) && ( l.z <= v.z ) && ( v.z <= u.z );
	}

	// Surface in Cube?
	static
	bool
	contains(
	 Vertex const & l,
	 Vertex const & u,
	 Surface const & surface
	);

private: // Data

	std::uint16_t d_ = 0u; // Depth in tree
	Vertex l_ = Vertex( 0.0 ); // Lower corner
	Vertex u_ = Vertex( 0.0 ); // Upper corner
	Vertex c_ = Vertex( 0.0 ); // Center point
	Real w_ = 0.0; // Side width
	Real r_ = 0.0; // Enclosing sphere radius
#if defined(_MSC_VER) && _MSC_VER < 1900 && !defined(__INTEL_COMPILER) // Visual C++ 2013 doesn't support array initializers in member declaration
	SurfaceOctreeCube * cubes_[ 2 ][ 2 ][ 2 ]; // Children (nullptrs if not present)
#else
	SurfaceOctreeCube * cubes_[ 2 ][ 2 ][ 2 ] = { { { nullptr, nullptr }, { nullptr, nullptr } },{ { nullptr, nullptr }, { nullptr, nullptr } } }; // Children (nullptrs if not present)
#endif
	Surfaces surfaces_; // Surfaces

}; // SurfaceOctreeCube

// Octree-Safe Vector Inverse
inline
SurfaceOctreeCube::Vertex
octree_inverse( SurfaceOctreeCube::Vertex const & v )
{
	return SurfaceOctreeCube::Vertex(
	 ( v.x != 0.0 ? 1.0 / v.x : 0.0 ),
	 ( v.y != 0.0 ? 1.0 / v.y : 0.0 ),
	 ( v.z != 0.0 ? 1.0 / v.z : 0.0 )
	);
}

// Globals
extern SurfaceOctreeCube surfaceOctree;

} // EnergyPlus

//=================================================================================
// NOTICE
//
// Copyright (c) 1996-2015 The Board of Trustees of the University of Illinois
// and The Regents of the University of California through Ernest Orlando Lawrence
// Berkeley National Laboratory. All rights reserved.
//
// Portions of the EnergyPlus software package have been developed and copyrighted
// by other individuals, companies and institutions. These portions have been
// incorporated into the EnergyPlus software package under license. For a complete
// list of contributors, see "Notice" located in main.cc.
//
// NOTICE: The U.S. Government is granted for itself and others acting on its
// behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
// reproduce, prepare derivative works, and perform publicly and display publicly.
// Beginning five (5) years after permission to assert copyright is granted,
// subject to two possible five year renewals, the U.S. Government is granted for
// itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
// worldwide license in this data to reproduce, prepare derivative works,
// distribute copies to the public, perform publicly and display publicly, and to
// permit others to do so.
//
// TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.
//=================================================================================

#endif
