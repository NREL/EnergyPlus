// EnergyPlus Headers
#include <EnergyPlus/SurfaceOctree.hh>
#include <EnergyPlus/DataSurfaces.hh>

// ObjexxFCL Headers
#include <ObjexxFCL/Fmath.hh>

// C++ Headers
#include <algorithm>
#include <cmath>
#include <limits>

namespace EnergyPlus {
namespace Octree {

// Package: Surface Octree System
//
// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
//
// Purpose: Spatial sort of surfaces for fast, scalable identification of active surfaces
// for some algorithms such as solar shading, solar reflection, and daylighting obstruction
//
// History:
//  Sep 2015: Initial experimental release
//
// Notes:
//  This code is experimental at this time
//  Variations and parameter tuning are planned to improve performance

// Dev Notes:
//  Initial simple design uses "tight" cubes (no overlap) and surfaces filtering down to deepest cube they fit in completely
//  Alt: Use "loose" cubes oversied by x2 or some other factor to allow surfaces to filter down further: This requires more cubes to be processed for a given operation
//  Alt: Filter all surfaces down to leaf cubes placing a surface in any cube it intersects: More specificity but redundant surfaces in each operation so must collect them in a set
//  Implement copy and move ctors/assignment later if needed
//  Could cache cube index triple rel to parent for exact center calc? Or do this only OTF during fill/construction?
//  Could round bounding box width to a power of 2 for exact halving (prob. not needed for E+)


	// Surface in Cube?
	bool
	SurfaceOctreeCube::
	contains( Surface const & surface ) const
	{
		for ( Vertex const & v : surface.Vertex ) { // All surface vertices must be in cube
			if ( ! contains( v ) ) return false;
		}
		return true;
	}

	// Surfaces Outer Cube Initilization
	void
	SurfaceOctreeCube::
	init( ObjexxFCL::Array1< Surface > & surfaces )
	{
		assert( d_ == 0u );
		surfaces_.clear();
		surfaces_.reserve( surfaces.size() );
		for( Surface & surface : surfaces ) {
			if ( ! surface.Vertex.empty() ) surfaces_.push_back( &surface ); // Skip no-vertex "surfaces"
		}

		// No surfaces handler
		if ( surfaces_.empty() ) {
			l_ = u_ = c_ = Vertex( 0.0 );
			w_ = 0.0;
			return;
		}

		// Bounding box corners and center
		Surface const & surface_0( *surfaces_[ 0 ] );
		assert( ! surface_0.Vertex.empty() );
		l_ = u_ = surface_0.Vertex[ 0 ]; // Initialize corners to first vertex of first surface
		for ( Surface const * surface_p : surfaces_ ) { // Surfaces
			auto const & vertices( surface_p->Vertex );
			for ( auto const & vertex : vertices ) { // Expand cube to hold surface vertices
				l_.min( vertex );
				u_.max( vertex );
			}
		}
		c_ = cen( l_, u_ ); // Center vertex

		// Expand bounding box to cube with uniform side width
		Vertex const d( u_ - l_ ); // Diagonal
		w_ = ObjexxFCL::max( d.x, d.y, d.z );
		Real const h( 0.5 * w_ ); // Half-width
		l_ = c_ - h;
		u_ = c_ + h;
		//std::cout << "SurfaceOctree bounding box: \n" << l_ << '\n' << c_ << '\n' << u_ << std::endl; //Debug/////

		assert( valid() );

		// Branch sub-tree
		branch();
	}

	// Valid?
	bool
	SurfaceOctreeCube::
	valid() const
	{
		if ( le( l_, c_ ) && le( c_, u_ ) ) {
			Real const tol2( std::max( std::max( ObjexxFCL::magnitude_squared( l_ ), ObjexxFCL::magnitude_squared( u_ ) ) * ObjexxFCL::square( 4 * std::numeric_limits< Real >::epsilon() ), ObjexxFCL::square( 4 * std::numeric_limits< Real >::min() ) ) );
			if ( ObjexxFCL::distance_squared( c_, cen( l_, u_ ) ) <= tol2 ) {
				Real const tol( std::max( std::sqrt( tol2 ), 4 * std::numeric_limits< Real >::min() ) );
				Vertex const d( u_ - l_ ); // Diagonal
				return ( std::abs( d.x - w_ ) <= tol ) && ( std::abs( d.x - d.y ) <= tol ) && ( std::abs( d.x - d.z ) <= tol ); // Uniform side witdths?
			}
		}
		return false;
	}

	// Branch to Sub-Tree
	void
	SurfaceOctreeCube::
	branch()
	{
		if ( ( surfaces_.size() > maxSurfaces_ ) && ( d_ < maxDepth_ ) ) {
			// Assign Surfaces to cubes containing them //Do Try loose cubes
			Surfaces surfaces_all;
			surfaces_all.swap( surfaces_ );
			for ( auto * surface_p : surfaces_all ) { // Surfaces
				surfaceBranch( *surface_p );
			}

			// Branch sub-tree recursively
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->branch();
			}
		}
	}

	// Surface Branch Processing
	void
	SurfaceOctreeCube::
	surfaceBranch( Surface & surface )
	{
		Real const h( 0.5 * w_ ); // Half-width

		Vertex sl( 0.0 ), su( 0.0 ); // Surface bounding box corners
		sl = su = surface.Vertex[ 0 ]; // Initialize corners to first vertex of first surface
		auto const & vertices( surface.Vertex ); // Surface vertices
		for ( auto const & vertex : vertices ) { // Expand bounding box to hold surface vertices
			sl.min( vertex );
			su.max( vertex );
		}
		Vertex const ctr( cen( sl, su ) );
		int const i( ctr.x <= c_.x ? 0 : 1 );
		int const j( ctr.y <= c_.y ? 0 : 1 );
		int const k( ctr.z <= c_.z ? 0 : 1 );
		Real const x( i * h );
		Real const y( j * h );
		Real const z( k * h );
		Vertex const l( l_.x + x, l_.y + y, l_.z + z );
		Vertex const u( c_.x + x, c_.y + y, c_.z + z );
		if ( le( l, sl ) && ge( u, su ) ) { // Surface is contained in sub-cube
			SurfaceOctreeCube * & cube = cubes_[ i ][ j ][ k ]; // Visual C++ 2013 bug prevents constructor notation
			if ( ! cube ) cube = new SurfaceOctreeCube( d_ + 1, l, u, h );
			cube->add( surface );
		} else { // Surface is in this cube
			surfaces_.push_back( &surface );
		}
	}

	// Surface in Cube?
	bool
	SurfaceOctreeCube::
	contains(
	 Vertex const & l,
	 Vertex const & u,
	 Surface const & surface
	)
	{
		for ( Vertex const & v : surface.Vertex ) { // All surface vertices must be in cube
			if ( ! contains( l, u, v ) ) return false;
		}
		return true;
	}

// Globals
SurfaceOctreeCube surfaceOctree;

} // Octree
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
