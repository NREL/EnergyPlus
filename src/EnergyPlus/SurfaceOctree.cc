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
//  Initial octree is for use in daylighting
//   Surfaces without vertices are omitted
//   Transparent surfaces are omitted (can't obstruct light)
//  Parameters to support variants for different purposes is anticipated
//  The use of multiple octrees for faster lookups of surface type subsets may be beneficial (avoid post-lookup conditional filtering)
//  Variations and parameter tuning are planned to improve performance
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

	// Line Segment Intersects Enclosing Sphere?
	bool
	SurfaceOctreeCube::
	segmentIntersectsSphere( Vertex const & a, Vertex const & b ) const
	{
		Vertex const ab( b - a );
		Real const ab_mag_squared( ab.mag_squared() );
		if ( ab_mag_squared == 0.0 ) { // Segment is a point
			return ObjexxFCL::distance_squared( a, c_ ) <= r_;
		} else { //Do Test if it is worth it to check a or b in sphere first (depends how often that is true)
			Vertex const ac( c_ - a );
			Real const projection_fac( dot( ac, ab ) / ab_mag_squared );
			if ( ( 0.0 <= projection_fac ) && ( projection_fac <= 1.0 ) ) { // Projected (closest) point is on ab segment
				return ObjexxFCL::distance_squared( ac, projection_fac * ab ) <= r_;
			} else { // Projection (closest) point is outside of ab segment: Intersects iff a or b are in sphere
				return ( ObjexxFCL::distance_squared( a, c_ ) <= r_ ) || ( ObjexxFCL::distance_squared( b, c_ ) <= r_ );
			}
		}
	}

	// Ray Intersects Enclosing Sphere?
	bool
	SurfaceOctreeCube::
	rayIntersectsSphere( Vertex const & a, Vertex const & dir ) const // Ray is from a with direction of unit vector dir
	{
		assert( std::abs( dir.mag_squared() - 1.0 ) < 4 * std::numeric_limits< Real >::epsilon() ); // Check unit vector
		//Do Test if it is worth it to check a in sphere first (depends how often that is true)
		Vertex const ac( c_ - a );
		Real const projection_fac( dot( ac, dir ) );
		if ( 0.0 <= projection_fac ) { // Projected (closest) point is on ray
			return ObjexxFCL::distance_squared( ac, projection_fac * dir ) <= r_;
		} else { // Projection (closest) point is outside of ray: Intersects iff a is in sphere
			return ObjexxFCL::distance_squared( a, c_ ) <= r_;
		}
	}

	// Line Intersects Enclosing Sphere?
	bool
	SurfaceOctreeCube::
	lineIntersectsSphere( Vertex const & a, Vertex const & dir ) const
	{
		assert( std::abs( dir.mag_squared() - 1.0 ) < 4 * std::numeric_limits< Real >::epsilon() ); // Check unit vector
		Vertex const ac( c_ - a );
		return ac.mag_squared() - ObjexxFCL::square( ObjexxFCL::dot( ac, dir ) ) <= r_;
	}

	// Line Segment Intersects Cube?
	bool
	SurfaceOctreeCube::
	segmentIntersectsCube( Vertex const & a, Vertex const & b ) const
	{
		// Check if a or b in cube: This is optional but can be a fast short-circuit if this happens often enough //Do Test if this improves performance in practice
		if ( contains( a ) || contains( b ) ) return true;

		// Use separating axis theorem (faster variants exist)
		Vertex const m( mid( a, b ) - c_ ); // Mid-point relative to cube center
		Vertex const mb( b - c_ - m ); // ab mid-point to b half segment vector
		Vertex const e( std::abs( mb.x ), std::abs( mb.y ), std::abs( mb.z ) ); // Extent of half ab segment
		Real const h( 0.5 * w_ ); // Half-width
		// Check if x,y,z axes are separating
		if ( std::abs( m.x ) > h + e.x ) return false;
		if ( std::abs( m.y ) > h + e.y ) return false;
		if ( std::abs( m.z ) > h + e.z ) return false;
		// Check if cross product axes are separating
		if ( std::abs( m.y * mb.z - m.z * mb.y ) > h * ( e.z + e.y ) ) return false;
		if ( std::abs( m.x * mb.z - m.z * mb.x ) > h * ( e.z + e.x ) ) return false;
		if ( std::abs( m.x * mb.y - m.y * mb.x ) > h * ( e.y + e.x ) ) return false;
		return true;
	}

	// Ray Intersects Cube?
	bool
	SurfaceOctreeCube::
	rayIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv ) const
	{
		// Note: dir_inv coordinates corresponding to a zero dir coordinate are not used and can be set to zero
		//Do Try 0 <= tmax <= seg_length for segmentIntersectsCube: Faster?

		assert( std::abs( dir.mag_squared() - 1.0 ) < 4 * std::numeric_limits< Real >::epsilon() ); // Check unit vector
		assert( ( dir.x == 0.0 ) || ( std::abs( dir_inv.x - ( 1.0 / dir.x ) ) < 2 * std::numeric_limits< Real >::epsilon() * std::abs( dir_inv.x ) ) );
		assert( ( dir.y == 0.0 ) || ( std::abs( dir_inv.y - ( 1.0 / dir.y ) ) < 2 * std::numeric_limits< Real >::epsilon() * std::abs( dir_inv.y ) ) );
		assert( ( dir.z == 0.0 ) || ( std::abs( dir_inv.z - ( 1.0 / dir.z ) ) < 2 * std::numeric_limits< Real >::epsilon() * std::abs( dir_inv.z ) ) );

		// Check if ray origin (a) is in cube
		if ( contains( a ) ) return true;

		// Smits' method: Largest distance to 3 visible cube faces along ray is the candidate entry point
		Real tx, ty, tz; // Ray position parameter for intersections with box face planes
		if ( dir.x != 0.0 ) {
			tx = ( ( dir.x > 0.0 ? l_.x : u_.x ) - a.x ) * dir_inv.x;
		} else { // dir.x == 0
			tx = std::numeric_limits< Real >::lowest();
		}
		if ( dir.y != 0.0 ) {
			ty = ( ( dir.y > 0.0 ? l_.y : u_.y ) - a.y ) * dir_inv.y;
		} else { // dir.y == 0
			ty = std::numeric_limits< Real >::lowest();
		}
		if ( dir.z != 0.0 ) {
			tz = ( ( dir.z > 0.0 ? l_.z : u_.z ) - a.z ) * dir_inv.z;
		} else { // dir.z == 0
			tz = std::numeric_limits< Real >::lowest();
		}

		Real const tmax( ObjexxFCL::max( tx, ty, tz ) );
		if ( tmax >= 0.0 ) { // Intersection is on ray: See if it is within cube extent
			if ( tx == tmax ) {
				Real const y( a.y +( tmax * dir.y ) );
				if ( ( y < l_.y ) || ( y > u_.y ) ) return false;
				Real const z( a.z +( tmax * dir.z ) );
				if ( ( z < l_.z ) || ( z > u_.z ) ) return false;
			} else if ( ty == tmax ) {
				Real const x( a.x +( tmax * dir.x ) );
				if ( ( x < l_.x ) || ( x > u_.x ) ) return false;
				Real const z( a.z +( tmax * dir.z ) );
				if ( ( z < l_.z ) || ( z > u_.z ) ) return false;
			} else { // tz == tmax
				Real const x( a.x +( tmax * dir.x ) );
				if ( ( x < l_.x ) || ( x > u_.x ) ) return false;
				Real const y( a.y +( tmax * dir.y ) );
				if ( ( y < l_.y ) || ( y > u_.y ) ) return false;
			}
			return true;
		} else { // Intersection is on backwards projection of ray
			return false;
		}
	}

	// Line Intersects Cube?
	bool
	SurfaceOctreeCube::
	lineIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv ) const
	{
		// Note: dir_inv coordinates corresponding to a zero dir coordinate are not used and can be set to zero

		assert( std::abs( dir.mag_squared() - 1.0 ) < 4 * std::numeric_limits< Real >::epsilon() ); // Check unit vector
		assert( ( dir.x == 0.0 ) || ( std::abs( dir_inv.x - ( 1.0 / dir.x ) ) < 2 * std::numeric_limits< Real >::epsilon() * std::abs( dir_inv.x ) ) );
		assert( ( dir.y == 0.0 ) || ( std::abs( dir_inv.y - ( 1.0 / dir.y ) ) < 2 * std::numeric_limits< Real >::epsilon() * std::abs( dir_inv.y ) ) );
		assert( ( dir.z == 0.0 ) || ( std::abs( dir_inv.z - ( 1.0 / dir.z ) ) < 2 * std::numeric_limits< Real >::epsilon() * std::abs( dir_inv.z ) ) );

		// Smits' method: Largest distance to 3 visible cube faces along ray is the candidate entry point
		Real txmin, txmax, tymin, tymax, tzmin, tzmax; // Ray position parameter for intersections with box face planes
		if ( dir.x > 0.0 ) {
			txmin = ( l_.x - a.x ) * dir_inv.x;
			txmax = ( u_.x - a.x ) * dir_inv.x;
		} else if ( dir.x < 0.0 ) {
			txmin = ( u_.x - a.x ) * dir_inv.x;
			txmax = ( l_.x - a.x ) * dir_inv.x;
		} else { // dir.x == 0
			txmin = std::numeric_limits< Real >::lowest();
			txmax = std::numeric_limits< Real >::max();
		}
		if ( dir.y > 0.0 ) {
			tymin = ( l_.y - a.y ) * dir_inv.y;
			tymax = ( u_.y - a.y ) * dir_inv.y;
		} else if ( dir.y < 0.0 ) {
			tymin = ( u_.y - a.y ) * dir_inv.y;
			tymax = ( l_.y - a.y ) * dir_inv.y;
		} else { // dir.y == 0
			tymin= std::numeric_limits< Real >::lowest();
			tymax= std::numeric_limits< Real >::max();
		}
		if ( ( txmin > tymax ) || ( tymin > txmax ) ) return false;
		if ( dir.z > 0.0 ) {
			tzmin = ( l_.z - a.z ) * dir_inv.z;
			tzmax = ( u_.z - a.z ) * dir_inv.z;
		} else if ( dir.z < 0.0 ) {
			tzmin = ( u_.z - a.z ) * dir_inv.z;
			tzmax = ( l_.z - a.z ) * dir_inv.z;
		} else { // dir.z == 0
			tzmin= std::numeric_limits< Real >::lowest();
			tzmax= std::numeric_limits< Real >::max();
		}
		Real const tmin( ObjexxFCL::max( txmin, tymin, tzmin ) );
		if ( tmin > tzmax ) return false;
		Real const tmax( ObjexxFCL::min( txmax, tymax, tzmax ) );
		if ( tzmin > tmax ) return false;
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
			if (
			 ( ! surface.Vertex.empty() ) && // Skip no-vertex "surfaces"
			 ( ! surface.IsTransparent ) // Skip transparent surfaces
			) {
				surfaces_.push_back( &surface );
			}
		}

		// No surfaces handler
		if ( surfaces_.empty() ) {
			l_ = u_ = c_ = Vertex( 0.0 );
			w_ = r_ = 0.0;
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
		Vertex const diagonal( u_ - l_ ); // Diagonal
		w_ = ObjexxFCL::max( diagonal.x, diagonal.y, diagonal.z );
		r_ = 0.75 * ( w_ * w_ );
		Real const h( 0.5 * w_ ); // Half-width
		l_ = c_ - h;
		u_ = c_ + h;
		//std::cout << "SurfaceOctree bounding box: \n" << l_ << '\n' << c_ << '\n' << u_ << std::endl; //Debug/////

		assert( valid() );

		// Branch sub-tree
		branch();
	}

	// Surfaces that Line Segment Intersects Cube's Enclosing Sphere
	void
	SurfaceOctreeCube::
	surfacesSegmentIntersectsSphere( Vertex const & a, Vertex const & b, Surfaces & surfaces )
	{
		if ( segmentIntersectsSphere( a, b ) ) {
			// Add this cube's surfaces
			if ( ! surfaces_.empty() ) surfaces.insert( surfaces.end(), surfaces_.begin(), surfaces_.end() );

			// Recurse
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->surfacesSegmentIntersectsSphere( a, b, surfaces );
			}
		}
	}

	// Surfaces that Ray Intersects Cube's Enclosing Sphere
	void
	SurfaceOctreeCube::
	surfacesRayIntersectsSphere( Vertex const & a, Vertex const & dir, Surfaces & surfaces )
	{
		if ( rayIntersectsSphere( a, dir ) ) {
			// Add this cube's surfaces
			if ( ! surfaces_.empty() ) surfaces.insert( surfaces.end(), surfaces_.begin(), surfaces_.end() );

			// Recurse
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->surfacesRayIntersectsSphere( a, dir, surfaces );
			}
		}
	}

	// Surfaces that Line Intersects Cube's Enclosing Sphere
	void
	SurfaceOctreeCube::
	surfacesLineIntersectsSphere( Vertex const & a, Vertex const & dir, Surfaces & surfaces )
	{
		if ( lineIntersectsSphere( a, dir ) ) {
			// Add this cube's surfaces
			if ( ! surfaces_.empty() ) surfaces.insert( surfaces.end(), surfaces_.begin(), surfaces_.end() );

			// Recurse
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->surfacesLineIntersectsSphere( a, dir, surfaces );
			}
		}
	}

	// Surfaces that Line Segment Intersects Cube
	void
	SurfaceOctreeCube::
	surfacesSegmentIntersectsCube( Vertex const & a, Vertex const & b, Surfaces & surfaces )
	{
		if ( segmentIntersectsCube( a, b ) ) {
			// Add this cube's surfaces
			if ( ! surfaces_.empty() ) surfaces.insert( surfaces.end(), surfaces_.begin(), surfaces_.end() );

			// Recurse
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->surfacesSegmentIntersectsCube( a, b, surfaces );
			}
		}
	}

	// Surfaces that Ray Intersects Cube
	void
	SurfaceOctreeCube::
	surfacesRayIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv, Surfaces & surfaces )
	{
		if ( rayIntersectsCube( a, dir, dir_inv ) ) {
			// Add this cube's surfaces
			if ( ! surfaces_.empty() ) surfaces.insert( surfaces.end(), surfaces_.begin(), surfaces_.end() );

			// Recurse
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->surfacesRayIntersectsCube( a, dir, dir_inv, surfaces );
			}
		}
	}

	// Surfaces that Line Intersects Cube
	void
	SurfaceOctreeCube::
	surfacesLineIntersectsCube( Vertex const & a, Vertex const & dir, Vertex const & dir_inv, Surfaces & surfaces )
	{
		if ( lineIntersectsCube( a, dir, dir_inv ) ) {
			// Add this cube's surfaces
			if ( ! surfaces_.empty() ) surfaces.insert( surfaces.end(), surfaces_.begin(), surfaces_.end() );

			// Recurse
			SurfaceOctreeCube * * p = &cubes_[ 0 ][ 0 ][ 0 ];
			for ( int i = 0; i < 8; ++i ) {
				if ( p[ i ] ) p[ i ]->surfacesLineIntersectsCube( a, dir, dir_inv, surfaces );
			}
		}
	}

	// Valid?
	bool
	SurfaceOctreeCube::
	valid() const
	{
		if ( le( l_, c_ ) && le( c_, u_ ) ) {
			Real const tol2( std::max( std::max( ObjexxFCL::magnitude_squared( l_ ), ObjexxFCL::magnitude_squared( u_ ) ) * ( 4 * std::numeric_limits< Real >::epsilon() ), 2 * std::numeric_limits< Real >::min() ) );
			if ( ObjexxFCL::distance_squared( c_, cen( l_, u_ ) ) <= tol2 ) {
				Real const tol( std::max( std::sqrt( std::max( ObjexxFCL::magnitude_squared( l_ ), ObjexxFCL::magnitude_squared( u_ ) ) ) * ( 4 * std::numeric_limits< Real >::epsilon() ), 2 * std::numeric_limits< Real >::min() ) );
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
		if ( ( surfaces_.size() > 8u ) && ( d_ < 255u ) ) { //Do Tune max surfaces and max depth parameters
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
