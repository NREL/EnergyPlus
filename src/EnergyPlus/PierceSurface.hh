// EnergyPlus, Copyright (c) 1996-2015, The Board of Trustees of the University of Illinois and
// The Regents of the University of California, through Lawrence Berkeley National Laboratory
// (subject to receipt of any required approvals from the U.S. Dept. of Energy). All rights
// reserved.
//
// If you have questions about your rights to use or distribute this software, please contact
// Berkeley Lab's Innovation & Partnerships Office at IPO@lbl.gov.
//
// NOTICE: This Software was developed under funding from the U.S. Department of Energy and the
// U.S. Government consequently retains certain rights. As such, the U.S. Government has been
// granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable,
// worldwide license in the Software to reproduce, distribute copies to the public, prepare
// derivative works, and perform publicly and display publicly, and to permit others to do so.
//
// Redistribution and use in source and binary forms, with or without modification, are permitted
// provided that the following conditions are met:
//
// (1) Redistributions of source code must retain the above copyright notice, this list of
//     conditions and the following disclaimer.
//
// (2) Redistributions in binary form must reproduce the above copyright notice, this list of
//     conditions and the following disclaimer in the documentation and/or other materials
//     provided with the distribution.
//
// (3) Neither the name of the University of California, Lawrence Berkeley National Laboratory,
//     the University of Illinois, U.S. Dept. of Energy nor the names of its contributors may be
//     used to endorse or promote products derived from this software without specific prior
//     written permission.
//
// (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in stand-alone form
//     without changes from the version obtained under this License, or (ii) Licensee makes a
//     reference solely to the software portion of its product, Licensee must refer to the
//     software as "EnergyPlus version X" software, where "X" is the version number Licensee
//     obtained under this License and may not use a different name for the software. Except as
//     specifically required in this Section (4), Licensee shall not use in a company name, a
//     product name, in advertising, publicity, or other promotional activities any name, trade
//     name, trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or confusingly
//     similar designation, without Lawrence Berkeley National Laboratory's prior written consent.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the
// features, functionality or performance of the source code ("Enhancements") to anyone; however,
// if you choose to make your Enhancements available either publicly, or directly to Lawrence
// Berkeley National Laboratory, without imposing a separate written license agreement for such
// Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free
// perpetual license to install, use, modify, prepare derivative works, incorporate into other
// computer software, distribute, and sublicense such enhancements or derivative works thereof,
// in binary and source code form.

#ifndef EnergyPlus_PierceSurface_hh_INCLUDED
#define EnergyPlus_PierceSurface_hh_INCLUDED

// Purpose: Functions for checking if a ray hits a surface
//
// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
//
// History:
//  Jun 2015: Last update of legacy version based on DOE-2 DPIERC
//  Jan 2016: Initial release
//
// Notes:
//  This is filling the role of the former PierceSurface function authored by Fred Winkelmann and based on
//   DOE-2.1E subroutine DPIERC and some aspects of this version are analogous
//  To match the former behavior ray's with origin exactly on the surface are treated as not hitting
//  These functions are VERY performance critical for daylighting and solar reflection
//   This high-performance implementation was built to complement the octree system for scalability of those systems
//  This has been carefully designed for speed but is probably not be optimal yet
//   For EnergyPlus most surfaces are rectangular so that is the most important for performance
//   Inlining, storing preprocessed values in Surface, 2D projection, & short circuiting are used here for speed
//   Agressive inlining options may be needed to get peak performance
//   Don't make changes here without validating the performance impact

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <ALWAYS_INLINE.hh>
#include <DataSurfaces.hh>

// ObjexxFCL Headers
#include <ObjexxFCL/Vector2.hh>
#include <ObjexxFCL/Vector3.hh>
#include <ObjexxFCL/Vector4.hh>

// C++ Headers
#include <cassert>
#include <limits>

namespace EnergyPlus {

inline
void
PierceSurface_Triangular(
	DataSurfaces::Surface2D const & s2d, // 2D surface
	Vector2< Real64 > const & h2d, // 2D hit point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Check if a 2D hit point is in a triangular 2D surface
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release
	//
	// Notes:
	//  Pulled this case out into separate function to facilitate inlining

	using DataSurfaces::Surface2D;
	Surface2D::Vertices const & vs( s2d.vertices ); // 2D surface vertices
	Surface2D::Vectors const & es( s2d.edges ); // 2D surface edge vectors
	if ( es[ 0 ].cross( h2d - vs[ 0 ] ) < 0.0 ) return;
	if ( es[ 1 ].cross( h2d - vs[ 1 ] ) < 0.0 ) return;
	if ( es[ 2 ].cross( h2d - vs[ 2 ] ) < 0.0 ) return;
	hit = true;
}

inline
void
PierceSurface_Convex(
	DataSurfaces::Surface2D const & s2d, // 2D surface
	Vector2< Real64 > const & h2d, // 2D hit point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Check if a 2D hit point is in a convex 2D surface
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release
	//
	// Notes:
	//  Pulled this rare case out into separate function to facilitate inlining

	using DataSurfaces::Surface2D;
	Surface2D::Vertices const & vs( s2d.vertices ); // 2D surface vertices
	Surface2D::Vectors const & es( s2d.edges ); // 2D surface edge vectors
	Surface2D::Vertices::size_type const n( vs.size() );
	assert( n >= 3u );
	switch ( n ) {
	case 8:
		if ( es[ 7 ].cross( h2d - vs[ 7 ] ) < 0.0 ) return;
	case 7:
		if ( es[ 6 ].cross( h2d - vs[ 6 ] ) < 0.0 ) return;
	case 6:
		if ( es[ 5 ].cross( h2d - vs[ 5 ] ) < 0.0 ) return;
	case 5:
		if ( es[ 4 ].cross( h2d - vs[ 4 ] ) < 0.0 ) return;
	case 4:
		if ( es[ 3 ].cross( h2d - vs[ 3 ] ) < 0.0 ) return;
	case 3:
		if ( es[ 2 ].cross( h2d - vs[ 2 ] ) < 0.0 ) return;
		if ( es[ 1 ].cross( h2d - vs[ 1 ] ) < 0.0 ) return;
		if ( es[ 0 ].cross( h2d - vs[ 0 ] ) < 0.0 ) return;
		hit = true;
		return;
	default:
		for ( Surface2D::Vertices::size_type i = 0; i < n; ++i ) {
			if ( es[ i ].cross( h2d - vs[ i ] ) < 0.0 ) return;
		}
		hit = true;
		return;
	}
}

ALWAYS_INLINE
void
PierceSurface_polygon(
	DataSurfaces::SurfaceData const & surface, // Surface
	Vector3< Real64 > const & hitPt, // Ray-plane intersection point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Check if hit point on surface plane is in surface polygon
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release

	using DataSurfaces::Surface2D;
	using Vertex2D = ObjexxFCL::Vector2< Real64 >;
	Surface2D const & s2d( surface.surface2d );
	int const axis( s2d.axis );
	Vertex2D const h2d( axis == 0 ? hitPt.y : hitPt.x, axis == 2 ? hitPt.y : hitPt.z ); // Hit point in 2D surface's plane
	if ( ( h2d.x < s2d.vl.x ) || ( s2d.vu.x < h2d.x ) || ( h2d.y < s2d.vl.y ) || ( s2d.vu.y < h2d.y ) ) return; // Misses 2D surface bounding box
	ShapeCat const shapeCat( surface.shapeCat );
	if ( shapeCat == ShapeCat::Rectangular ) { // Rectangular is most common: Special case algorithm is faster but assumes these are really rectangular
		Vertex2D const v0h( h2d - s2d.vertices[ 0 ] );
		Real64 const he1( v0h.dot( s2d.edges[ 0 ] ) );
		if ( ( he1 < 0.0 ) || ( he1 > s2d.s1 ) ) return;
		Real64 const he3( -v0h.dot( s2d.edges[ 3 ] ) );
		if ( ( he3 < 0.0 ) || ( he3 > s2d.s3 ) ) return;
		hit = true;
	} else if ( shapeCat == ShapeCat::Triangular ) { // Cross products all nonnegative <=> Hit point in triangle
		PierceSurface_Triangular( s2d, h2d, hit );
	} else if ( shapeCat == ShapeCat::Convex ) { // This is O( n ): For sufficiently large n should use O( log n ) slab or fan algorithm: Does this occur in EnergyPlus?
		PierceSurface_Convex( s2d, h2d, hit );
	} else if ( shapeCat == ShapeCat::Nonconvex ) {
		PierceSurface_Convex( s2d, h2d, hit ); //Do Implement proper nonconvex algorithm or add one-time warning if this is hit
	}
}

ALWAYS_INLINE
void
PierceSurface(
	DataSurfaces::SurfaceData const & surface, // Surface
	Vector3< Real64 > const & rayOri, // Ray origin point
	Vector3< Real64 > const & rayDir, // Ray direction unit vector
	Vector3< Real64 > & hitPt, // Ray-plane intersection point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Check if a ray hits a surface and return the point of intersection
	//  with the surface's plane if they intersect.
	//  Convex and concave surfaces with 3 or more vertices are supported.
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release

	// Input checks
	assert( std::abs( rayDir.mag_squared() - 1.0 ) < 4 * std::numeric_limits< Real64 >::epsilon() ); // Check unit vector

	// Find ray intersection with surface plane
	hit = false;
	DataSurfaces::SurfaceData::Plane const & plane( surface.plane );
	Real64 const den( ( plane.x * rayDir.x ) + ( plane.y * rayDir.y ) + ( plane.z * rayDir.z ) ); // float gives faster division
	if ( den == 0.0 ) { // Ray is parallel to plane: This not treated as piercing even if ray lies in plane
		return;
	} else { // Ray intersects plane
		Real64 const num( -( ( plane.x * rayOri.x ) + ( plane.y * rayOri.y ) + ( plane.z * rayOri.z ) + plane.w ) ); // float gives faster division
		if ( num * den <= 0.0 ) { // Ray points away from surface or ray origin is on surface: This looks odd but is fast way to check for different signs
			return;
		} else { // Ray points toward surface: Compute hit point
			Real64 const t( num / den ); // Ray parameter at plane intersection: hitPt = RayOri + t * rayDir
			hitPt.x = rayOri.x + ( t * rayDir.x ); // Compute by coordinate to avoid Vertex temporaries
			hitPt.y = rayOri.y + ( t * rayDir.y );
			hitPt.z = rayOri.z + ( t * rayDir.z );
		}
	}

	// Check if hit point is in surface polygon
	PierceSurface_polygon( surface, hitPt, hit );
}

ALWAYS_INLINE
void
PierceSurface(
	int const iSurf, // Surface index
	Vector3< Real64 > const & rayOri, // Ray origin point
	Vector3< Real64 > const & rayDir, // Ray direction unit vector
	Vector3< Real64 > & hitPt, // Ray-plane intersection point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Overload taking surface index instead of surface
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release

	PierceSurface( DataSurfaces::Surface( iSurf ), rayOri, rayDir, hitPt, hit );
}

ALWAYS_INLINE
void
PierceSurface(
	DataSurfaces::SurfaceData const & surface, // Surface
	Vector3< Real64 > const & rayOri, // Ray origin point
	Vector3< Real64 > const & rayDir, // Ray direction unit vector
	Real64 const dMax, // Max distance from rayOri to hit point
	Vector3< Real64 > & hitPt, // Ray-plane intersection point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Check if a ray hits a surface and return the point of intersection
	//  with the surface's plane if they intersect.
	//  Convex and concave surfaces with 3 or more vertices are supported.
	//  This overload limits the ray-surface distance for a hit.
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release

	// Input checks
	assert( std::abs( rayDir.mag_squared() - 1.0 ) < 4 * std::numeric_limits< Real64 >::epsilon() ); // Check unit vector
	assert( dMax >= 0.0 ); // Distance must be nonnegative

	// Find ray intersection with surface plane
	hit = false;
	DataSurfaces::SurfaceData::Plane const & plane( surface.plane );
	Real64 const den( ( plane.x * rayDir.x ) + ( plane.y * rayDir.y ) + ( plane.z * rayDir.z ) ); // float gives faster division
	if ( den == 0.0 ) { // Ray is parallel to plane: This not treated as piercing even if ray lies in plane
		return;
	} else { // Ray intersects plane
		Real64 const num( -( ( plane.x * rayOri.x ) + ( plane.y * rayOri.y ) + ( plane.z * rayOri.z ) + plane.w ) ); // float gives faster division
		if ( num * den <= 0.0 ) { // Ray points away from surface or ray origin is on surface: This looks odd but is fast way to check for different signs
			return;
		} else { // Ray points toward surface: Compute hit point
			Real64 const t( num / den ); // Ray parameter at plane intersection: hitPt = RayOri + t * rayDir
			if ( t > dMax ) return; // Hit point exceeds distance from rayOri limit
			hitPt.x = rayOri.x + ( t * rayDir.x ); // Compute by coordinate to avoid Vertex temporaries
			hitPt.y = rayOri.y + ( t * rayDir.y );
			hitPt.z = rayOri.z + ( t * rayDir.z );
		}
	}

	// Check if hit point is in surface polygon
	PierceSurface_polygon( surface, hitPt, hit );
}


ALWAYS_INLINE
void
PierceSurface(
	int const iSurf, // Surface index
	Vector3< Real64 > const & rayOri, // Ray origin point
	Vector3< Real64 > const & rayDir, // Ray direction unit vector
	Real64 const dMax, // Max distance from rayOri to hit point
	Vector3< Real64 > & hitPt, // Ray-plane intersection point
	bool & hit // Ray intersects surface?
)
{
	// Purpose: Overload taking surface index instead of surface
	//
	// Author: Stuart Mentzer (Stuart_Mentzer@objexx.com)
	//
	// History:
	//  Jan 2016: Initial release

	PierceSurface( DataSurfaces::Surface( iSurf ), rayOri, rayDir, dMax, hitPt, hit );
}

} // EnergyPlus

#endif
