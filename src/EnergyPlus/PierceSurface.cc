// ObjexxFCL Headers
#include <ObjexxFCL/Vector3.hh>

// EnergyPlus Headers
#include <PierceSurface.hh>
#include <DataSurfaces.hh>

namespace EnergyPlus {

void
PierceSurface(
	int const ISurf, // Surface index
	Vector3< Real64 > const & R1, // Point from which ray originates
	Vector3< Real64 > const & RN, // Unit vector along in direction of ray whose
	int & IPIERC, // =1 if line through point R1 in direction of unit vector
	Vector3< Real64 > & CPhit // Point that ray along RN intersects plane of surface
)
{

	// SUBROUTINE INFORMATION:
	//       AUTHOR         Fred Winkelmann
	//       DATE WRITTEN   July 1997
	//       MODIFIED       Sept 2003, FCW: modification of Daylighting routine DayltgPierceSurface
	//       RE-ENGINEERED  June 2015, Stuart Mentzer: Performance tuned and merged 3 equivalent variants

	// PURPOSE OF THIS SUBROUTINE:
	// Returns point CPhit that line through point R1 in direction of unit vector RN intersects
	// the plan of surface ISurf. IPIERC = 1 if CPhit is inside the perimeter of ISurf. If not,
	// IPIERC = 0. This routine works for convex and concave surfaces with 3 or more vertices.
	// METHODOLOGY EMPLOYED:

	// REFERENCES:
	// Based on DOE-2.1E subroutine DPIERC.

	// USE STATEMENTS:na

	// Locals
	// SUBROUTINE PARAMETER DEFINITIONS:na
	// INTERFACE BLOCK SPECIFICATIONS:na
	// DERIVED TYPE DEFINITIONS:na

	// SUBROUTINE ARGUMENT DEFINITIONS:

	using namespace DataSurfaces;

	// FLOW:
	IPIERC = 0;

	// Aliases
	auto const & surface( Surface( ISurf ) );
	auto const & V( surface.Vertex );
	Vector3< Real64 > const V2( V( 2 ) );

	// Set the first two A
	Vector3< Real64 > const A1( V2 - V( 1 ) );
	Vector3< Real64 > const A2( V( 3 ) - V2 );

	// Vector normal to surface (A1 X A2)
	Vector3< Real64 > const SN( cross( A1, A2 ) );

	// Scale factor, the solution of SN.(CPhit-V2) = 0 and
	// CPhit = R1 + SCALE*RN, where CPhit is the point that RN,
	// when extended, intersects the plane of the surface.
	Real64 const F2 = dot( SN, RN );
	if ( std::abs( F2 ) < 0.01 ) return; // Skip surfaces that are parallel to RN
	Real64 const SCALE = dot( SN, V2 - R1 ) / F2; // Scale factor
	if ( SCALE <= 0.0 ) return; // Skip surfaces that RN points away from
	//Tuned Avoid array temporary and unroll: Was CPhit = R1 + RN * SCALE
	CPhit.x = R1.x + ( RN.x * SCALE );
	CPhit.y = R1.y + ( RN.y * SCALE );
	CPhit.z = R1.z + ( RN.z * SCALE );

	// Two cases: rectangle and non-rectangle; do rectangle
	// first since most common shape and faster calculation
	auto shape( surface.Shape );
	if ( shape == Rectangle || shape == RectangularDoorWindow || shape == RectangularOverhang || shape == RectangularLeftFin || shape == RectangularRightFin ) { // Surface is rectangular
		Vector3< Real64 > const CCC( CPhit - V2 ); // Vector from vertex 2 to CP
		// Intersection point, CCC, is inside rectangle if
		// 0 < CCC.A2 < A2.A2 AND 0 < CCC.AAA < AAA.AAA
		Real64 const DOTCB( dot( CCC, A2 ) ); // Dot product of vectors CCC and A2
		if ( ( DOTCB < 0.0 ) || ( DOTCB > A2.magnitude_squared() ) ) return;
		Real64 const DOTCA( -dot( CCC, A1 ) ); // Dot product of vectors CCC and AAA (AAA == -A1)
		if ( ( DOTCA < 0.0 ) || ( DOTCA > A1.magnitude_squared() ) ) return;
		IPIERC = 1; // Surface is intersected
	} else { // Surface is not rectangular
		// If at least one of these dot products is negative intersection point is outside of surface
		if ( dot( cross( A1, CPhit - V( 1 ) ), SN ) < 0.0 ) return;
		if ( dot( cross( A2, CPhit - V2 ), SN ) < 0.0 ) return;
		int const NV( surface.Sides ); // Number of vertices
		assert( NV >= 3 );
		if ( NV > 3 ) {
			if ( NV == 4 ) {
				if ( dot( cross( V( 4 ) - V( 3 ), CPhit - V( 3 ) ), SN ) < 0.0 ) return;
			} else { // NV > 4
				for ( int N = 3; N < NV; ++N ) {
					if ( dot( cross( V( N + 1 ) - V( N ), CPhit - V( N ) ), SN ) < 0.0 ) return;
				}
			}
		}
		if ( dot( cross( V( 1 ) - V( NV ), CPhit - V( NV ) ), SN ) < 0.0 ) return; // Last vertex
		IPIERC = 1; // Surface is intersected
	}

}

//     NOTICE

//     Copyright (c) 1996-2014 The Board of Trustees of the University of Illinois
//     and The Regents of the University of California through Ernest Orlando Lawrence
//     Berkeley National Laboratory.  All rights reserved.

//     Portions of the EnergyPlus software package have been developed and copyrighted
//     by other individuals, companies and institutions.  These portions have been
//     incorporated into the EnergyPlus software package under license.   For a complete
//     list of contributors, see "Notice" located in main.cc.

//     NOTICE: The U.S. Government is granted for itself and others acting on its
//     behalf a paid-up, nonexclusive, irrevocable, worldwide license in this data to
//     reproduce, prepare derivative works, and perform publicly and display publicly.
//     Beginning five (5) years after permission to assert copyright is granted,
//     subject to two possible five year renewals, the U.S. Government is granted for
//     itself and others acting on its behalf a paid-up, non-exclusive, irrevocable
//     worldwide license in this data to reproduce, prepare derivative works,
//     distribute copies to the public, perform publicly and display publicly, and to
//     permit others to do so.

//     TRADEMARKS: EnergyPlus is a trademark of the US Department of Energy.

} // EnergyPlus
