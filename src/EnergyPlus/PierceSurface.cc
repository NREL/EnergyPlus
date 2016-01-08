// EnergyPlus, Copyright (c) 1996-2016, The Board of Trustees of the University of Illinois and
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

} // EnergyPlus
