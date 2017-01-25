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

#ifndef Vectors_hh_INCLUDED
#define Vectors_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array1S.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataVectorTypes.hh>

namespace EnergyPlus {

namespace Vectors {

	// Using/Aliasing
	using DataVectorTypes::PlaneEq;
	using DataVectorTypes::Polyhedron;
	using DataVectorTypes::Vector;
	using DataVectorTypes::Vector_2d;

	//MODULE PARAMETER DEFINITIONS

	// Object Data
	extern Vector const XUnit;
	extern Vector const YUnit;
	extern Vector const ZUnit;

	// Functions

	Real64
	AreaPolygon(
		int const n,
		Array1A< Vector > p
	);

	Real64
	VecSquaredLength( Vector const & vec );

	Real64
	VecLength( Vector const & vec );

	Vector
	VecNegate( Vector const & vec );

	Vector
	VecNormalize( Vector const & vec );

	void
	VecRound(
		Vector & vec,
		Real64 const roundto
	);

	void
	DetermineAzimuthAndTilt(
		Array1S< Vector > Surf, // Surface Definition
		int const NSides, // Number of sides to surface
		Real64 & Azimuth, // Outward Normal Azimuth Angle
		Real64 & Tilt, // Tilt angle of surface
		Vector & lcsx,
		Vector & lcsy,
		Vector & lcsz,
		Real64 const surfaceArea,
		Vector const & NewellSurfaceNormalVector
	);

	void
	PlaneEquation(
		Array1A< Vector > verts, // Structure of the surface
		int const nverts, // Number of vertices in the surface
		PlaneEq & plane, // Equation of plane from inputs
		bool & error // returns true for degenerate surface
	);

	Real64
	Pt2Plane(
		Vector const & pt, // Point for determining the distance
		PlaneEq const & pleq // Equation of the plane
	);

	void
	CreateNewellAreaVector(
		Array1S< Vector > const VList,
		int const NSides,
		Vector & OutNewellAreaVector
	);

	void
	CreateNewellSurfaceNormalVector(
		Array1S< Vector > const VList,
		int const NSides,
		Vector & OutNewellSurfaceNormalVector
	);

	void
	CompareTwoVectors(
		Vector const & vector1, // standard vector
		Vector const & vector2, // standard vector
		bool & areSame, // true if the two vectors are the same within specified tolerance
		Real64 const tolerance // specified tolerance
	);

	void
	CalcCoPlanarNess(
		Array1A< Vector > Surf,
		int const NSides,
		bool & IsCoPlanar,
		Real64 & MaxDist,
		int & ErrorVertex
	);

	void
	CalcPolyhedronVolume(
		Polyhedron const & Poly,
		Real64 & Volume
	);

} // Vectors

} // EnergyPlus

#endif
