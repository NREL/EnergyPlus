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

#ifndef SolarReflectionManager_hh_INCLUDED
#define SolarReflectionManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1D.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Vector3.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataGlobals.hh>
#include <UtilityRoutines.hh>

namespace EnergyPlus {

namespace SolarReflectionManager {

	// Using/Aliasing

	// Data
	// MODULE PARAMETER DEFINITIONS:na

	// DERIVED TYPE DEFINITIONS:

	// MODULE VARIABLE DECLARATIONS:
	extern int TotSolReflRecSurf; // Total number of exterior surfaces that can receive reflected solar
	extern int TotPhiReflRays; // Number of rays in altitude angle (-90 to 90 deg) for diffuse refl calc
	extern int TotThetaReflRays; // Number of rays in azimuth angle (0 to 180 deg) for diffuse refl calc

	// SUBROUTINE SPECIFICATIONS FOR MODULE ExteriorSolarReflectionManager

	// Types

	struct SolReflRecSurfData
	{
		// Members
		int SurfNum; // Number of heat transfer surface
		std::string SurfName; // Name of heat transfer surface
		int NumRecPts; // Number of receiving points
		Array1D< Vector3< Real64 > > RecPt; // Coordinates of receiving point on receiving surface in global CS (m)
		Vector3< Real64 > NormVec; // Unit outward normal to receiving surface
		Real64 ThetaNormVec; // Azimuth of surface normal (radians)
		Real64 PhiNormVec; // Altitude of surface normal (radians)
		int NumReflRays; // Number of rays from this receiving surface
		Array1D< Vector3< Real64 > > RayVec; // Unit vector in direction of ray from receiving surface
		Array1D< Real64 > CosIncAngRay; // Cosine of angle between ray and receiving surface outward normal
		Array1D< Real64 > dOmegaRay; // Delta solid angle associated with ray
		Array2D< Vector3< Real64 > > HitPt; // For each receiving point and ray, coords of hit point on obstruction
		// that is closest to receiving point (m)
		Array2D_int HitPtSurfNum; // Number of surface containing the hit point for a ray, except:
		//  0 => ray does not hit an obstruction, but hits sky
		//  -1 => ray does not hit an obstruction, but hits ground
		Array2D< Real64 > HitPtSolRefl; // Beam-to-diffuse solar reflectance at hit point
		Array2D< Real64 > RecPtHitPtDis; // Distance from receiving point to hit point (m)
		Array2D< Vector3< Real64 > > HitPtNormVec; // Hit point's surface normal unit vector pointing into hemisphere
		//  containing the receiving point
		Array1D_int PossibleObsSurfNums; // Surface numbers of possible obstructions for a receiving surf
		int NumPossibleObs; // Number of possible obstructions for a receiving surface

		// Default Constructor
		SolReflRecSurfData() :
			SurfNum( 0 ),
			NumRecPts( 0 ),
			NormVec( 0.0 ),
			ThetaNormVec( 0.0 ),
			PhiNormVec( 0.0 ),
			NumReflRays( 0 ),
			NumPossibleObs( 0 )
		{}

	};

	// Object Data
	extern Array1D< SolReflRecSurfData > SolReflRecSurf;

	// Functions

	void
	InitSolReflRecSurf();

	//=====================================================================================================

	void
	CalcBeamSolDiffuseReflFactors();

	void
	FigureBeamSolDiffuseReflFactors( int const iHour );

	//=================================================================================================

	void
	CalcBeamSolSpecularReflFactors();

	void
	FigureBeamSolSpecularReflFactors( int const iHour );

	//=================================================================================================

	void
	CalcSkySolDiffuseReflFactors();

} // SolarReflectionManager

} // EnergyPlus

#endif
