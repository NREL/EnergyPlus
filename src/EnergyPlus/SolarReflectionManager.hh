#ifndef SolarReflectionManager_hh_INCLUDED
#define SolarReflectionManager_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>
#include <ObjexxFCL/Array2D.hh>
#include <ObjexxFCL/Array3D.hh>
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

		// Member Constructor
		SolReflRecSurfData(
			int const SurfNum, // Number of heat transfer surface
			std::string const & SurfName, // Name of heat transfer surface
			int const NumRecPts, // Number of receiving points
			Array1< Vector3< Real64 > > const & RecPt, // Coordinates of receiving point on receiving surface in global CS (m)
			Vector3< Real64 > const & NormVec, // Unit outward normal to receiving surface
			Real64 const ThetaNormVec, // Azimuth of surface normal (radians)
			Real64 const PhiNormVec, // Altitude of surface normal (radians)
			int const NumReflRays, // Number of rays from this receiving surface
			Array1< Vector3< Real64 > > const & RayVec, // Unit vector in direction of ray from receiving surface
			Array1< Real64 > const & CosIncAngRay, // Cosine of angle between ray and receiving surface outward normal
			Array1< Real64 > const & dOmegaRay, // Delta solid angle associated with ray
			Array2< Vector3< Real64 > > const & HitPt, // For each receiving point and ray, coords of hit point on obstruction
			Array2_int const & HitPtSurfNum, // Number of surface containing the hit point for a ray, except:
			Array2< Real64 > const & HitPtSolRefl, // Beam-to-diffuse solar reflectance at hit point
			Array2< Real64 > const & RecPtHitPtDis, // Distance from receiving point to hit point (m)
			Array2< Vector3< Real64 > > const & HitPtNormVec, // Hit point's surface normal unit vector pointing into hemisphere
			Array1_int const & PossibleObsSurfNums, // Surface numbers of possible obstructions for a receiving surf
			int const NumPossibleObs // Number of possible obstructions for a receiving surface
		) :
			SurfNum( SurfNum ),
			SurfName( SurfName ),
			NumRecPts( NumRecPts ),
			RecPt( RecPt ),
			NormVec( NormVec ),
			ThetaNormVec( ThetaNormVec ),
			PhiNormVec( PhiNormVec ),
			NumReflRays( NumReflRays ),
			RayVec( RayVec ),
			CosIncAngRay( CosIncAngRay ),
			dOmegaRay( dOmegaRay ),
			HitPt( HitPt ),
			HitPtSurfNum( HitPtSurfNum ),
			HitPtSolRefl( HitPtSolRefl ),
			RecPtHitPtDis( RecPtHitPtDis ),
			HitPtNormVec( HitPtNormVec ),
			PossibleObsSurfNums( PossibleObsSurfNums ),
			NumPossibleObs( NumPossibleObs )
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

	//=================================================================================================

	void
	PierceSurface(
		int const ISurf, // Surface index
		Vector3< Real64 > const & R1, // Point from which ray originates
		Vector3< Real64 > const & RN, // Unit vector along in direction of ray whose
		int & IPIERC, // =1 if line through point R1 in direction of unit vector
		Vector3< Real64 > & CPhit // Point that ray along RN intersects plane of surface
	);

	//     NOTICE

	//     Copyright © 1996-2014 The Board of Trustees of the University of Illinois
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

} // SolarReflectionManager

} // EnergyPlus

#endif
