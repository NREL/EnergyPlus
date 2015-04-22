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
