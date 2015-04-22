#ifndef DXFEarClipping_hh_INCLUDED
#define DXFEarClipping_hh_INCLUDED

// ObjexxFCL Headers
#include <ObjexxFCL/Array1A.hh>

// EnergyPlus Headers
#include <EnergyPlus.hh>
#include <DataVectorTypes.hh>

namespace EnergyPlus {

namespace DXFEarClipping {

	// Using/Aliasing
	using DataVectorTypes::Vector;
	using DataVectorTypes::Vector_2d;
	using DataVectorTypes::dTriangle;

	// Data

	// Derived type definitions:
	// na

	// Module variable declarations:
	// na
	extern bool trackit;
	// Subroutine specifications for module <module_name>:

	// Functions

	bool
	InPolygon(
		Vector const & point,
		Array1A< Vector > poly,
		int const nsides
	);

	Real64
	Modulus( Vector const & point );

	int
	Triangulate(
		int const nsides, // number of sides to polygon
		Array1A< Vector > polygon,
		Array1D< dTriangle > & outtriangles,
		Real64 const surfazimuth, // surface azimuth angle (outward facing normal)
		Real64 const surftilt, // surface tilt angle
		std::string const & surfname, // surface name (for error messages)
		int const surfclass // surface class
	);

	Real64
	angle_2dvector(
		Real64 const xa, // vertex coordinate
		Real64 const ya, // vertex coordinate
		Real64 const xb, // vertex coordinate
		Real64 const yb, // vertex coordinate
		Real64 const xc, // vertex coordinate
		Real64 const yc // vertex coordinate
	);

	bool
	polygon_contains_point_2d(
		int const nsides, // number of sides (vertices)
		Array1A< Vector_2d > polygon, // points of polygon
		Vector_2d const & point // point to be tested
	);

	void
	generate_ears(
		int const nvert, // number of vertices in polygon
		Array1A< Vector_2d > vertex,
		Array1A_int ears, // number of ears possible (dimensioned to nvert)
		int & nears, // number of ears found
		Array1A_int r_vertices, // number of reflex vertices (>180) possible
		int & nrverts, // number of reflex vertices found (>=180)
		Array1A_int c_vertices, // number of convex vertices
		int & ncverts, // number of convex vertices found (< 180)
		Array1A_bool removed, // array that shows if a vertex has been removed (calling routine)
		Array1A_int earvert, // vertex indicators for first ear
		Array1A< Real64 > rangles
	);

	void
	CalcWallCoordinateTransformation(
		int const nsides,
		Array1A< Vector > polygon,
		Real64 const surfazimuth,
		Real64 const surftilt, // unused1208
		Array1A< Real64 > xvt,
		Array1A< Real64 > yvt,
		Array1A< Real64 > zvt
	);

	void
	CalcRfFlrCoordinateTransformation(
		int const nsides,
		Array1A< Vector > polygon,
		Real64 const surfazimuth, // unused1208
		Real64 const surftilt,
		Array1A< Real64 > xvt,
		Array1A< Real64 > yvt,
		Array1A< Real64 > zvt
	);

	void
	reorder( int & nvert ); // unused1208

} // DXFEarClipping

} // EnergyPlus

#endif
