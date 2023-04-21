//	plane3.h
// =================================================================================
// Copyright 1992-2009	Regents of University of California
//						Lawrence Berkeley National Laboratory

//  Authors: W.L. Carroll and R.J. Hitchcock
//           Building Technologies Department
//           Lawrence Berkeley National Laboratory

// This work was supported by the Assistant Secretary for Energy Efficiency
// and Renewable Energy, Office of Building Technologies,
// Building Systems and Materials Division of the
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

// NOTICE: The Government is granted for itself and others acting on its behalf
// a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce,
// prepare derivative works, and perform publicly and display publicly.
// Beginning five (5) years after (date permission to assert copyright was obtained),
// subject to two possible five year renewals, the Government is granted for itself
// and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
// license in this data to reproduce, prepare derivative works, distribute copies to
// the public, perform publicly and display publicly, and to permit others to do so.
// NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
// THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL
// LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
// INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE
// WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
// =================================================================================

namespace BldgGeomLib {

class DirGen;
class RHCoordSys3;

class plane3
{
public:

	// Constructors
	plane3();
	plane3(const point3& p0, const vector3& normal);
	plane3(const point3& p0, const RHCoordSys3& cs);
	plane3(const point3& p0, const point3& p1, const point3& p2);
//	plane3(const plane3 &p);			// Copy constructor

	~plane3();

	// Accessors
	point3	Origin() const;
	vector3	normVec() const;	//	ics zAxis
	vector3	icsAxis(int ii) const;
	RHCoordSys3 internalCS();
	Double	phi() const;
	Double	theta() const;
	Double	zeta() const;

	// Other Methods
	Double	DistTo(const point3& pExt) const;
	point3	Project(const point3&) const;
	bool	Behind(const point3& pExt) const;
	bool	Parallel(const plane3& plExt) const;

	// Private...
protected:

	point3		origin;
	RHCoordSys3	ics;
};


class surf3 : public plane3
{
public:

	// Constructors

	surf3();
	surf3(const string name, const vector<point3>& vp3);
	surf3(const string name, const point3& p0, const RHCoordSys3& cs, const vector<point2>& vp2);
	surf3(const string name, const point3& p0, Double Azimuth, Double Tilt, Double AxialRot, const vector<point2>& vp2);
	surf3(const string name, const point3& p0, Double Azimuth, Double Tilt, Double AxialRot, Double wd, Double ht);
//	surf3(const surf3 &s);			// Copy constructor

	~surf3();

	// Accessors
	point2	vert2D(int ii) const;
	poly2	vert2D() const;
	point3	vert3D(int ii) const;
	point2	point3to2D(const point3& p3d) const;
	point3	point2to3D(const point2& p3d) const;
	int		nvert() const;
	string	Name() const;


	// Other Methods
	void	SetName(const string name);
	point3	Centroid() {return point2to3D(vert2.Centroid());}
	bool	intersect(const ray3& r, Double& param) const;
	bool	Behind(const surf3& sExt) const;
	bool	Visible(const surf3& sExt) const;
	Double	FFtoPoint(point3& p0, vector3& dir) const;

	// Private...

protected:
	string	name;
	poly2	vert2;
	int		iHits;
};


	//	functions OUTSIDE of any object
	vector3			NewellVector(const vector<point3>& vertList3);

}	//	end namespace BldgGeomLib


std::ostream	&operator << (std::ostream &s, const BldgGeomLib::plane3 &pl);
std::istream	&operator >> (std::istream &s, BldgGeomLib::plane3 &pl);

std::ostream	&operator << (std::ostream &s, const BldgGeomLib::surf3 &pl);
std::istream	&operator >> (std::istream &s, BldgGeomLib::surf3 &pl);
