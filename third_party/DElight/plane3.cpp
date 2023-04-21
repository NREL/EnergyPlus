//	plane3.cpp
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

#include <iostream>
#include <sstream>
#include <iomanip>
#include <string>
#include <vector>
#include <cmath>
using namespace std;

#ifndef INFINITY
extern double INFINITY;
#endif
extern double NaN_QUIET;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include "BGL.h"
namespace BGL = BldgGeomLib;
namespace BldgGeomLib {

// plane3
	// Constructors
plane3::plane3() : origin(0,0,0), ics() { }

plane3::plane3(const point3& p0, const vector3& dir) : origin(p0)
{
	vector3	v0 = dir;
	// zero-length dir vector
	if (sqrlen(v0) == 0) {
		// assume z-axis = [0 0 1]
		ics = RHCoordSys3();	//	legal CoordSys3, but x-, y-axis not meaningful
	}
	else ics = RHCoordSys3(v0,point3(0,0,0),point3(1,0,0));	//	legal CoordSys3, but x-, y-axis arbitrarily chosen
}

plane3::plane3(const point3& p0, const RHCoordSys3& cs) : origin(p0), ics(cs)
{ }

plane3::plane3(const point3& p0,const point3& p1,const point3& p2) : origin(p0), ics(p0,p1,p2)
{ }

plane3::~plane3() { }

	// Accessors
point3	plane3::Origin() const
{
	return origin;
}

vector3	plane3::normVec() const
{
	return ics[2];
}

vector3	plane3::icsAxis(int ii) const
{
	return ics[ii];
}

RHCoordSys3 plane3::internalCS()
{
	return ics;
}

Double	plane3::phi() const
{
	return (ics.RotAngles())[0];
}

Double	plane3::theta() const
{
	return (ics.RotAngles())[1];
}

Double	plane3::zeta() const
{
	return (ics.RotAngles())[2];
}

Double	plane3::DistTo(const point3& pExt) const
{
	//	> 0:  in front;  < 0: behind; ==0: on
	return dot(ics[2],(pExt-origin));
}

point3	plane3::Project(const point3& pExt) const
{
	return pExt - DistTo(pExt)*ics[2];
}

bool	plane3::Behind(const point3& pExt) const
{
	//	> 0:  in front;  < 0: behind; ==0: on
	return DistTo(pExt) <= 0;
}

bool	plane3::Parallel(const plane3& pl3) const
{
	return len(cross(ics[2],pl3.normVec())) == 0;	// XXXX need FUZZ here?
}

//	surf3

// Constructors
surf3::surf3()
{}

surf3::surf3(const string n, const vector<point3>& vp3)
: plane3(vp3[0],RHCoordSys3(NewellVector(vp3),vp3[0],vp3[1])), name(n), iHits(0)
{
	vector<point2>	vpoly2(vp3.size());	//	OK!!!

	//	NO test for co-planarity of original 3D vertex points is done here!
	for (int ii=0; ii<(int)vp3.size(); ii++) {
		vector3	v3 = vp3[ii] - vp3[0];
		vpoly2[ii] = point2(dot(v3,ics[0]),dot(v3,ics[1]));
	}
	vert2 = poly2(vpoly2);
}


surf3::surf3(const string n, const point3& p0, const RHCoordSys3& cs, const vector<point2>& vp2) :
	plane3(p0,cs), name(n), vert2(vp2), iHits(0)
{ }

surf3::surf3(const string n, const point3& p0, Double Azimuth, Double Tilt, Double AxialRot, const vector<point2>& vp2) :
	plane3(p0,RHCoordSys3(Azimuth,Tilt,AxialRot)), name(n), vert2(vp2), iHits(0)
{ }

surf3::surf3(const string n, const point3& p0, Double Azimuth, Double Tilt, Double AxialRot, Double wd, Double ht) :
	plane3(p0,RHCoordSys3(Azimuth,Tilt,AxialRot)), name(n), iHits(0)
{
	vector<point2> vp2;
	vp2.push_back(point2(0,0));
	vp2.push_back(point2(wd,0));
	vp2.push_back(point2(wd,ht));
	vp2.push_back(point2(0,ht));
	vert2 = poly2(vp2);
}


//	surf3::surf3(const surf3 &s);			// Copy constructor

surf3::~surf3()
{ }

// Accessors
point3	surf3::vert3D(int ii) const
{
	return origin + vert2[ii][0]*ics[0] + vert2[ii][1]*ics[1];
}

point2	surf3::vert2D(int ii) const
{
	return vert2[ii];
}

poly2	surf3::vert2D() const
{
	return vert2;
}

int		surf3::nvert() const
{
	return vert2.size();
}

string	surf3::Name() const
{
	return name;
}

void	surf3::SetName(const string n)
{
	name = n;
}


// Other Methods
point2	surf3::point3to2D(const point3& p3d) const
{
	vector3	v3 = p3d - origin;
	return point2(dot(v3,ics[0]),dot(v3,ics[1]));
}

point3	surf3::point2to3D(const point2& p2d) const
{
	return origin + p2d[0]*ics[0] + p2d[1]*ics[1];
}

bool	surf3::intersect(const ray3& r0, Double& param) const
//	param is distance from r0 to intersection
{
	if (!r0.intersect(*this,param)) return 0;
	//	otherwise ray intersects plane
	//	3D intersection point - also lies in plane
	point3	pInt3 = r0.PointOnLine(param);
	//	form 2D point in plane from pInt3
	point2 pInt2 = point3to2D(pInt3);
	//	2D point-in-poly test
	return vert2.PointInPoly(pInt2);
}

bool	surf3::Behind(const surf3& sExt) const
{
	for(int ii=0; ii<sExt.nvert(); ii++) {
		if (!plane3::Behind(sExt.vert3D(ii))) return true;
	}
	return false;
}

bool	surf3::Visible(const surf3& sExt) const
{
	if (Parallel(sExt)) return false;
	if (Behind(sExt)) return false;
	return true;
}

Double	surf3::FFtoPoint(point3& p0, vector3& ndir) const
//	XXXX needs careful treatment of sign of result
{
	// XXXX needs tests for "behind" and "!points toward" for efficiency
	// what happens when done on a vertex-by-vertex basis???
//	vector3	vSum(0,0,0);	//	vector sum
	Double	sSum = 0;		//	scalar sum
	vector3	r1, r2;
	Double	theta;
	r1 = vert3D(0) - p0;
	for(int ii=0; ii<vert2.size(); ii++) {
		r2 = vert3D((ii+1)%vert2.size()) - p0;
		vector3	vcross = -cross(r1,r2);		// - sign is mystery!
		theta = acos(dot(r1,r2)/(len(r1)*len(r2)));
//		vSum += vcross * (theta / len(vcross));
		//	N.B.: ray3-Poly3 INTERSECTION TEST:
		//	if ANY value of "term" through the loop is (-) then
		//	the ray ray3(p0,ndir) DOES NOT INTERSECT the Poly3 defined on surf3
		//	This test is SLOWER than using the poly2 intersection test
		Double term = dot(ndir,vcross) * (theta / len(vcross));
// 		cout << ii << ' ' << r1 << ' ' << r2 << ' ' << vcross << ' ' << dot(ndir,vcross) << ' ' << theta << ' ' << term << '\n';
		sSum += term;
		r1 = r2;
	}
//	return dot(ndir,vSum) / (2*vl_pi*len(ndir));
	return sSum / (2*PI*len(ndir));
}

}	//	end namespace BldgGeomLib

using namespace BldgGeomLib;

ostream &operator << (ostream &s, const plane3 &plane)
{
	return(s << '[' << plane.Origin() << ' ' << plane.normVec() << ']');
}

istream &operator >> (istream &s, plane3 &plane)
{
    point3	origin;
	vector3	normdir;
    Char	c;
	std::ostringstream osstream;

	// Expected format: [Point3 Vec3] = [[1 2 3] [4 5 6]]

    while (s >> c && isspace(c))
		;

    if (c == '[')
    {
		s >> origin >> normdir;

		if (!s)
		{
//			cerr << "Error: Expected point while reading plane\n";
			osstream << "plane3: Expected point while reading plane\n";
			writewndo(osstream.str(),"e");
			return(s);
		}

		while (s >> c && isspace(c))
			;

		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading plane\n";
			osstream << "plane3: Expected ']' while reading plane\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading plane\n";
		osstream << "plane3: Expected '[' while reading plane\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}

	plane = plane3(origin,normdir);
    return(s);
}

ostream	&operator << (ostream &s, const surf3 &surf)
{
	s << surf.Name() << ' ';
	s << '[';
	for (int ii=0; ii<surf.nvert(); ii++) {
		s << surf.vert3D(ii);
	}
	s << ']';
	return(s);
}

istream	&operator >> (istream &s, surf3 &surf)
{
    Char	c;
	string	name;
	point3	p3;
    vector<point3>	VertexList;	//
	std::ostringstream osstream;

	// Expected format: name [Point3 Point3 ... for N verts] = name [[1 2 3] [4 5 6] .. [P Q R]]

	s >> name;	//	XXXX fix this logic!!!
    while (s >> c && isspace(c))
		;
    if (c == '[')
    {
		while (s) {
			s >> p3;
			VertexList.push_back(p3);

			if (!s)
			{
//				cerr << "Error: Expected point while reading vert3DList\n";
				osstream << "surf3: Expected point while reading vert3DList\n";
				writewndo(osstream.str(),"e");
				return(s);
			}
		}
		while (s >> c && isspace(c))
			;

		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading surf3\n";
			osstream << "surf3: Expected ']' while reading surf3\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading surf3\n";
		osstream << "surf3: Expected '[' while reading surf3\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}

	surf = surf3(name,VertexList);
    return(s);
}
vector3	BGL::NewellVector(const vector<point3>& v3List)
{
	//	Compute Newell Vector
	//	NOTE: "Vector3 Area" = len(NewellVector)
	//	NOTE:  get SAME result no matter which vertex you start with and whether it is coplanar or not!!!
	//	Thus only needs to be computed once, starting with any vertex point

	vector3	vNewell(0,0,0);	//	reset
	if (v3List.size() < 3) return vNewell; 	//	invalid v3List

	vector3	v1, v2;

	v1 = v3List[1] - v3List[0];
	for (int ii=2; ii<(int)v3List.size(); ii++) {
		v2 = v3List[ii] - v3List[0];
		vNewell += cross(v1,v2);
		v1 = v2;
	}
	return vNewell/2;
}
