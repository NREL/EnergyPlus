// line3.cpp
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

#include <vector>
#include <cmath>
#include <cctype>
#include <iostream>
#include <sstream>
using namespace std;

extern double NaN_QUIET;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include "BGL.h"

namespace BldgGeomLib {

// line3
	// Constructors
line3::line3() : origin(0,0,0), lDir(1,0,0) { }

line3::line3(const point3& p1, const vector3& dir) : origin(p1), lDir(dir) 
{
	// zero-length dir vector
	if (sqrlen(lDir) == 0) {
		lDir = vector3(1,0,0);	// assume x-axis 
		return;
	}
	else normalize(lDir);
}

line3::line3(const point3& p1, const point3& p2) : origin(p1)
{
	// p1, p2 coincident
	if (p1 == p2) {
		lDir = vector3(1,0,0);	// assume x-axis 
		return;
	}

	double xDelta = p2[0] - p1[0];
	double yDelta = p2[1] - p1[1];
	double zDelta = p2[2] - p1[2];
	lDir = norm(vector3(xDelta,yDelta,zDelta));
	return;
}


line3::~line3() { }

	// Accessors
point3	line3::Origin() const
{
	return origin;
}

vector3	line3::dir() const
{
	return lDir;
}

// other methods
vector3	line3::normVec() const
//	only one of an infinite number of normals
{
	return norm(cross(lDir,vector3(origin - point3(0,0,0))));	
}

point3 line3::PointOnLine(double param) const
{
	return (origin + param*lDir);
}

double	line3::DistTo(const point3& pExt) const
{

	return len(cross(lDir,(pExt-origin)));
}

// lineseg3
lineseg3::lineseg3() : line3(), length(0) 
{ }

lineseg3::lineseg3(const point3& p1, const vector3& dir, double len) : line3(p1,dir), length(len) 
{ }

lineseg3::lineseg3(const point3& p1, const point3& p2) : line3(p1,p2) 
{
	double Dx = p2[0] - p1[0];
	double Dy = p2[1] - p1[1];
	double Dz = p2[2] - p1[2];
	length = sqrt(Dx*Dx + Dy*Dy + Dz*Dz);
}

lineseg3::~lineseg3() { }

double		lineseg3::Length() const
{
	return length;
}

point3		lineseg3::end(int ii) const
{
	switch (ii) {
	case 1:
		return origin;
	case 2:
		return origin + length*lDir;
	default:
		return point3(NaN_QUIET,NaN_QUIET,NaN_QUIET);	//	need error handler here!
	}
}


// ray3
ray3::ray3() : line3() { }

ray3::ray3(const point3& p1, const vector3& dir) : line3(p1,dir) { }

ray3::ray3(const point3& p1, const point3& p2) : line3(p1,p2) { }

ray3::~ray3() { }


bool	ray3::PointsToward(const plane3& pl3) const
{
	double	dist = pl3.DistTo(this->Origin());
	return (dist*dot(lDir,pl3.normVec()) < 0);
}

bool	ray3::intersect(const plane3& pl3, double& param) const
{
	//	parallel ?
 	//	ray points away from plane ?
	if (!PointsToward(pl3)) return 0;
	//	otherwise ray intersects
	param = dot(pl3.normVec(),(pl3.Origin() - origin)) / dot(pl3.normVec(),lDir);
	return 1;
}


}	//	end namespace BldgGeomLib

using namespace BldgGeomLib;

ostream &operator << (ostream &s, const line3 &line)
{
	return(s << '[' << line.Origin() << ' ' << line.dir() << ']');
}

istream &operator >> (istream &s, line3 &line)
{
    point3	lorigin;
	vector3	ldir;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [Point3 Vec3] = [[1 2 3] [4 5 6]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		s >> lorigin >> ldir;	

		if (!s)
		{
//			cerr << "Error: Expected number while reading line\n";
			osstream << "line3: Expected number while reading line\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading vector\n";
			osstream << "line3: Expected ']' while reading vector\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading vector\n";
		osstream << "line3: Expected '[' while reading vector\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	line = line3(lorigin,ldir);
    return(s);
}

ostream &operator << (ostream &s, const lineseg3 &ls)
{
	return(s << '[' << ls.end(1) << ' ' << ls.end(2) << ']');
}

istream &operator >> (istream &s, lineseg3 &ls)
{
    point3	end1, end2;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [Point2 Point2] = [[1 2 3] [4 5 6]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		s >> end1 >> end2;	

		if (!s)
		{
//			cerr << "Error: Expected number while reading line\n";
			osstream << "lineseg3: Expected number while reading line\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading vector\n";
			osstream << "lineseg3: Expected ']' while reading vector\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading vector\n";
		osstream << "lineseg3: Expected '[' while reading vector\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	ls = lineseg3(end1,end2);
    return(s);
}

ostream &operator << (ostream &s, const ray3 &ray)
{
	return(s << '[' << ray.Origin() << ' ' << ray.dir() << ']');
}

istream &operator >> (istream &s, ray3 &ray)
{
    point3	rorigin;
	vector3	rdir;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [Point2 Vec2] = [[1 2 3] [4 5 6]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		s >> rorigin >> rdir;	

		if (!s)
		{
//			cerr << "Error: Expected point while reading ray\n";
			osstream << "ray3: Expected number while reading line\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading ray\n";
			osstream << "ray3: Expected ']' while reading vector\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading ray\n";
		osstream << "ray3: Expected '[' while reading vector\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	ray = ray3(rorigin,rdir);
    return(s);
}
