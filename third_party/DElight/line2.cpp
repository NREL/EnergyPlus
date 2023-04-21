// line2.cpp
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
#include <cctype>
#include <iostream>
#include <sstream>
using namespace std;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include "BGL.h"

#include <limits>
#ifndef INFINITY
extern Double INFINITY;
#endif
extern Double NaN_QUIET;
extern Double MAXPointTol;

namespace BldgGeomLib {

// line2
	// Constructors
line2::line2() : origin(0,0), lDir(1,0) { }

line2::line2(const point2& p1, const vector2& dir) : origin(p1), lDir(dir) 
{
	// zero-length dir vector
	if (lDir[0] == 0 && lDir[1] == 0) {
		lDir = vector2(1,0);	// assume horizontal 
		return;
	}
	else normalize(lDir);
}

line2::line2(const point2& p1, const point2& p2) : origin(p1)
{
	// p1, p2 coincident
	if (p1 == p2) {
		lDir = vector2(1,0);	// assume horizontal 
		return;
	}

	Double xDelta = p2[0] - p1[0];
	Double yDelta = p2[1] - p1[1];
	lDir = norm(vector2(xDelta,yDelta));
	return;
}


line2::~line2() { }

	// Accessors
point2	line2::Origin() const
{
	return origin;
}

vector2	line2::dir() const
{
	return lDir;
}

// other methods
vector2	line2::normVec() const
//	"right-handed" normal
{
	return vector2(lDir[1],-lDir[0]);	
}

Double	line2::xIntercept() const
{
	if (lDir[1] == 0) {	//	horizontal line
		return INFINITY;	//	NOT DEFINED!!!
	}
	else {	
		return (origin[0] - origin[1]*lDir[0]/lDir[1]);	
	}
}

Double	line2::yIntercept() const
{
	if (lDir[0] == 0) {	//	vertical line
		return INFINITY;	//	NOT DEFINED!!!
	}
	else {
		return (origin[1] - origin[0]*lDir[1]/lDir[0]);
	}
}

point2 line2::PointOnLine(Double param) const
{
	return (origin + param*lDir);
}


Double	line2::DistToPoint(const point2& pExt) const
{
	vector2	v0(origin,pExt);
	return fabs(lDir[0]*v0[1] - lDir[1]*v0[0]);
}

Double		line2::intersectG(const line2& l2) const	//	protected
//	 general intersection case
{
	vector2	v12 = l2.Origin() - origin;
	Double	b = dot(lDir,v12);
	Double	c = dot(normVec(),v12);
	if (fabs(dot(normVec(),l2.dir())) < MAXPointTol) return -1.e+100;	// l1 and l2 are parallel WLC 3/26/2004
	Double	a = c * dot(lDir,l2.dir()) / dot(normVec(),l2.dir());
	return -a + b;
	//	General note:  if param = 0  origin of line1 ("this") is a point ON line 2
}

Double		line2::intersectH(const line2& l2) const	//	protected
//	intersection: "this" is horizontal
//	more efficient than intersectG - use when possible
{
	if (l2.dir()[0] == 0) {  // l2 is perpendicular to "this", i.e. is vertical
		return l2.Origin()[0] - origin[0];
	}  
	else {
		return (origin[1] - l2.Origin()[1])*l2.dir()[0]/l2.dir()[1] - (origin[0] - l2.Origin()[0]);
	}
}

int		line2::intersect(const line2& l2, Double& param) const
{
	if (lDir == l2.dir()) {
		return 0;	//	parallel lines
	}
	if (lDir[0] == 1) {	//	"this" is horizontal
		param = intersectH(l2);
	}
	else {	//	otherwise general case
		param = intersectG(l2);
	}
	return 1;
}

// lineseg2
lineseg2::lineseg2() : line2(), length(0) 
{ }

lineseg2::lineseg2(const point2& p1, const vector2& dir, Double len) : line2(p1,dir), length(len) 
{ }

lineseg2::lineseg2(const point2& p1, const point2& p2) : line2(p1,p2) 
{
	Double Dx = p2[0] - p1[0];
	Double Dy = p2[1] - p1[1];
	length = sqrt(Dx*Dx + Dy*Dy);
}

lineseg2::~lineseg2() { }

point2		lineseg2::end(int ii) const
{
	switch (ii) {
	case 1:
		return origin;
	case 2:
		return origin + length*lDir;
	default:
		return point2(NaN_QUIET,NaN_QUIET);	//XXXX	need range error handler here!
	}
}

int	lineseg2::intersect(const lineseg2& lsExt, Double& intersectDist) const
{
	//	construct temporary ray2 from LineSeg
	ray2 r0(origin,lDir);
	//	do ray intersection test
	int		intersectType;
	intersectType = r0.intersect(lsExt,intersectDist);
	//	test results
	switch (intersectType) {
		case -1: {
			if (fabs(intersectDist) <= MAXPointTol) return -1;
			if (fabs(Length() - intersectDist) <= MAXPointTol) return -1;
			return 0;
		}
		case 0: return 0;
		case 1: {
			if (fabs(Length() - intersectDist) <= MAXPointTol) return -1;
			if (Length() < intersectDist - MAXPointTol) return 0;
			return 1;
		}
		default: return 0;
	}

}


// ray2
ray2::ray2() : line2() { }

ray2::ray2(const point2& p1, const vector2& dir) : line2(p1,dir) { }

ray2::ray2(const point2& p1, const point2& p2) : line2(p1,p2) { }

ray2::~ray2() { }


bool	ray2::PointsTowardLine(const line2& l2) const
{
	vector2	v21(l2.Origin(),origin);
	Double	a1 = dot(v21,l2.normVec()) * dot(lDir,l2.normVec());
	return (a1 < 0);
}

int		ray2::intersect(const line2& l2, Double& param) const
{
	//	parallel, not coincident 
	if ((lDir == l2.dir()) && (l2.DistToPoint(origin))) return 0;
	//	otherwise...
	if (lDir[0] == 1) {	//	ray is +horizontal
		param = intersectH(l2);
	}
	else {	//	otherwise general case
		param = intersectG(l2);
	}
	if (param < 0) return 0;	//	NO intersect
	if (param == 0) return -1;	//	ray origin is on line
	return 1;
}

int		ray2::intersect(const lineseg2& ls2, Double& param) const
{
	//	parallel, not coincident 
	if ((lDir == ls2.dir()) && (ls2.DistToPoint(origin))) return 0;
	//	otherwise...
	if (lDir[0] == 1) {	//	ray is +horizontal: "crossings" algorithm
		// LineSeg bounding box screening tests
		if (origin[1] > ls2.yMax())	return 0;	//	above
		if (origin[1] == ls2.yMax())	return -1;	//	end point
		if (origin[1] < ls2.yMin())	return 0;	//	below
		if (origin[1] == ls2.yMin())	return -1;	//	end point
		if (origin[0] >= ls2.xMax())	return 0;	//	to the right
		//	else test for intersect inside LS bounding box
		param = intersectH(ls2);
		if ( param < 0) return 0;	//	Ray origin to the right of LineSeg
		if ( param == 0) return -1;	//	Ray origin on LineSeg or one of its end points
		//	else intersects lineSeg
		return 1;
	}
	else {	//	otherwise general case
		param = intersectG(ls2);
		if (param < -1.e+100) return 0;	//	LineSeg is parallel to (and maybe on) ray - WLC 3/26/2004
		if (param < -MAXPointTol) return 0;	//	LineSeg "behind" ray origin
		//	intersect point 
		point2	pInt = origin + param*lDir;
		// parametric length along LineSeg from its origin to pInt
		int coord = (abs(ls2.dir()[0]) > abs(ls2.dir()[1])) ? 0 : 1;
		Double Sparam = (pInt[coord] - ls2.Origin()[coord]) / ls2.dir()[coord];
//		cout << "param: " << param << " Sparam: " << Sparam << ' ' << Sparam - ls2.Length() << '\n';
		if (Sparam < -MAXPointTol) return 0;	//	outside l.s. lower end point
		if (abs(Sparam) <= MAXPointTol) return -1;	//	intersects l.s. lower end point
		if (abs(Sparam - ls2.Length()) <= MAXPointTol) return -1;	//	intersects l.s. upper end point
		if (Sparam > ls2.Length() + MAXPointTol) return 0;	//	outside l.s. upper end point
		if (abs(param) <= MAXPointTol) return -1;	//	Ray origin on LineSeg
		return 1;	//	else intersects LineSeg
	}
}

}	//	end namespace BldgGeomLib

using namespace BldgGeomLib;

ostream &operator << (ostream &s, const line2 &line)
{
	return(s << '[' << line.Origin() << ' ' << line.dir() << ']');
}

istream &operator >> (istream &s, line2 &line)
{
    point2	lorigin;
	vector2	ldir;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [Point2 Vec2] = [[1 2] [3 4]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		s >> lorigin >> ldir;	

		if (!s)
		{
//			cerr << "Error: Expected number while reading line\n";
			osstream << "line2: Expected number while reading line\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading vector\n";
			osstream << "line2: Expected ']' while reading vector\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading vector\n";
		osstream << "line2: Expected '[' while reading vector\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	line = line2(lorigin,ldir);
    return(s);
}

ostream &operator << (ostream &s, const lineseg2 &ls)
{
	return(s << '[' << ls.end(1) << ' ' << ls.end(2) << ']');
}

istream &operator >> (istream &s, lineseg2 &ls)
{
    point2	end1, end2;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [Point2 Point2] = [[1 2] [3 4]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		s >> end1 >> end2;	

		if (!s)
		{
//			cerr << "Error: Expected number while reading line\n";
			osstream << "lineseg2: Expected number while reading line\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading vector\n";
			osstream << "lineseg2: Expected ']' while reading vector\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading vector\n";
		osstream << "lineseg2: Expected '[' while reading vector\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	ls = lineseg2(end1,end2);
    return(s);
}

 
ostream &operator << (ostream &s, const ray2 &ray)
{
	return(s << '[' << ray.Origin() << ' ' << ray.dir() << ']');
}

istream &operator >> (istream &s, ray2 &ray)
{
    point2	rorigin;
	vector2	rdir;
    Char	c;
	std::ostringstream osstream;
	
	// Expected format: [Point2 Vec2] = [[1 2] [3 4]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		s >> rorigin >> rdir;	

		if (!s)
		{
//			cerr << "Error: Expected number while reading line\n";
			osstream << "ray2: Expected number while reading line\n";
			writewndo(osstream.str(),"e");
			return(s);
		}
			
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading vector\n";
			osstream << "ray2: Expected ']' while reading vector\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading vector\n";
		osstream << "ray2: Expected '[' while reading vector\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	ray = ray2(rorigin,rdir);
    return(s);
}

