//	poly2.cpp
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
#include <sstream>
#include <sstream>
#include <iostream>
#include <algorithm> // for min/max
using namespace std;

// writewndo() Error handler include
#include "DElightManagerC.h"

#include "BGL.h"

#include "rand.h"

#ifndef INFINITY
extern double INFINITY;
#endif
extern double NaN_QUIET;
extern double MAXPointTol;

//	poly2 implementation

namespace BldgGeomLib {

poly2::poly2() {}	

poly2::poly2(const vector<point2>& VertList)
{
	ostringstream throwstr;
	int	ii, jj;
	int Nv = VertList.size();

	// check for minimum size
	if (Nv < 3) {
		throwstr << "poly2: Nv < 3: " << Nv;
//		throw throwstr.str();
	}
	//	error-check and load internal vertex list point-by-point
	for (ii=0; ii<Nv; ii++) {
		//	check for zero-length edges
		if (VertList[ii] == VertList[(Nv+ii-1)%Nv]) {
//			throwstr << "poly2: repeated vertex: " << ii << ' ' << VertList[ii] << '\n';
//			throw throwstr.str();
		}
		//	check for edge crossings
		if (ii>0) {
			lineseg2	p2edge(VertList[ii],VertList[(ii+1)%Nv]);
			Double	tdist;
			for (jj=0; jj<ii; jj++) {
				int iVal = p2edge.intersect(lsEdge(jj),tdist);
				if (iVal == 1) {
//					throwstr << "poly2: illegal edge intersection: e" << ii << "-e" << jj << ' ' << p2edge.Origin() << ' ' << tdist << ' ' << p2edge.Origin() + p2edge.dir()*tdist << '\n';
//					throw throwstr.str();
				}
			}
		}
		//	otherwise add the point to the poly2 vert list vPoly
		vPoly.push_back(VertList[ii]);
	}

	//	check for valid TotExtAngle
	Double	tea = TotExtAngle();
	if (abs(tea - 2*PI) > MAXPointTol) {
		throwstr << "poly2 error: TotExtAngle != +2PI: " << tea << '\n';
//		throw throwstr.str();
	}
	Double	poly2Area = Area();
	//	check for degeneracy via area == 0
	//	check for vertex CCW order (OK) or CW order (bad) via area < 0
	if (poly2Area <= 0) {
		throwstr << "poly2: illegal Area: " << poly2Area << '\n';
//		throw throwstr.str();
	}

	//	init MinMax elements
	vMinMax();
}

poly2::~poly2()
{
}

int			poly2::size() const
{
	return vPoly.size();
}

lineseg2	poly2::lsEdge(int ii) const		//	ii = 0 -> N-1
{
	return lineseg2(vPoly[ii%vPoly.size()], vPoly[(ii+1)%vPoly.size()]);
}

vector2	poly2::vEdge(int ii) const		//	ii = 0 -> N-1
{
	return vector2(vPoly[ii%vPoly.size()], vPoly[(ii+1)%vPoly.size()]);
}

Double	poly2::ExtAngle(int ii) const		//	ii = 0 -> N-1
//	N.B.  This can't discriminate between Theta = +PI or -PI - always gives +PI
{
	vector2	v1, v2;
	v1 = ii == 0 ? vEdge(vPoly.size()-1) : vEdge(ii-1);
	v2 = vEdge(ii);
	Double	cosTheta = dot(v1,v2) / (len(v1)*len(v2));
	if (cosTheta > 1) cosTheta = 1;
	else if (cosTheta < -1) cosTheta = -1;
	Double	Theta = acos(cosTheta);
	Double	arg = (v1[0]*v2[1] - v1[1]*v2[0]);	//	sinTheta = arg / (len(v1)*len(v2))
	if (arg < 0) Theta = -Theta;
//	cerr << ii << ' ' << cosTheta << ' ' << Theta << ' ' << arg << '\n';
	return Theta;
}

Double	poly2::TotExtAngle() const
{
	Double	totAngle = 0;
	for (int ii=0; ii<(int)vPoly.size(); ii++) {
		totAngle += ExtAngle(ii);
	}
	return totAngle;
}

point2		poly2::Centroid() const
//	MODS	WLC 3/16/2005
{
//	cout << "poly2::Centroid:\n"; 

	int		ii;
	int		Nv = vPoly.size();

	point2	vert1, vert2;;
	point2	TriCenter;
	point2	polycent1(0,0), polycent2(0,0);
	vector2	vedge1, vedge2, vnorm;
//	Double	len1, len2;
	Double	TotArea2 = 0, TriArea2;	//	2 x actual values

	for (ii = 1; ii < Nv-1; ii++) {
		//	triangle area
		vert1 = vPoly[ii];
		vert2 = vPoly[ii+1];
		vedge1 = vert1 - vPoly[0];
		vedge2 = vert2 - vPoly[0];

		TriArea2 = vedge1[0]*vedge2[1] - vedge2[0]*vedge1[1];	//	actually 2 x TriArea
//		cout << ii << "TriArea2 " << TriArea2 << " vedge1: [" << vedge1[0] << "," << vedge1[1] << "]" << " vedge2: [" << vedge2[0] << "," << vedge2[1] << "]\n"; 

		//	triangle centroid
		if (TriArea2 == 0) {	//	handle zero-area degenerate cases by jiggling points
			vert1 += 1.e-6*vector2(1,0);
			vert2 += 1.e-6*vector2(0,1);
			vedge1 = vert1 - vPoly[0];
			vedge2 = vert2 - vPoly[0];
			TriArea2 = vedge1[0]*vedge2[1] - vedge2[0]*vedge1[1];	//	actually 2 x TriArea
//			cout << ii << "TriArea2 " << TriArea2 << " vedge1: [" << vedge1[0] << "," << vedge1[1] << "]" << " vedge2: [" << vedge2[0] << "," << vedge2[1] << "]\n"; 
		}
		TotArea2 += TriArea2;
		TriCenter = vPoly[0] + (vedge1 + vedge2)/3;
		polycent1 += TriCenter*TriArea2 - point2(0,0);
		polycent2 += TriCenter - point2(0,0);

//		cout << ii << " TriArea2: " << TriArea2 << " TriCenter: [" << TriCenter[0] << "," << TriCenter[1] << "]\n"; 
//		cout << ii << " TriArea2: " << TriArea2 << " polycent2: " << polycent2 << "\n"; 

	}
//	cout << "TotArea2: " << TotArea2;
	return (TotArea2 != 0 ? polycent1 / TotArea2 : polycent2 / (Nv-2));
}

Double		poly2::Area() const	
// Comp Geom FAQ 2.01
{
	int	ii, Nv = vPoly.size();
	if (Nv < 3) return 0;
/*
	// FORM 3 version
	Double	Area = 0;
	for (ii = 0; ii <= Nv-1; ii++) {
		Area += (vPoly[ii][0] + vPoly[(ii+1)%Nv][0])*(vPoly[(ii+1)%Nv][1] - vPoly[ii][1]);	// Comp Geom FAQ 2.01
	}
*/
	// FORM 4 version
	Double	Area2 = 0;
	for (ii = 1; ii <= Nv; ii++) {
		Area2 += vPoly[ii%Nv][0]*(vPoly[(ii+1)%Nv][1] - vPoly[(ii-1)][1]);	// Comp Geom FAQ 2.01
	}
	return Area2/2;
}

Double		poly2::Circumference() const
{
	int		ii, Nv = vPoly.size();
	Double	TotEdgeLen = 0;

	for (ii = 1; ii <= Nv; ii++) {
		TotEdgeLen += lsEdge(ii).Length();
	}
	return TotEdgeLen;
}

Double		poly2::CircumferenceRatio() const
{
	Double area = Area();
	return (area == 0 ? 0 : Circumference() / (2*sqrt(PI*area)));
}

	void	poly2::vMinMax()
{
	vxMax = vPoly[0][0];
	vxMin = vPoly[0][0];
	vyMax = vPoly[0][1];
	vyMin = vPoly[0][1];
	for (int ii=1; ii<(int)vPoly.size(); ii++) {
		vxMax = max(vxMax,vPoly[ii][0]);	//	VS2008
		vxMin = min(vxMin,vPoly[ii][0]);	//	VS2008
		vyMax = max(vyMax,vPoly[ii][1]);	//	VS2008
		vyMin = min(vyMin,vPoly[ii][1]);	//	VS2008
	}
}

bool		poly2::PointInPoly(const point2& p0) const
{
	//	Poly bounding box test
	if (p0[0] <= vxMin || p0[0] >= vxMax) return false;
	if (p0[1] <= vyMin || p0[1] >= vyMax) return false;

	//	else Ray-PolyEdge intresection tests
	ray2	rHor(p0,vector2(1,0));
	bool	inFlag = false;
	bool	vFlag = false;
	point2 vInt;
	for (int jj=0; jj<size(); jj++) {
//		cout << "\n" << jj << " " << (jj+1)%4 << " " << vPoly[jj] << " " << vPoly[(jj+1)%4] << " ";
		if (vFlag && (sqrdist(vInt,vPoly[jj]) < MAXPointTol || sqrdist(vInt,vPoly[(jj+1)%4]) < MAXPointTol )) {	//	cached vertex intersection point test
			vFlag = false;
			continue;	//	skip test for this edge
		}
		Double tParam = NaN_QUIET;
		int	result = rHor.intersect(lsEdge(jj),tParam);
//		cout << result << " " << tParam << " " ;
		if (result == 0) {
			continue;
		}
		if (result == -1) {	//	vertex intersection
			vFlag = true;
			vInt = p0 + tParam*rHor.dir();
// 			cout << vInt << " ";
			if (vInt[1] == yMax() || vInt[1] == yMin()) continue;	//	NECESSARY! - but only works for convex polygons!
		}
		inFlag = !inFlag;
// 		cout << inFlag << " ";
	}
// 		cout << "\n";
	return	inFlag;
}

point2		poly2::RandInPoly() const
{
	Double dx = xMax() - xMin();
	Double dy = yMax() - yMin();
	Double range = max(dx,dy);	//	VS2008

	// rejection sampling logic  
	point2 p0;	
	do {
		Double	x = xMin() + range*RandU();
		Double	y = yMin() + range*RandU();
		p0 = point2(x,y);	
	} while (!PointInPoly(p0));	
	return p0;
}

}	//	end namespace BldgGeomLib

using namespace BldgGeomLib;

ostream &operator << (ostream &s, const poly2& p2)
{
	s << '[' ;
	for (int ii=0; ii<p2.size(); ii++) {
		s << p2[ii];
	}
	s << ']';
	return(s);
}

istream &operator >> (istream &s, poly2 &p2)
{
    Char	c;
	std::ostringstream osstream;
	point2		pt2;
    vector<point2>	VertexList;
	
	// Expected format: [Point2 Point2 ... for N verts] = [[1 2] [3 4] .. [N M]]
	
    while (s >> c && isspace(c))		
		;
		
    if (c == '[')						
    {
		while (s) {
			s >> pt2;	
			VertexList.push_back(pt2);	

			if (!s)
			{
//				cerr << "Error: Expected point while reading poly2\n";
				osstream << "poly2: Expected point while reading poly2\n";
				writewndo(osstream.str(),"e");
				return(s);
			}
		}	
		while (s >> c && isspace(c))
			;
			
		if (c != ']')
    	{
    		s.clear(ios::failbit);
//	    	cerr << "Error: Expected ']' while reading poly\n";
			osstream << "poly2: Expected ']' while reading poly2\n";
			writewndo(osstream.str(),"e");
	    	return(s);
    	}
	}
    else
	{
	    s.clear(ios::failbit);
//	    cerr << "Error: Expected '[' while reading poly\n";
		osstream << "poly2: Expected '[' while reading poly2\n";
		writewndo(osstream.str(),"e");
	    return(s);
	}
	
	p2 = poly2(VertexList);
    return(s);
}

