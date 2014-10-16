//	NodeMesh2.h
/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */

// This work was supported by the Assistant Secretary for Energy Efficiency 
// and Renewable Energy, Office of Building Technologies, 
// Building Systems and Materials Division of the 
// U.S. Department of Energy under Contract No. DE-AC03-76SF00098.

/*
NOTICE: The Government is granted for itself and others acting on its behalf 
a paid-up, nonexclusive, irrevocable worldwide license in this data to reproduce, 
prepare derivative works, and perform publicly and display publicly. 
Beginning five (5) years after (date permission to assert copyright was obtained),
subject to two possible five year renewals, the Government is granted for itself 
and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
license in this data to reproduce, prepare derivative works, distribute copies to 
the public, perform publicly and display publicly, and to permit others to do so. 
NEITHER THE UNITED STATES NOR THE UNITED STATES DEPARTMENT OF ENERGY, NOR ANY OF
THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL 
LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY 
INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE 
WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
*/

#include <iostream>
#include <string>
#include <vector>
using namespace std;

#include "BGL.h"
namespace BGL = BldgGeomLib;

#include "NodeMesh2.h"

Double	NodeMesh2::TotArea()
{
	Double	totarea = 0;
	for (int ii=0; ii<(int)meshList.size(); ii++) totarea += meshList[ii].area;

	return totarea;
}

void	NodeMesh2::SummaryDump()
{
	cout << "NodeMesh2:\n";
	cout << "NodeCount(meshsize): " << size() << "\n";
	cout << "TotNodeArea; " << TotArea() << "\n";
}

void	NodeMesh2::NodeDump()
{
	for (Int ii=0; ii<(int)meshList.size(); ii++) {
		cout << ii << " " << meshList[ii].position << " " << meshList[ii].area << "\n";
	}
}


Int		NodeMesh2::NearestToPext(BGL::point2 PExt) {
	Int ii, iimin = -1;
	Double	rdistsq, rdistsqmin = 1.e+50;

	for (ii=0; ii<(int)meshList.size(); ii++ ) {
		if ( meshList[ii].area == 0) continue;
		rdistsq = BGL::sqrlen(meshList[ii].position - PExt) / meshList[ii].area;
		if (rdistsq < rdistsqmin) {
			rdistsqmin = rdistsq;
			iimin = ii;
		}
	}
	return iimin;
}

int		NodeMesh2::grid1(const BGL::poly2& b2, int minNodes)
{
	if ( (b2.Area() <= 0) || (minNodes <= 0) ) {
		meshList.clear();	//	reset container
		return 0;
	}
	return NodeMesh2::grid1(b2,b2.Area()/minNodes);
}

int		NodeMesh2::grid1(const BGL::poly2& b2, double maxNodeArea)
{
	//	NOTE: order of 1st two lines is important
	meshList.clear();	//	reset container

	Node		nodeTmp;
	BGL::point2	p2Tmp;

	double		area = b2.Area();
	if ( (area == 0) || (maxNodeArea <= 0) || (maxNodeArea >= area) ) {	// force one node
		nodeTmp.area = area;
		nodeTmp.position = b2.Centroid();
		meshList.push_back(nodeTmp);
		return meshList.size();
	}

	//	set node properties, etc...
	double	nodeLen = sqrt(maxNodeArea);
//	cout << "mesh properties:\n";
//	cout << "nodeLen: " << nodeLen << "\n";

	//	find "starting point minus one square" for mesh
	double	px, px0 = b2.xMin() + nodeLen/2;
	double	py, py0 = b2.yMin() + nodeLen/2;
//	cout << "pstart: " << BGL::point2(px0,py0) << "\n";

	int count1 = 0;
	int count2 = 0;
	//	form candidate mesh points
	px = px0;
	py = py0;
	while( py < b2.yMax() ) {
		while( px < b2.xMax() ) {
			count1 += 1;
			BGL::point2 p2Tmp(px,py);
//			cout << count1 << " " << p2Tmp;
			//	if PointInPoly2 - add to mesh list
			if 	(b2.PointInPoly(p2Tmp)) {
				count2 += 1;
				nodeTmp.position = p2Tmp;
				nodeTmp.area = maxNodeArea;
				meshList.push_back(nodeTmp);
//				cout << " 1\n";
			}
//			else {
//				cout << " 0\n";
//			}
			px += nodeLen;
		}
		px = px0;
		py += nodeLen;
	}
//	cout << "count1: " << count1 << " count2: " << count2 << "\n";
	
	// no qualifying nodes found
	if ( meshList.size() == 0 ) {	// force one node
		nodeTmp.area = area;
		nodeTmp.position = b2.Centroid();
		meshList.push_back(nodeTmp);
	}

	return meshList.size();
}

int		NodeMesh2::grid2(const BGL::poly2& b2, double maxNodeArea)
{
	meshList.clear();	//	reset container
	if ( (b2.Area() <= 0) || (maxNodeArea <= 0) ) return 0;	//	zero nodes
	//	set node properties, etc...
	double	nodeLen = sqrt(maxNodeArea);
//	cout << "node properties:\n";
//	cout << "area: " << nodeArea << "\n";
//	cout << "nodeLen: " << nodeLen << "\n";

	
//	cout << "mesh properties:\n";

	//	find "starting point minus one square" for mesh
//	double	px, px0 = b2.xMin() - nodeLen/2;
//	double	py, py0 = b2.yMin() - nodeLen/2;
	double	px, px0 = b2.xMin() + nodeLen/1.999;
	double	py, py0 = b2.yMin() + nodeLen/1.999;
//	cout << "pstart: " << BGL::point2(px0,py0) << "\n";

	Node		nodeTmp;
	BGL::point2	p2Tmp;
	int count1 = 0;
	int count2 = 0;
	//	form candidate node center (mesh) points
	px = px0;
	py = py0;
	while( py < b2.yMax() + nodeLen/2 ) {
		while( px < b2.xMax() + nodeLen/2 ) {
			count1 += 1;
			//	test node center position and 4 virtual vertexes for PointInPoly2
			int	inCount = 0;
			BGL::point2	p2center(px,py);
			if 	(b2.PointInPoly(p2center)) inCount += 1;
			if 	(b2.PointInPoly(p2center - BGL::vector2(-nodeLen/2,-nodeLen/2))) inCount += 1;
			if 	(b2.PointInPoly(p2center - BGL::vector2( nodeLen/2,-nodeLen/2))) inCount += 1;
			if 	(b2.PointInPoly(p2center - BGL::vector2( nodeLen/2, nodeLen/2))) inCount += 1;
			if 	(b2.PointInPoly(p2center - BGL::vector2(-nodeLen/2, nodeLen/2))) inCount += 1;
//			cout << count1 << " " << p2center << " " << inCount << "\n";
			//	if PointInPoly2 - add to mesh list
			if 	(inCount) {
				count2 += inCount;
				nodeTmp.position = p2center;
				nodeTmp.area = maxNodeArea*inCount/5;
				meshList.push_back(nodeTmp);
			}
			px += nodeLen;
		}
		px = px0;
		py += nodeLen;
	}
//	cout << "count1: " << count1 << " count2: " << count2 << "\n";

	return meshList.size();
}

int	NodeMesh2::remove(const BGL::poly2& rpoly)
{
	int nCount = 0;
	vector<Node> tmpList;
	for (int ii=0; ii<(int)meshList.size(); ii++) {
		if 	(rpoly.PointInPoly(meshList[ii].position)) 	{
//			cout << "NodeMesh2: node " << ii << " at position " << meshList[ii].position << " removed\n";
			nCount+=1;
			continue;
		}
		tmpList.push_back(meshList[ii]);
	}
	meshList = tmpList;	//	replace old list with new list
//	cout << "NodeMesh2: " << nCount << " nodes removed\n";
	return nCount;
}

