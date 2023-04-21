// WLCSurface.cpp
/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: W.L. Carroll and R.J. Hitchcock
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
//#include "Globals.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm> // for min/max
using namespace std;

// BGLincludes
#include "BGL.h"
namespace BGL = BldgGeomLib;

#include	"DEF.H"
#include	"NodeMesh2.h"
#include	"WLCSurface.h"

WLCSurface::WLCSurface()
{ }

WLCSurface::~WLCSurface()
{ }

WLCSurface::WLCSurface(string Name, BGL::point3 Origin, Double Sazm, Double Stilt, Double Szeta, vector<BGL::point2> p2List, Double maxNodeArea)
: surf3(Name, Origin, Sazm, Stilt, Szeta, p2List), vis_refl(1.), mesh(vert2,maxNodeArea)
{ }

WLCSurface::WLCSurface(string Name, BGL::point3 Origin, Double Sazm, Double Stilt, Double Szeta, vector<BGL::point2> p2List, Double maxNodeArea, Double visrefl)
: surf3(Name, Origin, Sazm, Stilt, Szeta, p2List), vis_refl(visrefl), mesh(vert2,maxNodeArea)
{ }

WLCSurface::WLCSurface(string Name, BGL::point3 Origin, Double Sazm, Double Stilt, Double Szeta, Double width, Double height, Double maxNodeArea)
: surf3(Name, Origin, Sazm, Stilt, Szeta, width, height), vis_refl(1.), mesh(vert2,maxNodeArea)
{ }

WLCSurface::WLCSurface(string Name, BGL::point3 Origin, Double Sazm, Double Stilt, Double Szeta, Double width, Double height, Double maxNodeArea, Double visrefl)
: surf3(Name, Origin, Sazm, Stilt, Szeta, width, height), vis_refl(visrefl), mesh(vert2,maxNodeArea)
{ }

WLCSurface::WLCSurface(string Name, vector<BGL::point3> p3List, Double maxNodeArea)
{
	WLCSurfInit(Name, p3List, maxNodeArea);

}

void	WLCSurface::WLCSurfInit(string Name, vector<BGL::point3> p3List, Double maxNodeArea)
{
	name = Name;
	origin = p3List[0];
	ics = BGL::RHCoordSys3(p3List[0],p3List[1],p3List[2]);
	vector<BGL::point2>	p2List(p3List.size());	//	OK!!!

	//	NO test for co-planarity of original 3D vertex points is done here!
	for (int ii=0; ii<(int)p3List.size(); ii++) {
		BGL::vector3	v3 = p3List[ii] - p3List[0];
		p2List[ii] = BGL::point2(BGL::dot(v3,ics[0]),BGL::dot(v3,ics[1]));
	}
	vert2 = BGL::poly2(p2List);
	mesh = NodeMesh2(vert2,maxNodeArea);
}


Double	WLCSurface::Area() const {return vert2.Area();}

Double		WLCSurface::VisRefl() {return vis_refl;}

void WLCSurface::Dump()
{
	cout	<<	"WLCSurface: " << "\n";
	cout	<<  '"' << Name() << '"' << '\n';
	cout	<<  "offset: " << origin << '\n';
	cout	<<  "l.c.s.: " <<  ics << '\n';
	cout	<<  "vNorm: " << normVec() << '\n';
	cout	<<	"azm: " << phi()*180/PI << " tilt: " << theta()*180/PI << " zeta: " << zeta()*180/PI << '\n';
	cout	<<	"nVerts: " << nvert() << '\n';
	cout	<<	"Area: " << Area() << '\n';
	cout	<<	"Vis_Refl: " << vis_refl << '\n';
	cout	<<  "boundary2D: " <<  vert2 << '\n';
	cout	<<  "boundary3D: " ;
	for (int ii=0; ii<nvert(); ii++) cout << vert3D(ii) << ' ';
	cout	<< '\n';

	mesh.SummaryDump();
}

void	WLCSurface::NodeDump()
{
	mesh.SummaryDump();
	for (int ii=0; ii<MeshSize(); ii++) {
		cout << ii << " " ;
		cout << NodePosition2D(ii) << " ";
		cout << NodePosition3D(ii) << " ";
		cout << NodeArea(ii) <<" ";
//		cout << GetNodeData(ii).val1;
		cout <<"\n";
	}
}


Int	WLCSurface::MeshSize()
{
	return mesh.size();
}

Int	WLCSurface::MeshGrid1(Double nodeArea_max)
{
	return mesh.grid1(vert2,nodeArea_max);
}

Int	WLCSurface::MeshGrid2(Double nodeArea_max)
{
	return mesh.grid2(vert2,nodeArea_max);
}

Int		WLCSurface::MeshCutout(const BGL::poly2& p2)
{
	return mesh.remove(p2);
}

BGL::point2		WLCSurface::NodePosition2D(Int NodeIterator)
{
	return mesh[NodeIterator].position;
}

BGL::point3		WLCSurface::NodePosition3D(Int NodeIterator)
{
	return point2to3D(mesh[NodeIterator].position);
}

Double		WLCSurface::NodeArea(Int NodeIterator)
{
	return mesh[NodeIterator].area;
}

Double		WLCSurface::NodeOmega(Int NodeIterator, BGL::point3 extPoint)
{
	//	extPoint "in-front-of node" visibility test
	if (plane3::Behind(extPoint)) return 0;

	Double	visDot;
	Double	NodeSolidAngle = 0.;

	BGL::vector3	VDir_wcs = extPoint - NodePosition3D(NodeIterator);
	visDot = BGL::dot(BGL::norm(VDir_wcs),ics[2]);
	NodeSolidAngle = min(2*PI,visDot/BGL::sqrlen(VDir_wcs))*NodeArea(NodeIterator);	//	sqrlen() = 0 is trapped by 1st visibility culling above
		/*
		cout <<
				ii+1 <<
				" " << NodePosition3D(ii) <<
				" " << visDot <<
//				" " << VDir_wcs <<
//				" " << NodeArea(ii) <<
//				" " << BGL::sqrlen(VDir_wcs) <<
				" " << ExtPtSolidAngle <<
				" "	<< "\n";
		*/
	return NodeSolidAngle;
}

Int		WLCSurface::NearestNodeIndx(BGL::point2 extPoint)
{
	return mesh.NearestToPext(extPoint);
}

Double		WLCSurface::NodeTotIllum(Int NodeIterator)
{
	NodeData	nd = mesh[NodeIterator].data;
	return		nd.CFSIllum + nd.NodeIllum + nd.WindowIllum;
}

Double		WLCSurface::NodeTotLum(Int NodeIterator)
{
	return		vis_refl*NodeTotIllum(NodeIterator)/PI;	//	factor of 1/PI is for Lambertian surface: Ashdown Eqn 1.11
}


NodeData	WLCSurface::GetNodeData(Int NodeIterator)
{
	return mesh[NodeIterator].data;
}

void	WLCSurface::SetNodeData(Int NodeIterator, NodeData indata)
{
	mesh[NodeIterator].data = indata;
}

BGL::vector3	WLCSurface::DirNodetoExt(Int NodeIterator, BGL::point3 ExtPoint)
{
	return BGL::norm(BGL::vector3(NodePosition3D(NodeIterator), ExtPoint));
}

void SurfNodeIllumPlotArray(vector<WLCSurface *> pWLCSurfList, string outfilename)
{
	int		jj, kk;

	//	open plotfile
	ofstream	plotfile(outfilename.c_str());
	for (jj=0; jj<(int)pWLCSurfList.size(); jj++) {	// pWLCSurfList loop

		//	the following ASSUMES Node areas are const and nodes are square
		double	nodeArea = pWLCSurfList[jj]->NodeArea(0);

		double	xMeshMin, xMeshMax;
		double	yMeshMin, yMeshMax;
		BGL::point2	SurfNodePosition2D;
		for (kk=0; kk < pWLCSurfList[jj]->MeshSize(); kk++) {	//  WLCSurf mesh iterator loop pass 2
			SurfNodePosition2D = pWLCSurfList[jj]->NodePosition2D(kk);
			//	find mesh x/y max, x/y min
			if (kk == 0) {
				xMeshMin = xMeshMax = SurfNodePosition2D[0];
				yMeshMin = yMeshMax = SurfNodePosition2D[1];
			}
			if (SurfNodePosition2D[0] < xMeshMin) xMeshMin = SurfNodePosition2D[0];
			if (SurfNodePosition2D[0] > xMeshMax) xMeshMax = SurfNodePosition2D[0];
			if (SurfNodePosition2D[1] < yMeshMin) yMeshMin = SurfNodePosition2D[1];
			if (SurfNodePosition2D[1] > yMeshMax) yMeshMax = SurfNodePosition2D[1];
		}
//		cout << "xMeshMin: " << xMeshMin << " xMeshMax: " << xMeshMax << " yMeshMin: " << yMeshMin << " yMeshMax: " << yMeshMax << "\n";

		BGL::point2	LLHC(xMeshMin,yMeshMin);
//		cout << "LLHC: " << LLHC << "\n";

		int		Nx = (int)((xMeshMax - xMeshMin)/sqrt(nodeArea) + 1);
		int		Ny = (int)((yMeshMax - yMeshMin)/sqrt(nodeArea) + 1);
//		cout << "Nx: " << Nx << " Ny: " << Ny << "\n";

		//	create 2D PlotArray
		vector< vector<double> > PlotArray(Nx);
		int ii1;
		for (ii1=0; ii1<Nx; ii1++) PlotArray[ii1].resize(Ny,0);


		int		xIndx;
		int		yIndx;
		for (kk=0; kk < pWLCSurfList[jj]->MeshSize(); kk++) {	//  WLCSurf mesh iterator loop pass 3
			SurfNodePosition2D = pWLCSurfList[jj]->NodePosition2D(kk);
			xIndx = (int)((SurfNodePosition2D[0] - LLHC[0])/sqrt(nodeArea));
			yIndx = (int)((SurfNodePosition2D[1] - LLHC[1])/sqrt(nodeArea));
			PlotArray[xIndx][yIndx] = pWLCSurfList[jj]->NodeTotIllum(kk);
		}

		//	print 2D PlotArray to file
		plotfile << "Surface: " << pWLCSurfList[jj]->Name() << "\n";
		for (int ii2=0; ii2<Ny; ii2++) {
//			plotfile << ii2 << " ";
			for (ii1=0; ii1<Nx; ii1++) {
				plotfile << PlotArray[ii1][ii2] << " ";
			}
			plotfile << "\n";
		}
	}	//	end jj loop
	plotfile.close();
	cout << "surf.plot" << " saved\n";
}
