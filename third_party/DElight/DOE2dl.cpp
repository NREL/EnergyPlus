//	DOE2dl.cpp
/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: W.L. Carroll and R.J. Hitchcock
 *           Building Technologies Department
 *           Lawrence Berkeley National Laboratory
 */
/**************************************************************
 * C Language Implementation of DOE2.1d and Superlite 3.0
 * Daylighting Algorithms with new Complex Fenestration System
 * analysis algorithms.
 *
 * The original DOE2 daylighting algorithms and implementation
 * in FORTRAN were developed by F.C. Winkelmann at the
 * Lawrence Berkeley National Laboratory.
 *
 * The original Superlite algorithms and implementation in FORTRAN
 * were developed by Michael Modest and Jong-Jin Kim
 * under contract with Lawrence Berkeley National Laboratory.
 **************************************************************/

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
#pragma warning(disable:4786)

// Standard includes
#include <iostream>
#include <vector>
#include <map>
using namespace std;

// BGLincludes
#include "BGL.h"
namespace BGL = BldgGeomLib;

#include	"CONST.H"
#include	"DBCONST.H"
#include	"DEF.H"
#include	"helpers.h"
#include	"hemisphiral.h"
#include	"NodeMesh2.h"
#include	"WLCSurface.h"
#include	"btdf.h"
#include	"CFSSystem.h"
#include	"CFSSurface.h"
#include	"DOE2DL.H"

SURF::SURF() :
nwndos(0), ncfs(0)
{  }

void	SURF::WLCSURFInit(string Name, Double maxNodeArea)
{
	//	assumes SURF.v3List_WLC has been initialized: CCW order for INWARD-pointing normal

	//	initialize the WLCSurface part of SURF
	WLCSurfInit(Name, v3List_WLC, maxNodeArea);

    // Check to assure that the number of mesh nodes does not exceed the array limit
    int iMaxSurfNodes = MAX_SURF_NODES;
    Double dSurfArea = Area();
    while (MeshSize() > MAX_SURF_NODES) {
        Double dAllowableMaxNodeArea = dSurfArea / iMaxSurfNodes;
        // Re-init the WLCSurface part of SURF using this maxNodeArea
	    WLCSurfInit("", v3List_WLC, dAllowableMaxNodeArea);
        iMaxSurfNodes--;
    }

	//	do mesh cutouts for child apertures
    // assumes child apertures have been fully initialized prior to this point
	int ii, jj;
	vector<BGL::point2> p2List;
	BGL::vector3	v3;
	//	LOOP over nWNDOs
	for (ii=0; ii<nwndos; ii++) {
		p2List.resize(wndo[ii]->nvert());
		for (jj=0; jj<wndo[ii]->nvert(); jj++) {
			v3 = wndo[ii]->vert3D(jj) - vert3D(0);
			p2List[jj] = BGL::point2(BGL::dot(v3,icsAxis(0)),BGL::dot(v3,icsAxis(1)));
		}
		MeshCutout(BGL::poly2(p2List));
	}
	//	LOOP over nCFSsurfs
	for (ii=0; ii<ncfs; ii++) {
		p2List.resize(cfs[ii]->nvert());
		for (jj=0; jj<cfs[ii]->nvert(); jj++) {
			v3 = cfs[ii]->vert3D(jj) - vert3D(0);
			p2List[jj] = BGL::point2(BGL::dot(v3,icsAxis(0)),BGL::dot(v3,icsAxis(1)));
		}
		MeshCutout(BGL::poly2(p2List));
	}

	//	load SURF variables
	nnodes =  MeshSize();
	for (ii=0; ii<nnodes; ii++) {
		node_areas[ii] = NodeArea(ii);
		BGL::point3 p3 = NodePosition3D(ii);
		node[ii][0] = p3[0];
		node[ii][1] = p3[1];
		node[ii][2] = p3[2];
	}
}

Double	SURF::NetArea()
{
	int	ii;
	Double area = Area();

	for (ii=0; ii<nwndos; ii++) {
		area -= wndo[ii]->Area();
	}
	for (ii=0; ii<ncfs; ii++) {
		area -= cfs[ii]->Area();
	}

	return area;
}


void	WNDO::WLCWNDOInit(Double maxNodeArea)
{
	//	assumes WNDO.v3List_WLC has been initialized: CCW order for INWARD-pointing normal

	//	initialize the WLCSurface part of WNDO
	WLCSurfInit("", v3List_WLC, maxNodeArea);

    // Check to assure that the number of mesh nodes does not exceed the array limit
    int iMaxWndoNodes = MAX_WNDO_NODES;
    Double dWndoArea = Area();
    while (MeshSize() > MAX_WNDO_NODES) {
        Double dAllowableMaxNodeArea = dWndoArea / iMaxWndoNodes;
        // Re-init the WLCSurface part of WNDO using this maxNodeArea
	    WLCSurfInit("", v3List_WLC, dAllowableMaxNodeArea);
        iMaxWndoNodes--;
    }
	nnodes =  MeshSize();

	for (int ii=0; ii<nnodes; ii++) {
		node_areas[ii] = NodeArea(ii);
		BGL::point3 p3 = NodePosition3D(ii);
		node[ii][0] = p3[0];
		node[ii][1] = p3[1];
		node[ii][2] = p3[2];
	}
}

