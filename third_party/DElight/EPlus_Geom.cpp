/* Copyright 1992-2009	Regents of University of California
 *						Lawrence Berkeley National Laboratory
 *
 *  Authors: R.J. Hitchcock and W.L. Carroll
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
#include <vector>
#include <map>
#include <fstream>
#include <cstring>
#include <limits>

using namespace std;

// BGL includes
#include "BGL.h"
namespace BGL = BldgGeomLib;

// includes
#include "CONST.H"
#include "DBCONST.H"
#include "DEF.H"

// WLC includes
#include "NodeMesh2.h"
#include "WLCSurface.h"
#include "helpers.h"
#include "hemisphiral.h"
#include "btdf.h"
#include "CFSSystem.h"
#include "CFSSurface.h"

// includes
#include "DOE2DL.H"
#include "EPlus_Geom.h"
#include "geom.h"
#include "GeomMesh.h"
#include "struct.h"

/****************************** subroutine CalcGeomFromEPlus *****************************/
/* Calculates geometrical values required by daylight factor calculations. */
/* The bldg structure must be fully initialized prior to this call. */

/* Building geometry input is assumed to already be in World Coordinate System. */
/****************************** subroutine CalcGeomFromEPlus *****************************/
int CalcGeomFromEPlus(
	BLDG *bldg_ptr)			/* bldg structure pointer */
{
	int iz, is, ivert, icoord, iw;

	// Transfer all Surface and Aperture vertex coordinates from the BGL data structure to SURF and WNDO::vert[NCOORDS][NVERTS]
	for (iz=0; iz<bldg_ptr->nzones; iz++) {

		for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {

			// Transfer Window vertices first so they are known at time of Surface meshing nodal calcs
			for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {

				for (ivert=0; ivert<NVERTS; ivert++) {

					for (icoord=0; icoord<NCOORDS; icoord++) {

                        BGL::point3 pt3D = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vPt3VerticesWCS_OCCW[ivert];

						bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][ivert] = pt3D[icoord];

					} // window vertex coordinate loop
				} // window vertex loop

				// Calc window height and width needed for nodal and radiosity calcs
				CalcWindowGeomFromVertices(bldg_ptr, iz, is, iw);

				// Mesh this window
                // Meshing now done in WLCSURFInit - RJH 2/25/04
//				wndo_nodal_calcs(bldg_ptr, num_wnodes, iz, is, iw, pofdmpfile);

			} // window loop

			for (ivert=0; ivert<NVERTS; ivert++) {

				for (icoord=0; icoord<NCOORDS; icoord++) {

                    BGL::point3 pt3D = bldg_ptr->zone[iz]->surf[is]->vPt3VerticesWCS_OCCW[ivert];

					bldg_ptr->zone[iz]->surf[is]->vert[icoord][ivert] = pt3D[icoord];

				} // surface vertex coordinate loop
			} // surface vertex loop

			// Calc additional surface geometry needed for nodal and radiosity calcs
			CalcSurfaceGeomFromVertices(bldg_ptr, iz, is);

			// Mesh this surface
            // Meshing now done in WLCSURFInit - RJH 2/25/04
//			nodal_calcs(bldg_ptr, num_nodes, iz, is, pofdmpfile);

		} // surface loop
	} // zone loop

	return(0);
}

/****************************** subroutine CalcSurfaceGeomFromVertices *****************************/
/* Calculates additional Surface geometrical values given vertex coordinates. */

/* Surface vertex coordinates are assumed to already be in World Coordinate System. */
/****************************** subroutine CalcSurfaceGeomFromVertices *****************************/
int CalcSurfaceGeomFromVertices(
	BLDG *bldg_ptr,			/* bldg structure pointer */
	int iz,					/* index of current zone */
	int is)					/* index of current surface */
{

	// Calc surface width and height,
	// outward (DOE2) and inward (Slite) unit vector surface normals in DOE2 BCS
	// and surface direction cosines in Slite convention (i.e., LLC from inside), BUT in DOE2 WCS (i.e., Y=North, X=East)

	int icoord;	// loop index
	double dist10, dist12;	// distances between vertices == surface height and width
	double svert0[NCOORDS],  svert1[NCOORDS],  svert2[NCOORDS];	// vertex coords
	double svect10[NCOORDS],  svect12[NCOORDS];	// unit vectors in Surface LCS Y and X axes

	// Surface vertices in DOE2 convention order.
	for (icoord=0; icoord<NCOORDS; icoord++) {
		svert0[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][0];
		svert1[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][1];
		svert2[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][2];
	}

	// Unit vectors from surface vertex 1 to 0 and 1 to 2.
	dist10 = 0.;
	dist12 = 0.;
	for (icoord=0; icoord<NCOORDS; icoord++) {
		svect10[icoord] = svert0[icoord] - svert1[icoord];
		svect12[icoord] = svert2[icoord] - svert1[icoord];
		dist10 += svect10[icoord] * svect10[icoord];
		dist12 += svect12[icoord] * svect12[icoord];
	}

	// Surface width and height
	dist10 = sqrt(dist10);
	bldg_ptr->zone[iz]->surf[is]->height = dist10;
	dist12 = sqrt(dist12);
	bldg_ptr->zone[iz]->surf[is]->width = dist12;

	// Surface Y and X axes in DOE2 WCS
	for (icoord=0; icoord<NCOORDS; icoord++) {
		svect10[icoord] /= dist10;
		svect12[icoord] /= dist12;
	}

	// Surface X-axis direction cosines in Slite convention, BUT in DOE2 WCS
	// Which means this X-axis direction is opposite to DOE2 convention
	bldg_ptr->zone[iz]->surf[is]->dircos[0] = -(svect12[0]);
	bldg_ptr->zone[iz]->surf[is]->dircos[1] = -(svect12[1]);
	bldg_ptr->zone[iz]->surf[is]->dircos[2] = -(svect12[2]);

	// Surface Y-axis direction cosines in Slite convention, BUT in DOE2 WCS
	bldg_ptr->zone[iz]->surf[is]->dircos[3] = svect10[0];
	bldg_ptr->zone[iz]->surf[is]->dircos[4] = svect10[1];
	bldg_ptr->zone[iz]->surf[is]->dircos[5] = svect10[2];

	/* Outward facing unit vector normal to surface (i.e., DOE2 outward normal) */
	dcross(svect12,svect10,bldg_ptr->zone[iz]->surf[is]->outward_uvect);

	/* Inward facing unit vector normal to surface (i.e., Slite inward normal == Z-axis direction cosines???) */
	dcross(svect10,svect12,bldg_ptr->zone[iz]->surf[is]->inward_uvect);

	// Surface Z-axis direction cosines in Slite convention, BUT in DOE2 WCS
	bldg_ptr->zone[iz]->surf[is]->dircos[6] = bldg_ptr->zone[iz]->surf[is]->inward_uvect[0];
	bldg_ptr->zone[iz]->surf[is]->dircos[7] = bldg_ptr->zone[iz]->surf[is]->inward_uvect[1];
	bldg_ptr->zone[iz]->surf[is]->dircos[8] = bldg_ptr->zone[iz]->surf[is]->inward_uvect[2];

//*pofdmpfile << "EPlus Surface[" << is << "] dircos[0] = " << bldg_ptr->zone[iz]->surf[is]->dircos[0] << " dircos[1] = " << bldg_ptr->zone[iz]->surf[is]->dircos[1] << " dircos[2] = " << bldg_ptr->zone[iz]->surf[is]->dircos[2] << "\n";
//*pofdmpfile << "EPlus Surface[" << is << "] dircos[3] = " << bldg_ptr->zone[iz]->surf[is]->dircos[3] << " dircos[4] = " << bldg_ptr->zone[iz]->surf[is]->dircos[4] << " dircos[5] = " << bldg_ptr->zone[iz]->surf[is]->dircos[5] << "\n";
//*pofdmpfile << "EPlus Surface[" << is << "] dircos[6] = " << bldg_ptr->zone[iz]->surf[is]->dircos[6] << " dircos[7] = " << bldg_ptr->zone[iz]->surf[is]->dircos[7] << " dircos[8] = " << bldg_ptr->zone[iz]->surf[is]->dircos[8] << "\n\n";

	return(0);
}

/****************************** subroutine CalcWindowGeomFromVertices *****************************/
/* Calculates additional Window geometrical values given vertex coordinates. */

/* Window vertex coordinates are assumed to already be in World Coordinate System. */
/****************************** subroutine CalcWindowGeomFromVertices *****************************/
int CalcWindowGeomFromVertices(
	BLDG *bldg_ptr,			/* bldg structure pointer */
	int iz,					/* index of current zone */
	int is,					/* index of current surface */
	int iw)					/* index of current window */
{

	// Calc window width and height,

	int icoord;	// loop index
	double dist10, dist12;	// distances between vertices == window height and width
	double svert0[NCOORDS],  svert1[NCOORDS],  svert2[NCOORDS];	// vertex coords
	double svect10[NCOORDS],  svect12[NCOORDS];	// unit vectors in Surface LCS Y and X axes

	// window vertices in DOE2 convention order.
	for (icoord=0; icoord<NCOORDS; icoord++) {
		svert0[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][0];
		svert1[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][1];
		svert2[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][2];
	}

	// Vectors from window vertex 1 to 0 and 1 to 2.
	dist10 = 0.;
	dist12 = 0.;
	for (icoord=0; icoord<NCOORDS; icoord++) {
		svect10[icoord] = svert0[icoord] - svert1[icoord];
		svect12[icoord] = svert2[icoord] - svert1[icoord];
		dist10 += svect10[icoord] * svect10[icoord];
		dist12 += svect12[icoord] * svect12[icoord];
	}

	// window width and height
	dist10 = sqrt(dist10);
	bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height = dist10;
	dist12 = sqrt(dist12);
	bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width = dist12;

	return(0);
}
