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
#include "geom.h"
#include "GeomMesh.h"

/*************************** subroutine dircos_calc **************************/
/* Calculates direction cosine values for a surface in slite bldg coord sys. */
/* Based on Superlite conventions. */
/* Uses surface tilt and zone azimuths to determine slite surface angles. */
/* Radiosity modification */
// RJH - 8/26/03 - This routine is used for direct input to DElight (i.e., not from EnergyPlus)
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/*************************** subroutine dircos_calc **************************/
int dircos_calc(
	BLDG *bldg_ptr,	/* pointer to bldg structure */
	int iz,			/* current zone index */
	int is)			/* current surface index */
{
	double beta1, beta2, psi1, psi2;	/* slite surface orientation angles */
	double surf_azm_bs;	/* surface azm in DOE2 bldg coord sys */
	double sbx, cbx, sby, cby, spx, cpx, spy, cpy;	/* temp sin/cos vars */

	/* calc slite beta and psi angles from surf tilt and bldg coord sys azm */
	/* note that DOE2 bldg coord sys is clockwise north and */
	/* slite angles are counter-clockwise south */

	/* beta1 is always 90 since all DOE2 surfaces have a horiz local x-axis */
	beta1 = 90.;

	// RJH 8-26-03 - Use Surface Tilt in bldg sys coords here
//	beta2 = fabs(bldg_ptr->zone[iz]->surf[is]->tilt_zs - 90.0);
	beta2 = fabs(bldg_ptr->zone[iz]->surf[is]->tilt_bs - 90.0);

	/* calc surface azimuth in DOE2 bldg coord sys using zone azm */
//	surf_azm_bs = bldg_ptr->zone[iz]->surf[is]->azm_zs + bldg_ptr->zone[iz]->azm;
	// RJH 8-26-03 - Use Surface Azm directly here
	surf_azm_bs = bldg_ptr->zone[iz]->surf[is]->azm_bs;

	/* psi1 based on surf_azm_bs and converted from DOE2 to slite bldg coord system */
	psi1 = -(surf_azm_bs + 90.0) + 180.0;

	/* psi2 is always 0 for vertical surfaces */
	if (bldg_ptr->zone[iz]->surf[is]->tilt_zs == 90.0) {
		psi2 = 0.0;
	}
	/* for non-vertical surfaces, relationship between psi2 and azm is */
	/* based on tilt (tilt > 90 => azm points "down") */
	else if (bldg_ptr->zone[iz]->surf[is]->tilt_zs > 90.0) {
		psi2 = surf_azm_bs;
		/* convert to slite bldg coord sys */
		psi2 = -(psi2) + 180.0;
	}
	else { // if (bldg_ptr->zone[iz]->surf[is]->tilt_zs < 90.0) {
		psi2 = surf_azm_bs + 180.0;
		if (psi2 > 360.0) psi2 -= 360.0;
		/* convert to slite bldg coord sys */
		psi2 = -(psi2) + 180.0;
	}
/* rob strt
fprintf(dmpfile,"surface [%s] tilt_zs = %7.2f azm_bs = %7.2f\nbeta1 = %7.2f beta2 = %7.2f psi1 = %7.2f psi2 = %7.2f\n", bldg_ptr->zone[iz]->surf[is]->name,bldg_ptr->zone[iz]->surf[is]->tilt_zs,surf_azm_bs,beta1,beta2,psi1,psi2);
rob end */

	/* convert angles to radians */
	beta1 *= DTOR;
	beta2 *= DTOR;
	psi1 *= DTOR;
	psi2 *= DTOR;
/* rob strt
fprintf(dmpfile,"beta1 = %7.2f beta2 = %7.2f psi1 = %7.2f psi2 = %7.2f\n", beta1,beta2,psi1,psi2);
rob end */

	/* calc sin and cos of slite angles */
	sbx = sin(beta1);
	cbx = cos(beta1);
	sby = sin(beta2);
	cby = cos(beta2);
	spx = sin(psi1);
	cpx = cos(psi1);
	spy = sin(psi2);
	cpy = cos(psi2);

	/* calc slite direction cosine values of angles for this surf */
	/* Note: equations for dircos 0 and 1 are reversed from slite */
	/* because of differences in slite and doe2 coordinate systems */
	bldg_ptr->zone[iz]->surf[is]->dircos[1] = sbx * cpx;
	bldg_ptr->zone[iz]->surf[is]->dircos[0] = sbx * spx;
	bldg_ptr->zone[iz]->surf[is]->dircos[2] = cbx;
	/* Note: equations for dircos 3 and 4 are reversed from slite */
	bldg_ptr->zone[iz]->surf[is]->dircos[4] = sby * cpy;
	bldg_ptr->zone[iz]->surf[is]->dircos[3] = sby * spy;
	bldg_ptr->zone[iz]->surf[is]->dircos[5] = cby;
	/* Note: equations for dircos 6 and 7 are reversed from slite */
	bldg_ptr->zone[iz]->surf[is]->dircos[7] = sbx * spx * cby - cbx * sby * spy;
	bldg_ptr->zone[iz]->surf[is]->dircos[6] = cbx * sby * cpy - sbx * cpx * cby;
	bldg_ptr->zone[iz]->surf[is]->dircos[8] = sbx * sby * (spy * cpx - cpy * spx);

//*pofdmpfile << "dircos_calc() Surface[" << is << "] dircos[0] = " << bldg_ptr->zone[iz]->surf[is]->dircos[0] << " dircos[1] = " << bldg_ptr->zone[iz]->surf[is]->dircos[1] << " dircos[2] = " << bldg_ptr->zone[iz]->surf[is]->dircos[2] << "\n";
//*pofdmpfile << "dircos_calc() Surface[" << is << "] dircos[3] = " << bldg_ptr->zone[iz]->surf[is]->dircos[3] << " dircos[4] = " << bldg_ptr->zone[iz]->surf[is]->dircos[4] << " dircos[5] = " << bldg_ptr->zone[iz]->surf[is]->dircos[5] << "\n";
//*pofdmpfile << "dircos_calc() Surface[" << is << "] dircos[6] = " << bldg_ptr->zone[iz]->surf[is]->dircos[6] << " dircos[7] = " << bldg_ptr->zone[iz]->surf[is]->dircos[7] << " dircos[8] = " << bldg_ptr->zone[iz]->surf[is]->dircos[8] << "\n\n";


	return(0);
}

/*************************** subroutine dircos_calc_new **************************/
/* Calculates direction cosine values for a surface using Superlite conventions, */
/* BUT in DOE2 WCS. */
// RJH - 10/16/03 - This routine is used for old format input to DElight (i.e., not from EnergyPlus)
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/*************************** subroutine dircos_calc_new **************************/
int dircos_calc_new(
	BLDG *bldg_ptr,	/* pointer to bldg structure */
	int iz,			/* current zone index */
	int is)			/* current surface index */
{
	// Calc surface direction cosines in Slite convention (i.e., LLC from inside), BUT in DOE2 WCS (i.e., Y=North, X=East)

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
	dist12 = sqrt(dist12);

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

//*pofdmpfile << "dircos_calc_new() Surface[" << is << "] dircos[0] = " << bldg_ptr->zone[iz]->surf[is]->dircos[0] << " dircos[1] = " << bldg_ptr->zone[iz]->surf[is]->dircos[1] << " dircos[2] = " << bldg_ptr->zone[iz]->surf[is]->dircos[2] << "\n";
//*pofdmpfile << "dircos_calc_new() Surface[" << is << "] dircos[3] = " << bldg_ptr->zone[iz]->surf[is]->dircos[3] << " dircos[4] = " << bldg_ptr->zone[iz]->surf[is]->dircos[4] << " dircos[5] = " << bldg_ptr->zone[iz]->surf[is]->dircos[5] << "\n";
//*pofdmpfile << "dircos_calc_new() Surface[" << is << "] dircos[6] = " << bldg_ptr->zone[iz]->surf[is]->dircos[6] << " dircos[7] = " << bldg_ptr->zone[iz]->surf[is]->dircos[7] << " dircos[8] = " << bldg_ptr->zone[iz]->surf[is]->dircos[8] << "\n\n";


	return(0);
}

/*************************** subroutine nodal_calcs **************************/
/* Calculates nodal area for each surface */
/* Calculates nodal coordinates in bldg coord sys. */
/* Based on Superlite conventions. */
/* Radiosity modification */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/*************************** subroutine nodal_calcs **************************/
int nodal_calcs(
	BLDG *bldg_ptr,	/* pointer to bldg structure */
	int num_nodes,	/* total number of nodes on surface */
	int iz,			/* current zone index */
	int is)			/* current surface index */
{
	int n_width, n_height;	/* temp node count holders */
	int icoord, iwidth, iheight;		/* loop indexes */
	int inode;		/* current node counter and index */
	double v0[NCOORDS], v1[NCOORDS], v2[NCOORDS];	/* surface vertices */
	double edge_width[NCOORDS], edge_height[NCOORDS];	/* node edge lengths */
	double row_strt[NCOORDS], test_node[NCOORDS];	/* tmp vars for node calcs */

	/* calculate number of nodes in width and height directions. */
	/* based on user entered total number of nodes and surface aspect ratio */
	n_width = (int)(sqrt(num_nodes * bldg_ptr->zone[iz]->surf[is]->width / bldg_ptr->zone[iz]->surf[is]->height) + 0.5);
	bldg_ptr->zone[iz]->surf[is]->n_width = n_width;
	n_height = (int)((num_nodes / n_width) + 0.5);
	/* assure that n_width * n_height < MAX_SURF_NODES */
	while ((n_width * n_height) > MAX_SURF_NODES)
		// Decrement n_height (i.e., favor more nodes along width of surface).
		n_height--;
	bldg_ptr->zone[iz]->surf[is]->n_height = n_height;

	/* calculate nodal area for each surface */
	bldg_ptr->zone[iz]->surf[is]->node_area = (bldg_ptr->zone[iz]->surf[is]->width * bldg_ptr->zone[iz]->surf[is]->height) / (n_width * n_height);

	/* Calculate coordinates of each node on surface */

	/* Calculate node edge lengths (signed) for surface. */
	/* Surface vertices (numbered counter-clockwise starting at */
	/* upper left (vert[icoord][0]) viewed from OUTSIDE of room). */
	/* Note that v1 is therefore DOE-2 input origin. */
	for (icoord=0; icoord<NCOORDS; icoord++) {
		v0[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][0];
		v1[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][1];
		v2[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][2];
		/* signed length of width edge along each axis */
		/* equals the width of the surface along each axis divided by */
		/* the number of nodes along the width */
		edge_width[icoord] = (v2[icoord]-v1[icoord]) / n_width;
		/* signed length of height edge along each axis */
		/* equals the height of the surface along each axis divided by */
		/* the number of nodes along the height */
		edge_height[icoord] = (v0[icoord]-v1[icoord]) / n_height;
	}

//*pofdmpfile << " Surface edge_width[0] = " << edge_width[0] << " edge_width[1] = " << edge_width[1] << " edge_width[2] = " << edge_width[2] << "\n";
//*pofdmpfile << " Surface edge_height[0] = " << edge_height[0] << " edge_height[1] = " << edge_height[1] << " edge_height[2] = " << edge_height[2] << "\n";

	/* Calc coordinates of nodal starting point, positioned near DOE2 origin */
	/* at 1/2 node height "below" edge of surface defined by v1 and v2 */
	for (icoord=0; icoord<NCOORDS; icoord++) {
		row_strt[icoord] = v1[icoord] + edge_width[icoord] * 0.5 - edge_height[icoord] * 0.5;
	}
	inode = 0;
	/* calc coordinates of nodes on surface */
	for (iheight=0; iheight<n_height; iheight++) {
		/* calc first node in row */
		for (icoord=0; icoord<NCOORDS; icoord++) {
			/* move from first node of previous row to first node of this row */
			row_strt[icoord] += edge_height[icoord];
			test_node[icoord] = row_strt[icoord];
		}
		/* check to see if test node falls within a surface cutout */
		// RJH 10-16-03 switch to new cutout chk version
//		if (cutout_chk(test_node,bldg_ptr,iz,is) == 0) {
		if (cutout_chk_new(test_node,bldg_ptr,iz,is) == 0) {
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->surf[is]->node[inode][icoord] = test_node[icoord];
			}
			inode += 1;
		}
		for (iwidth=1; iwidth<n_width; iwidth++) {
			/* calc next node along current row */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				test_node[icoord] += edge_width[icoord];
			}
			/* check to see if test node falls within a surface cutout */
			// RJH 10-16-03 switch to new cutout chk version
	//		if (cutout_chk(test_node,bldg_ptr,iz,is) == 0) {
			if (cutout_chk_new(test_node,bldg_ptr,iz,is) == 0) {
				for (icoord=0; icoord<NCOORDS; icoord++) {
					bldg_ptr->zone[iz]->surf[is]->node[inode][icoord] = test_node[icoord];
				}
				inode += 1;
			}
		}
	}

	/* store number of nodes on this surface */
	bldg_ptr->zone[iz]->surf[is]->nnodes =  inode;

	return(0);
}

/************************* subroutine wndo_nodal_calcs ************************/
/* Calculates nodal area for each window */
/* Calculates window nodal coordinates in bldg coord sys. */
/* Based on Superlite conventions. */
/* Radiosity modification */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************* subroutine wndo_nodal_calcs ************************/
int wndo_nodal_calcs(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	int num_wnodes,		/* total number of nodes on window */
	int iz,				/* current zone index */
	int is,				/* current surface index */
	int iw)				/* current window index */
{
	int n_width, n_height;		/* number of nodes in width and height directions */
	int icoord, iwidth, iheight;		/* loop indexes */
	int inode;		/* current node counter and index */
	double v0[NCOORDS], v1[NCOORDS], v2[NCOORDS];	/* surface vertices */
	double edge_width[NCOORDS], edge_height[NCOORDS];	/* node edge lengths */
	double row_strt[NCOORDS], next_node[NCOORDS];	/* tmp vars for node calcs */

	/* calculate number of nodes in width and height directions. */
	/* based on user entered total number of nodes and window aspect ratio */
	n_width = (int)(sqrt(num_wnodes * bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width / bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height) + 0.5);
	bldg_ptr->zone[iz]->surf[is]->wndo[iw]->n_width = n_width;
	n_height = (int)((num_wnodes / n_width) + 0.5);
	/* assure that n_width * n_height < MAX_WNDO_NODES */
	while ((n_width * n_height) > MAX_WNDO_NODES)
		// Decrement n_height (i.e., favor more nodes along width of surface).
		n_height--;
	bldg_ptr->zone[iz]->surf[is]->wndo[iw]->n_height = n_height;

	/* calculate nodal area for each window */
	bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node_area = (bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width * bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height) / (n_width * n_height);

	/* Calculate coordinates of each node on window */

	/* Calculate node edge lengths (signed) for window. */
	/* Window vertices (numbered counter-clockwise starting at */
	/* upper left (vert[icoord][0]) viewed from OUTSIDE of room). */
	/* Note that v1 is therefore input origin. */
	for (icoord=0; icoord<NCOORDS; icoord++) {
		v0[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][0];
		v1[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][1];
		v2[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][2];
		/* signed length of width edge along each axis */
		/* equals the width of the window along each axis divided by */
		/* the number of nodes along the width */
		edge_width[icoord] = (v2[icoord]-v1[icoord]) / n_width;
		/* signed length of height edge along each axis */
		/* equals the height of the window along each axis divided by */
		/* the number of nodes along the height */
		edge_height[icoord] = (v0[icoord]-v1[icoord]) / n_height;
	}

//*pofdmpfile << " Window edge_width[0] = " << edge_width[0] << " edge_width[1] = " << edge_width[1] << " edge_width[2] = " << edge_width[2] << "\n";
//*pofdmpfile << " Window edge_height[0] = " << edge_height[0] << " edge_height[1] = " << edge_height[1] << " edge_height[2] = " << edge_height[2] << "\n";

	/* Calc coordinates of nodal starting point, positioned near DOE2 origin */
	/* at 1/2 node height "below" edge of window defined by v1 and v2 */
	for (icoord=0; icoord<NCOORDS; icoord++) {
		row_strt[icoord] = v1[icoord] + edge_width[icoord] * 0.5 - edge_height[icoord] * 0.5;
	}
	inode = 0;
	/* calc coordinates of nodes on window */
	for (iheight=0; iheight<n_height; iheight++) {
		/* calc first node in row */
		for (icoord=0; icoord<NCOORDS; icoord++) {
			/* move from first node of previous row to first node of this row */
			row_strt[icoord] += edge_height[icoord];
			bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node[inode][icoord] = row_strt[icoord];
			/* init next_node for subsequent incrementing */
			next_node[icoord] = row_strt[icoord];
		}
		inode += 1;
		for (iwidth=1; iwidth<n_width; iwidth++) {
			/* calc next node along current row */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				next_node[icoord] += edge_width[icoord];
				bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node[inode][icoord] = next_node[icoord];
			}
			inode += 1;
		}
	}

	/* store number of nodes on this window */
	bldg_ptr->zone[iz]->surf[is]->wndo[iw]->nnodes = inode;

	return(0);
}

/*************************** subroutine cutout_chk **************************/
/* Checks to see if node falls within a cutout region of a surface. */
/* Returns 0 if node does not fall within any cutout regions. */
/* Returns 1 if node falls within a cutout region. */
/* Based on Superlite conventions. */
/* Radiosity modification */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/*************************** subroutine cutout_chk **************************/
int cutout_chk(
	double test_node[NCOORDS],	/* node coordinates */
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	int iz,						/* current zone index */
	int is)						/* current surface index */
{
	int iw;	/* window loop index */
	int icoord;	/* coordinate loop index */
	double xjp, yjp, xyp, etaj, xsij;	/* tmp calc vars */

	/* loop through all cutouts (windows) in current surface */
	for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {
		xjp = 0.;
		yjp = 0.;
		for (icoord=0; icoord<NCOORDS; icoord++) {
			xyp = test_node[icoord] - bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][2];
			// rjh 8/2/00
			// Change sign to account for difference in Slite and DElight coordinate systems.
			if (icoord == 1) xyp = -(xyp);
			xjp += xyp * bldg_ptr->zone[iz]->surf[is]->dircos[icoord];
			yjp += xyp * bldg_ptr->zone[iz]->surf[is]->dircos[icoord+3];
		}
		etaj = yjp / bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height;
		/* does the node fall outside of the height range of this window? */
		if (fabs(etaj - 0.5) > 0.5) continue;
		xsij = xjp / bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width;
		/* does the node fall outside of the width range of this window? */
		if (fabs(xsij - 0.5) > 0.5) continue;
		/* the node must fall within the window */
		return(1);
	}

	return(0);
}

/*************************** subroutine cutout_chk_new **************************/
/* Checks to see if node falls within a cutout region of a surface. */
/* Returns 0 if node does not fall within any cutout regions. */
/* Returns 1 if node falls within a cutout region. */
/* Based on Superlite conventions. */
// RJH - 10-16-03 - Modified for new dircos calcs
/* Radiosity modification */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/*************************** subroutine cutout_chk_new **************************/
int cutout_chk_new(
	double test_node[NCOORDS],	/* node coordinates */
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	int iz,						/* current zone index */
	int is)						/* current surface index */
{
	int iw;	/* window loop index */
	int icoord;	/* coordinate loop index */
	double xjp, yjp, xyp, etaj, xsij;	/* tmp calc vars */

	/* loop through all cutouts (windows) in current surface */
	for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {
		xjp = 0.;
		yjp = 0.;
		for (icoord=0; icoord<NCOORDS; icoord++) {
			xyp = test_node[icoord] - bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][2];
			xjp += xyp * bldg_ptr->zone[iz]->surf[is]->dircos[icoord];
			yjp += xyp * bldg_ptr->zone[iz]->surf[is]->dircos[icoord+3];
		}
		etaj = yjp / bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height;
		/* does the node fall outside of the height range of this window? */
		if (fabs(etaj - 0.5) > 0.5) continue;
		xsij = xjp / bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width;
		/* does the node fall outside of the width range of this window? */
		if (fabs(xsij - 0.5) > 0.5) continue;
		/* the node must fall within the window */
		return(1);
	}

	return(0);
}
