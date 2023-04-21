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
#include "struct.h"
#include "GeomMesh.h"

/****************************** subroutine geometrans *****************************/
/* Translates user oriented bldg geometry to bldg (i.e., global) coord system. */
/* The bldg structure must be fully initialized prior to this call (except for zone shades). */
/* This routine transforms all bldg surfaces (walls, windows, bshades) in one call. */
/* Zone shades (i.e., overhangs and fins) are created here from window and host surface data. */
/* Converted and modified from DOE2.1D FORTRAN code in GEOPR1(). */

/* Modifications included to support Superlite 3.0 radiosity algorithms. */
/****************************** subroutine geometrans *****************************/
int geometrans(
	BLDG *bldg_ptr,			/* bldg structure pointer */
	int num_nodes,			/* total number of nodes on surface */
	int num_wnodes,			/* total number of nodes on window */
	ofstream* pofdmpfile)	/* ptr to LBLDLL error dump file */
{
	int iz, is, iw, izs, ish, irp, lzs;/* indexes */
	double height, width, azm, tilt, azm_zone, xtrans, ytrans;	/* parameter holders */

	/* Zone loop */
	for (iz=0; iz<bldg_ptr->nzones; iz++) {

		/* set azm_zone for future use */
		azm_zone = bldg_ptr->zone[iz]->azm;

		/* Surface loop */
		for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {

			/* radiosity modification (move Window loop to allow subsequent */
			/* identification of surface nodes within window cutout) */
			/* Window loop */
			for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {

				/* calculate derived quantities for window */

				/* locate all window vertices in surface coord system */
				height = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height;
				width = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width;
				rectan(height,width,bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert);
				xtrans = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->origin[X];
				ytrans = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->origin[Y];
				transl(xtrans,ytrans,bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert);

				/* locate all window vertices in zone coord system */
				azm = bldg_ptr->zone[iz]->surf[is]->azm_zs;
				tilt = bldg_ptr->zone[iz]->surf[is]->tilt_zs;
				walloc(bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert,bldg_ptr->zone[iz]->surf[is]->origin,azm,tilt);

				/* locate all window vertices in bldg coord system */
				zonloc(bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert,bldg_ptr->zone[iz]->origin,azm_zone);

				/* radiosity modification */
				/* calculate window nodal area and coordinates in bldg coord sys */
				wndo_nodal_calcs(bldg_ptr,num_wnodes,iz,is,iw);

                /* Create zone shades for shaded windows. */
				for (lzs=0; lzs<NZSHADES; lzs++) {
					/* Does this window have zone shades (overhang/fins)? */
					if (bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_x[lzs] > 0.0) {
						/* Check MAX zone shades. */
						if (bldg_ptr->zone[iz]->nzshades < MAX_ZONE_SHADES) {
							/* Create new zshade. */
							izs = bldg_ptr->zone[iz]->nzshades;
							bldg_ptr->zone[iz]->zshade[izs] = new ZSHADE;
							if (bldg_ptr->zone[iz]->zshade[izs] == NULL) {
//							if ((bldg_ptr->zone[iz]->zshade[izs] = (ZSHADE *)malloc(sizeof(ZSHADE))) == NULL) {
								*pofdmpfile << "ERROR: DElight Insufficient memory for ZONE SHADE allocation.\n";
								return(-1);
							}
							/* Initialize new zshade. */
							struct_init("ZSHADE",(char *)bldg_ptr->zone[iz]->zshade[izs]);
							/* Calc zshade vertices. */
							zshade_calc_verts(bldg_ptr,iz,is,iw,izs,lzs);
							/* Increment number of zone shades. */
							bldg_ptr->zone[iz]->nzshades++;
						}
						else {
							*pofdmpfile << "ERROR: DElight Maximum number of zone shades has been exceeded!\n";
							return (-1);
						}
					}
				}
			}

			/* calculate derived quantities for surface */

			/* locate all surface vertices in surface coord system */
			height = bldg_ptr->zone[iz]->surf[is]->height;
			width = bldg_ptr->zone[iz]->surf[is]->width;
			rectan(height,width,bldg_ptr->zone[iz]->surf[is]->vert);

			/* locate all surface vertices in zone coord system */
			azm = bldg_ptr->zone[iz]->surf[is]->azm_zs;
			tilt = bldg_ptr->zone[iz]->surf[is]->tilt_zs;
			walloc(bldg_ptr->zone[iz]->surf[is]->vert,bldg_ptr->zone[iz]->surf[is]->origin,azm,tilt);

			/* locate all surface vertices in bldg coord system */
			zonloc(bldg_ptr->zone[iz]->surf[is]->vert,bldg_ptr->zone[iz]->origin,azm_zone);

			// Store calculated bldg coord sys verts in the WNDO vPt3VerticesWCS_OCCW list
			BGL::point3			p3TmpPt;
			for (int iVert=0; iVert<NVERTS; iVert++) {
				// Store the vertex coords as a BGL point3
				p3TmpPt = BGL::point3(bldg_ptr->zone[iz]->surf[is]->vert[0][iVert], bldg_ptr->zone[iz]->surf[is]->vert[1][iVert], bldg_ptr->zone[iz]->surf[is]->vert[2][iVert]);
			    // Store the BGL point3 in the SURF vPt3VerticesWCS_OCCW list
                // This Vertex list begins at outside "upper-left" and is in OUTSIDE-CounterClockWise order
                bldg_ptr->zone[iz]->surf[is]->vPt3VerticesWCS_OCCW.push_back(p3TmpPt);
			}

			/* calculate surface azimuth and tilt in bldg coord system */
			apol(bldg_ptr->zone[iz]->surf[is]->vert,&bldg_ptr->zone[iz]->surf[is]->azm_bs,&bldg_ptr->zone[iz]->surf[is]->tilt_bs);

			/* radiosity modification */
			/* calculate surface direction cosine values (slite) in bldg coord sys */
//			dircos_calc(bldg_ptr,iz,is, pofdmpfile);
			// RJH 10-16-03 Replace with new version that uses DOE2 WCS, but Superlite conventions
			dircos_calc_new(bldg_ptr,iz,is);

			/* radiosity modification */
			/* calculate nodal area and coordinates in bldg coord sys */
			nodal_calcs(bldg_ptr,num_nodes,iz,is);

			/* radiosity modification */
			/* Calc outward (DOE2) and inward (Slite) unit vector surface normals in DOE2 BCS */
			/* Surface vertices in DOE2 convention order. */
			int icoord;	// loop index
			double dist10, dist12;	// distances between vertices
			double svert0[NCOORDS],  svert1[NCOORDS],  svert2[NCOORDS];	// vertex coords
			double svect10[NCOORDS],  svect12[NCOORDS];	// vectors

			for (icoord=0; icoord<NCOORDS; icoord++) {
				svert0[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][0];
				svert1[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][1];
				svert2[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][2];
			}
			/* Unit vectors from surface vertex 1 to 0 and 1 to 2. */
			dist10 = 0.;
			dist12 = 0.;
			for (icoord=0; icoord<NCOORDS; icoord++) {
				svect10[icoord] = svert0[icoord] - svert1[icoord];
				svect12[icoord] = svert2[icoord] - svert1[icoord];
				dist10 += svect10[icoord] * svect10[icoord];
				dist12 += svect12[icoord] * svect12[icoord];
			}
			dist10 = sqrt(dist10);
			dist12 = sqrt(dist12);
			for (icoord=0; icoord<NCOORDS; icoord++) {
				svect10[icoord] /= dist10;
				svect12[icoord] /= dist12;
			}
			/* Outward facing unit vector normal to surface (i.e., DOE2 outward normal) */
			dcross(svect12,svect10,bldg_ptr->zone[iz]->surf[is]->outward_uvect);
			/* Inward facing unit vector normal to surface (i.e., Slite inward normal) */
			dcross(svect10,svect12,bldg_ptr->zone[iz]->surf[is]->inward_uvect);
		}

		/* Reference Point loop */
		for (irp=0; irp<bldg_ptr->zone[iz]->nrefpts; irp++) {
			/* locate reference point in bldg coord system */
			refptloc(bldg_ptr->zone[iz]->ref_pt[irp],bldg_ptr->zone[iz]->origin,azm_zone);
		}
	}

	/* Building Shades loop */
	for (ish=0; ish<bldg_ptr->nbshades; ish++) {

		/* calculate derived quantities for building shade */

		/* locate all building shade vertices in shade-surface coord system */
		height = bldg_ptr->bshade[ish]->height;
		width = bldg_ptr->bshade[ish]->width;
		rectan(height,width,bldg_ptr->bshade[ish]->vert);

		/* locate all building shade vertices in bldg coord system */
		azm = bldg_ptr->bshade[ish]->azm;
		tilt = bldg_ptr->bshade[ish]->tilt;
		walloc(bldg_ptr->bshade[ish]->vert,bldg_ptr->bshade[ish]->origin,azm,tilt);
	}

	return(0);
}

/****************************** subroutine rectan *****************************/
/* Locates the vertices of a rectangular surface in the surface coordinate system. */
/* Uses height and width of rectangle and origin coordinates. */
/* Vertices are numbered counterclockwise from upper left viewed from outside. */
/* The second vertex (index 1) is therefore the origin. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine rectan *****************************/
int rectan(
	double height,	/* rectangle height */
	double width,	/* rectangle width */
	double rectangle[NCOORDS][NVERTS])	/* rectangle vertices[coordinate][vertex] */
{
	rectangle[X][0] = 0.;		/* x */ /* upper left vertex */
	rectangle[Y][0] = height;	/* y */
	rectangle[Z][0] = 0.;		/* z */
	rectangle[X][1] = 0.;		/* x */ /* lower left vertex (origin) */
	rectangle[Y][1] = 0.;		/* y */
	rectangle[Z][1] = 0.;		/* z */
	rectangle[X][2] = width;	/* x */ /* lower right vertex */
	rectangle[Y][2] = 0.;		/* y */
	rectangle[Z][2] = 0.;		/* z */
	rectangle[X][3] = width;	/* x */ /* upper right vertex */
	rectangle[Y][3] = height;	/* y */
	rectangle[Z][3] = 0.;		/* z */

	return(0);
}

/****************************** subroutine transl *****************************/
/* Locates the vertices of sub-surfaces in the surface coordinate system. */
/* Uses vertices established in rectan() and origin x and y coords of sub-surf. */
/* Vertices are numbered counterclockwise from upper left viewed from outside. */
/* The second vertex (index 1) is therefore the sub-surface origin. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine transl *****************************/
int transl(
	double xtrans,	/* sub-surface translation on the x axis */
	double ytrans,	/* sub-surface translation on the y axis */
	double subsurf[NCOORDS][NVERTS])	/* sub-surface vertices[coordinate][vertex] */
{
	int ivert;	/* vertex index */

	for (ivert=0; ivert<NVERTS; ivert++) {
		subsurf[X][ivert] += xtrans;
		subsurf[Y][ivert] += ytrans;
	}

	return(0);
}

/****************************** subroutine walloc *****************************/
/* Locates the vertices of walls and sub-surfaces in the zone coord system. */
/* Locates the vertices of building shades in the bldg coord system. */
/* Uses vertices established in transl(), and host surf origin, tilt and azimuth. */
/* Vertices are numbered counterclockwise from upper left viewed from outside. */
/* The second vertex (index 1) is therefore the surface origin. */
/* NOTE: when AZM=TILT=surf_origin[*]=0, vert[x]=-surf[x] and vert[y]=-surf[y]. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine walloc *****************************/
int walloc(
	double vert[NCOORDS][NVERTS],/* vertices to be located */
	double surf_origin[NCOORDS],	/* host surface origin coords */
	double azm,	/* host surface azimuth */
	double tilt)	/* host surface tilt */
{
	int ivert;	/* vertex index */
	double oldx, oldy;	/* incoming x and y coords of each vertex */
	double azm_rad, tilt_rad;	/* azm and tilt in radians */
	double cosazm, sinazm, costilt, sintilt;

	azm_rad = azm * DTOR;
	tilt_rad = tilt * DTOR;
	cosazm = cos(azm_rad);
	sinazm = sin(azm_rad);
	costilt = cos(tilt_rad);
	sintilt = sin(tilt_rad);

	for (ivert=0; ivert<NVERTS; ivert++) {

		oldx = vert[X][ivert];
		oldy = vert[Y][ivert];

		vert[X][ivert] = surf_origin[X] - oldx * cosazm - oldy * sinazm * costilt;
		vert[Y][ivert] = surf_origin[Y] + oldx * sinazm - oldy * cosazm * costilt;
		vert[Z][ivert] = surf_origin[Z] + oldy * sintilt;
	}

	return(0);
}

/****************************** subroutine zonloc *****************************/
/* Transform the vertices of walls and sub-surfaces to the bldg coord system. */
/* Uses vertices established in walloc(), and zone origin and azimuth. */
/* Vertices are numbered counterclockwise from upper left viewed from outside. */
/* The second vertex (index 1) is therefore the surface origin. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine zonloc *****************************/
int zonloc(
	double vert[NCOORDS][NVERTS],/* vertices to be transformed */
	double zone_origin[NCOORDS],	/* zone origin coords */
	double azm)					/* zone azimuth */
{
	int ivert;			/* vertex index */
	double oldx, oldy;	/* incoming x and y coords of each vertex */
	double azm_rad;		/* azm in radians */
	double cosazm, sinazm;

	azm_rad = azm * DTOR;
	cosazm = cos(azm_rad);
	sinazm = sin(azm_rad);

	for (ivert=0; ivert<NVERTS; ivert++) {

		oldx = vert[X][ivert];
		oldy = vert[Y][ivert];

		vert[X][ivert] = zone_origin[X] + oldx * cosazm + oldy * sinazm;
		vert[Y][ivert] = zone_origin[Y] - oldx * sinazm + oldy * cosazm;
		vert[Z][ivert] += zone_origin[Z];
	}

	return(0);
}

/****************************** subroutine refptloc *****************************/
/* Transform the zone system coords of a reference point into the bldg coord system. */
/* Uses zone sys coords, zone origin and zone azimuth. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine refptloc *****************************/
int refptloc(
	REFPT *ref_pt,	/* ref_pt pointer */
	double zone_origin[NCOORDS],	/* zone origin coords */
	double azm_zone)				/* zone azimuth */
{
	double azm_rad;		/* zone azm in radians */
	double cosazm, sinazm;

	azm_rad = azm_zone * DTOR;
	cosazm = cos(azm_rad);
	sinazm = sin(azm_rad);

	ref_pt->bs[X] = zone_origin[X] + ref_pt->zs[X] * cosazm + ref_pt->zs[Y] * sinazm;
	ref_pt->bs[Y] = zone_origin[Y] - ref_pt->zs[X] * sinazm + ref_pt->zs[Y] * cosazm;
	ref_pt->bs[Z] = zone_origin[Z] + ref_pt->zs[Z];

	return(0);
}

/****************************** subroutine apol *****************************/
/* Calculates surface azimuth and tilt of surface in the bldg coord system. */
/* Uses vertices established in zonloc(). */
/* NOTE: aparea calculation in DOE2.1D code has been removed from this routine. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine apol *****************************/
int apol(
	double vert[NCOORDS][NVERTS],/* surface vertices in bldg coord system */
	double *apazm_ptr,	/* surface azimuth_ptr with respect to bldg coord system */
	double *aptilt_ptr)	/* surface tilt_ptr with respect to bldg coord system */
{
	int ivert;						/* vertex index */
	double xcomp, ycomp, zcomp;		/* local intermediate vars */
	double x0, y0, z0, x1, y1, z1;	/* local intermediate vars */
	double azm, tilt, proj, area;	/* local intermediate vars */

	xcomp = 0.;
	ycomp = 0.;
	zcomp = 0.;
	x0 = vert[X][NVERTS-1];
	y0 = vert[Y][NVERTS-1];
	z0 = vert[Z][NVERTS-1];

	for (ivert=0; ivert<NVERTS; ivert++) {
		x1 = vert[X][ivert];
		y1 = vert[Y][ivert];
		z1 = vert[Z][ivert];
		xcomp += y0 * z1 - y1 * z0;
		ycomp += z0 * x1 - z1 * x0;
		zcomp += x0 * y1 - x1 * y0;
		x0 = x1;
		y0 = y1;
		z0 = z1;
	}

	area = sqrt(xcomp*xcomp+ycomp*ycomp+zcomp*zcomp)/2.;

	/* NOTE: apazm and aptilt are inited to 0.0 in struct_init call */
	if (area == 0.0) return(0);

	tilt = acos(zcomp / (2.0 * area));
	proj = sqrt(xcomp*xcomp + ycomp*ycomp);

	if ((proj - (0.0001 * area)) > 0.0) {
		if (xcomp < 0.0) {
			if (ycomp < 0.0) {
				azm = 3.1416 + asin(-xcomp / proj);
			}
			else {
				azm = 4.7124 + asin(ycomp / proj);
			}
		}
		else {
			if (ycomp < 0.0) {
				azm = 1.5708 + asin(-ycomp / proj);
			}
			else {
				azm = asin(xcomp / proj);
			}
		}
	}
	else {
		azm = 0.0;
	}
	*apazm_ptr = azm / DTOR;
	*aptilt_ptr = tilt / DTOR;

	return(0);
}

/****************************** subroutine dcross *****************************/
/* Calculates cross product between vectors vecta and vectb. */
/* This (vectc) is the vector normal for surfaces for which vecta and vectb */
/* have been calculated from DOE2 BCS surface vertices 0, 1 (origin), and 2. */
/* If vecta is from vertex 1 to 2 (along width edge), and vectb is from 1 to 0 (along height edge), */
/*		then vectc is DOE2 outward normal. */
/* If vecta is from vertex 1 to 0, and vectb is from 1 to 2, */
/*		then vectc is Superlite inward normal. */
/* If vecta and vectb are unit vectors, then (vectc) is a unit vector normal. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dcross *****************************/
int dcross(
	double vecta[NCOORDS],	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectb[NCOORDS],	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectc[NCOORDS])	/* return vector coordinates (X=x, Y=y, Z=z) */
{
	vectc[X] = vecta[Y] * vectb[Z] - vecta[Z] * vectb[Y];
	vectc[Y] = vecta[Z] * vectb[X] - vecta[X] * vectb[Z];
	vectc[Z] = vecta[X] * vectb[Y] - vecta[Y] * vectb[X];

	return(0);
}

/****************************** function ddot *****************************/
/* Calculates dot product of vectors vecta and vectb. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** function ddot *****************************/
double ddot(
	double vecta[NCOORDS],	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectb[NCOORDS])	/* vector coordinates (X=x, Y=y, Z=z) */
{
	double ddotval;	/* returned value */

	ddotval = vecta[X] * vectb[X] + vecta[Y] * vectb[Y] + vecta[Z] * vectb[Z];

	return(ddotval);
}

/****************************** subroutine dpierc *****************************/
/* Called by dhitsh() */
/* Returns 0 if the line thru point r1 in direction of unit vector rn does not */
/* intersect the building shade rectangle defined by vertices v1, v2 and v3, */
/* with normal (v3-v2)*(v1-v2). */
/* Returns 1 (-1) if front (back) of the rectangle is intersected. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dpierc *****************************/
int dpierc(
	int *ipierc_ptr,	/* return value (0=no intersect, 1=front, -1=back) */
	double v1[NCOORDS],	/* bldg-shade vertex coordinates (X=x, Y=y, Z=z) */
	double v2[NCOORDS],	/* bldg-shade vertex coordinates (X=x, Y=y, Z=z) */
	double v3[NCOORDS],	/* bldg-shade vertex coordinates (X=x, Y=y, Z=z) */
	double r1[NCOORDS],	/* point coordinates (X=x, Y=y, Z=z) */
	double rn[NCOORDS])	/* unit vector coordinates (X=x, Y=y, Z=z) */
{
	int icoord;		/* coordinate index */
	double vecta[NCOORDS];	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectb[NCOORDS];	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectc[NCOORDS];	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectba[NCOORDS];	/* vector coordinates (X=x, Y=y, Z=z) */
	double f1, f2;	/* scale factors */
	double scale;
	double dotcb, dotca;	/* ddot() return values */

	*ipierc_ptr = 0;

	/* vectors from v2 to v1 and v2 to v3 */
	for (icoord=X; icoord<NCOORDS; icoord++) {
		vecta[icoord] = v1[icoord] - v2[icoord];
		vectb[icoord] = v3[icoord] - v2[icoord];
	}

	/* vector normal to rectangle */
	dcross(vectb, vecta, vectba);

	/* scale factor */
	f1 = 0.;
	f2 = 0.;
	for (icoord=X; icoord<NCOORDS; icoord++) {
		f1 += vectba[icoord] * (v2[icoord] - r1[icoord]);
		f2 += vectba[icoord] * rn[icoord];
	}
	if (f2 == 0.0) return(0);
	scale = f1 / f2;
	if (scale <= 0.0) return(0);

	/* vector-c from v2 to point that ray along rn intersects plane of rectangle */
	for (icoord=X; icoord<NCOORDS; icoord++)
		vectc[icoord] = r1[icoord] + rn[icoord] * scale - v2[icoord];

	/* intersection point, c, inside rectangle tests */
	dotcb = ddot(vectc, vectb);
	if (dotcb < 0.0) return(0);
	if (dotcb > ddot(vectb,vectb)) return(0);
	dotca = ddot(vectc,vecta);
	if (dotca < 0.0) return(0);
	if (dotca > ddot(vecta,vecta)) return(0);
	*ipierc_ptr = 1;
	if (ddot(rn,vectba) > 0.0) *ipierc_ptr = -1;

	return(0);

}

/****************************** subroutine dthlim *****************************/
/* Determines limits of integration of sky azimuth angle for a surface */
/* receiving light from sky elements of altitude phsky. */
/* The normal to the receiving surface has azimuth thsur and altitude phsur. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dthlim *****************************/
int dthlim(
	double *thmin_ptr,	/* return value of lower limit of integration */
	double *thmax_ptr,	/* return value of upper limit of integration */
	double phsky,	/* altitude of sky elements (radians) */
	double thsur,	/* azimuth of receiving surface normal (radians) */
	double phsur)	/* altitude of receiving surface normal (radians) */
{
	double tltsur;
	double avar;

	if (fabs(phsur) >= 0.035) {
		tltsur = PIOVR2 - phsur;
		if ((phsky > tltsur) || (fabs(phsky) > (PI - tltsur))) {
			*thmin_ptr = -PI;
			*thmax_ptr = PI;
			return(0);
		}
		avar = -tan(phsky) / tan(tltsur);
		avar = fabs(acos(avar));
		*thmin_ptr = thsur - avar;
		*thmax_ptr = thsur + avar;
		return(0);
	}
	else {	/* surface is within 2 degrees of vertical */
		*thmin_ptr = thsur - PIOVR2;
		*thmax_ptr = thsur + PIOVR2;
		return(0);
	}
}

/****************************** function dhitsh *****************************/
/* Determines if a ray from r1 in the direction of rn intersects a shading surface. */
/* Returns hit flag, ihit: 0=no hit, 1=zone-shade is hit, */
/* 2=zone surface is hit on the exterior (the side with luminance), */
/* 3=building-shade is hit from behind, 4=building-shade is hit from the front. */
/* If 2 then returns index value of zone surface that was hit. */
/* If 3 or 4, then returns index value of building-shade that was hit. */
/* Current implementation deals with zone-shades, self shading caused by */
/* exterior of zone surfaces other than current surface, and building-shades. */
/* Note that the order of shade intersection detection is first zone-shades, */
/* next zone surface (self) shading and last building-shades. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** function dhitsh *****************************/
int dhitsh(
	HIT *hit_ptr,		/* pointer to bldg-shade hit structure */
	double r1[NCOORDS],	/* origin of ray rn */
	double rn[NCOORDS],	/* ray */
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	int izone,			/* index of current zone */
	int iWndoSurf,		/* index of current surface containing the Window */
	int iNodeSurf)		/* index of current surface containing the Node, if applicable */
							// Note: use Window Surface for both iWndoSurf and iNodeSurf if dhitsh() call is not for a surface node.
{
	int iz, izs, is, ish, icoord;		/* indexes */
	double v1[NCOORDS], v2[NCOORDS], v3[NCOORDS];
	int ipierc;		/* return value from dpierc() */

	hit_ptr->ihit = 0;
	hit_ptr->hitshade = 0;
	hit_ptr->hitzone = 0;

	/* loop over zone-shades in all zones */
	for (iz=0; iz<bldg_ptr->nzones; iz++) {
		for (izs=0; izs<bldg_ptr->zone[iz]->nzshades; izs++) {
			/* get zone shade vertices */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				v1[icoord] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][0];
				v2[icoord] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][1];
				v3[icoord] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][2];
			}
			/* dpierc() return value (0=no intersect, 1=front, -1=back) */
			dpierc(&ipierc,v1,v2,v3,r1,rn);
			/* if current shade is not hit then loop to next shade */
			if (ipierc == 0) continue;
			/* if current shade is hit return appropriate value */
			hit_ptr->hitshade = izs;
			hit_ptr->ihit = 1;
			return(0);
		}
	}

	/* loop over all zone surfaces (except current surface) in all zones */
	for (iz=0; iz<bldg_ptr->nzones; iz++) {
		for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {
			/* skip current surface containing window */
			if ((iz == izone) && (is == iWndoSurf)) continue;
			/* skip current surface containing node, if applicable */
			if ((iz == izone) && (is == iNodeSurf)) continue;
			/* get surface vertices */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				v1[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][0];
				v2[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][1];
				v3[icoord] = bldg_ptr->zone[iz]->surf[is]->vert[icoord][2];
			}
			/* dpierc() return value (0=no intersect, 1=front, -1=back) */
			dpierc(&ipierc,v1,v2,v3,r1,rn);
			/* if front (exterior) of current surface is hit return appropriate value */
			if (ipierc == 1) {
				hit_ptr->hitzone = iz;
				hit_ptr->hitshade = is;
				hit_ptr->ihit = 2;
				return(0);
			}
		}
	}

	/* loop over building-shades */
	for (ish=0; ish<bldg_ptr->nbshades; ish++) {
		/* get shade vertices */
		for (icoord=0; icoord<NCOORDS; icoord++) {
			v1[icoord] = bldg_ptr->bshade[ish]->vert[icoord][0];
			v2[icoord] = bldg_ptr->bshade[ish]->vert[icoord][1];
			v3[icoord] = bldg_ptr->bshade[ish]->vert[icoord][2];
		}
		/* dpierc() return value (0=no intersect, 1=front, -1=back) */
		dpierc(&ipierc,v1,v2,v3,r1,rn);
		/* if current shade is not hit then loop to next shade */
		if (ipierc == 0) continue;
		/* if current shade is hit return appropriate values */
		hit_ptr->hitshade = ish;
		if (ipierc == -1) hit_ptr->ihit = 3;
		if (ipierc == 1) hit_ptr->ihit = 4;
		return(0);
	}

	return(0);
}

/****************************** subroutine zshade_calc_verts *****************************/
/* Determines all vertices of zone shades in bldg coord system (BCS) */
/* based on zshade location as viewed from inside looking outward (0=overhang, 1=right fin, 2=left fin). */
/* Vertices are numbered counterclockwise from upper left viewed back along the */
/* zone shade outward normal (i.e., from side facing wndo). */
/* The second vertex (index 1) is therefore the zshade origin. */
/* Note that vertices are oriented from outside looking in and shades are oriented from inside looking out. */
/* 1/98 Modifications: */
/*	- Set zone shade vertices located on window host surface using window vertices in BCS. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine zshade_calc_verts *****************************/
int zshade_calc_verts(
	BLDG *bldg_ptr,	/* bldg structure pointer */
	int iz,			/* zone index */
	int is,			/* surface index */
	int iw,			/* window index */
	int izs,		/* zshade index */
	int lzs)		/* zshade location (0=overhang, 1=right fin, 2=left fin). */
{
	double zshade_x, zshade_y;	/* zone shade depth and distance from wndo */
	double w0[NCOORDS], w1[NCOORDS], w2[NCOORDS], w3[NCOORDS];	/* window vertices */
	double uvect10[NCOORDS], uvect21[NCOORDS], uvect12[NCOORDS];	/* unit vectors between window vertices */
	double dist10, dist21, dist12;		/* distance between vertices */
	double wnorm[NCOORDS];		/* window outward normal vector */
	int icoord;		/* loop index */

	/* Determine zone shade vertices (bldg sys coords) located on window host surface. */

	/* Get window vertices numbered counter-clockwise starting at upper left viewed from OUTSIDE of room. */
	for (icoord=0; icoord<NCOORDS; icoord++) {
		w0[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][0];
		w1[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][1];
		w2[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][2];
		w3[icoord] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][3];
	}

	/* Calc unit vectors between wndo vertices: */
	/*   1 to 0 (i.e., "up" along host surface height), */
	/*   2 to 1 (i.e., "right to left viewed from outside" along host surface width). */
	/*   1 to 2 (i.e., "left to right viewed from outside" along host surface width). */
	dist10 = 0.;
	dist21 = 0.;
	dist12 = 0.;
	for (icoord=0; icoord<NCOORDS; icoord++) {
		uvect10[icoord] = w0[icoord] - w1[icoord];
		dist10 += uvect10[icoord] * uvect10[icoord];
		uvect21[icoord] = w1[icoord] - w2[icoord];
		dist21 += uvect21[icoord] * uvect21[icoord];
		uvect12[icoord] = w2[icoord] - w1[icoord];
		dist12 += uvect12[icoord] * uvect12[icoord];
	}
	dist10 = sqrt(dist10);
	dist21 = sqrt(dist21);
	dist12 = sqrt(dist12);
	for (icoord=0; icoord<NCOORDS; icoord++) {
		uvect10[icoord] /= dist10;
		uvect21[icoord] /= dist21;
		uvect12[icoord] /= dist12;
	}

    /* Zshade distance from wndo along host surface. */
    zshade_y = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_y[lzs];

	/* Set bldg coord system vertices based on zshade location (type). */
	switch (lzs)
	{
		case 0:	/* overhang */
			/* Calc overhang vertices located on window host surface. */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][1] = w0[icoord] + uvect10[icoord] * zshade_y;
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][2] = w3[icoord] + uvect10[icoord] * zshade_y;
			}
			break;
		case 1:	/* right fin */
			/* Calc right fin vertices located on window host surface. */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][2] = w1[icoord] + uvect21[icoord] * zshade_y;
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][3] = w0[icoord] + uvect21[icoord] * zshade_y;
			}
			break;
		case 2:	/* left fin */
			/* Calc left fin vertices located on window host surface. */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][0] = w3[icoord] + uvect12[icoord] * zshade_y;
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][1] = w2[icoord] + uvect12[icoord] * zshade_y;
			}
			break;
	}

	/* Determine zone shade vertices (bldg sys coords) not located on window host surface. */

	/* Calc unit vector normal to window (pointing away from room) */
	dcross(uvect12,uvect10,wnorm);

    /* Shorten reference to overhang depth. */
    zshade_x = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_x[lzs];

	/* Calc off-surface vertices based on zshade location. */
	switch (lzs)
	{
		case 0:	/* overhang */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][0] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][1] + wnorm[icoord] * zshade_x;
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][3] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][2] + wnorm[icoord] * zshade_x;
			}
			break;
		case 1:	/* right fin */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][0] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][3] + wnorm[icoord] * zshade_x;
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][1] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][2] + wnorm[icoord] * zshade_x;
			}
			break;
		case 2:	/* left fin */
			for (icoord=0; icoord<NCOORDS; icoord++) {
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][2] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][1] + wnorm[icoord] * zshade_x;
				bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][3] = bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][0] + wnorm[icoord] * zshade_x;
			}
			break;
	}

	return(0);
}
