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

int geometrans(
	BLDG *bldg_ptr,			/* bldg structure pointer */
	int num_nodes,			/* total number of nodes on surface */
	int num_wnodes,			/* total number of nodes on window */
	ofstream* pofdmpfile);	/* ptr to LBLDLL error dump file */

int rectan(
	double height,	/* rectangle height */
	double width,	/* rectangle width */
	double rectangle[NCOORDS][NVERTS]);	/* rectangle vertices[coordinate][vertex] */

int transl(
	double xtrans,	/* sub-surface translation on the x axis */
	double ytrans,	/* sub-surface translation on the y axis */
	double subsurf[NCOORDS][NVERTS]);	/* sub-surface vertices[coordinate][vertex] */

int walloc(
	double vert[NCOORDS][NVERTS],/* vertices to be located */
	double surf_origin[NCOORDS],	/* host surface origin coords */
	double azm,	/* host surface azimuth */
	double tilt);	/* host surface tilt */

int zonloc(
	double vert[NCOORDS][NVERTS],/* vertices to be transformed */
	double zone_origin[NCOORDS],	/* zone origin coords */
	double azm);					/* zone azimuth */

int refptloc(
	REFPT *ref_pt,	/* ref_pt pointer */
	double zone_origin[NCOORDS],	/* zone origin coords */
	double azm_zone);				/* zone azimuth */

int apol(
	double vert[NCOORDS][NVERTS],/* surface vertices in bldg coord system */
	double *apazm_ptr,	/* surface azimuth_ptr with respect to bldg coord system */
	double *aptilt_ptr);	/* surface tilt_ptr with respect to bldg coord system */

int dcross(
	double vecta[NCOORDS],	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectb[NCOORDS],	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectc[NCOORDS]);	/* return vector coordinates (X=x, Y=y, Z=z) */

double ddot(
	double vecta[NCOORDS],	/* vector coordinates (X=x, Y=y, Z=z) */
	double vectb[NCOORDS]);	/* vector coordinates (X=x, Y=y, Z=z) */

int dpierc(
	int *ipierc_ptr,	/* return value (0=no intersect, 1=front, -1=back) */
	double v1[NCOORDS],	/* bldg-shade vertex coordinates (X=x, Y=y, Z=z) */
	double v2[NCOORDS],	/* bldg-shade vertex coordinates (X=x, Y=y, Z=z) */
	double v3[NCOORDS],	/* bldg-shade vertex coordinates (X=x, Y=y, Z=z) */
	double r1[NCOORDS],	/* point coordinates (X=x, Y=y, Z=z) */
	double rn[NCOORDS]);	/* unit vector coordinates (X=x, Y=y, Z=z) */

int dthlim(
	double *thmin_ptr,	/* return value of lower limit of integration */
	double *thmax_ptr,	/* return value of upper limit of integration */
	double phsky,	/* altitude of sky elements (radians) */
	double thsur,	/* azimuth of receiving surface normal (radians) */
	double phsur);	/* altitude of receiving surface normal (radians) */

int dhitsh(
	HIT *hit_ptr,		/* pointer to bldg-shade hit structure */
	double r1[NCOORDS],	/* origin of ray rn */
	double rn[NCOORDS],	/* ray */
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	int izone,			/* index of current zone */
	int iWndoSurf,		/* index of current surface containing the Window */
	int iNodeSurf);		/* index of current surface containing the Node, if applicable */

int zshade_calc_verts(
	BLDG *bldg_ptr,	/* bldg structure pointer */
	int iz,			/* zone index */
	int is,			/* surface index */
	int iw,			/* window index */
	int izs,		/* zshade index */
	int lzs);		/* zshade location (0=overhang, 1=right fin, 2=left fin). */

