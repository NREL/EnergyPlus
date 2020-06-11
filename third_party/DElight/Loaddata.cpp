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
#include <fstream>
#include <map>
#include <vector>
#include <string>
#include <cstring>
#include <stdlib.h>

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
#include "Loaddata.h"
#include "struct.h"
#include "geom.h"

/****************************** subroutine load_bldg *****************************/
/* Loads a building data file (*.in) from disk into an initialized bldg structure. */
/****************************** subroutine load_bldg *****************************/
int load_bldg(
	BLDG *bldg_ptr,		/* building structure pointer */
	FILE *infile,		/* pointer to building data file */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	char cInputLine[MAX_CHAR_LINE+1];	/* Input line */
	char *token;						/* Input token pointer */
	int icoord, im, iz, ils, is, iw, ish, irp, ihr;/* indexes */
	int izshade_x, izshade_y;/* indexes */

	/* Read and discard the second heading line (first line is read in DElight2() main */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

	/* Read site data */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %s\n",bldg_ptr->name); //,_countof(bldg_ptr->name));
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->lat);
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->lon);
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->alt);
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->azm);
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->timezone);
	/* monthly atmospheric moisture */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	/* tokenize the line label */
	token = strtok(cInputLine," ");
	for (im=0; im<MONTHS; im++) {
		token = strtok(NULL," ");
		bldg_ptr->atmmoi[im] = atof(token);
	}
	/* monthly atmospheric turbidity */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	/* tokenize the line label */
	token = strtok(cInputLine," ");
	for (im=0; im<MONTHS; im++) {
		token = strtok(NULL," ");
		bldg_ptr->atmtur[im] = atof(token);
	}

	/* Read and discard ZONES headings lines */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

	/* Read zone data */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %d\n",&bldg_ptr->nzones);
	for (iz=0; iz<bldg_ptr->nzones; iz++) {
		bldg_ptr->zone[iz] = new ZONE;
		if (bldg_ptr->zone[iz] == NULL) {
//		if ((bldg_ptr->zone[iz] = (ZONE *)malloc(sizeof(ZONE))) == NULL) {
			*pofdmpfile << "ERROR: DElight Insufficient memory for ZONE allocation\n";
			return(-1);
		}
		struct_init("ZONE",(char *)bldg_ptr->zone[iz]);

		/* Read and discard ZONE headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->name); //,_countof(bldg_ptr->zone[iz]->name));
		/* zone origin in bldg system coordinates */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		/* tokenize the line label */
		token = strtok(cInputLine," ");
		for (icoord=0; icoord<NCOORDS; icoord++) {
			token = strtok(NULL," ");
			bldg_ptr->zone[iz]->origin[icoord] = atof(token);
		}
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->azm);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->mult);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->flarea);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->volume);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->lighting);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->min_power);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->min_light);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->lt_ctrl_steps);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->lt_ctrl_prob);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->view_azm);

		/* Read and discard ZONE LIGHTING SCHEDULE headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		/* Read lighting schedule data */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->nltsch);
		for (ils=0; ils<bldg_ptr->zone[iz]->nltsch; ils++) {
			bldg_ptr->zone[iz]->ltsch[ils] = new LTSCH;
			if (bldg_ptr->zone[iz]->ltsch[ils] == NULL) {
//			if ((bldg_ptr->zone[iz]->ltsch[ils] = (LTSCH *)malloc(sizeof(LTSCH))) == NULL) {
				*pofdmpfile << "ERROR: DElight Insufficient memory for LIGHT SCHEDULE allocation\n";
				return(-1);
			}
			struct_init("LTSCH",(char *)bldg_ptr->zone[iz]->ltsch[ils]);

			/* Read and discard LIGHTING SCHEDULE DATA headings lines */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->ltsch[ils]->name); //,_countof(bldg_ptr->zone[iz]->ltsch[ils]->name));
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ltsch[ils]->mon_begin);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ltsch[ils]->day_begin);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ltsch[ils]->mon_end);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ltsch[ils]->day_end);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ltsch[ils]->dow_begin);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ltsch[ils]->dow_end);
			/* lighting schedule hourly fractions */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			/* tokenize the line label */
			token = strtok(cInputLine," ");
			for (ihr=0; ihr<HOURS; ihr++) {
				token = strtok(NULL," ");
				bldg_ptr->zone[iz]->ltsch[ils]->frac[ihr] = atof(token);
			}
		}

		/* Read and discard ZONE SURFACES headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		/* Read surface data */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->nsurfs);
		for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {
			bldg_ptr->zone[iz]->surf[is] = new SURF;
			if (bldg_ptr->zone[iz]->surf[is] == NULL) {
//			if ((bldg_ptr->zone[iz]->surf[is] = (SURF *)malloc(sizeof(SURF))) == NULL) {
				*pofdmpfile << "ERROR: DElight Insufficient memory for SURFACE allocation\n";
				return(-1);
			}
			struct_init("SURF",(char *)bldg_ptr->zone[iz]->surf[is]);

			/* Read and discard ZONE SURFACE DATA headings lines */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->surf[is]->name); //,_countof(bldg_ptr->zone[iz]->surf[is]->name));
			/* surface origin in zone system coordinates */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			/* tokenize the line label */
			token = strtok(cInputLine," ");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				token = strtok(NULL," ");
				bldg_ptr->zone[iz]->surf[is]->origin[icoord] = atof(token);
			}
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->height);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->width);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->azm_zs);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->tilt_zs);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->vis_refl);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->ext_vis_refl);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->gnd_refl);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->surf[is]->type);

			/* Read and discard SURFACE WINDOWS headings lines */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

			/* Transform surface geometry here for use in CFSSurface construction */
			// RJH 10-19-2001 - This happens again in geom.cpp to assure consistency with earlier implementation.

			/* locate all surface vertices in surface coord system */
			rectan(bldg_ptr->zone[iz]->surf[is]->height, bldg_ptr->zone[iz]->surf[is]->width, bldg_ptr->zone[iz]->surf[is]->vert);

			/* locate all surface vertices in zone coord system */
			walloc(bldg_ptr->zone[iz]->surf[is]->vert, bldg_ptr->zone[iz]->surf[is]->origin, bldg_ptr->zone[iz]->surf[is]->azm_zs, bldg_ptr->zone[iz]->surf[is]->tilt_zs);

			/* locate all surface vertices in bldg coord system */
			zonloc(bldg_ptr->zone[iz]->surf[is]->vert, bldg_ptr->zone[iz]->origin, bldg_ptr->zone[iz]->azm);

			/* calculate surface azimuth and tilt in bldg coord system */
			apol(bldg_ptr->zone[iz]->surf[is]->vert, &bldg_ptr->zone[iz]->surf[is]->azm_bs, &bldg_ptr->zone[iz]->surf[is]->tilt_bs);

			// Construct the SURF using WLC method.
			// RJH 10-19-2001 - Replaced by above.
//			Vec3d SurfaceOrigin(dOrigin[0],
//				dOrigin[1],
//				dOrigin[2]);	// offset coordinates of SURF origin from zone sys origin
//			bldg_ptr->zone[iz]->surf[is]->Init(SurfaceOrigin,
//				dHeight,
//				dWidth,
//				dAzm_zs,
//				dTilt_zs);

			/* Read window data */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->surf[is]->nwndos);
			for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {
				bldg_ptr->zone[iz]->surf[is]->wndo[iw] = new WNDO;
				if (bldg_ptr->zone[iz]->surf[is]->wndo[iw] == NULL) {
//				if ((bldg_ptr->zone[iz]->surf[is]->wndo[iw] = (WNDO *)malloc(sizeof(WNDO))) == NULL) {
					*pofdmpfile << "ERROR: DElight Insufficient memory for WINDOW allocation\n";
					return(-1);
				}
				struct_init("WNDO",(char *)bldg_ptr->zone[iz]->surf[is]->wndo[iw]);

				/* Read and discard WINDOW headings lines */
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->surf[is]->wndo[iw]->name); //,_countof(bldg_ptr->zone[iz]->surf[is]->wndo[iw]->name));
				/* window origin in surface system coordinates */
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				/* tokenize the line label */
				token = strtok(cInputLine," ");
				for (icoord=0; icoord<NCOORDS; icoord++) {
					token = strtok(NULL," ");
					bldg_ptr->zone[iz]->surf[is]->wndo[iw]->origin[icoord] = atof(token);
				}
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->surf[is]->wndo[iw]->glass_type); //,_countof(bldg_ptr->zone[iz]->surf[is]->wndo[iw]->glass_type));
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_flag);
				if (bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_flag != 0) {
					if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
					sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_type); //,_countof(bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_type));
				}
				/* window overhang/fin zone shade depth (ft) */
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				/* tokenize the line label */
				token = strtok(cInputLine," ");
				for (izshade_x=0; izshade_x<NZSHADES; izshade_x++) {
					token = strtok(NULL," ");
					bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_x[izshade_x] = atof(token);
				}
				/* window overhang/fin zone shade distance from window (ft) */
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				/* tokenize the line label */
				token = strtok(cInputLine," ");
				for (izshade_y=0; izshade_y<NZSHADES; izshade_y++) {
					token = strtok(NULL," ");
					bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_y[izshade_y] = atof(token);
				}
			}

			/* Read and discard SURFACE CFS headings lines */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

			/* Read CFS data */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->surf[is]->ncfs);
			for (int icfs=0; icfs<bldg_ptr->zone[iz]->surf[is]->ncfs; icfs++) {

				/* Read and discard CFS headings lines */
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

				/* Read and store CFS name line */
				char charCFSname[MAX_CHAR_UNAME+1];
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %s\n",charCFSname); //,_countof(charCFSname));
				/* CFS origin in surface system coordinates */
				double dCFSorigin[NCOORDS];
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				/* tokenize the line label */
				token = strtok(cInputLine," ");
				for (icoord=0; icoord<NCOORDS; icoord++) {
					token = strtok(NULL," ");
					dCFSorigin[icoord] = atof(token);
				}
				// CFS geometry
				BGL::vector3 v3CFSoffset(dCFSorigin[0], dCFSorigin[1], dCFSorigin[2]);
				double dCFSheight, dCFSwidth, dCFSrotation;
				int iCFSnodesheight, iCFSnodeswidth;
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSheight);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSwidth);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSrotation);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %d\n",&iCFSnodesheight);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %d\n",&iCFSnodeswidth);
				// CFS System
				string cCFStype;
				char charCFStype[MAX_CHAR_UNAME+1];
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %s\n",charCFStype); //,_countof(charCFStype));
				cCFStype = charCFStype;
				// CFS System parameter list
				double dCFSBFlux, dCFSConeAngle, dCFSTheta, dCFSPhi;
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSBFlux);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSConeAngle);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSTheta);
				if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
				sscanf(cInputLine,"%*s %lf\n",&dCFSPhi);

				// Create new CFS surface object and store pointer
// RJH - new constructor based on vertex list               CFSSurface(SURF *Parent, string CFStype, Double rotation, vector<BGL::point3> p3List,Double CFSmaxNodeArea);
// RJH - commented out 2-25-04 for later correction
//                bldg_ptr->zone[iz]->surf[is]->cfs[icfs] = new CFSSurface(bldg_ptr->zone[iz]->surf[is], cCFStype, dCFSrotation, v3CFSoffset, dCFSmaxNodeArea);
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->SetMesh(iCFSnodesheight, iCFSnodeswidth);
				// Set the CFS uname
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->Name(charCFSname);
				//	related CFSsyst inits
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->SetCFSSystParam("BFlux0", dCFSBFlux);		// BFlux0
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->SetCFSSystParam("ConeAngle", dCFSConeAngle);		// ConeAngle, deg
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->SetCFSSystParam("Theta0", dCFSTheta);//Theta0: Beam theta output direction, degrees from CFS normal
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->SetCFSSystParam("Phi0", dCFSPhi);	//Phi0:	Beam azimuth output direction, degrees CCW from CFS_x axis
//				bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->InitCFSSyst();
			}
		}

		/* Read and discard REFERENCE POINT headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		/* Read reference point data */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->nrefpts);
		for (irp=0; irp<bldg_ptr->zone[iz]->nrefpts; irp++) {
			bldg_ptr->zone[iz]->ref_pt[irp] = new REFPT;
			if (bldg_ptr->zone[iz]->ref_pt[irp] == NULL) {
//			if ((bldg_ptr->zone[iz]->ref_pt[irp] = (REFPT *)malloc(sizeof(REFPT))) == NULL) {
				*pofdmpfile << "ERROR: DElight Insufficient memory for REFERENCE POINT allocation\n";
				return(-1);
			}
			struct_init("REFPT",(char *)bldg_ptr->zone[iz]->ref_pt[irp]);

			/* Read and discard REFERENCE POINT DATA headings lines */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %s\n",bldg_ptr->zone[iz]->ref_pt[irp]->name); //,_countof(bldg_ptr->zone[iz]->ref_pt[irp]->name));
			/* reference point in zone system coordinates */
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			/* tokenize the line label */
			token = strtok(cInputLine," ");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				token = strtok(NULL," ");
				bldg_ptr->zone[iz]->ref_pt[irp]->zs[icoord] = atof(token);
			}
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->ref_pt[irp]->zone_frac);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->zone[iz]->ref_pt[irp]->lt_set_pt);
			if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
			sscanf(cInputLine,"%*s %d\n",&bldg_ptr->zone[iz]->ref_pt[irp]->lt_ctrl_type);
			/* Allocate memory for window luminance factors for each ref_pt<->wndo combination */
			for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {
				for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {
					bldg_ptr->zone[iz]->ref_pt[irp]->wlum[is][iw] = new WLUM;
					if (bldg_ptr->zone[iz]->ref_pt[irp]->wlum[is][iw] == NULL) {
//					if ((bldg_ptr->zone[iz]->ref_pt[irp]->wlum[is][iw] = (WLUM *)malloc(sizeof(WLUM))) == NULL) {
						*pofdmpfile << "ERROR: DElight Insufficient memory for WINDOW LUMINANCE allocation\n";
						return(-1);
					}
					struct_init("WLUM",(char *)bldg_ptr->zone[iz]->ref_pt[irp]->wlum[is][iw]);
				}
			}
		}
	}

	/* Read and discard BUILDING SHADES headings lines */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

	/* Read building shade data */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %d\n",&bldg_ptr->nbshades);
	for (ish=0; ish<bldg_ptr->nbshades; ish++) {
		bldg_ptr->bshade[ish] = new BSHADE;
		if (bldg_ptr->bshade[ish] == NULL) {
//		if ((bldg_ptr->bshade[ish] = (BSHADE *)malloc(sizeof(BSHADE))) == NULL) {
			*pofdmpfile << "ERROR: DElight Insufficient memory for BUILDING SHADE allocation\n";
			return(-1);
		}
		struct_init("BSHADE",(char *)bldg_ptr->bshade[ish]);

		/* Read and discard BUILDING SHADE headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %s\n",bldg_ptr->bshade[ish]->name); //,_countof(bldg_ptr->bshade[ish]->name));
		/* bldg shade origin in bldg system coordinates */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		/* tokenize the line label */
		token = strtok(cInputLine," ");
		for (icoord=0; icoord<NCOORDS; icoord++) {
			token = strtok(NULL," ");
			bldg_ptr->bshade[ish]->origin[icoord] = atof(token);
		}
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->bshade[ish]->height);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->bshade[ish]->width);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->bshade[ish]->azm);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->bshade[ish]->tilt);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->bshade[ish]->vis_refl);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&bldg_ptr->bshade[ish]->gnd_refl);
	}

	return(0);
}

/****************************** subroutine load_lib *****************************/
/* Loads a library data file (*.lib) from disk into an initialized lib structure. */
/****************************** subroutine load_lib *****************************/
int load_lib(
	LIB *lib_ptr,		/* library structure pointer */
	FILE *infile,		/* pointer to library data file */
	ofstream* pofdmpfile)	/* ptr to dump file */
{
	char cInputLine[MAX_CHAR_LINE+1];	/* Input line */
	int ig, iws;					/* indexes */

	/* Read and discard heading line */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

	/* Read and discard GLASS TYPES headings lines */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

	/* Read glass type data */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %d\n",&lib_ptr->nglass);
	for (ig=0; ig<lib_ptr->nglass; ig++) {
		lib_ptr->glass[ig] = new GLASS;
		if (lib_ptr->glass[ig] == NULL) {
//		if ((lib_ptr->glass[ig] = (GLASS *)malloc(sizeof(GLASS))) == NULL) {
			*pofdmpfile << "ERROR: DElight Insufficient memory for GLASS allocation\n";
			return(-1);
		}
		struct_init("GLASS",(char *)lib_ptr->glass[ig]);

		/* Read and discard GLASS TYPE DATA headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %s\n",lib_ptr->glass[ig]->name); //,_countof(lib_ptr->glass[ig]->name));
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->vis_trans);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->inside_refl);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->cam1);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->cam2);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->cam3);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->cam4);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->cam9);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->E10hemi_trans);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->E10coef[0]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->E10coef[1]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->E10coef[2]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->E10coef[3]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusDiffuse_Trans);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusCoef[0]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusCoef[1]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusCoef[2]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusCoef[3]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusCoef[4]);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->glass[ig]->EPlusCoef[5]);
	}

	/* Read and discard WSHADE TYPES headings lines */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

	/* Read wshade type data */
	if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
	sscanf(cInputLine,"%*s %d\n",&lib_ptr->nwshades);
	for (iws=0; iws<lib_ptr->nwshades; iws++) {
		lib_ptr->wshade[iws] = new WSHADE;
		if (lib_ptr->wshade[iws] == NULL) {
//		if ((lib_ptr->wshade[iws] = (WSHADE *)malloc(sizeof(WSHADE))) == NULL) {
			*pofdmpfile << "ERROR: DElight Insufficient memory for WINDOW SHADE allocation\n";
			return(-1);
		}
		struct_init("WSHADE",(char *)lib_ptr->wshade[iws]);

		/* Read and discard WSHADE TYPE DATA headings lines */
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;

		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %s\n",lib_ptr->wshade[iws]->name); //,_countof(lib_ptr->wshade[iws]->name));
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->wshade[iws]->vis_trans);
		if (fgets(cInputLine, MAX_CHAR_LINE, infile) == NULL) return -1;
		sscanf(cInputLine,"%*s %lf\n",&lib_ptr->wshade[iws]->inside_refl);
	}

	return(0);
}
