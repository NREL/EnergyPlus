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
#include "savedata.h"

/****************************** subroutine dump_bldg *****************************/
/* Writes bldg structure data to disk. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dump_bldg *****************************/
void dump_bldg(
	BLDG *bldg_ptr,		/* building structure pointer */
	FILE *outfile)		/* pointer to building data file */
{
	int ivert, icoord, im, iz, ils, is, iw, izs, ish, irp, izshade, idc, inode;	/* indexes */
	int iphs, iths, imon, ihr;	/* indexes */

	/* BLDG dump headings */
	fprintf(outfile,"\n");
	fprintf(outfile,"BUILDING DATA\n");

	/* Dump site data */
	fprintf(outfile,"Building_Name %s\n", bldg_ptr->name);
	fprintf(outfile,"Site_Latitude %5.2lf\n", bldg_ptr->lat);
	fprintf(outfile,"Site_Longitude %5.2lf\n", bldg_ptr->lon);
	fprintf(outfile,"Site_Altitude %5.2lf\n", bldg_ptr->alt);
	fprintf(outfile,"Building_Azimuth %5.2lf\n", bldg_ptr->azm);
	fprintf(outfile,"Site_TimeZone %5.2lf\n", bldg_ptr->timezone);
	/* monthly atmospheric turbidity */
	fprintf(outfile,"Atm_Turbidity");
	for (im=0; im<MONTHS; im++) {
		fprintf(outfile," %5.2lf", bldg_ptr->atmtur[im]);
	}
	fprintf(outfile,"\n");
	/* monthly atmospheric moisture */
	fprintf(outfile,"Atm_Moisture");
	for (im=0; im<MONTHS; im++) {
		fprintf(outfile," %5.2lf", bldg_ptr->atmmoi[im]);
	}
	fprintf(outfile,"\n");
	/* ----- derived quantities ----- */
	fprintf(outfile,"Exterior Horizontal Illuminance Components\n");
	fprintf(outfile,"        Overcast_Sky Clear_Sky Clear_Sun\n");
	for (iphs=(NPHS-1); iphs>=0; iphs--) {
		fprintf(outfile,"SunAlt %d: %9.2lf %9.2lf %9.2lf\n", iphs, bldg_ptr->hillumskyo[iphs], bldg_ptr->hillumskyc[iphs], bldg_ptr->hillumsunc[iphs]);
	}

	/* Dump zone data */
	fprintf(outfile,"\n");
	fprintf(outfile,"ZONES\n");
	fprintf(outfile,"N_Zones %d\n", bldg_ptr->nzones);
	for (iz=0; iz<bldg_ptr->nzones; iz++) {

		/* Write ZONE headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"ZONE DATA\n");

		fprintf(outfile,"Zone %s\n", bldg_ptr->zone[iz]->name);
		/* zone origin in bldg system coordinates */
		fprintf(outfile,"BldgSystem_Zone_Origin");
		for (icoord=0; icoord<NCOORDS; icoord++) {
			fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->origin[icoord]);
		}
		fprintf(outfile,"\n");
		fprintf(outfile,"Zone_Azimuth %5.2lf\n", bldg_ptr->zone[iz]->azm);
		fprintf(outfile,"Zone_Multiplier %5.2lf\n", bldg_ptr->zone[iz]->mult);
		fprintf(outfile,"Zone_Floor_Area %5.2lf\n", bldg_ptr->zone[iz]->flarea);
		fprintf(outfile,"Zone_Volume %5.2lf\n", bldg_ptr->zone[iz]->volume);
		fprintf(outfile,"Zone_Lighting %5.2lf\n", bldg_ptr->zone[iz]->lighting);
		fprintf(outfile,"Min_Input_Power %5.2lf\n", bldg_ptr->zone[iz]->min_power);
		fprintf(outfile,"Min_Light_Fraction %5.2lf\n", bldg_ptr->zone[iz]->min_light);
		fprintf(outfile,"Light_Ctrl_Steps %d\n", bldg_ptr->zone[iz]->lt_ctrl_steps);
		fprintf(outfile,"Light_Ctrl_Prob %5.2lf\n", bldg_ptr->zone[iz]->lt_ctrl_prob);
		fprintf(outfile,"View_Azimuth %5.2lf\n", bldg_ptr->zone[iz]->view_azm);
		fprintf(outfile,"Max_Grid_Node_Area %5.2lf\n", bldg_ptr->zone[iz]->max_grid_node_area);

		/* Write ZONE LIGHTING SCHEDULE headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"ZONE LIGHTING SCHEDULES\n");
		fprintf(outfile,"N_Lt_Scheds %d\n", bldg_ptr->zone[iz]->nltsch);

		/* Dump lighting schedule data */
		for (ils=0; ils<bldg_ptr->zone[iz]->nltsch; ils++) {

			/* Write ZONE LIGHTING SCHEDULE headings lines */
			fprintf(outfile,"\n");
			fprintf(outfile,"ZONE LIGHTING SCHEDULE DATA\n");

			fprintf(outfile,"Lt_Sched %s\n", bldg_ptr->zone[iz]->ltsch[ils]->name);
			fprintf(outfile,"Month_Begin %d\n", bldg_ptr->zone[iz]->ltsch[ils]->mon_begin);
			fprintf(outfile,"Day_Begin %d\n", bldg_ptr->zone[iz]->ltsch[ils]->day_begin);
			fprintf(outfile,"Month_End %d\n", bldg_ptr->zone[iz]->ltsch[ils]->mon_end);
			fprintf(outfile,"Day_End %d\n", bldg_ptr->zone[iz]->ltsch[ils]->day_end);
			fprintf(outfile,"Day_of_Week_Begin %d\n", bldg_ptr->zone[iz]->ltsch[ils]->dow_begin);
			fprintf(outfile,"Day_of_Week_End %d\n", bldg_ptr->zone[iz]->ltsch[ils]->dow_end);

			fprintf(outfile,"Hour_Fractions");
			for (ihr=0; ihr<HOURS; ihr++) {
				fprintf(outfile," %3.1lf", bldg_ptr->zone[iz]->ltsch[ils]->frac[ihr]);
			}
			fprintf(outfile,"\n");
			/* ----- derived quantities ----- */
			fprintf(outfile,"Day_of_Year_Begin %d\n", bldg_ptr->zone[iz]->ltsch[ils]->doy_begin);
			fprintf(outfile,"Day_of_Year_End %d\n", bldg_ptr->zone[iz]->ltsch[ils]->doy_end);
		}

		/* Write ZONE SURFACE headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"ZONE SURFACES\n");
		fprintf(outfile,"N_Surfaces %d\n", bldg_ptr->zone[iz]->nsurfs);

		/* Dump surface data */
		for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {

			/* Write SURFACE headings lines */
			fprintf(outfile,"\n");
			fprintf(outfile,"ZONE SURFACE DATA\n");

			fprintf(outfile,"Surface %s\n", bldg_ptr->zone[iz]->surf[is]->name);
			/* surface origin in zone system coordinates */
			fprintf(outfile,"ZoneSystem_Surface_Origin");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->origin[icoord]);
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Height %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->height);
			fprintf(outfile,"Width %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->width);
			fprintf(outfile,"ZoneSystem_Azimuth %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->azm_zs);
			fprintf(outfile,"ZoneSystem_Tilt %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->tilt_zs);
			fprintf(outfile,"Vis_Refl %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->vis_refl);
			fprintf(outfile,"Ext_Refl %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->ext_vis_refl);
			fprintf(outfile,"Gnd_Refl %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->gnd_refl);
			fprintf(outfile,"Surface_Type %d\n", bldg_ptr->zone[iz]->surf[is]->type);
			fprintf(outfile,"Area %8.2f\n", bldg_ptr->zone[iz]->surf[is]->area);
			fprintf(outfile,"E10Surf_Type_Index %d\n", bldg_ptr->zone[iz]->surf[is]->E10ndx);

			/* ----- derived quantities ----- */
			fprintf(outfile,"BldgSystem_Surface_Vertices\n");
			for (ivert=0; ivert<NVERTS; ivert++) {
				fprintf(outfile,"Vertex %d: ",ivert);
				for (icoord=0; icoord<NCOORDS; icoord++) {
					fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->vert[icoord][ivert]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"BldgSystem_Surface_Azimuth %5.2lf\n",bldg_ptr->zone[iz]->surf[is]->azm_bs);
			fprintf(outfile,"BldgSystem_Surface_Tilt %5.2lf\n",bldg_ptr->zone[iz]->surf[is]->tilt_bs);
			// outward pointing unit vector (DOE2) (bldg sys coords)
			fprintf(outfile,"BldgSystem_Surface_Outward_Normal_Unit_Vector");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->outward_uvect[icoord]);
			}
			fprintf(outfile,"\n");
			// inward pointing unit vector (Superlite) (bldg sys coords)
			fprintf(outfile,"BldgSystem_Surface_Inward_Normal_Unit_Vector");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->inward_uvect[icoord]);
			}
			fprintf(outfile,"\n");
			// surface direction cosine values (slite)
			fprintf(outfile,"Surface_Direction_Cosines");
			for (idc=0; idc<NDC; idc++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->dircos[idc]);
			}
			fprintf(outfile,"\n");

			fprintf(outfile,"Surface Exterior Luminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->ovrlum);
			fprintf(outfile,"Surface Exterior Luminances from Clear Sky\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->skylum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"Surface Exterior Luminances from Clear Sun\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->sunlum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}

			fprintf(outfile,"Surface_Node_Area %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->node_area);
			fprintf(outfile,"Surface_Width_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->n_width);
			fprintf(outfile,"Surface_Height_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->n_height);
			fprintf(outfile,"Surface_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->nnodes);

			fprintf(outfile,"Surface_Total_Direct_Illuminance_Data\n");
			fprintf(outfile,"Surface Total Direct Illuminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->TotDirectOvercastIllum);
			fprintf(outfile,"Surface Total Direct Illuminances from Clear Sky\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->TotDirectSkyCIllum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"Surface Total Direct Illuminances from Clear Sun\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->TotDirectSunCIllum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}

			fprintf(outfile,"Surface_Node_Data\n");
			for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->nnodes; inode++) {
				fprintf(outfile,"\nNode %3d BldgSystem_Node_Coordinates: ",inode);
				for (icoord=0; icoord<NCOORDS; icoord++) {
					fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->node[inode][icoord]);
				}
				fprintf(outfile,"\n");

			    fprintf(outfile,"Surface_Node_Area %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->node_areas[inode]);
				fprintf(outfile,"Surface Node Luminances\n");
				fprintf(outfile,"Surface Node Direct Luminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->direct_skyolum[inode]);
				fprintf(outfile,"Surface Node Direct Luminances from Clear Sky\n");
				fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
				for (iphs=(NPHS-1); iphs>=0; iphs--) {
					fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
						fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->direct_skyclum[inode][iphs][iths]);
					}
					fprintf(outfile,"\n");
				}
				fprintf(outfile,"Surface Node Direct Luminances from Clear Sun\n");
				fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
				for (iphs=(NPHS-1); iphs>=0; iphs--) {
					fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
						fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->direct_sunclum[inode][iphs][iths]);
					}
					fprintf(outfile,"\n");
				}

				fprintf(outfile,"Surface Node Total Luminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->skyolum[inode]);
				fprintf(outfile,"Surface Node Total Luminances from Clear Sky\n");
				fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
				for (iphs=(NPHS-1); iphs>=0; iphs--) {
					fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
						fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->skyclum[inode][iphs][iths]);
					}
					fprintf(outfile,"\n");
				}
				fprintf(outfile,"Surface Node Total Luminances from Clear Sun\n");
				fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
				for (iphs=(NPHS-1); iphs>=0; iphs--) {
					fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
						fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->sunclum[inode][iphs][iths]);
					}
					fprintf(outfile,"\n");
				}
			}

			/* Dump window data */
			/* Write SURFACE WINDOW headings lines */
			fprintf(outfile,"\n");
			fprintf(outfile,"SURFACE WINDOWS\n");
			fprintf(outfile,"N_Windows %d\n", bldg_ptr->zone[iz]->surf[is]->nwndos);
			for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {

				/* Write WINDOW DATA headings lines */
				fprintf(outfile,"\n");
				fprintf(outfile,"SURFACE WINDOW DATA\n");

				fprintf(outfile,"Window %s\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->name);
				/* window origin in surface system coordinates */
				fprintf(outfile,"SurfSystem__Window_Origin");
				for (icoord=0; icoord<NCOORDS; icoord++) {
					fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->origin[icoord]);
				}
				fprintf(outfile,"\n");
				fprintf(outfile,"Height %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->height);
				fprintf(outfile,"Width %5.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->width);
				fprintf(outfile,"Glass_Type %s\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->glass_type);
				fprintf(outfile,"Shade_Flag %d\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_flag);
				if (bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_flag != 0) {
					fprintf(outfile,"Shade_Type %s\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->shade_type);
				}
				/* window overhang/fin zone shade depth (ft) (viewed from inside looking outward (0=overhang, 1=right fin, 2=left fin)) */
				fprintf(outfile,"Window_Overhang_Fin_Depth_Values");
				for (izshade=0; izshade<NZSHADES; izshade++) {
					fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_x[izshade]);
				}
				fprintf(outfile,"\n");
				/* window overhang/fin zone shade distance from window (ft) */
				fprintf(outfile,"Window_Overhang_Fin_Distance_Values");
				for (izshade=0; izshade<NZSHADES; izshade++) {
					fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->zshade_y[izshade]);
				}
				fprintf(outfile,"\n");

				/* ----- derived quantities ----- */
				fprintf(outfile,"BldgSystem_Window_Vertices\n");
				for (ivert=0; ivert<NVERTS; ivert++) {
					fprintf(outfile,"Vertex %d: ",ivert);
					for (icoord=0; icoord<NCOORDS; icoord++) {
						fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->vert[icoord][ivert]);
					}
					fprintf(outfile,"\n");
				}

				fprintf(outfile,"Window Center Exterior Luminance from Overcast Sky\n");
				fprintf(outfile,"%8.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->wlumskyo);
				fprintf(outfile,"Window Center Exterior Luminances from Clear Sky\n");
				fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
				for (iphs=(NPHS-1); iphs>=0; iphs--) {
					fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
						fprintf(outfile," %8.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->wlumsky[iphs][iths]);
					}
					fprintf(outfile,"\n");
				}
				fprintf(outfile,"Window Center Exterior Luminances from Clear Sun\n");
				fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
				for (iphs=(NPHS-1); iphs>=0; iphs--) {
					fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
						fprintf(outfile," %8.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->wlumsun[iphs][iths]);
					}
					fprintf(outfile,"\n");
				}

				fprintf(outfile,"Window_Node_Area %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node_area);
				fprintf(outfile,"Window_Width_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->n_width);
				fprintf(outfile,"Window_Height_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->n_height);
				fprintf(outfile,"Window_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->nnodes);

				fprintf(outfile,"Window_Node_Data\n");
				for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->wndo[iw]->nnodes; inode++) {
					fprintf(outfile,"Node %3d BldgSystem_Node_Coordinates: ",inode);
					for (icoord=0; icoord<NCOORDS; icoord++) {
						fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node[inode][icoord]);
					}
					fprintf(outfile,"\n");

				    fprintf(outfile,"Window_Node_Area %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node_areas[inode]);
					fprintf(outfile,"Window Node Luminances\n");
					fprintf(outfile,"Window Node Direct Luminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_skyolum[inode]);
					fprintf(outfile,"Window Node Direct Luminances from Clear Sky\n");
					fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
					for (iphs=(NPHS-1); iphs>=0; iphs--) {
						fprintf(outfile,"SunAlt %d: ",iphs);
						for (iths=(NTHS-1); iths>=0; iths--) {
							fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_skyclum[inode][iphs][iths]);
						}
						fprintf(outfile,"\n");
					}
					fprintf(outfile,"Window Node Direct Luminances from Clear Sun\n");
					fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
					for (iphs=(NPHS-1); iphs>=0; iphs--) {
						fprintf(outfile,"SunAlt %d: ",iphs);
						for (iths=(NTHS-1); iths>=0; iths--) {
							fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_sunclum[inode][iphs][iths]);
						}
						fprintf(outfile,"\n");
					}

					fprintf(outfile,"Window Node Total Luminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->skyolum[inode]);
					fprintf(outfile,"Window Node Total Luminances from Clear Sky\n");
					fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
					for (iphs=(NPHS-1); iphs>=0; iphs--) {
						fprintf(outfile,"SunAlt %d: ",iphs);
						for (iths=(NTHS-1); iths>=0; iths--) {
							fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->skyclum[inode][iphs][iths]);
						}
						fprintf(outfile,"\n");
					}
					fprintf(outfile,"Window Node Total Luminances from Clear Sun\n");
					fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
					for (iphs=(NPHS-1); iphs>=0; iphs--) {
						fprintf(outfile,"SunAlt %d: ",iphs);
						for (iths=(NTHS-1); iths>=0; iths--) {
							fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->surf[is]->wndo[iw]->sunclum[inode][iphs][iths]);
						}
						fprintf(outfile,"\n");
					}
				}
			}

			/* Dump CFS Surface data */
			/* Write CFS SURFACE headings lines */
			fprintf(outfile,"\n");
			fprintf(outfile,"CFS SURFACE\n");
			fprintf(outfile,"N_CFSs %d\n", bldg_ptr->zone[iz]->surf[is]->ncfs);
			for (int icfs=0; icfs<bldg_ptr->zone[iz]->surf[is]->ncfs; icfs++) {

				/* Write CFS SURFACE DATA headings lines */
				fprintf(outfile,"\n");
				fprintf(outfile,"CFS SURFACE DATA\n");

                // Write CFS data
	            fprintf(outfile,"CFS Name %s\n", bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->Name().c_str());
	            fprintf(outfile,"CFS Type %s\n", bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->TypeName().c_str());
	            fprintf(outfile,"CFS N_Nodes %d\n", bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->MeshSize());
				fprintf(outfile,"BldgSystem_CFS_Vertices (inside lower-left corner counter-clockwise)\n");
				for (ivert=0; ivert<bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->nvert(); ivert++) {
					fprintf(outfile,"Vertex %d: ",ivert);
                    BGL::point3 pt3TmpPt = bldg_ptr->zone[iz]->surf[is]->cfs[icfs]->vert3D(ivert);
					for (icoord=0; icoord<NCOORDS; icoord++) {
						fprintf(outfile," %5.2lf", pt3TmpPt[icoord]);
					}
					fprintf(outfile,"\n");
				}
			} // End CFS loop
		} // End Surface loop

		/* Write ZONE SHADES headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"ZONE SHADES\n");
		fprintf(outfile,"N_ZShades %d\n", bldg_ptr->zone[iz]->nzshades);

		/* Write ZONE SHADE DATA */
		for (izs=0; izs<bldg_ptr->zone[iz]->nzshades; izs++) {
			fprintf(outfile,"\n");
			fprintf(outfile,"ZONE SHADE DATA\n");
			fprintf(outfile,"ZShade %s\n", bldg_ptr->zone[iz]->zshade[izs]->name);
			/* shade origin in zone system coordinates */
			fprintf(outfile,"ZoneSystem_ZShade_Origin");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->zshade[izs]->origin[icoord]);
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Height %5.2lf\n", bldg_ptr->zone[iz]->zshade[izs]->height);
			fprintf(outfile,"Width %5.2lf\n", bldg_ptr->zone[iz]->zshade[izs]->width);
			fprintf(outfile,"ZoneSystem_Azimuth %5.2lf\n", bldg_ptr->zone[iz]->zshade[izs]->azm_zs);
			fprintf(outfile,"ZoneSystem_Tilt %5.2lf\n", bldg_ptr->zone[iz]->zshade[izs]->tilt_zs);

			/* ----- derived quantities ----- */
			fprintf(outfile,"BldgSystem_ZShade_Vertices\n");
			for (ivert=0; ivert<NVERTS; ivert++) {
				fprintf(outfile,"Vertex %d: ",ivert);
				for (icoord=0; icoord<NCOORDS; icoord++) {
					fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->zshade[izs]->vert[icoord][ivert]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"BldgSystem_Azimuth %5.2lf\n", bldg_ptr->zone[iz]->zshade[izs]->azm_bs);
			fprintf(outfile,"BldgSystem_Tilt %5.2lf\n", bldg_ptr->zone[iz]->zshade[izs]->tilt_bs);
		}

		/* Write ZONE REFERENCE POINT headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"ZONE REFERENCE POINTS\n");
		fprintf(outfile,"N_Reference_Points %d\n", bldg_ptr->zone[iz]->nrefpts);

		/* Dump ref pt data */
		for (irp=0; irp<bldg_ptr->zone[iz]->nrefpts; irp++) {

			/* Write REFERENCE POINT DATA headings lines */
			fprintf(outfile,"\n");
			fprintf(outfile,"ZONE REFERENCE POINT DATA\n");

			fprintf(outfile,"Reference_Point %s\n", bldg_ptr->zone[iz]->ref_pt[irp]->name);
			/* ref pt in zone system coordinates */
			fprintf(outfile,"ZoneSystem_RefPt_Coords");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->ref_pt[irp]->zs[icoord]);
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Zone_Fraction %5.2lf\n", bldg_ptr->zone[iz]->ref_pt[irp]->zone_frac);
			fprintf(outfile,"Light_Set_Pt %5.2lf\n", bldg_ptr->zone[iz]->ref_pt[irp]->lt_set_pt);
			fprintf(outfile,"Light_Ctrl_Type %d\n", bldg_ptr->zone[iz]->ref_pt[irp]->lt_ctrl_type);

			/* --------------- derived quantities ------------------ */
			/* ref pt in bldg system coordinates */
			fprintf(outfile,"BldgSystem_RefPt_Coords");
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->zone[iz]->ref_pt[irp]->bs[icoord]);
			}
			fprintf(outfile,"\n");

			fprintf(outfile,"Reference Point Direct Illuminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->ref_pt[irp]->direct_skyoillum);
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Direct Illuminances from Clear Sky\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
					for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->ref_pt[irp]->direct_skycillum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Direct Illuminances from Clear Sun\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->ref_pt[irp]->direct_suncillum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"\n");

			fprintf(outfile,"Reference Point Total Illuminance from Overcast Sky %8.2lf\n", bldg_ptr->zone[iz]->ref_pt[irp]->skyoillum);
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Total Illuminances from Clear Sky\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->ref_pt[irp]->skycillum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Total Illuminances from Clear Sun\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.5lf", bldg_ptr->zone[iz]->ref_pt[irp]->suncillum[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Monthly Average Daylight Illuminances\n");
			fprintf(outfile,"                                                            Hour of Day\n");
			fprintf(outfile,"\n");
			fprintf(outfile,"Month      1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24\n");
			fprintf(outfile,"-----   ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----\n");
			for (imon=0; imon<MONTHS; imon++) {
				fprintf(outfile,"%3d    ",imon+1);
				for (ihr=0; ihr<HOURS; ihr++) {
					fprintf(outfile," %4.0lf", bldg_ptr->zone[iz]->ref_pt[irp]->day_illum[imon][ihr]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Daylight Factor for Overcast Sky %8.4lf\n", bldg_ptr->zone[iz]->ref_pt[irp]->dfskyo);
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Daylight Factors for Clear Sky\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.4lf", bldg_ptr->zone[iz]->ref_pt[irp]->dfsky[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
			fprintf(outfile,"\n");
			fprintf(outfile,"Reference Point Daylight Factors for Clear Sun\n");
			fprintf(outfile,"            SunAzm-4 SunAzm-3 SunAzm-2 SunAzm-1 SunAzm-0\n");
			for (iphs=(NPHS-1); iphs>=0; iphs--) {
				fprintf(outfile,"SunAlt %d: ",iphs);
				for (iths=(NTHS-1); iths>=0; iths--) {
					fprintf(outfile," %8.4lf", bldg_ptr->zone[iz]->ref_pt[irp]->dfsun[iphs][iths]);
				}
				fprintf(outfile,"\n");
			}
		}
		/* --------------- calculated ZONE quantities ------------------ */
		// monthly avg fraction lighting energy reduction
		fprintf(outfile,"\n");
		fprintf(outfile,"Lighting Zone Monthly Average Fraction Lighting Energy Reduction\n");
		fprintf(outfile,"                                                            Hour of Day\n");
		fprintf(outfile,"\n");
		fprintf(outfile,"Month      1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24\n");
		fprintf(outfile,"-----   ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----\n");
		for (imon=0; imon<MONTHS; imon++) {
			fprintf(outfile,"%3d    ",imon+1);
			for (ihr=0; ihr<HOURS; ihr++) {
				fprintf(outfile," %4.2lf", bldg_ptr->zone[iz]->lt_reduc[imon][ihr]);
			}
			fprintf(outfile,"\n");
		}
		/* annual avg fraction lighting energy reduction */
		fprintf(outfile,"\n");
		fprintf(outfile,"Lighting Zone Annual Average Fraction Lighting Energy Reduction\n");
		fprintf(outfile,"                                                            Hour of Day\n");
		fprintf(outfile,"\n");
		fprintf(outfile,"           1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24\n");
		fprintf(outfile,"        ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----\n");
		fprintf(outfile,"       ");
		for (ihr=0; ihr<HOURS; ihr++) {
			fprintf(outfile," %4.2lf", bldg_ptr->zone[iz]->annual_reduc[ihr]);
		}
		fprintf(outfile,"\n");
	}

	/* Write BUILDING SHADES headings lines */
	fprintf(outfile,"\n");
	fprintf(outfile,"BUILDING SHADES\n");
	fprintf(outfile,"N_BShades %d\n", bldg_ptr->nbshades);

	/* Write BUILDING SHADE DATA */
	for (ish=0; ish<bldg_ptr->nbshades; ish++) {
		fprintf(outfile,"\n");
		fprintf(outfile,"BUILDING SHADE DATA\n");
		fprintf(outfile,"BShade %s\n", bldg_ptr->bshade[ish]->name);
		/* shade origin in building system coordinates */
		fprintf(outfile,"BldgSystem_BShade_Origin");
		for (icoord=0; icoord<NCOORDS; icoord++) {
			fprintf(outfile," %5.2lf", bldg_ptr->bshade[ish]->origin[icoord]);
		}
		fprintf(outfile,"\n");
		fprintf(outfile,"Height %5.2lf\n", bldg_ptr->bshade[ish]->height);
		fprintf(outfile,"Width %5.2lf\n", bldg_ptr->bshade[ish]->width);
		fprintf(outfile,"Azimuth %5.2lf\n", bldg_ptr->bshade[ish]->azm);
		fprintf(outfile,"Tilt %5.2lf\n", bldg_ptr->bshade[ish]->tilt);
		fprintf(outfile,"Vis_Refl %5.2lf\n", bldg_ptr->bshade[ish]->vis_refl);
		fprintf(outfile,"Gnd_Refl %5.2lf\n", bldg_ptr->bshade[ish]->gnd_refl);

		/* ----- derived quantities ----- */
		fprintf(outfile,"BldgSystem_BShade_Vertices\n");
		for (ivert=0; ivert<NVERTS; ivert++) {
			fprintf(outfile,"Vertex %d: ",ivert);
			for (icoord=0; icoord<NCOORDS; icoord++) {
				fprintf(outfile," %5.2lf", bldg_ptr->bshade[ish]->vert[icoord][ivert]);
			}
			fprintf(outfile,"\n");
		}
		fprintf(outfile,"Building Shade Luminance from Sky for Clear Sky\n");
		for (iphs=(NPHS-1); iphs>=0; iphs--) {
			fprintf(outfile,"SunAlt %d: ",iphs);
			for (iths=(NTHS-1); iths>=0; iths--) {
				fprintf(outfile," %5.2lf", bldg_ptr->bshade[ish]->skylum[iphs][iths]);
			}
			fprintf(outfile,"\n");
		}
		fprintf(outfile,"Building Shade Luminance from Sun for Clear Sky\n");
		for (iphs=(NPHS-1); iphs>=0; iphs--) {
			fprintf(outfile,"SunAlt %d: ",iphs);
			for (iths=(NTHS-1); iths>=0; iths--) {
				fprintf(outfile," %5.2lf", bldg_ptr->bshade[ish]->sunlum[iphs][iths]);
			}
			fprintf(outfile,"\n");
		}
		fprintf(outfile,"Building Shade Luminance for Overcast Sky = %5.2lf\n", bldg_ptr->bshade[ish]->ovrlum);
	}

	return;
}

/****************************** subroutine dump_lib *****************************/
/* Writes library structure data to disk. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine dump_lib *****************************/
void dump_lib(
	LIB *lib_ptr,		/* library structure pointer */
	FILE *outfile)		/* pointer to library data file */
{
	int ig, iws;	/* indexes */

	/* LIB dump headings */
	fprintf(outfile,"\n");
	fprintf(outfile,"LIBRARY DATA\n");

	/* Dump glass data */
	fprintf(outfile,"GLASS TYPES\n");
	fprintf(outfile,"N_Glass_Types %d\n", lib_ptr->nglass);
	for (ig=0; ig<lib_ptr->nglass; ig++) {

		/* Write GLASS TYPE DATA headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"GLASS TYPE DATA\n");

		fprintf(outfile,"Name %s\n", lib_ptr->glass[ig]->name);
		fprintf(outfile,"Visible_Transmittance %10.6lf\n", lib_ptr->glass[ig]->vis_trans);
		fprintf(outfile,"Inside_Reflectance %10.6lf\n", lib_ptr->glass[ig]->inside_refl);
		fprintf(outfile,"CAM1 %10.6lf\n", lib_ptr->glass[ig]->cam1);
		fprintf(outfile,"CAM2 %10.6lf\n", lib_ptr->glass[ig]->cam2);
		fprintf(outfile,"CAM3 %10.6lf\n", lib_ptr->glass[ig]->cam3);
		fprintf(outfile,"CAM4 %10.6lf\n", lib_ptr->glass[ig]->cam4);
		fprintf(outfile,"CAM9 %10.6lf\n", lib_ptr->glass[ig]->cam9);
		fprintf(outfile,"E10Hemispherical_Transmittance %10.6lf\n", lib_ptr->glass[ig]->E10hemi_trans);
		fprintf(outfile,"E10Coefficient1 %10.6lf\n", lib_ptr->glass[ig]->E10coef[0]);
		fprintf(outfile,"E10Coefficient2 %10.6lf\n", lib_ptr->glass[ig]->E10coef[1]);
		fprintf(outfile,"E10Coefficient3 %10.6lf\n", lib_ptr->glass[ig]->E10coef[2]);
		fprintf(outfile,"E10Coefficient4 %10.6lf\n", lib_ptr->glass[ig]->E10coef[3]);
		fprintf(outfile,"W4hemi_trans %10.6lf\n", lib_ptr->glass[ig]->W4hemi_trans);
		fprintf(outfile,"W4vis_fit1 %10.6lf\n", lib_ptr->glass[ig]->W4vis_fit1);
		fprintf(outfile,"W4vis_fit2 %10.6lf\n", lib_ptr->glass[ig]->W4vis_fit2);
		fprintf(outfile,"EPlusDiffuse_Trans %10.6lf\n", lib_ptr->glass[ig]->EPlusDiffuse_Trans);
		fprintf(outfile,"EPlusCoef1 %10.6lf\n", lib_ptr->glass[ig]->EPlusCoef[0]);
		fprintf(outfile,"EPlusCoef2 %10.6lf\n", lib_ptr->glass[ig]->EPlusCoef[1]);
		fprintf(outfile,"EPlusCoef3 %10.6lf\n", lib_ptr->glass[ig]->EPlusCoef[2]);
		fprintf(outfile,"EPlusCoef4 %10.6lf\n", lib_ptr->glass[ig]->EPlusCoef[3]);
		fprintf(outfile,"EPlusCoef5 %10.6lf\n", lib_ptr->glass[ig]->EPlusCoef[4]);
		fprintf(outfile,"EPlusCoef6 %10.6lf\n", lib_ptr->glass[ig]->EPlusCoef[5]);
	}

	/* Dump wshade data */
	fprintf(outfile,"\n");
	fprintf(outfile,"WSHADE TYPES\n");
	fprintf(outfile,"N_WShade_Types %d\n", lib_ptr->nwshades);
	for (iws=0; iws<lib_ptr->nwshades; iws++) {

		/* Write WSHADE TYPE DATA headings lines */
		fprintf(outfile,"\n");
		fprintf(outfile,"WSHADE TYPE DATA\n");

		fprintf(outfile,"Name %s\n", lib_ptr->wshade[iws]->name);
		fprintf(outfile,"Visible_Transmittance %5.2lf\n", lib_ptr->wshade[iws]->vis_trans);
		fprintf(outfile,"Inside_Reflectance %5.2lf\n", lib_ptr->wshade[iws]->inside_refl);
	}

	return;
}
