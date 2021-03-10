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
#include "struct.h"

/****************************** subroutine struct_init *****************************/
/* Initializes data structure elements. */
/****************************** subroutine struct_init *****************************/
int struct_init(
	const char *type,	/* string identifier of structure to be initialized */
	void *sptr)		/* generic pointer to structure to be initialized */
{
	int		ii, jj, kk, ll;

	if (strcmp(type,"WNDO") == 0) {
		strcpy(((WNDO *)sptr)->name,"");
		for(ii =0; ii<NCOORDS; ii++) {
			((WNDO *)sptr)->origin[ii] = 0.;
		}
		((WNDO *)sptr)->height = 0.;
		((WNDO *)sptr)->width = 0.;
		((WNDO *)sptr)->nvertices = 0;
		strcpy(((WNDO *)sptr)->glass_type,"");
		((WNDO *)sptr)->shade_flag = 0;
		strcpy(((WNDO *)sptr)->shade_type,"");
		for(ii =0; ii<NZSHADES; ii++) {
			((WNDO *)sptr)->zshade_x[ii] = 0.F;
			((WNDO *)sptr)->zshade_y[ii] = 0.F;
		}
		/* ----- derived quantities ----- */
		for(ii =0; ii<NCOORDS; ii++) {
			for(jj =0; jj<NVERTS; jj++) {
				((WNDO *)sptr)->vert[ii][jj] = 0.;
			}
		}
		/* ----- interreflection derived quantities ----- */
		((WNDO *)sptr)->node_area = 0.;
		((WNDO *)sptr)->n_width = 0;
		((WNDO *)sptr)->n_height = 0;
		((WNDO *)sptr)->nnodes = 0;
		for(ii =0; ii<MAX_WNDO_NODES; ii++) {
			((WNDO *)sptr)->node_areas[ii] = 0.;
			((WNDO *)sptr)->direct_skyolum[ii] = 0.;
			((WNDO *)sptr)->skyolum[ii] = 0.;
			for(jj =0; jj<NCOORDS; jj++) {
				((WNDO *)sptr)->node[ii][jj] = 0.;
			}
		}
		for(kk =0; kk<NPHS; kk++) {
			for(ll =0; ll<NTHS; ll++) {
				((WNDO *)sptr)->wlumsky[kk][ll] = 0.;
				((WNDO *)sptr)->wlumsun[kk][ll] = 0.;
				for(ii =0; ii<MAX_WNDO_NODES; ii++) {
					((WNDO *)sptr)->direct_skyclum[ii][kk][ll] = 0.;
					((WNDO *)sptr)->direct_sunclum[ii][kk][ll] = 0.;
					((WNDO *)sptr)->skyclum[ii][kk][ll] = 0.;
					((WNDO *)sptr)->sunclum[ii][kk][ll] = 0.;
				}
			}
		}
		((WNDO *)sptr)->wlumskyo = 0;
	}
	else if (strcmp(type,"LTSCH") == 0) {
		strcpy(((LTSCH *)sptr)->name,"");
		((LTSCH *)sptr)->mon_begin = 0;
		((LTSCH *)sptr)->day_begin = 0;
		((LTSCH *)sptr)->mon_end = 0;
		((LTSCH *)sptr)->day_end = 0;
		((LTSCH *)sptr)->dow_begin = 0;
		((LTSCH *)sptr)->dow_end = 0;
		for(ii =0; ii<HOURS; ii++) {
			((LTSCH *)sptr)->frac[ii] = 1.;
		}
		((LTSCH *)sptr)->doy_begin = 0;
		((LTSCH *)sptr)->doy_end = 0;
	}
	else if (strcmp(type,"WLUM") == 0) {
		for(ii =0; ii<NPHS; ii++) {
			for(jj =0; jj<NTHS; jj++) {
				((WLUM *)sptr)->sfsky[ii][jj] = 0.;
				((WLUM *)sptr)->sfsun[ii][jj] = 0.;
			}
		}
		((WLUM *)sptr)->sfskyo = 0.;
		((WLUM *)sptr)->omega = 0.;
		((WLUM *)sptr)->omegaw = 0.;
	}
	else if (strcmp(type,"REFPT") == 0) {
		strcpy(((REFPT *)sptr)->name,"");
		for(ii =0; ii<NCOORDS; ii++) {
			((REFPT *)sptr)->zs[ii] = 0.;
			((REFPT *)sptr)->bs[ii] = 0.;
		}
		((REFPT *)sptr)->zone_frac = 0.;
		((REFPT *)sptr)->lt_set_pt = 0.;
		((REFPT *)sptr)->lt_ctrl_type = 0;
		((REFPT *)sptr)->skyoillum = 0.;
		((REFPT *)sptr)->daylight = 0.;
		((REFPT *)sptr)->glarendx = 0.;
		((REFPT *)sptr)->frac_power = 0.;
		((REFPT *)sptr)->dfskyo = 0.;
		((REFPT *)sptr)->bfskyo = 0.;
		for(ii =0; ii<NPHS; ii++) {
			for(jj =0; jj<NTHS; jj++) {
				((REFPT *)sptr)->skycillum[ii][jj] = 0.;
				((REFPT *)sptr)->suncillum[ii][jj] = 0.;
				((REFPT *)sptr)->dfsky[ii][jj] = 0.;
				((REFPT *)sptr)->dfsun[ii][jj] = 0.;
				((REFPT *)sptr)->bfsky[ii][jj] = 0.;
				((REFPT *)sptr)->bfsun[ii][jj] = 0.;
			}
		}
		for(ii =0; ii<MONTHS; ii++) {
			for(jj =0; jj<HOURS; jj++) {
				((REFPT *)sptr)->day_illum[ii][jj] = 0.;
				((REFPT *)sptr)->glare[ii][jj] = 0.;
			}
		}
		for(ii =0; ii<MAX_ZONE_SURFS; ii++) {
			for(jj =0; jj<MAX_SURF_WNDOS; jj++) {
				((REFPT *)sptr)->wlum[ii][jj] = NULL;
			}
		}
		for(ii =0; ii<NSKYTYPE; ii++) {
			((REFPT *)sptr)->dcm_glare[ii] = 0.;
		}
		/* --------------- interreflection variables --------------- */
		((REFPT *)sptr)->delf_overcast = 0.;
		((REFPT *)sptr)->direct_skyoillum = 0.;
		for(kk =0; kk<NPHS; kk++) {
			for(ll =0; ll<NTHS; ll++) {
				((REFPT *)sptr)->delf_skyclear[kk][ll] = 0.;
				((REFPT *)sptr)->delf_sunclear[kk][ll] = 0.;
				((REFPT *)sptr)->direct_skycillum[kk][ll] = 0.;
				((REFPT *)sptr)->direct_suncillum[kk][ll] = 0.;
			}
		}
	}
	else if (strcmp(type,"SURF") == 0) {
		strcpy(((SURF *)sptr)->name,"");
		for(ii =0; ii<NCOORDS; ii++) {
			((SURF *)sptr)->origin[ii] = 0.;
		}
		((SURF *)sptr)->height = 0.;
		((SURF *)sptr)->width = 0.;
		((SURF *)sptr)->azm_zs = 0.;
		((SURF *)sptr)->tilt_zs = 90.;
		((SURF *)sptr)->vis_refl = 0.5;
		((SURF *)sptr)->ext_vis_refl = 0.;
		((SURF *)sptr)->gnd_refl = 0.2;
		((SURF *)sptr)->type = 2;
		((SURF *)sptr)->area = 0.;
		((SURF *)sptr)->E10ndx = 0;
		((SURF *)sptr)->nwndos = 0;
		for(ii =0; ii<MAX_SURF_WNDOS; ii++) {
			((SURF *)sptr)->wndo[ii]  = NULL;
		}
		((SURF *)sptr)->ncfs = 0;
		for(ii =0; ii<MAX_SURF_CFS; ii++) {
			((SURF *)sptr)->cfs[ii]  = NULL;
		}
		((SURF *)sptr)->nvertices = 0;
		/* ----- derived quantities ----- */
		for(ii =0; ii<NCOORDS; ii++) {
			((SURF *)sptr)->outward_uvect[ii] = 0.;
			((SURF *)sptr)->inward_uvect[ii] = 0.;
			for(jj =0; jj<NVERTS; jj++) {
				((SURF *)sptr)->vert[ii][jj] = 0.;
			}
		}
		((SURF *)sptr)->azm_bs = 0.;
		((SURF *)sptr)->tilt_bs = 0.;
		((SURF *)sptr)->TotDirectOvercastIllum = 0.;
		for(ii =0; ii<NPHS; ii++) {
			for(jj =0; jj<NTHS; jj++) {
				((SURF *)sptr)->skylum[ii][jj] = 0.;
				((SURF *)sptr)->sunlum[ii][jj] = 0.;
				((SURF *)sptr)->TotDirectSkyCIllum[ii][jj] = 0.;
				((SURF *)sptr)->TotDirectSunCIllum[ii][jj] = 0.;
			}
		}
		((SURF *)sptr)->ovrlum = 0.;
		/* ----- interreflection derived quantities ----- */
		for(ii =0; ii<NDC; ii++) {
			((SURF *)sptr)->dircos[ii] = 0.;
		}
		((SURF *)sptr)->node_area = 0.;
		((SURF *)sptr)->n_width = 0;
		((SURF *)sptr)->n_height = 0;
		((SURF *)sptr)->nnodes = 0;
		for(ii =0; ii<MAX_SURF_NODES; ii++) {
			((SURF *)sptr)->node_areas[ii] = 0.;
			((SURF *)sptr)->direct_skyolum[ii] = 0.;
			((SURF *)sptr)->skyolum[ii] = 0.;
			for(jj =0; jj<NCOORDS; jj++) {
				((SURF *)sptr)->node[ii][jj] = 0.;
			}
			for(kk =0; kk<NPHS; kk++) {
				for(ll =0; ll<NTHS; ll++) {
					((SURF *)sptr)->direct_skyclum[ii][kk][ll] = 0.;
					((SURF *)sptr)->direct_sunclum[ii][kk][ll] = 0.;
					((SURF *)sptr)->skyclum[ii][kk][ll] = 0.;
					((SURF *)sptr)->sunclum[ii][kk][ll] = 0.;
				}
			}
		}
	}
	else if (strcmp(type,"ZONE") == 0) {
		strcpy(((ZONE *)sptr)->name,"");
		for(ii =0; ii<NCOORDS; ii++) {
			((ZONE *)sptr)->origin[ii] = 0.;
		}
		((ZONE *)sptr)->azm = 0.;
		((ZONE *)sptr)->mult = 1.;
		((ZONE *)sptr)->flarea = 0.;
		((ZONE *)sptr)->volume = 0.;
		((ZONE *)sptr)->lighting = 0.;
		((ZONE *)sptr)->min_power = 0.;
		((ZONE *)sptr)->min_light = 0.;
		((ZONE *)sptr)->lt_ctrl_steps = 0;
		((ZONE *)sptr)->lt_ctrl_prob = 1.;
		((ZONE *)sptr)->view_azm = 0.;
		((ZONE *)sptr)->max_grid_node_area = 1.;
		((ZONE *)sptr)->nltsch = 0;
		for(ii =0; ii<MAX_LT_SCHEDS; ii++) {
			((ZONE *)sptr)->ltsch[ii]  = NULL;
		}
		((ZONE *)sptr)->nsurfs = 0;
		for(ii =0; ii<MAX_ZONE_SURFS; ii++) {
			((ZONE *)sptr)->surf[ii]  = NULL;
		}
		((ZONE *)sptr)->nzshades = 0;
		for(ii =0; ii<MAX_ZONE_SHADES; ii++) {
			((ZONE *)sptr)->zshade[ii]  = NULL;
		}
		((ZONE *)sptr)->nrefpts = 0;
		for(ii =0; ii<MAX_REF_PTS; ii++) {
			((ZONE *)sptr)->ref_pt[ii]  = NULL;
		}
		strcpy(((ZONE *)sptr)->e10zonename,"");
		((ZONE *)sptr)->eleclt_details = 0;
		((ZONE *)sptr)->frac_power = 0.;
		((ZONE *)sptr)->ltsch_id = 0;
		for(ii =0; ii<MONTHS; ii++) {
			for(jj =0; jj<HOURS; jj++) {
				((ZONE *)sptr)->annual_reduc[jj] = 0.;
				((ZONE *)sptr)->lt_reduc[ii][jj] = 0.;
			}
		}
	}
	else if (strcmp(type,"BLDG") == 0) {
		strcpy(((BLDG *)sptr)->name,"");
		((BLDG *)sptr)->lat = 0.;
		((BLDG *)sptr)->lon = 0.;
		((BLDG *)sptr)->alt = 0.;
		((BLDG *)sptr)->azm = 0.;
		((BLDG *)sptr)->timezone = 0.;
		for(ii =0; ii<MONTHS; ii++) {
			((BLDG *)sptr)->atmtur[ii] = 0.;
			((BLDG *)sptr)->atmmoi[ii] = 0.;
		}
		((BLDG *)sptr)->nzones = 0;
		for(ii =0; ii<MAX_BLDG_ZONES; ii++) {
			((BLDG *)sptr)->zone[ii]  = NULL;
		}
		((BLDG *)sptr)->nbshades = 0;
		for(ii =0; ii<MAX_BLDG_SHADES; ii++) {
			((BLDG *)sptr)->bshade[ii]  = NULL;
		}
		/* ----- derived quantities ----- */
		for(kk =0; kk<NPHS; kk++) {
			((BLDG *)sptr)->hillumskyc[kk] = 0.;
			((BLDG *)sptr)->hillumskyo[kk] = 0.;
			((BLDG *)sptr)->hillumsunc[kk] = 0.;
		}
	}
	else if (strcmp(type,"ZSHADE") == 0) {
		strcpy(((ZSHADE *)sptr)->name,"");
		for(ii =0; ii<NCOORDS; ii++) {
			((ZSHADE *)sptr)->origin[ii] = 0.;
		}
		((ZSHADE *)sptr)->height = 0.;
		((ZSHADE *)sptr)->width = 0.;
		((ZSHADE *)sptr)->azm_zs = 0.;
		((ZSHADE *)sptr)->tilt_zs = 0.;
		/* ----- derived quantities ----- */
		for(ii =0; ii<NCOORDS; ii++) {
			for(jj =0; jj<NVERTS; jj++) {
				((ZSHADE *)sptr)->vert[ii][jj] = 0.;
			}
		}
		((ZSHADE *)sptr)->azm_bs = 0.;
		((ZSHADE *)sptr)->tilt_bs = 0.;
	}
	else if (strcmp(type,"BSHADE") == 0) {
		strcpy(((BSHADE *)sptr)->name,"");
		for(ii =0; ii<NCOORDS; ii++) {
			((BSHADE *)sptr)->origin[ii] = 0.;
		}
		((BSHADE *)sptr)->height = 0.;
		((BSHADE *)sptr)->width = 0.;
		((BSHADE *)sptr)->azm = 0.;
		((BSHADE *)sptr)->tilt = 0.;
		((BSHADE *)sptr)->vis_refl = 0.5;
		((BSHADE *)sptr)->gnd_refl = 0.2;
		/* ----- derived quantities ----- */
		for(ii =0; ii<NCOORDS; ii++) {
			for(jj =0; jj<NVERTS; jj++) {
				((BSHADE *)sptr)->vert[ii][jj] = 0.;
			}
		}
		for(ii =0; ii<NPHS; ii++) {
			for(jj =0; jj<NTHS; jj++) {
				((BSHADE *)sptr)->skylum[ii][jj] = 0.;
				((BSHADE *)sptr)->sunlum[ii][jj] = 0.;
			}
		}
		((BSHADE *)sptr)->ovrlum = 0.;
	}
	else if (strcmp(type,"GLASS") == 0) {
		strcpy(((GLASS *)sptr)->name,"");
		((GLASS *)sptr)->vis_trans = 1.;
		((GLASS *)sptr)->inside_refl = 0.15;
		((GLASS *)sptr)->cam1 = 0.;
		((GLASS *)sptr)->cam2 = 0.;
		((GLASS *)sptr)->cam3 = 0.;
		((GLASS *)sptr)->cam4 = 0.;
		((GLASS *)sptr)->cam9 = 0.;
		((GLASS *)sptr)->E10hemi_trans = 1.F;
		((GLASS *)sptr)->W4hemi_trans = 1.F;
		((GLASS *)sptr)->W4vis_fit1 = 0.F;
		((GLASS *)sptr)->W4vis_fit2 = 0.F;
		for(ii =0; ii<4; ii++) {
			((GLASS *)sptr)->E10coef[ii]  = 0.;
		}
	}
	else if (strcmp(type,"WSHADE") == 0) {
		strcpy(((WSHADE *)sptr)->name,"");
		((WSHADE *)sptr)->vis_trans = 1.;
		((WSHADE *)sptr)->inside_refl = 0.;
	}
	else if (strcmp(type,"LIB") == 0) {
		strcpy(((LIB *)sptr)->name,"");
		((LIB *)sptr)->nglass = 0;
		((LIB *)sptr)->nwshades = 0;
		for(ii =0; ii<MAX_LIB_COMPS; ii++) {
			((LIB *)sptr)->glass[ii]  = NULL;
			((LIB *)sptr)->wshade[ii]  = NULL;
		}
	}
	else if (strcmp(type,"ZONE_REFL") == 0) {
		((ZONE_REFL *)sptr)->nwtot = 0;
		((ZONE_REFL *)sptr)->atot = 0.;
		((ZONE_REFL *)sptr)->arhtot = 0.;
		for(ii =0; ii<NTILTS; ii++) {
			((ZONE_REFL *)sptr)->ar[ii]  = 0.;
			((ZONE_REFL *)sptr)->arh[ii]  = 0.;
		}
		((ZONE_REFL *)sptr)->avg_refl = 0.;
	}
	else return (-1);

	return(0);
}
