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
#include "Radiosity.h"
#include "TOOLS.H"

/************************* subroutine slite_interreflect ************************/
/* Interreflection calculations based on radiosity approach */
/* taken from Superlite. */
/* Calculates configuration (form) factors between pairs of nodes */
/* and iterates to determine interreflected daylight contribution. */
/* Calculates configuration (form) factors between reference points and */
/* visible surface nodes to determine interreflected daylight contribution. */
/* Based on Superlite conventions. */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************* subroutine slite_interreflect ************************/
int slite_interreflect(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	LIB *lib_ptr,		/* pointer to library structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int niterate,		/* number of radiosity iterations */
	ofstream* pofdmpfile)	/* ptr to LBLDLL error dump file */
{
	int iter;		/* interreflection iteration loop index */
	int iz, is, iw, inode, iphs, iths;	/* loop indexes */
	int igt;		/* glass type index */
	double frac;	/* surface reflectance divided by PI */

    // Init return value
    int iReturnVal = 0;

	/* for each zone in the bldg */
	for (iz=0; iz<bldg_ptr->nzones; iz++) {
		/* for each surface in the zone */
		for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {
			/* for each surface node */
			for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->nnodes; inode++) {
				/* for overcast sky condition, init each surface node total illuminance to its initial illuminance */
				bldg_ptr->zone[iz]->surf[is]->skyolum[inode] = bldg_ptr->zone[iz]->surf[is]->direct_skyolum[inode];
				/* for each Sun Position Altitude */
				for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
					/* for each Sun Position Azimuth */
					for (iths=0; iths<sun_ptr->nths; iths++) {
						/* for each clear sky sun position, init each surface node total luminance to its initial luminance */
						bldg_ptr->zone[iz]->surf[is]->skyclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[is]->direct_skyclum[inode][iphs][iths];
						bldg_ptr->zone[iz]->surf[is]->sunclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[is]->direct_sunclum[inode][iphs][iths];
					}
				}
			}
			/* for each window in the surface */
			for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {
				/* for each window node */
				for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->wndo[iw]->nnodes; inode++) {
					/* for overcast sky condition, init each window node total luminance to its initial luminance */
					bldg_ptr->zone[iz]->surf[is]->wndo[iw]->skyolum[inode] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_skyolum[inode];
					/* for each Sun Position Altitude */
					for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
						/* for each Sun Position Azimuth */
						for (iths=0; iths<sun_ptr->nths; iths++) {
							/* for each clear sky sun position, init each window node total luminance to its initial luminance */
							bldg_ptr->zone[iz]->surf[is]->wndo[iw]->skyclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_skyclum[inode][iphs][iths];
							bldg_ptr->zone[iz]->surf[is]->wndo[iw]->sunclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_sunclum[inode][iphs][iths];
						}
					}
				}
			}
		}
		/* go through desired number of iterations */
		for (iter=0; iter<niterate; iter++) {
			/* for each surface in this zone */
			for (is=0; is<bldg_ptr->zone[iz]->nsurfs; is++) {
				/* for each window in this surface */
				for (iw=0; iw<bldg_ptr->zone[iz]->surf[is]->nwndos; iw++) {
					/* get library index of current window glass type */
					igt = lib_index(lib_ptr,"glass",bldg_ptr->zone[iz]->surf[is]->wndo[iw]->glass_type);
					/* if invalid glass type, continue */
					if (igt < 0) continue;
					/* if window inside reflectance is small, its contribution */
					/* is neglected, for computer efficiency */
					if (lib_ptr->glass[igt]->inside_refl <= 0.15) continue;
					frac = lib_ptr->glass[igt]->inside_refl / PI;
					/* call window interreflection routine to loop through other surfaces */
					/* in this zone and interreflect between this window */
					wndo_interreflect(bldg_ptr,sun_ptr,iz,is,iw,frac);
				}
				/* now, for this surface itself - */
				/* if surface inside reflectance is small, its contribution */
				/* is neglected, for computer efficiency */
				if (bldg_ptr->zone[iz]->surf[is]->vis_refl <= 0.15) continue;
				frac = bldg_ptr->zone[iz]->surf[is]->vis_refl / PI;
				/* call surface interreflection routine to loop through other surfaces */
				/* in this zone and interreflect to current surface */
				surf_interreflect(bldg_ptr,sun_ptr,iz,is,frac);
			}
		}
		/* calculate totl illumination for ref_pts due to initial direct and interreflected daylight */
        int iRefptIllumRetVal;
		if ((iRefptIllumRetVal = refpt_total_illum(bldg_ptr,sun_ptr,iz)) < 0) {
            // If errors were detected then return now, else register warnings and continue processing
            if (iRefptIllumRetVal != -10) {
				*pofdmpfile << "ERROR: DElight Bad return from refpt_total_illum()\n";
				return(-1);
            }
            else {
                iReturnVal = -10;
            }
        }
	}

	return(iReturnVal);
}

/************************** subroutine surf_interreflect *************************/
/* Loops through all other surfaces in current zone to interreflect */
/* light with current surface. */
/* Based on Superlite conventions. */
/* cfs modification */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************** subroutine surf_interreflect *************************/
int surf_interreflect(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int iz,				/* current zone index */
	int isurf,			/* current surface index */
	double frac)		/* surface reflectance divided by PI */
{
	int inode;					/* current surface node loop index */
	int icoord;					/* node coordinate loop index */
	int iphs, iths;				/* sun position loop indexes */
	int jsurf, jnode;		/* loop indexes for other surfaces in current zone */
	double scb1, scb2, ssq;	/* temp calc vars */
	int icos1, icos2;				/* temp calc vars */
	double yyy[NCOORDS];	/* temp coordinate calc var */
	double fij;	/* configuration factor between nodes on surfs i and j */
	double delf_overcast[MAX_SURF_NODES];
	double delf_skyclear[MAX_SURF_NODES][NPHS][NTHS];	/* temp accumulators for reflected light */
	double delf_sunclear[MAX_SURF_NODES][NPHS][NTHS];	/* temp accumulators for reflected light */

	/* init accumulators for each node on current surface */
	for (inode=0; inode<bldg_ptr->zone[iz]->surf[isurf]->nnodes; inode++) {
		/* for overcast sky condition */
		delf_overcast[inode] = 0.;
		/* for each Sun Position Altitude */
		for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
			/* for each Sun Position Azimuth */
			for (iths=0; iths<sun_ptr->nths; iths++) {
				/* for each clear sky sun position */
				delf_skyclear[inode][iphs][iths] = 0.;
				delf_sunclear[inode][iphs][iths] = 0.;
			}
		}
	}
	/* for each non-window surface in current zone */
	/* Note - the present assumption is that there are */
	/* no internal obstructions. */
	for (jsurf=0; jsurf<bldg_ptr->zone[iz]->nsurfs; jsurf++) {
		/* skip current surface */
		if (jsurf == isurf) continue;
		/* for each node on current surface */
		for (inode=0; inode<bldg_ptr->zone[iz]->surf[isurf]->nnodes; inode++) {
			/* for each node on other (reflecting) surface */
			for (jnode=0; jnode<bldg_ptr->zone[iz]->surf[jsurf]->nnodes; jnode++) {
				/* calc configuration (form) factor fij */
				scb1 = 0.;
				scb2 = 0.;
				ssq = 0.;
				for (icoord=0; icoord<NCOORDS; icoord++) {
					yyy[icoord] = bldg_ptr->zone[iz]->surf[jsurf]->node[jnode][icoord] - bldg_ptr->zone[iz]->surf[isurf]->node[inode][icoord];
					/* Note: yyy[Y] sign is changed to account for differences in */
					/* slite and doe2 coordinate systems */
					// RJH - removed 1/9/2004 after correcting direction cosines to DOE2 coordinate system
					//if (icoord == 1) yyy[icoord] = -(yyy[icoord]);
					scb1 += yyy[icoord] * bldg_ptr->zone[iz]->surf[isurf]->dircos[icoord+6];
					scb2 -= yyy[icoord] * bldg_ptr->zone[iz]->surf[jsurf]->dircos[icoord+6];
					ssq += yyy[icoord] * yyy[icoord];
				}
				icos1 = (int)(1.0 + scb1 / (1.0 + ssq));
				icos2 = (int)(1.0 + scb2 / (1.0 + ssq));
				fij = scb1 * scb2 / (ssq * ssq) * bldg_ptr->zone[iz]->surf[jsurf]->node_areas[jnode] * icos1 * icos2;
				fij = fij / (1.0 + 0.6 * fij * fij);
				/* for overcast sky condition, accumulate reflected light from node on reflecting surface */
				delf_overcast[inode] += fij * bldg_ptr->zone[iz]->surf[jsurf]->skyolum[jnode];

// rjh debug
//*pofdmpfile << "Configuration Factor: fij = " << fij << " isurf = " << isurf << " inode = " << inode << " jsurf = " << jsurf << " jnode = " << jnode << "\n";

				/* for each Sun Position Altitude */
				for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
					/* for each Sun Position Azimuth */
					for (iths=0; iths<sun_ptr->nths; iths++) {
						/* for each clear sky sun position, accumulate reflected light from node on reflecting surface */
						delf_skyclear[inode][iphs][iths] += fij * bldg_ptr->zone[iz]->surf[jsurf]->skyclum[jnode][iphs][iths];
						delf_sunclear[inode][iphs][iths] += fij * bldg_ptr->zone[iz]->surf[jsurf]->sunclum[jnode][iphs][iths];
					}
				}
			}
		}
	}

	/* iteration finished for current surface, */
	/* improve values for total node luminance for each node on current surface */
	for (inode=0; inode<bldg_ptr->zone[iz]->surf[isurf]->nnodes; inode++) {
		/* for overcast sky condition */
		bldg_ptr->zone[iz]->surf[isurf]->skyolum[inode] = bldg_ptr->zone[iz]->surf[isurf]->direct_skyolum[inode] + frac * delf_overcast[inode];
		/* for each Sun Position Altitude */
		for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
			/* for each Sun Position Azimuth */
			for (iths=0; iths<sun_ptr->nths; iths++) {
				/* for each clear sky sun position */
				bldg_ptr->zone[iz]->surf[isurf]->skyclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[isurf]->direct_skyclum[inode][iphs][iths] + frac * delf_skyclear[inode][iphs][iths];
				bldg_ptr->zone[iz]->surf[isurf]->sunclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[isurf]->direct_sunclum[inode][iphs][iths] + frac * delf_sunclear[inode][iphs][iths];
			}
		}
	}

	return(0);
}

/************************** subroutine wndo_interreflect *************************/
/* Loops through all other surfaces in current zone to interreflect */
/* light with current window. */
/* Based on Superlite conventions. */
/* Note that Superlite does not interreflect light between pairs of windows. */
/* cfs modification */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************** subroutine wndo_interreflect *************************/
int wndo_interreflect(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int iz,				/* current zone index */
	int is,				/* current surface index */
	int iw,				/* current window index */
	double frac)		/* surface reflectance divided by PI */
{
	int inode;					/* current window node loop index */
	int icoord;					/* node coordinate loop index */
	int iphs, iths;				/* sun position loop indexes */
	int jsurf, jnode;		/* loop indexes for other surfaces in current zone */
	double scb1, scb2, ssq, icos1, icos2;	/* temp calc vars */
	double yyy[NCOORDS];	/* temp coordinate calc var */
	double fij;	/* configuration factor between nodes on wndo i and surfs j */
	double delf_overcast[MAX_SURF_NODES];
	double delf_skyclear[MAX_SURF_NODES][NPHS][NTHS];	/* temp accumulators for reflected light */
	double delf_sunclear[MAX_SURF_NODES][NPHS][NTHS];	/* temp accumulators for reflected light */

	/* init accumulators for each node on current window */
	for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->wndo[iw]->nnodes; inode++) {
		/* for overcast sky condition */
		delf_overcast[inode] = 0.;
		/* for each Sun Position Altitude */
		for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
			/* for each Sun Position Azimuth */
			for (iths=0; iths<sun_ptr->nths; iths++) {
				/* for each clear sky sun position */
				delf_skyclear[inode][iphs][iths] = 0.;
				delf_sunclear[inode][iphs][iths] = 0.;
			}
		}
	}

	/* for each non-window surface in current zone */
	/* Note - the present assumption is that there are */
	/* no internal obstructions. */
	for (jsurf=0; jsurf<bldg_ptr->zone[iz]->nsurfs; jsurf++) {
		/* skip current window host surface */
		if (jsurf == is) continue;
		/* for each node on current window */
		for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->wndo[iw]->nnodes; inode++) {
			/* for each node on other surface */
			for (jnode=0; jnode<bldg_ptr->zone[iz]->surf[jsurf]->nnodes; jnode++) {
				/* calc configuration (form) factor */
				scb1 = 0.;
				scb2 = 0.;
				ssq = 0.;
				for (icoord=0; icoord<NCOORDS; icoord++) {
					yyy[icoord] = bldg_ptr->zone[iz]->surf[jsurf]->node[jnode][icoord] - bldg_ptr->zone[iz]->surf[is]->wndo[iw]->node[inode][icoord];
					/* Note: yyy[Y] sign is changed to account for differences in */
					/* slite and doe2 coordinate systems */
					// RJH - removed 1/9/2004 after correcting direction cosines to DOE2 coordinate system
					//if (icoord == 1) yyy[icoord] = -(yyy[icoord]);
					/* note - wndo direction cosine values are same as host surface */
					scb1 += yyy[icoord] * bldg_ptr->zone[iz]->surf[is]->dircos[icoord+6];
					scb2 -= yyy[icoord] * bldg_ptr->zone[iz]->surf[jsurf]->dircos[icoord+6];
					ssq += yyy[icoord] * yyy[icoord];
				}
				icos1 = 1.0 + scb1 / (1.0 + ssq);
				icos2 = 1.0 + scb2 / (1.0 + ssq);
				fij = scb1 * scb2 / (ssq * ssq) * bldg_ptr->zone[iz]->surf[jsurf]->node_areas[jnode] * icos1 * icos2;
				fij = fij / (1.0 + 0.6 * fij * fij);

				/* for overcast sky condition, accumulate reflected light from node on reflecting surface */
				delf_overcast[inode] += fij * bldg_ptr->zone[iz]->surf[jsurf]->skyolum[jnode];
				/* for each Sun Position Altitude */
				for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
					/* for each Sun Position Azimuth */
					for (iths=0; iths<sun_ptr->nths; iths++) {
						/* for each clear sky sun position, accumulate reflected light from node on reflecting surface */
						delf_skyclear[inode][iphs][iths] += fij * bldg_ptr->zone[iz]->surf[jsurf]->skyclum[jnode][iphs][iths];
						delf_sunclear[inode][iphs][iths] += fij * bldg_ptr->zone[iz]->surf[jsurf]->sunclum[jnode][iphs][iths];
					}
				}
			}
		}
	}

	/* iteration finished for current window, */
	/* improve values for total node luminance for each node on current window */
	for (inode=0; inode<bldg_ptr->zone[iz]->surf[is]->nnodes; inode++) {
		/* for overcast sky condition */
		bldg_ptr->zone[iz]->surf[is]->wndo[iw]->skyolum[inode] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_skyolum[inode] + frac * delf_overcast[inode];
		/* for each Sun Position Altitude */
		for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
			/* for each Sun Position Azimuth */
			for (iths=0; iths<sun_ptr->nths; iths++) {
				/* for each clear sky sun position */
				bldg_ptr->zone[iz]->surf[is]->wndo[iw]->skyclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_skyclum[inode][iphs][iths] + frac * delf_skyclear[inode][iphs][iths];
				bldg_ptr->zone[iz]->surf[is]->wndo[iw]->sunclum[inode][iphs][iths] = bldg_ptr->zone[iz]->surf[is]->wndo[iw]->direct_sunclum[inode][iphs][iths] + frac * delf_sunclear[inode][iphs][iths];
			}
		}
	}

	return(0);
}

/************************* subroutine refpt_total_illum ************************/
/* Loops through all reference points in current zone to calculate */
/* total illuminance. */
/* Based on Superlite conventions. */
/****************************************************************************/
/* C Language Implementation of Superlite Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/************************* subroutine refpt_total_illum ************************/
int refpt_total_illum(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	SUN_DATA *sun_ptr,	/* pointer to sun data structure */
	int iz)				/* current zone index */
{
	double refpt_dircos[NDC];	/* ref_pt direction cosine values (slite) */
	int irp;			/* current reference point index */
	int jsurf, jnode;	/* loop indexes for non-wndo surfaces in current zone */
	int icoord;	/* coordinate loop index */
	int iphs, iths;			/* sun position loop indexes */
	double scb1, scb2, ssq;	/* tmp calc vars */
	int icos1, icos2;				/* tmp calc vars */
	double yyy[NCOORDS];	/* tmp coordinate calc var */
	double fij;	/* configuration factor between ref_pt and node on surf j */

    // Init return value
    int iReturnVal = 0;

	/* Algorithms are the same as for internal reflections, but no iteration. */
	/* Because of their importance, configuration factors for close nodes */
	/* are calculated more accurately. */

	/* Note - the present assumption is that there are */
	/* no internal obstructions. */

	/* Note - these algorithms assume that all reference points are on */
	/* a horizontal plane facing upward (DOE2 outward normal downward) */

	/* calc direction cosine values */
	/* Note: see calc_dircos() for complete logic for arbitrary surfaces */
	// RJH - 8/24/03 - Only Z-axis direction cosines are needed for ref pts
	// and they are always 0,0,1 since ref pts are assumed to be on horizontal surface facing upward.
	for (int idircos=0; idircos<NDC; idircos++)
	{
		refpt_dircos[idircos] = 0.0;
	}
	refpt_dircos[8] = 1.0;

	/* for each surface in this zone */
	for (jsurf=0; jsurf<bldg_ptr->zone[iz]->nsurfs; jsurf++) {
		/* for each ref_pt in this zone */
		for (irp=0; irp<bldg_ptr->zone[iz]->nrefpts; irp++) {
			/* for each node on surface */
			for (jnode=0; jnode<bldg_ptr->zone[iz]->surf[jsurf]->nnodes; jnode++) {
				/* calc configuration (form) factor */
				scb1 = 0.;
				scb2 = 0.;
				ssq = 0.;
				for (icoord=0; icoord<NCOORDS; icoord++) {
					yyy[icoord] = bldg_ptr->zone[iz]->surf[jsurf]->node[jnode][icoord] - bldg_ptr->zone[iz]->ref_pt[irp]->bs[icoord];
					/* Note: yyy[Y] sign is changed to account for differences in */
					/* slite and doe2 coordinate systems */
					// RJH - removed 1/9/2004 after correcting direction cosines to DOE2 coordinate system
					//if (icoord == 1) yyy[icoord] = -(yyy[icoord]);
					scb1 += yyy[icoord] * refpt_dircos[icoord+6];
					scb2 -= yyy[icoord] * bldg_ptr->zone[iz]->surf[jsurf]->dircos[icoord+6];
					ssq += yyy[icoord] * yyy[icoord];
				}
				icos1 = (int)(1.0 + scb1 / (1.0 + ssq));
				icos2 = (int)(1.0 + scb2 / (1.0 + ssq));
				fij = scb1 * scb2 / (ssq * ssq) * bldg_ptr->zone[iz]->surf[jsurf]->node_areas[jnode] * icos1 * icos2;
				/* if this is a close node (relative to node area) */
				/* then calculate config factor more accurately */

                // Change to Output warning message - RJH 2/25/04
                // Change to Output ERROR message and return for termination - RJH 3/11/05
				// Ignore - RJH 2/12/09
				if ((bldg_ptr->zone[iz]->surf[jsurf]->node_areas[jnode] / ssq) >= 1.0) {
//					*pofdmpfile << "ERROR: DElight inaccurate daylight illuminance interreflection calculation for zone " <<bldg_ptr->zone[iz]->name << "\n";
//					*pofdmpfile << "ERROR: Node area too large relative to distance between reference point [" << bldg_ptr->zone[iz]->ref_pt[irp]->name << "] and node [" << jnode << "] for Surface " <<bldg_ptr->zone[iz]->surf[jsurf]->name << "\n";
//					*pofdmpfile << "ERROR: Try moving Reference Point away from Surface or reducing Maximum Node Gridding Area for this zone.\n";
//					*pofdmpfile << "ERROR: This error can also result from too large differences in areas between surfaces in zone.\n";
// Warning return value  iReturnVal = -10;
//	                return(-1); // ERROR return value
				}

				/* for overcast sky condition, accumulate reflected light from node on reflecting surface */
				bldg_ptr->zone[iz]->ref_pt[irp]->delf_overcast += fij * bldg_ptr->zone[iz]->surf[jsurf]->skyolum[jnode];
				/* for each Sun Position Altitude */
				for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
					/* for each Sun Position Azimuth */
					for (iths=0; iths<sun_ptr->nths; iths++) {
						/* for each clear sky sun position, accumulate reflected light from node on reflecting surface */
						bldg_ptr->zone[iz]->ref_pt[irp]->delf_skyclear[iphs][iths] += fij * bldg_ptr->zone[iz]->surf[jsurf]->skyclum[jnode][iphs][iths];
						bldg_ptr->zone[iz]->ref_pt[irp]->delf_sunclear[iphs][iths] += fij * bldg_ptr->zone[iz]->surf[jsurf]->sunclum[jnode][iphs][iths];
					}
				}
			}
		}
	}

	/* Add the internal-reflection contribution to the reference point */
	/* initial illumination due to direct distribution from the cfs. */

	/* for each ref_pt in this zone */
	for (irp=0; irp<bldg_ptr->zone[iz]->nrefpts; irp++) {
		/* for overcast sky condition, accumulate reflected light from node on reflecting surface */
		bldg_ptr->zone[iz]->ref_pt[irp]->skyoillum = bldg_ptr->zone[iz]->ref_pt[irp]->direct_skyoillum + bldg_ptr->zone[iz]->ref_pt[irp]->delf_overcast / PI;
		/* for each Sun Position Altitude */
		for (iphs=0; iphs<sun_ptr->nphs; iphs++) {
			/* for each Sun Position Azimuth */
			for (iths=0; iths<sun_ptr->nths; iths++) {
				/* for each clear sky sun position, accumulate reflected light from node on reflecting surface */
				bldg_ptr->zone[iz]->ref_pt[irp]->skycillum[iphs][iths] = bldg_ptr->zone[iz]->ref_pt[irp]->direct_skycillum[iphs][iths] + bldg_ptr->zone[iz]->ref_pt[irp]->delf_skyclear[iphs][iths] / PI;
				bldg_ptr->zone[iz]->ref_pt[irp]->suncillum[iphs][iths] = bldg_ptr->zone[iz]->ref_pt[irp]->direct_suncillum[iphs][iths] + bldg_ptr->zone[iz]->ref_pt[irp]->delf_sunclear[iphs][iths] / PI;
			}
		}
	}

	return(iReturnVal);
}
