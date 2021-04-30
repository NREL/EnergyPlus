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
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <vector>
#include <map>
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
#include "EPlus_ECM.H"

/******************************** subroutine CalcInterpolationVars *******************************/
// Called from EPlus_DElight.cpp
/* Calculates displacement ratios and boundary indexes */
/* for use in daylight factor interpolation. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine CalcInterpolationVars *******************************/
int CalcInterpolationVars(
	BLDG *bldg_ptr,		/* building data structure pointer */
	double dSunDirCos[NCOORDS],	/* direction cosines of current sun position */
	double phsmin,		/* minimum sun altitude used in dcof() */
	double phsmax,		/* maximum sun altitude used in dcof() */
	double phsdel,		/* sun altitude increment used in dcof() */
	double thsmin,		/* minimum sun azimuth used in dcof() */
	double thsmax,		/* maximum sun azimuth used in dcof() */
	double thsdel,		/* sun azimuth increment used in dcof() */
	int *iphs_ptr,		/* sun position altitude interpolation index */
	int *iths_ptr,		/* sun position azimuth interpolation index */
	double *phratio_ptr,	/* sun position altitude interpolation displacement ratio */
	double *thratio_ptr)	/* sun position azimuth interpolation displacement ratio */
{
	double phsun, thsun, phsund, thsund;	/* sun alt and azm (radians and degrees) */
	double phs, ths;			/* sun index vars */

	/* Calc current sun alt and azm in degrees from its direction cosines */
	phsun = 1.5708 - acos(dSunDirCos[2]);
	phsund = phsun / DTOR;
	thsund = atan2(dSunDirCos[1],dSunDirCos[0]) / DTOR;
	/* Convert thsund to coord sys in which S=0 and E=90 */
	thsund += 90.0 - bldg_ptr->azm / DTOR;
	/* Restrict thsund to -180 to 180 interval */
	if (thsund > -180.0) thsund += 360.;
	if (thsund > 180.0) thsund -= 360.0 * (1.0 + floor(thsund/540.0));
	thsun = thsund * DTOR;

	/* Calc alt and azm interpolation indexes and displacement ratios */
	/* Restrict alt and azm to dcof() bounds */
	if (phsund < phsmin) phsund = phsmin;
	if (phsund > phsmax) phsund = phsmax;
	if (thsund < thsmin) thsund = thsmin;
	if (thsund > thsmax) thsund = thsmax;
	/* alt and azm lower interpolation indexes */
	phs = (phsund - phsmin) / phsdel;
	ths = (thsund - thsmin) / thsdel;
	*iphs_ptr = (int)floor(phs);
	*iths_ptr = (int)floor(ths);
	/* alt and azm interpolation displacement ratios */
	*phratio_ptr = phs - (double)(*iphs_ptr);
	*thratio_ptr = ths - (double)(*iths_ptr);

	return(0);
}

/******************************** subroutine CalcZoneInteriorIllum *******************************/
// Called from EPlus_DElight.cpp
/* Calculates interior daylight illuminance at each reference point in a daylit zone, */
// For a given time / sun position
/* Assumes no window shades. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/******************************** subroutine CalcZoneInteriorIllum *******************************/
int	CalcZoneInteriorIllum(
	ZONE *zone_ptr,			/* bldg->zone data structure pointer */
	double dHISKF,						/* Exterior horizontal illuminance from sky (lum/m^2) */
	double dHISUNF,						/* Exterior horizontal beam illuminance (lum/m^2) */
	double dCloudFraction,				/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	int iphs,		/* sun altitude interpolation lower bound index */
	int iths,		/* sun azimuth interpolation lower bound index */
	double phratio,	/* sun altitude interpolation displacement ratio */
	double thratio)	/* sun azimuth interpolation displacement ratio */
{
	double hisunf;	/* clear sky horiz illum sun component */
	double chiskf;	/* clear sky horiz illum sky component */
	double ohiskf;	/* overcast sky horiz illum sky component */
	int irp;				/* ref pt loop index */
	int ip_lo, ip_hi;		/* sun altitude low and high interpolation indexes */
	int it_lo, it_hi;		/* sun azimuth low and high interpolation indexes */
	double lower, upper;		/* temp interpolation lower and upper values */
	double skyfac, sunfac;	/* clear sky interpolated factors */

	// Calculate required exterior horiz illum components from EPlus given components
	double etacld;	// weighting factor for clear and overcast sky illum components
	if (dCloudFraction > 0.2) etacld = 1.0 - (dCloudFraction - 0.2) * 1.25;
	else etacld = 1.0;

	// clear sky horiz illum sky component
	chiskf = dHISKF * etacld;

	// overcast sky horiz illum sky component
	ohiskf = dHISKF * (1.0 - etacld);

	// clear sky horiz illum sun component
	hisunf = dHISUNF;

	/* Set low and high alt and azm indexes */
	ip_lo = iphs;
	if (iphs != (NPHS-1)) ip_hi = iphs + 1;
	else ip_hi = iphs;
	it_lo = iths;
	if (iths != (NTHS-1)) it_hi = iths + 1;
	else it_hi = iths;

	// Calc interior illum for each RefPt in Zone by interpolation
	for (irp=0; irp<zone_ptr->nrefpts; irp++) {
		/* Interpolate clear sky daylight factors */
		upper = (zone_ptr->ref_pt[irp]->dfsky[ip_hi][it_hi] - zone_ptr->ref_pt[irp]->dfsky[ip_hi][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsky[ip_hi][it_lo];
		lower = (zone_ptr->ref_pt[irp]->dfsky[ip_lo][it_hi] - zone_ptr->ref_pt[irp]->dfsky[ip_lo][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsky[ip_lo][it_lo];
		skyfac = (upper - lower) * phratio + lower;
		upper = (zone_ptr->ref_pt[irp]->dfsun[ip_hi][it_hi] - zone_ptr->ref_pt[irp]->dfsun[ip_hi][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsun[ip_hi][it_lo];
		lower = (zone_ptr->ref_pt[irp]->dfsun[ip_lo][it_hi] - zone_ptr->ref_pt[irp]->dfsun[ip_lo][it_lo]) * thratio + zone_ptr->ref_pt[irp]->dfsun[ip_lo][it_lo];
		sunfac = (upper - lower) * phratio + lower;

		/* Multiply daylight factors by appropriate exterior horizontal illuminance components */
		zone_ptr->ref_pt[irp]->daylight = sunfac * hisunf + skyfac * chiskf + zone_ptr->ref_pt[irp]->dfskyo * ohiskf;
	}

	return(0);
}
