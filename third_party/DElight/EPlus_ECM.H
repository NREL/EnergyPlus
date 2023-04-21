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
	double *thratio_ptr);	/* sun position azimuth interpolation displacement ratio */

int	CalcZoneInteriorIllum(
	ZONE *zone_ptr,			/* bldg->zone data structure pointer */
	double dHISKF,						/* Exterior horizontal illuminance from sky (lum/m^2) */
	double dHISUNF,						/* Exterior horizontal beam illuminance (lum/m^2) */
	double dCloudFraction,				/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	int iphs,		/* sun altitude interpolation lower bound index */
	int iths,		/* sun azimuth interpolation lower bound index */
	double phratio,	/* sun altitude interpolation displacement ratio */
	double thratio);	/* sun azimuth interpolation displacement ratio */
