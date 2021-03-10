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

int	dillum(
	double cloud_fraction,	/* fraction of sky covered by clouds (0.0=clear 1.0=overcast) */
	BLDG *bldg_ptr,			/* building data structure pointer */
	SUN_DATA *sun_ptr,		/* pointer to sun data structure */
	RUN_DATA *run_ptr,		/* pointer to runtime data structure */
	int wx_flag,			/* weather availability flag */
	FILE *wxfile_ptr,		/* TMY2 weather file pointer */
	ofstream* pofdmpfile);	/* ptr to dump file */

int	davail(
	double *chilsk_ptr,	/* clear sky horiz illum sky component */
	double *chilsu_ptr,	/* clear sky horiz illum sun component */
	double *ohilsk_ptr,	/* overcast sky horiz illum sky component */
	double *cdirlw_ptr,	/* luminous efficacy for direct solar radiation from clear sky */
	double *cdiflw_ptr,	/* luminous efficacy for diffuse radiation from clear sky */
	double *odiflw_ptr,	/* luminous efficacy for diffuse radiation from overcast sky */
	int imon,			/* current month */
	double phsun,		/* sun altitude */
	double thsun,		/* sun azimuth */
	double solic[MONTHS],/* extraterrestrial illum for first day of each month */
	BLDG *bldg_ptr,		/* building data structure pointer */
	ofstream* pofdmpfile);	/* ptr to dump file */

int	dlumef(
	double *cdirlw_ptr,	/* luminous efficacy for direct solar radiation from clear sky */
	double *cdiflw_ptr,	/* luminous efficacy for diffuse radiation from clear sky */
	double *odiflw_ptr,	/* luminous efficacy for diffuse radiation from overcast sky */
	int imon,			/* current month */
	double phsun,		/* sun altitude */
	BLDG *bldg_ptr);	/* building data structure pointer */

int	dplumef(
	double* pdirlw_ptr,	/* Perez luminous efficacy for direct solar radiation from clear sky */
	double* pdiflw_ptr,	/* Perez luminous efficacy for diffuse radiation from sky */
	double bscc,		/* diffuse horiz radiation from measured data (Btu/ft2-h) */
	double rdncc,		/* measured direct solar radiation (Btu/ft2-h) */
	double phsun,		/* sun altitude (radians) */
	double solic[MONTHS],/* extraterrestrial illum for first day of each month */
	int imon,			/* current month */
	SUN2_DATA *sun2_ptr,/* pointer to sun2 data structure */
	BLDG *bldg_ptr,		/* building data structure pointer */
	ofstream* pofdmpfile);	/* ptr to dump file */

int init_avail(
	double chilsk[HOURS],	/* clear sky horiz illum sky component */
	double chilsu[HOURS],	/* clear sky horiz illum sun component */
	double ohilsk[HOURS],	/* overcast sky horiz illum sky component */
	double cdirlw[HOURS],	/* luminous efficacy for direct solar radiation from clear sky */
	double cdiflw[HOURS],	/* luminous efficacy for diffuse radiation from clear sky */
	double odiflw[HOURS]);	/* luminous efficacy for diffuse radiation from overcast sky */

int	dextil(
	double *hisunf_ptr,	/* current hour clear sky horiz illum sun component */
	double *chiskf_ptr,	/* current hour clear sky horiz illum sky component */
	double *ohiskf_ptr,	/* current hour overcast sky horiz illum sky component */
	int wx_flag,	/* weather data availability flag (1=avail) */
	double chilsu,	/* current hour clear sky horiz illum sun component from davail() */
	double chilsk,	/* current hour clear sky horiz illum sky component from davail() */
	double ohilsk,	/* current hour overcast sky horiz illum sky component from davail() */
	//double cdirlw,	/* CIE luminous efficacy for direct solar radiation from clear sky */
	//double cdiflw,	/* CIE luminous efficacy for diffuse radiation from clear sky */
	//double odiflw,	/* CIE luminous efficacy for diffuse radiation from overcast sky */
	double phsun,		/* sun altitude (radians) */
	double solic[MONTHS],/* extraterrestrial illum for first day of each month */
	int imon,			/* current month */
	BLDG *bldg_ptr,		/* building data structure pointer */
	SUN2_DATA *sun2_ptr,/* pointer to sun2 data structure */
	ofstream* pofdmpfile);	/* ptr to dump file */

int	dintil(
	ZONE *zone_ptr,	/* bldg->zone data structure pointer */
	int imon,		/* current month */
	int ihr,		/* current hour */
	double hisunf,	/* current hour clear sky horiz illum sun component */
	double chiskf,	/* current hour clear sky horiz illum sky component */
	double ohiskf,	/* current hour overcast sky horiz illum sky component */
	int iphs,		/* sun altitude interpolation lower bound index */
	int iths,		/* sun azimuth interpolation lower bound index */
	double phratio,	/* sun altitude interpolation displacement ratio */
	double thratio);	/* sun azimuth interpolation displacement ratio */

int calc_sun(
	double *phsun_ptr,	/* sun position altitude */
	double *thsun_ptr,	/* sun position azimuth */
	double *phratio_ptr,	/* sun position altitude interpolation displacement ratio */
	double *thratio_ptr,	/* sun position azimuth interpolation displacement ratio */
	int *iphs_ptr,		/* sun position altitude interpolation index */
	int *iths_ptr,		/* sun position azimuth interpolation index */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2 data structure */
	double phsmin,		/* minimum sun altitude used in dcof() */
	double phsmax,		/* maximum sun altitude used in dcof() */
	double phsdel,		/* sun altitude increment used in dcof() */
	double thsmin,		/* minimum sun azimuth used in dcof() */
	double thsmax,		/* maximum sun azimuth used in dcof() */
	double thsdel,		/* sun azimuth increment used in dcof() */
	BLDG *bldg_ptr);		/* building data structure pointer */

int dltsys(
	ZONE *zone_ptr,			/* bldg->zone data structure pointer */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2 data structure */
	ofstream* pofdmpfile);	/* ptr to dump file */
