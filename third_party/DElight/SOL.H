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

int	dzenlm(
	double *zenl,			/* clear sky zenith luminance */
	double *tfac,			/* turbidity factor */
	int mon,				/* month of calculation (0 to 11) */
	BLDG *bldg_ptr,			/* pointer to bldg structure (atmos turb & mois arrays )*/
	double phsun);			/* sun altitude (radians) */
 
double dskylu(
	int skytype,		/* sky type (0=clear, 1=overcast) */
	double thsky,		/* azimuth of sky element (radians) */
	double phsky,		/* altitude of sky element (radians) */
	double thsun,		/* azimuth of sun (radians) */
	double phsun,		/* altitude of sun (radians) */
	double zenl);			/* clear sky zenith luminance */

int dsolic(
	double solic[MONTHS]); /* extraterrestrial irradiance for 1st of each month (0 to 11) */

double dnsol(
	double solic[MONTHS],/* extraterrestrial irradiance for 1st of every month */
	BLDG *bldg_ptr,		/* bldg pointer */
	int mon,			/* month of calc */
	double phsun,		/* sun altitude (rad) */
	double tfac,			/* turbidity factor calculated by dzenlm */
	ofstream* pofdmpfile);	/* ptr to dump file */

int dhill(
	double *hlskyc_ptr,	/* horiz illum sky component (clear sky) for given sun alt position */
	double *hlsunc_ptr,	/* horiz illum sun component (clear sky) for given sun alt position */
	double *hlskyo_ptr,	/* horiz illum sky component (overcast sky) for given sun alt position */
	BLDG *bldg_ptr,	/* bldg structure pointer */
	int mon,		/* month of calculation */
	double phsun,	/* sun altitude (rad) */
	double thsun,	/* sun azimuth (rad) */
	double zenl,		/* zenith luminance for this month */
	double tfac,		/* turbidity factor for this month */
	double solic[MONTHS],	/* extraterrestrial illum for each month */
	ofstream* pofdmpfile);	/* ptr to dump file */

int dshdlu(
	BLDG *bldg_ptr,			/* pointer to bldg structure */
	double phsun,			/* sun position altitude */
	double thsun,			/* sun position azimuth */
	int iphs,				/* sun position altitude index */
	int iths,				/* sun position azimuth index */
	double solic[MONTHS],	/* ext irrad for 1st of each month */
	double tfac,				/* turbidity factor calculated by dzenlm() */
	double zenl,				/* zenith luminance calculated by dzenlm() */
	double hlskyc,			/* horiz lum from clear sky calculated by dhill() */
	double hlskyo,			/* horiz lum from overcast sky calculated by dhill() */
	double hlsunc,			/* horiz lum from clear sun calculated by dhill() */
	ofstream* pofdmpfile);	/* ptr to dump file */

int sun1(
	int dayofyr,			/* sequential day of year */
	SUN1_DATA *sun1_ptr,	/* pointer to sun1_data structure */
	BLDG *bldg_ptr);			/* pointer to bldg structure */

int sun2(
	int imon,				/* month of the year */
	int iday,				/* day of the month */
	int ihr,				/* hour of the day */
	SUN1_DATA *sun1_ptr,	/* pointer to sun1_data structure */
	SUN2_DATA *sun2_ptr,	/* pointer to sun2_data structure */
	BLDG *bldg_ptr,			/* pointer to bldg structure */
	int wx_flag,			/* weather availability flag */
	FILE *wxfile_ptr);		/* TMY2 weather file pointer */
