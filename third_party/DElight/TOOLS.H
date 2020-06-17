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
double POLYF(
	double dCosI,		/* cosine of the angle of incidence */
	double EPCoef[6]);	/* EnergyPlus coefs of angular transmission */

int lib_index(
	LIB *lib_ptr,						/* pointer to library */
	const char component[MAX_CHAR_UNAME+1],	/* library component category */
	char uname[MAX_CHAR_UNAME+1]);

int free_bldg(
	BLDG *bldg_ptr);	/* pointer to building data */

int free_lib(
	LIB *lib_ptr);	/* pointer to library data */

int get_sched(
	BLDG *bldg_ptr,	/* pointer to bldg structure */
	int dayofyr,	/* current sequential day of year */
	int dayofweek);	/* current day of week (1=Mon to 7=Sun) */

int calc_sched_days(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	RUN_DATA *run_ptr);	/* pointer to run period data structure */

int julian_date(
	int *dateJul_ptr,		/* pointer to Julian date */
	int month,
	int day,
	int year);

int get_day_of_week(
	int *dow_ptr,		/* pointer to day of week (1=Mon to 7=Sun) */
	RUN_DATA *run_ptr);	/* pointer to runtime data structure */

double ran0(
	int *idum);

int init_monlength(
	int monlength[12],	/* number of days in each month array */
	int iFebDays);		/* number of days in February */

double fit4(
	double dcosb,	// cos of angle of incidence
	double dTvFit1,	// coef1
	double dTvFit2);// coef2

char *str_rmblnk(
	char *s1);

char *str_blnk2undr(
	char *s1);

