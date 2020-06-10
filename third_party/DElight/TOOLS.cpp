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
#include <ctime>
#include <cstdlib>
#include <algorithm> // for min/max

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
#include "TOOLS.H"

/****************************** subroutine POLYF *****************************/
// PURPOSE OF THIS FUNCTION:
// Evaluates glazing beam transmittance or absorptance of the form
// A(1)*X + A(2)*X^2 + A(3)*X^3 + A(4)*X^4 + A(5)*X^5 + A(6)*X^6
// where X is the cosine of the angle of incidence (0.0 to 1.0)
/****************************************************************************/
/* AUTHOR         Fred Winkelmann */
/* DATE WRITTEN   February 1999 */
/* DATE MODIFIED  October 1999, FW: change to 6th order polynomial over */
/*					entire incidence angle range */
/****************************** subroutine POLYF *****************************/
double POLYF(
	double dCosI,		/* cosine of the angle of incidence */
	double EPCoef[6])	/* EnergyPlus coefs of angular transmission */
{
	double transmittance;	// transmittance at angle of incidence

	if(dCosI < 0.0 || dCosI > 1.0)
	  transmittance = 0.0;
	else
	  transmittance = dCosI*(EPCoef[0]+dCosI*(EPCoef[1]+dCosI*(EPCoef[2]+dCosI*(EPCoef[3]+dCosI*(EPCoef[4]+dCosI*EPCoef[5])))));

	return(transmittance);
}

/****************************** subroutine lib_index *****************************/
/* Searches library of components for the specified component name. */
/* Returns index of located component. */
/* Returns value of -1 if no matching component was located. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine lib_index *****************************/
int lib_index(
	LIB *lib_ptr,						/* pointer to library */
	const char component[MAX_CHAR_UNAME+1],	/* library component category */
	char uname[MAX_CHAR_UNAME+1])
{
	int ii;

	if (strcmp(component,"glass") == 0) {
		for (ii=0; ii<MAX_LIB_COMPS; ii++)
			if (lib_ptr->glass[ii] != NULL)
				if (strcmp(uname,lib_ptr->glass[ii]->name) == 0)
					return (ii);
	}
	else if (strcmp(component,"wshade") == 0) {
		for (ii=0; ii<MAX_LIB_COMPS; ii++)
			if (lib_ptr->wshade[ii] != NULL) if (strcmp(uname,lib_ptr->wshade[ii]->name) == 0) return (ii);
	}
	return(-1);
}

/****************************** subroutine free_bldg *****************************/
/* Frees malloced memory used in bldg data structure. */
/* RJH 7/25/03 - malloc/free changed to new/delete */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine free_bldg *****************************/
int free_bldg(
	BLDG *bldg_ptr)	/* pointer to building data */
{
	int izone, isurf, iwndo, irp, ibshd, iltsch, izs;	/* loop indexes */

	for (izone=0; izone<MAX_BLDG_ZONES; izone++) {
		if (bldg_ptr->zone[izone] == NULL) continue;
		for (iltsch=0; iltsch<MAX_LT_SCHEDS; iltsch++) {
			if (bldg_ptr->zone[izone]->ltsch[iltsch] == NULL) continue;
			delete(bldg_ptr->zone[izone]->ltsch[iltsch]);
			bldg_ptr->zone[izone]->ltsch[iltsch] = NULL;
		}
		for (isurf=0; isurf<MAX_ZONE_SURFS; isurf++) {
			if (bldg_ptr->zone[izone]->surf[isurf] == NULL) continue;
			for (iwndo=0; iwndo<MAX_SURF_WNDOS; iwndo++) {
				if (bldg_ptr->zone[izone]->surf[isurf]->wndo[iwndo] == NULL) continue;
				delete(bldg_ptr->zone[izone]->surf[isurf]->wndo[iwndo]);
				bldg_ptr->zone[izone]->surf[isurf]->wndo[iwndo] = NULL;
			}
			delete(bldg_ptr->zone[izone]->surf[isurf]);
//			free(bldg_ptr->zone[izone]->surf[isurf]);
			bldg_ptr->zone[izone]->surf[isurf] = NULL;
		}
		for (izs=0; izs<MAX_ZONE_SHADES; izs++) {
			if (bldg_ptr->zone[izone]->zshade[izs] == NULL) continue;
			delete(bldg_ptr->zone[izone]->zshade[izs]);
			bldg_ptr->zone[izone]->zshade[izs] = NULL;
		}
		for (irp=0; irp<MAX_REF_PTS; irp++) {
			if (bldg_ptr->zone[izone]->ref_pt[irp] == NULL) continue;
			for (isurf=0; isurf<MAX_ZONE_SURFS; isurf++) {
				for (iwndo=0; iwndo<MAX_SURF_WNDOS; iwndo++) {
					if (bldg_ptr->zone[izone]->ref_pt[irp]->wlum[isurf][iwndo] == NULL) continue;
					delete(bldg_ptr->zone[izone]->ref_pt[irp]->wlum[isurf][iwndo]);
					bldg_ptr->zone[izone]->ref_pt[irp]->wlum[isurf][iwndo] = NULL;
				}
			}
			delete(bldg_ptr->zone[izone]->ref_pt[irp]);
			bldg_ptr->zone[izone]->ref_pt[irp] = NULL;
		}
		delete(bldg_ptr->zone[izone]);
		bldg_ptr->zone[izone] = NULL;
	}
	for (ibshd=0; ibshd<MAX_BLDG_SHADES; ibshd++) {
		if (bldg_ptr->bshade[ibshd] == NULL) continue;
		delete(bldg_ptr->bshade[ibshd]);
		bldg_ptr->bshade[ibshd] = NULL;
	}

	return(0);
}

/****************************** subroutine free_lib *****************************/
/* Frees malloced memory used in lib data structure. */
/* RJH 7/25/03 - malloc/free changed to new/delete */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine free_lib *****************************/
int free_lib(
	LIB *lib_ptr)	/* pointer to library data */
{
	int igt, iwshd;	/* loop indexes */

	for (igt=0; igt<MAX_LIB_COMPS; igt++) {
		if (lib_ptr->glass[igt] == NULL) continue;
		delete(lib_ptr->glass[igt]);
		lib_ptr->glass[igt] = NULL;
	}
	for (iwshd=0; iwshd<MAX_LIB_COMPS; iwshd++) {
		if (lib_ptr->wshade[iwshd] == NULL) continue;
		delete(lib_ptr->wshade[iwshd]);
		lib_ptr->wshade[iwshd] = NULL;
	}

	return(0);
}

/****************************** subroutine get_sched *****************************/
/* Determines lighting scehdule indexes for each zone for given day of year and */
/* day of week. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine get_sched *****************************/
int get_sched(
	BLDG *bldg_ptr,	/* pointer to bldg structure */
	int dayofyr,	/* current sequential day of year */
	int dayofweek)	/* current day of week (1=Mon to 7=Sun) */
{
	int iz, ils;	/* loop indexes */
	int doy_begin, doy_end;	/* temp day of year vars */
	int dow_begin, dow_end;	/* temp day of week vars */
	int iRetVal = 0;

	for (iz=0; iz<bldg_ptr->nzones; iz++) {
		int iLtschFound = 0;
		for (ils=0; ils<bldg_ptr->zone[iz]->nltsch; ils++) {
			doy_begin = bldg_ptr->zone[iz]->ltsch[ils]->doy_begin;
			doy_end = bldg_ptr->zone[iz]->ltsch[ils]->doy_end;
			if ((dayofyr >= doy_begin) && (dayofyr <= doy_end)) {
				dow_begin = bldg_ptr->zone[iz]->ltsch[ils]->dow_begin;
				dow_end = bldg_ptr->zone[iz]->ltsch[ils]->dow_end;
				if ((dayofweek >= dow_begin) && (dayofweek <= dow_end)) {
					bldg_ptr->zone[iz]->ltsch_id = ils;
					iLtschFound = 1;
					break;
				}
			}
		}
		// Register the fact that a lighting schedule was not found for at least one zone.
		if (!iLtschFound) iRetVal = -1;
	}

	return(iRetVal);
}

/****************************** subroutine calc_sched_days *****************************/
/* Determines sequential beginning and ending day of year for each lighting schedule */
/* defined in the bldg structure for the given run period year. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine calc_sched_days *****************************/
int calc_sched_days(
	BLDG *bldg_ptr,		/* pointer to bldg structure */
	RUN_DATA *run_ptr)	/* pointer to run period data structure */
{
	int dateJul;	/* Julian date */
	int jan01;		/* Julian date of Jan 01 for run period year */
	int month, day;
	int iz, ils;	/* loop indexes */

	julian_date(&jan01,1,1,run_ptr->year);
	for (iz=0; iz<bldg_ptr->nzones; iz++) {
		for (ils=0; ils<bldg_ptr->zone[iz]->nltsch; ils++) {
			month = bldg_ptr->zone[iz]->ltsch[ils]->mon_begin;
			day = bldg_ptr->zone[iz]->ltsch[ils]->day_begin;
			julian_date(&dateJul,month,day,run_ptr->year);
			bldg_ptr->zone[iz]->ltsch[ils]->doy_begin = dateJul - jan01 + 1;

			month = bldg_ptr->zone[iz]->ltsch[ils]->mon_end;
			day = bldg_ptr->zone[iz]->ltsch[ils]->day_end;
			julian_date(&dateJul,month,day,run_ptr->year);
			bldg_ptr->zone[iz]->ltsch[ils]->doy_end = dateJul - jan01 + 1;
		}
	}

	return(0);
}

/****************************** subroutine julian_date *****************************/
/* Determines Julian date given month, day and year. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine julian_date *****************************/
int julian_date(
	int *dateJul_ptr,		/* pointer to Julian date */
	int month,
	int day,
	int year)
{
	int		u74, u75;
	int		int75, int74;
	int		yr;

	if (year < 2000) yr = year % 1900;
	else yr = year % 2000 + 100;
	u75 = yr - 76;
	if (month > 2)
		u74 = month + 1;
	else {
		u74 = month + 13;
		u75 = u75 - 1;
	}
	int75 = u75 * 1461 / 4;
	int74 = u74 * 306 / 10;
	*dateJul_ptr = -122 + day + int75 + int74 - 1;

	return(0);
}

/****************************** subroutine get_day_of_week *****************************/
/* Determines day of week (1=Mon to 7=Sun) for first day in run period. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine get_day_of_week *****************************/
int get_day_of_week(
	int *dow_ptr,		/* pointer to day of week (1=Mon to 7=Sun) */
	RUN_DATA *run_ptr)	/* pointer to runtime data structure */
{
	int	dateJul;
	int month, day, year;

	month = run_ptr->mon_begin;
	day = run_ptr->day_begin;
	year = run_ptr->year;

	julian_date(&dateJul,month,day,year);

	*dow_ptr = dateJul % 7 + 1;

	return(0);
}

/****************************** subroutine ran0 *****************************/
/* Generates a random number between 0 and 1. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine ran0 *****************************/
double ran0(
	int *idum)
{
	static double y,maxran,v[98];
	double dum;
	static int iff=0;
	int j;
	unsigned i,k;

	if (*idum < 0 || iff == 0) {
		iff=1;
		i=2;
		do {
			k=i;
			i<<=1;
		} while (i);
		maxran=k;
		srand(*idum);
		*idum=1;
		for (j=1;j<=97;j++) dum=rand();
		for (j=1;j<=97;j++) v[j]=rand();
		y=rand();
	}
	j=(int)(1+97.0*y/maxran);
//	if (j > 97 || j < 1) fprintf(dmpfile,"RAN0: This cannot happen.\n");
	y=v[j];
	v[j]=rand();
	return y/maxran;
}

/****************************** subroutine init_monlength *****************************/
/* Initializes number of days in each month array. */
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine init_monlength *****************************/
int init_monlength(
	int monlength[12],	/* number of days in each month array */
	int iFebDays)		/* number of days in February */
{
	monlength[0] = 31;
	monlength[1] = iFebDays;
	monlength[2] = 31;
	monlength[3] = 30;
	monlength[4] = 31;
	monlength[5] = 30;
	monlength[6] = 31;
	monlength[7] = 31;
	monlength[8] = 30;
	monlength[9] = 31;
	monlength[10] = 30;
	monlength[11] = 31;

	return(0);
}

/****************************** subroutine fit4 *****************************/
// Calculates transmitted visible transmittance for given incident angle
// using previously fit curve coefs.
/****************************************************************************/
/* C Language Implementation of DOE2 Daylighting Algorithms */
/* by Rob Hitchcock */
/* Building Technologies Program, Lawrence Berkeley Laboratory */
/****************************** subroutine fit4 *****************************/
double fit4(
	double dcosb,	// cos of angle of incidence
	double dTvFit1,	// coef1
	double dTvFit2)	// coef2
{
	return max(0.0, dcosb * (2.0 - dcosb + (1.0 - dcosb) * ( 1.0 - dcosb) * (dTvFit1 + dTvFit2 * (2.0 + dcosb))));
}

/****************************** utilities *****************************/


char *str_rmblnk(
	char	*s1)
{
	int	length = strlen(s1);
	while (s1[--length] == ' ');
	s1[length+1] = '\0';
	return(s1);
}

char *str_blnk2undr(
	char	*s1)
{
	int	ii;

	int	length = strlen(s1);
	/* replace all blanks with underscore */
	for (ii=0; ii<length; ii++) if (s1[ii] == ' ') s1[ii] = '_';
	/* remove trailing underscores */
	while (s1[--length] == '_');
	s1[length+1] = '\0';
	return(s1);
}
